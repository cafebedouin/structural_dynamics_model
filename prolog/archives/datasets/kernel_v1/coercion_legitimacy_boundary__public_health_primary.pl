% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: State Coercion Legitimacy Boundary (Public Health Primary Reading)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel: the
 *   legitimacy boundary for state coercion in medical contexts. The
 *   public_health_primary reading holds that when collective harm-prevention
 *   demonstrably outweighs individual autonomy cost, the state may compel
 *   medical intervention. This is ONE interpretation of the boundary; sibling
 *   readings (bodily_autonomy_primary and proportionality_reading) interpret
 *   it differently. The three readings coexist across different
 *   institutional, legal, and ethical frameworks — no single reading
 *   currently forecloses the others, though they generate distinct policy
 *   consequences. The public_health_primary reading grounds its authority in
 *   epidemiological necessity and collective welfare; it shifts unvaccinated
 *   individuals into the victim set (coerced subjects) while
 *   immunocompromised populations exit victimhood and enter the beneficiary
 *   set (protected by high vaccination rates). The constraint exhibits
 *   tangled_rope structure: genuine coordination function (synchronizing
 *   population immunity) entangled with asymmetric extraction (concentrated
 *   coercion on individuals who refuse). The extractiveness trajectory (0.32
 *   → 0.62 over the interval) reflects enforcement apparatus accumulation —
 *   legal penalties increase, employment exclusions deepen, social stigma
 *   compounds — as the authority structure hardens. The suppression
 *   trajectory (0.45 → 0.72) tracks the institutional machinery built to
 *   enforce compliance: documentation requirements, workplace monitoring,
 *   exclusion from public spaces. The theater ratio remains low (0.20 → 0.35)
 *   because the mandate's functional mechanism (preventing disease
 *   transmission) is genuine, not performative — this reading sees
 *   enforcement as necessary and justified, not ritual.
 *
 * KEY AGENTS:
 *   - Vaccine-Hesitant Individuals: Primary victims (powerless/trapped) — face legal penalties, employment loss, school exclusion with no exit option. Compressed into victim set by this reading's framework.
 *   - Religious Objectors: Primary victims (powerless/trapped) — conscience-based refusal is treated as equivalent to willful non-compliance; no exemption pathway recognized within this reading's frame.
 *   - Immunocompromised Populations: Shifted from victims to beneficiaries (moderate/constrained) — protected by high vaccination rates but extracted from through surveillance and medical monitoring.
 *   - Public Health Authority: Primary beneficiary (institutional/arbitrage) — exercises legitimate coercive power, experiences mandate as coordination mechanism, maintains authority to set thresholds.
 *   - Medical Ethics Reformers: Organized agents (organized/constrained) — challenge the reading's framework; see mandate as temporary emergency measure with sunset path through technology maturation.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — evaluates the reading's internal consistency and empirical support for its core claim that collective harm justifies autonomy override.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.58).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.72).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "State Coercion Legitimacy Boundary (Public Health Primary Reading)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, '5af5a0da-a803-4a99-914d-8320515125a6').
narrative_ontology:cs_kernel_codification('5af5a0da-a803-4a99-914d-8320515125a6', fixed_text).
narrative_ontology:cs_authority_grounding('5af5a0da-a803-4a99-914d-8320515125a6', extraction).
narrative_ontology:cs_interpretation_layer_present('5af5a0da-a803-4a99-914d-8320515125a6').
narrative_ontology:cs_reading_relation('5af5a0da-a803-4a99-914d-8320515125a6', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('5af5a0da-a803-4a99-914d-8320515125a6', coercion_legitimacy_boundary__proportionality_reading, influences).
narrative_ontology:cs_axiom('5af5a0da-a803-4a99-914d-8320515125a6', foundational, collective_harm_outweighs_autonomy).
narrative_ontology:cs_axiom_status(collective_harm_outweighs_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('5af5a0da-a803-4a99-914d-8320515125a6', collective_harm_outweighs_autonomy, empirically_contingent).
narrative_ontology:cs_axiom('5af5a0da-a803-4a99-914d-8320515125a6', foundational, state_authorized_coercion_for_public_health).
narrative_ontology:cs_axiom_status(state_authorized_coercion_for_public_health, holdable).
narrative_ontology:cs_axiom_grounding('5af5a0da-a803-4a99-914d-8320515125a6', state_authorized_coercion_for_public_health, instrumental).
narrative_ontology:cs_reference_frame('5af5a0da-a803-4a99-914d-8320515125a6', epidemiologically_justified_emergency_authority).
narrative_ontology:cs_drift_state('5af5a0da-a803-4a99-914d-8320515125a6', endemic_phase_authority_persistence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5af5a0da-a803-4a99-914d-8320515125a6', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, pediatric_unvaccinated).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, disease_control_apparatus).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, religious_objectors).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, medical_autonomy_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COERCED SUBJECT (SNARE) — Individuals mandated to undergo medical intervention they refuse experience this as pure extraction without coordination benefit. No exit option available: employment requires vaccination, school enrollment requires vaccination, no genuine alternative. Maximum suppression (legal penalty, social stigma, loss of livelihood). The mandate is framed as collective benefit but the individual bears concentrated cost with no choice.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__public_health_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH AUTHORITY (ROPE) — Sees mandate as pure coordination mechanism: communicating disease risk, synchronizing population immunity, solving collective action problem. Experiences the constraint as functional coordination with minimal coercive overhead from their institutional vantage point. Authority has arbitrage options — can choose enforcement level, exemptions, messaging frame. Net beneficiary through institutional mandate.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__public_health_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: IMMUNOCOMPROMISED POPULATION (TANGLED ROPE) — Genuinely benefit from vaccination rates (protection from disease transmission), experiencing legitimate coordination function. But also bear extraction cost: surveillance requirements, medical monitoring, institutional tracking of their vulnerability status. The constraint both protects and extracts from them. Exit options are constrained (cannot refuse participation in surveillance without losing protection). Mixed coordination and asymmetric extraction.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__public_health_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDICAL ETHICS REFORM COALITION (SCAFFOLD) — Organized groups (patient rights advocates, some medical associations, civil liberties organizations) see the mandate as a temporary policy solution with a sunset: as vaccines improve (higher efficacy, fewer side effects), as variant surveillance improves (risk-stratification becomes possible), and as alternative protective measures mature (rapid treatment protocols), the necessity for population-wide mandates declines. Low effective extraction because the coalition perceives a clear exit path through technological and policy maturation.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__public_health_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PANDEMIC EMERGENCY AUTHORITY (PITON) — Institutional machinery built during acute emergency (lockdowns, vaccine mandates, travel restrictions) persists through inertia despite lower necessity as disease severity declines and immunity (from vaccination or prior infection) becomes widespread. The authority structure maintains coercive infrastructure (legal penalties, employment exclusions) without clear functional necessity. Theater ratio is high — the mandate ritual persists as institutional performance of control rather than proportional response to current epidemiological threat.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__public_health_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PUBLIC HEALTH PRIMARY FRAME (TANGLED ROPE) — From the reading's own analytical standpoint, state coercion for disease prevention serves both a genuine coordination function (synchronizing population immunity to prevent outbreaks) AND an extraction mechanism (concentrating control over bodies with health state dependent on compliance). This frame holds that when collective harm-prevention demonstrably outweighs individual autonomy cost, coercion becomes legitimate. The constraint is tangled because the reading explicitly accepts asymmetric extraction as justified by collective benefit. This is the frame's core commitment.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__public_health_primary, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__public_health_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coercion_legitimacy_boundary__public_health_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coercion_legitimacy_boundary__public_health_primary, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, TR),
    TR >= 0.70.

:- end_tests(coercion_legitimacy_boundary__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading justifies substantial state power over bodies and medical choices in the name of collective harm-prevention. The measured extractiveness reflects the enforcement apparatus (legal penalties, employment exclusions, access restrictions) built to compel compliance. From the reading's own frame, this extraction is legitimate because it is proportional to the collective benefit achieved. However, extractiveness is not maximal (would be 0.75+) because: (1) the constraint preserves some choice architecture (education/employment requirement vs. outright arrest), (2) exemptions for medical contraindications exist in most jurisdictions, and (3) the mandate functions through institutional channels (law, employment policy) rather than direct state violence. The trajectory of rising extractiveness (0.32 → 0.62) reflects deepening enforcement as the authority initially cautious in emergency measures becomes more confident and entrenched. Suppression (0.72): High. Barriers to refusing the mandate are substantial and multilayered: legal penalties (fines, loss of professional licenses), economic penalties (employment exclusion), social penalties (stigma, isolation), and access penalties (exclusion from schools, public venues). These barriers are intentionally engineered by the authority to increase compliance cost. The trajectory of rising suppression (0.45 → 0.72) shows the enforcement apparatus hardening as emergency measures transition into institutionalized policy. Theater ratio (0.35): Moderate-low. The mandate's functional mechanism is genuine — vaccination does reduce transmission and disease burden — so the constraint is not purely performative. However, some theater is present: (1) the mandate sometimes exceeds epidemiological necessity (e.g., vaccinating already-immune individuals, mandating vaccines against low-threat variants), (2) the messaging often emphasizes collective benefit while downplaying individual risks, and (3) the authority's framing of necessity sometimes obscures uncertainty (presenting epidemiological estimates as certainties). The theater ratio remains relatively low because, from this reading's perspective, the mandate is largely functional, not ritual.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap in this constraint is between beneficiary and victim framings. The public health authority sees the mandate as coordination (Rope) — they are solving a collective action problem: individuals may not vaccinate even though population-wide vaccination benefits everyone, creating a prisoner's dilemma that the mandate resolves. The authority experiences this as legitimate and functional. The vaccine-hesitant individual sees coercion (Snare) — they have no choice, face escalating penalties, and experience the constraint as pure extraction. The immunocompromised population occupies an ambiguous middle: they benefit from high vaccination rates (protection) but are extracted from through surveillance and medical monitoring (Tangled Rope). The analytical observer applying the public_health_primary frame sees legitimate tangled_rope: genuine coordination function (synchronizing immunity) entangled with justified asymmetric extraction (the autonomy override is proportional to collective benefit). But the bodily_autonomy_primary reading would see this same constraint as snare or worse — pure extraction masked by public health framing. The perspectival gap reveals that the classification outcome hinges entirely on whether the reading's core axiom (collective harm outweighs autonomy) is accepted.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural relationship to the constraint — whether they are beneficiaries (low d) or victims (high d). In this constraint: Vaccine-hesitant individuals have d ≈ 0.85 (high): They are primary targets of extraction, trapped with no exit, and bear concentrated costs (penalties, exclusion). The public health authority has d ≈ 0.08 (low): They are primary beneficiaries, exercise institutional control, and have arbitrage options (setting thresholds, exemptions, enforcement level). Immunocompromised populations have d ≈ 0.45 (moderate): They benefit from vaccination rates but are extracted from through surveillance. The reading's own frame determines who counts as a victim. Under bodily_autonomy_primary, all vaccine refusers are simply exercising rights and should not be in the victim set at all. Under public_health_primary, vaccine refusers are reclassified from autonomous agents to coercible subjects in the victim set. This is not a factual change in who experiences extraction — it is a normative reclassification of who counts as a legitimate target of extraction. The reading's axioms determine the victim set.
 *
 * MANDATROPHY ANALYSIS:
 *   The public_health_primary reading avoids mandatrophy by explicitly accepting the tangled_rope classification. The constraint is NOT presented as pure rope (coordination without asymmetric extraction) or as pure snare (extraction masked as coordination). Instead, the reading holds that the constraint is genuinely tangled: it serves a real coordination function (synchronizing population immunity) AND imposes asymmetric extraction (overriding individual autonomy). The reading's core claim is that this tangle is legitimate — the coordination benefit justifies the extraction cost. This is structurally coherent: mandatrophy is avoided by directly addressing why the asymmetry is justified, not by hiding it. However, mandatrophy risk emerges from the false summit gate: the bodily_autonomy_primary reading treats the public health frame as naturalization of what it views as a constructed institutional choice. If the engine detects beneficiaries on the mountain classification (authority interests in emergency power, pharmaceutical industry interests in vaccination rates), it triggers false summit evaluation. The public_health_primary reading is explicitly NOT a mountain — it accepts the tangled structure. But the reading's competitors might frame it as a false summit: 'the public health authority benefits from the mandate framing; therefore the mandate is not a natural law requiring coercion, but a constructed extraction mechanism.' This is the mandatrophy's real location: between the public_health_primary frame (extraction is legitimate because justified) and the bodily_autonomy_primary frame (extraction is never legitimate regardless of justification).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_harm_quantification,
    'What threshold of collective harm (disease mortality, hospitalization capacity, transmission rate) justifies overriding individual autonomy? Is the threshold the same for all infectious diseases?',
    'Comparative analysis of mandate implementation across disease severity gradients (COVID-19 vs seasonal influenza vs measles); epidemiological modeling of harm prevented vs autonomy cost; historical case studies of mandate necessity',
    'If threshold is disease-specific and high: few mandates are justified (most diseases fall below proportionality threshold). If threshold is low or universal: most infectious disease mandates become structurally justified (but proportionality sibling reading forecloses this logic). This is the reading''s core irreducible uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_harm_quantification, empirical, 'Threshold of collective harm that justifies autonomy override').

omega_variable(
    extraction_vs_coordination_entanglement,
    'How much of the measured extractiveness (0.58) is legitimate institutional overhead for public health coordination vs. genuine exploitation of emergency authority?',
    'Cost-benefit analysis of mandate enforcement infrastructure; comparison of implementation costs across jurisdictions with different transparency; audit of penalties applied to refusers vs. coordination costs of synchronizing immunity',
    'If most extraction is genuine overhead: constraint is closer to pure rope (coordination is legitimate). If extraction substantially exceeds overhead: constraint is closer to snare masked by public health framing. This omega reveals whether the reading''s justification is structural or rhetorical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_entanglement, empirical, 'Ratio of legitimate coordination overhead to extractive penalty').

omega_variable(
    authority_scope_drift,
    'Once legitimized for a specific high-threat disease (COVID-19), does the coercive authority scope expand to lower-threat diseases where collective harm does not justify autonomy override?',
    'Historical tracking of mandate scope post-COVID: which diseases remain mandated, what are the stated justifications, do epidemiological thresholds match the claimed proportionality; survey of public health authority stated rationale for scope decisions',
    'If scope expands into low-threat domains: reading''s proportionality gate fails in practice (sibling proportionality reading''s critique is empirically supported). If scope contracts appropriately: reading''s logic holds. This tests whether the reading''s framework prevents institutional ratcheting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_scope_drift, empirical, 'Whether coercive authority scope expands beyond initial public health threshold').

omega_variable(
    autonomy_vs_collective_commensurability,
    'Are individual autonomy and collective harm-prevention truly commensurable in a single moral calculus, or are they incommensurable values that cannot be weighed against each other?',
    'Philosophical analysis of bodily autonomy as a deontological right vs. collective benefit as a consequentialist value; examination of whether the reading''s framework collapses into utilitarianism or retains deontological constraints',
    'If incommensurable: the reading''s core premise (weighing autonomy against collective benefit) is foundationally flawed — bodily autonomy primary reading forecloses this logic. If commensurable: the reading''s framework is valid but requires specifying the exchange rate. This is the reading''s deepest structural vulnerability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_vs_collective_commensurability, conceptual, 'Moral commensurability of autonomy and collective harm').

omega_variable(
    informed_consent_degradation,
    'Does mandate framing (as public health necessity) prevent genuine informed consent to vaccination, reducing the constraint from legitimate coercion to fraud-enabled coercion?',
    'Analysis of mandate communication vs. informed consent literature standards; surveys of coerced subjects'' understanding of risks/benefits; comparison of autonomy-respecting and mandate conditions on vaccine acceptance rates and adverse event reporting',
    'If mandates prevent informed consent: the constraint loses its legitimacy basis (coercion requires transparency about alternatives). If consent can be preserved within mandate: reading''s logic is structurally sound but requires specific institutional design. This omega tests the reading''s implementation integrity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_consent_degradation, empirical, 'Whether mandate framing prevents informed consent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coercion_pub_theater_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(coercion_pub_theater_t4, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 4, 0.35).

% Extraction over time
narrative_ontology:measurement(coercion_pub_extractiveness_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(coercion_pub_extractiveness_t2, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(coercion_pub_extractiveness_t4, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(coercion_pub_extractiveness_t6, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 6, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(coercion_pub_suppression_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(coercion_pub_suppression_t2, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(coercion_pub_suppression_t4, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 4, 0.72).
narrative_ontology:measurement(coercion_pub_suppression_t6, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, proportionality_reading).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, informed_consent_doctrine).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, emergency_authority_ratchet).

% DUAL FORMULATION NOTE:
% The coercion_legitimacy_boundary kernel has three structurally distinct readings, each instantiating a different constraint with different ε values and beneficiary/victim sets. This file documents the public_health_primary reading (ε=0.58, tangled_rope). The bodily_autonomy_primary reading (ε would be ~0.72, snare) classifies the same institutional arrangements as unambiguous extraction. The proportionality_reading (ε would be ~0.38, tangled_rope with stricter threshold) imposes a narrower gate on when extraction is justified. All three readings link via network.affects_constraints to represent the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__public_health_primary, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
