% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__proportionality_reading, []).

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
 *   constraint_id: public_health_mandate_authority__proportionality_reading
 *   human_readable: Public Health Mandate Authority (Proportionality Reading)
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   The proportionality reading of public health mandate authority constructs
 *   legitimacy as a **dynamic, threat-indexed constraint** on governmental
 *   power to impose medical interventions. Under this reading, mandate
 *   legitimacy and extractiveness are not fixed properties but slide along
 *   four dimensions: (1) severity of the epidemiological threat, (2)
 *   availability of non-coercive alternatives, (3) magnitude of bodily or
 *   economic coercion imposed, and (4) duration of imposition. Mandates are
 *   justified (low extractiveness, coordination-dominant) only when all four
 *   dimensions align: high threat + no alternatives + proportional coercion +
 *   limited duration. As any dimension weakens, mandate legitimacy erodes and
 *   extractiveness rises. The constraint exhibits full perspectival
 *   heterogeneity: trapped noncompliers experience maximum extraction
 *   (snare); conditionally exempt populations experience mixed
 *   coordination-extraction (tangled rope); public health authorities
 *   experience a functional coordination mechanism (rope); immunocompromised
 *   populations experience asymmetric benefit and burden (tangled rope);
 *   proportionality enforcement mechanisms function as temporal sunset logic
 *   (scaffold); and the analytical observer risks naturalizing this
 *   institutional framework as immutable law of epidemiology (mountain →
 *   false summit). The core analytical claim: proportionality is not a
 *   rhetorical cover for unlimited mandate power but an actual structural
 *   constraint that should recalibrate mandate intensity as threat recedes.
 *
 * KEY AGENTS:
 *   - Mandate Noncompliers: Powerless/trapped (biological/vocational/civic attachment), experience maximum extraction — absolute prohibition on alternatives, enforcement without proportionality threshold perceivable from their position
 *   - Immunocompromised Populations: Organized/constrained, experience mixed extraction and benefit — mandate protects them but may exclude them if based on vaccination status alone
 *   - Public Health Authority: Institutional/arbitrage, experience coordination function — can calibrate mandate scope and duration based on threat level and has exit to voluntary measures
 *   - Proportionality Courts/Ethics Boards: Organized/mobile, enforce temporal and intensity constraints — can mandate mandate recession as threat declines; represent operative constraint on authority
 *   - Bodily Autonomy Advocates: Organized/mobile, experience mandate as existential constraint on liberty; dispute whether any threat level justifies coercion (sibling reading anchor)
 *   - Public Health Primacy Advocates: Institutional/arbitrage, dispute whether proportionality should limit mandate scope; claim public health benefit justifies unlimited mandate (sibling reading anchor)
 *   - Analytical Observer: Civilizational/analytical, risk naturalizing proportionality framework as immutable epidemiological law rather than contingent constitutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.52).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.58).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority (Proportionality Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '2370f68c-27e7-40b7-8b5a-4da15bb441c9').
narrative_ontology:cs_kernel_codification('2370f68c-27e7-40b7-8b5a-4da15bb441c9', formalized).
narrative_ontology:cs_authority_grounding('2370f68c-27e7-40b7-8b5a-4da15bb441c9', lineage).
narrative_ontology:cs_interpretation_layer_present('2370f68c-27e7-40b7-8b5a-4da15bb441c9').
narrative_ontology:cs_reading_relation('2370f68c-27e7-40b7-8b5a-4da15bb441c9', public_health_mandate_authority__public_health_primary, influences).
narrative_ontology:cs_reading_relation('2370f68c-27e7-40b7-8b5a-4da15bb441c9', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('2370f68c-27e7-40b7-8b5a-4da15bb441c9', foundational, proportionality_constrains_mandate_legitimacy).
narrative_ontology:cs_axiom_status(proportionality_constrains_mandate_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2370f68c-27e7-40b7-8b5a-4da15bb441c9', proportionality_constrains_mandate_legitimacy, deontological).
narrative_ontology:cs_axiom('2370f68c-27e7-40b7-8b5a-4da15bb441c9', foundational, mandate_necessity_conditional_on_alternatives).
narrative_ontology:cs_axiom_status(mandate_necessity_conditional_on_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('2370f68c-27e7-40b7-8b5a-4da15bb441c9', mandate_necessity_conditional_on_alternatives, empirically_contingent).
narrative_ontology:cs_reference_frame('2370f68c-27e7-40b7-8b5a-4da15bb441c9', balancing_test_constitutional_framework).
narrative_ontology:cs_drift_state('2370f68c-27e7-40b7-8b5a-4da15bb441c9', contemporary_post_pandemic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2370f68c-27e7-40b7-8b5a-4da15bb441c9', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, high_transmission_prevention_agents).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, mandate_noncompliers).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, bodily_autonomy_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MANDATE NONCOMPLIER (SNARE) — Agent faces absolute prohibition on exit; refusal triggers mandatory enforcement (employment loss, institutional exclusion, legal penalty). No meaningful alternatives; coercion is maximum. Trapped by regulatory regime with no proportionality brake perceivable from inside the constraint.
constraint_indexing:constraint_classification(public_health_mandate_authority__proportionality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONDITIONALLY EXEMPT POPULATION (TANGLED ROPE) — Medical exemptions, religious exemptions, prior-infection recognition exist as theoretical alternatives but are constrained by administrative burden, proof requirements, and discretionary denial. Agent benefits from collective protection yet bears asymmetric burden of seeking exemption. Mixed coordination (collective goods) and extraction (selective burden placement).
constraint_indexing:constraint_classification(public_health_mandate_authority__proportionality_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (ROPE) — Experiences mandate as coordination mechanism for disease containment. Authority has exit option (arbitrage): can calibrate mandate scope, intensity, and duration based on threat level. Can transition to voluntary measures without institutional collapse. Mandate serves genuine coordinating function.
constraint_indexing:constraint_classification(public_health_mandate_authority__proportionality_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IMMUNOCOMPROMISED COALITION (TANGLED ROPE) — Benefits from mandate (reduced transmission risk) but constrained by mandate design that may exclude them or create perverse incentives (e.g., mandate based on vaccination status alone may not account for immunocompromised unvaccinated or vaccine-resistant populations). Organizational capacity allows advocacy but lacks ultimate exit option — cannot escape epidemiological vulnerability.
constraint_indexing:constraint_classification(public_health_mandate_authority__proportionality_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROPORTIONALITY ENFORCEMENT MECHANISM (SCAFFOLD) — Sunset logic embedded in proportionality standards: mandate intensity should decline as threat level declines, duration should be limited to epidemiological necessity, coercion magnitude should scale with invasion severity. Organized agents (courts, ethics boards) with mobile exit can calibrate mandate. When threat recedes, proportionality framework mandates mandate recession. Temporal boundary condition.
constraint_indexing:constraint_classification(public_health_mandate_authority__proportionality_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, infectious disease dynamics and population immunity constitute natural constraints on liberty: some degree of collective coordination is structurally inevitable when pathogenic threat exists. Liberty itself depends on population health. This perspective treats proportionality balancing as immutable natural law — the necessity of trading liberty for epidemiological containment. Engine's false summit detector will identify beneficiaries and reveal this as naturalization of a contingent constitutional framework.
constraint_indexing:constraint_classification(public_health_mandate_authority__proportionality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__proportionality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_health_mandate_authority__proportionality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_health_mandate_authority__proportionality_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate. The proportionality reading treats extractiveness as dynamic, varying with threat level. At low threat (e.g., endemic seasonal respiratory virus), mandates fail proportionality on 'necessity' dimension — alternatives suffice, making mandate appear extractive (shift toward snare). At high threat (e.g., novel high-mortality pathogen), mandates meet proportionality — no viable alternatives exist, making mandate appear as legitimate coordination (shift toward rope/tangled rope). The baseline value (0.52) represents a **moderate threat scenario** where proportionality analysis produces mixed results: some populations benefit, some bear disproportionate burden. Suppression (0.58): Moderate-high. Mandates employ regulatory prohibition and employment/institutional exclusion — significant coercive force. But suppression is constrained by proportionality framework: proportional suppression should decline as threat reclines. The measurement trajectory shows suppression rising (0.25 → 0.78) as threat escalates, then expected to decline as threat recedes (not shown beyond time point 6). Theater ratio (0.48): Moderate-low. Under the proportionality reading, mandate communications should justify each coercive measure by reference to threat, alternatives, and duration. Theater is relatively low because the reading treats proportionality as an **operative constraint**, not rhetorical cover. Theater rises at low threat (0.62) when proportionality justification becomes attenuated, falls at high threat (0.35) when threat-necessity is clear. This trajectory reveals the reading's claim: as threat is unambiguous, performative justification declines; as threat recedes, performative justification must increase to maintain mandate legitimacy or mandate must be rescinded.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces maximum perspectival gap. The noncomplier sees snare (absolute prohibition, enforcement, no perceivable proportionality threshold). The authority sees rope (functional coordination, threat-responsive calibration, arbitrage exit). The immunocompromised coalition sees mixed benefit-and-burden (tangled rope). Proportionality enforcement sees temporal constraint (scaffold with sunset as threat recedes). The analytical observer risks seeing mountain (natural epidemiological law requiring liberty-health tradeoff). The kernel contest surface in this gap: does proportionality actually constrain mandate power (proportionality_reading), or do authorities claim proportionality while maintaining unlimited mandate scope (public_health_primary falsely wearing proportionality clothing), or do any mandates constitute unjustifiable violation (bodily_autonomy_primary)? The gap reveals which reading is operative in practice: if mandates persist long after threat recedes, proportionality is theater (piton) and public_health_primary reading governs actual behavior. If mandates are rescinded when proportionality metrics fail, proportionality reading governs and public_health_primary is foreclosed.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's structural position determines directionality value (d) and experienced extractiveness (χ). Mandate noncompliers: d ≈ 0.95 (maximum target), d-value approaches 1.0 → f(d) ≈ 1.42 (powerless multiplier) → high χ. These agents face absolute prohibition and enforcement; their only exit is flight. Conditionally exempt populations: d ≈ 0.60 (asymmetric cost distribution), moderate/constrained power → d-value 0.60 → f(d) ≈ 0.88 → moderate χ. Public health authority: d ≈ 0.08 (beneficiary with arbitrage exit), institutional/arbitrage → d-value 0.08 → f(d) ≈ -0.08 → negative χ (authority experiences mandate as enabling rather than extractive). Immunocompromised coalition: d ≈ 0.45 (mixed costs and benefits), organized/constrained → d-value 0.45 → f(d) ≈ 0.38 → moderate-low χ. Proportionality enforcement: d ≈ 0.50 (symmetric observer position), organized/mobile → d-value 0.50 → f(d) ≈ 0.65 → moderate χ but with exit capacity (can calibrate or rescind). The proportionality reading's core claim: d and f(d) are **threat-indexed**. As threat level rises, noncompliers' d approaches 1.0 (their victim status becomes justified); as threat recedes, d should fall toward 0.3 (noncompliers become arbitrary targets, extraction rises). The engine's derived d values should track threat trajectory; if they don't, proportionality constraint is inoperative.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading resolves mandatrophy by introducing a **metric-indexed constraint** on mandate legitimacy: extractiveness is not fixed but varies with threat level, alternative availability, coercion magnitude, and duration. This requires the analyst to ask not 'Is this mandate a Rope or Snare?' but 'Under what conditions does this mandate classify as what type?' The reading avoids false mandatrophy resolution by refusing to collapse the six types into a single claim. Instead, it maps the conditions under which each type is appropriate: Snare (mandate persists despite low threat or availability of alternatives), Tangled Rope (mixed benefit and burden with proportional coercion), Rope (clear threat, no alternatives, proportional response, functional coordination), Scaffold (time-limited mandate with sunset logic). The reading is mandatrophy-resolving because it treats classification as **contextual and revisable**, not frozen. The engine's task becomes: as threat level changes (empirical measurement), does mandate intensity change correspondingly? If yes, proportionality operates and the constraint is legitimately tangled rope/rope. If no, proportionality is theater and the constraint degrades to snare/piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_level_measurement_ambiguity,
    'What constitutes ''sufficient threat level'' to justify proportionality of a particular mandate intensity? Who measures threat, and by what standard?',
    'Epidemiological comparative analysis: case fatality rate thresholds, transmission rate dynamics, healthcare system capacity burden. Institutional analysis: which decision-maker controls threat assessment, what appeals mechanisms exist, what external review is available.',
    'If threat measurement is opaque or authority-controlled: mandate appears as extraction even under proportionality framework (Snare). If measurement is transparent and externally reviewable: proportionality framework functions as designed (Tangled Rope with operative constraint on extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threat_level_measurement_ambiguity, empirical, 'Who measures threat level and by what epistemic standard').

omega_variable(
    alternative_containment_sufficiency,
    'For a given threat level, do non-coercive alternatives (voluntary measures, targeted protection of vulnerable populations, voluntary isolation protocols) achieve epidemiological sufficiency, or is mandate coercion structurally necessary?',
    'Comparative epidemiological data: voluntary vs mandatory intervention outcomes in equivalent threat scenarios. Historical analysis of containment success with and without mandates. Population behavior modeling with and without coercive enforcement.',
    'If alternatives suffice: proportionality framework collapses — mandate fails proportionality test on ''necessity'' dimension (victim classification shifts, extractiveness rises to snare range). If alternatives are insufficient: proportionality framework holds (tangled rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_containment_sufficiency, empirical, 'Whether non-coercive alternatives achieve epidemiological containment').

omega_variable(
    proportionality_metric_alignment,
    'Do the four dimensions of proportionality (threat severity, alternative availability, coercion magnitude, duration constraint) actually determine mandate legitimacy and extractiveness, or do political/institutional factors decouple from this calculus?',
    'Institutional analysis: cases where proportionality metrics would justify mandate recession but mandates persisted; cases where low-proportionality mandates were imposed but faced successful challenge. Comparative constitutionalism: how different jurisdictions operationalize proportionality and what outcomes follow.',
    'If metrics align with outcomes: proportionality reading is operative constraint on extraction (tangled rope, scaffold). If decoupled: proportionality is theater masking extraction (piton). If metrics are manipulated by authority: proportionality is false summit (snare with naturalization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_metric_alignment, empirical, 'Whether proportionality metrics actually constrain mandate legitimacy').

omega_variable(
    victim_boundary_determination,
    'Which populations count as ''victims'' in the proportionality framework? Does a mandate protect some populations (immunocompromised) while extracting from others (bodily autonomy-privileged noncompliers)? Or is the victim set determinate?',
    'Epidemiological decomposition: which populations experience net harm from mandate, which net benefit, which mixed effect. Equity analysis: how mandate impacts vary by baseline health status, access to exemption mechanisms, economic capacity to absorb employment loss.',
    'If victim boundary is ambiguous or context-dependent: extractiveness is dynamic, constrained by threat level (reading''s core claim). If victim boundary is fixed regardless of threat: constraint collapses to pure extraction (snare). If victim boundary is deliberately obscured: constraint is false summit (snare disguised as proportionality).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_boundary_determination, conceptual, 'How victim populations are determined under proportionality framework').

omega_variable(
    proportionality_vs_primary_readings_foreclosure,
    'Does commitment to proportionality framework logically foreclose the bodily_autonomy_primary reading (which denies that any level of threat can justify mandate coercion) or the public_health_primary reading (which denies that proportionality considerations can limit mandate scope)?',
    'Logical analysis of axiom commitments: if proportionality grants legitimacy to mandates conditional on threat/alternative/coercion/duration metrics, can a framework simultaneously hold that (a) no threat level justifies coercion, or (b) any public health benefit justifies unlimited mandate scope? Map the logical space and identify genuine foreclosures vs coexistence.',
    'If proportionality forecloses bodily_autonomy_primary: the two readings are incompatible within a single framework. If proportionality forecloses public_health_primary: constraint on mandate scope is operative. If neither is foreclosed: all three readings coexist across different institutional and political contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_vs_primary_readings_foreclosure, conceptual, 'Logical relationship between proportionality reading and the two sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phma_prop_theater_low_threat, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(phma_prop_theater_moderate_threat, public_health_mandate_authority__proportionality_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(phma_prop_theater_high_threat, public_health_mandate_authority__proportionality_reading, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(phma_prop_extr_low_threat, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(phma_prop_extr_moderate_threat, public_health_mandate_authority__proportionality_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(phma_prop_extr_high_threat, public_health_mandate_authority__proportionality_reading, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(phma_prop_supp_low_threat, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(phma_prop_supp_moderate_threat, public_health_mandate_authority__proportionality_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(phma_prop_supp_high_threat, public_health_mandate_authority__proportionality_reading, suppression_requirement, 6, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% Public health mandate authority is a contested kernel with three structurally distinct readings, each constituting a separate constraint. The proportionality_reading treats mandate legitimacy as dynamic and threat-indexed; the public_health_primary reading treats mandate scope as unrestricted by proportionality; the bodily_autonomy_primary reading denies that any threat level justifies mandate coercion. The three constraints have different ε values: proportionality (ε=0.52, dynamic) reflects mixed coordination-extraction that varies with context; public_health_primary (expected ε>0.70, snare-range) treats mandate as institution-benefiting extraction disguised as public health; bodily_autonomy_primary (expected ε<0.25, rope or mountain-range) treats mandate as illegitimate constraint on inalienable rights. Each constraint is a separate JSON file linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__proportionality_reading, institutional, 0.08).
constraint_indexing:directionality_override(public_health_mandate_authority__proportionality_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
