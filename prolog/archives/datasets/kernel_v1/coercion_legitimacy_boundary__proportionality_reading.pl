% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__proportionality_reading, []).

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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Coercion Legitimacy Scales with Disease Severity (Proportionality Reading)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the PROPORTIONALITY READING of the contested
 *   kernel 'coercion_legitimacy_boundary.' The reading holds that state
 *   coercion in medical mandates is justified only when disease severity
 *   (mortality, hospitalization, transmission dynamics) reaches a threshold
 *   where the collective harm of uncontrolled transmission exceeds the
 *   individual harm of forced medical intervention. On this reading, measles
 *   (R0=12-18, CFR=0.2%, pre-vaccine death toll ~1000/year in endemic
 *   countries) justifies mandatory vaccination, while seasonal influenza
 *   (R0=1.3, CFR=0.1%, endemic circulating) does not. The proportionality
 *   principle creates a framework for distinguishing legitimate public health
 *   coercion from institutional authority expansion. This reading coexists
 *   with two sibling readings: bodily_autonomy_primary (which holds that
 *   coercion is categorically impermissible regardless of disease severity)
 *   and public_health_primary (which holds that state may compel intervention
 *   whenever collective harm prevention outweighs individual autonomy — a
 *   standard that can justify flu mandates, respiratory syncytial virus
 *   mandates, and much broader coercion). The proportionality reading
 *   occupies the middle ground: disease severity matters; thresholds exist;
 *   some diseases justify coercion, others do not. The extractiveness value
 *   (0.52) reflects moderate institutional extraction layered onto legitimate
 *   disease control: authorities face incentives to expand coercion beyond
 *   justified thresholds, and institutional mission creep is documented in
 *   pandemic era policies. The rising trajectory (0.35 → 0.52 over interval)
 *   shows increasing institutional extraction as authorities blur
 *   disease-severity distinctions (flu mandates for healthcare workers
 *   despite low CFR; broad testing mandates for endemic low-mortality
 *   variants).
 *
 * KEY AGENTS:
 *   - Disease Control Authority: Institutional beneficiary (institutional/arbitrage) — designs mandate policy; benefits from expanded coercion authority; experiences policy as coordination
 *   - Unvaccinated Individual (High-Severity Pathogen): Primary victim (powerless/trapped) — faces legal exclusion, social ostracism, coercive intervention; no exit options; no coordination benefit
 *   - Unvaccinated Individual (Low-Severity Pathogen): Secondary victim (powerful/mobile) — faces moderate extraction via workplace mandates but can exit into other sectors; coordination benefit is minimal (low-severity disease does not require mandate)
 *   - Vaccine-Hesitant Affluent Individual: Tertiary actor (powerful/mobile) — experiences tangled rope (can exit via private school, telehealth, relocation but faces legal/social pressure); benefits from disease-controlled environment but bears extraction cost
 *   - Public Health Institution: Institutional beneficiary (organized/constrained) — coordinates disease control (genuine coordination function) while extracting authority and budget from expansive mandate justification
 *   - Bodily Autonomy Principle: Abstract victim (powerless/trapped) — institutional coercion erodes the principle through normalization of medical coercion; no countervailing force
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional boundary-setting as inherent epidemiological law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.52).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.58).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Coercion Legitimacy Scales with Disease Severity (Proportionality Reading)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, 'f0309193-d23a-4029-9360-a8e8aa546f53').
narrative_ontology:cs_kernel_codification('f0309193-d23a-4029-9360-a8e8aa546f53', fixed_text).
narrative_ontology:cs_authority_grounding('f0309193-d23a-4029-9360-a8e8aa546f53', extraction).
narrative_ontology:cs_interpretation_layer_present('f0309193-d23a-4029-9360-a8e8aa546f53').
narrative_ontology:cs_reading_relation('f0309193-d23a-4029-9360-a8e8aa546f53', coercion_legitimacy_boundary__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('f0309193-d23a-4029-9360-a8e8aa546f53', coercion_legitimacy_boundary__public_health_primary, influences).
narrative_ontology:cs_axiom('f0309193-d23a-4029-9360-a8e8aa546f53', foundational, disease_severity_determines_legitimacy).
narrative_ontology:cs_axiom_status(disease_severity_determines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f0309193-d23a-4029-9360-a8e8aa546f53', disease_severity_determines_legitimacy, instrumental).
narrative_ontology:cs_axiom('f0309193-d23a-4029-9360-a8e8aa546f53', foundational, coercion_proportional_to_collective_harm).
narrative_ontology:cs_axiom_status(coercion_proportional_to_collective_harm, holdable).
narrative_ontology:cs_axiom_grounding('f0309193-d23a-4029-9360-a8e8aa546f53', coercion_proportional_to_collective_harm, deontological).
narrative_ontology:cs_reference_frame('f0309193-d23a-4029-9360-a8e8aa546f53', severity_scaled_coercion_authority).
narrative_ontology:cs_drift_state('f0309193-d23a-4029-9360-a8e8aa546f53', contemporary_institutional_practice, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f0309193-d23a-4029-9360-a8e8aa546f53', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, disease_control_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, high_transmission_disease_public).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, severely_immunocompromised_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, bodily_autonomy_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNVACCINATED INDIVIDUAL FACING HIGH-SEVERITY MANDATE (SNARE) — For measles (R0=12-18, CFR=0.2%), mandates appear justified. The individual faces legal exclusion from school/employment, social ostracism, and coercive vaccination with minimal exit options. Suppression is high: alternatives (homeschooling, private practice) are costly and legally constrained. No genuine coordination benefit flows to the coerced individual — the constraint exists to extract compliance. Classification: Snare.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__proportionality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VACCINE-HESITANT AFFLUENT INDIVIDUAL (TANGLED ROPE) — Can afford private school, telehealth, or relocation to permissive jurisdictions. Experiences both coordination benefit (lives in disease-controlled environment) and extraction (faces legal and social pressure despite mobility). The constraint coordinates disease control while asymmetrically extracting from those with fewer resources. Classification: Tangled Rope.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__proportionality_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: DISEASE CONTROL AUTHORITY (ROPE) — Experiences the mandate as pure coordination: communicating risk, enforcing vaccination thresholds, preventing outbreak cascades. Benefits from compliance without bearing the suppression costs. Arbitrage: can shift jurisdiction-level policy. Classification: Rope (from authority perspective, this is coordination).
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__proportionality_reading, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: UNVACCINATED INDIVIDUAL FACING LOW-SEVERITY PATHOGEN MANDATE (TANGLED ROPE) — Influenza (R0=1.3, CFR=0.1%) does not justify coercive mandates under proportionality reading. Yet healthcare worker mandates for flu exist in many jurisdictions. Individual experiences moderate extraction (can avoid healthcare work, but career cost is real) and minimal coordination benefit (flu vaccination has low collective-action payoff). Suppression is moderate: alternatives exist but are costly. The constraint here borders on unjustified — the extractiveness is similar to measles but the disease severity does not warrant it. Classification: Tangled Rope (approaching snare).
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__proportionality_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HEALTH INSTITUTION (TANGLED ROPE) — Public health agencies coordinate disease control (genuine coordination function) while extracting authority and budget resources from justification that blurs pathogen severity tiers (flu mandates grouped with measles). The institution benefits from expansive coercion authority; it bears costs only if legitimacy erodes. This is institutional extraction: the constraint simultaneously serves coordination AND centralizes control. Classification: Tangled Rope.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__proportionality_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY QUARANTINE LAW (PITON) — Pre-COVID quarantine and isolation authority derives from 19th-century plague/cholera response. The legal machinery persists through institutional inertia despite declining disease severity (modern medicine has reduced CFR dramatically). Invoked rhetorically but rarely enforced (theater ratio high). The constraint is performatively maintained — legislatures periodically reaffirm quarantine statutes without substantive revision. Classification: Piton.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__proportionality_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalizing perspective, some coercion is inherent to epidemiology: disease transmission dynamics create collective-action problems that individual choice cannot solve (tragedy of the commons). On this view, coercion is a natural law of infectious disease control — no polity can avoid it. However, this naturalizes a contingent institutional decision (WHICH diseases merit coercion; HOW MUCH coercion; for WHAT duration). The engine's false-summit detector will flag this as constructed, not natural.
constraint_indexing:constraint_classification(coercion_legitimacy_boundary__proportionality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coercion_legitimacy_boundary__proportionality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coercion_legitimacy_boundary__proportionality_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, TR),
    TR >= 0.70.

:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts substantial authority compliance from coerced individuals, particularly at high disease-severity threshold (measles). The extractiveness is not maximal because the proportionality frame creates a principled limit: authorities cannot justify unlimited coercion for all pathogens. The rising trajectory (0.35 → 0.52) reflects institutional mission creep: as authorities apply measles-justified coercion logic to low-severity pathogens (flu, endemic viruses), the constraint slips toward pure extraction. The midpoint value (0.48 at t=5) represents the moment when institutional authority begins applying coercive mechanisms beyond original disease-severity justification. Suppression (0.58): Moderate-high. Legal exclusion from school/employment, social ostracism, health care access restrictions create substantial barriers to non-compliance. But suppression is not total: private schools, telehealth, relocation, religious exemptions (in some jurisdictions) provide costly exits. Theater ratio (0.55): Moderate. The constraint has genuine functional content (disease control works; vaccination is effective), but performance elements exist: public messaging emphasizes collective benefit over individual risk, media coverage inflates disease severity (during mpox, for instance), and institutional statements often blur disease-severity boundaries. As mission creep accelerates, theater increases — by t=10, theater ratio approaches piton levels (0.55).
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between disease-control-authority and unvaccinated-individual. The authority experiences the constraint as coordination (Rope) — solving a collective-action problem, preventing outbreak cascades, protecting vulnerable populations. The individual experiences the constraint as extraction (Snare for high-severity, Tangled Rope for low-severity). The gap reveals the structural source: the authority bears zero suppression cost (legal and social machinery exists; compliance is automatic for most people) while the coerced individual bears all suppression cost. The proportionality reading creates a diagnostic tool: if extractiveness rises while disease severity remains constant (flu vaccine mandates for all workers despite stable CFR), institutional extraction is revealed. The piton perspective (legacy quarantine law) shows that coercive authority persists through inertial legal machinery, decoupled from contemporary disease severity — the law exists because historical plagues created it, not because current diseases justify it. The false-summit mountain perspective reveals the temptation to naturalize the proportionality frame itself as an immutable epidemiological law, when it is actually a contingent institutional choice among three competing readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's structural position relative to this specific constraint. Disease-control-authority is a net beneficiary (low d → negative f(d) → negative effective extraction from their perspective); they experience the constraint as coordination. Unvaccinated individuals at high-severity threshold are net victims (high d → high f(d) → high experienced extraction); they experience snare. The affluent vaccine-hesitant individual has partial exit via arbitrage (medium d → medium f(d) → tangled rope experience). Public health institution benefits institutionally (low institutional d) but faces principal-agent risk from individual-level victims (creating a secondary high d for the institution-as-accountable-actor). The proportionality reading's core structural feature is that d varies by disease severity — a high-R0/high-CFR pathogen justifies high d (treats unvaccinated as legitimate victim), while low-severity pathogen should produce lower d (treats unvaccinated as having stronger autonomy claim). Mission creep occurs when institutions apply high-d logic to low-severity pathogens, inflating experienced extraction beyond what disease severity warrants.
 *
 * MANDATROPHY ANALYSIS:
 *   Extractiveness = 0.52, below the 0.70 threshold, so mandatrophy resolution is not required. However, the rising trajectory (0.35 → 0.52) creates a secondary mandatrophy risk: as suppression and theater increase while disease severity remains stable (for low-severity pathogens), the constraint approaches snare-territory (ε > 0.66, suppression > 0.60). At that point, institutional authority would be sustaining coercive mandates for low-severity pathogens without proportionality justification — a structural shift from tangled_rope (mixed coordination + extraction) to snare (pure extraction with suppression). The constraint resolves mandatrophy by explicitly anchoring coercion legitimacy to disease severity: measles justifies, flu does not. If the institution extends high-severity coercion to low-severity pathogens, it abandons the proportionality frame and the classification shifts to public_health_primary (a different constraint file).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disease_severity_threshold_ambiguity,
    'Where is the legitimate threshold for proportionality? Below what R0/CFR threshold does coercion become unjustified?',
    'Comparative analysis across jurisdictions of mandate policies correlated with pathogen metrics (R0, CFR, hospitalization rate, duration); cost-benefit analysis of coercion harms vs disease harms across severity spectrum',
    'If threshold is empirically determined by harms analysis: extractiveness drops (ε ≈ 0.35-0.40); snares become tangled ropes become ropes. If threshold is politically negotiated: extractiveness rises (ε ≈ 0.60-0.70); institutional extraction exploits vagueness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disease_severity_threshold_ambiguity, empirical, 'Location of disease-severity threshold justifying coercion under proportionality principle').

omega_variable(
    institutional_mission_creep,
    'Does public health authority expand coercive justification beyond original disease-severity logic (for flu, endemic respiratory viruses, etc.)?',
    'Historical tracking of mandate scope relative to disease severity across 20+ years; analysis of institutional statements and legal briefs; comparison of stated pathogen-severity thresholds to actual mandate decisions',
    'Evidence of mission creep: extractiveness is institutional (ε rises to 0.65+); coercion serves institutional authority preservation, not disease control. No mission creep: extractiveness remains disease-driven (ε ≈ 0.45-0.55).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_mission_creep, empirical, 'Whether public health authority expands coercion beyond disease-severity justification').

omega_variable(
    proportionality_axiom_contestation,
    'Does the proportionality axiom remain philosophically holdable, or has it been effectively superseded by public-health-primary reasoning in practice?',
    'Analysis of constitutional court rulings, legislative statements, professional ethics codes; tracking which axiom frames decisions in high-stakes cases (measles outbreaks, mpox, pandemic emergencies)',
    'If proportionality remains holdable in legal tradition: classification stable. If public-health-primary has overridden proportionality in practice: this reading''s authority erodes; classification shifts toward public_health_primary constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_axiom_contestation, conceptual, 'Whether proportionality axiom remains philosophically live or has been overridden in practice').

omega_variable(
    reading_differentiation_empirical,
    'Can empirical policy data distinguish between proportionality_reading (measles=yes, flu=no) and public_health_primary (both yes) in actual jurisdictional decisions?',
    'Policy audit: does jurisdiction mandate measles vaccination but not flu? Does it provide cost-benefit justification tied to disease severity? Does mandate scope expand when disease severity drops? Do courts uphold measles mandates while striking down flu mandates citing proportionality?',
    'Strong empirical differentiation: readings are distinct structural constraints (two files). Weak differentiation: readings may be covering similar institutional behavior with different normative frames (consider merging).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_differentiation_empirical, empirical, 'Empirical policy divergence between proportionality and public-health-primary readings').

omega_variable(
    bodily_autonomy_foreclosure_condition,
    'What disease parameters would trigger foreclosure of the bodily-autonomy-primary reading? (Under what conditions would autonomy advocates concede that coercion is justified?)',
    'Survey of bodily-autonomy-primary advocates; analysis of their stated limiting conditions; historical cases where autonomy advocates supported coercive intervention',
    'Clear foreclosure condition: bodily_autonomy_primary is not foreclosed by proportionality_reading (both remain coexistent). Unclear/absent condition: bodily autonomy reading lacks internal coherence; if no severity level would justify coercion, the reading forecloses public_health_primary logically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bodily_autonomy_foreclosure_condition, conceptual, 'Conditions under which bodily-autonomy-primary reading would concede coercion justification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coercion_prop_theater_t0, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(coercion_prop_theater_t5, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(coercion_prop_theater_t10, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(coercion_prop_ext_t0, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(coercion_prop_ext_t5, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(coercion_prop_ext_t10, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(coercion_prop_supp_t0, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(coercion_prop_supp_t5, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(coercion_prop_supp_t10, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, medical_mandate_mission_creep).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, pandemic_authority_ratchet).

% DUAL FORMULATION NOTE:
% This is one of three readings of the kernel 'coercion_legitimacy_boundary'. The proportionality_reading occupies the middle ground between bodily_autonomy_primary (coercion never justified) and public_health_primary (coercion justified when collective benefit > individual cost). Each reading is a distinct constraint with different ε, different victim sets, and different institutional implications. The three stories form a kernel family: all three compete for legitimacy in actual policy institutions. The proportionality_reading's structural signature is that disease severity is the determinant variable — ε scales with R0 and CFR; suppression increases as severity increases. The public_health_primary reading would have constant ε regardless of disease severity (coercion is justified whenever public health calculus favors it). The bodily_autonomy_primary reading would have low ε but high suppression of institutional authority (coercion is structurally illegitimate regardless of disease severity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
