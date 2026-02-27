% ============================================================================
% CONSTRAINT STORY: us_vaccine_recommendation_dismantling_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_vaccine_recommendation_dismantling_2026, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_vaccine_recommendation_dismantling_2026
 *   human_readable: Dismantling of National Childhood Vaccine Recommendations
 *   domain: political/social/public_health
 *
 * SUMMARY:
 *   The dismantling of national childhood vaccine recommendations in 2026
 *   represents a structural extraction mechanism disguised as
 *   decentralization reform. A new presidential administration, aligned with
 *   anti-vaccine advocacy networks, has effectively removed the CDC's
 *   authoritative role in recommending vaccination schedules, replacing it
 *   with a fragmented 'state and individual choice' framework. This
 *   dissolution of the unified ACIP schedule creates extractive asymmetry:
 *   anti-vaccine organizations capture the new discretionary space
 *   (arbitrage), while pediatric populations and immunocompromised children
 *   bear the costs of fragmented protection (trapped). The constraint
 *   exhibits all intermediate types depending on observer position — from
 *   pure Snare for the most vulnerable, to Tangled Rope for parents facing
 *   uncertainty, to Rope for the political beneficiary coalition, to Piton
 *   for public health infrastructure attempting to perform outdated functions
 *   under degraded conditions. The theater_ratio trajectory (0.25 → 0.55)
 *   reflects increasing performative content: CDC and state health
 *   departments now routinely disclaim their own guidance as 'one option
 *   among many,' a theatrical equivocation that masks the underlying
 *   extraction. The extractiveness trajectory (0.35 → 0.68) shows
 *   intensifying asymmetry as political capture deepens and outbreak risks
 *   accumulate.
 *
 * KEY AGENTS:
 *   - Pediatric Population: Primary victims (powerless/trapped) — children cannot exit vaccination systems or choose autonomously; bear biological costs of fragmentation
 *   - Immunocompromised Children: Highest-risk victims (powerless/trapped) — life-dependent on herd immunity; most vulnerable to extractive schedule fragmentation
 *   - Parents/Guardians: Secondary victims (moderate/constrained) — face fragmented guidance; nominally gain 'choice' but lose coordinating information; constrained by information asymmetry
 *   - Anti-Vaccine Industry & Political Coalition: Primary beneficiaries (institutional/arbitrage) — capture regulatory discretion; gain market/political advantage; expand ideological footprint
 *   - Public Health Infrastructure (CDC, State Health Departments): Constrained institutional actors (institutional/constrained) — perform degraded functions; maintain outdated recommendations while officially disclaiming them; theater increases
 *   - Organized Pediatric Medicine (AAP, pediatric societies): Organized but suppressed (organized/constrained) — cannot recommend above fragmented schedule without political reputational cost
 *   - International Public Health (WHO, GAVI): Organized external observers (organized/mobile) — maintain parallel frameworks; see US fragmentation as temporary; envision eventual reharmonization
 *   - Epidemiologists & Data Scientists: Analytical observers (analytical/analytical) — track outcomes; see extraction mechanism revealed through disease incidence patterns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_vaccine_recommendation_dismantling_2026, 0.68).
domain_priors:suppression_score(us_vaccine_recommendation_dismantling_2026, 0.72).
domain_priors:theater_ratio(us_vaccine_recommendation_dismantling_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_vaccine_recommendation_dismantling_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_vaccine_recommendation_dismantling_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_vaccine_recommendation_dismantling_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_vaccine_recommendation_dismantling_2026, snare).
narrative_ontology:human_readable(us_vaccine_recommendation_dismantling_2026, "Dismantling of National Childhood Vaccine Recommendations").
narrative_ontology:topic_domain(us_vaccine_recommendation_dismantling_2026, "political/social/public_health").

domain_priors:requires_active_enforcement(us_vaccine_recommendation_dismantling_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_vaccine_recommendation_dismantling_2026, anti_vaccine_industry).
narrative_ontology:constraint_beneficiary(us_vaccine_recommendation_dismantling_2026, political_coalition_skeptics).
narrative_ontology:constraint_beneficiary(us_vaccine_recommendation_dismantling_2026, regulatory_capture_actors).
narrative_ontology:constraint_victim(us_vaccine_recommendation_dismantling_2026, pediatric_population).
narrative_ontology:constraint_victim(us_vaccine_recommendation_dismantling_2026, immunocompromised_children).
narrative_ontology:constraint_victim(us_vaccine_recommendation_dismantling_2026, public_health_infrastructure).
narrative_ontology:constraint_victim(us_vaccine_recommendation_dismantling_2026, herd_immunity_thresholds).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEDIATRIC POPULATION (SNARE) — Children have zero exit capacity from vaccination schedules; cannot choose autonomously; trapped in a system that now offers fragmented, inconsistent protection. d≈0.98, f(d)≈1.43, σ=1.0 → χ≈0.98. Maximum extraction and maximum suppression.
constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IMMUNOCOMPROMISED CHILDREN (SNARE) — Highest-risk subpopulation; life-dependent on herd immunity thresholds; no exit option; bear maximum cost of fragmented schedule. d≈1.00, f(d)≈1.43, σ=1.0 → χ≈0.98. Extraction targets most vulnerable.
constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: PARENTS/GUARDIANS (TANGLED ROPE) — Face fragmented guidance; some benefit from perception of 'choice' (psychological coordination); most face increased decision burden, uncertainty, and health risk. Mix of coercion (fragmentation as suppression) and nominal agency. d≈0.62, f(d)≈0.85, σ=1.0 → χ≈0.57.
constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANTI-VACCINE INDUSTRY & POLITICAL COALITION (ROPE) — Primary beneficiaries. See dismantling as coordination success: aligning political movement, media platforms, and product sales. Extraction is the hidden function masked as coordination. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.07. Negative effective extraction = net beneficiary. High arbitrage capacity.
constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HEALTH INFRASTRUCTURE (PITON) — CDC, state health departments maintain performative compliance with dismantled schedules. Theater ratio=0.55 reflects that official communications now routinely disclaim their own recommendations as 'one option among many' (theatrical equivocation). Infrastructure persists through inertia and legal obligation, not functional capacity. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ORGANIZED PEDIATRIC MEDICINE (SNARE) — AAP, pediatric societies, hospital networks are constrained by administrative pressure and political fragmentation. Cannot recommend above fragmented schedule without professional reputational cost. Organized but suppressed. d≈0.70, f(d)≈1.08, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: INTERNATIONAL PUBLIC HEALTH (SCAFFOLD) — WHO, GAVI, and allied nations maintain parallel recommendation frameworks. US dismantling creates temporary coordination failure (national/international divergence) but also creates pathway for eventual US reintegration if political winds shift. Sees sunset: when disease resurgence or political transition requires reharmonization. d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.24.
constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: EPIDEMIOLOGICAL ANALYSIS (TANGLED ROPE) — From data perspective, the dismantling serves dual function: genuine coordination failure (uncertainty as information) + extraction (political capture overriding public health signal). Measles/pertussis outbreaks will reveal asymmetry. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.62.
constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_vaccine_recommendation_dismantling_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_vaccine_recommendation_dismantling_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_vaccine_recommendation_dismantling_2026, TR),
    TR >= 0.70.

:- end_tests(us_vaccine_recommendation_dismantling_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.68): High. The dismantling creates asymmetric benefit flow: anti-vaccine actors capture discretionary space while pediatric populations bear biological and epidemiological costs. This is not symmetrical decentralization (which would be Rope); it is extractive capture masked as autonomy. The trajectory from 0.35 to 0.68 reflects intensifying asymmetry as regulatory capture deepens. Suppression (0.72): Very high. Multiple suppression mechanisms: (1) Administrative de-emphasis of evidence-based recommendations, (2) Weaponized uncertainty — 'both sides' framing of settled science, (3) Platform/media suppression of counter-messaging, (4) Career suppression — pediatricians face professional pressure for recommending above fragmented schedule, (5) Information asymmetry — parents face manipulated choice environments. Theater Ratio (0.55): Moderate-high and rising. CDC and state health departments maintain official communication channels but increasingly disclaim their own guidance, shifting to theatrical neutrality ('consult your provider'). This is the performative infrastructure Piton signal. The rising trajectory (0.25 → 0.55) reflects Goodhart drift: the infrastructure's stated function (unified recommendation) is replaced by theatrical deference while extraction continues. Claimed Type (Snare): The configuration matches pure extraction: high extractiveness, high suppression, minimal genuine coordination benefit for victims. The political beneficiary coalition (institutional/arbitrage) experiences coordination gains, but these are relative to the extracted population — they do not represent genuine efficiency gains or Pareto improvements.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival inversion along the power axis. From powerless children (trapped), the constraint is pure Snare — no exit, no coordination benefit, maximum cost. From institutional beneficiaries (arbitrage), the constraint is Rope — they experience it as coordinating their political/commercial interests. From public health infrastructure (constrained), it is Piton — the apparatus persists through inertia while its core function (unified recommendation) degrades. From parents (moderate/constrained), it is Tangled Rope — nominal agency (choice) mixed with suppressed information and constrained exit. From international observers (organized/mobile), it is Scaffold with sunset — a temporary policy failure that will eventually reverse when political winds shift or disease resurgence forces re-centralization. The perspectival gap is not ambiguity about what the constraint is; it is clarity that different observers experience legitimately different structures from the same policy. The powerless child has the correct structural reading (Snare). The institutional beneficiary has the convenient reading (Rope). The analytical observer must hold both simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Pediatric Population: Victim + trapped → d≈0.98, f(d)≈1.43. Maximum extraction. Zero autonomy in vaccination decisions; trapped in fragmented system; biologically vulnerable. Immunocompromised Children: Victim + trapped → d≈1.00, f(d)≈1.43. Maximum extraction. Life-dependent on herd immunity; most vulnerable to fragmentation. Parents: Victim + constrained → d≈0.62, f(d)≈0.85. High extraction but nominally constrained by 'choice' rhetoric; constrained exit because refusing all vaccines carries social/reputational cost, as does selective refusal. Anti-Vaccine Coalition: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. High arbitrage capacity (can shift regulatory environment, capture media narratives, commercialize alternatives). Public Health Infrastructure: Victim + constrained → d≈0.65, f(d)≈1.00. Constrained by political environment; cannot recommend above fragmented schedule without administrative retaliation; trapped between legal obligation and political pressure. Pediatric Societies: Victim + constrained → d≈0.70, f(d)≈1.08. Organized but suppressed; institutional members face career risk if they publicly advocate unified schedule over fragmented framework.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED — Extraction > 0.70: This constraint required mandatrophy resolution to rule out the false positive that 'decentralization is coordination.' The decentralization framing is the extraction mechanism's rhetorical cover. Mandatrophy analysis reveals: (1) WHO benefits? Anti-vaccine coalition gains regulatory discretion, media prominence, market access. (2) WHO bears costs? Pediatric populations, immunocompromised children, public health infrastructure. (3) Is there genuine coordination gain? No — unified recommendations were coordination that solved collective action problems (parents had reliable information, pediatricians had consistent guidance, herd immunity thresholds were achievable). Fragmentation destroys that coordination; it does not replace it with superior coordination. Decentralization would be coordination if the fragmenting authority were devolving genuinely to local actors with local knowledge and local accountability (true federalism). Instead, discretion is being captured by national anti-vaccine networks with national reach and no local accountability. This is not federalism; it is national extraction through the appearance of decentralization. (4) Perspectival test: Beneficiary sees Rope. Victims see Snare. No middle ground (Tangled Rope) exists because the extraction is not coupled to genuine coordination — it is parasitic on the coordination framework's dissolution. Consensus classification: Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_outcome_distinction,
    'Is the dismantling intended as genuine decentralization/coordination reform vs. intentional extraction mechanism disguised as choice?',
    'Analysis of policy documents, funding flows to anti-vaccine organizations, deliberate suppression of counter-messaging vs. genuine policy debate fostering',
    'If genuine decentralization: reclassifies as Scaffold. If intentional extraction: confirms Snare. Determines whether political capture vs. federalism is the operative mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_outcome_distinction, empirical, 'Intent distinction: decentralization reform vs. intentional extraction mechanism').

omega_variable(
    outbreak_severity_threshold,
    'What disease resurgence threshold triggers political reversal or emergency re-centralization of recommendations?',
    'Longitudinal tracking of case counts (measles, pertussis, polio) correlated with policy reversals; state-level variation in outcomes',
    'If threshold < 100 deaths/year: system stabilizes as scaffold with sunset. If threshold > 10,000 deaths/year: extraction window extends; classification remains Snare longer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(outbreak_severity_threshold, empirical, 'Disease resurgence threshold for policy reversal').

omega_variable(
    parental_agency_authenticity,
    'Does fragmented schedule actually expand parental autonomy or does it substitute state coercion with market/platform manipulation?',
    'Content analysis of anti-vaccine platforms; A/B testing of parental decision-making under unified vs. fragmented schedules; measurement of informed consent quality',
    'If authentic agency: tangled_rope classification stands (real coordination + real extraction). If market manipulation substitutes for state coercion: reclassifies as pure Snare (extraction unchanged, suppression mechanism altered).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parental_agency_authenticity, empirical, 'Whether fragmentation expands authentic parental autonomy or substitutes platform manipulation').

omega_variable(
    regulatory_capture_depth,
    'Are key ACIP appointees, HHS officials, or state health commissioners directly funded by anti-vaccine organizations or ideologically aligned outside the normal policy process?',
    'Conflict-of-interest disclosures; funding flow analysis; timeline of policy shifts correlated with appointments; comparison of appointment patterns to prior administrations',
    'If capture is deep and systematic: extraction mechanism is regulatory apparatus itself. If capture is shallow: dismantling may partially reflect genuine ideological shift. Affects whether the constraint is ''institutional capture'' (piton) or ''structural extraction'' (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Depth of regulatory capture by anti-vaccine interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_vaccine_recommendation_dismantling_2026, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(baseline_theater, us_vaccine_recommendation_dismantling_2026, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mid_theater_2yr, us_vaccine_recommendation_dismantling_2026, theater_ratio, 2, 0.4).
narrative_ontology:measurement(final_theater_4yr, us_vaccine_recommendation_dismantling_2026, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(baseline_extractiveness, us_vaccine_recommendation_dismantling_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mid_extractiveness_2yr, us_vaccine_recommendation_dismantling_2026, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(final_extractiveness_4yr, us_vaccine_recommendation_dismantling_2026, base_extractiveness, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_vaccine_recommendation_dismantling_2026, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_vaccine_recommendation_dismantling_2026, 0.45).
narrative_ontology:affects_constraint(us_vaccine_recommendation_dismantling_2026, measles_outbreak_2027_2028).
narrative_ontology:affects_constraint(us_vaccine_recommendation_dismantling_2026, pertussis_hospitalization_surge).
narrative_ontology:affects_constraint(us_vaccine_recommendation_dismantling_2026, childhood_mortality_epidemiology).
narrative_ontology:affects_constraint(us_vaccine_recommendation_dismantling_2026, regulatory_capture_public_health).
narrative_ontology:affects_constraint(us_vaccine_recommendation_dismantling_2026, herd_immunity_threshold_failure).

% DUAL FORMULATION NOTE:
% This constraint is downstream of regulatory capture (political coalition formation) but represents a distinct structural mechanism: the weaponization of decentralization rhetoric to dissolve public health coordination. Upstream: political realignment enabling anti-vaccine dominance. This constraint: extraction through recommendation fragmentation. Downstream: disease outbreaks and epidemiological failures reveal the extraction mechanism. All members of this constraint family are linked through intensifying asymmetry over time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_vaccine_recommendation_dismantling_2026, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
