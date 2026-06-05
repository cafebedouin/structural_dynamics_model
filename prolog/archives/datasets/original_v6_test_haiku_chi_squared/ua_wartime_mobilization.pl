% ============================================================================
% CONSTRAINT STORY: ua_wartime_mobilization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ua_wartime_mobilization, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ua_wartime_mobilization
 *   human_readable: Wartime Mobilization Law in Ukraine
 *   domain: political/military
 *
 * SUMMARY:
 *   Ukraine's wartime mobilization law (enacted 2022, expanded 2024-2025)
 *   represents a state constraint that oscillates between legitimacy
 *   (necessary coordination for existential defense) and extraction
 *   (systematic suppression of exit options and asymmetric burden
 *   distribution). The constraint embodies the classic wartime dilemma:
 *   conscription is rationally necessary for national defense against
 *   invasion, yet the structural suppression of exit options (legal penalties
 *   for draft evasion, border controls, occupational conscription) produces a
 *   Snare from the perspective of the conscripted population. The
 *   extractiveness has increased over 24 months (0.55 → 0.72) as the conflict
 *   persists and conscription scope expands (gender-neutral eligibility,
 *   lower age limits, broader occupational conscription). Theater ratio
 *   remains low (0.35) because the enforcement is genuinely functional — the
 *   state faces existential military threat, and mobilization produces real
 *   military capacity, not merely performative activity. However, the
 *   presence of systematic elite exemptions (Tangled Rope perspective)
 *   combined with powerless-cohort suppression (Snare perspective) reveals
 *   that the law functions both as coordination mechanism (existential
 *   defense) AND as extraction mechanism (burden disproportionately borne by
 *   non-elite conscripts). The central analytical tension is whether wartime
 *   exigency legitimates this extraction structure, or whether the expanding
 *   scope and increasing extractiveness signal drift toward institutionalized
 *   conscription infrastructure maintained beyond wartime necessity.
 *
 * KEY AGENTS:
 *   - Conscripted Male Citizens: Primary victims (powerless/trapped) — bear full extraction; exit options systematically closed by law and enforcement
 *   - Draft-Eligible Population (Expanded Cohorts): Secondary victims (moderate/constrained) — face constrained exit; may include women and gender-diverse citizens in expanded conscription; bear significant extraction with more capacity than powerless cohort
 *   - Ukrainian Elite / Oligarchs: Secondary beneficiary (powerful/mobile) — experience mobilization as mixed coordination and extraction; have genuine exit options via wealth; face lower personal risk through exemptions and deferments
 *   - Ukrainian State Defense Apparatus: Primary beneficiary (institutional/arbitrage) — coordinates national defense; extracts labor and life value from conscripted population; benefits from collective victory
 *   - International Humanitarian Organizations: Oversight actors (organized/constrained) — view law as temporary scaffold with humanitarian constraints; provide external monitoring and constraint on state behavior
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — assesses whether wartime exigency legitimates the extraction structure or whether expanding scope indicates drift toward institutionalized conscription
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ua_wartime_mobilization, 0.72).
domain_priors:suppression_score(ua_wartime_mobilization, 0.78).
domain_priors:theater_ratio(ua_wartime_mobilization, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ua_wartime_mobilization, extractiveness, 0.72).
narrative_ontology:constraint_metric(ua_wartime_mobilization, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(ua_wartime_mobilization, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ua_wartime_mobilization, snare).
narrative_ontology:human_readable(ua_wartime_mobilization, "Wartime Mobilization Law in Ukraine").
narrative_ontology:topic_domain(ua_wartime_mobilization, "political/military").

domain_priors:requires_active_enforcement(ua_wartime_mobilization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ua_wartime_mobilization, ukrainian_state_defense_apparatus).
narrative_ontology:constraint_victim(ua_wartime_mobilization, conscripted_male_citizens).
narrative_ontology:constraint_victim(ua_wartime_mobilization, draft_eligible_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSCRIPTED MALE CITIZEN (SNARE) — Trapped by law and nation-state enforcement. Exit options systematically closed: crossing borders without permission is illegal; draft evasion incurs legal penalties and social stigma; refusal is grounds for arrest. The constraint extracts full labor value and life risk. d≈0.98, f(d)≈1.42, σ=1.0 → χ≈1.02.
constraint_indexing:constraint_classification(ua_wartime_mobilization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DRAFT-ELIGIBLE POPULATION WITH EXPANDED CONSCRIPTION (SNARE) — As of 2024-2025, Ukraine expanded conscription eligibility beyond traditional male-only cohorts to women and gender-diverse citizens in certain roles. This broader cohort faces constrained exit: legal emigration is difficult under wartime restrictions; work deferments are narrowly applied; essential occupations have narrow carve-outs. d≈0.92, f(d)≈1.35, σ=1.0 → χ≈0.97.
constraint_indexing:constraint_classification(ua_wartime_mobilization, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UKRAINIAN ELITE / OLIGARCHS (TANGLED ROPE) — Wealthier citizens have genuine exit options (emigration, secured positions in support roles, legal deferrals for essential economic roles). They experience mobilization law as a coordination mechanism (sharing national defense burden) AND asymmetric extraction (leveraging wealth to minimize personal risk). Benefits from state victory; bears lower cost through elite exemptions. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.22.
constraint_indexing:constraint_classification(ua_wartime_mobilization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: UKRAINIAN STATE DEFENSE APPARATUS (ROPE) — The state enacts mobilization law to coordinate national defense: conscription pools human resources for existential defense against invasion. From the state's perspective, this is fundamentally a coordination mechanism — aligning individual incentives with collective survival. The law is functional (not primarily extractive) because state victory benefits all citizens including conscripts. d≈0.02, f(d)≈-0.14, σ=1.0 → χ≈-0.10.
constraint_indexing:constraint_classification(ua_wartime_mobilization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL HUMANITARIAN & OVERSIGHT ACTORS (SCAFFOLD) — International bodies (ICRC, UNHCR, UN Human Rights Monitoring Mission) view mobilization law as a temporary wartime measure with necessary sunset logic: conscription intensity should decline as military advantage stabilizes or conflict ends. The law is enforceable with humanitarian constraints (welfare of conscripts, medical exemptions). d≈0.55, f(d)≈0.75, σ=1.1 → χ≈0.48. Theater ratio remains low (0.35) — enforcement is functional, not purely performative, because state faces existential threat.
constraint_indexing:constraint_classification(ua_wartime_mobilization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (SNARE) — From a global historical perspective, wartime conscription during existential defense is a legitimate state power. However, the structural data (ε=0.72, suppression=0.78, theater=0.35, beneficiary=state, victims=conscripts) classifies this as Snare, not as a natural law or justifiable extraction. The analytical perspective sees this as snare rather than mountain because the extraction is contingent on the war's continuation — it is not an immutable structural feature of Ukrainian society. Post-war, the conscription infrastructure must be dismantled or consensually reformed. This differs from true natural laws (mathematical limits) or coordinated defenses (mutual agreement to shared burden). The snare classification holds because exit options remain systematically suppressed even for groups (elites) with material capacity to exit.
constraint_indexing:constraint_classification(ua_wartime_mobilization, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ua_wartime_mobilization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ua_wartime_mobilization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ua_wartime_mobilization, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ua_wartime_mobilization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ua_wartime_mobilization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72): High and increasing. The state extracts full labor value, life risk, and bodily autonomy from conscripted population. The increase from 0.55 (initial mobilization) to 0.72 (2024-2025) reflects expansion of conscription scope beyond traditional male cohorts, inclusion of occupational conscription, and persistence of enforcement despite battlefield stabilization. The extractiveness plateau at 0.72 rather than maximum reflects that conscription is legitimated by genuine wartime exigency and international legal frameworks, not purely extractive greed. Suppression (0.78): High. Exit options are systematically closed: legal penalties for evasion; border controls preventing emigration; occupational conscription extends enforcement to previously protected sectors; conscientious objection paths are narrow or absent; refusal is grounds for criminal prosecution. However, suppression is not total (0.95+) because some elite citizens successfully obtain exemptions, and humanitarian space for objection exists within constraints. Theater ratio (0.35): Low. Mobilization enforcement is functionally oriented, not performative. The state genuinely needs military manpower for defense against invasion; conscripts are deployed in combat; enforcement produces measurable military capacity. Low theater indicates that the constraint's primary function is real (coordination/extraction), not theatrical maintenance. The low theater also distinguishes this from Piton classification — the mobilization law is not inertial or vestigial.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap runs between the powerless conscript (Snare: trapped, full extraction) and the state defense apparatus (Rope: coordination, net beneficiary). From the conscript's view, mobilization is pure extraction — they are compelled to risk their life for the state's defense. From the state's view, mobilization is coordination — aligning all citizens behind existential defense. The secondary gap runs between the elite cohort (Tangled Rope: mobile exit, asymmetric burden) and the non-elite conscript (Snare: trapped, symmetric burden). The elite experience both coordination benefits (state victory protects their assets and status) and extraction exemption (they bear lower casualty risk than non-elite cohorts). The international observer (Scaffold perspective) sees the law as temporary emergency measure with necessary sunset — it is functional and necessary now, but requires institutional dismantling post-conflict. The civilizational analyst (Snare perspective) notes that even accepting wartime exigency, the expanding scope and increasing extractiveness signal institutional drift toward conscription permanence, suggesting that wartime exigency is being used to normalize a state apparatus that may persist beyond the exigency itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Conscripted Male Citizens: Victim + trapped → d≈0.98, f(d)≈1.42. Near-maximal extraction. Minimal exit capacity; legal penalties for refusal; compelled bodily service. Draft-Eligible Population (Expanded): Victim + constrained → d≈0.92, f(d)≈1.35. Very high extraction. Constrained exit (emigration is difficult under wartime restrictions; work deferments narrowly applied); compelled service in broader occupational scope than traditional conscription. Ukrainian Elite: Beneficiary + mobile → d≈0.35, f(d)≈0.30. Moderate effective extraction (or moderate benefit). Mobile exit (wealth enables emigration, legal deferrals); beneficiary from state victory and defense of assets; bear lower casualty risk. Ukrainian State Defense: Beneficiary + arbitrage → d≈0.02, f(d)≈-0.14. Net beneficiary. Extracts resources and manpower; benefits from coordinated defense; possesses unilateral enforcement power.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL CASE: Wartime mobilization law resolves mandatrophy by distinguishing JUSTIFIED EXTRACTION from PURE SNARE. The law is snare-classified (high extractiveness, high suppression, powerless cohort), yet the mandatrophy is partially resolved by the wartime exigency: the state faces genuine existential threat from invasion; conscription is rationally necessary for defense; the constraint coordinates collective action for mutual survival. However, the mandatrophy is NOT fully resolved because: (1) The elite cohort experiences the same law as Tangled Rope (mixed coordination and extraction), revealing that the constraint functions differently based on structural position. (2) Extractiveness is increasing (0.55 → 0.72) despite military stabilization, suggesting institutional drift toward conscription perpetuation beyond wartime exigency. (3) The law's scope is expanding (gender-neutral conscription, occupational conscription, lower age limits), indicating institutional normalization rather than temporary emergency. The resolution is that wartime conscription law is CONDITIONALLY JUSTIFIED as a Snare: it is legitimate extraction during the exigency, but the expanding scope and increasing extractiveness require post-conflict demobilization and institutional reform to prevent the wartime apparatus from solidifying into permanent conscription infrastructure. If the state fails to implement credible sunset provisions and demobilization, the law drifts from justified wartime Snare toward illegitimate peacetime Snare-in-perpetuity. Current status (ε=0.72, mandatrophy_resolved=true) reflects that the wartime exigency provides justification, but this justification is temporally bounded and contingent on institutional commitment to demobilization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wartime_exigency_threshold,
    'At what point does a wartime conscription law transition from justified state emergency power (Rope/Scaffold) to sustained extraction (Snare)?',
    'Historical comparison: duration of mobilization laws in other post-WW2 conflicts; empirical measurement of conscript welfare, casualty rates, and economic burden; statistical comparison of burden distribution across socioeconomic groups',
    'If threshold < 2 years continuous warfare: current status as Snare is justified. If threshold > 5 years: analysis must consider whether the law has transitioned to extraction disguised as emergency, suggesting need for democratic reformulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wartime_exigency_threshold, empirical, 'Threshold at which emergency conscription becomes sustained extraction').

omega_variable(
    elite_exemption_legitimacy,
    'Do exemptions for economic elites reflect legitimate essential-services deferments or systematic class-based extraction avoidance?',
    'Audit of deferment applications and approvals by socioeconomic quintile; comparison of casualty rates across income brackets; analysis of criteria for essential-occupation exemptions; tracking of draft-eligible wealthy citizens who fled vs those who served',
    'If elites bear proportional burden: mobilization law approaches Rope classification (shared sacrifice). If casualty rates show 3:1 or higher ratio (conscript:elite), then tangled_rope perspective from elite cohort is accurate — asymmetric extraction confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(elite_exemption_legitimacy, empirical, 'Whether elite exemptions constitute legitimate deferrals or class-based extraction').

omega_variable(
    postwar_demobilization_credibility,
    'Is the mobilization law credibly temporary, with institutional commitment to demobilization after conflict resolution? Or does the wartime apparatus show signs of institutional perpetuation?',
    'Legislative statements on sunset provisions; comparative analysis of WW2 and Cold War demobilization timelines; expert assessment of military doctrine regarding peacetime force structure; tracking of state expansion of conscription scope (age limits, gender, occupational categories) during conflict',
    'If credible sunset with documented demobilization plans: Scaffold classification is accurate. If no sunset provisions and expanding scope: law drifts toward Snare-in-perpetuity, suggesting state use of external threat to normalize conscription infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(postwar_demobilization_credibility, conceptual, 'Whether mobilization law has genuine sunset or perpetuates as permanent state apparatus').

omega_variable(
    conscript_agency_and_refusal_costs,
    'What are the actual enforcement and punishment costs for conscription refusal? Are they calibrated to wartime exigency or do they constitute coercive extraction?',
    'Tracking of legal prosecutions for draft evasion; analysis of punishment severity across jurisdictions; comparison with international norms for wartime conscientious objection; documented cases of refusal and outcomes',
    'If punishments are severe (imprisonment, asset seizure): high suppression (0.78) is confirmed, snare classification holds. If alternative service paths exist with reasonable costs: suppression may be lower than assessed, potentially shifting moderate-cohort perspective toward Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conscript_agency_and_refusal_costs, empirical, 'Actual enforcement severity and refusal costs for conscription').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ua_wartime_mobilization, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uamob_tr_t0, ua_wartime_mobilization, theater_ratio, 0, 0.28).
narrative_ontology:measurement(uamob_tr_t12, ua_wartime_mobilization, theater_ratio, 12, 0.32).
narrative_ontology:measurement(uamob_tr_t24, ua_wartime_mobilization, theater_ratio, 24, 0.35).

% Extraction over time
narrative_ontology:measurement(uamob_be_t0, ua_wartime_mobilization, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(uamob_be_t12, ua_wartime_mobilization, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(uamob_be_t24, ua_wartime_mobilization, base_extractiveness, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ua_wartime_mobilization, enforcement_mechanism).
narrative_ontology:affects_constraint(ua_wartime_mobilization, ukrainian_labor_migration_restrictions).
narrative_ontology:affects_constraint(ua_wartime_mobilization, ukrainian_border_control_wartime).
narrative_ontology:affects_constraint(ua_wartime_mobilization, ukrainian_emergency_economic_controls).

% DUAL FORMULATION NOTE:
% Wartime mobilization law is a high-level constraint that structures multiple downstream constraints: labor migration restrictions enforce conscription scope; border controls prevent draft evasion; emergency economic controls allocate resources to defense. These constraints are linked through the enforcement apparatus of mobilization law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
