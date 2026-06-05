% ============================================================================
% CONSTRAINT STORY: coffee_cardiovascular_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coffee_cardiovascular_2026, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: coffee_cardiovascular_2026
 *   human_readable: The Caffeine Paradox Realignment
 *   domain: medical/cardiology/public_health
 *
 * SUMMARY:
 *   For decades, cardiologists have cautioned patients with atrial
 *   fibrillation (AF) against coffee consumption, based on the mechanistic
 *   plausibility that caffeine (an adenosine antagonist) increases heart rate
 *   and blood pressure — potentially triggering arrhythmias. However, large
 *   prospective cohort studies and recent meta-analyses have revealed a
 *   paradox: heavy coffee drinkers actually have lower AF risk than light or
 *   non-drinkers, and no RCT has demonstrated that coffee increases AF
 *   incidence in the general population. This reversal exposes a classic
 *   constraint structure: decades of risk-averse medical guidance persist
 *   despite mounting evidence of their invalidity. The constraint now
 *   functions as a tangled rope — it coordinates simplified,
 *   liability-reducing clinical practice (benefiting the medical
 *   establishment and risk-averse physicians) while suppressing patient
 *   autonomy and informed choice among AF patients. The theater ratio has
 *   risen as the guidance has become increasingly performative: physicians
 *   continue cautioning against coffee not because the evidence supports it,
 *   but because the traditional heuristic persists through institutional
 *   inertia and liability concerns. The constraint is being challenged by
 *   organized evidence-based medicine advocates pushing for guideline
 *   revision, creating a scaffold-like sunset scenario where the old
 *   restriction is expected to fade as new guidelines adopt individualized
 *   risk assessment. The analytical observer risks naturalizing the
 *   mechanistic constraint (adenosine antagonism is real pharmacology)
 *   without recognizing that the institutional restriction is a contingent
 *   policy choice, not a natural law.
 *
 * KEY AGENTS:
 *   - AF Patients: Primary victim (powerless/trapped) — trapped between sacrificing a daily habit and violating medical advice; no exit pathway
 *   - Primary Care Physicians / Internists: Secondary actors (moderate/constrained) — constrained by liability and standard-of-care expectations; benefit from simple heuristics
 *   - Cardiology Establishment: Primary beneficiary (institutional/arbitrage) — benefits from coordinating narrative that simplifies guidelines and reduces liability
 *   - Evidence-Based Medicine Coalition: Organized beneficiary (organized/constrained) — pushing for guideline revision; sees constraint as temporary with sunset
 *   - Academic Cardiologists: Powerful actors (powerful/mobile) — experience constraint as both coordination (simplified practice) and extraction (credibility cost of maintaining weak guidance)
 *   - Risk-Averse Clinical Tradition: Institutional beneficiary (institutional/arbitrage) — maintains performative coffee restriction through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks conflating mechanistic truth (caffeine does increase HR/BP) with institutional policy (blanket avoidance)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coffee_cardiovascular_2026, 0.38).
domain_priors:suppression_score(coffee_cardiovascular_2026, 0.62).
domain_priors:theater_ratio(coffee_cardiovascular_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coffee_cardiovascular_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(coffee_cardiovascular_2026, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(coffee_cardiovascular_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coffee_cardiovascular_2026, tangled_rope).
narrative_ontology:human_readable(coffee_cardiovascular_2026, "The Caffeine Paradox Realignment").
narrative_ontology:topic_domain(coffee_cardiovascular_2026, "medical/cardiology/public_health").

domain_priors:requires_active_enforcement(coffee_cardiovascular_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coffee_cardiovascular_2026, conventional_cardiology_establishment).
narrative_ontology:constraint_beneficiary(coffee_cardiovascular_2026, risk_averse_physicians).
narrative_ontology:constraint_victim(coffee_cardiovascular_2026, coffee_consuming_patients).
narrative_ontology:constraint_victim(coffee_cardiovascular_2026, patient_autonomy_and_informed_choice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ATRIAL FIBRILLATION PATIENT (SNARE) — Receives decades of cautionary guidance based on plausible mechanism but contradicted by recent epidemiology. Trapped: must either accept the restriction (sacrificing daily habit and social participation) or violate medical advice (bearing psychological cost and uncertainty). No exit pathway; bears full cost of the constraint. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE INTERNIST/GP (TANGLED ROPE) — Constrained by liability, standard-of-care expectations, and institutional inertia. Also benefits from simple heuristics (blanket avoidance easier than nuanced risk communication). The constraint coordinates risk-averse practice patterns (beneficial for system stability) while extracting effort from patient autonomy and physician-patient trust. d≈0.72, f(d)≈1.12, σ=1.0 → χ≈0.43.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE CARDIOLOGY ESTABLISHMENT (ROPE) — Benefits from a coordinating narrative (coffee avoidance is prudent) that simplifies clinical decision-making and reduces liability exposure. The constraint functions as institutional coordination: it maintains uniform practice guidelines and reduces the cognitive load of differential risk assessment. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.04. Net beneficiary; sees the constraint as protective coordination.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE EVIDENCE-BASED MEDICINE COALITION (SCAFFOLD) — Organized institutions (EBM researchers, meta-analysts, guideline-revision bodies) recognize the paradox: new epidemiological evidence contradicts old mechanistic assumptions. These groups are actively building alternative pathways (revised guidelines, shared decision-making tools, risk stratification by CHA₂DS₂-VASc score) with a clear sunset: as evidence synthesis matures and guidelines update, the old blanket restriction loses institutional force. The coalition sees the current constraint as temporary, solvable by rigorous evidence review and guideline modernization. d≈0.38, f(d)≈0.38, σ=1.0 → χ≈0.14. Low effective extraction; the coalition has agency and sees a clear exit.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE RISK-AVERSE CLINICAL TRADITION (PITON) — The coffee restriction persists in clinical practice, medical textbooks, and patient education materials largely through institutional inertia despite declining empirical support. The ritual of cautioning against coffee is performative: it signals physician care and diligence without requiring engagement with the actual epidemiological literature. Theater_ratio=0.68 reflects this performativity. The medical establishment maintains the guidance partly because alternatives (nuanced risk communication, individualized decision-making) require higher cognitive and communicative overhead. The constraint is degraded — maintained by tradition, not function.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE CARDIOLOGIST SPECIALIST (TANGLED ROPE) — Powerful actors (academic cardiologists, guideline authors) experience the constraint as both coordination and extraction. Coordination: blanket guidance coordinates practice across skill levels and geographies, simplifying liability and standard-of-care expectations. Extraction: the guideline also extracts credibility and authority from specialists — they must maintain the public-facing guidance even as private literature reviews and collaborative discussions reveal its weakness. They have mobile exit options (publish rebuttal evidence, propose guideline revision) but face reputational and institutional costs. d≈0.45, f(d)≈0.45, σ=1.0 → χ≈0.17.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER / MECHANISTIC VIEW (MOUNTAIN) — From a pharmacological-first-principles perspective, caffeine is an adenosine antagonist that increases heart rate and blood pressure acutely. This mechanism IS real, and some vulnerable populations (severe AF, uncontrolled hypertension) do face genuine risks. The observer might frame coffee restriction as a natural law of pharmacology: adenosine antagonism → cardiac stress in susceptible patients. However, the structural data (ε=0.38, suppression=0.62, theater=0.68) contradicts a mountain classification. The mechanism is real but not sufficient to generate a universal restriction — the constraint is sustained by institutional inertia and risk-averse traditions, not by irreducible physiology.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coffee_cardiovascular_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coffee_cardiovascular_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coffee_cardiovascular_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(coffee_cardiovascular_2026, TR),
    TR >= 0.70.

:- end_tests(coffee_cardiovascular_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts patient autonomy and informed choice for decades, but the extraction is not as severe as a snare because many patients simply ignore the guidance and consume coffee anyway. The AF population is not uniformly trapped — many are empowered by finding contradictory information online. The extractiveness reflects the institutional capacity to suppress optimal choice, not complete behavioral control. Suppression (0.62): Moderate-high. Suppression mechanisms include physician authority, liability concerns, institutional guidelines, and patient uncertainty about contradictory information. However, suppression is incomplete — patients can and do access epidemiological evidence; major medical journals have published pro-coffee findings; online patient communities discuss the paradox. Theater ratio (0.68): High and rising. The caution against coffee is increasingly performative — it signals physician diligence and risk-aversion without requiring engagement with the actual evidence literature. Cardiologists often privately acknowledge the epidemiological evidence while publicly maintaining the traditional guidance due to institutional inertia and liability concerns. The measurement trajectory shows theater rising from 0.52 to 0.68 over 30 years as the evidence accumulated but the guidance remained fixed.
 *
 * PERSPECTIVAL GAP:
 *   The constraint displays the full perspectival range. The AF patient sees a snare: trapped between competing costs with no good exit. The internist sees tangled rope: genuine coordination function (simplified liability-reducing practice) coupled with extraction of patient autonomy. The cardiologist sees rope: coordination of field-wide practice, no extraction from their perspective. The EBM coalition sees a temporary scaffold: evidence synthesis and guideline revision will sunset the old restriction within 5-10 years. The clinical tradition sees piton: the coffee restriction persists through ritual and inertia despite declining function. The specialist sees tangled rope: benefits from coordination (field alignment) but pays credibility costs for maintaining weak guidance. The analytical observer risks seeing a mountain: caffeine's cardiac effects are real pharmacology. However, the structural metrics (ε=0.38, not ≤0.25) contradict a mountain classification, revealing this as false naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   AF patients: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction; cannot exit the constraint without costs. Internists: Victim + constrained (by liability/guidelines) but also beneficiary (simplified heuristics) → d≈0.72, f(d)≈1.12. Mixed experience; classified as tangled rope. Cardiology establishment: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; sees constraint as coordinating institutional practice. EBM coalition: Organized + constrained by institutional resistance but with mobile exit (evidence publication, guideline advocacy) → d≈0.38, f(d)≈0.38. Low effective extraction relative to power; the coalition has agency. Cardiologists: Powerful actors with mobile exit → d≈0.45, f(d)≈0.45. Experience constraint as mixed (coordination benefit + credibility cost). Risk-averse tradition: Institutional beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification derives from theater gate (0.68 ≥ 0.70), not from directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION STRATEGY: The mandatrophy is resolved by recognizing that the constraint operates across multiple institutional and individual scales with different classification outcomes at each scale. At the individual patient level, the constraint is a snare: the AF patient is powerless and trapped, bearing extraction of autonomy while receiving no coordination benefit. At the institutional level (cardiology practice), the constraint functions as a rope: it coordinates field-wide practice and simplifies liability. At the epistemic level (evidence-based medicine), the constraint is recognized as a scaffold degrading in real time: organized actors are building evidence-based alternatives (individualized risk assessment, shared decision-making) that are visibly replacing the old blanket restriction. The analytical observer's temptation to naturalize the constraint (caffeine mechanisms are real) is a false summit test: the mechanism is real, but the institutional restriction is not a necessary consequence of the mechanism — many low-risk AF patients could safely consume moderate coffee. The ensemble classification (tangled rope as claimed_type) captures the primary structure: this is a hybrid coordination-extraction constraint sustained by institutional inertia but actively being replaced by evidence-based alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dose_response_threshold,
    'Does a clinically meaningful dose-response relationship exist for caffeine and AF risk, or is the relationship non-monotonic or negligible above a certain threshold?',
    'Meta-analysis of dose-specific RCT and observational data; dose-response modeling; stratification by habitual consumption level',
    'If threshold exists and is high (>400mg/day): most patients can safely consume typical coffee. If relationship is linear and low threshold: restriction justified. If non-monotonic (U-shaped): moderate consumption may be cardioprotective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dose_response_threshold, empirical, 'Dose-response relationship between caffeine and atrial fibrillation').

omega_variable(
    confounding_by_habitual_use,
    'How much of the protective epidemiological association (heavy coffee drinkers have lower AF risk) reflects reverse causation (AF patients avoid coffee) vs true protective effect vs unmeasured confounders?',
    'Analysis of temporal precedence in cohort studies; instrumental variable analysis; sensitivity analysis for unmeasured confounding',
    'If fully explained by reverse causation: restriction is justified for all patients. If protective effect is real: restriction is harmful, constraint inverts to snare against coffee consumption. If confounded: true effect size is unknown.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confounding_by_habitual_use, empirical, 'Whether epidemiological coffee-protection signal is confounded or causal').

omega_variable(
    mechanism_specificity,
    'Does the mechanistic risk (adenosine antagonism → cardiac stress) apply to all AF etiologies equally, or are certain arrhythmia subtypes or patient phenotypes genuinely more sensitive?',
    'Mechanistic studies stratified by AF type (paroxysmal vs persistent); genetic markers of caffeine sensitivity; electrophysiology studies',
    'If mechanism is universal: blanket restriction justified. If specific subtypes are sensitive: personalized guidance becomes defensible (and constraints become snare-like only for sensitive subgroup). If no dose-response in most patients: restriction is unnecessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_specificity, empirical, 'Whether mechanistic risk applies universally or to specific AF subtypes').

omega_variable(
    guideline_revision_pace,
    'Will major cardiovascular guideline bodies (ACC/AHA, ESC) update coffee recommendations within 5 years to reflect new evidence?',
    'Timeline tracking of guideline revision cycles; publication of systematic reviews; feedback from guideline authors',
    'If revision occurs: scaffold timeline is real, constraint will degrade via formal deprioritization. If no revision: institutional inertia sustains constraint despite evidence, confirming piton classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(guideline_revision_pace, preference, 'Timeline for cardiovascular guideline revision on coffee consumption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coffee_cardiovascular_2026, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coffee_tr_t0, coffee_cardiovascular_2026, theater_ratio, 0, 0.52).
narrative_ontology:measurement(coffee_tr_t15, coffee_cardiovascular_2026, theater_ratio, 15, 0.68).
narrative_ontology:measurement(coffee_tr_t30, coffee_cardiovascular_2026, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(coffee_be_t0, coffee_cardiovascular_2026, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(coffee_be_t15, coffee_cardiovascular_2026, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(coffee_be_t30, coffee_cardiovascular_2026, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coffee_cardiovascular_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(coffee_cardiovascular_2026, arrhythmia_treatment_guideline_lag).
narrative_ontology:affects_constraint(coffee_cardiovascular_2026, patient_autonomy_in_cardiology).

% DUAL FORMULATION NOTE:
% The caffeine paradox decomposes into two related but distinct constraints: (1) the empirical question of whether coffee causes AF (ε ≈ 0.08, increasingly mountain as evidence accumulates), and (2) the institutional question of whether medical guidelines reflect current evidence (ε ≈ 0.38, tangled rope). This story focuses on the institutional constraint. The empirical constraint (does coffee cause AF?) would be a separate mountain story with low ε and low suppression, deriving its institutional force entirely from downstream institutional lag.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coffee_cardiovascular_2026, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
