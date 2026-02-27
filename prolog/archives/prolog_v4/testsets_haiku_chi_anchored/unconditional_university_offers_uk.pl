% ============================================================================
% CONSTRAINT STORY: unconditional_university_offers_uk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_university_offers_uk, []).

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
 *   constraint_id: unconditional_university_offers_uk
 *   human_readable: Use of Unconditional Offers in UK University Admissions
 *   domain: economic/social/education
 *
 * SUMMARY:
 *   Following the removal of student number caps in 2015, UK universities
 *   transitioned from a regulated admissions system with conditional offers
 *   (based on predicted grades and final exam results) to a more competitive
 *   deregulated market. In this environment, high-ranking universities began
 *   issuing 'unconditional offers' — admissions decisions made months before
 *   A-level exams were taken, with no explicit performance condition. This
 *   constraint captures the structural tension between two mechanisms:
 *   universities solving a coordination problem (securing enrollment
 *   commitments in a demand-uncertain market) and extracting information
 *   value and commitment optionality from prospective students. The
 *   constraint exhibits the full diagnostic range of Deferential Realism
 *   classification depending on the observer's position: the prospective
 *   student experiences a snare (trapped by sunk emotional commitment and
 *   information asymmetry), the lower-ranked university experiences tangled
 *   rope (both coordinating on enrollment stabilization and being extracted
 *   from in market competition), the high-ranking university experiences rope
 *   (solving coordination without perceived extraction), the regulator
 *   experiences scaffold (a temporary market failure with a sunset via
 *   transparency and enforcement), the admissions system itself exhibits
 *   piton characteristics (performative meritocracy maintained through
 *   inertia), and the analytical observer sees tangled rope (genuine
 *   coordination function + asymmetric extraction).
 *
 * KEY AGENTS:
 *   - Prospective Students: Primary victim (powerless/trapped) — face information asymmetry and sunk emotional commitment; cannot exit without social penalty
 *   - High-Ranking Universities: Primary beneficiary (institutional/arbitrage) — capture enrollment commitments early, enjoy prestige advantages, can issue offers confidently
 *   - Lower-Ranked Universities: Secondary victim/participant (moderate/constrained) — forced to adopt unconditional offers to compete; experience market share losses to prestige competition
 *   - University Marketing and Admissions Departments: Secondary beneficiary (institutional/arbitrage) — gain by converting uncertain demand into early committed enrollment; reduce recruitment risk
 *   - Educational Meritocracy (abstract): Victim (powerless/trapped) — undermined by unconditional offers decoupling admission from demonstrated achievement
 *   - Regulatory Bodies (Office for Students, Russell Group): Organized actors (organized/constrained) — see market failure; attempting sunset via transparency and conditional offer codes
 *   - University Admissions System: Institutional theater (institutional/arbitrage) — maintains performative meritocracy narrative while operating as market-driven mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_university_offers_uk, 0.58).
domain_priors:suppression_score(unconditional_university_offers_uk, 0.62).
domain_priors:theater_ratio(unconditional_university_offers_uk, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_university_offers_uk, extractiveness, 0.58).
narrative_ontology:constraint_metric(unconditional_university_offers_uk, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unconditional_university_offers_uk, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_university_offers_uk, tangled_rope).
narrative_ontology:human_readable(unconditional_university_offers_uk, "Use of Unconditional Offers in UK University Admissions").
narrative_ontology:topic_domain(unconditional_university_offers_uk, "economic/social/education").

domain_priors:requires_active_enforcement(unconditional_university_offers_uk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_university_offers_uk, high_ranking_universities).
narrative_ontology:constraint_beneficiary(unconditional_university_offers_uk, university_marketing_departments).
narrative_ontology:constraint_victim(unconditional_university_offers_uk, prospective_students_information_asymmetry).
narrative_ontology:constraint_victim(unconditional_university_offers_uk, lower_ranked_universities).
narrative_ontology:constraint_victim(unconditional_university_offers_uk, educational_meritocracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROSPECTIVE STUDENT (SNARE) — Receives unconditional offer early, creating sunk emotional commitment before final exam results. Cannot meaningfully exit: rejecting an unconditional offer signals ingratitude to university; holding multiple unconditional offers creates social pressure to 'choose.' Trapped by information asymmetry (universities know acceptance rates; students don't). d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOWER-RANKED UNIVERSITY (TANGLED ROPE) — Constrained by competition for student numbers; must adopt unconditional offers to remain recruitment-competitive, yet this accelerates student poaching by higher-ranked institutions. Experiences both coordination (all universities adopt similar tactics to stabilize enrollment) and extraction (market share lost to offer wars). d≈0.70, f(d)≈1.05, σ=0.9 → χ≈0.60.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HIGH-RANKING UNIVERSITY (ROPE) — Benefits from first-mover advantage in unconditional offer arms race; can issue offers confidently knowing students will accept due to prestige signaling. Experiences the constraint as coordination: unconditional offers solve the problem of securing enrollment commitments in a deregulated market. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EDUCATION REGULATOR / ADVISORY BODIES (SCAFFOLD) — Organized institutions (Ofsted, university regulators, student unions) see unconditional offers as a temporary market failure with a sunset: proposed reforms include conditional offer requirements, transparency mandates, and fair admissions codes. The Office for Students has signaled concerns; Russell Group universities have issued voluntary guidance limiting unconditional offers. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.18. Low effective extraction because regulatory bodies have agency and enforcement mechanisms.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: UNIVERSITY ADMISSIONS THEATER (PITON) — The unconditional offer system maintains performative aspects of meritocratic selection (conditional on exam results) while actually operating via early strategic signaling and market positioning. The ritual of conditional offers — students earn admission by achieving grades — persists in language and process, but unconditional offers bypass this theater entirely. theater_ratio≈0.68 reflects the gap between the meritocratic narrative and the market-driven reality. Institutional inertia maintains the dual system.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a systemic view, unconditional offers serve both a genuine coordination function (resolving commitment and matching problems in a deregulated market) and an extraction function (universities extract information value and commitment from students before results, capturing upside while shifting downside risk). The constraint exhibits classic tangled rope structure: coordination (solving for enrollment stability) + asymmetric extraction (universities gain without merit loss, students lose optionality). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.43.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_university_offers_uk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unconditional_university_offers_uk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unconditional_university_offers_uk, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_university_offers_uk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unconditional_university_offers_uk, TR),
    TR >= 0.70.

:- end_tests(unconditional_university_offers_uk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Universities extract information value (binding students before they can shop offers comparatively) and commitment optionality (students lose the ability to renegotiate after results). This is substantial but not total — students who decline have exit routes (other universities, alternative paths), and universities don't have a monopoly on talent. The rise in extractiveness from 0.25 (2015, post-cap removal but pre-unconditional-offer adoption) to 0.58 (2021, peak market adoption) reflects the widening of the practice. Suppression (0.62): Moderate-high. Information asymmetry is substantial: universities know historical acceptance rates and enrollment targets; students do not. Social norms make declining an offer costly (signals ungratefulness). But suppression is not total — some universities have published transparency, student guides exist, and regulatory pressure is increasing. Theater ratio (0.68): High. The unconditional offer system maintains the theater of meritocratic selection ('based on your predicted grades and potential') while actually operating via early strategic signaling. The gap between the narrative (merit-based) and the reality (market-based) has widened over the interval as unconditional offers have become standard practice. Claimed type: Tangled rope. The constraint has both a genuine coordination function (universities solving for enrollment demand uncertainty in a deregulated market) and asymmetric extraction (students lose optionality and information advantage). Requires active enforcement: Yes — the coordination function would be maintained even with conditional-only offers; the extraction is what requires active maintenance (resistance to transparency, regulatory pushback against conditional-offer mandates).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range. The prospective student sees a snare: trapped by sunk commitment before learning final grades, facing information asymmetry, unable to exit without social penalty. The high-ranking university sees rope: solving the legitimate coordination problem of matching uncertain demand to available places, experiencing the system as a fair way to secure commitment. The lower-ranked university sees tangled rope: the system is both enabling (they can participate in the market through unconditional offers) and extractive (they lose students to the prestige ranking effect in the offer arms race). The regulator sees scaffold: a temporary market failure (unconditional offers as a deregulation artifact) with a sunset via transparency mandates and conditional-offer codes. The university admissions system exhibits piton characteristics: the performative ritual of meritocratic selection persists in language and process (conditional on meeting grades) while the actual mechanism has shifted to market signaling. The analytical observer sees tangled rope: both coordination (solving for enrollment stability) and extraction (asymmetric information and commitment) are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Prospective students: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Cannot exit the market (university education is seen as necessary), sunk commitment creates lock-in, information asymmetry creates structural disadvantage. High-ranking universities: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Minimal extraction (negative chi means net beneficiary). Can choose not to issue unconditional offers (institutional arbitrage option), prestige gives them natural advantage without extraction. Lower-ranked universities: Victim + constrained → d≈0.70, f(d)≈1.05. Significant extraction. Constrained by competitive pressure to match high-ranking university offer strategy; cannot arbitrage out (they lose enrollment if they don't compete). Educational meritocracy: Victim + trapped → d≈0.90, f(d)≈1.35. Abstract collective that cannot organize or exit; the system structurally undermines the merit principle. Regulators: Organized + constrained → d≈0.35, f(d)≈0.32. Low effective extraction. Have enforcement power (institutional leverage) but constrained by university autonomy concerns and political economy (universities are major employers and research funders). Analytical observer: Constrained by deregulation structure and market incentives, but not fully trapped → d≈0.55, f(d)≈0.75. Mixed extraction reflecting both coordination and market failure components.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via perspectival decomposition. The apparent paradox is: 'Are unconditional offers a coordination mechanism (Rope) solving for enrollment uncertainty, or an extraction mechanism (Snare) exploiting student information asymmetry?' The resolution is that they are BOTH structurally, from different observation points. From the university's perspective (especially high-ranking), the system is Rope — universities are coordinating on a mechanism to match demand to supply in an uncertain market. From the prospective student's perspective, the system is Snare — they are trapped by sunk emotional commitment and information disadvantage. The tangled rope classification from the analytical perspective captures that BOTH are structurally real: the coordination function is genuine (unconditional offers do solve for enrollment commitment), and the extraction is also genuine (universities gain optionality and information value that students lose). The deregulation (2015 cap removal) created conditions where universities could use unconditional offers as a coordination mechanism, but the structure of the mechanism creates asymmetric information and sunk commitment effects that manifest as extraction. The mandatrophy is resolved by recognizing that 'is this coordination or extraction?' is not a binary property of the system, but a perspectival property: from the beneficiary's view, it's coordination; from the victim's view, it's extraction; from the analytical view integrating both, it's tangled rope. Extractiveness > 0.70 threshold: The 0.58 value does not trigger the > 0.70 gate, so the mandatrophy resolution requirement is satisfied by the structural analysis rather than by additional empirical data collection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    student_welfare_threshold,
    'At what point does the psychological commitment lock-in of an unconditional offer cross from coordination benefit (early certainty) to extraction harm (loss of optionality and bargaining power)?',
    'Longitudinal student surveys measuring satisfaction, regret, and counterfactual willingness-to-switch; analysis of student withdrawal rates post-unconditional offer',
    'If early certainty benefit > commitment lock-in cost: unconditional offers are coordination (Rope). If lock-in cost > early certainty benefit: unconditional offers are extraction (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(student_welfare_threshold, empirical, 'Whether student welfare gains from early certainty exceed losses from commitment lock-in').

omega_variable(
    information_asymmetry_measurability,
    'Can universities be required to disclose acceptance rates, conditional offer rates, and enrollment targets such that information asymmetry becomes symmetric?',
    'Regulatory transparency mandates; measurement of acceptance rate knowledge before/after disclosure requirements; impact on offer acceptance decisions',
    'If transparency eliminates asymmetry: constraint downgrades to Rope (coordination only). If universities conceal or mislead: constraint remains Snare (extraction via opacity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_measurability, empirical, 'Whether transparency disclosure can eliminate university-student information asymmetry').

omega_variable(
    regulatory_enforcement_capacity,
    'Does the Office for Students (and equivalent regulators) have sufficient enforcement power to compel conditional offer reversion, or are reforms merely advisory?',
    'Audit of regulatory penalties for violations; correlation between regulatory guidance and actual admission practice changes; interview data on university compliance motivation',
    'If enforcement is strong: scaffold sunset is real, constraint will degrade to Rope. If advisory only: regulatory mechanisms are piton theater, constraint persists as Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_capacity, empirical, 'Whether regulatory bodies can enforce conditional offer requirements').

omega_variable(
    deregulation_causality,
    'Would unconditional offers have proliferated absent the 2015 student number cap removal, or are they endemic to competitive university systems?',
    'Comparative analysis of unconditional offer rates in regulated vs deregulated higher education markets (US, Australia, EU jurisdictions with and without caps); time-series analysis of UK offer practices before/after 2015',
    'If endemic: constraint is Mountain (inherent to competition). If deregulation-specific: constraint is contingent policy artifact (Tangled Rope/Snare/Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deregulation_causality, empirical, 'Whether unconditional offers are caused by deregulation or endemic to competitive markets').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_university_offers_uk, 2015, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_university_offers_uk, theater_ratio, 0, 0.35).
narrative_ontology:measurement(unco_tr_t3, unconditional_university_offers_uk, theater_ratio, 3, 0.52).
narrative_ontology:measurement(unco_tr_t6, unconditional_university_offers_uk, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_university_offers_uk, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(unco_be_t3, unconditional_university_offers_uk, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(unco_be_t6, unconditional_university_offers_uk, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_university_offers_uk, resource_allocation).
narrative_ontology:affects_constraint(unconditional_university_offers_uk, student_debt_asymmetry_uk).
narrative_ontology:affects_constraint(unconditional_university_offers_uk, university_prestige_hierarchy_lock_in).

% DUAL FORMULATION NOTE:
% The unconditional offer constraint is downstream of the deregulation decision (2015 cap removal) and feeds into broader student debt and prestige system effects. The constraint represents a distinct structural phenomenon: universities solving for enrollment demand via early commitment mechanisms, which creates secondary effects in student welfare and educational meritocracy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_university_offers_uk, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
