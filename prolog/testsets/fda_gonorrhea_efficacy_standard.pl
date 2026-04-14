% ============================================================================
% CONSTRAINT STORY: fda_gonorrhea_efficacy_standard
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fda_gonorrhea_efficacy_standard, []).

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
 *   constraint_id: fda_gonorrhea_efficacy_standard
 *   human_readable: FDA Unified Efficacy Standard for Gonorrhea Treatment
 *   domain: pharmaceutical/regulatory/public_health
 *
 * SUMMARY:
 *   The FDA efficacy standard for new oral gonorrhea antibiotics exemplifies
 *   a regulatory constraint that performs both protective coordination and
 *   extractive market gatekeeping. The constraint emerged from legitimate
 *   concerns about drug safety and resistance management, but has evolved
 *   into a mechanism that blocks potentially beneficial treatments while
 *   maintaining market advantage for established manufacturers. The Advisory
 *   Committee's rejection of a candidate oral antibiotic reflects a 95%
 *   efficacy threshold that may not reflect current epidemiological realities
 *   of fluoroquinolone resistance. For patients infected with resistant
 *   strains, the standard creates a structural trap: existing drugs fail, and
 *   the regulatory barrier prevents access to novel options. For researchers
 *   and smaller manufacturers, the barrier extracts through delayed market
 *   entry and increased development costs. For public health authorities and
 *   the established pharmaceutical industry, the standard performs
 *   coordination by preventing degradation of antibiotic stewardship. The
 *   constraint's theater ratio (0.65) reflects that the FDA deliberation
 *   appears scientifically rigorous while operating on outdated efficacy
 *   thresholds, creating substantial performative content. The extractiveness
 *   trajectory (0.35→0.58 over 10 years) shows increasing extraction as
 *   resistance rates accelerated and the efficacy standard's protective
 *   function degraded while its gatekeeping function strengthened.
 *
 * KEY AGENTS:
 *   - Treatment-Resistant Patient: Primary victim (powerless/trapped) — faces failed existing treatments and FDA-blocked alternatives; no structural exit
 *   - Antibiotic Researcher/Developer: Secondary victim (moderate/constrained) — benefits from coordination function but bears extraction through regulatory delay and high development costs
 *   - Established Pharmaceutical Manufacturer: Primary beneficiary (institutional/arbitrage) — protected market position through regulatory moat; can arbitrage across jurisdictions
 *   - Public Health Authority: Organized actor (organized/mobile) — sees temporary coordination problem with alternative pathways (diagnostics, vaccines) as exit routes
 *   - FDA Advisory Panel: Institutional actor (institutional/constrained) — maintains performative efficacy standard; sees own process as degraded but constrained to follow inherited precedent
 *   - Analytical Observer: Neutral analyst (analytical/analytical) — observes genuine dual function (protective coordination + extractive gatekeeping) that resists reduction to single type
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fda_gonorrhea_efficacy_standard, 0.58).
domain_priors:suppression_score(fda_gonorrhea_efficacy_standard, 0.62).
domain_priors:theater_ratio(fda_gonorrhea_efficacy_standard, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fda_gonorrhea_efficacy_standard, extractiveness, 0.58).
narrative_ontology:constraint_metric(fda_gonorrhea_efficacy_standard, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fda_gonorrhea_efficacy_standard, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fda_gonorrhea_efficacy_standard, tangled_rope).
narrative_ontology:human_readable(fda_gonorrhea_efficacy_standard, "FDA Unified Efficacy Standard for Gonorrhea Treatment").
narrative_ontology:topic_domain(fda_gonorrhea_efficacy_standard, "pharmaceutical/regulatory/public_health").

domain_priors:requires_active_enforcement(fda_gonorrhea_efficacy_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fda_gonorrhea_efficacy_standard, established_drug_manufacturers).
narrative_ontology:constraint_beneficiary(fda_gonorrhea_efficacy_standard, clinical_trial_infrastructure).
narrative_ontology:constraint_victim(fda_gonorrhea_efficacy_standard, antibiotic_resistant_patients).
narrative_ontology:constraint_victim(fda_gonorrhea_efficacy_standard, drug_development_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TREATMENT-RESISTANT PATIENT (SNARE) — Patient infected with antibiotic-resistant gonorrhea has no exit: existing drugs fail, FDA standard blocks novel oral option. Trapped between failed treatment and regulatory barrier. Maximum experienced extraction — no alternatives available within constraint.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ANTIBIOTIC RESEARCHER (TANGLED ROPE) — Benefits from coordination function (FDA efficacy standard prevents race-to-bottom in drug safety). Simultaneously bears extraction cost: high regulatory barrier delays market entry, reduces investment attractiveness, creates 10+ year development timeline. Constrained exit — can develop drugs but faces unified efficacy gate controlling market access.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ESTABLISHED MANUFACTURER (ROPE) — Sees efficacy standard as coordination mechanism protecting market position. High regulatory barrier creates moat: competitors must invest billions to clear FDA hurdle. Net beneficiary — extraction runs toward this actor. Can arbitrage between jurisdictions (approve elsewhere first, pressure FDA later).
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH AUTHORITY (SCAFFOLD) — Sees efficacy standard as temporary coordination problem with sunset. Advanced diagnostics (rapid resistance testing), combination therapies, and vaccine development pathways are emerging alternatives that will eventually bypass the antibiotic bottleneck. Standard has built-in review mechanism (5-year efficacy review cycles) creating explicit sunset logic. Mobile exit through alternative intervention pathways.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: FDA ADVISORY PANEL (PITON) — The specific efficacy standard applied to gonorrhea oral antibiotics is substantially performative. Panel lacks current resistance surveillance data at time of voting; standard is calibrated to preclinical efficacy thresholds developed for earlier-generation drugs; voting process performs scientific rigor while underlying data has degraded relevance. Theater ratio (0.65) reflects that the panel's deliberation appears rigorous but the informational basis is outdated. Maintained through institutional inertia rather than current functional verification.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global view, the efficacy standard performs both coordination (prevents unsafe drugs) and extraction (blocks needed treatments). Unlike a pure mountain (immutable scientific law), the standard is a constructed institutional rule. Unlike pure rope, it creates genuine harm by delaying access. The dual function (real coordination + real extraction) and active enforcement (FDA committee voting) make this legitimately tangled.
constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fda_gonorrhea_efficacy_standard_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fda_gonorrhea_efficacy_standard, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fda_gonorrhea_efficacy_standard, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fda_gonorrhea_efficacy_standard, TR),
    TR >= 0.70.

:- end_tests(fda_gonorrhea_efficacy_standard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The efficacy standard blocks market access for new oral antibiotics, benefiting established manufacturers and maintaining high development costs that deter entry. However, the extraction is not maximal because: (1) the standard has a stated protective function (drug safety and resistance management) that is not entirely pretextual, and (2) alternative interventions exist (IV cephalosporins, although increasingly failing). Suppression (0.62): Moderate-high. Barriers include the 95% efficacy threshold itself, high trial costs ($500M+ for antibiotic development), lengthy review timelines (10+ years), and lack of incentive structures (antibiotic profits are low relative to other drug classes). But suppression is not total — drugs can eventually clear the standard; the pathway is constrained rather than entirely closed. Theater ratio (0.65): Moderate-high. The FDA Advisory Committee's voting process appears rigorous (expert review, evidence presentation), but the underlying efficacy threshold is outdated, calibrated to older resistance landscapes. The deliberation performs scientific authority while operating on stale data.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiary (established manufacturer) experiences the standard as protective coordination — it prevents generic competition and maintains stewardship. Victim (resistant patient) experiences it as pure extraction — the regulatory barrier blocks needed treatment. Researcher experiences it as tangled rope — the standard provides coordination (ensures safety) but extracts through delayed market entry. The gap reflects genuine structural differences: each agent's exit options, power level, and position in the extraction flow are different. The standard is not 'really' any single type; it is legitimately tangled across perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (established manufacturers) have institutional power and arbitrage options — they can lobby for standard maintenance or shift to other therapeutic areas. Their directionality (d) is low (~0.20), yielding negative effective extractiveness from their perspective. Victims (resistant patients) have powerless status and trapped exit — no alternatives within or outside the constraint. Their d is high (~0.90), yielding maximum experienced extraction. Researchers occupy the middle: moderate power, constrained exit (can develop drugs but face high barrier). Their d is moderate (~0.55), consistent with tangled rope classification. The suppression value (0.62) is not scaled by these directionality parameters — suppression is a raw structural property of the constraint itself, reflecting the objective barriers to market entry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by clarifying the dual mechanism: the efficacy standard performs legitimate coordination (prevents unsafe drugs from reaching market) while simultaneously extracting value (blocks competition, maintains prices, delays access to beneficial treatments for resistant infections). This is the definition of tangled rope — genuine coordination function PLUS asymmetric extraction. The classification cannot be reduced to 'really just a snare' or 'really just a rope' because both the protective function and the extractive mechanism are structurally real. The ambiguity is not resolvable by choosing a measurement basis — it reflects genuine institutional hybridity. Mandatrophy is resolved by accepting that the standard is legitimately tangled, not by denying either the protective or extractive component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resistance_trajectory_certainty,
    'What is the true current rate of fluoroquinolone-resistant N. gonorrhoeae, and does it exceed the threshold at which an oral antibiotic with 90% efficacy becomes a net public health benefit despite lower efficacy than IV cephalosporin standard?',
    'National surveillance data (CDC GISP program) with resistance rates stratified by region and risk population; prospective comparison of treatment outcomes with oral 90%-efficacy drug vs. fluoroquinolone-resistant strains in realistic patient populations',
    'If resistance rate > 30% in target populations: the efficacy standard is extractive (blocks beneficial treatment). If resistance rate < 15%: standard may be defensibly protective. Resistance trajectory also matters — accelerating resistance changes the cost-benefit calculus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_trajectory_certainty, empirical, 'Current fluoroquinolone resistance rate and acceleration trajectory').

omega_variable(
    efficacy_threshold_derivation,
    'Was the 95% efficacy threshold for new oral gonorrhea antibiotics derived from current epidemiological data, or inherited from older standards for different drug classes and resistance landscapes?',
    'Document review of FDA guidance documents and historical efficacy standard precedents; comparison to 2022-2025 resistance surveillance data and treatment failure rates under current standard',
    'If inherited from older standards: threshold is a piton (performative, maintained by inertia) and extraction occurs because standard is not calibrated to current conditions. If newly derived: standard is legitimately coordinating. This determines whether the regulation is tangled rope (mixed) or pure snare (blocking needed treatment).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_threshold_derivation, empirical, 'Whether efficacy threshold reflects current epidemiology or historical precedent').

omega_variable(
    alternative_pathway_viability,
    'What is the realistic timeline for alternative gonorrhea interventions (rapid diagnostics, combination therapies, vaccines) to reduce dependence on this specific oral antibiotic pathway?',
    'Clinical pipeline review; analysis of CDC/WHO resistance containment strategies; assessment of vaccine development progress (e.g., meningococcal vaccine cross-reactivity)',
    'If alternatives mature within 5-7 years: scaffold perspective is valid (temporary barrier with sunset). If alternatives are 15+ years away: scaffold classification is aspirational rather than structural, and the constraint becomes more purely extractive. Determines whether mandatrophy is resolved (real sunset) or deferred (appearance of exit without functional exit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_pathway_viability, empirical, 'Timeline for alternative intervention pathways to mature').

omega_variable(
    treatment_failure_attribution,
    'When an oral antibiotic candidate fails to meet the 95% efficacy threshold in trial, is the failure primarily due to pharmacological inadequacy, trial design sensitivity, or evolving resistance in the trial population during enrollment?',
    'Re-analysis of failed trial datasets; mechanism-of-action studies; correlation of failure with strain resistance patterns in enrolled patients',
    'If failures are primarily pharmacological: standard is protective coordination. If failures are trial design artifacts or resistance dynamics: standard blocks viable treatments and is extractive. Determines whether the regulation serves its stated safety function or performs coordination while achieving extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treatment_failure_attribution, empirical, 'Root cause of efficacy threshold failures in clinical trials').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fda_gonorrhea_efficacy_standard, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gonor_tr_t0, fda_gonorrhea_efficacy_standard, theater_ratio, 0, 0.45).
narrative_ontology:measurement(gonor_tr_t5, fda_gonorrhea_efficacy_standard, theater_ratio, 5, 0.58).
narrative_ontology:measurement(gonor_tr_t10, fda_gonorrhea_efficacy_standard, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(gonor_be_t0, fda_gonorrhea_efficacy_standard, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gonor_be_t5, fda_gonorrhea_efficacy_standard, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(gonor_be_t10, fda_gonorrhea_efficacy_standard, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fda_gonorrhea_efficacy_standard, enforcement_mechanism).
narrative_ontology:affects_constraint(fda_gonorrhea_efficacy_standard, antibiotic_stewardship_incentive).
narrative_ontology:affects_constraint(fda_gonorrhea_efficacy_standard, rare_disease_drug_development).
narrative_ontology:affects_constraint(fda_gonorrhea_efficacy_standard, drug_pricing_power_asymmetry).

% DUAL FORMULATION NOTE:
% The efficacy standard constraint is upstream of multiple downstream constraints: it affects antibiotic stewardship policy (by controlling which drugs can be prescribed), drug development incentives (by raising entry costs), and pricing power asymmetries (by maintaining market concentration). Each downstream constraint has its own extractiveness value reflecting domain-specific factors, but all are structurally influenced by this efficacy standard's gate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
