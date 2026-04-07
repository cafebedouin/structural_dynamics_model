% ============================================================================
% CONSTRAINT STORY: ncaa_eligibility_rules
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ncaa_eligibility_rules, []).

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
 *   constraint_id: ncaa_eligibility_rules
 *   human_readable: NCAA Eligibility Rules for Student Athletes
 *   domain: economic/labor
 *
 * SUMMARY:
 *   NCAA eligibility rules create a structural constraint on student-athlete
 *   economic participation: athletes are barred from earning compensation
 *   (salary, endorsements, direct payments) in exchange for participating in
 *   college sports, while universities and the NCAA capture billions in
 *   revenue from media rights, ticket sales, and merchandise. The constraint
 *   exhibits the full spectrum of Deferential Realism classifications
 *   depending on observer position. For trapped athletes with no alternative
 *   income pathways, eligibility rules function as pure extraction (Snare) —
 *   they are unpaid labor generators with no exit options. For universities
 *   and the NCAA, the same rules function as coordination (Rope) — they
 *   standardize competition, prevent cost spiraling, and create predictable
 *   media markets. For the broader amateur athletics ideology, the constraint
 *   functions as degraded theater (Piton) — the amateurism principle persists
 *   through institutional inertia despite empirical irrelevance (professional
 *   infrastructure pervades college sports). The constraint is undergoing
 *   systematic reform through organized pressure (athlete unions, state
 *   legislation, NIL legalization post-2021), creating a Scaffold dynamic:
 *   sunset mechanisms are built in through generational transition. The
 *   extractiveness score (0.58) reflects moderate-high extraction: athletes
 *   lose significant earning potential (estimated 50-80% of market wage in
 *   revenue sports) but receive offsetting scholarship benefits that provide
 *   partial value, especially for non-elite athletes. The suppression score
 *   (0.72) reflects high barriers to exit: athletes cannot transfer without
 *   penalty, cannot earn endorsements (historically), and face
 *   legal/contractual enforcement of eligibility rules. Theater ratio (0.65)
 *   reflects that amateurism justification is increasingly performative —
 *   professional structures are pervasive, yet the amateur label persists.
 *
 * KEY AGENTS:
 *   - Student Athletes (Revenue-Generating Sports): Primary victims (powerless/trapped) — bear full extraction via labor value suppression; highest extractiveness experience
 *   - Non-Elite Student Athletes: Secondary victims (moderate/constrained) — constrained exits via scholarship dependency; moderate extraction offset by educational access
 *   - NCAA Organization: Primary beneficiary (institutional/arbitrage) — rule-setter and revenue capture point; $1B+ annual extraction from media rights
 *   - University Athletic Departments: Secondary beneficiaries (institutional/arbitrage) — capture ticket sales, merchandise, and training infrastructure value; extract via athlete labor suppression
 *   - Media Broadcasters: Tertiary beneficiaries (institutional/arbitrage) — profit from athlete performance without direct compensation to performers
 *   - Athlete Advocacy Coalition: Organized counterforce (organized/constrained) — dismantling constraint through state legislation, NIL rights, transfer portal reforms; scaffold logic applies
 *   - Amateur Athletics Ideology: Institutional fiction (institutional/arbitrage) — maintains amateurism framing despite professional reality; piton classification derives from theater persistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ncaa_eligibility_rules, 0.58).
domain_priors:suppression_score(ncaa_eligibility_rules, 0.72).
domain_priors:theater_ratio(ncaa_eligibility_rules, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ncaa_eligibility_rules, extractiveness, 0.58).
narrative_ontology:constraint_metric(ncaa_eligibility_rules, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ncaa_eligibility_rules, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ncaa_eligibility_rules, snare).
narrative_ontology:human_readable(ncaa_eligibility_rules, "NCAA Eligibility Rules for Student Athletes").
narrative_ontology:topic_domain(ncaa_eligibility_rules, "economic/labor").

domain_priors:requires_active_enforcement(ncaa_eligibility_rules).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ncaa_eligibility_rules, ncaa_organization).
narrative_ontology:constraint_beneficiary(ncaa_eligibility_rules, university_athletic_departments).
narrative_ontology:constraint_beneficiary(ncaa_eligibility_rules, media_broadcasters).
narrative_ontology:constraint_victim(ncaa_eligibility_rules, student_athletes).
narrative_ontology:constraint_victim(ncaa_eligibility_rules, non_elite_athletes).
narrative_ontology:constraint_victim(ncaa_eligibility_rules, athlete_earning_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STUDENT ATHLETE (SNARE) — Structurally trapped. Cannot receive direct compensation for athletic performance, endorsements, or name/image/likeness (historically; partially relaxed post-2021). Cannot transfer schools without penalty or sitting out seasons. Cannot negotiate terms of participation. Bears full cost of amateurism fiction while universities and NCAA extract billions in revenue. Zero degrees of freedom — exit options are foreclosed by NCAA enforcement and institutional gatekeeping.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNIVERSITY ATHLETIC DEPARTMENT (ROPE) — Experiences constraint as coordination mechanism. Eligibility rules standardize competition, enabling predictable scheduling, media contracts, and conference alignment. Benefits from amateurism fiction (reduces compensation costs) while capturing revenue from ticket sales, merchandise, and media rights. Net beneficiary with high exit capacity — can lobby NCAA for rule changes, negotiate media deals, adjust recruiting strategies. Effective extraction runs toward this actor.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: NCAA ORGANIZATION (ROPE) — Primary beneficiary and rule-setter. Eligibility rules serve a genuine coordination function: standardize competition, prevent cost-spiraling arms races, maintain amateurism fiction that legitimizes the enterprise. Also the primary extractor: NCAA captures ~$1B+ annually in tournament media rights (March Madness, bowl games) that flow from the eligibility restriction. High exit capacity — maintains rules through governance authority. Net beneficiary with maximal agency.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NON-ELITE ATHLETES (SNARE) — Majority of student-athletes. Receive scholarship benefits (tuition, housing) but are barred from earning endorsement income, participating in professional leagues concurrently, or monetizing their own athletic performance. Constrained exits: can leave sports entirely (end scholarship), transfer (NCAA penalties), or attempt professional path (forfeit college eligibility). Unlike elite athletes with endorsement opportunities (post-2021), non-elite athletes have no alternative income pathway. Moderate power through collective organization (athlete unions emerging post-2023), but current enforcement heavily asymmetric.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ATHLETE ADVOCACY COALITION (SCAFFOLD) — Organized agents (athlete unions, legal advocates, state legislatures) are dismantling the amateurism constraint through sunset mechanisms: NIL rights (Name/Image/Likeness, post-2021), transfer portal reforms (2023+), and compensation law changes (California Fair Pay to Play Act, state-level legislation). These mechanisms are creating alternative pathways that bypass NCAA gatekeeping. Theater ratio declining as formal compensation replaces amateurism fiction. Constraint retains enforcement power but is being systematically undermined by organized pressure. Sunset logic applies: reform maturity estimated 5-10 years as state legislation harmonizes and athlete power consolidates.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: AMATEUR ATHLETICS IDEOLOGY (PITON) — The philosophical principle that college athletics should remain 'amateur' — driven by 19th-century Oxbridge amateurism tradition and 20th-century class anxiety (distinguishing amateur from professional). Largely performative: professional infrastructure pervades college sports (coaching salaries $5M+, training facilities, media apparatus), yet the amateurism label persists through institutional inertia. Athletes are not 'amateurs' — they are unpaid professionals. The ideology maintains itself through rhetorical repetition (NCAA repeatedly affirms commitment to amateurism while revenue grows) rather than functional necessity. Theater ratio 0.65 reflects this performative gap: the constraint's stated purpose (preserve amateur character) contradicts its structural reality (professional extraction).
constraint_indexing:constraint_classification(ncaa_eligibility_rules, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a generational/global perspective, NCAA eligibility rules exhibit both genuine coordination (standardized competition enabling media markets) and significant asymmetric extraction (student-athletes barred from earning market value while universities/NCAA profit). The constraint requires active enforcement (NCAA rules compliance offices, transfer restrictions, eligibility certification). Beneficiaries (NCAA, universities) benefit from coordination; victims (athletes) bear extraction. Suppression is high (legal barriers, contract enforcement) but not absolute (NIL reform is reducing it). Classification: Tangled Rope — hybrid coordination-extraction with organized counterforce reducing suppression over time.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ncaa_eligibility_rules_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ncaa_eligibility_rules, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ncaa_eligibility_rules, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ncaa_eligibility_rules, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ncaa_eligibility_rules, TR),
    TR >= 0.70.

:- end_tests(ncaa_eligibility_rules_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Student-athletes in revenue-generating sports (football, basketball) generate estimated $1B+ in value (broadcast media rights, ticket sales, merchandise) while receiving $15K-50K annual compensation via scholarship. The market wage for comparable labor (professional minor leagues, international systems) suggests 50-80% of revenue flows to athletes in market systems. Under NCAA restrictions, athletes capture 0-10% of their generated value. However, extractiveness is not at snare maximum (0.70+) because scholarship benefits provide genuine offset value, especially for non-elite athletes without alternative income sources. Non-revenue sport athletes receive less extraction (educational access is their primary benefit). Average ε = 0.58 reflects the constraint's mixed severity. Suppression (0.72): High. NCAA enforcement mechanisms are substantial: eligibility certification offices, transfer portal restrictions, loss of eligibility for rule violations, legal contracts binding athletes. Alternative pathways have been systematically foreclosed: NIL restrictions (until 2021), agent contact restrictions, concurrent professional league participation restrictions. However, suppression is not absolute (0.85+) because athlete unions and state legislation are creating new pathways. Current trajectory shows suppression declining (post-2021 NIL legalization, transfer portal loosening) but still substantially enforced. Theater ratio (0.65): Moderate-high. The amateurism justification is performative: NCAA budgets $1B+, coaches earn $5M+, training facilities rival professional standards, media apparatus is professional-grade. Yet the amateurism label persists through repeated rhetorical affirmation despite structural irrelevance. Theater has declined over the measurement interval (from 0.78 to 0.65) as NIL legalization and state compensation law have made the amateurism fiction harder to maintain. Continued decline expected as reform matures.
 *
 * PERSPECTIVAL GAP:
 *   Trapped student-athletes in revenue sports see extraction (Snare): they are prevented from monetizing their own performance while universities profit. NCAA and universities see coordination (Rope): eligibility rules serve genuine functions (standardize competition, enable media markets, prevent cost spiraling). Non-elite athletes see mixed effects (Tangled Rope): they benefit from educational access but face substantial labor suppression. Athlete advocacy coalition sees temporary constraint (Scaffold): reform is underway through state legislation and NIL legalization, with a sunset arc of 5-10 years. Amateur athletics ideology sees only its own fiction (Piton): the amateurism principle persists through institutional momentum despite having lost functional relevance. Analytical observer sees hybrid coordination-extraction (Tangled Rope): genuine coordination function (standardized competition enabling media) combined with asymmetric extraction (athlete labor suppression), with organized counterforce degrading suppression over time. The perspectival gap is driven by differential exit options: beneficiaries (NCAA, universities) have high exit capacity (can modify rules, adjust revenue models); victims (athletes, especially non-elite) have low exit capacity (cannot leave without losing scholarships, cannot monetize performance).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from agent position in the extraction flow. Student-athletes are victims with trapped exits (high d → high f(d) → high experienced extractiveness). Non-elite athletes are victims with constrained exits (moderate d → moderate f(d) → moderate experienced extractiveness). NCAA and universities are beneficiaries with arbitrage exits (low d → low/negative f(d) → negative experienced extractiveness, i.e., they benefit). Athlete advocacy coalition has organized status with constrained exits (moderate d → moderate f(d), but their organizing activity is shifting the constraint toward Scaffold, reducing effective suppression). The analytical observer (analytical power, analytical exits) occupies a neutral structural position from which they can assess the asymmetry without bearing it themselves (d ≈ 0.72, moderate experience of the constraint as structural observation). The engine derives d automatically from beneficiary/victim declarations and exit options — student-athletes are marked as victims (trapped), NCAA/universities as beneficiaries (arbitrage), and the mathematics produces the differential χ values that correspond to each perspective's classification.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint does not exhibit mandatrophy because the analytical observer correctly identifies it as Tangled Rope (hybrid coordination-extraction), not as pure extraction (Snare) being labeled coordination, nor as coordination (Rope) being mislabeled extraction. The coordination function is real: eligibility rules do standardize competition and enable the media business model. The extraction is also real: athletes bear suppressed labor value. The tangled_rope classification is mandatrophy-resolved because it accounts for both functions explicitly. The constraint requires active enforcement (NCAA eligibility offices, transfer restrictions, legal contracts), beneficiaries are present (NCAA, universities, media), victims are present (student-athletes), and the hybrid nature is structural, not observational. The Scaffold perspective is not a mandatrophy escape — it is a legitimate perspectival reading from the vantage of organized counterforce (athlete unions, state legislation) that is dismantling the constraint through sunset mechanisms. No false natural law is being invoked (unlike the amateur athletics ideology, which is piton, not mountain). The classification hierarchy is: Snare (trapped athletes), Tangled Rope (analytical view), Rope (beneficiary view), Scaffold (organized counterforce view), Piton (amateurism ideology view). This hierarchy is coherent and non-mandatrophic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    college_education_value_fungibility,
    'Is the scholarship benefit (tuition + housing) a genuine coordination value or a concealment mechanism for extraction?',
    'Compare athlete lifetime earnings (with vs without scholarship) to market wages for similarly skilled workers; calculate opportunity cost of training time vs educational attainment; survey athlete perception of value equivalence',
    'If genuine value: constraint is partial extraction with offsetting benefit (lower base ε). If concealment: constraint is pure extraction with theater (higher ε, higher theater ratio). Current evidence suggests partial offset — elite athletes at revenue-generating sports see net extraction; non-elite athletes with genuine educational access see partial benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(college_education_value_fungibility, empirical, 'Whether scholarship benefits offset extraction of labor value').

omega_variable(
    market_rate_discrepancy_magnitude,
    'What is the true market wage for student-athlete labor (forgone NIL rights, training time, public performance value)?',
    'Comparative analysis: comparable professional athletes in minor leagues, international student-athlete markets (European club systems), or hypothetical athlete labor market if NCAA restrictions removed',
    'If market gap > 70% of revenue: extraction is near-maximal (supports ε > 0.65). If gap < 40%: extraction is moderate (supports ε ≈ 0.40). Current estimates suggest gap of 50-80% for revenue-generating sports.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_rate_discrepancy_magnitude, empirical, 'True market wage for student-athlete labor vs NCAA compensation').

omega_variable(
    reform_sustainability_threshold,
    'At what level of NIL legalization and state compensation law does NCAA amateurism enforcement collapse?',
    'Track adoption rates of state NIL/compensation laws, athlete use of alternative income pathways (post-2021), NCAA rule compliance decline, institutional defection from NCAA governance',
    'If collapse occurs before 2030: scaffold sunset is real, constraint transitions to rope/piton by 2035. If NIL remains marketable but NCAA rules persist: hybrid state emerges (partial extraction, high theater). Current trajectory suggests partial sunset — elite athlete exceptions expanding, non-elite athletes still constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_sustainability_threshold, empirical, 'Threshold for NCAA amateurism enforcement collapse').

omega_variable(
    non_revenue_sport_constraint_justification,
    'Do eligibility rules serve a coordination function for non-revenue sports (swimming, volleyball, wrestling) distinct from their extraction role in revenue sports?',
    'Comparative analysis of non-revenue vs revenue sport athlete experience; survey whether non-revenue athletes perceive constraint as coordination vs extraction; assess whether eligibility rules could be relaxed for non-revenue sports without compromising competitive integrity',
    'If coordination function significant for non-revenue: constraint is partially justified as rope for those athletes. If extractive everywhere: constraint is snare across all sports with no legitimate coordination cover. Evidence suggests mixed: non-revenue athletes see minor coordination benefit but still face suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_revenue_sport_constraint_justification, empirical, 'Whether non-revenue sports justify amateurism rules').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ncaa_eligibility_rules, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ncaa_tr_t0, ncaa_eligibility_rules, theater_ratio, 0, 0.78).
narrative_ontology:measurement(ncaa_tr_t20, ncaa_eligibility_rules, theater_ratio, 20, 0.72).
narrative_ontology:measurement(ncaa_tr_t40, ncaa_eligibility_rules, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(ncaa_be_t0, ncaa_eligibility_rules, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ncaa_be_t20, ncaa_eligibility_rules, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(ncaa_be_t40, ncaa_eligibility_rules, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ncaa_eligibility_rules, resource_allocation).
narrative_ontology:affects_constraint(ncaa_eligibility_rules, college_athlete_transfer_restrictions).
narrative_ontology:affects_constraint(ncaa_eligibility_rules, nil_endorsement_gatekeeping).

% DUAL FORMULATION NOTE:
% NCAA eligibility rules form a constraint cluster with two downstream constraints: (1) transfer restrictions (student-athlete mobility suppression), (2) NIL endorsement gatekeeping (pre-2021 historical, now partially reformed). All three share the same beneficiary (NCAA/universities) but operate through different suppression mechanisms. Transfer restrictions suppress athlete bargaining power; NIL gatekeeping suppresses income pathways; eligibility rules suppress market entry entirely. Decomposition necessary because each has distinct reform trajectories and ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ncaa_eligibility_rules, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
