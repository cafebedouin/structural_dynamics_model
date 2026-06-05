% ============================================================================
% CONSTRAINT STORY: ncaa_eligibility_rules
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   domain: economic/labor_regulation
 *
 * SUMMARY:
 *   The NCAA eligibility rules governing student-athlete compensation create
 *   a structural constraint that bundles legitimate coordination functions
 *   (recruiting, athletic development, education integration) with extraction
 *   mechanisms (labor cost suppression, revenue concentration). The
 *   constraint generates approximately $14 billion annually in college sports
 *   revenue while restricting student-athletes' ability to monetize their
 *   labor. The extractiveness has increased from ~0.35 (1980: limited
 *   commercial sports media, regional recruitment) to ~0.58 (2010: national
 *   media contracts, international recruitment, expanded athletic programs).
 *   Theater ratio has risen from ~0.50 to ~0.65 as compliance offices
 *   proliferated and enforcement procedures became more elaborate while
 *   actual rule consistency degraded. The constraint appears as pure
 *   extraction (Snare) to low-income athletes trapped without exit options,
 *   as mixed coordination-extraction (Tangled Rope) to mid-tier athletes with
 *   constrained mobility, as coordination mechanism (Rope) to elite athletes
 *   with professional alternatives, and as theatrical regulatory apparatus
 *   (Piton) to the NCAA itself.
 *
 * KEY AGENTS:
 *   - Low-income student athletes: Primary victim (powerless/trapped) — cannot monetize athletic ability, face scholarship gaps, lack family wealth to offset restrictions
 *   - International student athletes: Primary victim (powerless/trapped) — face additional restrictions on work authorization and eligibility, often dependent on scholarship for access
 *   - Mid-tier athletes: Secondary victim (moderate/constrained) — benefit from scholarships but constrained from earning independent income, can exit only through reduced athletic participation
 *   - Elite recruits with professional prospects: Beneficiary-adjacent (powerful/arbitrage) — can arbitrage into professional leagues or endorsement markets, experience rules as coordination framework
 *   - Universities and athletic departments: Primary beneficiary (institutional/arbitrage) — capture labor cost savings through amateurism rules, concentrate revenue from student-athlete performance
 *   - NCAA administrative apparatus: Primary beneficiary (institutional/arbitrage) — regulates rules to maintain league structure and revenue concentration, benefits from institutional complexity
 *   - Athletes' unions and labor movements: Organized victim (organized/constrained) — attempt to exit through unionization, legal challenges, and transfer portal reform; face enforcement costs and legal barriers
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — sees the constraint as contingent institutional arrangement serving both coordination and extraction simultaneously
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ncaa_eligibility_rules, 0.58).
domain_priors:suppression_score(ncaa_eligibility_rules, 0.68).
domain_priors:theater_ratio(ncaa_eligibility_rules, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ncaa_eligibility_rules, extractiveness, 0.58).
narrative_ontology:constraint_metric(ncaa_eligibility_rules, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ncaa_eligibility_rules, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ncaa_eligibility_rules, tangled_rope).
narrative_ontology:human_readable(ncaa_eligibility_rules, "NCAA Eligibility Rules for Student Athletes").
narrative_ontology:topic_domain(ncaa_eligibility_rules, "economic/labor_regulation").

domain_priors:requires_active_enforcement(ncaa_eligibility_rules).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ncaa_eligibility_rules, universities_athletic_departments).
narrative_ontology:constraint_beneficiary(ncaa_eligibility_rules, ncaa_administrative_apparatus).
narrative_ontology:constraint_beneficiary(ncaa_eligibility_rules, wealthy_student_athletes_with_alternatives).
narrative_ontology:constraint_victim(ncaa_eligibility_rules, low_income_student_athletes).
narrative_ontology:constraint_victim(ncaa_eligibility_rules, international_student_athletes).
narrative_ontology:constraint_victim(ncaa_eligibility_rules, athletes_without_family_wealth).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME STUDENT ATHLETE (SNARE) — Trapped between amateurism rules and economic necessity. Cannot monetize athletic ability, cannot work sufficient hours to offset scholarship gaps, no viable exit from the constraint. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-TIER ATHLETE (TANGLED ROPE) — Benefits from scholarships and athletic infrastructure but constrained by amateurism rules. Can exit through reduced athletic participation but at significant career cost. Coordination function (infrastructure, coaching, access) is real; extraction is also real (revenue capture, contract restrictions). d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.59.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE RECRUIT WITH PROFESSIONAL PROSPECTS (ROPE) — Can arbitrage into professional leagues, portal transfers, or endorsement markets. Experiences eligibility rules as coordinating mechanism for athletic development. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.01. Net beneficiary.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: UNIVERSITY ATHLETIC DEPARTMENT (TANGLED ROPE) — Beneficiary of amateurism rules (labor cost suppression, revenue concentration). Also coordinates legitimate athletic development and educational integration. Active enforcement through compliance offices. Experiences rules as enabling coordination while extracting labor value. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary; classification as tangled rope reflects the coordination function present alongside extraction.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ATHLETES' COALITION / LABOR MOVEMENT (SNARE) — Organized actors attempting to exit see the eligibility rules as pure extraction mechanism designed to prevent collective action. Unionization efforts, legal challenges (O'Bannon, Alston), and transfer portal reform are constrained exits — each challenge faces enforcement costs. d≈0.78, f(d)≈1.12, σ=1.0 → χ≈0.65.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: NCAA REGULATORY APPARATUS (PITON) — The enforcement machinery itself exhibits high theater: compliance offices, eligibility committees, and appellate processes perform regulatory legitimacy while actual enforcement is inconsistent across schools and sports. Enforcement depends on institutional inertia; alternatives (free-market athlete compensation, union recognition) would collapse the apparatus. theater_ratio≈0.65. The NCAA sees its own rules as increasingly indefensible but maintains them through procedural theater.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the eligibility rules coordinate a complex system (recruitment, development, education integration) while extracting significant value from a specific demographic (low-income athletes of color). The constraint is not immutable natural law but a contingent institutional arrangement that serves coordination functions alongside extraction. d≈0.65, f(d)≈1.00, σ=1.2 → χ≈0.70.
constraint_indexing:constraint_classification(ncaa_eligibility_rules, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ncaa_eligibility_rules_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ncaa_eligibility_rules, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ncaa_eligibility_rules, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.58): High-moderate. The NCAA rules extract significant economic value from student-athletes through prohibition on monetization while universities capture $14B+ annually in related revenue. However, the extraction is not absolute (0.70+) because legitimate coordination functions are present (scholarships do provide value, athletic infrastructure is real, educational integration has value). The baseline extractiveness has risen over 30 years as media valuations increased without corresponding athlete compensation, indicating accumulation of extraction. Suppression (0.68): High. Multiple barriers prevent exit or alternatives: amateurism rules prohibit direct compensation, work-hour restrictions limit employment income, international students face visa work restrictions, athletes face retaliation for attempting collective action (NCAA sanctions on athletes involved in litigation), and the power asymmetry (individual athlete vs institution) prevents negotiation. Theater ratio (0.65): Moderate-high. NCAA compliance procedures, eligibility committees, and appellate processes perform legitimacy while enforcement is inconsistent and often selective. The theatrical element has increased as the rule system becomes harder to defend on substantive grounds, requiring more elaborate procedural theater.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. Low-income athletes classify it as Snare (pure extraction with no coordination benefit for them personally). Mid-tier athletes see Tangled Rope (legitimate coordination alongside extraction). Elite athletes see Rope (primarily coordination mechanism enabling athletic development). Universities see it as beneficial Rope (coordination enabling recruitment and competition). The NCAA sees it as Piton (deteriorating rule system maintained through institutional inertia and theater). Athletes' coalitions see it as Snare (extraction mechanism designed to prevent collective action). The analytical observer sees Tangled Rope (real coordination bundled with real extraction). This gap is not measurement error — it reflects genuine structural asymmetry. The same rule produces different objective outcomes for different demographic groups.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income athletes: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction target. Cannot exit, cannot negotiate. International students: Victim + trapped → d≈0.95, f(d)≈1.42. Visa work restrictions add to amateurism rules. Mid-tier athletes: Victim + constrained → d≈0.68, f(d)≈1.02. Can exit through reduced participation but at career cost. Elite athletes: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Can arbitrage into professional markets. Universities: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net labor cost savings. NCAA: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.09. Revenue concentration mechanism. Athletes' coalitions: Victim + constrained → d≈0.78, f(d)≈1.12. Organized but exit is costly (legal, enforcement, reputational).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the tangled_rope classification is correct at the systemic level (real coordination + real extraction) while snare classification is correct for low-income athlete subpopulation. The constraint is not mislabeled as coordination when it is extraction — the presence of both functions is genuine. The rope classification for elite athletes is also genuine (they experience primarily coordination benefits). The perspectival gap is not an error in classification logic but a reflection of structural asymmetry. The system provides coordination services (coaching, facilities, competition, education integration) that have real value (~$30-50k annually), but captures value extraction (~$50-150k annually in direct revenue minus scholarship costs, depending on sport) from a specific demographic without compensation. The mandatrophy resolves by showing that this constraint is structurally both coordination and extraction simultaneously, with differential incidence across demographic groups. The constraint does not reduce to either type; both descriptions are accurate from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scholarship_value_measurement,
    'What is the true economic value of a scholarship relative to the market value of the athlete''s labor?',
    'Comparative analysis of scholarship stipends vs professional league minimum wages; cost-of-living adjustment for athlete expenses; athlete survey data on scholarship adequacy',
    'If scholarships cover 100%+ of costs: extraction mechanism is weaker (closer to Rope). If scholarships cover <70%: extraction mechanism is severe (closer to pure Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scholarship_value_measurement, empirical, 'True economic value of scholarship relative to athlete labor').

omega_variable(
    coordination_function_necessity,
    'Is the amateurism eligibility rule necessary for the coordination functions it serves (recruitment, competitive balance, education integration), or could those functions operate under alternative regimes (salary caps, athlete unions, free agency)?',
    'Comparative institutional analysis of collegiate athletic systems with different eligibility structures (some international models); controlled experiments with transfer portal reforms; historical analysis of professionalization trends in other sports',
    'If coordination functions require amateurism: Tangled Rope is correct classification. If coordination functions would survive professionalization: classification shifts toward pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, conceptual, 'Whether amateurism rule is necessary for coordination functions').

omega_variable(
    demographic_targeting,
    'Are eligibility rules deliberately designed to concentrate extraction on low-income and international athletes, or is this demographic incidence a side effect of neutral rules?',
    'Historical analysis of rule design (legislative intent); correlation analysis of rule changes with demographic impact; comparative study of eligibility rules in different NCAA divisions and how they differentially impact populations',
    'If deliberate: extraction mechanism is targeted predatory (pure Snare). If incidental: extraction is structural but not deliberately discriminatory (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_targeting, empirical, 'Whether eligibility rules deliberately target low-income athletes').

omega_variable(
    enforcement_consistency,
    'Are NCAA eligibility rules enforced consistently across institutions, sports, and demographic groups, or does enforcement variance indicate that the rule system serves theater rather than actual regulation?',
    'Audit study of enforcement outcomes (who gets sanctioned for violations, severity distribution); comparative analysis of violations detected in high-profile vs marginal programs; statistical analysis of enforcement disparities by school wealth and athletic program revenue',
    'If enforcement is consistent: rules are functional (Tangled Rope). If enforcement is inconsistent: rules are theater, enabling selective extraction (Piton or Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_consistency, empirical, 'Consistency of NCAA eligibility rule enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ncaa_eligibility_rules, 1980, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ncaa_tr_t0, ncaa_eligibility_rules, theater_ratio, 0, 0.5).
narrative_ontology:measurement(ncaa_tr_t15, ncaa_eligibility_rules, theater_ratio, 15, 0.6).
narrative_ontology:measurement(ncaa_tr_t30, ncaa_eligibility_rules, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(ncaa_be_t0, ncaa_eligibility_rules, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ncaa_be_t15, ncaa_eligibility_rules, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(ncaa_be_t30, ncaa_eligibility_rules, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ncaa_eligibility_rules, resource_allocation).
narrative_ontology:affects_constraint(ncaa_eligibility_rules, student_debt_accumulation).
narrative_ontology:affects_constraint(ncaa_eligibility_rules, athletic_labor_market_segmentation).
narrative_ontology:affects_constraint(ncaa_eligibility_rules, university_endowment_extraction).

% DUAL FORMULATION NOTE:
% The NCAA eligibility rules decompose into multiple constraint stories depending on observable: (1) amateurism_labor_prohibition (pure extraction mechanism, ε≈0.75, Snare), (2) athletic_infrastructure_provision (coordination mechanism, ε≈0.15, Rope), (3) competitive_balance_regulation (mixed, ε≈0.42, Tangled Rope). These are linked via network.affects_constraints because the labor prohibition (1) prevents the infrastructure provision (2) from functioning as pure coordination, and the competitive balance goal (3) depends on suppression of market signals.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ncaa_eligibility_rules, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
