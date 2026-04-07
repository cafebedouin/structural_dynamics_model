% ============================================================================
% CONSTRAINT STORY: k12_school_funding_inequality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_k12_school_funding_inequality, []).

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
 *   constraint_id: k12_school_funding_inequality
 *   human_readable: K-12 School Funding Inequality in the United States
 *   domain: education/fiscal_policy
 *
 * SUMMARY:
 *   K-12 school funding inequality in the United States is a structural
 *   constraint that combines genuine coordination challenges (how to fund
 *   schools at scale across diverse communities) with systematic extraction
 *   of resources from low-income and rural students toward wealthy districts.
 *   The constraint operates through property-tax-based funding mechanisms
 *   that create feedback loops: wealthy communities generate high tax revenue
 *   per student, which enables better schools, which attracts higher-income
 *   residents, which increases property values, which increases the tax base
 *   further. Simultaneously, low-income communities face declining
 *   population, lower property values, and thus lower tax revenue per
 *   student. This cycle is maintained through active policy enforcement
 *   (state legislatures preserve property-tax-based structures, limit
 *   equalization formula effectiveness, and enable school choice mechanisms
 *   that selectively benefit higher-income families). The constraint exhibits
 *   all six types from different perspectives because it genuinely serves
 *   coordination functions (enabling communities to fund local schools) while
 *   systematically extracting resources from trapped students with no exit
 *   options. Theater has increased over the measurement interval as
 *   equalization formulas, choice mechanisms, and accountability systems have
 *   proliferated without substantially reducing inequality — the performance
 *   of addressing inequality has increased while the substantive gaps
 *   persist.
 *
 * KEY AGENTS:
 *   - Low-Income Students: Primary victim (powerless/trapped) — cannot exit geographic jurisdiction or relocation barriers; face resource deprivation during critical developmental years
 *   - Under-Resourced Districts: Secondary victims (moderate/constrained) — can marginally improve through local effort but face structural revenue barriers and population decline
 *   - Wealthy School Districts: Primary beneficiary (institutional/arbitrage) — capture resources through property tax system; can exit via economic clustering and continued appreciation
 *   - State Legislature: Primary enforcer (institutional/arbitrage) — maintains property-tax-based structure despite equalization mandate; enables choice mechanisms that preserve stratification
 *   - School Choice Coalition: Organized agents (organized/constrained) — claim to solve the problem through market mechanisms but create secondary extraction (cream-skimming, administrative rent-seeking)
 *   - Equalization Formula: Institutional mechanism (institutional/arbitrage) — performs the role of addressing inequality while preserving structural extraction; theater-dominant
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy-contingent inequality as a law of federalism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(k12_school_funding_inequality, 0.58).
domain_priors:suppression_score(k12_school_funding_inequality, 0.72).
domain_priors:theater_ratio(k12_school_funding_inequality, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(k12_school_funding_inequality, extractiveness, 0.58).
narrative_ontology:constraint_metric(k12_school_funding_inequality, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(k12_school_funding_inequality, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(k12_school_funding_inequality, tangled_rope).
narrative_ontology:human_readable(k12_school_funding_inequality, "K-12 School Funding Inequality in the United States").
narrative_ontology:topic_domain(k12_school_funding_inequality, "education/fiscal_policy").

domain_priors:requires_active_enforcement(k12_school_funding_inequality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(k12_school_funding_inequality, wealthy_school_districts).
narrative_ontology:constraint_beneficiary(k12_school_funding_inequality, property_tax_revenue_jurisdictions).
narrative_ontology:constraint_victim(k12_school_funding_inequality, low_income_students).
narrative_ontology:constraint_victim(k12_school_funding_inequality, under_resourced_districts).
narrative_ontology:constraint_victim(k12_school_funding_inequality, rural_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME STUDENT (SNARE) — Trapped within geographic jurisdiction; cannot exit to better-funded district without family relocation (prohibitive cost). Trapped within age/developmental window (cannot defer schooling). Suppression is near-total: structural barriers to exit (residential segregation, housing costs, transportation) prevent escape. Experiences maximum extraction — resource deprivation during critical developmental years with no alternative. No genuine coordination benefit — the constraint exists to extract resources upward, not to solve a collective action problem.
constraint_indexing:constraint_classification(k12_school_funding_inequality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNDER-RESOURCED DISTRICT (TANGLED ROPE) — Constrained but not fully trapped: can marginally improve conditions through bond elections, grants, or operational efficiency, but faces high barriers (declining population, lower property tax base, voter fatigue). Experiences genuine coordination function (managing educational service delivery across students) alongside asymmetric extraction (resources systematically flow toward wealthy districts via state funding allocation mechanisms). Active enforcement required: state legislature maintains the property-tax-based funding structure that drives inequality.
constraint_indexing:constraint_classification(k12_school_funding_inequality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: WEALTHY SCHOOL DISTRICT (ROPE) — Primary beneficiary (institutional/arbitrage). Experiences the constraint as coordination: tax base pooling and spending decisions align resources with their community's preferences. Can exit the constraint via property value appreciation, economic clustering, and selective residence — arbitrage is genuine. No experienced extraction; net beneficial relationship. The constraint solves their coordination problem (how to fund schools at their preferred level) with minimal friction.
constraint_indexing:constraint_classification(k12_school_funding_inequality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: SCHOOL CHOICE COALITION (TANGLED ROPE) — Organized agents (charter networks, education reformers, voucher advocates) see funding inequality as a coordination problem solvable through market mechanisms: per-pupil funding portability, open enrollment, and school choice enable low-income students to arbitrage to better schools. But school choice itself creates secondary extraction mechanisms: cream-skimming (selection bias in who participates), increased overhead (administrative duplication across competing operators), and political concentration of governance. The coalition both solves a coordination problem (student access to better options) and creates asymmetric extraction (power concentrated in choice operators). Theater includes substantial performance theater: choice accountability reporting that masks cream-skimming dynamics and selection bias.
constraint_indexing:constraint_classification(k12_school_funding_inequality, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE EQUALIZATION FORMULA (PITON) — Most states maintain equalization formulas (weighted funding for high-need students, base aid guarantees) that are substantially performative. The formulas acknowledge inequality but maintain weak enforcement, allow wealthy districts to augment state baseline with local revenue, and fail to actually equalize (per-pupil spending gaps persist despite formulas). The mechanism persists through institutional inertia — it creates an appearance of effort to address inequality while preserving structural extraction. Theater ratio is high because the formula's inputs and adjustment factors are complex rituals that obscure the persistence of underlying inequality. The formula's primary function has atrophied: equalization in theory, wealth preservation in practice.
constraint_indexing:constraint_classification(k12_school_funding_inequality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, unequal resource distribution is an immutable property of federalist systems: decentralization necessarily produces variance in local capacity to fund public goods. This perspective sees funding inequality as emerging naturally from constitutional structure (local property tax base, state sovereignty, market housing dynamics). However, structural data contradicts this classification — empirical analysis shows that countries with equally federal systems (Canada, Switzerland, Germany) achieve far lower funding inequality through transfer mechanisms and floor funding. The mountain classification is a false summit: it naturalizes what is actually a contingent policy choice (allowing local wealth to determine school resources) as a law of governance.
constraint_indexing:constraint_classification(k12_school_funding_inequality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(k12_school_funding_inequality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(k12_school_funding_inequality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(k12_school_funding_inequality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(k12_school_funding_inequality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(k12_school_funding_inequality, TR),
    TR >= 0.70.

:- end_tests(k12_school_funding_inequality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts resources from low-income students and under-resourced districts toward wealthy districts. The extraction is not maximal (0.85+) because some equalization occurs, some high-need students benefit from targeted funding, and some districts maintain reasonable quality of life outcomes despite resource gaps. But the baseline extraction is substantial — per-pupil spending gaps of 2:1 or 3:1 between districts are common, and these gaps compound over a student's K-12 career. The v1.0 measurement (0.42) reflected lower precision in identifying the extraction mechanism; the refined value reflects clear structural data on resource flow asymmetry. Suppression (0.72): High. Multiple barriers prevent low-income students from exiting the constraint: (1) residential constraint — relocation to wealthier district requires housing price appreciation most low-income families cannot achieve; (2) age constraint — cannot defer K-12 education to a later time when circumstances might improve; (3) information constraint — family may lack knowledge of alternative schools or choice mechanisms; (4) transportation constraint — open enrollment requires student transportation which families may not afford; (5) social cost — moving or changing schools carries identity/relationship costs. The combination of barriers is near-total, though not quite to the level of trapped (0.85+) because some escape routes exist (school choice, charter networks, relocation with family support). Theater ratio (0.65): Moderate-high. State equalization formulas, school accountability systems, and choice mechanisms create performative complexity that obscures the persistence of underlying inequality. Complex funding formulas give appearance of rigorous redistribution while weak enforcement allows wealthy districts to augment state baseline with local revenue. Choice accountability reporting masks selection bias in participation. Theater has increased over time as policy mechanisms have proliferated without proportional reduction in inequality.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the low-income student's Snare perception and the wealthy district's Rope perception reflects the complete inversion of their structural positions. From the trapped student's position, the constraint is pure extraction with suppression near-total. From the arbitrage-enabled wealthy district's position, the constraint is pure coordination with no felt extraction. The under-resourced district's Tangled Rope perception is accurate — they genuinely coordinate (deliver education) while experiencing systematic extraction (resource disadvantage). The school choice coalition's perspective obscures secondary extraction mechanisms (cream-skimming, administrative overhead, political concentration) that may be worse for trapped students than the original constraint. The equalization formula's Piton status reveals that state institutional response has become substantially performative — the policy mechanisms (formulas, choice accountability) create appearance of effort without proportional substantive change. The civilizational analytical observer's false summit demonstrates how perspective can naturalize contingent policy arrangements: funding inequality is framed as inherent to federalism, but comparative analysis shows federal systems with lower inequality use different policy designs (floor funding, stringent equalization, tighter transfer mechanisms). The false summit is policy-contingent, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position relative to the resource flow. Low-income students face maximum d (0.95+) — they are full extraction targets with trapped exit options. Under-resourced districts face high d (0.70-0.80) — they are constrained victims but with some marginal agency. Wealthy districts face low d (0.10-0.20) — they are beneficiaries with arbitrage exit options (can further cluster, appreciate property values, exit entirely). The state legislature (enforcer) has d around 0.35-0.45 — it maintains the constraint (which benefits some constituencies) while formally committed to equalization (apparent beneficiary status). The school choice coalition has intermediate d (0.55-0.65) — they claim to solve the problem but enable secondary extraction. The equalization formula is effectively neutral (d ≈ 0.50) — it formally redistributes but fails to substantially equalize due to weak enforcement. These d values are derived from exit options: trapped agents experience maximum f(d); arbitrage agents experience minimum or negative extraction. The beneficiary/victim declarations (wealthy districts benefit, low-income students and under-resourced districts are victims) directly feed the computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint's classification varies legitimately across structural positions, and the variation itself is diagnostic. The beneficiary's Rope classification is correct for their position — they experience the system as coordination. The victim's Snare classification is correct for their position — they experience the system as pure extraction with no alternative. The analytical observer's false summit diagnosis (Mountain is incorrect; the constraint is policy-contingent, not natural law) is the diagnostic signal: when the system's own institutions (equalization formula, accountability mechanisms) are substantially performative (theater_ratio 0.65+) despite formal commitment to addressing the problem, the constraint is being actively maintained as extractive, not emerging naturally from structural necessity. The refinement from v1.0 (which treated this as straightforward Snare at 0.72 extractiveness) to v1.1 (tangled rope/piton hybrid with explicit false summit diagnosis) clarifies that the constraint is enforcement-dependent: state legislature maintains property-tax-based structures and limits equalization despite formal commitment, which is evidence of active extraction, not natural emergence. This reclassifies the constraint toward the tangled_rope cluster (genuine coordination function + asymmetric extraction + active enforcement required) and away from the pure-snare hypothesis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_poverty_vs_funding,
    'Does funding inequality cause poverty-correlated educational outcomes, or do poverty demographics select into under-resourced districts?',
    'Instrumental variable analysis using district boundary discontinuities and quasi-experimental variation in funding levels; longitudinal tracking of outcomes at identical poverty levels in high-vs-low-funding contexts',
    'If funding causes outcomes: constraint is snare (extraction prevents capability). If selection dominates: constraint may be rope or scaffold (outcome gaps reflect broader inequality, not school-specific extraction). This drives classification certainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_poverty_vs_funding, empirical, 'Causality between funding and outcomes vs. poverty-driven selection').

omega_variable(
    equalization_mechanism_sufficiency,
    'Can state equalization formulas theoretically eliminate funding gaps, or do they encounter hard limits (tax base, administrative capacity, property values)?',
    'Comparative analysis of equalization formula performance across states; identification of threshold effects where formulas break down; modeling of maximum achievable equalization under different policy designs',
    'If formulas can equalize: constraint is tangled_rope with clear sunset (equalization formula refinement). If hard limits exist: constraint is structural snare (extraction is inherent to federalism unless fundamental restructuring). Classification depends on policy ceiling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equalization_mechanism_sufficiency, empirical, 'Whether equalization formulas can theoretically eliminate funding inequality').

omega_variable(
    school_choice_filtering,
    'Does per-pupil funding portability (school choice) actually reduce extraction for low-income students, or does it primarily enable cream-skimming and administrative rent-seeking?',
    'Comparison of post-choice outcomes for students with equal baseline characteristics between choice-participating and non-participating cohorts; measurement of selection bias in participation and stability of charter/choice school closures',
    'If choice reduces extraction: scaffold perspective correct, sunset is genuine (market mechanisms solve the problem). If choice enables cream-skimming: choice becomes secondary snare (new extraction mechanism replaces old one). Reclassifies the school choice coalition''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(school_choice_filtering, empirical, 'Whether school choice portability reduces extraction or enables cream-skimming').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) primarily structural (geographic, economic barriers to relocation) or partially internalized (low-income families internalize residential constraints as inevitable)?',
    'Post-relocation outcome tracking; cross-cultural comparison with societies where relocation costs are lower; survey measurement of perceived vs. actual barriers to exit',
    'If primarily structural: suppression is external and could be reduced by policy (reduce relocation costs, enable open enrollment). If internalized: suppression persists even after structural barriers are lowered; requires identity/narrative reframing alongside policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism').

omega_variable(
    federalism_comparative_design,
    'Do equally federal systems (Canada, Switzerland, Germany, Australia) with lower funding inequality use fundamentally different transfer mechanisms, or do they apply property-tax-plus-equalization models more stringently?',
    'Comparative institutional analysis of transfer formulas, floor funding mechanisms, and enforcement strictness across federal democracies; identification of design choices (not just scale) that produce variance',
    'If design differences are determinative: US funding inequality is policy-contingent, not natural law (false summit diagnosis confirmed). If all federal systems produce similar gaps when measured at equivalent development levels: constraint may be closer to mountain. This shapes the false summit analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federalism_comparative_design, empirical, 'Comparative analysis of federal systems'' funding inequality mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(k12_school_funding_inequality, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(k12fund_tr_t0, k12_school_funding_inequality, theater_ratio, 0, 0.45).
narrative_ontology:measurement(k12fund_tr_t20, k12_school_funding_inequality, theater_ratio, 20, 0.58).
narrative_ontology:measurement(k12fund_tr_t40, k12_school_funding_inequality, theater_ratio, 40, 0.65).
narrative_ontology:measurement(k12fund_tr_t60, k12_school_funding_inequality, theater_ratio, 60, 0.65).

% Extraction over time
narrative_ontology:measurement(k12fund_be_t0, k12_school_funding_inequality, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(k12fund_be_t20, k12_school_funding_inequality, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(k12fund_be_t40, k12_school_funding_inequality, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(k12fund_be_t60, k12_school_funding_inequality, base_extractiveness, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(k12_school_funding_inequality, resource_allocation).
narrative_ontology:affects_constraint(k12_school_funding_inequality, residential_segregation_feedback).
narrative_ontology:affects_constraint(k12_school_funding_inequality, school_to_prison_pipeline).
narrative_ontology:affects_constraint(k12_school_funding_inequality, wealth_intergenerational_transmission).

% DUAL FORMULATION NOTE:
% K-12 funding inequality is structurally upstream of residential segregation (funding gaps drive school quality differences which drive residential choice, which reinforces segregation patterns) and downstream of broader wealth distribution inequality. Separate constraint stories model the specific feedback loops: funding → school quality → residential selection → funding. This story focuses on the education system's internal extraction mechanism; the residential segregation constraint models the same phenomenon from a housing market perspective. Both constraints link to the intergenerational wealth transmission constraint because school quality is a primary mechanism for inter-generational persistence of economic advantage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(k12_school_funding_inequality, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
