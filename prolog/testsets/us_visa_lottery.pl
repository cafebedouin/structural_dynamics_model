% ============================================================================
% CONSTRAINT STORY: us_visa_lottery
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_visa_lottery, []).

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
 *   constraint_id: us_visa_lottery
 *   human_readable: US Diversity Visa Lottery
 *   domain: political_economy/immigration
 *
 * SUMMARY:
 *   The US Diversity Visa lottery allocates 50,000 immigrant visas annually
 *   through random selection from eligible countries with historically low
 *   immigration rates to the United States. Established by the 1990
 *   Immigration Act, the DV-1 program aims to diversify the immigration
 *   stream. However, the constraint exhibits a profound structural
 *   contradiction: a formally random allocation mechanism layered over
 *   per-country caps, skill-selective application prerequisites, and fee
 *   barriers that operationally concentrate benefits and distribute costs
 *   unequally. From the perspective of lottery losers and citizens of
 *   high-immigration-pressure countries, the lottery is a pure extraction
 *   device: they bear application costs, bureaucratic overhead, and the
 *   psychological cost of hope with near-zero probability of benefit. From
 *   the perspective of selective employers and immigration bureaucracy, the
 *   lottery is coordination infrastructure that solves labor supply and
 *   policy compliance problems. The rising theater_ratio (0.42 → 0.58 over 30
 *   years) reflects growing disjunction between the fairness narrative
 *   (random selection) and the operational gatekeeping function (per-country
 *   caps and skill biases). This constraint is a diagnostic case of how
 *   formalized randomness can mask structural extraction.
 *
 * KEY AGENTS:
 *   - Lottery Losers: Primary victims (powerless/trapped) — bear full application costs with ~0.03% winning probability per application
 *   - High-Immigration Countries: Primary victims (powerless/trapped) — India, Philippines, Mexico etc. structurally excluded by per-country 7% cap despite eligibility
 *   - Selective Employers: Primary beneficiary (institutional/arbitrage) — access mid-skilled workers without EB visa sponsorship competition
 *   - Immigration Bureaucracy: Secondary beneficiary (institutional/constrained) — generates application fees (~$100M+ annually), maintains visa allocation authority
 *   - Family Reunification Seekers: Secondary victim (organized/constrained) — constrained by family obligation, enabled by kinship networks
 *   - Policy Ritual Observers: Analytical perspective (analytical/analytical) — see performative fairness masking structural gatekeeping
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_visa_lottery, 0.52).
domain_priors:suppression_score(us_visa_lottery, 0.68).
domain_priors:theater_ratio(us_visa_lottery, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_visa_lottery, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_visa_lottery, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_visa_lottery, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_visa_lottery, tangled_rope).
narrative_ontology:human_readable(us_visa_lottery, "US Diversity Visa Lottery").
narrative_ontology:topic_domain(us_visa_lottery, "political_economy/immigration").

domain_priors:requires_active_enforcement(us_visa_lottery).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_visa_lottery, us_selective_employer_constituency).
narrative_ontology:constraint_beneficiary(us_visa_lottery, immigration_bureaucracy).
narrative_ontology:constraint_victim(us_visa_lottery, excluded_high_immigration_countries).
narrative_ontology:constraint_victim(us_visa_lottery, diversity_lottery_losers).
narrative_ontology:constraint_victim(us_visa_lottery, family_reunification_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOTTERY LOSER (SNARE) — Applicant from eligible country with near-zero winning probability (0.0003 odds) trapped in bureaucratic randomness. No appeal mechanism, no skill-based alternative pathway, no recourse. Extraction is near-total: applicant bears full cost (application fees, documentation, time, psychological cost of hope) with essentially zero probability of benefit. Suppression is maximal — alternatives (family sponsorship, employment visa) are blocked or have multi-year waits.
constraint_indexing:constraint_classification(us_visa_lottery, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EXCLUDED HIGH-IMMIGRATION COUNTRY (SNARE) — India, Philippines, Mexico, and other high-immigration nations are structurally disadvantaged by the per-country cap (7% maximum of available visas). Citizens face extraction through exclusion: the same lottery rules that theoretically apply to all countries operationally suppress their chances by 85-90% relative to low-immigration countries. They bear the cost of visa system complexity and bureaucratic overhead with minimal probability of benefit. No exit from the constraint without sovereign policy change.
constraint_indexing:constraint_classification(us_visa_lottery, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: FAMILY REUNIFICATION SEEKER (TANGLED ROPE) — Individual with family ties in the US faces extraction (multi-year wait times, limited visa categories, no guarantee) but also experiences coordination benefits through US family connections and kinship networks that facilitate initial settlement. Constrained exit: cannot walk away from family obligation, but organized kin networks provide partial agency and information. Mixed classification reflects both the barrier (extraction) and the social infrastructure (coordination) that characterizes family-based immigration pathways.
constraint_indexing:constraint_classification(us_visa_lottery, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SELECTIVE EMPLOYER (ROPE) — High-skilled labor market actors (tech companies, research institutions, healthcare providers) benefit from the DV lottery as a coordination mechanism: it supplies mid-skilled workers without requiring employer sponsorship (EB visa) or competing with workers sponsored by rivals. The lottery offloads hiring risk to the randomness system and provides plausible deniability for selective hiring. Employers experience the constraint as pure coordination — a mechanism that solves the labor arbitrage problem. High exit: employers can shift hiring strategies, use alternative visa categories, or offshore work.
constraint_indexing:constraint_classification(us_visa_lottery, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: IMMIGRATION BUREAUCRACY (TANGLED ROPE) — USCIS and DOS manage the DV lottery as both a coordination mechanism (allocates scarce visa slots across countries) and an extraction mechanism (generates application fees, maintains bureaucratic authority, ensures predictable visa distribution). Constrained exit: statutory mandate to administer the lottery limits policy flexibility, but agency discretion over implementation details and fee structures creates room for extraction. The lottery simultaneously serves coordination function (matching visa supply to policy intent) and extraction function (generating revenue and bureaucratic gatekeeping).
constraint_indexing:constraint_classification(us_visa_lottery, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: POLICY RITUAL (PITON) — The diversity visa lottery persists as a performative response to the 1990 Immigration Act's mandate to 'diversity' US immigration sources. The randomness mechanism appeals to fairness ideology while operationally maintaining gatekeeping: the per-country caps ensure that high-immigration-pressure countries remain excluded despite the lottery. Theater is high (fairness narrative, transparency of randomness) while function is degraded (effectiveness at actually diversifying sources is limited by simultaneous structural barriers). The constraint is maintained through institutional inertia — replacing it would require congressional action that faces organized opposition from selective employer constituencies.
constraint_indexing:constraint_classification(us_visa_lottery, piton,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 7: GLOBAL ASPIRING IMMIGRANT CLASS (SNARE) — From a civilizational perspective, the lottery system extracts global labor value through bureaucratic gatekeeping. The constraint forces potential migrants to apply repeatedly, accumulate documentation, pay fees (avg $500-$1000 across applications), and wait years for each draw. The system's design ensures that success requires either luck or institutional privilege (knowledge of application mechanics, money for repeated tries, network connections to lottery winners). Extraction is structural and total for the powerless: global poor bear 100% of costs with minimal benefit probability.
constraint_indexing:constraint_classification(us_visa_lottery, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_visa_lottery_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_visa_lottery, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_visa_lottery, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_visa_lottery, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_visa_lottery, TR),
    TR >= 0.70.

:- end_tests(us_visa_lottery_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The lottery extracts from applicants through application fees, repeated cycles (most applicants reapply 3-5 times), and opportunity costs, while producing minimal benefit (50K visas distributed to ~15M qualified applicants globally annually = 0.33% per-application win probability). The per-country cap further concentrates extraction: citizens of high-immigration countries face 85-90% suppression of their nominal chances. However, extractiveness is not total (0.66+): some coordination benefit exists for the immigration system (allocates visas according to policy intent), and winners experience genuine immigration gain. Suppression (0.68): High. Multiple barriers suppress alternatives: family sponsorship has multi-year backlogs; employment sponsorship requires employer sponsorship and wage certification; humanitarian categories are tightly limited. No pathway exists for ordinary citizens of excluded countries to bypass the lottery. Theater ratio (0.58): Moderate-high and rising. The randomness narrative appeals to fairness ideology and transparency, while the operational mechanism (per-country caps, fee barriers, skill bias in application materials) ensures structured gatekeeping. The theater has increased over time as the contradiction between fairness narrative and exclusionary outcome has become more visible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence. Lottery losers see a snare: random selection provides legitimating narrative for what is operationally a complete extraction device. High-immigration countries see exclusion: the per-country cap ensures their citizens bear disproportionate cost. Selective employers see coordination: the lottery solves their labor supply problem without requiring them to compete in employer sponsorship markets. Immigration bureaucracy sees mandate fulfillment: the lottery implements the 1990 Act's diversity intent while generating fee revenue. The family reunification seeker sees mixed extraction and coordination: constrained by structural barriers but enabled by kinship networks. The analytical observer sees a piton: a degraded policy ritual where the fairness narrative (randomness) masks structural gatekeeping (per-country caps, skill biases, fee barriers). The perspectival gap reveals that the constraint's primary function differs by observer: for the powerless it is pure extraction, for the institutional it is coordination, for the analytical it is performative gatekeeping.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by agent. Lottery losers (powerless/trapped) experience d ≈ 0.95 (full target): they are victims with no exit options. Their experienced extractiveness is maximum. High-immigration countries (powerless/trapped) experience d ≈ 0.92 (near-full target): structurally disadvantaged by per-country caps. Selective employers (institutional/arbitrage) experience d ≈ 0.10 (near-beneficiary): they have high exit (can use other visa categories) and benefit from the lottery's labor supply coordination. Immigration bureaucracy (institutional/constrained) experiences d ≈ 0.35 (moderate beneficiary): they benefit from fee revenue and visa authority but are constrained by statutory mandate. Family reunification seekers (organized/constrained) experience d ≈ 0.55 (neutral): they bear extraction costs but benefit from kinship network coordination effects. The perspectival gap between lottery losers and selective employers is maximal — they experience the same constraint as snare vs. rope respectively, revealing the deep asymmetry in how the system's costs and benefits are distributed.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the extraction-vs-coordination ambiguity by decomposing the DV lottery into its actual structural components. The lottery IS a coordination mechanism (allocates visa supply across eligible countries according to policy intent). The lottery IS ALSO an extraction mechanism (distributes costs to applicants and benefits to employers and bureaucracy). The tangled rope classification captures both simultaneously: extractiveness (0.52) reflects the net asymmetry between costs borne by applicants and benefits captured by employers; suppression (0.68) reflects the multiple barriers that prevent exit to alternative pathways; requires_active_enforcement (true) reflects that the constraint depends on continuous bureaucratic administration and legal authority. The rising theater_ratio (0.42 → 0.58) indicates degradation in the coordination function: the fairness narrative becomes increasingly performative as the per-country cap's exclusionary effect becomes visible. If the theater_ratio reaches 0.70+, the constraint would degrade from tangled rope to piton (institutional inertia). The mandatrophy is resolved by recognizing that the constraint is legitimately both coordination (for the immigration system) and extraction (for individual applicants and excluded countries) — the classification depends on which structural relationship you measure from. No single type is 'the' answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    randomness_authenticity_vs_gatekeeping,
    'Does the random-draw mechanism genuinely distribute visa opportunity fairly, or does it operationally function as a gatekeeping device that advantages high-skill, high-resource applicants despite randomness?',
    'Longitudinal analysis of diversity visa winner profiles: education level, income, prior US connections, language proficiency. Comparison to population-level demographics in source countries.',
    'If winners are random across socioeconomic strata: rope classification (pure coordination) confirmed. If winners cluster in high-skill/high-resource profiles: snare classification (extraction disguised as randomness) confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(randomness_authenticity_vs_gatekeeping, empirical, 'Whether lottery randomness genuinely distributes opportunity or functions as gatekeeping').

omega_variable(
    per_country_cap_structural_necessity,
    'Is the per-country 7% cap necessary for coordination (preventing any single source from overwhelming the system) or is it primarily an extraction mechanism (maintaining exclusion of high-immigration countries)?',
    'Counterfactual modeling: visa distribution under uncapped lottery vs. under different cap levels. Immigration system stress analysis under different distribution scenarios. Comparison to visa distributions in other developed democracies without per-country caps.',
    'If cap is coordination necessity: constraint is hybrid (tangled rope). If cap is primarily exclusionary: constraint is snare operating through ''neutral'' rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(per_country_cap_structural_necessity, conceptual, 'Whether per-country cap is coordination necessity or exclusionary mechanism').

omega_variable(
    alternative_visa_category_substitution,
    'Would eliminating the DV lottery shift demand to other visa categories (family sponsorship, employment sponsorship) or would it reduce overall immigration?',
    'Time-series analysis of visa category demand elasticity. Natural experiments from policy changes in other countries. Prospective surveys of applicants on alternative pathways.',
    'If significant substitution: lottery is extractive overlay on a coordination system (tangled rope confirmed). If little substitution: lottery fills genuine visa supply gap (rope or mountain confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_visa_category_substitution, empirical, 'Whether eliminating lottery shifts demand to other categories').

omega_variable(
    bureaucratic_fee_extraction_magnitude,
    'What portion of the constraint''s extractiveness is attributable to application fees and repeated application cycles vs. structural visa scarcity?',
    'Fee analysis: total fees collected from DV applicants annually. Per-applicant cost modeling (application fees, translation, medical exams, travel to interviews). Comparison of fee burden to applicant income in source countries.',
    'If fees > 20% of total extraction: fee removal would significantly reduce snare classification. If fees < 10%: structural visa scarcity is the primary extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bureaucratic_fee_extraction_magnitude, empirical, 'Magnitude of bureaucratic fee extraction vs. visa scarcity extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_visa_lottery, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dv_lottery_tr_t0, us_visa_lottery, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dv_lottery_tr_t15, us_visa_lottery, theater_ratio, 15, 0.52).
narrative_ontology:measurement(dv_lottery_tr_t30, us_visa_lottery, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(dv_lottery_be_t0, us_visa_lottery, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dv_lottery_be_t15, us_visa_lottery, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(dv_lottery_be_t30, us_visa_lottery, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_visa_lottery, resource_allocation).
narrative_ontology:affects_constraint(us_visa_lottery, us_family_sponsored_visa_backlog).
narrative_ontology:affects_constraint(us_visa_lottery, us_employment_visa_gatekeeping).

% DUAL FORMULATION NOTE:
% The DV lottery is downstream of broader US immigration visa allocation constraints. Its extractiveness is amplified by bottlenecks in family sponsorship and employment visa pathways, which force applicants toward the lottery as a supposedly lower-barrier alternative. All three constraints share a common structural problem (visa supply scarcity) but operate through different allocation mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_visa_lottery, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
