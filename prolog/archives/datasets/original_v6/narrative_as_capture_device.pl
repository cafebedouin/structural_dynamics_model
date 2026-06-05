% ============================================================================
% CONSTRAINT STORY: narrative_as_capture_device
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_narrative_as_capture_device, []).

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
 *   constraint_id: narrative_as_capture_device
 *   human_readable: Life-Script Narrative as Financial Capture Device
 *   domain: political_economy/consumer_finance/social_control
 *
 * SUMMARY:
 *   The life-script narrative — house at 28, car at 19, marriage and children
 *   on schedule — functions as a financial capture device by setting major
 *   debt commitments before financial literacy develops. Young adults enter
 *   30-year mortgages and multi-year auto loans during the narrow window
 *   (ages 19-28) when social pressure peaks but financial understanding
 *   remains minimal. The constraint exhibits high extraction (0.68) because
 *   the temporal lock-in prevents informed consent: by the time borrowers
 *   understand amortization schedules, opportunity costs, and alternative
 *   pathways, they are already locked into decades of payments. Suppression
 *   (0.72) operates through both narrative framing (alternatives are
 *   presented as deviant or irresponsible) and structural barriers (credit
 *   scores punish late adoption, rental markets extract without building
 *   equity, car-dependent infrastructure makes auto loans nearly mandatory).
 *   Theater ratio (0.58) reflects the performative financial literacy
 *   industry: mandatory disclosures, counseling sessions, and educational
 *   materials that arrive after commitment rather than before, creating the
 *   appearance of informed consent without its substance. The constraint is
 *   downstream of obligation_floor_as_control (which sets the baseline debt
 *   burden) and desire_cultivation_ratchet (which ensures the narrative
 *   remains aspirational rather than cautionary).
 *
 * KEY AGENTS:
 *   - Young Adults: Primary victim (powerless/trapped) — enter commitments pre-literacy, bear decades of extraction, cannot exit without severe penalty
 *   - First-Time Borrowers: Primary victim (powerless/trapped) — at moment of commitment, lack both literacy and negotiating power
 *   - Financial Institutions: Primary beneficiary (institutional/arbitrage) — capture predictable cohorts at optimal life stages, benefit from information asymmetry and temporal lock-in
 *   - Real Estate Industry: Secondary beneficiary (institutional/arbitrage) — life-script narrative channels demand into homeownership, sustaining price appreciation
 *   - Automotive Finance Sector: Secondary beneficiary (institutional/arbitrage) — narrative makes car ownership a prerequisite for adulthood, ensuring early loan uptake
 *   - Financially Literate Late Adopters: Mixed position (moderate/constrained) — delayed commitment until post-literacy, experience both coordination and extraction
 *   - Financial Literacy Movement: Organized agents (organized/mobile) — building alternative pathways and generational norm shift
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees irreducible coordination-extraction hybridity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(narrative_as_capture_device, 0.68).
domain_priors:suppression_score(narrative_as_capture_device, 0.72).
domain_priors:theater_ratio(narrative_as_capture_device, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(narrative_as_capture_device, extractiveness, 0.68).
narrative_ontology:constraint_metric(narrative_as_capture_device, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(narrative_as_capture_device, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(narrative_as_capture_device, snare).
narrative_ontology:human_readable(narrative_as_capture_device, "Life-Script Narrative as Financial Capture Device").
narrative_ontology:topic_domain(narrative_as_capture_device, "political_economy/consumer_finance/social_control").

domain_priors:requires_active_enforcement(narrative_as_capture_device).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(narrative_as_capture_device, financial_institutions).
narrative_ontology:constraint_beneficiary(narrative_as_capture_device, real_estate_industry).
narrative_ontology:constraint_beneficiary(narrative_as_capture_device, automotive_finance_sector).
narrative_ontology:constraint_victim(narrative_as_capture_device, young_adults).
narrative_ontology:constraint_victim(narrative_as_capture_device, first_time_borrowers).
narrative_ontology:constraint_victim(narrative_as_capture_device, financially_illiterate_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YOUNG ADULT DEBTOR (SNARE) — Enters major financial commitments (mortgage at 28, car loan at 19) before developing financial literacy. Cannot exit without severe credit damage and social stigma. The narrative presents these commitments as natural life milestones rather than financial instruments. Maximum extraction: bears interest payments for decades while locked into consumption patterns that prevent wealth accumulation.
constraint_indexing:constraint_classification(narrative_as_capture_device, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIRST-TIME BORROWER (SNARE) — At the moment of commitment, lacks both financial literacy and credit history to negotiate terms. The narrative framing ('everyone does this,' 'you need a car to work,' 'renting is throwing money away') suppresses alternative pathways. Immediate time horizon reflects that the trap closes at signing — the 30-year mortgage is locked in before the borrower understands amortization schedules or opportunity cost.
constraint_indexing:constraint_classification(narrative_as_capture_device, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: FINANCIAL INSTITUTION (ROPE) — Experiences the life-script narrative as pure coordination: cultural norms channel predictable cohorts into standardized financial products at optimal life stages. The institution benefits from information asymmetry and temporal lock-in but frames the relationship as service provision. Net beneficiary with arbitrage exit — can shift to other lending markets if this one becomes unprofitable.
constraint_indexing:constraint_classification(narrative_as_capture_device, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FINANCIALLY LITERATE LATE ADOPTER (TANGLED ROPE) — Delayed major debt commitments until after acquiring financial literacy (age 30+). Experiences genuine coordination benefit (access to housing, transportation) but also sees the extraction mechanism clearly. Constrained exit: can refinance or pay down debt strategically but cannot fully escape the system without severe lifestyle trade-offs. Mixed experience: benefits from the infrastructure while bearing extraction costs.
constraint_indexing:constraint_classification(narrative_as_capture_device, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: FINANCIAL LITERACY MOVEMENT (SCAFFOLD) — Organized coalition (consumer protection advocates, financial education nonprofits, progressive policy groups) building alternative pathways: mandatory financial literacy curricula, delayed credit access, income-share agreements as mortgage alternatives. Sees the narrative capture as temporary — generational shift toward financial education and alternative models will sunset the traditional life-script within 20-30 years. Low effective extraction because coalition has agency and sees structural change pathway.
constraint_indexing:constraint_classification(narrative_as_capture_device, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the life-script narrative serves both coordination (standardized life stages enable long-term planning, intergenerational wealth transfer, social cohesion) and extraction (captures cohorts before informed consent, locks in decades of interest payments, suppresses alternative pathways). The coordination function is genuine but asymmetrically distributed: institutions capture the coordination surplus while individuals bear the extraction costs. Tangled Rope reflects irreducible hybridity at the analytical level.
constraint_indexing:constraint_classification(narrative_as_capture_device, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(narrative_as_capture_device_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(narrative_as_capture_device, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(narrative_as_capture_device, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(narrative_as_capture_device, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(narrative_as_capture_device_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The temporal gap between commitment (ages 19-28) and literacy acquisition (ages 30+) creates structural information asymmetry. Financial institutions capture borrowers during the window of maximum social pressure and minimum financial understanding. The extraction is not total (0.68 rather than 0.85+) because some coordination function exists: standardized products enable scale economies, and predictable life stages facilitate long-term planning. But the bulk of the measured extraction is rent: interest rate spreads between informed and uninformed borrowers, profit margins in consumer finance that exceed risk-adjusted returns in other sectors, and the suppression of alternative models (cooperative housing, income-share agreements, car-sharing) that would reduce lock-in. Suppression (0.72): High. Operates through multiple channels: (1) Narrative framing — alternatives are presented as deviant ('failure to launch,' 'not a real adult'), irresponsible ('throwing money away on rent'), or impractical ('you need a car to work'). (2) Structural barriers — credit scoring systems punish late adoption, rental markets extract without building equity, car-dependent infrastructure makes auto ownership nearly mandatory in most US regions. (3) Temporal pressure — the narrow window (ages 19-28) when social expectations peak creates urgency that overrides deliberation. (4) Information asymmetry — mandatory disclosures arrive after commitment (at closing, in loan documents) rather than before, and financial literacy education (when provided) focuses on managing existing debt rather than evaluating whether to take on debt. Theater ratio (0.58): Moderate-high. The financial literacy industry is substantially performative: mandatory counseling sessions for first-time homebuyers occur after pre-approval (when the borrower is already committed), disclosure documents are written in legal language that obscures rather than clarifies, and financial education curricula focus on budgeting and credit management (how to service debt) rather than on opportunity cost and alternative models (whether to take on debt). The theater has increased over the interval as regulatory responses to predatory lending (post-2008) added disclosure requirements without addressing the underlying information asymmetry or temporal lock-in. However, theater is not total — some financial literacy programs do reach young adults before commitment, and some borrowers do acquire literacy through family transmission or self-education.
 *
 * PERSPECTIVAL GAP:
 *   The young adult debtor experiences pure extraction (Snare) — locked into decades of payments before understanding the terms, with no exit that doesn't destroy credit and social standing. The financial institution experiences pure coordination (Rope) — the life-script narrative solves the problem of channeling predictable cohorts into standardized products, and the institution frames information asymmetry as a natural feature of the market rather than as extraction. The financially literate late adopter experiences mixed coordination and extraction (Tangled Rope) — benefits from access to housing and transportation but sees clearly how the temporal lock-in and information asymmetry extract rents. The financial literacy movement sees a temporary problem with a sunset (Scaffold) — generational norm shifts and alternative models will reduce the narrative's power within 20-30 years. The analytical observer sees irreducible hybridity (Tangled Rope) — the coordination function (standardized life stages enable planning and intergenerational wealth transfer) and extraction function (temporal lock-in captures borrowers pre-literacy) are structurally inseparable at the civilizational level. The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?' The debtor's snare, the institution's rope, the late adopter's tangled rope, the movement's scaffold, and the observer's tangled rope are all legitimate readings of the same constraint from different indices.
 *
 * DIRECTIONALITY LOGIC:
 *   Young adults and first-time borrowers are victims with trapped exit options, yielding high d values (0.90-0.95) and maximum experienced extraction. They enter commitments before literacy develops and cannot exit without severe credit damage, social stigma, and material hardship (losing housing, losing transportation in car-dependent regions). Financial institutions, real estate industry, and automotive finance sector are beneficiaries with arbitrage exit options, yielding low d values (0.05-0.10) and negative or minimal experienced extraction. They benefit from the narrative's channeling function and can shift to other markets if this one becomes unprofitable. Financially literate late adopters are victims (bear extraction costs) but with constrained rather than trapped exit (can refinance, pay down strategically, or exit with significant but not catastrophic cost), yielding moderate d values (0.60-0.65) and moderate experienced extraction. The analytical observer uses the canonical analytical d value (0.72), reflecting the observer's structural distance from the extraction flow but recognition of the constraint's asymmetric impact.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not resolve the mandatrophy — it instantiates it. The analytical perspective classifies as Tangled Rope (irreducible coordination-extraction hybridity), but the powerless perspectives classify as Snare (pure extraction with minimal coordination benefit). The gap reveals that 'coordination' and 'extraction' are not observer-independent properties but indexical classifications that depend on structural position. From the institution's perspective, the narrative coordinates: it channels demand predictably and enables long-term planning. From the debtor's perspective, the narrative extracts: it locks in commitments before informed consent and suppresses alternatives. Both are true simultaneously because they are measurements from different indices. The mandatrophy is not resolved by choosing one classification over the other but by recognizing that the presheaf over the observation site — the collection of all perspectival classifications and the transformations between them — IS the constraint's structure. The constraint is a Snare from (powerless, biographical, trapped, national) and a Rope from (institutional, immediate, arbitrage, national) and a Tangled Rope from (analytical, civilizational, analytical, global), and these classifications do not contradict each other — they are different sections of the same sheaf.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_threshold_timing,
    'At what age does financial literacy reach sufficient depth to enable informed consent for 30-year financial commitments?',
    'Longitudinal studies correlating age of financial education, age of first major debt, and long-term financial outcomes (default rates, wealth accumulation, debt-to-income ratios at age 40+)',
    'If threshold is age 25: current system captures most borrowers pre-literacy (snare confirmed). If threshold is age 35+: even delayed adopters lack informed consent (snare extends further). If threshold is age 20: narrative framing rather than literacy gap is primary mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_threshold_timing, empirical, 'Age threshold for informed financial consent').

omega_variable(
    narrative_vs_structural_suppression,
    'Is the suppression mechanism primarily narrative (cultural framing that makes alternatives unthinkable) or structural (economic barriers that make alternatives unaffordable)?',
    'Cross-cultural comparison: societies with different life-script narratives but similar economic structures; cohort analysis of individuals who reject the narrative (do they face material barriers or social stigma?)',
    'If primarily narrative: suppression is internalized (identity_locked dynamics), and counter-narrative campaigns could reduce extraction. If primarily structural: suppression is material (trapped dynamics), and policy intervention (income floors, alternative credit models) is required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrative_vs_structural_suppression, conceptual, 'Whether suppression is narrative or structural').

omega_variable(
    generational_sunset_trajectory,
    'Is the financial literacy movement''s scaffold perspective empirically grounded, or is it aspirational?',
    'Trend analysis: age of first mortgage and car loan over time; adoption rates of alternative financial models (income-share agreements, cooperative housing, car-sharing); effectiveness of financial literacy curricula in delaying debt commitment',
    'If sunset is real: extraction will decline over 20-30 years as norms shift (scaffold confirmed). If sunset is aspirational: financial institutions will adapt the narrative faster than literacy spreads (scaffold collapses to snare or tangled rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_sunset_trajectory, empirical, 'Whether generational shift will sunset the constraint').

omega_variable(
    coordination_floor_magnitude,
    'How much of the measured extraction is genuine coordination cost (standardized products enable scale economies, predictable cohorts reduce risk premiums) vs pure rent extraction?',
    'Comparative analysis: interest rate spreads between informed and uninformed borrowers; profit margins in consumer finance vs other lending sectors; cross-national comparison of mortgage and auto loan terms in markets with different financial literacy levels',
    'If coordination floor is high (>0.30): constraint is tangled rope from more perspectives. If coordination floor is low (<0.15): constraint is snare from more perspectives. Determines whether the analytical tangled rope classification holds or collapses to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_floor_magnitude, empirical, 'Magnitude of genuine coordination cost vs rent extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(narrative_as_capture_device, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(narc_theater_1970, narrative_as_capture_device, theater_ratio, 0, 0.35).
narrative_ontology:measurement(narc_theater_1985, narrative_as_capture_device, theater_ratio, 15, 0.48).
narrative_ontology:measurement(narc_theater_2000, narrative_as_capture_device, theater_ratio, 30, 0.58).
narrative_ontology:measurement(narc_theater_2015, narrative_as_capture_device, theater_ratio, 45, 0.62).

% Extraction over time
narrative_ontology:measurement(narc_extract_1970, narrative_as_capture_device, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(narc_extract_1985, narrative_as_capture_device, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(narc_extract_2000, narrative_as_capture_device, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(narc_extract_2015, narrative_as_capture_device, base_extractiveness, 45, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(narrative_as_capture_device, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of obligation_floor_as_control (which sets baseline debt burden) and desire_cultivation_ratchet (which ensures the narrative remains aspirational). The narrative_as_capture_device is the mechanism by which those upstream constraints lock in: the life-script narrative channels young adults into the obligation floor before they understand its terms, and the desire ratchet ensures that the narrative's milestones (house, car, family) remain aspirational rather than cautionary even as debt burdens rise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
