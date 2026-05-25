% ============================================================================
% CONSTRAINT STORY: first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_held_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: first_held_reading
 *   human_readable: Digital Money as First-Held Non-Physical Instrument (Practical Store of Value Reading)
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   Digital money emerges as a practical store of value when individuals and
 *   institutions first hold non-physical monetary instruments — electronic
 *   balances in bank accounts, digital wallets, payment network credits — as
 *   reliable replacements for physical cash. This reading of the
 *   digital_money_origin kernel focuses on the infrastructural moment: when
 *   technology, institutional capacity, and individual access converge to
 *   enable ordinary people to hold and transfer monetary value without
 *   physical substrate. The constraint captures the coordination problem
 *   (monetary system modernization) alongside asymmetric extraction: those
 *   with infrastructure access (smartphones, internet, banking relationships)
 *   capture the benefits (efficiency, financial inclusion, seigniorage
 *   gains); those without access (unbanked populations, rural economies,
 *   infrastructure-excluded nations) bear the cost (exclusion from payments,
 *   cash depreciation, forced modernization). The theater_ratio rises from
 *   0.15 (1990s: genuine innovation and experimentation) to 0.35 (2010s:
 *   increasing regulatory theater and central bank performative responses as
 *   digital money becomes politically contested). Extractiveness rises from
 *   0.25 (emergence phase: technology barrier) to 0.52 (maturity phase:
 *   institutional/network effects consolidate). This is NOT the
 *   'became_thinkable_reading' (which would emphasize cognitive possibility
 *   and ideological acceptance) nor the 'regulatory_recognition_reading'
 *   (which would emphasize state legitimacy and legal status). This reading
 *   is purely about practical hold-ability — when non-physical money became a
 *   materially viable store of value for ordinary economic agents.
 *
 * KEY AGENTS:
 *   - Early Adopters with Infrastructure Access (institutional/arbitrage): banks, fintech startups, tech-enabled merchants, smartphone users in developed economies. Primary beneficiaries capturing efficiency gains and first-mover seigniorage.
 *   - Unbanked and Infrastructure-Excluded Populations (powerless/trapped): 1.7–2.0 billion people without bank accounts or reliable electricity/internet. Primary victims excluded from digital money coordination benefits.
 *   - Cash-Dependent Merchants and Populations (moderate/constrained): small retailers, informal economy workers, rural populations with partial infrastructure access. Secondary victims facing rising cash-handling costs and declining acceptance.
 *   - Government Monetary Authorities (organized/constrained): central banks, treasury departments. Constrained by dual mandate to coordinate monetary system modernization while maintaining financial inclusion and monetary sovereignty.
 *   - Financial Intermediaries (institutional/arbitrage): banks, payment processors, money transmitters. Primary beneficiaries with agency to choose technologies, timing, and market entry.
 *   - Technology Infrastructure Operators (institutional/arbitrage): telecom providers, internet providers, device manufacturers. Beneficiaries capturing value through infrastructure rent as digital money adoption requires their services.
 *   - Analytical Observer (analytical/analytical): risks naturalizing contingent institutional choices (which populations get access, which payment technologies dominate, which states maintain monetary control) as inevitable consequences of technology logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_held_reading, 0.52).
domain_priors:suppression_score(first_held_reading, 0.48).
domain_priors:theater_ratio(first_held_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_held_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(first_held_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(first_held_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_held_reading, tangled_rope).
narrative_ontology:human_readable(first_held_reading, "Digital Money as First-Held Non-Physical Instrument (Practical Store of Value Reading)").
narrative_ontology:topic_domain(first_held_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(first_held_reading, distributed).
narrative_ontology:cs_authority_grounding(first_held_reading, extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_held_reading, early_adopters_with_access).
narrative_ontology:constraint_beneficiary(first_held_reading, technology_infrastructure_operators).
narrative_ontology:constraint_beneficiary(first_held_reading, financial_intermediaries).
narrative_ontology:constraint_victim(first_held_reading, unbanked_populations).
narrative_ontology:constraint_victim(first_held_reading, infrastructure_excluded_agents).
narrative_ontology:constraint_victim(first_held_reading, cash_dependent_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNBANKED POPULATIONS (SNARE) — Trapped outside the digital money ecosystem. Cannot participate in the practical store-of-value function because they lack access to requisite infrastructure (electricity, internet, devices, banking relationships). Maximum extraction: excluded from the coordination benefits while bearing the social cost as digital money displaces cash acceptance. No exit option within the constraint structure.
constraint_indexing:constraint_classification(first_held_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CASH-DEPENDENT MERCHANTS AND POPULATIONS (TANGLED ROPE) — Constrained by rising costs of cash handling and declining acceptance infrastructure. The constraint coordinates monetary system transition (genuine coordination function) but asymmetrically extracts from those who transition latest or incompletely. Benefits from broader payment system integration exist, but access barriers and learning costs are distributed unevenly.
constraint_indexing:constraint_classification(first_held_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EARLY ADOPTER FINANCIAL INTERMEDIARIES (ROPE) — Primary beneficiaries (banks, fintech firms, payment processors). Experience the constraint as pure coordination: enabling customers to hold non-physical money solves the mutual problem of payments infrastructure modernization. Arbitrage exit — they can move across payment networks, select technologies, and shift between digital money types. Net beneficiary with high agency.
constraint_indexing:constraint_classification(first_held_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GOVERNMENT MONETARY AUTHORITIES (TANGLED ROPE) — Constrained by dual mandate: coordinate monetary system transition to digital infrastructure while maintaining financial inclusion and monetary sovereignty. Benefits from seigniorage efficiency and transaction oversight; bears cost of managing dual-currency transition, technological obsolescence, and inclusion gaps. Active enforcement required to maintain state authority over money creation.
constraint_indexing:constraint_classification(first_held_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY CASH INFRASTRUCTURE (PITON) — Physical cash production, sorting, and distribution systems persist through institutional inertia despite declining functional necessity. Central banks and currency printers maintain cash supplies, conduct anti-counterfeiting research, and manage physical currency retirement — largely performative functions as digital money rises. The theater persists because alternatives have not fully replaced cash and institutional actors have sunk costs in existing infrastructure.
constraint_indexing:constraint_classification(first_held_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, digital money emerges inevitably from the logic of networked information: once communication infrastructure exists, money-like instruments must follow because they are cheaper to transmit digitally than physically. This perspective views the first-held reading as an immutable consequence of technology diffusion. However, this naturalizes what is structurally a coordination constraint with identifiable beneficiaries — a false summit revealing that institutional choices (not physical laws) determine adoption.
constraint_indexing:constraint_classification(first_held_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_held_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(first_held_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(first_held_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_held_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(first_held_reading, TR),
    TR >= 0.70.

:- end_tests(first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderately high, reflecting genuine infrastructure barriers and network effects. The constraint is not pure coordination (Rope) because access is asymmetric — early adopters with device/electricity/banking access capture benefits while the excluded bear costs without receiving coordination benefits. The constraint is not pure extraction (Snare) because significant genuine coordination occurs: digital money does solve real monetary system problems (transaction speed, settlement efficiency, financial transparency). The moderate level reflects that the extraction mechanism is primarily access-based (who has infrastructure) rather than coercive (forced participation with minimal benefit). Suppression (0.48): Moderate. Barriers include lack of devices (unbanked populations), lack of electricity/internet (infrastructure-excluded regions), lack of banking relationships (formal economy exclusion), lack of digital literacy, and policy/regulatory constraints. But suppression is not total — some groups can and do transition through mobile money alternatives (M-Pesa model), government programs, or informal digital networks. Theater ratio (0.35): Low-to-moderate. The constraint exhibits relatively little performative activity compared to legacy cash systems — digital money deployment is increasingly functional and verified through use. Theater increases over time as regulatory authorities create compliance theater around anti-money-laundering, know-your-customer rules, and central bank digital currency pilots (pushing ratio upward in later period). The claimed_type (tangled_rope) reflects the genuine coordination (monetary system modernization) plus the asymmetric extraction (access-based exclusion).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how indexical classification captures real structural differences in lived experience. The early-adopter institution experiences the constraint as pure coordination (Rope) — the system solves problems they care about and they have agency to shape its evolution. The unbanked population experiences it as pure extraction (Snare) — they are excluded from the coordination benefits and have no capacity to exit or reshape the system. The cash-dependent merchant experiences it as mixed (Tangled Rope) — the system offers genuine benefits (faster payment processing) alongside genuine costs (rising complexity, device requirements, learning burden). These are not differences in opinion or perspective on the same constraint — they are differences in the constraint's actual structural properties relative to each agent's position. The perspectival gap is objective.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position: their institutional power, infrastructure access, exit options, and relationship to the extraction flow. Early adopters (beneficiaries with arbitrage options) derive low d ≈ 0.10–0.20 → negative χ (they experience the constraint as beneficial). Monetary authorities (organized actors with constrained options) derive moderate d ≈ 0.50–0.60 → moderate χ (mixed experience). Cash-dependent populations (moderate power, constrained exit) derive d ≈ 0.65–0.75 → elevated χ (experiencing asymmetric cost). Unbanked populations (powerless, trapped by infrastructure) derive d ≈ 0.95 → maximum χ (maximum experienced extraction). The powerless perspective produces Snare because trapped exit + victim status → d ≈ 0.95 → f(d) ≈ 1.42 → χ ≥ 0.66 even with moderate ε. The institutional perspective (beneficiaries) produces Rope despite same ε because arbitrage exit + beneficiary status → d ≈ 0.15 → f(d) ≈ -0.01 → χ ≤ 0.35.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy (coordination vs. extraction classification ambiguity) by showing that BOTH coordination and extraction are genuine structural features. Digital money performs real coordination — it solves legitimate monetary system problems (transaction speed, efficiency, financial oversight). Simultaneously, it extracts asymmetrically from those without infrastructure access. The tangled_rope classification captures both: the constraint coordinates for some agents (early adopters, financial intermediaries) while extracting from others (unbanked, infrastructure-excluded). This is not a matter of perspective or framing — it is a structural asymmetry in the constraint's function. Early adopters experience benefits from coordination without extraction; excluded populations experience extraction without coordination benefits; intermediate populations experience both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    first_held_definition_boundary,
    'What constitutes ''first held as practical store of value'' — does this reading begin when digital instruments were technically possible, when they became sufficiently reliable, when ordinary individuals could access them, or when they achieved material substitution for physical money?',
    'Historical periodization: (1) technical possibility (ARPANET 1969+, email money proposals 1980s), (2) institutional deployment (DigiCash 1990s, PayPal 1998, mobile money 2000s), (3) ordinary adoption (M-Pesa Kenya 2007, smartphone banking 2010s), (4) monetary displacement (Bitcoin 2009+, CBDC pilots 2020s). Each boundary yields different constraint origin dates and different victim/beneficiary sets.',
    'Earlier boundary (technical possibility): extractiveness ≈0.25–0.30 (mountain/rope territory), beneficiaries are technology researchers/infrastructure operators, victims are abstract future populations. Later boundary (practical adoption/displacement): extractiveness ≈0.55–0.65 (tangled_rope/snare territory), beneficiaries are early adopters with access, victims are unbanked and infrastructure-excluded. The reading''s ε is definition-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(first_held_definition_boundary, conceptual, 'Definition boundary for ''first held as practical store of value''').

omega_variable(
    sibling_reading_became_thinkable,
    'The ''became_thinkable_reading'' instantiates digital money as emerging when it became conceptually possible for ordinary people to imagine non-physical money as legitimate. How does this sister reading''s ε differ from the first_held_reading''s ε=0.52?',
    'The became_thinkable_reading likely has lower ε (≈0.20–0.35, rope/scaffold territory) because cognitive shifts require less extraction enforcement than practical infrastructure deployment. Became_thinkable emphasizes ideological shifts (decoupling money from physical substance in collective imagination); first_held emphasizes technological barriers and network effects (the infrastructure-dependent extraction mechanism). Different ε → different constraint type → different structural analysis.',
    'If became_thinkable_reading''s ε is significantly lower (0.10–0.30 range): the cognition reading is a mountain or rope — the ideological framing is nearly inevitable once information technology exists. If first_held''s ε=0.52 reflects actual implementation barriers: the practical reading is tangled_rope — genuine coordination plus asymmetric extraction. The two readings represent different structural mechanisms (cognitive vs. infrastructural) and should not be conflated into a single constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_became_thinkable, conceptual, 'Structural difference between became_thinkable and first_held readings').

omega_variable(
    sibling_reading_regulatory_recognition,
    'The ''regulatory_recognition_reading'' instantiates digital money as emerging when state/regulatory authorities officially recognized non-physical instruments as legal tender or payment mechanisms. How does this reading''s extracted constraint differ structurally?',
    'Regulatory_recognition_reading ε likely reflects state capacity and institutional coordination requirements (≈0.40–0.50, tangled_rope territory). The constraint would emphasize state monopoly on money recognition and the bargaining between private digital innovators and regulatory authorities. Beneficiaries would include states gaining tax/monetary control; victims would include private money issuers facing legal restrictions.',
    'If regulatory_recognition''s constraint has ε near 0.40–0.50 and differs primarily in beneficiary/victim framing (states vs. early adopters): the readings are observationally distinguishable but structurally related — both are tangled_rope, differing in which institutional actor captures primary extraction. If regulatory_recognition has markedly different ε (e.g., 0.25 or 0.70): it represents a genuinely distinct structural mechanism (either lighter coordination or heavier extraction) and should be written as a separate family member.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_regulatory_recognition, conceptual, 'Structural difference between regulatory_recognition and first_held readings').

omega_variable(
    infrastructure_access_threshold,
    'What threshold of population infrastructure access (% with internet/electricity/devices) is required for digital money to function as a practical store of value at scale?',
    'Global survey data: countries achieving digital money adoption show infrastructure access thresholds of 45–65% (smart phones + internet + banking infrastructure). Below these thresholds, cash alternatives persist. Analysis of adoption curves: whether constraint becomes binding at 30%, 50%, 70%, or only at 90%+ access.',
    'If threshold is low (30–40%): digital money operates as snare for the unbanked majority in threshold-countries; constraint''s extractiveness rises. If threshold is high (70%+): digital money functions as coordination for most populations; constraint''s extractiveness falls; the snare perspective is temporally limited (applies only during transition). Affects whether this reading covers emergence only, or emergence plus sustained extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_access_threshold, empirical, 'Infrastructure access threshold for digital money practical adoption').

omega_variable(
    network_effects_lock_in,
    'Once digital money infrastructure reaches critical mass in a population (≈60%+ adoption), does the constraint''s mechanism shift from coordination/access-based to network-effect lock-in-based extraction?',
    'Comparative case study: countries at different adoption phases. Early-adoption phase (10–40%): constraint is access/coordination problem. Critical-mass phase (40–70%): constraint is mixed coordination-extraction. Post-critical-mass (70%+): constraint is lock-in-based (cash options disappear, holdouts face surcharges/exclusion). Measure extractiveness change across phases.',
    'If network effects create phase transition: a single constraint story cannot span the full lifecycle — need separate stories for emergence/early-adoption vs. mature/lock-in phases. The first_held_reading would cover the emergence phase only; a sister ''lock_in_reading'' would cover post-critical-mass extraction. Different ε values → different constraint types → different structural analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_lock_in, empirical, 'Phase transition: emergence coordination vs. mature network-effect lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_held_reading, 1990, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fhr_theater_1990, first_held_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fhr_theater_2000, first_held_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(fhr_theater_2010, first_held_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(fhr_extractiveness_1990, first_held_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(fhr_extractiveness_2000, first_held_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(fhr_extractiveness_2010, first_held_reading, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_held_reading, resource_allocation).
narrative_ontology:affects_constraint(first_held_reading, became_thinkable_reading).
narrative_ontology:affects_constraint(first_held_reading, regulatory_recognition_reading).
narrative_ontology:affects_constraint(first_held_reading, cash_system_depreciation).
narrative_ontology:affects_constraint(first_held_reading, financial_inclusion_constraint).

% DUAL FORMULATION NOTE:
% The first_held_reading is part of a three-member constraint family decomposing the contested kernel 'digital_money_origin'. Each reading instantiates a structurally distinct constraint with different ε values and extraction mechanisms: (1) first_held_reading (ε=0.52, tangled_rope) — infrastructural barriers and network effects; (2) became_thinkable_reading (ε≈0.25–0.35, rope) — cognitive shifts and ideological acceptance; (3) regulatory_recognition_reading (ε≈0.40–0.50, tangled_rope) — state authority and legal recognition. The readings share a domain kernel but represent different structural mechanisms. Do not conflate into a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
