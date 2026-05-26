% ============================================================================
% CONSTRAINT STORY: regulatory_recognition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_recognition_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: regulatory_recognition_reading
 *   human_readable: Regulatory Recognition as the Origin of Digital Money
 *   domain: monetary_history/institutional_economics/technology_regulation
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the contested kernel
 *   'what is the origin of digital money?' — specifically, the regulatory
 *   recognition reading. This reading claims that digital money emerged when
 *   monetary authorities formally incorporated it into statistical aggregates
 *   (M1, M2), regulatory frameworks (capital requirements, reserve rules),
 *   and policy transmission mechanisms. This is distinct from technological
 *   emergence (when the first digital payment system was built) and adoption
 *   emergence (when users first began using digital systems at scale). The
 *   regulatory reading has the latest date and produces a specific constraint
 *   structure: incumbent financial institutions and central banks benefit
 *   from controlling the institutional definition of 'money,' while
 *   unregulated innovators and early adopters face extraction through
 *   compliance barriers and regulatory risk. The constraint exhibits tangled
 *   rope structure because it combines genuine coordination functions (money
 *   supply definition, financial stability tracking) with asymmetric
 *   extraction (restricting which payment systems count as legitimate). The
 *   constraint's extractiveness has increased from 0.35 to 0.58 over the
 *   measurement interval (corresponding to the period 2010-2020), reflecting
 *   accumulating regulatory restrictions, KYC/AML enforcement, and
 *   institutional incorporation of digital payment systems. Theater ratio
 *   remains relatively low (0.32-0.45) because the regulatory apparatus
 *   performs substantial policy functions; it is not purely theatrical,
 *   though it contains significant performative elements (financial stability
 *   monitoring that doesn't actually prevent crises).
 *
 * KEY AGENTS:
 *   - Incumbent Financial Institutions: Primary beneficiary (institutional/arbitrage) — benefit from regulatory exclusivity and definitional gatekeeping; can arbitrage between jurisdictions; experience the constraint as protective coordination
 *   - Central Banking Authority: Secondary beneficiary (institutional/constrained) — benefits from policy instrument stability and money supply tracking; but constrained by need to coordinate actual financial stability and prevent system-level risks
 *   - Unregulated Innovators: Primary victim (powerless/trapped) — face capital requirements, licensing mandates, KYC/AML enforcement, and consumer protection rules; cannot operate without compliance; cannot exit jurisdiction without abandoning market
 *   - Early Adopters and Small Merchants: Secondary victim (moderate/constrained) — benefit from payment network but bear regulatory risk, platform dependency, and potential asset seizure; constrained by regulatory uncertainty
 *   - Decentralized Payment Networks: Tertiary victim (organized/mobile) — operate outside regulatory recognition; cannot access banking rails; persist through institutional inertia despite degraded real-world function (piton perspective)
 *   - Analytical Observer: Observational position (analytical/analytical) — reveals that regulatory recognition is contingent institutional choice, not inevitable technological development; risks naturalizing definitional closure as natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_recognition_reading, 0.58).
domain_priors:suppression_score(regulatory_recognition_reading, 0.68).
domain_priors:theater_ratio(regulatory_recognition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_recognition_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_recognition_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(regulatory_recognition_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(regulatory_recognition_reading, "Regulatory Recognition as the Origin of Digital Money").
narrative_ontology:topic_domain(regulatory_recognition_reading, "monetary_history/institutional_economics/technology_regulation").

domain_priors:requires_active_enforcement(regulatory_recognition_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:constraint_beneficiary(regulatory_recognition_reading, central_banking_authorities).
narrative_ontology:constraint_victim(regulatory_recognition_reading, unregulated_innovators).
narrative_ontology:constraint_victim(regulatory_recognition_reading, early_adopters).
narrative_ontology:constraint_victim(regulatory_recognition_reading, decentralized_payment_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNREGULATED INNOVATORS (SNARE) — Early digital money developers (crypto pioneers, payment processors, decentralized network operators) face regulatory barriers: capital requirements, licensing mandates, KYC/AML enforcement, consumer protection rules. They cannot exit the jurisdiction without abandoning their market; they cannot operate without regulatory compliance. The constraint extracts compliance costs and forces institutional adaptation. No coordination benefit is experienced — only the coercive requirement to conform to regulatory definitions of 'money.'
constraint_indexing:constraint_classification(regulatory_recognition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EARLY ADOPTER ECOSYSTEM (TANGLED ROPE) — Users and small merchants who adopted digital payment systems before regulatory recognition experience both coordination (access to a functioning payment network) and extraction (regulatory risk, platform dependency, potential asset seizure). They benefit from network effects but bear the cost of regulatory uncertainty and institutional fragility. Moderate extraction with genuine coordination function.
constraint_indexing:constraint_classification(regulatory_recognition_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FINANCIAL INSTITUTIONS (ROPE) — Central banks and commercial banks experience the regulatory framework as coordination infrastructure. The constraint standardizes what counts as 'money' for statistical and policy purposes, creating a clear legal perimeter they control. They benefit from regulatory exclusivity and can arbitrage between jurisdictions. The constraint functions as protective coordination — defining digital money INTO the regulatory system ensures incumbents maintain gatekeeping power over new payment forms.
constraint_indexing:constraint_classification(regulatory_recognition_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CENTRAL BANKING AUTHORITY (TANGLED ROPE) — Central banks must coordinate monetary policy, financial stability monitoring, and payment system oversight. Digital money's regulatory incorporation serves genuine coordination functions (money supply tracking, systemic risk assessment, consumer protection). But the authority also extracts power and institutional relevance by controlling the definition of 'money' — any payment form that escapes regulatory recognition threatens their policy instruments. Active enforcement is required because unregulated digital systems exist and would function without official recognition.
constraint_indexing:constraint_classification(regulatory_recognition_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DECENTRALIZED PAYMENT NETWORK COMMUNITY (PITON) — Open-source communities (Bitcoin, Ethereum, payment protocols) continue to operate outside regulatory recognition. Their institutional function is degraded — they cannot access banking rails, cannot settle in fiat currency without intermediaries, cannot participate in official statistics. Yet they persist through institutional inertia (network effects, user commitment) despite minimal real-world function as 'money' by regulatory definition. The theatrical performance is network loyalty; the functional coordination is severely constrained.
constraint_indexing:constraint_classification(regulatory_recognition_reading, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, digital money 'emerged' at the regulatory recognition point because the institutional definition fixed what counts as money in the formal system. But this emergence is contingent: digital systems functioned as payment mechanisms decades before regulatory incorporation. The constraint combines genuine coordination (the need to define and track money supply) with extraction (the power to define what counts as money and which systems are legitimate). The analytical view reveals that regulatory recognition was not inevitable but was a choice that benefited incumbents and constrained alternatives.
constraint_indexing:constraint_classification(regulatory_recognition_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_recognition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_recognition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_recognition_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_recognition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_recognition_reading, TR),
    TR >= 0.70.

:- end_tests(regulatory_recognition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The regulatory incorporation creates substantial extraction costs for unregulated systems: compliance infrastructure, licensing, KYC/AML operations, regulatory reporting. Early adopters and innovators bore these costs while incumbents captured definitional power. However, 0.58 rather than 0.75+ because genuine coordination functions exist — money supply tracking and financial stability monitoring are real institutional needs that regulatory incorporation serves. The constraint is not pure extraction like a debt trap (snare) but hybrid coordination-extraction. Suppression (0.68): High. Multiple barriers prevent exit and alternatives: regulatory barriers (legal prohibition on unregulated money transmission), institutional barriers (access to banking rails requires regulatory approval), economic barriers (compliance costs eliminate small-scale operators), and informational barriers (regulatory authority can redefine what counts as 'money'). Suppression increased over time as KYC/AML enforcement intensified. Theater ratio (0.45): Moderate-low. The regulatory apparatus performs genuine policy functions (money supply aggregation, financial stability monitoring) but also performs theater (consumer protection claims that don't prevent fraud, stability claims that don't prevent crises). Theater is lower than pure performative constraints because the coordination functions are real, even if imperfect.
 *
 * PERSPECTIVAL GAP:
 *   The constraint is a case study in how the same institutional mechanism reads as beneficial (incumbents' view) or extractive (innovators' view) depending on structural position. From the regulatory authority's perspective, the constraint solves a genuine problem: defining money supply for policy transmission. From the unregulated innovator's perspective, the constraint uses policy rationale as cover for restricting competition. Both perspectives are factually accurate — the constraint does solve a policy problem AND does restrict competition. The gap reveals that 'solving a real problem' and 'restricting competition' are not mutually exclusive; they are two aspects of the same institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Each actor's experienced extractiveness is computed from beneficiary/victim status + exit options. Institutional beneficiaries with arbitrage options (can operate globally, can lobby regulatory change, can relocate) experience low d. Powerless innovators with trapped exit (cannot operate without license, cannot escape jurisdiction, face seizure) experience high d. The analytical observer at global scope sees this as a global coordination problem (defining money across jurisdictions) with embedded extraction (each jurisdiction's definition privileges incumbents). No single 'correct' directionality — the computation reveals structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED (mandatrophy_resolved = false). The constraint exhibits the core mandatrophy: it simultaneously coordinates (defines money supply, enables policy transmission) and extracts (restricts what payment systems count as legitimate, concentrates gatekeeping power). The tangled rope classification is precise — all three gates are satisfied (beneficiaries, victims, active enforcement). But the fundamental tension remains: regulators justify the constraint on coordination grounds (we need to track money supply); incumbents benefit from the extraction; innovators bear the cost. Resolving the mandatrophy would require answering: Would the coordination function persist if the extraction mechanism were removed? If regulators could define money for policy purposes WITHOUT restricting what payment systems could operate, would financial stability improve or degrade? This is an empirical question about whether the current regulatory architecture is the minimal coordination mechanism or an extracted surplus on top of coordination. Current evidence is ambiguous — some jurisdictions (Switzerland, Singapore) achieve coordination with lower extraction; others (US, EU) maintain higher regulatory barriers. Mandatrophy remains open pending comparative institutional analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_vs_institutional_emergence,
    'Does ''emergence'' refer to technological existence, practical adoption, or institutional recognition? Each reading produces different origin dates and different constraint structures.',
    'Historical decomposition: (a) First functional digital payment implementation (1980s); (b) First mass adoption (2000s mobile money, Bitcoin 2009); (c) First regulatory incorporation (2010s+ depending on jurisdiction). Assess which observable was actually measured when scholars claim ''digital money emerged.''',
    'This constraint (regulatory_recognition_reading) assumes institutional recognition as the emergence criterion, yielding the latest date. Sibling readings (technological_existence, first_adoption) would have earlier dates, different beneficiary/victim structures, and different constraint types. Regulatory reading fixes the ''origin'' by definitional closure, while earlier readings show digital money functioned before recognition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_vs_institutional_emergence, conceptual, 'Technological emergence vs. institutional recognition as the origin criterion').

omega_variable(
    coordination_necessity_vs_extraction_benefit,
    'Does regulatory recognition primarily serve the coordination function (standardizing money supply definition for policy) or the extraction function (granting incumbents gatekeeping power over what counts as legitimate payment)?',
    'Counterfactual analysis: What would happen if digital systems were recognized WITHOUT the regulatory restrictions? If financial stability improved (or remained stable), coordination hypothesis is falsified and the constraint is primarily extractive. If regulatory restrictions enable genuine policy functions that would fail without them, coordination hypothesis is supported.',
    'If primarily extractive (likely): Snare classification is more accurate than Tangled Rope. If primarily coordinating (unlikely): Tangled Rope is accurate. Classification outcome determines mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_vs_extraction_benefit, empirical, 'Whether regulatory recognition serves coordination or extraction').

omega_variable(
    reading_contingency_kernel_ambiguity,
    'This constraint instantiates ONE reading of the kernel ''what is the origin of digital money?'' Other readings (technological_existence_reading, first_adoption_reading) measure different observables and produce different ε values and constraint types. Is the kernel''s ambiguity itself a constraint — is there institutional benefit to maintaining multiple simultaneous readings so that actors can invoke whichever serves their interests?',
    'Discourse analysis: Track which reading is invoked in regulatory, academic, and industry contexts. If regulators invoke ''recognition as origin'' while innovators invoke ''first adoption as origin,'' the ambiguity itself stabilizes extractive arrangements. If readings are treated as mutually exclusive rather than complementary, the kernel is contested rather than merely ambiguous.',
    'If kernel ambiguity is weaponized: the constraint family (regulatory_recognition + technological_existence + adoption_reading) is itself an extractive apparatus maintaining definitional uncertainty. The three stories should be linked with notes on strategic invocation. If readings are genuinely incommensurate: three separate constraint families with no cross-story network links.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contingency_kernel_ambiguity, conceptual, 'Whether kernel ambiguity about digital money''s origin is strategically weaponized').

omega_variable(
    regulatory_capture_signature,
    'Does the regulatory incorporation of digital money represent genuine central bank policy capacity or regulatory capture by incumbent financial institutions? The two read the same institutional structure very differently.',
    'Regulatory history analysis: Did central banks independently determine that digital money required incorporation, or did they respond to incumbent bank lobbying? Track meeting minutes, regulatory comment periods, and central bank policy papers. If capture: incumbent pressure drove the regulatory expansion. If autonomous policy: central banks determined incorporation served their functions.',
    'If captured: the institutional beneficiary is primarily incumbent banks, not central banks — perspective 4 classification should be downgraded from Rope to identity_locked Snare (institution cognitively captured by captured industry preference). If autonomous: central banking authority genuinely experiences this as coordination, and Rope classification is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_signature, empirical, 'Whether regulatory incorporation reflects central bank policy autonomy or incumbent capture').

omega_variable(
    falsity_of_emergence_timeline,
    'Is the regulatory recognition reading''s implicit claim true — that digital money only ''emerged'' when formally recognized? Or does this reading conflate institutional incorporation with technological/social emergence?',
    'Historical reconstruction: Document functional digital payment systems that existed before regulatory recognition in each jurisdiction. Bitcoin (2009) predates regulatory recognition in most jurisdictions by 5-10 years. Mobile money (M-Pesa, 2007) functioned as de facto digital currency in Kenya before banking system incorporation. If digital systems demonstrably functioned before recognition, the claim that recognition = emergence is false.',
    'If false: the regulatory reading mystifies institutional power as natural law (''that''s when it emerged''). The constraint is primarily extractive — the reading naturalize incumbents'' definitional victory. If true (digital systems did not function until recognized): the reading accurately captures when payment systems achieved stable institutional form.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(falsity_of_emergence_timeline, empirical, 'Whether digital money functionally existed before regulatory recognition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_recognition_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regulatory_theater_early, regulatory_recognition_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(regulatory_theater_mid, regulatory_recognition_reading, theater_ratio, 3, 0.39).
narrative_ontology:measurement(regulatory_theater_late, regulatory_recognition_reading, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(regulatory_extract_early, regulatory_recognition_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regulatory_extract_mid, regulatory_recognition_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(regulatory_extract_late, regulatory_recognition_reading, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_recognition_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(regulatory_recognition_reading, 0.18).
narrative_ontology:affects_constraint(regulatory_recognition_reading, cbdc_monetary_sovereignty).
narrative_ontology:affects_constraint(regulatory_recognition_reading, stablecoin_regulatory_arbitrage).
narrative_ontology:affects_constraint(regulatory_recognition_reading, banking_system_disruption_risk).

% DUAL FORMULATION NOTE:
% This constraint (regulatory_recognition_reading) is part of a constraint family decomposing 'digital money origin' kernel. The technological_existence_reading and first_adoption_reading are separate constraint stories with different ε values (likely 0.25-0.35 for technical emergence, 0.40-0.50 for adoption emergence). All three stories share the same beneficiary/victim structure at the macro level (incumbents vs innovators) but operate at different timescales and through different mechanisms. The regulatory reading shows the latest origin date and the highest extractiveness because it captures the institutional closure point — when the definition became legally fixed. Earlier readings show digital money functioning before that closure, revealing that recognition was not inevitable but was a policy choice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_recognition_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
