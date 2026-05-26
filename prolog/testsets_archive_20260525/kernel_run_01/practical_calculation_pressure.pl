% ============================================================================
% CONSTRAINT STORY: practical_calculation_pressure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_practical_calculation_pressure, []).

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
 *   constraint_id: practical_calculation_pressure
 *   human_readable: Practical Calculation Pressure and Epistemological Resistance to Zero
 *   domain: history_of_mathematics/epistemology/philosophy
 *
 * SUMMARY:
 *   Zero's entry into Western mathematics (12th-16th centuries) presents a
 *   structurally complex constraint that appears to differ fundamentally
 *   depending on the observer's position within mathematical culture. Indian
 *   merchants and administrators developed zero pragmatically—as a solution
 *   to accounting and astronomical calculation. Islamic administrative
 *   apparatus inherited and systematized this innovation. European resistance
 *   emerged not from mathematical objection but from philosophical
 *   incoherence: classical Greek definition of number as magnitude of units
 *   cannot accommodate zero. Yet practical pressure—double-entry bookkeeping,
 *   commercial calculation, navigational astronomy—forced adoption despite
 *   the philosophical cost. The constraint exhibits genuine coordination
 *   function (simplified arithmetic solves real problems) alongside
 *   asymmetric extraction (philosophical coherence must be abandoned). The
 *   transition was neither inevitable nor forced purely by logic: it resulted
 *   from pragmatic pressure that eventually overcame institutional and
 *   philosophical resistance. The university establishment maintains
 *   theater—teaching Euclidean geometry as true mathematics while students
 *   use zero for practical calculation—indicating functional degradation of
 *   mathematical authority. Open-science-like intellectual coalition
 *   (Fibonacci, printing press, merchant guilds) circumvents university
 *   control, creating alternative knowledge pathways that normalize decimal
 *   arithmetic outside academic institutional gates.
 *
 * KEY AGENTS:
 *   - Classical Greek Philosophical Framework: Victim (powerless/trapped) — definition of number as magnitude cannot accommodate zero; must either dissolve or reject practical calculation
 *   - European Philosophical Coherence: Victim (powerless/trapped) — abstract category bound by Greek authority; zero adoption forces either contradiction or abandonment of classical foundation
 *   - Indian Merchant and Administrator Class: Beneficiary (powerful/arbitrage) — zero emerges from their counting and accounting needs; enables commerce and administration
 *   - Islamic Administrative Apparatus: Beneficiary (institutional/arbitrage) — inherits and systematizes Indian mathematics; operational necessity for 500+ years before European contact
 *   - European Commercial Institutions: Beneficiary (institutional/arbitrage) — double-entry bookkeeping, price tracking, inventory all simplified by positional notation
 *   - European Natural Philosophers: Mixed (moderate/constrained) — must choose between classical authority and computational utility; both costs and benefits
 *   - Medieval University Establishment: Inertial actor (institutional/constrained) — maintains classical framework performatively while adopting zero pragmatically
 *   - Intellectual Reformers (Fibonacci, Pacioli, Printing Press): Organized agents (organized/constrained) — build alternative knowledge pathways outside university control
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing pragmatic choice as mathematical inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(practical_calculation_pressure, 0.38).
domain_priors:suppression_score(practical_calculation_pressure, 0.52).
domain_priors:theater_ratio(practical_calculation_pressure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(practical_calculation_pressure, extractiveness, 0.38).
narrative_ontology:constraint_metric(practical_calculation_pressure, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(practical_calculation_pressure, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(practical_calculation_pressure, tangled_rope).
narrative_ontology:human_readable(practical_calculation_pressure, "Practical Calculation Pressure and Epistemological Resistance to Zero").
narrative_ontology:topic_domain(practical_calculation_pressure, "history_of_mathematics/epistemology/philosophy").

domain_priors:requires_active_enforcement(practical_calculation_pressure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(practical_calculation_pressure, indian_merchant_class).
narrative_ontology:constraint_beneficiary(practical_calculation_pressure, islamic_administrative_apparatus).
narrative_ontology:constraint_beneficiary(practical_calculation_pressure, european_commercial_institutions).
narrative_ontology:constraint_victim(practical_calculation_pressure, philosophical_coherence).
narrative_ontology:constraint_victim(practical_calculation_pressure, classical_greek_geometric_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PHILOSOPHICAL COHERENCE (SNARE) — The category of number itself is trapped by the constraint. Greek geometric philosophy (Euclid, Aristotle) has no logical tools to incorporate zero without dissolving foundational definitions (magnitude cannot be zero; number is quantity of units). Zero's adoption forces either accepting internal contradiction or abandoning the entire framework. This abstract victim bears maximum extraction with no exit — the philosophical system must either transform beyond recognition or reject practical calculation entirely.
constraint_indexing:constraint_classification(practical_calculation_pressure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EUROPEAN NATURAL PHILOSOPHERS (TANGLED ROPE) — Face constraints: adopting zero requires admitting non-classical definitions of number; rejecting it means accepting cumbersome Roman numerals for increasingly complex calculations needed in commerce and astronomy. Both coordination benefit (simplified arithmetic) and asymmetric extraction (must abandon classical authority) coexist. Career risk from challenging Greek authority balanced against practical need for computational efficiency.
constraint_indexing:constraint_classification(practical_calculation_pressure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EUROPEAN COMMERCIAL INSTITUTIONS (ROPE) — See zero as pure coordination mechanism. Double-entry bookkeeping, ledger calculations, price and inventory tracking all become dramatically simpler with positional notation and zero. The constraint facilitates their core function. No extraction experienced — zero is a resource that enables operation. Institutional arbitrage: can switch calculation systems as needed, benefits from the technology.
constraint_indexing:constraint_classification(practical_calculation_pressure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ISLAMIC ADMINISTRATIVE APPARATUS (ROPE) — Zero and decimal place-value have been operational necessity for 500+ years (inherited Indian mathematics, tax administration, geometric optics). The constraint is pure coordination — enables governance and technical calculation. No philosophical problem experienced because adoption has already resolved through practice (pragmatic epistemology). Arbitrage: can maintain multiple numeric systems simultaneously for different contexts.
constraint_indexing:constraint_classification(practical_calculation_pressure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: INDIAN MERCHANT CLASS (ROPE) — Zero emerged as pragmatic solution to counting problems: accounting, astronomical calculation, measurement. No philosophical coherence crisis — numerals are tools, not ontological claims. The constraint solves genuine coordination problems in commerce and administration. Powerful + arbitrage: can choose calculation contexts; experiences zero as enabling rather than extractive.
constraint_indexing:constraint_classification(practical_calculation_pressure, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: MEDIEVAL UNIVERSITY ESTABLISHMENT (PITON) — Maintains elaborate defenses of classical mathematics and Euclidean geometry as the framework within which all calculation must occur. Adopts zero gradually, performatively — teaching dual systems (Roman numerals as pure mathematics, Arabic numerals as applied computation), maintaining the fiction that true mathematics is geometrical while mechanics use the practical tool. Theater increases as the gap between teaching (Euclidean geometry) and utility (zero-based calculation) widens. Function has degraded: university mathematics becomes scholastic exercise disconnected from computational reality.
constraint_indexing:constraint_classification(practical_calculation_pressure, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: INTELLECTUAL REFORMERS (SCAFFOLD) — Organized agents (Fibonacci, Pacioli, early printing press) see zero adoption as a temporary coordination failure with a clear sunset: once decimal arithmetic is systematized and transmitted through printed texts, the epistemological resistance collapses. Open advocacy for zero (Liber Abaci, Summa) creates parallel knowledge pathways outside university control. Low effective extraction because reformers have agency and see the constraint as solvable within generational timeframe. Sunset: achieved by 16th century when printing standardizes decimal notation.
constraint_indexing:constraint_classification(practical_calculation_pressure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, positional notation with zero is mathematically inevitable once counting systems scale beyond a certain complexity threshold. Any base-n system with place values requires a placeholder symbol, and zero is that placeholder by logical necessity. The constraint appears to be a universal fact of mathematical development — not a historical contingency but an inherent consequence of how numbering systems evolve. However, this risks naturalizing what is actually a contingent choice with epistemological resistance. FALSE SUMMIT CANDIDATE.
constraint_indexing:constraint_classification(practical_calculation_pressure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(practical_calculation_pressure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(practical_calculation_pressure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(practical_calculation_pressure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(practical_calculation_pressure, TR),
    TR >= 0.70.

:- end_tests(practical_calculation_pressure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Zero's adoption extracts from classical philosophy but benefits computation and commerce. The extraction is real—Greek epistemology is damaged—but not maximized because merchants and administrators do not experience cost. The value reflects that computational benefit is substantial but philosophical loss is real. Suppression (0.52): Moderate. Classical authority creates genuine resistance (philosophical frameworks deeply embedded in curriculum and prestige); practical utility creates pressure (accounting and astronomy demand decimal notation). Neither force is total — some merchants use Roman numerals anyway, some philosophers adopt zero pragmatically. Theater ratio (0.58): Moderate-high and rising over the interval. Universities teach Euclidean geometry as the 'real' mathematics while students practice zero-based arithmetic, creating performative gap. Theater increases as complexity of calculation outpaces classical framework's utility — the ritual of teaching geometry persists longer than its functional purpose. The measurement shows theater rising from 0.35 (early adoption, zero seen as merchant tool, not mathematical) to 0.58 (later adoption, dual systems taught side-by-side, gap between doctrine and practice widening).
 *
 * PERSPECTIVAL GAP:
 *   This constraint illustrates the full tension between pragmatic and philosophical assessment of a mathematical innovation. The merchant and administrator classes experience zero as pure coordination (Rope)—it solves their counting problems. Islamic apparatus also experiences coordination (Rope)—operational necessity for centuries. European commercial institutions similarly experience coordination (Rope)—enables bookkeeping. But European philosophers and universities experience extraction (Snare) from classical authority, or mixed cost-benefit (Tangled Rope) if they adopt zero while defending Euclidean framework. The intellectual reformers experience zero as a temporary problem with a sunset (Scaffold)—printing and open advocacy will overcome university resistance. The university establishment experiences its own degradation (Piton)—maintains ritual of classical geometry while practicality moves elsewhere. The analytical observer risks seeing mathematical inevitability (Mountain)—zero is logically necessary for place-value systems—but this naturalizes what is actually a historical contingency driven by merchant pressure. The perspectival gap reveals that zero's adoption was not determined by logic alone but by the economic power of merchants and administrators to force philosophical reconstruction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by structural position relative to the extraction flow. Beneficiaries (merchants, administrators, commercial institutions) have d near 0 (experience zero as enabling, not extractive)—powerful actors with arbitrage options who choose adoption because it serves them. Victims (classical philosophy, philosophical coherence) have d near 1 (experience maximum extraction)—powerless, trapped—cannot exit the encounter with zero without dissolving their foundational commitments. Mixed agents (European philosophers) have d near 0.55 (symmetric, experiencing both cost and benefit)—constrained exit because adopting zero requires abandoning classical authority but rejecting it means rejecting utility. The organized reformers (intellectual coalition) have d around 0.35-0.40 (mostly beneficiaries who see the constraint as temporary)—can organize around open pathways that bypass institutional resistance. The piton institutional agent (university) has d around 0.50 (inertial, neither clear beneficiary nor clear victim, just maintaining ritual)—constrained to keep teaching classical geometry even as practical mathematics moves elsewhere. The analytical observer has high d (0.72-0.73) if treating zero adoption as inevitable law, but this derives from naturalizing the pragmatic pressure, not from logical necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is the tension between 'zero is a logical necessity for base-n systems' (would support mountain classification) and 'zero adoption was driven by merchant/administrative pressure' (would support snare or tangled_rope). The resolution: zero IS logically necessary for sufficiently complex positional systems, but the practical pressure to develop positional systems came from merchant and administrative counting needs, not from abstract mathematical reasoning. The constraint is neither pure natural law nor pure extraction, but tangled coordination (practical systems do need zero) with asymmetric extraction (philosophical coherence must be sacrificed). The false summit lies in claiming mathematical inevitability when the driving force was economic pressure. The mandatrophy is resolved by decomposing: (1) the logical necessity of zero GIVEN a positional system (approaches mountain), (2) the practical pressure that forced adoption of positional systems (approaches snare/tangled_rope). These are separate claims with different ε values. This document treats them as a single constraint because they are historically inseparable—the practical pressure and the logical structure co-evolved—but omega_reading_contest_or_single_constraint flags the conceptual decomposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    philosophical_necessity_vs_pragmatic_accident,
    'Is zero''s adoption driven by philosophical inevitability (logical consequence of place-value systems) or by pragmatic pressure (merchant and administrator need) that eventually forced philosophical reconstruction?',
    'Historical reconstruction of adoption timeline: Did philosophical arguments precede or follow practical adoption? Analysis of which cultures adopted zero first and under what conditions (administrative pressure vs pure mathematical reasoning).',
    'If necessity: constrains classification toward mountain (universal logical property). If pragmatic accident: reshapes toward snare/tangled_rope (contingent extraction via economic pressure). The analytical observer''s mountain classification is a false summit if pragmatism, not logic, drove adoption.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(philosophical_necessity_vs_pragmatic_accident, empirical, 'Whether zero adoption was philosophically inevitable or pragmatically contingent').

omega_variable(
    extraction_from_classical_authority,
    'Does zero''s adoption represent genuine epistemological progress that required abandoning false classical constraints, or extraction wherein merchant and administrative classes impose calculation tools that damage philosophical coherence?',
    'Comparative analysis: Do geometrically-grounded number systems (classical) maintain coherence that zero-based systems lose? Can both frameworks coexist or does one necessarily invalidate the other? What was actually lost vs gained in the transition?',
    'If extraction: classical philosophy is the victim; zero adoption is snare (forced loss). If progress: philosophy was false constraint; zero adoption is liberation (rope or scaffold). The identity of beneficiary/victim rotates depending on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_from_classical_authority, conceptual, 'Whether zero adoption represents progress or extraction from classical authority').

omega_variable(
    reading_contest_or_single_constraint,
    'Is this a single constraint (epistemological resistance to zero) observed from different power positions, or does it represent multiple readings of a contested kernel about what mathematical objects fundamentally are?',
    'Structure analysis: Do all perspectives describe the same structural phenomenon with different experiences of it? Or do some perspectives instantiate genuinely different commitments about the nature of number (different readings)? If readings diverge, identify the kernel (definition of number/magnitude) and map reading_relations.',
    'If single constraint: use indexical classification (this document structure). If contested kernel: decompose into separate stories per reading, each with its own ε and classified type. Different readings would have different beneficiaries and different theories of what zero is.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_or_single_constraint, conceptual, 'Whether this is one constraint or multiple readings of a number-definition kernel').

omega_variable(
    islamic_transmission_versus_reinvention,
    'Did European adoption of zero represent genuine transmission of Indian mathematical innovation through Islamic intermediaries, or independent European reinvention driven by computational pressure?',
    'Historical documentation: Trace Fibonacci''s sources, compare timing of European adoption with Islamic circulation, analyze whether European resistance arguments preceded or followed Islamic models. Did Europeans consciously adopt Islamic mathematics or gradually rediscover zero from first principles?',
    'If transmission: constraint operates across a single knowledge community (Indian → Islamic → European lineage); extraction flows from economic pressure on adopters. If reinvention: multiple independent pressures produced the same solution; constraint is more universal (approaches mountain). Affects network decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(islamic_transmission_versus_reinvention, empirical, 'Whether zero adoption was transmitted from Islamic sources or independently reinvented').

omega_variable(
    university_performance_and_theater_ambiguity,
    'Does the medieval university''s dual-system teaching (Euclidean geometry + practical numerals) represent genuine theater masking functional degradation, or legitimate multi-context reasoning?',
    'Curriculum analysis: Were dual systems taught because both were considered valid, or because universities were defending false authority while pragmatically adopting tools they denied philosophically? Did curriculum evolution show increasing theater (growing gap between official doctrine and actual use)?',
    'If theater: university is piton. If legitimate multi-context: university is rope (coordinating different problem domains). Affects whether theater_ratio of 0.58 is accurate or understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(university_performance_and_theater_ambiguity, empirical, 'Whether university dual-system teaching represents theater or legitimate multi-context reasoning').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(practical_calculation_pressure, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pcp_tr_t0, practical_calculation_pressure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pcp_tr_t300, practical_calculation_pressure, theater_ratio, 300, 0.48).
narrative_ontology:measurement(pcp_tr_t600, practical_calculation_pressure, theater_ratio, 600, 0.58).

% Extraction over time
narrative_ontology:measurement(pcp_be_t0, practical_calculation_pressure, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(pcp_be_t300, practical_calculation_pressure, base_extractiveness, 300, 0.31).
narrative_ontology:measurement(pcp_be_t600, practical_calculation_pressure, base_extractiveness, 600, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(practical_calculation_pressure, information_standard).
narrative_ontology:affects_constraint(practical_calculation_pressure, algebraic_abstraction_acceptance).
narrative_ontology:affects_constraint(practical_calculation_pressure, geometric_authority_degradation).

% DUAL FORMULATION NOTE:
% The practical calculation pressure constraint is upstream of broader mathematical authority shifts. Zero adoption enables algebraic manipulation (downstream constraint: algebraic_abstraction_acceptance) and simultaneously degrades geometric authority (downstream constraint: geometric_authority_degradation). These are separate stories with their own ε values reflecting the specific empirical status of algebraic methods and geometric prestige in different periods.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(practical_calculation_pressure, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
