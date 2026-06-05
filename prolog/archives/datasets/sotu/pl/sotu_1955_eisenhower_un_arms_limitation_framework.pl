% ============================================================================
% CONSTRAINT STORY: sotu_1955_eisenhower_un_arms_limitation_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1955_eisenhower_un_arms_limitation_framework, []).

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
 *   constraint_id: sotu_1955_eisenhower_un_arms_limitation_framework
 *   human_readable: UN Framework for Armament Limitation and Peaceful Atomic Energy Use (1955 Eisenhower Doctrine)
 *   domain: foreign_policy/arms_control
 *
 * SUMMARY:
 *   Eisenhower's 1955 articulation of UN-based armament limitation and
 *   peaceful atomic energy frameworks represents an attempt to
 *   institutionalize nuclear security through multilateral verification and
 *   transparency mechanisms. The constraint embeds strategic competition
 *   within a formally cooperative institutional structure, creating a hybrid
 *   mechanism that simultaneously coordinates mutual security (preventing
 *   accidental war, managing proliferation) and extracts compliance costs
 *   that distribute unequally across nations. The framework exhibits
 *   contradictory structural properties from different perspectives: genuine
 *   coordination mechanism (rope) for US-led powers with verification
 *   advantages; mixed coordination and extraction (tangled rope) for Soviet
 *   Union facing verification asymmetry; pure extraction (snare) for
 *   non-aligned nations bearing nuclear vulnerability without negotiation
 *   power; degraded theatrical institution (piton) where substantive
 *   disarmament stalls behind protocol and negotiation ritual. The theater
 *   ratio (0.58) reflects that UN negotiations produce visible arms control
 *   language and protocol frameworks while actual weapons development
 *   continues largely unmonitored outside UN purview. The constraint's
 *   extractiveness (0.52) marks it as a genuine hybrid: real mutual security
 *   benefits from reduced catastrophic risk and atomic energy coordination
 *   exist alongside real asymmetric burdens on nations lacking verification
 *   leverage or atomic technology access.
 *
 * KEY AGENTS:
 *   - US-Led Nuclear Alliance: Institutional beneficiary (institutional/arbitrage) — controls verification mechanisms, retains unilateral weapons development capacity, benefits from both security coordination and technological monopoly on atomic energy
 *   - Soviet Union: Organized participant (organized/constrained) — genuine security coordination benefit alongside verification burden and technological disadvantage; constrained by isolation cost if refusing framework
 *   - Non-Aligned Nations: Powerless within framework (powerless/trapped) — gain nominal security through UN umbrella while bearing constraints on nuclear autonomy and access to atomic energy technology; no exit option
 *   - Allied Nations with Regional Autonomy: Moderate institutional actors (moderate/constrained) — benefit from US security umbrella while constrained by restrictions on independent weapons development; can exit at diplomatic cost
 *   - UN Institutional Body: Ceremonial coordinator (institutional/arbitrage) — maintains framework through diplomatic inertia despite functional degradation; benefits from perceived authority role
 *   - Global Security Commons: Abstract victim (powerless/trapped) — bears cost of unresolved verification problems and proliferation asymmetry; no mechanism for independent voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1955_eisenhower_un_arms_limitation_framework, 0.52).
domain_priors:suppression_score(sotu_1955_eisenhower_un_arms_limitation_framework, 0.65).
domain_priors:theater_ratio(sotu_1955_eisenhower_un_arms_limitation_framework, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1955_eisenhower_un_arms_limitation_framework, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1955_eisenhower_un_arms_limitation_framework, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1955_eisenhower_un_arms_limitation_framework, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1955_eisenhower_un_arms_limitation_framework, tangled_rope).
narrative_ontology:human_readable(sotu_1955_eisenhower_un_arms_limitation_framework, "UN Framework for Armament Limitation and Peaceful Atomic Energy Use (1955 Eisenhower Doctrine)").
narrative_ontology:topic_domain(sotu_1955_eisenhower_un_arms_limitation_framework, "foreign_policy/arms_control").

domain_priors:requires_active_enforcement(sotu_1955_eisenhower_un_arms_limitation_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1955_eisenhower_un_arms_limitation_framework, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(sotu_1955_eisenhower_un_arms_limitation_framework, global_security_commons).
narrative_ontology:constraint_victim(sotu_1955_eisenhower_un_arms_limitation_framework, non_aligned_nations).
narrative_ontology:constraint_victim(sotu_1955_eisenhower_un_arms_limitation_framework, verification_transparency_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ALIGNED NATIONS (SNARE) — Small and medium powers lack veto or meaningful exit. Trapped within a framework designed by superpowers without their consultation. Accept nuclear vulnerability and negotiation theater as the price of nominal security via UN protection. No alternative institutional pathway available; security depends on superpowers' goodwill toward treaty compliance.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_un_arms_limitation_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED NATIONS WITH AUTONOMY CLAIMS (TANGLED ROPE) — Gain security benefits (reduced Soviet first-strike risk, atomic energy tech transfer) while bearing constraint on independent nuclear weapons development and regional military autonomy. Constrained rather than trapped — can pursue bilateral security pacts or withdraw from framework at diplomatic cost. Experience mixed coordination and extraction: genuine mutual security incentive alongside subordination to superpower-led verification regime.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_un_arms_limitation_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US-LED NUCLEAR ALLIANCE (ROPE) — Primary beneficiaries. Control verification mechanisms, set agenda through institutional design, retain unilateral nuclear capability while appearing cooperative. Arbitrage option: can exit framework while maintaining unilateral weapons development (US historical behavior through weapons miniaturization, yield enhancement). Framework solves genuine coordination problem (preventing accidental nuclear war, managing atomic energy proliferation) while preserving hegemonic advantage. Net benefit — extraction runs toward this bloc.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_un_arms_limitation_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOVIET UNION / WARSAW PACT (TANGLED ROPE) — Faces credible dilemma: accept verification regime (exposes military secrets, creates transparency risk) or reject framework and accept international isolation plus existential first-strike vulnerability. Genuine coordination benefit from reduced mutual catastrophe risk. Constrained by technological asymmetry (US advantages in detection, intelligence) and diplomatic isolation if refusing UN framework. Active enforcement of non-compliance has high cost — triggers alliance cohesion problems and US acceleration of own programs.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_un_arms_limitation_framework, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UN INSTITUTIONAL BODY (PITON) — Framework channels security concerns through UN mechanisms, creating performative verification theater. Security Council structure reflects WWII power distribution; Soviet veto prevents binding enforcement. UN's stated role in arms control proves largely ceremonial — actual verification happens through national intelligence (CIA, KGB). Institution persists through diplomatic inertia despite functional degradation. Theater ratio high: extensive negotiation rituals, inspection protocols, treaty language produce appearance of progress while substantive disarmament stalls.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_un_arms_limitation_framework, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRATEGIC STABILITY VIEW (MOUNTAIN) — From civilizational scale, nuclear weapons create an irreducible verification problem: any nation capable of producing fissile material can secretly build weapons. No inspection protocol can guarantee detection. This fundamental asymmetry (cannot prove negative — absence of weapons) makes any disarmament framework geometrically constrained. However, this perspective risks naturalizing what is partly institutional: the verification problem is real, but the framework's theatrical response substitutes for genuine transparency mechanisms. Engine flags as false summit.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_un_arms_limitation_framework, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1955_eisenhower_un_arms_limitation_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1955_eisenhower_un_arms_limitation_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1955_eisenhower_un_arms_limitation_framework, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1955_eisenhower_un_arms_limitation_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1955_eisenhower_un_arms_limitation_framework, TR),
    TR >= 0.70.

:- end_tests(sotu_1955_eisenhower_un_arms_limitation_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The framework creates genuine mutual security benefits (reduced mutual catastrophe risk, atomic energy coordination) while imposing asymmetric compliance costs. Non-nuclear nations bear proportionally higher constraints relative to benefit received. The US retains unilateral development capacity through weapons miniaturization and yield enhancement while appearing cooperative. Soviet Union experiences real verification disadvantage without corresponding reduction in constraints. Suppression (0.65): High. Nations cannot easily exit the framework — doing so signals untrustworthiness and isolation. Verification protocols constrain military program transparency. Access to atomic energy depends on compliance. Non-aligned nations have no institutional alternative for security coordination. Suppression is structural (no exit options exist at acceptable cost) rather than performative. Theater ratio (0.58): Moderate-high. UN negotiations produce extensive treaty language, inspection protocols, and diplomatic sessions. Actual disarmament verification happens through national intelligence (CIA, KGB) rather than UN mechanisms. Security Council structure provides Soviet veto over binding enforcement. The gap between negotiation intensity and substantive weapons reduction is significant. Theater has increased as negotiation has replaced actual verification in early measurement points.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how institutional design creates perspectival divergence on the same structural phenomenon. US-led alliance sees rope (genuine coordination solving mutual catastrophe problem while preserving hegemonic advantage). Soviet Union sees tangled rope (real security benefit constrained by verification disadvantage and isolation cost of exit). Non-aligned nations see snare (trapped within framework designed without their participation, bearing nuclear vulnerability and technology access constraints). UN institutional body sees piton (performative negotiation theater substituting for functional verification). Analytical observer risks seeing mountain (fundamental physics of verification asymmetry making all frameworks equivalent). The perspectival gap is not mere disagreement but reflects real structural differences: agents occupy different positions in the verification asymmetry. The framework's extraction flows from nuclear powers to non-nuclear powers, making directionality a critical differentiator.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply by agent structural position within the framework. US-led powers derive d ≈ 0.15 (beneficiary + arbitrage exit) — they control verification agenda, retain weapons capacity, benefit from atomic energy monopoly. Soviet Union derives d ≈ 0.58 (victim of verification asymmetry + constrained exit) — bears transparency burden without equivalent capability to verify US compliance; exit triggers isolation. Non-aligned nations derive d ≈ 0.92 (full victims + trapped) — no verification capacity, no weapons development autonomy, constrained atomic energy access, no exit. The framework's effective extractiveness (χ) is heavily scaled by agent power and exit options: beneficiaries with arbitrage options experience low χ; trapped agents experience high χ. Beneficiary declaration is essential to directionality — the framework's coordination benefits are real and flow primarily toward nuclear powers, making this a genuine tangled rope rather than snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework resolves the mandatrophy by showing that genuine coordination (mutual catastrophe reduction) coexists with genuine extraction (asymmetric compliance burden and tech monopoly). This is not a false dichotomy but a structural hybrid. The framework genuinely coordinates: nuclear powers benefit from reduced mutual first-strike risk and transparent arsenal communication prevents accidental escalation. The framework genuinely extracts: US retains unilateral weapons development through technological innovation; Soviet Union bears verification transparency burden; non-aligned nations bear constraints without negotiation power. The mandatrophy is resolved not by choosing rope or snare but by recognizing that the same structural mechanism serves both functions simultaneously. From US perspective: rope dominates. From Soviet perspective: tangled rope dominates. From non-aligned perspective: snare dominates. The framework's tangled rope classification reflects that coordination and extraction are inseparable in its institutional design — you cannot extract the coordination benefit without accepting the extraction cost, and you cannot refuse the extraction without losing the coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_asymmetry_irreducible,
    'Is the verification problem fundamental to nuclear weapons physics or contingent on 1955-era surveillance technology and institutional design?',
    'Comparison of verification effectiveness: intrusive on-site inspection vs satellite intelligence vs intelligence-sharing protocols. Historical analysis of detection rates for clandestine weapons programs (USSR, Israel, Pakistan, Iraq, North Korea).',
    'If fundamental: mountain classification holds; framework is coordination within irreducible constraint. If contingent: framework substitutes theater for solvable technical problem; classification shifts toward snare or piton depending on agent perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_asymmetry_irreducible, empirical, 'Whether verification asymmetry is physical law or institutional artifact').

omega_variable(
    soviet_genuine_compliance_intent,
    'Did Soviet participation in UN arms limitation framework represent genuine intent to reduce nuclear arsenal, or purely defensive posture against US encirclement?',
    'Historical analysis of Soviet weapons production rates vs treaty compliance declarations. Examination of declassified intelligence on Soviet weapons programs during framework period. Analysis of Soviet diplomatic messaging to domestic vs Western audiences.',
    'If genuine: tangled rope perspective confirmed — Soviet Union experienced real mixed coordination/extraction. If defensive only: Soviet perspective shifts toward snare (trapped by verification burden without actual reduction commitment) or piton (performative participation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(soviet_genuine_compliance_intent, empirical, 'Soviet genuine compliance intent vs defensive posturing').

omega_variable(
    atomic_energy_tech_transfer_extraction,
    'Does the ''Atoms for Peace'' component genuinely distribute peaceful nuclear technology to non-aligned nations, or does it primarily serve US commercial and strategic interests through monopolization of fuel supply?',
    'Analysis of fuel supply contracts, enrichment technology access, control of reactor designs. Comparison of technology transfer patterns: aligned nations vs non-aligned. Long-term fuel price and supply dependency.',
    'If genuine distribution: framework shows real coordination benefit for non-aligned nations. If monopolization: ''peaceful atomic energy'' becomes cover story for resource extraction and geopolitical leverage — tangled rope becomes snare for technology-dependent nations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atomic_energy_tech_transfer_extraction, empirical, 'Whether atomic energy tech transfer is genuine or strategic monopolization').

omega_variable(
    bilateral_superpower_agreements_bypass,
    'To what degree did US and USSR conduct actual arms control through bilateral back-channels (hot line, SALT talks, direct diplomacy) rather than UN framework?',
    'Declassified diplomatic records, presidential archives, comparison of framework treaty provisions vs actual bilateral agreements. Timeline analysis: when did substantive negotiation move from UN to bilateral channels?',
    'If substantial bypass: UN framework is theater masking bilateral negotiation; piton classification dominates. If framework remains primary: rope classification for superpowers has higher confidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bilateral_superpower_agreements_bypass, empirical, 'Degree of bilateral superpower bypass of UN framework').

omega_variable(
    extraction_distribution_inequality,
    'Does the framework impose symmetric constraints on nuclear powers (verification burden distributed equally) or asymmetric burden on developing nations (inspections, restrictions on atomic energy access)?',
    'Quantitative analysis of inspection frequencies by nation type. Cost analysis of verification compliance: nuclear powers'' cost vs non-nuclear nations'' opportunity cost from energy access restrictions. Comparison of tech transfer rates to aligned vs non-aligned nations.',
    'If symmetric: framework is genuine tangled rope for most agents. If asymmetric: framework is snare for non-aligned nations and piton for developed nations claiming equal burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_distribution_inequality, empirical, 'Whether constraints are symmetric or asymmetrically burden developing nations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1955_eisenhower_un_arms_limitation_framework, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(un_arms_tr_t0, sotu_1955_eisenhower_un_arms_limitation_framework, theater_ratio, 0, 0.42).
narrative_ontology:measurement(un_arms_tr_t5, sotu_1955_eisenhower_un_arms_limitation_framework, theater_ratio, 5, 0.51).
narrative_ontology:measurement(un_arms_tr_t10, sotu_1955_eisenhower_un_arms_limitation_framework, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(un_arms_be_t0, sotu_1955_eisenhower_un_arms_limitation_framework, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(un_arms_be_t5, sotu_1955_eisenhower_un_arms_limitation_framework, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(un_arms_be_t10, sotu_1955_eisenhower_un_arms_limitation_framework, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1955_eisenhower_un_arms_limitation_framework, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sotu_1955_eisenhower_un_arms_limitation_framework, 0.12).
narrative_ontology:affects_constraint(sotu_1955_eisenhower_un_arms_limitation_framework, soviet_verification_resistance).
narrative_ontology:affects_constraint(sotu_1955_eisenhower_un_arms_limitation_framework, nuclear_proliferation_asymmetry).
narrative_ontology:affects_constraint(sotu_1955_eisenhower_un_arms_limitation_framework, atoms_for_peace_fuel_monopoly).

% DUAL FORMULATION NOTE:
% This constraint is upstream of specific bilateral arms control agreements (SALT, INF treaties) but represents the institutional framework constraining how those agreements are negotiated and verified. The UN framework's theater ratio and extractiveness have specific consequences for downstream bilateral constraints — higher UN framework theater enables more bilateral back-channel negotiation with lower transparency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1955_eisenhower_un_arms_limitation_framework, institutional, 0.18).
constraint_indexing:directionality_override(sotu_1955_eisenhower_un_arms_limitation_framework, organized, 0.58).
constraint_indexing:directionality_override(sotu_1955_eisenhower_un_arms_limitation_framework, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
