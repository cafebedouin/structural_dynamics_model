% ============================================================================
% CONSTRAINT STORY: nero_imperial_expropriation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nero_imperial_expropriation, []).

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
 *   constraint_id: nero_imperial_expropriation
 *   human_readable: Nero's Imperial Legitimacy via Expropriation and Spectacle
 *   domain: political/economic/ancient_rome
 *
 * SUMMARY:
 *   Nero's imperial expropriation system (c. 54-68 CE) represents a canonical
 *   snare constraint: the coupling of arbitrary wealth seizure with
 *   spectacle-driven legitimacy. The system operated through trumped-up
 *   treason accusations (formal pretext), mass confiscations (mechanism), and
 *   resource redirection to games, architecture, and performances
 *   (legitimation theater). The constraint exhibits high extractiveness
 *   (ε=0.68: ~30-40% of senatorial wealth seized over the reign), high
 *   suppression (σ=0.72: victims have no legal recourse, no organized
 *   resistance capacity, no exit option), and extremely high theater (τ=0.81:
 *   the performative apparatus — public trials, constitutional fictions,
 *   spectacle — exceeds the functional constraint on actual power). Base
 *   extraction rose from ≈0.32 in the early reign (moderate 'normal' imperial
 *   taxation) to 0.68 by the final years (systematic predation), while
 *   theater rose correspondingly from 0.55 to 0.81, indicating that
 *   legitimacy maintenance required increasingly performative effort as the
 *   extraction mechanism became obvious. This is the inverse of a degrading
 *   piton: a snare that becomes MORE theatrical as its predatory nature
 *   becomes undeniable.
 *
 * KEY AGENTS:
 *   - Nero Imperial Household: Primary beneficiary (institutional/arbitrage) — captures expropriated wealth and directs resource allocation; experiences system as coordination of imperial stability
 *   - Roman Senatorial Aristocracy: Primary victim (powerless/trapped) — subject to arbitrary confiscation under legal pretext; no appeal mechanism or safe exit
 *   - Provincial Merchants/Wealthy Non-Senators: Secondary victim (moderate/trapped) — vulnerable to special taxation and seizure; less protected than senators by custom, but economically important
 *   - Plebeian Urban Populations: Tertiary victim (powerless/trapped) — compressed by grain price inflation and subsistence erosion as resources diverted to spectacle; cannot exit; cannot organize
 *   - Praetorian Guard: Organized co-beneficiary/enforcer (organized/constrained) — benefits from increased pay and preferential allocation; constrained into enforcement role
 *   - Republican Constitutional System: Institutional fiction (institutional/arbitrage) — formal law persists while expropriation operates outside legal bounds; theater ratio indicates law's performative role exceeds functional constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nero_imperial_expropriation, 0.68).
domain_priors:suppression_score(nero_imperial_expropriation, 0.72).
domain_priors:theater_ratio(nero_imperial_expropriation, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nero_imperial_expropriation, extractiveness, 0.68).
narrative_ontology:constraint_metric(nero_imperial_expropriation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nero_imperial_expropriation, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nero_imperial_expropriation, snare).
narrative_ontology:human_readable(nero_imperial_expropriation, "Nero's Imperial Legitimacy via Expropriation and Spectacle").
narrative_ontology:topic_domain(nero_imperial_expropriation, "political/economic/ancient_rome").

domain_priors:requires_active_enforcement(nero_imperial_expropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nero_imperial_expropriation, nero_imperial_household).
narrative_ontology:constraint_victim(nero_imperial_expropriation, roman_aristocracy).
narrative_ontology:constraint_victim(nero_imperial_expropriation, wealthy_provincial_merchants).
narrative_ontology:constraint_victim(nero_imperial_expropriation, plebeian_subsistence_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROPERTIED ARISTOCRAT (SNARE) — Faces arbitrary expropriation under legal pretext (treason charges, estate confiscation). Exit requires fleeing the empire or death. d≈0.92, f(d)≈1.39, σ=1.1 → χ≈0.69. High extraction with no legitimate appeal.
constraint_indexing:constraint_classification(nero_imperial_expropriation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: PROVINCIAL MERCHANT (SNARE) — Vulnerable to special taxation, forced loans, and seizure under imperial diktat. Flight risks loss of all property; compliance yields extraction through legal confiscation. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.62. Extraction without protection.
constraint_indexing:constraint_classification(nero_imperial_expropriation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: URBAN PLEBEIAN POPULATION (SNARE) — Compressed by grain price inflation (resources redirected to spectacle/military). No exit: cannot leave Rome; cannot organize collective resistance. Subsistence erosion is slow extraction. d≈0.94, f(d)≈1.41, σ=0.8 → χ≈0.60. Maximum powerlessness.
constraint_indexing:constraint_classification(nero_imperial_expropriation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: PRAETORIAN GUARD (TANGLED ROPE) — Benefits from increased pay, preferential grain allocation, and imperial spectacle (gladiator shows as morale tool). Also coerced into enforcement role; loyalty is uncertain and extracted through privilege management. d≈0.45, f(d)≈0.45, σ=1.1 → χ≈0.29. Mixed: coordination (shared interest in imperial stability) + extraction (coerced enforcement).
constraint_indexing:constraint_classification(nero_imperial_expropriation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: IMPERIAL HOUSEHOLD (ROPE) — Coordinates consent through spectacle (games, building projects). Experiences constraint as a coordination mechanism: expropriation + spectacle = legitimacy. d≈0.08, f(d)≈-0.08, σ=1.1 → χ≈-0.06. Net beneficiary; sees system as mutually reinforcing coordination.
constraint_indexing:constraint_classification(nero_imperial_expropriation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: REPUBLICAN CONSTITUTIONAL FICTION (PITON) — The formal Roman Republic and its property law persist as fiction while expropriation operates outside legal bounds. Theater_ratio=0.81 reflects that the law's performative role (legitimating seizure via treason trial) exceeds its functional constraint on imperial power. d≈0.05, f(d)≈-0.10, σ=1.1 → χ≈-0.09. The constitutional fiction enables extraction while pretending to constrain it.
constraint_indexing:constraint_classification(nero_imperial_expropriation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, imperial expropriation might appear as an immutable feature of monarchy: unchecked executive power always extracts. However, base data (ε=0.68, suppression=0.72, theater=0.81) contradicts mountain classification. This is not a natural law but a contingent institutional pathology: other emperors (Augustus, Titus) achieved legitimacy without systematic expropriation. The 'inherent in monarchy' framing naturalizes what is Nero-specific extraction governance.
constraint_indexing:constraint_classification(nero_imperial_expropriation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nero_imperial_expropriation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nero_imperial_expropriation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nero_imperial_expropriation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nero_imperial_expropriation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nero_imperial_expropriation, TR),
    TR >= 0.70.

:- end_tests(nero_imperial_expropriation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not absolute. Nero seized perhaps 30-40% of senatorial wealth over his reign through confiscation, but not 100% of all property — some victims were merely impoverished rather than eliminated. Extraction was systematic and severe but left sufficient wealth in-place to avoid economic collapse. This is not universal expropriation but targeted predation. Suppression (0.72): Very high. Victims had minimal legitimate appeal; treason trials were show trials; attempted flight meant forfeiting property; military force backed seizures. However, suppression was not absolute — some victims escaped, some bribed their way to survival — hence 0.72 rather than 0.95. Theater ratio (0.81): Extremely high and increasing. Nero's expropriation machinery was wrapped in legal formalism (treason trials with witnesses), constitutional pretense (senate votes on confiscations), and spectacle (games funded by seized wealth, elaborate architecture projects). The theatrical apparatus — public trials, proclamations, games — served no functional constraint on power but rather legitimated it. As extraction became more obvious and aristocratic resentment grew, theater had to intensify to maintain the fiction of legal process.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a clean separability between beneficiary and victim perspectives with zero overlap. The imperial household sees coordination and mutual benefit (spectacle maintains plebeian loyalty; expropriation finances spectacle; legitimacy props up imperial rule). Senatorial victims see pure predation (arbitrary seizure, show trials, no recourse). Plebeian populations see indirect extraction (grain becomes scarce/expensive as resources redirect to games). The praetorian guard occupies a liminal position: beneficiary of increased pay and spectacle (coordination), but also enforcer of expropriation (coercion). The republican constitutional fiction operates as theater masking raw power — it is not a mechanism of coordination or justice, but a legitimation apparatus. This is the opposite of a scaffold (temporary coordinating measure) — it is a piton that thinks it is rope: degraded law persisting through inertia and theatrical maintenance. The analytical observer risks naturalizing Nero-specific pathology as inherent to monarchy, but other emperors (Augustus, Titus, Marcus Aurelius) achieved legitimacy without systematic expropriation, proving the constraint is contingent, not immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   Nero Imperial Household: Beneficiary + arbitrage exit → d≈0.08, f(d)≈-0.08. Net beneficiary (negative effective extraction). Senatorial Aristocracy: Victim + trapped exit → d≈0.92, f(d)≈1.39. Maximum extraction directionality. Provincial Merchants: Victim + trapped exit → d≈0.88, f(d)≈1.32. High extraction but slightly less than senatorial class (less protected by custom, but also less systematically targeted). Plebeian Populations: Victim + trapped exit → d≈0.94, f(d)≈1.41. Maximum extraction but mediated through indirect mechanisms (inflation, subsistence compression). Praetorian Guard: Mixed beneficiary/enforcer + constrained exit → d≈0.45, f(d)≈0.45. Moderate directionality reflecting both benefits (pay, spectacle) and coercion (enforcement, loyalty extraction). Republican Constitutional System: Beneficiary of legitimacy theater + arbitrage exit (for imperial power) → d≈0.05, f(d)≈-0.10. Negative effective extraction; the fiction enables extraction while appearing to constrain it.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE RESOLUTION: This constraint is classified as pure Snare across nearly all victim perspectives, with only the Praetorian Guard perspective showing Tangled Rope (mixed coordination + extraction). The mandatrophy is resolved by recognizing that the constraint serves NO legitimate coordination function from the victims' perspective — there is no collective action problem being solved by expropriation, no beneficial commons being maintained, no asymmetric but mutually beneficial trade. Expropriation appears as Rope (from the imperial beneficiary view) only because the beneficiary experiences it as coordinating imperial legitimacy with spectacle. But this coordination is entirely internal to the imperial household; it provides no coordination benefit to senatorial class, provincial merchants, or plebeians. For them, it is pure extraction. The high theater ratio (0.81) reflects that legitimacy maintenance required increasingly elaborate performative apparatus — treason trials, constitutional votes, spectacular games — not because these mechanisms were functional but because they were necessary to mask the predatory nature of the system as it became more obvious. The mandatrophy is NOT resolved by claiming 'maybe the victims benefit in ways we don't see' — the empirical record shows declining senatorial participation in governance, declining trust, increasing assassinations of Nero's inner circle, and ultimate regime collapse. No legitimate coordination function is evidenced. The constraint is a Snare, mandatrophy resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    senatorial_conspiracy_authenticity,
    'How many treason accusations against Nero''s senatorial victims were genuine conspiracies versus fabricated pretexts for expropriation?',
    'Cross-source historical analysis (Tacitus, Suetonius, Cassius Dio) of alleged conspiracies; correlation between treason charges and property seizures; investigation of whether specific accusers benefited financially',
    'If conspiracies were real: constraint shifts toward legitimate enforcement of law (Tangled Rope). If mostly fabricated: pure expropriation mechanism confirmed (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(senatorial_conspiracy_authenticity, empirical, 'Whether senatorial treason accusations were genuine or pretexts').

omega_variable(
    spectacle_legitimacy_efficacy,
    'Did imperial spectacle (games, architecture, performances) actually generate genuine political support among the plebeian population, or was it purely coercive theater masking resource extraction?',
    'Analysis of plebeian riot patterns relative to spectacle timing; investigation of whether plebeian grain subsidy reductions triggered unrest; comparison with control cases (provincial populations receiving less spectacle)',
    'If spectacle generated real support: constraint includes coordination function (Tangled Rope/Rope from plebeian perspective). If purely theater: pure extraction (Snare) confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spectacle_legitimacy_efficacy, empirical, 'Whether spectacle generated genuine political legitimacy or was coercive theater').

omega_variable(
    exit_velocity_alternative_paths,
    'For wealthy victims with advance warning, what proportion successfully escaped the empire (to Parthia, Egypt, or provinces beyond imperial reach) versus were trapped?',
    'Historical documentation of escapes and flight cases; investigation of Parthian and Egyptian refuge records for Roman exiles; analysis of assets seized post-escape',
    'If escape rate > 30%: exit options may be ''constrained'' rather than ''trapped'' for organized victims. If escape rate < 5%: truly trapped confirmation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_velocity_alternative_paths, empirical, 'Proportion of wealthy victims able to escape imperial reach').

omega_variable(
    agricultural_productivity_consequence,
    'Did resource extraction for spectacle directly reduce agricultural productivity and supply chains, or did the economy compensate through other mechanisms (slave labor reallocation, increased provincial output)?',
    'Analysis of grain price trends, provincial tax records, and agricultural productivity measurements across Nero''s reign; comparison with pre-Nero baseline and post-Nero recovery',
    'If productivity dropped sharply: extraction mechanisms confirmed as economically destructive (true Snare). If economy adapted: extraction was redistributive rather than destructive (Tangled Rope from economist perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agricultural_productivity_consequence, empirical, 'Whether resource extraction degraded agricultural productivity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nero_imperial_expropriation, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nero_tr_t0, nero_imperial_expropriation, theater_ratio, 0, 0.55).
narrative_ontology:measurement(nero_tr_t3, nero_imperial_expropriation, theater_ratio, 3, 0.7).
narrative_ontology:measurement(nero_tr_t7, nero_imperial_expropriation, theater_ratio, 7, 0.81).

% Extraction over time
narrative_ontology:measurement(nero_be_t0, nero_imperial_expropriation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(nero_be_t3, nero_imperial_expropriation, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(nero_be_t7, nero_imperial_expropriation, base_extractiveness, 7, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nero_imperial_expropriation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nero_imperial_expropriation, 0.55).
narrative_ontology:affects_constraint(nero_imperial_expropriation, roman_grain_supply_crisis).
narrative_ontology:affects_constraint(nero_imperial_expropriation, senatorial_power_decline).
narrative_ontology:affects_constraint(nero_imperial_expropriation, praetorian_guard_mutation).
narrative_ontology:affects_constraint(nero_imperial_expropriation, imperial_legitimacy_crisis).

% DUAL FORMULATION NOTE:
% This constraint is downstream of structural imperial politics but represents a distinct economic-coercive mechanism. Related constraints include grain supply disruption (upstream: resource allocation consequence), senatorial power degradation (downstream: political consequence), praetorian professionalization (collateral: enforcer evolution), and broader imperial legitimacy crisis (overarching: regime failure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nero_imperial_expropriation, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
