% ============================================================================
% CONSTRAINT STORY: nuclear_weapons_proliferation_prevention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_weapons_proliferation_prevention, []).

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
 *   constraint_id: nuclear_weapons_proliferation_prevention
 *   human_readable: Nuclear Weapons Proliferation Prevention Regime
 *   domain: international_security/political_economy
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty (NPT) and its associated
 *   verification regime constitute a global constraint on weapons development
 *   that exhibits simultaneous coordination and extraction functions. The
 *   regime coordinates a collective action problem (preventing nuclear
 *   weapons spread) while simultaneously perpetuating nuclear inequality and
 *   blocking disarmament pathways. The constraint manifests differently
 *   across institutional positions: as security coordination for nuclear
 *   weapons states, as vulnerability trap for non-nuclear states, as
 *   temporary scaffolding for disarmament advocates, and as performative
 *   ritual for verification institutions. The key structural ambiguity is
 *   whether the regime's asymmetry is a necessary security cost or an
 *   extractive perpetuation of nuclear advantage. Over the 50-year interval,
 *   the constraint has drifted from genuine coordination (1970s: NPT as
 *   foundation for disarmament pathway) toward higher extraction and theater
 *   (2020s: disarmament stalled, verification theater routinized, technology
 *   restrictions entrenched).
 *
 * KEY AGENTS:
 *   - Nuclear Weapons States (USA, Russia, China, France, UK): Primary beneficiaries (institutional/arbitrage) — preserve exclusive deterrent capability while blocking others' development
 *   - Non-Nuclear Weapons States: Primary victims (powerless/trapped) — accept weapons prohibition with asymmetric vulnerability to nuclear-armed actors
 *   - Rising Regional Powers (India, Pakistan, Israel, Iran, North Korea): Secondary actors (powerful/mobile) — experience mixed coordination benefits and extraction constraints
 *   - International Atomic Energy Agency: Institutional enforcement actor (institutional/arbitrage) — maintains verification theater despite known detection gaps
 *   - Nuclear Disarmament Movement: Organized advocates (organized/constrained) — see regime as temporary scaffold toward comprehensive disarmament
 *   - Analytical Observer: Sees the regime's hybrid nature at civilizational scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_weapons_proliferation_prevention, 0.58).
domain_priors:suppression_score(nuclear_weapons_proliferation_prevention, 0.72).
domain_priors:theater_ratio(nuclear_weapons_proliferation_prevention, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_weapons_proliferation_prevention, extractiveness, 0.58).
narrative_ontology:constraint_metric(nuclear_weapons_proliferation_prevention, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nuclear_weapons_proliferation_prevention, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_weapons_proliferation_prevention, tangled_rope).
narrative_ontology:human_readable(nuclear_weapons_proliferation_prevention, "Nuclear Weapons Proliferation Prevention Regime").
narrative_ontology:topic_domain(nuclear_weapons_proliferation_prevention, "international_security/political_economy").

domain_priors:requires_active_enforcement(nuclear_weapons_proliferation_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_weapons_proliferation_prevention, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(nuclear_weapons_proliferation_prevention, security_alliance_leaders).
narrative_ontology:constraint_victim(nuclear_weapons_proliferation_prevention, non_nuclear_weapons_states).
narrative_ontology:constraint_victim(nuclear_weapons_proliferation_prevention, global_nuclear_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR WEAPONS STATE (SNARE) — Trapped by the non-proliferation regime. Cannot develop nuclear deterrent without catastrophic sanctions and isolation. Faces asymmetric vulnerability to nuclear-armed neighbors despite NPT compliance. Bears full cost of the constraint with no credible exit option.
constraint_indexing:constraint_classification(nuclear_weapons_proliferation_prevention, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NUCLEAR WEAPONS STATES (ROPE) — Institutional actors (USA, Russia, China, France, UK) experience the non-proliferation regime as coordination mechanism that preserves their exclusive deterrent capability. The regime enables collective action against proliferation while protecting their own arsenals. Net beneficiaries with high exit capacity (can unilaterally withdraw or reinterpret commitments).
constraint_indexing:constraint_classification(nuclear_weapons_proliferation_prevention, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: RISING REGIONAL POWER (TANGLED ROPE) — Powerful state (e.g., India, Pakistan, Israel) experiences genuine security coordination benefits from the regime (reduced regional proliferation pressure) while also bearing significant extraction (constrained deterrent modernization, technology access restrictions). Mobile exit options but high political cost. Mixed coordination and asymmetric constraint.
constraint_indexing:constraint_classification(nuclear_weapons_proliferation_prevention, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: NUCLEAR DISARMAMENT MOVEMENT (SCAFFOLD) — Organized civil society and advocacy networks (ICAN, UN disarmament conferences) see the NPT as temporary scaffolding toward comprehensive disarmament. The constraint provides coordination framework with built-in sunset clause (Article VI disarmament commitment). Theater ratio reflects the performative gap between disarmament pledges and arsenal expansion.
constraint_indexing:constraint_classification(nuclear_weapons_proliferation_prevention, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: IAEA VERIFICATION SYSTEM (PITON) — International Atomic Energy Agency's inspection and verification mandate is substantially performative. Cannot detect covert weapons programs with high confidence (as revealed by Iraq, Iran, North Korea cases). Maintains legitimacy through ritual inspections and reporting despite known detection gaps. High theater ratio reflects the gap between verification theater and actual security assurance.
constraint_indexing:constraint_classification(nuclear_weapons_proliferation_prevention, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The non-proliferation regime is hybrid coordination and extraction: it genuinely reduces proliferation risk through coordination of supply controls and verification, while simultaneously perpetuating nuclear inequality and blocking disarmament pathways. The regime's persistence relies on enforced asymmetry (legal weapons for some, prohibited for others) combined with collective verification theater that masks actual proliferation pathways.
constraint_indexing:constraint_classification(nuclear_weapons_proliferation_prevention, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_weapons_proliferation_prevention_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nuclear_weapons_proliferation_prevention, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nuclear_weapons_proliferation_prevention, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_weapons_proliferation_prevention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nuclear_weapons_proliferation_prevention, TR),
    TR >= 0.70.

:- end_tests(nuclear_weapons_proliferation_prevention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The regime extracts from non-nuclear states through asymmetric vulnerability, technology restrictions, and constrained deterrent development. But extraction is not total because genuine coordination benefits exist (reduced regional proliferation risk). The value reflects that the constraint operates as hybrid — partial extraction hidden within coordination function. The 50-year trajectory (0.35→0.61) reflects increasing extraction as disarmament stalled and asymmetry entrenched. Suppression (0.72): High. Non-nuclear states face severe barriers to exit: security dependence on nuclear-armed allies, economic sanctions for proliferation, international isolation, and military vulnerability. These barriers are structural and enforced collectively. Theater ratio (0.68): High. The verification regime exhibits substantial performative content: IAEA inspections maintain legitimacy despite known inability to detect covert programs (Iraq, Iran, North Korea cases). Disarmament conferences produce rhetorical commitments (Article VI) while arsenals expand. This gap between verification theater and actual security assurance has increased over time (0.42→0.68).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. Nuclear weapons states see coordination (Rope from their perspective): the regime preserves their deterrent monopoly while preventing proliferation. Non-nuclear states see extraction (Snare): the regime denies them security options available to others. The open disarmament movement sees temporary scaffolding (Scaffold) — genuine institutional commitments with sunset clauses. But the 50-year trajectory reveals the scaffold is eroding into piton: disarmament commitments (Article VI) remain rhetorical while arsenals expand. The verification system sees its own performative maintenance (Piton): inspectors maintain institutional legitimacy through ritual despite known detection gaps. The analytical observer sees the regime's true structure as tangled rope: genuine coordination hiding real extraction, with the asymmetry perpetuated through enforcement and institutional theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapons states experience low directionality (d ≈ 0.10–0.20): they are institutional beneficiaries with arbitrage options (can withdraw from NPT, modernize arsenals, reshape regime). The sigmoid function produces negative or near-zero effective extraction — they experience the constraint as beneficial coordination. Non-nuclear weapons states experience high directionality (d ≈ 0.85–0.92): they are powerless victims with trapped exit options (cannot unilaterally exit without security collapse). The sigmoid produces high f(d) → high effective extraction χ experienced by these agents. The tangled rope classification holds globally because both coordination and asymmetric extraction are structurally real: the regime genuinely reduces proliferation risk (coordination function) while perpetuating nuclear inequality (extraction function). The piton classification of the IAEA reflects theater exceeding function — verification ritual persists because institutions maintain legitimacy through performance, not because verification confidence is high.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the regime is simultaneously coordination and extraction: it genuinely solves the collective action problem of preventing proliferation (rope function) while perpetuating asymmetric vulnerability and technology inequality (snare function). The mandatrophy prevention mechanism is the explicit accounting of beneficiaries (nuclear weapons states gaining from exclusive deterrent) and victims (non-nuclear states accepting constrained options). The tangled rope classification is mandatory because removing either the coordination function or the extraction function would produce a different structural reality — a pure coordination regime would require disarmament progress (sunset into rope or higher disarmament), while a pure extraction regime would collapse because non-weapons states would defect (breaking the collective action). The regime persists as tangled rope through the delicate balance: enough coordination benefit to maintain compliance, enough extraction to preserve asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disarmament_commitment_credibility,
    'Do Article VI disarmament commitments constitute a genuine sunset clause or indefinite rhetorical cover for perpetual weapon retention?',
    'Historical analysis of disarmament progress metrics since 1970; comparison of declared arsenal reductions vs stockpile modernization spending; analysis of negotiation deadlock patterns at disarmament conferences',
    'If credible: scaffold classification holds and sunset is real. If rhetorical: scaffold reclassifies as piton (performative maintenance of failed commitment), shifting the regime from temporary to degraded-permanent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disarmament_commitment_credibility, empirical, 'Whether disarmament commitments are genuine sunset or rhetorical cover').

omega_variable(
    verification_detection_confidence,
    'What is the true detection confidence for covert weapons programs under current IAEA and safeguards protocols?',
    'Comparison of IAEA effectiveness across Iraq, Iran, North Korea, Syria cases; analysis of detection latency and confidence intervals; assessment of detection probability for hypothetical covert enrichment pathways',
    'If confidence > 80%: verification theater is acceptable coordination cost. If confidence < 50%: the regime is snare for NPT-bound states (false security theater) with high extraction cost, fundamentally changing classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_detection_confidence, empirical, 'True detection confidence for covert weapons programs').

omega_variable(
    technology_transfer_asymmetry_necessity,
    'Is the restriction on civilian nuclear technology transfer to non-weapons states a necessary security measure or an extractive protection of nuclear-armed states'' technological advantage?',
    'Comparative analysis of proliferation risk by technology access level; assessment of whether restricted technologies have dual-use weapons pathways vs legitimate civilian applications; analysis of economic opportunity cost for restricted states',
    'If necessary: extraction is coordination cost, snare classification for non-weapons states is justified by security. If extractive: the regime is snare sustained by manufactured asymmetry rather than genuine security imperative, strengthening victim classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_asymmetry_necessity, conceptual, 'Whether technology restrictions are security-necessary or extractive asymmetry').

omega_variable(
    regional_deterrent_credibility_gap,
    'Does NPT participation meaningfully increase nuclear vulnerability for non-weapons states, or is the vulnerability difference marginal?',
    'Comparative security modeling: vulnerability of NPT member vs hypothetical nuclear-armed equivalent; historical analysis of conventional military conflicts between nuclear and non-nuclear states; assessment of deterrent credibility for non-superpower actors',
    'If meaningfully increases vulnerability: snare classification for non-weapons states is confirmed (real security extraction). If marginal: the victim status is partly aspirational/political, reducing snare severity and suggesting rope reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_deterrent_credibility_gap, empirical, 'Magnitude of security vulnerability increase from NPT participation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_weapons_proliferation_prevention, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nwpp_tr_t0, nuclear_weapons_proliferation_prevention, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nwpp_tr_t15, nuclear_weapons_proliferation_prevention, theater_ratio, 15, 0.62).
narrative_ontology:measurement(nwpp_tr_t30, nuclear_weapons_proliferation_prevention, theater_ratio, 30, 0.68).
narrative_ontology:measurement(nwpp_tr_t45, nuclear_weapons_proliferation_prevention, theater_ratio, 45, 0.71).

% Extraction over time
narrative_ontology:measurement(nwpp_be_t0, nuclear_weapons_proliferation_prevention, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nwpp_be_t15, nuclear_weapons_proliferation_prevention, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(nwpp_be_t30, nuclear_weapons_proliferation_prevention, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(nwpp_be_t45, nuclear_weapons_proliferation_prevention, base_extractiveness, 45, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_weapons_proliferation_prevention, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_weapons_proliferation_prevention, uranium_enrichment_technology_access).
narrative_ontology:affects_constraint(nuclear_weapons_proliferation_prevention, nuclear_security_alliance_coupling).
narrative_ontology:affects_constraint(nuclear_weapons_proliferation_prevention, disarmament_commitment_credibility).

% DUAL FORMULATION NOTE:
% The proliferation prevention regime decomposes into multiple structurally distinct constraints: uranium enrichment access (technology/economic), security alliance coupling (geopolitical), and disarmament commitment credibility (institutional). Each has different extractiveness values; the NPT regime coordinates across all three. This story models the regime as a unified constraint; downstream stories model the specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_weapons_proliferation_prevention, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
