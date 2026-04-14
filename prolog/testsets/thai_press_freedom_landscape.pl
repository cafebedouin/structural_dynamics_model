% ============================================================================
% CONSTRAINT STORY: thai_press_freedom_landscape
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thai_press_freedom_landscape, []).

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
 *   constraint_id: thai_press_freedom_landscape
 *   human_readable: Thai Press Freedom Constraint
 *   domain: political/media/governance
 *
 * SUMMARY:
 *   Thai press freedom operates as a complex constraint system in which state
 *   security apparatus, incumbent power holders, and institutional actors
 *   coordinate information control through overlapping legal mechanisms
 *   (Computer Crimes Act, lèse-majesté laws, broadcast licensing), economic
 *   leverage (advertising monopolies, market access), and periodic
 *   enforcement escalation. The constraint exhibits asymmetric extraction:
 *   state security apparatus experiences it as coordination (national
 *   stability mechanism), while independent journalists and critical outlets
 *   experience it as pure extraction (snare) with material barriers to exit
 *   and no independent function. The constraint's theater ratio (0.65)
 *   reflects that formal press freedoms are enshrined constitutionally and
 *   discussed rhetorically, while actual enforcement creates a gap between
 *   nominal rights and operational reality. The extractiveness value (0.68)
 *   has increased over the measurement interval (from 0.52 to 0.68 over 10-20
 *   years), indicating accumulation of enforcement mechanisms and tightening
 *   of state control rather than liberalization. The constraint appears
 *   durable through institutional inertia (piton perspective) while
 *   simultaneously facing long-term digital and demographic pressures that
 *   may erode state control capacity (scaffold perspective with generational
 *   sunset logic).
 *
 * KEY AGENTS:
 *   - Independent Journalists: Primary victim (powerless/trapped) — face legal vulnerability, economic dependence, and physical risk; no meaningful exit option
 *   - Critical Media Outlets: Primary victim (moderate/constrained) — constrained by advertising monopolies, licensing control, and legal threats; can reduce operations but cannot exit profitably
 *   - State Security Apparatus: Primary beneficiary (institutional/arbitrage) — experiences constraint as coordination mechanism for national stability; has exit options but does not exercise them
 *   - Incumbent Power Holders: Secondary beneficiary (institutional/arbitrage) — benefit from suppressed opposition narratives; aligned with state security apparatus interests
 *   - Compliant Media Conglomerate: Mixed actor (powerful/mobile) — genuinely powerful but structurally embedded in state-coordinated information ecosystem; benefits from market protection while constrained by editorial guidance
 *   - International Press Freedom Coalition: Organized external actor (organized/constrained) — perceive constraint as temporary and see sunset mechanism through digital/demographic change
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risk naturalizing threat perception from outdated historical periods as current security necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thai_press_freedom_landscape, 0.68).
domain_priors:suppression_score(thai_press_freedom_landscape, 0.72).
domain_priors:theater_ratio(thai_press_freedom_landscape, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thai_press_freedom_landscape, extractiveness, 0.68).
narrative_ontology:constraint_metric(thai_press_freedom_landscape, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(thai_press_freedom_landscape, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thai_press_freedom_landscape, snare).
narrative_ontology:human_readable(thai_press_freedom_landscape, "Thai Press Freedom Constraint").
narrative_ontology:topic_domain(thai_press_freedom_landscape, "political/media/governance").

domain_priors:requires_active_enforcement(thai_press_freedom_landscape).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(thai_press_freedom_landscape, state_security_apparatus).
narrative_ontology:constraint_beneficiary(thai_press_freedom_landscape, incumbent_power_holders).
narrative_ontology:constraint_victim(thai_press_freedom_landscape, independent_journalists).
narrative_ontology:constraint_victim(thai_press_freedom_landscape, critical_media_outlets).
narrative_ontology:constraint_victim(thai_press_freedom_landscape, democratic_discourse).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT JOURNALIST (SNARE) — Trapped by legal restrictions (Computer Crimes Act, lèse-majesté laws), economic dependency on state-controlled advertising revenue, and physical risk. Cannot exit journalism without abandoning professional identity; cannot practice journalism without accepting surveillance and legal vulnerability. Maximum experienced extraction.
constraint_indexing:constraint_classification(thai_press_freedom_landscape, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CRITICAL MEDIA OUTLET (SNARE) — Constrained by advertising monopolies, broadcast licensing control, and periodic legal threats. Can operate at reduced capacity (online-only, self-censored content) but faces high costs to exit (loss of audience, market irrelevance). No genuine coordination function exists — the constraint exists to suppress alternatives.
constraint_indexing:constraint_classification(thai_press_freedom_landscape, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE SECURITY APPARATUS (ROPE) — Perceives the constraint as a coordination mechanism for national stability. Benefits from information control; experiences the constraint as solving a collective action problem (preventing destabilizing speech). The apparatus has exit options (can relax controls) but does not exercise them because the constraint aligns with institutional interests. Net beneficiary.
constraint_indexing:constraint_classification(thai_press_freedom_landscape, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMPLIANT MEDIA CONGLOMERATE (TANGLED ROPE) — Structurally powerful (ownership of major outlets) but accepts state guidance through regulatory negotiation. Benefits from stability and preferential advertising access; bears costs of editorial control and audience skepticism. Coordination function (protecting oligopolistic market structure) is genuine; extraction is asymmetric (profitable for the conglomerate, constraining for public discourse).
constraint_indexing:constraint_classification(thai_press_freedom_landscape, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL PRESS FREEDOM COALITION (SCAFFOLD) — Organized actors (UNESCO, CPJ, international news agencies) experience the constraint as temporary through the lens of generational normative change and external pressure. See sunset mechanism: digital technologies, regional integration (ASEAN), and demographic shifts creating younger generations with access to information via VPN and encrypted messaging. Sunset estimated at 15-30 years as digital natives become political majority.
constraint_indexing:constraint_classification(thai_press_freedom_landscape, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — From civilizational timescale, the press freedom constraint persists through institutional inertia. The mechanics that justified information control during Cold War and communist insurgency threat (1950s-1980s) continue in performative form despite threat reduction. Theater ratio high (0.65) — media freedom is discussed in official rhetoric, independent outlets exist as symbolic exceptions, constitutional guarantees nominally protect press, yet actual extraction mechanisms persist unchanged. The constraint is maintained not because it solves the original coordination problem but because dismantling it would require institutional reform and power redistribution.
constraint_indexing:constraint_classification(thai_press_freedom_landscape, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thai_press_freedom_landscape_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(thai_press_freedom_landscape, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thai_press_freedom_landscape, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(thai_press_freedom_landscape, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(thai_press_freedom_landscape, TR),
    TR >= 0.70.

:- end_tests(thai_press_freedom_landscape_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting significant asymmetric extraction from journalists and critical outlets. The value is not 0.85+ because some coordination function exists (state perception of stability benefit is genuine, even if not universally accepted), and some outlets maintain operational capacity through self-adjustment rather than total suppression. The upward trajectory (0.52→0.68 over measurement interval) indicates compression of press freedom over time, not liberalization. Suppression (0.72): High. Multiple structural barriers exist: legal restrictions (Computer Crimes Act §ยง14.2, lèse-majesté law §ยง112), economic dependency on state-controlled advertising and market access, physical risk from enforcement, and surveillance capacity. These are not trivial costs — they represent structural immobility for many journalists. However, suppression is not 0.95 because some outlets do operate openly, some journalists do practice critical reporting (with elevated risk), and escape routes exist (international bureaus, digital platforms, private investigation). Theater ratio (0.65): Moderate-high. Thailand has constitutional press guarantees, independent outlets are nominally permitted, media pluralism is discussed in official discourse, and occasional prosecutions create the appearance of rule-of-law enforcement. Yet the enforcement pattern is selective (political speech suppressed more than financial crime exposure), and the gap between nominal freedoms and operational reality is substantial. The theater ratio increased over the interval as state control mechanisms became more sophisticated and legalized.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the victim (independent journalist) and beneficiary (state apparatus) positions. The journalist sees a snare — pure extraction with no coordination function from their position. The apparatus sees a rope — a coordination mechanism for collective stability. The compliant conglomerate sees a tangled rope — genuine market protection and stability coordination mixed with editorial constraints that reduce content quality and audience reach. The international coalition sees a scaffold with sunset — viewing the constraint as temporary and overcome by digital/demographic forces. The piton perspective reveals that much of the enforcement theater (courtroom procedures, written law) persists despite reduced threat justification, suggesting institutional inertia rather than current security necessity. The analytical observer risks the false summit of naturalizing this as inherent to governance, when it is actually a contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The journalist perspective (powerless/trapped) derives d ≈ 0.95 from the victim position and trapped exit options. No arbitrage escape exists; structural mobility is minimal (cannot practice journalism without vulnerability). The apparatus perspective (institutional/arbitrage) derives d ≈ 0.10 from the beneficiary position and arbitrage exit options (can relax controls if desired). The compliant conglomerate (powerful/mobile) derives d ≈ 0.40 from the mixed position: benefits from market protection but not as much as apparatus, and has higher exit capacity (could break with state guidance but chooses not to). These directionality values feed into the sigmoid f(d) to produce experienced extractiveness chi. Journalists experience near-maximal chi; apparatus experiences near-zero or negative chi (coordination benefit); conglomerate experiences moderate chi.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids mislabeling by correctly identifying the asymmetric extraction (snare from victim perspective) while acknowledging the genuine coordination function for state security apparatus (rope from apparatus perspective). The mandatrophy is resolved by accepting that the constraint IS both snare and rope, depending on structural position. The error would be to collapse this to a single type: calling it 'just a rope' naturalizes extraction, while calling it 'just a snare' erases the genuine stability coordination function. The tangled rope classification (for the compliant conglomerate) captures the mixed position precisely. The piton classification reveals that theater ratio is increasing while the underlying justification (coup/destabilization risk) may be decreasing, suggesting the constraint's lifetime is bounded by institutional reform pressure and demographic change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    computer_crimes_act_scope_ambiguity,
    'Is the Computer Crimes Act a proportionate response to genuine cybersecurity threats or a pretext for suppressing political speech?',
    'Comparative analysis of enforcement: ratio of prosecutions targeting actual cybercrime vs political speech; jurisdiction correlation between security incidents and prosecution patterns',
    'If genuine cybersecurity tool: classification shifts toward tangled_rope (coordination function is real). If pretext: classification reinforces snare (pure extraction mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computer_crimes_act_scope_ambiguity, empirical, 'Whether Computer Crimes Act functions as security tool or speech suppression mechanism').

omega_variable(
    lese_majeste_enforcement_discretion,
    'Does lèse-majesté law enforcement reflect genuine protection of royal institution or provide discretionary tool for political suppression?',
    'Analysis of prosecution patterns: correlation between enforcement waves and political transitions; distribution of defendants across class/power levels; judicial consistency in sentencing',
    'If institutional protection: snare classification stands (asymmetric extraction justified by state security). If political tool: snare deepens into pure extraction mechanism with no coordination rationale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lese_majeste_enforcement_discretion, empirical, 'Lèse-majesté enforcement pattern and discretionary scope').

omega_variable(
    digital_substitution_timeline,
    'How quickly will encrypted messaging, VPN access, and digital-native cohorts make centralized press control technically or politically obsolete?',
    'Tracking of digital-media adoption rates, VPN penetration, encrypted messaging platform usage; demographic analysis of news consumption by age cohort; generational attitude shifts toward information access',
    'If timeline < 10 years: scaffold perspective accelerates — sunset is imminent. If timeline > 30 years: scaffold is aspirational; state control mechanism is more durable than international optimism suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_substitution_timeline, empirical, 'Digital technology and demographic substitution timeline for press control').

omega_variable(
    military_coup_risk_and_constraint_duration,
    'How much of the current press freedom constraint is justified by actual coup/destabilization risk vs. how much reflects historical threat perception that is no longer proportionate?',
    'Risk assessment: comparative stability data (coup frequency 2000-2025 vs 1960-2000); analysis of insurgency/separatist threat reduction; expert panel assessment of current vs historical threat magnitude',
    'If actual risk is high: snare classification justified as security necessity (though still extraction). If risk is low and threat perception is historical: piton classification strengthens — constraint persists through obsolete institutional logic rather than current security needs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_coup_risk_and_constraint_duration, empirical, 'Actual coup and destabilization risk versus historical threat perception').

omega_variable(
    identity_lock_mechanism_for_journalists,
    'Are Thai journalists constrained by material barriers (legal, economic, physical) or by identity fusion with particular editorial lines that make independent practice feel impossible?',
    'Qualitative research: interviewer data on journalists'' sense of agency; trajectory analysis of journalists who exit to regional/international outlets (do they rediscover agency?); comparison of self-reported barriers to actual prosecutions',
    'If primarily material barriers: trapped exit option is correct; snare classification stands. If identity fusion dominates: identity_locked exit option better captures the mechanism; classification may shift to rope from identity-locked perspective at biographical timescale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_journalists, empirical, 'Whether journalist constraint is material or identity-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thai_press_freedom_landscape, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thai_press_tr_t0, thai_press_freedom_landscape, theater_ratio, 0, 0.48).
narrative_ontology:measurement(thai_press_tr_t5, thai_press_freedom_landscape, theater_ratio, 5, 0.58).
narrative_ontology:measurement(thai_press_tr_t10, thai_press_freedom_landscape, theater_ratio, 10, 0.65).
narrative_ontology:measurement(thai_press_tr_t15, thai_press_freedom_landscape, theater_ratio, 15, 0.67).

% Extraction over time
narrative_ontology:measurement(thai_press_be_t0, thai_press_freedom_landscape, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(thai_press_be_t5, thai_press_freedom_landscape, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(thai_press_be_t10, thai_press_freedom_landscape, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(thai_press_be_t15, thai_press_freedom_landscape, base_extractiveness, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thai_press_freedom_landscape, enforcement_mechanism).
narrative_ontology:affects_constraint(thai_press_freedom_landscape, thai_lese_majeste_law_enforcement).
narrative_ontology:affects_constraint(thai_press_freedom_landscape, thai_computer_crimes_act_scope).
narrative_ontology:affects_constraint(thai_press_freedom_landscape, thai_broadcast_licensing_control).
narrative_ontology:affects_constraint(thai_press_freedom_landscape, thai_digital_information_access).

% DUAL FORMULATION NOTE:
% Thai press freedom decomposes into four structurally distinct constraints: the general landscape (this story, ε=0.68, snare from victim perspective), the specific lèse-majesté enforcement mechanism (higher ε, pure extraction), the Computer Crimes Act discretionary application (lower ε initially, higher ε recently), and the broadcast licensing system (moderate ε, more coordination-like). Each has different empirical bases and different beneficiary/victim profiles. The landscape story covers the aggregate effect; decomposition enables analysis of specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(thai_press_freedom_landscape, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
