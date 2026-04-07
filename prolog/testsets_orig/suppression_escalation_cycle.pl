% ============================================================================
% CONSTRAINT STORY: suppression_escalation_cycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_suppression_escalation_cycle, []).

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
 *   constraint_id: suppression_escalation_cycle
 *   human_readable: Suppression Escalation Cycle
 *   domain: institutional/political/psychological
 *
 * SUMMARY:
 *   The suppression escalation cycle describes a structural trap in which
 *   institutional attempts to control dissent, resistance, or unauthorized
 *   voice trigger counter-innovations by targets, which in turn trigger
 *   institutional escalation, which triggers further target innovation, in a
 *   self-reinforcing cycle. The cycle is neither pure coordination (as the
 *   suppressing institution perceives it) nor pure extraction (as the target
 *   experiences it) but a hybrid dynamic where each agent's actions create
 *   the conditions for the other's escalation. The constraint exhibits all
 *   eight perspectives as a diagnostic exemplar for how suppression
 *   mechanisms, identity fusion, institutional degradation, and democratic
 *   transition dynamics interact. The extractiveness value (0.58) reflects
 *   that the primary extraction (concentration of coercive capacity and
 *   authorized voice in institutional hands) is moderate in the early phases
 *   and rises as the cycle accelerates. The theater ratio (0.65) captures
 *   that much suppression apparatus activity is performative — maintaining
 *   the appearance of order and information control — while actual
 *   information flow increasingly escapes institutional channels. The
 *   measurement trajectory shows extractiveness rising from 0.35 to 0.61 over
 *   nine time units as suppression escalates, then declining slightly to 0.58
 *   as the cycle reaches unsustainability. Theater ratio climbs from 0.48 to
 *   0.68 as institutional suppression becomes more ritual-bound and
 *   disconnected from actual information control. This pattern is
 *   characteristic of cycles that reach critical instability and trigger
 *   regime transition or institutional collapse.
 *
 * KEY AGENTS:
 *   - Targeted Population: Primary victim (powerless/trapped) — bears full cost of suppression escalation through restrictions on freedom, economic penalty, legal jeopardy, psychological toll
 *   - Identity-Locked Resister: Secondary victim (moderate/identity_locked) — cannot exit without abandoning core identity; experiences both coordination (mutual support networks) and extraction (rising enforcement costs)
 *   - Suppressing Institution: Primary beneficiary (institutional/arbitrage) — controls coercive apparatus, authorized speech, and institutional legitimacy; perceives suppression as coordination mechanism for order
 *   - Alternative Voice Ecosystem: Victim collective (powerless/trapped) — decentralized network bearing innovation and maintenance costs of evading suppression; trades functional voice for distributed resilience
 *   - Degraded Censorship Apparatus: Institutional actor (institutional/constrained) — formal censorship mechanisms persist through inertia despite functional obsolescence; theater-heavy
 *   - Powerful Dissident: Secondary beneficiary (powerful/mobile) — rare status-dissent combination enables selective suppression exposure and rare exit options; visible resistance reduces ecosystem theater
 *   - Democratic Transition Coalition: Organized external actor (organized/constrained) — promotes institutional openness and speech norms; temporary support structure with potential sunset
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent features of power asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(suppression_escalation_cycle, 0.58).
domain_priors:suppression_score(suppression_escalation_cycle, 0.72).
domain_priors:theater_ratio(suppression_escalation_cycle, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(suppression_escalation_cycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(suppression_escalation_cycle, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(suppression_escalation_cycle, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(suppression_escalation_cycle, snare).
narrative_ontology:human_readable(suppression_escalation_cycle, "Suppression Escalation Cycle").
narrative_ontology:topic_domain(suppression_escalation_cycle, "institutional/political/psychological").

domain_priors:requires_active_enforcement(suppression_escalation_cycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(suppression_escalation_cycle, suppressing_institution).
narrative_ontology:constraint_victim(suppression_escalation_cycle, targeted_population).
narrative_ontology:constraint_victim(suppression_escalation_cycle, alternative_voice_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SILENCED TARGET (SNARE) — Trapped population experiences maximal extraction. Suppression mechanisms (legal, economic, social) prevent exit. Each failed suppression triggers escalation, forcing target to invest increasing resources in mere survival or minimal voice. Cycle itself becomes the extraction mechanism — targets bear rising costs of escalating counter-suppression.
constraint_indexing:constraint_classification(suppression_escalation_cycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IDENTITY-LOCKED RESISTER (TANGLED ROPE) — Resister identity fused with opposition to suppression; cannot exit without abandoning core self-concept. Structurally mobile but identity-bound. Experiences genuine coordination (resisters organize mutual support, information sharing) alongside extraction (escalating suppression forces resource mobilization). Classification: Rope if identity frame shifted; Mountain if structural barriers absolute; Tangled Rope given actual bind of identity + rising enforcement costs.
constraint_indexing:constraint_classification(suppression_escalation_cycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: SUPPRESSING INSTITUTION (ROPE) — Experiences suppression as coordination mechanism: maintaining social order, controlling information flow, protecting institutional legitimacy. Suppression apparatus is solution to collective action problem from institutional perspective. Benefits from monopoly on authorized speech. Arbitrage exit available: can shift to dialogue if institutional power position sufficiently secure. Sees the cycle as stabilizing, not destabilizing.
constraint_indexing:constraint_classification(suppression_escalation_cycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ALTERNATIVE VOICE ECOSYSTEM (SNARE) — Decentralized network of marginalized media, underground publishing, diaspora communication. Each suppression escalation triggers ecosystem innovation (new platforms, routing, encryption). But the cost of maintenance and innovation is borne by ecosystem members with no institutional backing. Theater ratio high: much activity appears as survival or symbolic resistance rather than effective voice amplification. Trapped in the cycle — cannot exit without losing communication function entirely.
constraint_indexing:constraint_classification(suppression_escalation_cycle, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: DEGRADED CENSORSHIP APPARATUS (PITON) — Formal censorship systems (licensing boards, review committees, content moderation rules) persist long after their functional necessity has declined. Theater ratio high: reviewing content, maintaining bureaucracy, ritualistic enforcement consume resources while internet-scale information flow renders centralized censorship mechanically obsolete. Apparatus persists through institutional inertia. Decentralized alternatives (algorithm filtering, platform enforcement, peer moderation) have largely replaced formal censorship, but formal mechanisms persist for legitimacy theater.
constraint_indexing:constraint_classification(suppression_escalation_cycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: POWERFUL DISSIDENT (TANGLED ROPE) — High-status individual (academic, artist, former official) with rare combination of power and dissent. Experiences suppression escalation as selective extraction: some suppression mechanisms (legal action, funding cuts) constrain movement, but status creates exit options unavailable to powerless targets. Coordination function: powerful dissident's visible resistance reduces theater ratio for entire ecosystem by demonstrating that suppression is selective rather than universal. Extraction function: institution focuses suppression on powerless while leaving powerful dissident constrained but visible.
constraint_indexing:constraint_classification(suppression_escalation_cycle, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: DEMOCRATIC TRANSITION COALITION (SCAFFOLD) — Coalition of international actors (UN bodies, treaty organizations, civil society networks) promoting institutional openness and speech norms. Temporary support structure: suppression escalation eventually triggers regime transition, democratic opening, or international pressure sufficient to require institutional recalibration. Sunset clause: if sufficient external pressure and internal contradictions accumulate, suppression escalation becomes unsustainable and apparatus collapses. Coordination function: coalition models and advocates for alternative governance. Extraction function: transition mechanisms often impose costs on previously suppressed populations (vetting, lustration, truth commissions). Theater ratio moderate: much coalition activity is advocacy and normative work rather than direct pressure.
constraint_indexing:constraint_classification(suppression_escalation_cycle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal scope, suppression escalation cycles are inherent features of power asymmetry: when one agent controls coercive capacity and another seeks to evade control, the mathematical structure of the system implies escalation. Information-theoretic view: if target innovation in evasion exceeds suppressing institution's innovation in control, cycle is unsustainable; if institution's capacity exceeds target innovation, equilibrium emerges. This perspective risks naturalizing what is actually a contingent institutional arrangement with many historical alternatives (negotiation, co-optation, institutional reform, power-sharing).
constraint_indexing:constraint_classification(suppression_escalation_cycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(suppression_escalation_cycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(suppression_escalation_cycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(suppression_escalation_cycle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(suppression_escalation_cycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(suppression_escalation_cycle, TR),
    TR >= 0.70.

:- end_tests(suppression_escalation_cycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The suppressing institution extracts by concentrating voice, controlling information flow, and restricting target agency. The extraction is not maximal because targets retain some innovation capacity and alternative voice ecosystems survive, albeit with high costs. The rising trajectory from 0.35 to 0.61 reflects escalation as extraction deepens — initial suppression is selective and lower-cost; as targets innovate, suppression must escalate, raising institutional enforcement costs alongside target resistance costs. The slight decline from 0.61 to 0.58 at the cycle's critical point suggests unsustainability — at maximum escalation, the cycle becomes brittle and prone to collapse or transition. Suppression (0.72): High. Barriers to target exit and voice are substantial: legal prohibitions, economic sanctions, social stigma, psychological internalization of suppression narratives. Alternative voice ecosystems operate at high cost (distributed resources, encryption overhead, operational risk). Yet suppression is not absolute — some channels remain open, some information escapes, some individuals maintain public voice despite cost. Theater ratio (0.65): Moderately high. Suppression apparatus maintains performative elements (trials, licenses, bureaucratic review) that consume resources without fully controlling information flow. Alternative voice ecosystems develop workarounds (encrypted platforms, diaspora networks, underground publishing) that reduce institutional suppression's functional effectiveness. As cycle escalates, theater ratio increases because institutional suppression becomes more ritual-bound (elaborate legal frameworks, extensive security justifications) while actual information control erodes. This is the diagnostic signature of Piton dynamics within a Snare structure.
 *
 * PERSPECTIVAL GAP:
 *   The suppression escalation cycle produces eight distinct classifications from the same base properties, making it a diagnostic exemplar for perspectival pluralism. The suppressing institution's Rope classification reflects their genuine experience: suppression solves their coordination problem of maintaining institutional order and authorized narrative. The targeted population's Snare reflects their genuine experience: they are trapped targets with rising extraction costs and no exit. These are not disagreements about facts but about structural position. From the institution's vantage point, suppression is proportionate and functional; from the target's vantage point, suppression is severe and extractive. Neither perspective is false; both are locally true given their structural positions. The identity-locked resister's Tangled Rope reflects a third genuine structure: they are neither pure coordinating agents nor pure extraction targets but trapped in a hybrid where resistance organization (coordination function) and enforcement escalation (extraction function) interact within their identity frame. The powerful dissident's Tangled Rope differs structurally from the identity-locked resister's because the powerful dissident has genuine exit options that the identity-locked agent lacks, shifting their experienced extraction downward. The alternative voice ecosystem's Snare with high theater reflects that decentralized resistance networks bear escalating costs of evasion and innovation while producing diminishing amplification returns — the ecosystem's survival becomes the primary function rather than voice amplification. The degraded censorship apparatus's Piton reflects that formal suppression mechanisms persist despite their mechanical obsolescence in an information-saturated environment — the apparatus survives through institutional inertia and legitimacy theater, not functional necessity. The democratic transition coalition's Scaffold reflects that external pressure creates an exit path for the entire cycle: if institutional suppression becomes unsustainable due to internal contradictions and external pressure, regime transition creates a sunset clause for the entire suppression architecture. The analytical observer's Mountain is a false summit — it naturalizes the escalation cycle as an inevitable feature of power asymmetry, missing the historical fact that many regimes achieve voice governance through negotiation, power-sharing, or institutional reform rather than suppression escalation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality d is computed from their structural position: Are they beneficiaries or targets? Do they have exit capacity? Beneficiaries (suppressing institution: d ≈ 0.20, powerful dissident positioned as partial beneficiary: d ≈ 0.40) experience low or moderate f(d), producing low or moderate experienced extraction. Targets with trapped exit (targeted population, alternative voice ecosystem: d ≈ 0.90-0.95) experience high f(d) around 1.30-1.42, producing high experienced extraction. Identity-locked targets (identity-locked resister: d ≈ 0.75, beneficiary-side identity-locked like a true believer in suppression institution: d ≈ 0.25) experience medium f(d) around 1.05-1.15 because identity fusion creates an additional perceptual layer on top of material position. Organized external agents (democratic transition coalition: d ≈ 0.60) experience moderate f(d) around 0.95-1.05 because they are committed but not directly embedded in the suppression structure. Scope modifiers σ(S) apply: national scope (σ=1.0) is baseline; global scope (σ=1.2) amplifies chi for all agents, making suppression's effects more systemic; universal scope (σ=1.0 for analytical) preserves baseline. The measured suppression of 0.72 is a raw structural property — it does not scale by context, only extractiveness scales by f(d) and σ(S). This reflects that the barriers to exit (legal, economic, social, psychological) are structural features of the suppression apparatus itself, not dependent on whether the target is analytical or powerless.
 *
 * MANDATROPHY ANALYSIS:
 *   The suppression escalation cycle has a measured extractiveness of 0.58, which does not trigger the mandatrophy gate (ε > 0.70). However, the cycle exhibits mandatrophy-like dynamics at the conceptual level: the suppressing institution genuinely perceives itself as solving a coordination problem (order maintenance, institutional preservation), while the target perceives pure extraction. This is not a false mandatrophy (where the analyst incorrectly conflates Rope and Snare) but a genuine perspectival mandatrophy: multiple agents with equal information perceive the same constraint as having opposite primary functions. The resolution is not to choose one perspective as 'correct' but to recognize that suppression escalation cycles inherently hide their extraction function behind coordination rhetoric (the institution's perspective) while making extraction transparent to targets (the target's perspective). The mandatrophy is structural to asymmetric power. The escalation dynamic itself creates a false mandate: institutional actors believe suppression escalation is necessary for order, but the escalation is actually driven by institutional security logic and target innovation — it is not a coordination mechanism but a reactive extraction mechanism masked as order maintenance. The theatrical components (high theater ratio, performative rituals) reveal institutional awareness that suppression's functional necessity is declining (why else the elaborate justification theater?) — this suggests the cycle is reaching unsustainability and approaching a phase transition where institutional actors may be forced to recognize suppression as extraction rather than coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    escalation_threshold_trigger,
    'What determines whether suppression escalation is triggered by target resistance or by institutional preemption independent of target action?',
    'Temporal sequence analysis: Does escalation follow observable target action (delayed reaction) or precede it (preemptive)? Correlation between suppression intensity changes and prior target activity.',
    'If preemptive dominates: escalation is institutional security logic, not reaction. Classification shifts from Snare (reactive extraction) toward Tangled Rope (institution uses escalation as coordination of internal security). If reactive: escalation is extraction mechanism driven by target resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_threshold_trigger, empirical, 'Whether escalation is reactive response or preemptive institutional logic').

omega_variable(
    alternative_voice_ecosystem_sustainability,
    'Can decentralized alternative voice ecosystems sustain themselves indefinitely against escalating suppression, or is their sustainability dependent on external support or institutional weakness?',
    'Long-term survival analysis of underground networks, diaspora media, encrypted platforms under continuous suppression. Correlation between ecosystem innovation and institutional suppression capacity. Cost analysis of ecosystem maintenance vs institutional suppression budget.',
    'If self-sustaining: Snare classification is stable at powerless/trapped perspective. If dependent on external support or institutional weakness: Snare transforms into Tangled Rope when institutional capacity increases, or collapses entirely when suppression overwhelms innovation capacity. Theater ratio interpretation changes based on sustainability mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_voice_ecosystem_sustainability, empirical, 'Sustainability of decentralized alternatives against escalating suppression').

omega_variable(
    identity_locked_exit_capacity,
    'Can identity-locked resisters perceive or execute exit options if suppression intensity reaches critical thresholds, or does identity fusion prevent exit regardless of capacity?',
    'Case studies of resisters exiting suppressive environments: Did exit require identity shift (professional reorientation, relational reconstruction)? Did suppressors offer identity-compatible exit paths? Correlation between identity-lock strength and exit barriers.',
    'If identity exit is possible: identity_locked classification at biographical horizon should shift to constrained or mobile at higher thresholds. If identity-locked is immutable: resister is trapped within identity frame regardless of suppression intensity or institutional capacity. Affects interpretation of whether escalation cycle is structural or psychological.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_capacity, empirical, 'Whether identity-locked resisters can exit when suppression escalates').

omega_variable(
    institutional_suppression_intent,
    'Is suppression escalation driven by institutional intent to eliminate target voice, or by bureaucratic momentum, internal security logic, or reactive overreach?',
    'Analysis of institutional decision-making: Are escalation orders deliberate policy or emergent from security apparatus autonomy? Do institutional leaders reverse escalation if politically costlier than continuation? Correlation between institutional incentives and escalation patterns.',
    'If intentional: Snare classification stable; extraction is institutional strategy. If bureaucratic momentum: Piton classification more appropriate (degraded ritual persistence). If security logic without elimination intent: Tangled Rope more accurate (coordination function exists alongside extraction). Intent classification affects mandatrophy resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_suppression_intent, conceptual, 'Whether suppression escalation is intentional policy or institutional momentum').

omega_variable(
    external_pressure_effectiveness,
    'Can external democratic transition coalitions reduce suppression escalation cycles, or does external pressure trigger nationalist backlash that intensifies institutional suppression?',
    'Longitudinal analysis of suppression changes correlated with international pressure campaigns, sanctions, diplomatic intervention. Comparison of suppression trajectories in high vs low international pressure regimes. Evidence of backfire effect or strategic compliance.',
    'If external pressure effective: Scaffold perspective confirmed; transition coalition has genuine sunset function. If pressure ineffective or backfire-producing: Scaffold is aspirational rather than structural. Alternative exit mechanisms required; cycle may be self-sustaining. Theater ratio of international pressure becomes questionable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_pressure_effectiveness, empirical, 'Whether external pressure effectively reduces institutional suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(suppression_escalation_cycle, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supp_esc_tr_t0, suppression_escalation_cycle, theater_ratio, 0, 0.48).
narrative_ontology:measurement(supp_esc_tr_t3, suppression_escalation_cycle, theater_ratio, 3, 0.56).
narrative_ontology:measurement(supp_esc_tr_t6, suppression_escalation_cycle, theater_ratio, 6, 0.63).
narrative_ontology:measurement(supp_esc_tr_t9, suppression_escalation_cycle, theater_ratio, 9, 0.68).
narrative_ontology:measurement(supp_esc_tr_t12, suppression_escalation_cycle, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(supp_esc_be_t0, suppression_escalation_cycle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(supp_esc_be_t3, suppression_escalation_cycle, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(supp_esc_be_t6, suppression_escalation_cycle, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(supp_esc_be_t9, suppression_escalation_cycle, base_extractiveness, 9, 0.61).
narrative_ontology:measurement(supp_esc_be_t12, suppression_escalation_cycle, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(suppression_escalation_cycle, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(suppression_escalation_cycle, 0.12).
narrative_ontology:affects_constraint(suppression_escalation_cycle, identity_locked_capture).
narrative_ontology:affects_constraint(suppression_escalation_cycle, institutional_innovation_arms_race).
narrative_ontology:affects_constraint(suppression_escalation_cycle, regime_transition_instability).

% DUAL FORMULATION NOTE:
% The suppression escalation cycle is upstream of several downstream constraints. Institutional suppression directly feeds identity_locked_capture (targets internalize suppression narratives and develop identity fusion with resistance identity). The cycle drives institutional_innovation_arms_race (target voice innovation spurs institutional suppression innovation in an escalating competition). The accumulation of suppression unsustainability eventually triggers regime_transition_instability (the cycle becomes brittle and prone to collapse). All three downstream constraints have higher extractiveness values because they are consequences of the escalation cycle reaching critical intensity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(suppression_escalation_cycle, institutional, 0.22).
constraint_indexing:directionality_override(suppression_escalation_cycle, powerful, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
