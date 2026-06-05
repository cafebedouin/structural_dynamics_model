% ============================================================================
% CONSTRAINT STORY: voice_without_exit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_voice_without_exit, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: voice_without_exit
 *   human_readable: Voice Without Exit: The Performance Trap in Captive Advocacy
 *   domain: political_philosophy/organizational_theory/ethics_of_agency
 *
 * SUMMARY:
 *   Albert Hirschman's Exit, Voice, and Loyalty (1970) identified voice and
 *   exit as complementary mechanisms for organizational accountability:
 *   members can either complain (voice) or leave (exit), and the credibility
 *   of exit threat disciplines organizational responsiveness to voice. The
 *   voice-without-exit constraint emerges when exit barriers (economic
 *   dependency, geographic constraints, identity fusion, switching costs)
 *   eliminate the exit threat, transforming voice from a disciplining
 *   mechanism into performative ritual. This constraint operates across
 *   scales: individual employees in monopsony labor markets, citizens in
 *   authoritarian states, members of identity-constituted communities,
 *   patients in healthcare systems with high switching costs, students in
 *   educational institutions with credential lock-in. The structural delta is
 *   the transformation of advocacy into performance when departure is not
 *   credible. The primary observable is the Hirschman voice/exit ratio:
 *   organizational response rates to complaints from mobile vs captive
 *   members. The constraint exhibits all major DR types from different
 *   perspectives, making it a diagnostic exemplar for how exit options
 *   modulate experienced extraction. The theater_ratio (0.65) reflects that
 *   formal grievance mechanisms (HR complaint systems, suggestion boxes,
 *   employee surveys, town halls) persist as performance: leadership
 *   maintains the ritual because it signals responsiveness, but actual
 *   responsiveness correlates with complainant exit options rather than with
 *   grievance procedure compliance or complaint merit.
 *
 * KEY AGENTS:
 *   - Captive Members: Primary victims (powerless/trapped) — cannot credibly threaten exit due to economic dependency, geographic constraints, or identity lock; voice becomes performative ritual that leadership can safely ignore
 *   - Constrained Advocates: Secondary victims (moderate/constrained) — have exit options but at significant cost; voice retains some disciplining force but is weakened by leadership's knowledge of exit barriers
 *   - Mobile Members: Primary beneficiaries (powerful/arbitrage) — can exit costlessly to equivalent alternatives; voice carries full disciplining force because departure is credible threat
 *   - Organizational Leadership: Mixed position (institutional/mobile for mobile members, institutional/mobile for captive members) — benefits from ability to extract performative compliance from captive members while facing genuine accountability pressure from mobile members
 *   - Labor Organizing Coalition: Organized agents (organized/constrained) — building collective exit capacity through union formation, mutual aid networks, and portable benefits; see the constraint as temporary with sunset logic
 *   - Formal Grievance System: Institutional ritual (institutional/mobile) — maintains performative complaint mechanisms that signal responsiveness while actual responsiveness tracks exit credibility
 *   - Organizational Accountability: Abstract victim (powerless/trapped) — the collective good of responsive governance that cannot exit or organize; bears full cost of differential responsiveness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(voice_without_exit, 0.38).
domain_priors:suppression_score(voice_without_exit, 0.48).
domain_priors:theater_ratio(voice_without_exit, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(voice_without_exit, extractiveness, 0.38).
narrative_ontology:constraint_metric(voice_without_exit, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(voice_without_exit, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(voice_without_exit, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(voice_without_exit, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(voice_without_exit, tangled_rope).
narrative_ontology:human_readable(voice_without_exit, "Voice Without Exit: The Performance Trap in Captive Advocacy").
narrative_ontology:topic_domain(voice_without_exit, "political_philosophy/organizational_theory/ethics_of_agency").

domain_priors:requires_active_enforcement(voice_without_exit).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(voice_without_exit, organizational_leadership).
narrative_ontology:constraint_beneficiary(voice_without_exit, mobile_members).
narrative_ontology:constraint_victim(voice_without_exit, captive_members).
narrative_ontology:constraint_victim(voice_without_exit, organizational_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAPTIVE MEMBER (SNARE) — Cannot credibly threaten exit due to economic dependency, geographic constraints, or identity lock. Voice becomes performative ritual that leadership can safely ignore. Experiences maximum extraction: required to perform advocacy theater while bearing full cost of unresponsiveness.
constraint_indexing:constraint_classification(voice_without_exit, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CONSTRAINED ADVOCATE (TANGLED ROPE) — Has exit options but at significant cost (career damage, relocation burden, loss of seniority). Voice retains some disciplining force but is weakened by leadership's knowledge of exit barriers. Experiences mixed extraction: genuine coordination function (complaints do sometimes produce change) alongside asymmetric power (leadership responds selectively based on exit credibility).
constraint_indexing:constraint_classification(voice_without_exit, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MOBILE MEMBER (ROPE) — Can exit costlessly to equivalent alternatives. Voice carries full disciplining force because departure is credible threat. Experiences the constraint as pure coordination: advocacy mechanism works as designed because leadership must respond or lose valuable member.
constraint_indexing:constraint_classification(voice_without_exit, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZATIONAL LEADERSHIP RE MOBILE MEMBERS (ROPE) — Experiences voice from mobile members as genuine coordination mechanism. Must respond to complaints because exit threat is credible. The constraint functions as intended: voice disciplines leadership behavior.
constraint_indexing:constraint_classification(voice_without_exit, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZATIONAL LEADERSHIP RE CAPTIVE MEMBERS (TANGLED ROPE) — Benefits from ability to extract performative compliance (voice theater) without accountability pressure. Also benefits from genuine coordination when captive members identify real problems. Mixed extraction: the voice mechanism provides useful information while allowing leadership to ignore costly complaints from those who cannot leave.
constraint_indexing:constraint_classification(voice_without_exit, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: LABOR ORGANIZING COALITION (SCAFFOLD) — Organized agents building collective exit capacity through union formation, mutual aid networks, and portable benefits. See the voice-without-exit trap as temporary coordination failure with sunset: as collective bargaining and benefit portability mature, individual exit constraints decline and voice regains disciplining force. Estimated sunset: 15-25 years for labor mobility infrastructure to restore exit credibility.
constraint_indexing:constraint_classification(voice_without_exit, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: FORMAL GRIEVANCE SYSTEM (PITON) — The institutional complaint mechanisms (HR departments, suggestion boxes, employee surveys, town halls) persist as theater. Leadership maintains the ritual because it signals responsiveness while actual responsiveness correlates with complainant exit options, not with grievance procedure. The system sees its own degradation: procedures designed for accountability now function primarily as performance.
constraint_indexing:constraint_classification(voice_without_exit, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, voice-without-exit represents genuine coordination problem (organizations need feedback mechanisms) with embedded extraction (differential responsiveness based on exit credibility creates two-tier accountability). The constraint is not a natural law — exit barriers are contingent institutional arrangements — but neither is it pure extraction. Hirschman's framework reveals the structural asymmetry: voice and exit are complements, not substitutes, and voice without exit threat degrades into performance.
constraint_indexing:constraint_classification(voice_without_exit, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(voice_without_exit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(voice_without_exit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(voice_without_exit, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(voice_without_exit, TR),
    TR >= 0.70.

:- end_tests(voice_without_exit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts performative compliance from captive members (required to voice complaints through formal channels that leadership can safely ignore) while providing genuine coordination for mobile members (whose complaints receive responsive action). The extraction is real but not maximal — some captive members do receive responses when their complaints align with leadership priorities or when collective action temporarily restores exit credibility. Suppression (0.48): Moderate. Exit barriers include economic dependency (monopsony labor markets, employer-provided healthcare, pension vesting), geographic constraints (rural areas with single dominant employer, housing costs in high-opportunity regions), identity lock (professional identity fused with organizational membership, community belonging), and switching costs (credential non-portability, seniority loss, social network disruption). Suppression is significant but not total — some members can and do exit, and collective organizing can reduce individual exit barriers. Theater ratio (0.65): Moderate-high. Formal grievance mechanisms persist as performance: HR departments, suggestion boxes, employee surveys, and town halls signal responsiveness while actual responsiveness correlates with complainant exit options rather than with procedure compliance. The theater has increased over the interval as organizations have professionalized grievance rituals (more elaborate procedures, more documentation requirements, more performance of listening) while exit barriers have increased (labor market concentration, healthcare lock-in, housing cost divergence between regions).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from differential exit options. Mobile members see pure coordination (Rope) — the voice mechanism works as designed because their exit threat is credible. Captive members see pure extraction (Snare) — required to perform advocacy theater while bearing full cost of unresponsiveness. Constrained advocates see mixed coordination and extraction (Tangled Rope) — the system both enables and constrains their agency. Organizational leadership sees rope when facing mobile members (must respond to voice) and tangled_rope when facing captive members (can extract performative compliance while selectively responding). The labor organizing coalition sees a temporary problem with a sunset (Scaffold) — collective action and portable benefits are building alternative exit pathways. The formal grievance system sees its own degradation (Piton) — procedures designed for accountability now function primarily as performance. The analytical observer sees tangled_rope at the civilizational level — genuine coordination problem (organizations need feedback mechanisms) with embedded extraction (differential responsiveness based on exit credibility). The perspectival gap is not 'which type is correct?' but 'which exit options are you measuring from?' The presheaf over the observation site reveals that the constraint's type is indexical to exit credibility.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure reveals Hirschman's core insight: voice and exit are complements, not substitutes. Mobile members (d ≈ 0.15, beneficiaries with arbitrage exit) experience low effective extraction because their voice carries disciplining force — leadership must respond or lose valuable members. Captive members (d ≈ 0.95, victims with trapped exit) experience maximum extraction because their voice has degraded into performance — leadership can safely ignore complaints from those who cannot leave. Constrained advocates (d ≈ 0.55, victims with constrained exit) experience intermediate extraction — their voice retains some force but is weakened by leadership's knowledge of exit barriers. Organizational leadership experiences the constraint differently depending on which members they face: as rope when dealing with mobile members (genuine coordination, must respond to voice), as tangled_rope when dealing with captive members (mixed coordination and extraction, can selectively respond based on strategic priorities rather than complaint merit). The directionality asymmetry is the mechanism: the same voice infrastructure produces coordination for those who can exit and extraction for those who cannot.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by showing that the same voice infrastructure produces different constraint types depending on the agent's exit options. The mandatrophy is not 'is voice a coordination mechanism or an extraction mechanism?' but 'for whom?' Voice is coordination for those who can exit (mobile members experience rope) and extraction for those who cannot (captive members experience snare). The analytical classification (tangled_rope) captures this dual nature: the constraint has a genuine coordination function (organizations need feedback mechanisms, and voice does sometimes produce responsive action) AND asymmetric extraction (differential responsiveness based on exit credibility creates two-tier accountability). The scaffold perspective (labor organizing building collective exit capacity) is a real structural feature with sunset logic. The piton perspective (formal grievance systems as degraded ritual) is a real observation of institutional decay. No single type is 'the' answer — the constraint's type is a function of the observer's exit options. Hirschman's framework is itself a presheaf: voice/exit/loyalty are not three separate mechanisms but three perspectives on the same accountability structure, and the constraint's classification depends on which combination the observer can access.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voice_effectiveness_threshold,
    'At what exit cost does voice transition from disciplining mechanism to performative ritual?',
    'Empirical analysis of organizational response rates to complaints stratified by complainant exit options; identification of inflection point where response probability decouples from complaint merit and tracks exit credibility instead',
    'If threshold is low (exit cost > 10% income): most voice is already performative, and the constraint is more extractive than coordination. If threshold is high (exit cost > 50% income): voice retains function for most members, and the constraint is primarily coordination with extraction at the margins.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voice_effectiveness_threshold, empirical, 'Exit cost threshold where voice loses disciplining force').

omega_variable(
    collective_voice_substitution,
    'Can collective voice (union representation, collective bargaining) restore disciplining force when individual exit is not credible?',
    'Comparison of organizational responsiveness to individual vs collective complaints from captive members; analysis of whether collective action substitutes for individual exit threat or merely adds another layer of performance',
    'If collective voice substitutes effectively: the scaffold perspective is confirmed and the constraint has a real sunset. If collective voice also degrades into theater when members cannot exit the collective: the extraction mechanism is deeper than individual-level exit barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_voice_substitution, empirical, 'Whether collective voice can substitute for individual exit credibility').

omega_variable(
    leadership_intentionality,
    'Do organizational leaders consciously exploit exit asymmetry to ignore captive members, or does differential responsiveness emerge from resource constraints and triage logic?',
    'Analysis of internal decision-making processes; comparison of stated vs revealed preferences in complaint handling; identification of whether leadership explicitly tracks complainant exit options or whether differential responsiveness is emergent property of attention allocation',
    'If intentional: the constraint is more extractive (deliberate exploitation). If emergent: the constraint is more coordination failure (leadership would respond to all complaints if resources permitted, and differential responsiveness is rational triage given scarcity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(leadership_intentionality, conceptual, 'Whether differential responsiveness is intentional exploitation or emergent triage').

omega_variable(
    identity_lock_persistence,
    'For members who are identity-locked rather than materially trapped, does voice retain disciplining force through reputational mechanisms even when exit is not credible?',
    'Analysis of organizational responsiveness to complaints from identity-locked members (those who could materially exit but whose identity is fused with membership); comparison with responsiveness to materially trapped members',
    'If identity-locked members receive higher responsiveness than materially trapped members despite similar exit credibility: the extraction mechanism is material dependency, not exit barriers per se. If responsiveness is equivalent: identity lock and material trap produce equivalent voice degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether identity lock produces different voice dynamics than material entrapment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(voice_without_exit, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vwe_tr_t0, voice_without_exit, theater_ratio, 0, 0.35).
narrative_ontology:measurement(vwe_tr_t8, voice_without_exit, theater_ratio, 8, 0.5).
narrative_ontology:measurement(vwe_tr_t16, voice_without_exit, theater_ratio, 16, 0.58).
narrative_ontology:measurement(vwe_tr_t25, voice_without_exit, theater_ratio, 25, 0.65).

% Extraction over time
narrative_ontology:measurement(vwe_be_t0, voice_without_exit, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(vwe_be_t8, voice_without_exit, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(vwe_be_t16, voice_without_exit, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(vwe_be_t25, voice_without_exit, base_extractiveness, 25, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(voice_without_exit, enforcement_mechanism).
narrative_ontology:affects_constraint(voice_without_exit, monopsony_labor_market).
narrative_ontology:affects_constraint(voice_without_exit, employer_healthcare_lock).
narrative_ontology:affects_constraint(voice_without_exit, credential_portability_barrier).

% DUAL FORMULATION NOTE:
% The voice-without-exit constraint is downstream of exit_cost_asymmetry (the mountain-level observation that exit costs are structurally asymmetric between mobile and captive agents) but represents a distinct organizational-level constraint. The upstream constraint establishes that exit barriers exist; this constraint models how those barriers transform voice from disciplining mechanism to performative ritual. The constraints form a family: exit_cost_asymmetry (mountain, ε=0.08) → voice_without_exit (tangled_rope, ε=0.38) → specific institutional manifestations (monopsony labor markets, healthcare lock-in, credential barriers). Each has its own extractiveness value reflecting different structural levels.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
