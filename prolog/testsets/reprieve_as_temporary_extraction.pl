% ============================================================================
% CONSTRAINT STORY: reprieve_as_temporary_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reprieve_as_temporary_extraction, []).

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
 *   constraint_id: reprieve_as_temporary_extraction
 *   human_readable: Reprieve as Temporary Extraction
 *   domain: general/social_dynamics
 *
 * SUMMARY:
 *   A reprieve is a temporary suspension of enforcement of an obligation
 *   (debt payment, sentence, duty, punishment) while the underlying
 *   obligation formally persists. The constraint of
 *   reprieve-as-temporary-extraction arises when the reprieve mechanism
 *   itself becomes a vehicle for increased control: the reprieve creates
 *   psychological dependency ('I owe gratitude'), increases future compliance
 *   pressure ('I must earn my reprieve'), and systematizes re-obligation
 *   through cycles of partial relief and re-enforcement. The reprieve
 *   recipient experiences this as a trap — refusal of reprieve is interpreted
 *   as defiance and triggers harsher enforcement, while acceptance of
 *   reprieve locks them into a cycle where the debt is never actually
 *   cleared, only deferred. The constraint exhibits the full span of DR types
 *   depending on perspective: the authority experiences reprieve as pure
 *   coordination (temporarily suspending enforcement while maintaining
 *   accountability), organized reformers see it as a temporary scaffold with
 *   a sunset (systemic reform can replace the reprieve cycle), the
 *   institutional system maintains reprieve rituals with high theater (the
 *   machinery of mercy persists through institutional inertia), moderate
 *   victims experience mixed coordination and extraction (some relief but
 *   increased future obligation), the powerless victims experience pure
 *   extraction (the reprieve itself becomes the trap), and from a
 *   civilizational analytical view, the constraint risks being naturalized as
 *   an immutable feature of obligation itself.
 *
 * KEY AGENTS:
 *   - Reprieve Recipient: Primary victim (powerless/trapped) — experiences reprieve as a trap that increases control while providing temporary relief
 *   - Temporary Authority: Primary beneficiary (institutional/arbitrage) — grants reprieve to optimize extraction capacity and reduce enforcement costs
 *   - Future Obligation Bearer: Secondary victim (moderate/constrained) — the reprieve recipient who must navigate the reprieve period while incurring secondary obligations
 *   - Status Quo Beneficiary: Institutional beneficiary (institutional/arbitrage) — systems and power structures that benefit from the reprieve/re-obligation cycle
 *   - Reform Coalition: Organized agents (organized/constrained) — labor movements, debt forgiveness coalitions, restorative justice advocates viewing reprieve as temporary structure to be replaced
 *   - Institutionalized Reprieve System: Institutional maintenance (institutional/arbitrage) — formal machinery of reprieve (parole boards, debt forgiveness committees, clemency procedures) that sustains reprieve rituals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reprieve_as_temporary_extraction, 0.58).
domain_priors:suppression_score(reprieve_as_temporary_extraction, 0.65).
domain_priors:theater_ratio(reprieve_as_temporary_extraction, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reprieve_as_temporary_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(reprieve_as_temporary_extraction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(reprieve_as_temporary_extraction, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reprieve_as_temporary_extraction, tangled_rope).
narrative_ontology:human_readable(reprieve_as_temporary_extraction, "Reprieve as Temporary Extraction").
narrative_ontology:topic_domain(reprieve_as_temporary_extraction, "general/social_dynamics").

domain_priors:requires_active_enforcement(reprieve_as_temporary_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reprieve_as_temporary_extraction, temporary_authority).
narrative_ontology:constraint_beneficiary(reprieve_as_temporary_extraction, debt_holder).
narrative_ontology:constraint_beneficiary(reprieve_as_temporary_extraction, status_quo_beneficiary).
narrative_ontology:constraint_victim(reprieve_as_temporary_extraction, reprieve_recipient).
narrative_ontology:constraint_victim(reprieve_as_temporary_extraction, future_obligation_bearer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REPRIEVE RECIPIENT (SNARE) — The agent experiences temporary relief from immediate enforcement as a trap. The reprieve creates psychological dependency ('I owe a debt of gratitude') and locks the agent into compliance during the reprieve period. The alternative enforcement mechanisms (social shame, institutional retaliation, escalated punishment if reprieve is violated) create suppression exceeding what existed before the reprieve. The agent cannot exit: refusal of reprieve signals defiance, acceptance locks them in. Maximum experienced extraction because the reprieve itself becomes the enforcement mechanism.
constraint_indexing:constraint_classification(reprieve_as_temporary_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: FUTURE OBLIGATION BEARER (TANGLED ROPE) — The reprieve is marketed as mercy or coordination (relieving immediate hardship), but it systematizes extraction: the recipient now owes not just the original obligation but the reprieve itself. The reprieve creates a coordination function (all parties agree some temporary relief is beneficial) alongside asymmetric extraction (the cost of the reprieve — gratitude, future compliance, increased constraint on resistance — is borne entirely by the recipient). The future obligation bearer faces constrained exit: they can technically walk away from the debt, but the reprieve has increased the cost through social and psychological mechanisms.
constraint_indexing:constraint_classification(reprieve_as_temporary_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TEMPORARY AUTHORITY (ROPE) — The authority that grants the reprieve experiences it as pure coordination: temporarily suspending enforcement while maintaining the underlying obligation solves the collective action problem of keeping the recipient alive/functional without eliminating accountability. The reprieve enables continued extraction because the recipient is more productive under partial relief than under maximum enforcement. The authority experiences this as a coordination mechanism with genuine benefits to itself (lower enforcement costs, higher future extraction capacity). The authority has arbitrage options: it can extend the reprieve or terminate it based on its interests.
constraint_indexing:constraint_classification(reprieve_as_temporary_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INSTITUTIONALIZED REPRIEVE SYSTEM (PITON) — Across civilizational scales, formalized reprieve systems (debt jubilees, amnesties, clemency programs, moratoriums) show high theater: the institutional rituals of granting and withdrawing reprieve persist long after their functional purpose has been absorbed into ongoing extraction. Many reprieve systems are maintained through inertia — the machinery of decision-making around reprieve (parole boards, debt forgiveness committees, amnesty procedures) creates the appearance of mercy while the underlying extraction mechanism continues. Theater ratio reflects that much of the reprieve system's activity is performative legitimation rather than actual functional reprieve.
constraint_indexing:constraint_classification(reprieve_as_temporary_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized agents (labor movements, debt forgiveness coalitions, criminal justice reformers) see reprieve as a temporary coordination mechanism with a sunset: systemic debt cancellation, abolition of certain enforcement mechanisms, or restorative justice systems can replace the reprieve cycle entirely. The reform perspective views the reprieve/re-obligation cycle as an interim structure that should be replaced by structural change. Sunset logic: once the underlying extractive system is reformed, reprieve becomes unnecessary. The coalition has constrained but organized exit options — they can push for systemic change rather than accepting reprieve as permanent.
constraint_indexing:constraint_classification(reprieve_as_temporary_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal analytical view, reprieve might be classified as reflecting an immutable structure: any system of obligation (debt, punishment, duty) creates a relational reality in which the obligated party cannot simply exit without consequence. The reprieve itself is seen as a consequence of this immutable structure — the obligation persists beneath the reprieve. However, the structural data contradicts a pure mountain classification. The theater ratio (0.68) and suppression mechanisms suggest that reprieve's immutability is contingent on institutional maintenance, not an inherent law. False summit: what appears as a natural law of obligation is actually a sustained extractive arrangement.
constraint_indexing:constraint_classification(reprieve_as_temporary_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reprieve_as_temporary_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reprieve_as_temporary_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reprieve_as_temporary_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reprieve_as_temporary_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reprieve_as_temporary_extraction, TR),
    TR >= 0.70.

:- end_tests(reprieve_as_temporary_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reprieve increases extraction relative to baseline enforcement in two mechanisms: (1) psychological dependency and gratitude debt exceed the relief magnitude, (2) the reprieve creates a cycle in which the recipient must continuously earn reprieve by demonstrating compliance, creating systematic over-compliance. The trajectory shows extractiveness rising from 0.35 (initial period where reprieve relief is genuine) to 0.58 (reprieve cycle is fully established and extraction through compliance demands dominates). Suppression (0.65): High. The reprieve creates multi-layered suppression: (1) formal barriers to exit (refusing reprieve triggers harsher enforcement), (2) psychological barriers (the recipient has internalized gratitude debt), (3) institutional barriers (the reprieve system creates legitimacy for continued control). Theater ratio (0.68): High. Formalized reprieve systems show substantial performative content: the rituals of hearing, deciding, announcing reprieve are maintained even when the underlying extraction mechanism continues unchanged. Many reprieve systems are maintained through institutional inertia rather than functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The most revealing perspectival gap appears between the authority's rope classification and the recipient's snare classification. Both agree that the reprieve temporarily suspends enforcement, but they diverge on whether this is coordination or extraction. The authority sees coordination because from its position, the reprieve solves a real problem: keeping the recipient functional enough to eventually meet obligations. The recipient sees extraction because the reprieve creates new control mechanisms (gratitude debt, compliance demonstration, the threat of withdrawal) that exceed the relief provided. The reform coalition bridges this gap by reframing reprieve as a temporary structure: they agree with the authority that some temporary relief is necessary for functional outcomes, but they see the reprieve as a scaffold that should be replaced by structural change (debt cancellation, restorative justice, rehabilitation systems) rather than maintained as a permanent cycle. The natural law perspective risks naturalizing what is actually a contingent institutional arrangement: the idea that reprieve must be temporary (and thus the underlying obligation must persist) is an institution-specific choice, not a law of obligation.
 *
 * DIRECTIONALITY LOGIC:
 *   The reprieve creates structural asymmetry in how different agents experience the same constraint. The authority grants reprieve and experiences it as genuine coordination — temporarily reducing enforcement pressure while maintaining the underlying obligation. This beneficiary perspective derives low d (around 0.15) because the authority has arbitrage options and captures value from the reprieve (improved productivity from the recipient, legitimacy gain from appearing merciful, reduced enforcement costs). The reprieve recipient derives high d (around 0.85) because they experience the reprieve as a control mechanism: accepting it locks them into compliance, refusing it triggers escalated punishment. The moderate victim (future obligation bearer navigating the reprieve period) derives mid-range d (around 0.55) because they experience genuine relief alongside increased future obligation. The institutional reprieve system derives low d (around 0.10) because it is the beneficiary: the system maintains itself through the reprieve cycle and experiences no extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that reprieve can be either coordination or extraction depending on whether the reprieve cycles (repeated deferrals with re-obligation) or is final (one-time relief that cancels or substantially reduces the obligation). The tangled_rope classification reflects the empirically common case: most reprieve systems are deferral mechanisms that create cycles, meaning the reprieve includes genuine coordination (temporary relief) alongside genuine extraction (the cycle prevents permanent resolution and creates secondary obligations through gratitude and compliance pressure). The snare perspective from the recipient's structural position reveals that the extraction component dominates the recipient's experience even when both components are empirically present. The mandatrophy is resolved not by choosing one type as 'correct' but by recognizing that reprieve systems are designed to appear as coordination (rope) while functioning as extraction (snare) — the tangled_rope classification is the true structural reality. Reform systems that convert reprieve to cancellation would shift the constraint toward genuine rope or scaffold, resolving the mandatrophy by eliminating the extractive component entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reprieve_as_mercy_vs_extraction,
    'Is the reprieve genuinely an act of mercy that coordinates conflicting interests (keeping the debtor functional while maintaining the debt), or is it an extraction mechanism that increases compliance through gratitude and psychological dependency?',
    'Longitudinal outcome tracking: compare recidivism/reoffending/redefault rates for reprieve recipients vs. those receiving no reprieve but with equal baseline severity. If reprieve recipients show lower long-term reoffending (suggesting genuine mercy enabled recovery), classify as coordination. If reprieve recipients show equivalent or higher rates (suggesting reprieve intensifies control mechanisms without reducing underlying obligation), classify as extraction.',
    'If mercy: reprieve is legitimately rope or scaffold from beneficiary perspective. If extraction: reprieve is snare from all non-authority perspectives, and the tangled_rope classification is the true empirical reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reprieve_as_mercy_vs_extraction, empirical, 'Whether reprieve functions as mercy or as control mechanism').

omega_variable(
    gratitude_debt_asymmetry,
    'Does the reprieve create a secondary obligation (gratitude debt, compliance debt) that exceeds the relief provided by the reprieve itself?',
    'Behavioral measurement: track compliance costs before reprieve, relief magnitude during reprieve, and secondary obligations/behavioral restrictions imposed during reprieve period. If secondary obligations > reprieve relief, the reprieve is net extractive.',
    'If secondary obligations are minimal: reprieve is genuine temporary relief. If secondary obligations exceed relief: reprieve is a mechanism to increase control and extract more sophisticated compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gratitude_debt_asymmetry, empirical, 'Whether reprieve creates offsetting secondary obligations').

omega_variable(
    permanent_debt_persistence,
    'Does the original obligation formally expire after the reprieve period, or does it persist indefinitely with reprieve as a deferral mechanism?',
    'Institutional documentation analysis: examine the formal terms of reprieve systems (debt moratoriums, amnesties, parole, sentence commutations). Determine whether reprieve includes obligation cancellation or only deferral.',
    'If obligations formally expire with reprieve: reprieve is temporary structural relief. If obligations persist: reprieve is extraction mechanism masquerading as mercy — the recipient is permanently indebted and the reprieve is an installment plan for obligation repayment through compliance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(permanent_debt_persistence, empirical, 'Whether reprieve is deferral or cancellation of underlying obligation').

omega_variable(
    reprieve_withdrawal_severity,
    'What is the severity escalation when reprieve is withdrawn? Is it equivalent to original enforcement, or is it heightened as punishment for reprieve violation?',
    'Comparative analysis of enforcement severity: track punishment levels for initial violation vs. reprieve violation. If reprieve violation receives disproportionate punishment, reprieve is an extraction mechanism (increased stakes). If equivalent, reprieve is genuine temporary relief.',
    'If heightened: reprieve functions as a control mechanism with escalation logic. If equivalent: reprieve is genuine temporary deferral without additional extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reprieve_withdrawal_severity, empirical, 'Severity of enforcement escalation upon reprieve withdrawal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reprieve_as_temporary_extraction, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reprieve_tr_t0, reprieve_as_temporary_extraction, theater_ratio, 0, 0.55).
narrative_ontology:measurement(reprieve_tr_t2, reprieve_as_temporary_extraction, theater_ratio, 2, 0.62).
narrative_ontology:measurement(reprieve_tr_t4, reprieve_as_temporary_extraction, theater_ratio, 4, 0.68).

% Extraction over time
narrative_ontology:measurement(reprieve_be_t0, reprieve_as_temporary_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(reprieve_be_t2, reprieve_as_temporary_extraction, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(reprieve_be_t4, reprieve_as_temporary_extraction, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reprieve_as_temporary_extraction, attachment_coordination).
narrative_ontology:affects_constraint(reprieve_as_temporary_extraction, debt_cycle_recapitalization).
narrative_ontology:affects_constraint(reprieve_as_temporary_extraction, institutional_mercy_theatre).

% DUAL FORMULATION NOTE:
% Reprieve-as-temporary-extraction is part of a constraint family with debt cycling and institutional mercy maintenance. The reprieve mechanism is upstream: it enables the debt cycle (partial relief creating opportunity for re-obligation) and sustains the mercy theatre (the ritual of reprieve granting). Decomposition reflects different epsilon values: reprieve mechanism itself (0.58, tangled rope), the debt recapitalization cycle it enables (higher epsilon, snare), and the institutional theatre maintenance (lower epsilon, piton).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
