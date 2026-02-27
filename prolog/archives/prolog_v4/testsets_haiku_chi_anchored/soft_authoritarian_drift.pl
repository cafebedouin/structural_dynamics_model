% ============================================================================
% CONSTRAINT STORY: soft_authoritarian_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_soft_authoritarian_drift, []).

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
 *   constraint_id: soft_authoritarian_drift
 *   human_readable: The Incremental Compliance Web
 *   domain: political/social/technological
 *
 * SUMMARY:
 *   Soft authoritarian drift describes a structural phenomenon where
 *   democratic or open systems incrementally adopt restrictive measures —
 *   initially justified as temporary safety, public health, or efficiency
 *   responses — that accumulate into a comprehensive compliance web. Each
 *   individual measure appears minor (a content-moderation policy, a tracking
 *   requirement, a behavioral audit) and often carries genuine coordination
 *   benefits (reducing harmful content, preventing fraud, detecting threats).
 *   Over time, however, the cumulative effect is that individual autonomy
 *   becomes contingent on state or platform approval, and exit from the
 *   system becomes economically and socially impossible. The constraint
 *   exhibits different classifications from different structural
 *   perspectives: a powerless citizen sees a Snare with no exit; the state
 *   security apparatus sees a Rope coordinating public order; civil liberties
 *   advocates see a temporary Scaffold problem solvable through legal and
 *   technological reform; bureaucratic administrators see a degraded Piton
 *   ritual; moderate-power platform users see a Tangled Rope mixing
 *   coordination and extraction; the global capital elite see a Tangled Rope
 *   they can partially escape; and the analytical observer sees a genuinely
 *   hybrid mechanism where some surveillance serves coordination goals and
 *   some serves asymmetric extraction. The theater_ratio (0.81) reflects that
 *   much compliance machinery is performative: automated flags and audit
 *   trails create the appearance of systematic threat-detection while actual
 *   human review and consequential action remain concentrated and selective.
 *   The constraint's extractiveness (0.58) has increased dramatically over
 *   the measurement interval from 0.28, indicating that while coordination
 *   benefits (public health coordination, fraud reduction) remain roughly
 *   constant, extraction mechanisms (behavioral nudging, data harvesting,
 *   selective enforcement) have proliferated.
 *
 * KEY AGENTS:
 *   - Individual Citizens: Primary victim (powerless/trapped) — subject to graduated surveillance, compliance nudging, and social exclusion for non-compliance; cannot exit digital infrastructure without accepting total isolation
 *   - Platform User Community: Secondary victim (moderate/constrained) — benefit from platform coordination but subject to algorithmic ranking suppression, behavioral prediction, and content removal
 *   - State Security Apparatus: Primary beneficiary (institutional/arbitrage) — gains public order coordination, threat-detection capability, and population-scale behavioral data; can arbitrage regulatory focus
 *   - Platform Moderation Staff: Secondary beneficiary (institutional/constrained) — benefit from growth and legitimacy of compliance infrastructure, though often alienated from its actual purpose
 *   - Civil Liberties Coalition: Organized agent (organized/constrained) — see the drift as a temporary institutional failure with viable legal and technical exit pathways (encryption, decentralized platforms, constitutional reform)
 *   - Global Capital Elite: Powerful agent (powerful/mobile) — partially escape the compliance web through privacy services and alternative infrastructure; benefit from population-scale surveillance data while retaining exit optionality
 *   - Compliance Theater System: Institutional actor (institutional/arbitrage) — maintains degraded ritual (audit trails, automated flags) through bureaucratic inertia; sees its own machinery as performative but lacks incentive to replace it
 *   - Privacy Commons: Abstract victim (powerless/trapped) — collective right to privacy erosion; cannot organize or exit; bears full cost of surveillance asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(soft_authoritarian_drift, 0.58).
domain_priors:suppression_score(soft_authoritarian_drift, 0.68).
domain_priors:theater_ratio(soft_authoritarian_drift, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(soft_authoritarian_drift, extractiveness, 0.58).
narrative_ontology:constraint_metric(soft_authoritarian_drift, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(soft_authoritarian_drift, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(soft_authoritarian_drift, tangled_rope).
narrative_ontology:human_readable(soft_authoritarian_drift, "The Incremental Compliance Web").
narrative_ontology:topic_domain(soft_authoritarian_drift, "political/social/technological").

domain_priors:requires_active_enforcement(soft_authoritarian_drift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(soft_authoritarian_drift, state_security_apparatus).
narrative_ontology:constraint_beneficiary(soft_authoritarian_drift, platform_moderation_staff).
narrative_ontology:constraint_beneficiary(soft_authoritarian_drift, bureaucratic_administrators).
narrative_ontology:constraint_victim(soft_authoritarian_drift, individual_autonomy).
narrative_ontology:constraint_victim(soft_authoritarian_drift, speech_margin).
narrative_ontology:constraint_victim(soft_authoritarian_drift, privacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MONITORED CITIZEN (SNARE) — Powerless individual subject to graduated surveillance and compliance rules with no meaningful exit. Cannot opt out of digital infrastructure, mobility tracking, or content moderation without accepting total social exclusion. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80. Classification: Snare.
constraint_indexing:constraint_classification(soft_authoritarian_drift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PLATFORM USER COMMUNITY (TANGLED ROPE) — Moderate power but constrained exit. Benefits from platform coordination (connection, commerce, information access) while bearing extraction costs (behavioral nudging, data harvesting, algorithmic ranking suppression). d≈0.62, f(d)≈0.88, σ=1.0 → χ≈0.51. Classification: Tangled Rope.
constraint_indexing:constraint_classification(soft_authoritarian_drift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE SECURITY APPARATUS (ROPE) — Institutional beneficiary with arbitrage optionality (can migrate regulatory focus, deploy selective enforcement). Experiences constraint as coordination mechanism for threat-detection and order-maintenance. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary; negative effective extraction indicates pure benefit. Classification: Rope.
constraint_indexing:constraint_classification(soft_authoritarian_drift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CIVIL LIBERTIES COALITION (SCAFFOLD) — Organized agents perceive soft authoritarian drift as a temporary institutional failure with a sunset: legal challenges, regulatory reform, and alternative decentralized platforms (mesh networks, end-to-end encrypted infrastructure) represent real exit pathways. d≈0.48, f(d)≈0.63, σ=1.0 → χ≈0.36. Sunset rationale: encryption mandates, privacy legislation, and platform alternatives can reverse the compliance web over 15-25 years.
constraint_indexing:constraint_classification(soft_authoritarian_drift, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPLIANCE THEATER SYSTEM (PITON) — Degraded institutional machinery where performative compliance (audit trails, checkboxes, automated flags) has replaced genuine security or safety function. theater_ratio=0.81 satisfies piton gate (≥0.70). The system persists through bureaucratic inertia despite demonstrable inefficacy (false positives exceed actionable signals). Administrators see their own process as degraded but lack institutional incentive to replace it.
constraint_indexing:constraint_classification(soft_authoritarian_drift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GLOBAL CAPITAL ELITE (TANGLED ROPE) — Powerful actors with genuine mobile exit (can relocate, use alternative infrastructure, hire privacy services). The drift is a tangled rope from their perspective: they benefit from social order and consumer surveillance (data for monetization) while retaining exit optionality that lower-power agents lack. d≈0.35, f(d)≈0.36, σ=1.2 → χ≈0.25. Lower effective extraction due to mobile exit and structural coordination benefit.
constraint_indexing:constraint_classification(soft_authoritarian_drift, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Civilizational view: soft authoritarian drift combines genuine coordination gains (public health surveillance reducing pandemic risk, fraud detection protecting commerce) with asymmetric extraction (state power accumulation, inequality of surveillance access). Not a mountain (natural law) because the degree of drift is policy-contingent; not a rope (pure coordination) because suppression and theater are measurable. d≈0.58, f(d)≈0.79, σ=1.0 → χ≈0.46.
constraint_indexing:constraint_classification(soft_authoritarian_drift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(soft_authoritarian_drift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(soft_authoritarian_drift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(soft_authoritarian_drift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(soft_authoritarian_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(soft_authoritarian_drift, TR),
    TR >= 0.70.

:- end_tests(soft_authoritarian_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant value from lower-power agents through behavioral steering, data harvesting, and selective enforcement. However, extractiveness is not maximal (≥0.70) because genuine coordination benefits exist (fraud prevention reduces costs for all users, pandemic monitoring provided real public health value, and platform moderation does prevent some genuine harms). The trajectory from 0.28 to 0.58 indicates that extraction mechanisms have been layered onto coordination functions over time — the drift is real and accelerating. Suppression (0.68): High. Multiple barriers prevent exit or non-compliance: network-effect lock-in makes switching platforms prohibitively costly, algorithmic ranking suppression makes non-compliant voices economically invisible, and social/professional consequences (job loss, reputation damage, platform bans) create behavioral coercion. Suppression is not total (≥1.0) because some actors (capital elite, organized civil libertarians) have partial exit capacity. Theater ratio (0.81): Very high. Compliance machinery is heavily performative. Automated content-flagging systems generate false-positive rates exceeding true threats by 10-100x; audit trails create appearance of systematic process while actual consequential decisions remain concentrated in human hands; terms-of-service enforcement is selective and opaque. The high theater ratio indicates drift toward Piton-type degradation — the machinery persists through institutional inertia rather than efficacy. The trajectory from 0.42 to 0.81 shows that performative elements have proliferated: behavioral scoring, algorithmic transparency reports, and compliance certifications now dominate governance rhetoric while actual enforcement power remains centralized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the maximum possible perspectival divergence. The powerless citizen perceives a Snare (no exit, pure extraction, maximum d≈0.92). The state apparatus perceives a Rope (coordination benefit, arbitrage exit, d≈0.08). The civil liberties coalition perceives a Scaffold (temporary problem with legal and technical sunset pathways, organized escape routes). The compliance theater sees itself as a degraded Piton (performative machinery maintained by inertia). The moderate platform user perceives a Tangled Rope (mixed coordination and extraction, constrained exit). The capital elite perceive a Tangled Rope with partial escape (genuine mobile exit, both coordination and extraction benefits). The analytical observer perceives a Tangled Rope at civilizational scale (hybrid mechanism with both genuine safety coordination and asymmetric extraction). The perspectival gap arises because the constraint operates asymmetrically: those with exit capacity (capital, institutions, organized groups) experience the coordination benefits and can arbitrage the extraction; those without exit (citizens, privacy commons) experience the full force of both extraction and coercion. No single type is 'correct' — the presheaf of perspectives IS the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual citizens: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction target. Platform user community: Victim + constrained → d≈0.62, f(d)≈0.88. High extraction target but with some coordination benefit. State security apparatus: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary with low structural extraction (negative chi indicates benefit). Civil liberties coalition: Organized + constrained → d≈0.48, f(d)≈0.63. Moderate extraction pressure but organized capacity to resist. Capital elite: Beneficiary + mobile → d≈0.35, f(d)≈0.36. Partial beneficiary with genuine escape routes (mobile exit). Analytical observer: analytical → d≈0.58, f(d)≈0.79. Neither pure beneficiary nor pure victim; sees balanced hybrid. Compliance theater: Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Structured as beneficiary, though the institution perceives itself as degraded (piton classification from theater gate, not directionality).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RISK: The constraint's high theater ratio (0.81) and high extractiveness (0.58) create a false-positive risk — the system could be mislabeled as a Snare everywhere when it is actually a legitimate hybrid mechanism (Tangled Rope) with genuine coordination benefits layered with extraction. The mandatrophy is resolved by indexical analysis: from the powerless citizen perspective (d≈0.92), it IS a Snare — they experience only extraction and coercion, no coordination benefit. From the state apparatus perspective (d≈0.08), it IS a Rope — they experience only coordination benefit. The analytical observer perspective prevents oversimplification: the system contains BOTH real safety-coordination gains (pandemic response, fraud detection, harm reduction) AND real extraction mechanisms (behavioral steering, asymmetric data access, selective enforcement). The high theater ratio shows that performative elements have proliferated, but the underlying mechanism remains hybrid. The mandatrophy is resolved by refusing to collapse the perspectival structure into a single type: soft authoritarian drift is a Tangled Rope viewed from most positions, a Snare from those without exit, a Rope from beneficiary institutions, and a Scaffold from those who perceive viable exit pathways. The falsity would be claiming it is ONLY a Snare (erasing coordination benefits) or ONLY a Rope (erasing extraction). The correct classification is Tangled Rope from the baseline analytical perspective, with Snare, Scaffold, and Piton as valid perspectival readings from structurally different positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_reduction_threshold,
    'At what point does genuine public safety benefit (pandemic monitoring, fraud detection) become a threshold masking extractive surveillance?',
    'Comparative analysis: measure public health/security outcomes vs. behavioral coercion metrics; identify inflection point where surveillance expansion ceases to improve outcomes but extraction persists',
    'If threshold is near current state: drift is primarily extractive (Snare from most perspectives). If threshold is far higher: drift legitimately serves coordination (Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_reduction_threshold, empirical, 'Safety benefit threshold masking surveillance extraction').

omega_variable(
    voluntary_consent_fiction,
    'Can a system with network-effect lock-in (everyone on platform X, impossible to exclude yourself) ever achieve genuine ''voluntary'' compliance with its terms?',
    'Legal and economic analysis of platform switching costs; survey data on perceived choice; comparison to historical consent-under-duress standards',
    'If consent is illusory: classification shifts toward Snare even for moderate-power agents (exit becomes ''trapped'' not ''constrained''). If some genuine choice exists: Tangled Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_consent_fiction, conceptual, 'Whether network lock-in falsifies consent to compliance rules').

omega_variable(
    decentralization_viability,
    'Do decentralized alternatives (mesh networks, end-to-end encrypted platforms, federated systems) offer real escape from the compliance web or merely relocate it?',
    'Technical analysis of censorship resistance; historical tracking of decentralized platform adoption; study of whether decentralized systems develop their own compliance constraints',
    'If viable: scaffold sunset is real (open-science-style exit pathways). If decentralized systems replicate the compliance web: drift is structural to infrastructure itself (approaches Mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_viability, empirical, 'Whether decentralized platforms escape compliance constraints').

omega_variable(
    institutional_reversibility,
    'Once surveillance infrastructure and compliance bureaucracy are built, can they be meaningfully dismantled, or do they create irreversible asymmetric power?',
    'Historical case studies (Stasi dissolution, NSA reform efforts, GDPR effectiveness); measurement of whether surveillance capacity, once built, has ever been truly reduced rather than merely concealed',
    'If irreversible: drift approaches Mountain (constraint embedded in infrastructure). If reversible: scaffold sunset and legal-challenge pathways remain viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_reversibility, empirical, 'Whether surveillance infrastructure can be dismantled').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(soft_authoritarian_drift, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_auth_tr_t0, soft_authoritarian_drift, theater_ratio, 0, 0.42).
narrative_ontology:measurement(soft_auth_tr_t7, soft_authoritarian_drift, theater_ratio, 7, 0.62).
narrative_ontology:measurement(soft_auth_tr_t15, soft_authoritarian_drift, theater_ratio, 15, 0.81).

% Extraction over time
narrative_ontology:measurement(soft_auth_be_t0, soft_authoritarian_drift, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(soft_auth_be_t7, soft_authoritarian_drift, base_extractiveness, 7, 0.44).
narrative_ontology:measurement(soft_auth_be_t15, soft_authoritarian_drift, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(soft_authoritarian_drift, enforcement_mechanism).
narrative_ontology:affects_constraint(soft_authoritarian_drift, platform_moderation_asymmetry).
narrative_ontology:affects_constraint(soft_authoritarian_drift, state_surveillance_infrastructure).
narrative_ontology:affects_constraint(soft_authoritarian_drift, algorithmic_rank_suppression).

% DUAL FORMULATION NOTE:
% Soft authoritarian drift is a macroscopic constraint composed of three structurally distinct microconstraints: platform moderation asymmetry (ε≈0.45, enforcement-type coordination), state surveillance infrastructure (ε≈0.52, enforcement-type extraction), and algorithmic rank suppression (ε≈0.38, information-standard coordination mixed with extraction). The soft_authoritarian_drift story models the integrated effect; its upstream components have their own ε values and network dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(soft_authoritarian_drift, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
