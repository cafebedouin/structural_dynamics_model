% ============================================================================
% CONSTRAINT STORY: sadhu_integrity_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sadhu_integrity_protocol, []).

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
 *   constraint_id: sadhu_integrity_protocol
 *   human_readable: The Integrity Requirement (Sadhu's Sugar)
 *   domain: social/ethical
 *
 * SUMMARY:
 *   The integrity requirement mandates that an advisor must embody their own
 *   counsel before delivering it to others. This constraint creates a
 *   structural asymmetry: seekers depend on advisors for guidance but cannot
 *   independently verify whether the advisor actually follows the advice they
 *   dispense. The constraint functions simultaneously as a coordination
 *   mechanism (establishing trust and credibility) and as an extraction
 *   device (forcing seekers to accept advising on terms set by advisor
 *   incentives and capabilities). The trap emerges when seekers have no
 *   mechanism to distinguish sincere counsel from performative advice, yet
 *   the social/institutional framework forces them to treat the integrity
 *   requirement as a guarantee. The extractiveness has increased over time
 *   (0.35 → 0.58) as institutional enforcement has weakened and advisory
 *   professions have proliferated, increasing the ratio of unverifiable
 *   claims to verifiable outcomes. Theater has remained relatively low (0.32
 *   → 0.45) because the requirement itself is deeply internalized and widely
 *   accepted as normatively correct, even though its functional enforcement
 *   is theatrical.
 *
 * KEY AGENTS:
 *   - Advice Seekers: Primary victims (powerless/trapped) — depend on advisor integrity but have no exit or verification mechanism
 *   - Trusted Advisors: Primary beneficiaries (institutional/arbitrage) — gain credibility and market value from the integrity requirement; can operate across multiple seeker relationships
 *   - Conflicted Advisors: Secondary actors (moderate/constrained) — benefit from institutional authority but face disciplinary risk; cannot exit without abandoning role
 *   - Professional Ethics Apparatus: Institutional actor (institutional/arbitrage) — maintains licensing and disciplinary authority; exercises weak actual enforcement
 *   - Accountability Coalitions: Organized challengers (organized/mobile) — building peer-based verification alternatives that reduce reliance on individual advisor virtue
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks seeing the requirement as an immutable principle rather than a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sadhu_integrity_protocol, 0.58).
domain_priors:suppression_score(sadhu_integrity_protocol, 0.62).
domain_priors:theater_ratio(sadhu_integrity_protocol, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sadhu_integrity_protocol, extractiveness, 0.58).
narrative_ontology:constraint_metric(sadhu_integrity_protocol, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sadhu_integrity_protocol, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sadhu_integrity_protocol, snare).
narrative_ontology:human_readable(sadhu_integrity_protocol, "The Integrity Requirement (Sadhu's Sugar)").
narrative_ontology:topic_domain(sadhu_integrity_protocol, "social/ethical").

domain_priors:requires_active_enforcement(sadhu_integrity_protocol).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sadhu_integrity_protocol, advice_dispensers).
narrative_ontology:constraint_victim(sadhu_integrity_protocol, advice_seekers).
narrative_ontology:constraint_victim(sadhu_integrity_protocol, social_trust_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ADVICE SEEKER (SNARE) — Trapped in asymmetric information. The seeker cannot distinguish sincere counsel from performative advice; they have no exit option without abandoning the relationship or incurring epistemic/social cost. The constraint forces them to accept advising on terms set entirely by the advisor's structural incentives. Maximum extraction: the seeker bears the full cost of advisor hypocrisy while gaining no verification mechanism.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE TRUSTED ADVISOR (ROPE) — Sees the integrity requirement as coordination. The advisor benefits from the constraint by establishing social credibility; the expectation that they embody their counsel is precisely what makes their advice valuable and sought-after. They experience the constraint as a enabling mechanism rather than extraction. Exit via arbitrage: advisors can engage with multiple seekers, building networks of trust.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE CONFLICTED ADVISOR (TANGLED ROPE) — Moderate power but constrained exit. Advisors who operate within institutional frameworks (therapists, coaches, mentors) benefit from the integrity requirement (it establishes their authority) but also bear real costs if they fail to embody their own counsel. They face disciplinary action, loss of licensure, reputation damage. The constraint is both enabling (gives them market value) and extractive (forces behavioral conformity under threat of sanction). They cannot easily exit without abandoning their role.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INSTITUTIONAL ETHICS APPARATUS (PITON) — Professional codes of ethics enforce the integrity requirement through licensing, certification, and disciplinary boards. But the apparatus has largely become theatrical: ethics boards process cases slowly, rarely enforce against high-status violators, and rely on victim complaints rather than proactive monitoring. The institutional ritual persists as a credibility signal while actual enforcement capacity has atrophied. Theater ratio is low (0.45) because the requirement itself is genuine and widely internalized, not performative — but the enforcement machinery is degraded.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ACCOUNTABILITY COALITION (SCAFFOLD) — Organized seekers (support groups, community networks, peer circles) are building parallel verification mechanisms that bypass reliance on the individual advisor's internalized integrity. Peer accountability, group verification of advice quality, and distributed mentorship are creating alternative pathways where the integrity requirement is backed by structural redundancy rather than individual virtue. Exit is mobile: seekers can switch to community-based or peer-led models. Theater ratio is lower here because these mechanisms rely less on performative trust and more on observable outcomes and peer corroboration.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From a civilizational view, the integrity requirement appears as an immutable logical principle: advice cannot exceed the advisor's demonstrated capacity to follow it; therefore, integrity is inherent to all genuine counsel. This perspective risks naturalizing what is actually a contingent institutional requirement. The constraint functions as an extractive mechanism precisely because it masquerades as a natural law rather than a negotiable structural feature.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sadhu_integrity_protocol_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sadhu_integrity_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sadhu_integrity_protocol, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sadhu_integrity_protocol, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sadhu_integrity_protocol, TR),
    TR >= 0.70.

:- end_tests(sadhu_integrity_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The integrity requirement extracts from seekers by forcing them to accept advising on asymmetric information. Seekers cannot verify advisor compliance, yet they are expected to trust and follow counsel. The extraction is not total because some advisors do genuinely embody their counsel, and some seekers can exit (mobile exit exists but is costly). The trajectory from 0.35 to 0.58 reflects degradation of institutional enforcement: as ethics boards have weakened, the actual verification mechanism has atrophied, pushing the constraint toward pure extraction. Suppression (0.62): Moderate-high. Multiple barriers prevent seekers from challenging the integrity requirement or organizing alternative arrangements: social norms require trust in advisors; questioning an advisor's integrity damages the relationship; alternative verification mechanisms (peer accountability, community vetting) are less normalized than institutional authority; exit requires abandoning the advice relationship entirely or incurring social/epistemic cost. Theater (0.45): Moderate. The requirement itself is genuinely internalized and widely accepted as normatively correct — it is not primarily theatrical. But the enforcement apparatus is partially theatrical: ethics boards process complaints slowly; high-status advisors face lighter consequences; institutions rely on victim complaints rather than proactive monitoring. The relatively low theater ratio (compared to the verification bottleneck example) reflects that the requirement has strong cultural and psychological embedding, not just institutional performativity.
 *
 * PERSPECTIVAL GAP:
 *   The advisor sees coordination (rope) — the integrity requirement is what makes their counsel valuable and sought-after. They experience it as enabling their social role. The seeker sees extraction (snare) — they are forced to accept advising on terms they cannot verify. They have no exit without abandoning the relationship. The conflicted advisor (therapist, coach, mentor) sees mixed coordination and extraction (tangled rope) — the requirement is what establishes their authority, but it also subjects them to disciplinary risk and enforces behavioral conformity. The institutional ethics apparatus sees its own degraded ritual (piton) — the enforcement machinery persists through credibility value (licensing signals trust) but actual enforcement is weak. The accountability coalition sees a temporary institutional lock-in (scaffold) — peer-based verification and community accountability are building alternatives with lower extractiveness and higher transparency. The civilizational observer risks seeing a natural law (false mountain) — integrity seems inherent to all genuine counsel, when in fact it is a contingent institutional requirement that depends on unequal verification capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from structural position. Advice seekers are trapped (no exit without abandoning the relationship) and victims (they bear the cost of advisor hypocrisy they cannot detect). They derive d ≈ 0.90-0.95 (maximum target). Trusted advisors are institutional actors with arbitrage options (they can move between seekers, build networks, switch domains). They derive d ≈ 0.10-0.20 (beneficiary with mobility). Conflicted advisors have constrained exit (they cannot abandon their professional role without loss) and face both victim and beneficiary dynamics. They derive d ≈ 0.50-0.55 (symmetric). The powerless seeker and institutional beneficiary perspectives define the perspectival gap. The conflicted advisor perspective fills the middle ground, showing that moderate power with constrained exit produces tangled rope (mixed coordination-extraction). The sanctuary of the institutional advisor is their arbitrage option: they can always find new seekers, can build networks across communities, and can move between advisory domains. This mobility insulates them from the maximum extraction that trapped seekers experience.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing the genuine coordination function (advisors need credibility; seekers benefit from trusting counsel) from the extractive mechanism (seekers cannot verify compliance; advisors face minimal enforcement; trapped seekers have no exit). The snare classification does not deny the coordination value of integrity — it identifies the structural asymmetry that converts coordination into extraction. The rope perspective (advisor view) captures the genuine coordination. The scaffold perspective (accountability coalition view) shows how alternatives can reduce extractiveness without eliminating the need for some form of trust. The false mountain perspective (civilizational observer) is instructive: it shows how the integrity requirement is often naturalized as an inherent principle of advice rather than recognized as a contingent institutional arrangement. The piton perspective documents the degradation of enforcement mechanisms over time. The tangled rope perspective captures advisors who genuinely embody their counsel but face disciplinary sanction — they experience both the enabling and constraining functions of the requirement. The mandate against treating coordination and extraction as synonymous is honored by showing that all six types are legitimate readings from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integrity_verification_gap,
    'Can advice-seekers accurately verify whether an advisor embodies their own counsel, or is the verification itself based on narrative performance?',
    'Longitudinal tracking of advisor behavior vs. seeker perception; analysis of cases where seekers discovered advisor hypocrisy; comparison of perceived vs. documented advisor compliance with their own advice',
    'If verification is accurate: the integrity requirement functions as genuine coordination (rope). If verification is performative: it functions as pure extraction mechanism (snare) — seekers believe they have assurance but have only theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(integrity_verification_gap, empirical, 'Whether seekers can verify advisor integrity or only perceive it').

omega_variable(
    integrity_cost_distribution,
    'Do advisors who embody their own counsel experience genuine career/reputation benefits that outweigh the behavioral costs, or do they incur net losses that are compensated only by social approval?',
    'Economic analysis of advisor earnings and career advancement relative to integrity compliance; surveys of advisor motivation (intrinsic vs. extrinsic); comparison of high-integrity and low-integrity advisors'' career trajectories',
    'If benefits exceed costs: integrity requirement is genuine coordination (rope). If costs exceed benefits and are sustained only by social pressure: the requirement is extractive (snare/tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integrity_cost_distribution, empirical, 'Whether embodying counsel provides net benefit to advisors').

omega_variable(
    hypocrisy_detection_failure_rate,
    'What fraction of advisor hypocrisy goes undetected by seekers, and for how long?',
    'Case analysis of exposed advisor hypocrisy; seeker interviews about when they discovered violations; comparison with advisor self-reported violations; statistical analysis of professional misconduct reports vs. estimated actual violations',
    'If high failure rate: the constraint is functionally extractive because verification mechanism is broken. If low failure rate: the constraint approaches genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypocrisy_detection_failure_rate, empirical, 'What proportion of advisor hypocrisy remains undetected').

omega_variable(
    institutional_enforcement_capacity,
    'Do ethics boards and professional licensing bodies actually enforce the integrity requirement, or do they primarily process complaints without meaningful consequences?',
    'Analysis of disciplinary action rates; correlation between reported violations and license revocation; case-study examination of high-profile advisor misconduct outcomes; survey of enforcement agency staffing and caseload',
    'If enforcement is real: suppression is structural and the snare classification holds. If enforcement is theatrical: piton perspective is dominant and the constraint is maintained by inertia rather than functional suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_enforcement_capacity, empirical, 'Whether professional enforcement mechanisms actually function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sadhu_integrity_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sadhu_tr_t0, sadhu_integrity_protocol, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sadhu_tr_t5, sadhu_integrity_protocol, theater_ratio, 5, 0.38).
narrative_ontology:measurement(sadhu_tr_t10, sadhu_integrity_protocol, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(sadhu_be_t0, sadhu_integrity_protocol, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sadhu_be_t5, sadhu_integrity_protocol, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sadhu_be_t10, sadhu_integrity_protocol, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sadhu_integrity_protocol, enforcement_mechanism).
narrative_ontology:affects_constraint(sadhu_integrity_protocol, expert_authority_asymmetry).
narrative_ontology:affects_constraint(sadhu_integrity_protocol, therapeutic_relationship_power_differential).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
