% ============================================================================
% CONSTRAINT STORY: openbsd_netiquette_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_openbsd_netiquette_protocol, []).

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
 *   constraint_id: openbsd_netiquette_protocol
 *   human_readable: OpenBSD Mailing List Netiquette Protocol
 *   domain: technological/social
 *
 * SUMMARY:
 *   The OpenBSD mailing list netiquette protocol represents a hybrid
 *   coordination-extraction constraint operating at the intersection of
 *   technical communication standards and social gatekeeping. The constraint
 *   prioritizes content clarity, proper email formatting, and technical
 *   preparation over accessibility and user-friendliness. It exhibits a
 *   genuine coordination function (reducing noise, maintaining technical
 *   signal, creating searchable archives) while simultaneously extracting
 *   conformity costs from newcomers and casual participants. The constraint's
 *   power lies in its enforcement mechanism: violations are met with public
 *   correction, and implicitly-known standards are only revealed through
 *   breach. Over the interval measured (0-10 years), the theater ratio has
 *   increased (0.35 → 0.55), reflecting that the strict enforcement ritual
 *   increasingly serves norm-signaling and community identity functions as
 *   alternative communication tools (GitHub, modern mailing list software)
 *   absorb much of the actual technical collaboration. This divergence
 *   between performative enforcement and functional necessity marks the
 *   constraint's transition from pure coordination toward piton-like
 *   degradation.
 *
 * KEY AGENTS:
 *   - New Contributors: Primary victim (powerless/trapped) — encounter opaque standards, face public correction for violations, exit only through conformity or silence
 *   - Casual Participants: Secondary victim (powerless/constrained) — intimidated by strict norms, self-censor or avoid participation
 *   - Core OpenBSD Developers: Primary beneficiary (institutional/arbitrage) — experience constraint as low-overhead coordination, can leverage standards across projects
 *   - Active Community Members: Mixed agent (moderate/constrained) — benefit from high-signal list but risk correction for edge cases
 *   - Moderation Culture: Institutional enforcement system (institutional/arbitrage) — maintains public correction ritual; sees enforcement as norm signaling
 *   - Alternative Platforms: Organized replacement infrastructure (organized/mobile) — GitHub, modern tools, Discord providing parallel communication channels with different norms
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent technical-social choice as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openbsd_netiquette_protocol, 0.38).
domain_priors:suppression_score(openbsd_netiquette_protocol, 0.62).
domain_priors:theater_ratio(openbsd_netiquette_protocol, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openbsd_netiquette_protocol, extractiveness, 0.38).
narrative_ontology:constraint_metric(openbsd_netiquette_protocol, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(openbsd_netiquette_protocol, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openbsd_netiquette_protocol, tangled_rope).
narrative_ontology:human_readable(openbsd_netiquette_protocol, "OpenBSD Mailing List Netiquette Protocol").
narrative_ontology:topic_domain(openbsd_netiquette_protocol, "technological/social").

domain_priors:requires_active_enforcement(openbsd_netiquette_protocol).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openbsd_netiquette_protocol, core_openbsd_developers).
narrative_ontology:constraint_beneficiary(openbsd_netiquette_protocol, mailing_list_quality).
narrative_ontology:constraint_victim(openbsd_netiquette_protocol, new_contributors).
narrative_ontology:constraint_victim(openbsd_netiquette_protocol, casual_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEWCOMER (SNARE) — First-time contributors encounter an unwritten but strictly enforced code of communication standards. Posts failing to meet implicit expectations (proper threading, minimal quoting, correct subscript headers, code formatting) are met with public rebuke. No learning resources exist; the standards are only revealed through violation. Exit is available only through conformity or silence. d≈0.92, f(d)≈1.39, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACTIVE COMMUNITY MEMBER (TANGLED ROPE) — Moderate power agents (long-term contributors not on core team) benefit from the list's high signal-to-noise ratio and participate in technical problem-solving, but are also constrained by strict enforcement norms and risk public correction for minor infractions. They experience both the coordination function (efficient technical discussion) and extraction (asymmetric enforcement against edge cases). d≈0.68, f(d)≈1.05, σ=1.2 → χ≈0.48.
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CORE OPENBSD TEAM (ROPE) — Institutional actors (core developers, maintainers) experience the netiquette protocol as pure coordination: enforcing standards reduces their own overhead for parsing low-quality posts, ensures technically correct discussion, and protects the mailing list archives as a permanent technical reference. They can arbitrage the standard across venues (conferences, documentation, upstream projects). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MODERATION APPARATUS (PITON) — The enforcement ritual (public correction, list moderation) is partly performative. Many infractions go unremarked; enforcement is inconsistent. The public shaming serves theatrical purposes (norm signaling, community identity assertion) beyond actual problem-solving. theater_ratio=0.55 indicates moderate theatrical content. The apparatus persists because it still produces functional output (reduced noise) but is increasingly maintained by institutional inertia as alternative platforms (GitHub issues, Discord) absorb real technical collaboration. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.04.
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE COMMUNICATION INFRASTRUCTURE (SCAFFOLD) — Distributed organized agents (GitHub discussions, issue trackers, modern mailing list software with built-in formatting) are building parallel communication pathways that reduce dependence on strict netiquette enforcement. These alternatives provide coordination without asymmetric suppression. The scaffold has a sunset: as projects migrate to tools with better UX and format handling, the strict netiquette constraint's functional role diminishes. d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.25.
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT) — A naturalized interpretation frames strict netiquette as an immutable law of technical communication: 'high-signal lists require harsh filters; softer moderation inevitably degrades quality.' The structural data (ε=0.38, suppression=0.62) contradicts this. The constraint is contingent on (a) specific tooling choices (plain-text email), (b) specific enforcement culture (OpenBSD's curated identity), and (c) specific historical moment (pre-GitHub). The mountain classification is a false summit revealing how institutional practices become naturalized.
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openbsd_netiquette_protocol_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openbsd_netiquette_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openbsd_netiquette_protocol, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(openbsd_netiquette_protocol, TR),
    TR >= 0.70.

:- end_tests(openbsd_netiquette_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint does extract conformity costs from newcomers (time to learn implicit standards, risk of public correction), but the extraction is not as severe as pure snares (ε ≥ 0.46) because alternatives exist and experienced contributors can navigate the standards with minimal friction. The value has increased over time (0.22 → 0.38) as the implicit standards have accumulated and newer contributors face a steeper learning curve. Suppression (0.62): Moderate-high. Significant barriers to participation include: (1) opaque standards revealed only through violation, (2) public correction creating reputational risk, (3) high formatting/threading expectations unfamiliar to email clients outside technical communities, (4) archive searchability pressure creating bias against quick questions. However, suppression is not absolute — information exists in community documentation, and experienced users do answer questions from newcomers. Theater ratio (0.55): Moderate. The public correction ritual serves multiple functions: (1) genuine signal preservation (enforcing clarity improves list quality), but also (2) norm signaling (demonstrating community membership by participating in enforcement), (3) identity assertion (OpenBSD's 'curated' self-image). The theater ratio has increased from 0.35 to 0.55, reflecting that as alternative tools absorb functional collaboration, enforcement increasingly serves theatrical purposes.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests radically differently across perspectives. The core team sees efficient coordination (Rope) — the standards solve their problem of parsing low-quality posts. Active community members see mixed coordination and extraction (Tangled Rope) — the standards benefit them but they also enforce them asymmetrically. Newcomers see pure extraction (Snare) — implicit rules, public shaming, no exit except through conformity. The moderation apparatus sees degrading function masked by persistent ritual (Piton) — theater_ratio=0.55 reflects this balance. Alternative platforms see a solvable temporal problem (Scaffold) — GitHub's built-in formatting and threading eliminate most reasons for strict netiquette. The analytical observer risks naturalizing this as immutable law (false mountain) — framing harsh standards as necessary for technical communication, when the standards are actually contingent on email-based tools and a specific enforcement culture.
 *
 * DIRECTIONALITY LOGIC:
 *   Core OpenBSD developers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries. The constraint solves their problem efficiently. New contributors: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction. They cannot exit the list without conforming to opaque standards. Active community members: Mixed victim/beneficiary + constrained → d≈0.68, f(d)≈1.05. They experience both the signal-preservation benefit and the enforcement burden. Moderation apparatus: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Piton classification comes from theater_ratio gate, not from high chi. Alternative platforms: Organized + mobile → d≈0.45, f(d)≈0.45. Low effective extraction because these actors have genuine alternatives and exit capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely a Tangled Rope at the system level: it provides a real coordination function (reducing noise, preserving technical signal) while simultaneously extracting conformity costs asymmetrically from those with less power (newcomers, casual participants). The false mountain summit (naturalizing strict standards as inherent to technical communication) is revealed by the structural data and perspectival analysis. The constraint can be reformed without losing coordination benefits — this is demonstrated by the scaffold perspective: GitHub issues and modern mailing list software provide coordination without asymmetric suppression. The theater ratio of 0.55 indicates that roughly half the enforcement activity is performative norm signaling rather than functional signal preservation. As alternative tools mature, the theater ratio will rise and the extractiveness will decline, transitioning the constraint toward piton-like degradation. The mandatrophy is fully resolved: Tangled Rope at the system level, with false mountain threats from naturalization, and a real sunset path via alternative communication infrastructure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_consistency_criterion,
    'Is enforcement of netiquette standards applied consistently across contributors, or does it vary by social status (core developers exempt, newcomers harshly corrected)?',
    'Longitudinal analysis of mailing list corrections: track enforcement rate by contributor tenure, power level, and infraction type. Compare correction frequency for same violation type across different contributor classes.',
    'If consistently enforced: constraint is genuine coordination mechanism (Rope from core perspective). If inconsistent: constraint is selective extraction mechanism (Snare/Tangled Rope from disenfranchised perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_consistency_criterion, empirical, 'Whether netiquette enforcement is applied consistently or varies by social status').

omega_variable(
    quality_degradation_counterfactual,
    'Would technical discussion quality on OpenBSD lists noticeably degrade if netiquette enforcement were relaxed, or is the high quality primarily driven by contributor self-selection and technical expertise?',
    'Comparative analysis of mailing lists with similar expert populations but different moderation norms (e.g., Python-dev vs OpenBSD-tech). Measure signal-to-noise ratio, technical accuracy, and actionability of responses across platforms.',
    'If quality degrades significantly: netiquette enforcement is functionally necessary (Rope justified). If quality remains high: netiquette is primarily performative norm-signaling (Piton/Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_degradation_counterfactual, empirical, 'Whether relaxed netiquette enforcement would degrade technical discussion quality').

omega_variable(
    barrier_to_contribution_magnitude,
    'How many potential contributors are excluded by strict netiquette standards vs how many are actually filtered out by the coordination benefit (removing low-signal posts)?',
    'Survey data from OpenBSD community members (especially those who contributed once then stopped). Track archive of bounced posts and analysis of reasons for non-response to newcomer posts.',
    'If barrier is large relative to coordination benefit: constraint is extractive (Snare). If barrier is small: constraint is legitimate coordination mechanism (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barrier_to_contribution_magnitude, empirical, 'Relative magnitude of newcomer exclusion vs signal-to-noise filtering benefit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openbsd_netiquette_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(obsd_tr_t0, openbsd_netiquette_protocol, theater_ratio, 0, 0.35).
narrative_ontology:measurement(obsd_tr_t5, openbsd_netiquette_protocol, theater_ratio, 5, 0.45).
narrative_ontology:measurement(obsd_tr_t10, openbsd_netiquette_protocol, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(obsd_be_t0, openbsd_netiquette_protocol, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(obsd_be_t5, openbsd_netiquette_protocol, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(obsd_be_t10, openbsd_netiquette_protocol, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openbsd_netiquette_protocol, information_standard).
narrative_ontology:affects_constraint(openbsd_netiquette_protocol, open_source_gatekeeping_norms).
narrative_ontology:affects_constraint(openbsd_netiquette_protocol, email_based_collaboration_limits).

% DUAL FORMULATION NOTE:
% This constraint represents the intersection of tool affordances (email limitations) and enforcement culture (OpenBSD's curated identity). The constraint family includes separate stories for email-based collaboration limits (structural feature of the tool) and open-source gatekeeping norms (strategic social choice). The netiquette protocol is downstream of both: it emerges from the interaction between email constraints and gatekeeping culture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(openbsd_netiquette_protocol, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
