% ============================================================================
% CONSTRAINT STORY: reader_informed_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reader_informed_consent, []).

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
 *   constraint_id: reader_informed_consent
 *   human_readable: Reader Informed Consent in Information Systems
 *   domain: information_systems/cognitive_autonomy/epistemic_integrity
 *
 * SUMMARY:
 *   Reader informed consent in digital information systems operates at the
 *   intersection of epistemic autonomy and platform economics. Platforms
 *   provide genuine coordination benefits: access to diverse information
 *   sources, connection across geographic and social boundaries, and
 *   discovery mechanisms that would be impossible at pre-digital scale.
 *   Simultaneously, platforms extract value through attention capture,
 *   behavioral data harvesting, and algorithmic manipulation that serve
 *   advertising networks and platform operators. The constraint is tangled
 *   rope at its core: the coordination function is real (information access
 *   is genuinely coordinated), but the extraction asymmetry is also real
 *   (readers' behavioral autonomy is compromised for advertiser benefit). The
 *   consent apparatus (privacy policies, cookie banners, disclosure
 *   mechanisms) is substantially theater: designed to appear as if readers
 *   exercise informed choice while the underlying system operates with
 *   informational and cognitive asymmetries that make true consent impossible
 *   at scale. The trajectory over the measurement interval (years 0-6) shows
 *   theater ratio rising from 0.42 to 0.68 and extractiveness rising from
 *   0.35 to 0.58 as regulatory responses (GDPR, CCPA) created an appearance
 *   of consent mechanisms while platform operators developed increasingly
 *   sophisticated methods to preserve behavioral extraction.
 *
 * KEY AGENTS:
 *   - Individual Readers: Primary victims (powerless/trapped at epistemic commons level; moderate/constrained individually) — bear the cost of attention capture and behavioral modification, face information asymmetry
 *   - Platform Operators (Facebook, Google, TikTok, etc.): Primary beneficiaries (institutional/arbitrage) — extract advertising value through attention and behavioral data; control the information filtering apparatus
 *   - Advertising Networks: Secondary beneficiaries (institutional/arbitrage) — monetize the behavioral data and attention capture; shape algorithmic ranking to maximize engagement (extractive proxy for profit)
 *   - Epistemic Commons: Primary victim (powerless/trapped) — abstract collective good; readers' cognitive autonomy is systematically compromised; no alternative coordinating mechanism exists at comparable scale
 *   - Privacy & Digital Rights Coalition: Organized actors (organized/constrained) — recognize both coordination and extraction; pursue regulatory and technological reform; constrained by platform power and network lock-in
 *   - Alternative Platform Architects: Powerful actors with mobile exit (powerful/mobile) — building decentralized, user-controlled, or advertising-free alternatives; constrained by network effects and capital requirements
 *   - Regulators: Institutional actors attempting oversight (institutional/constrained) — subject to regulatory capture; consent regulations (GDPR, CCPA) may have been shaped to serve compliance theater rather than genuine autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reader_informed_consent, 0.58).
domain_priors:suppression_score(reader_informed_consent, 0.72).
domain_priors:theater_ratio(reader_informed_consent, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reader_informed_consent, extractiveness, 0.58).
narrative_ontology:constraint_metric(reader_informed_consent, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reader_informed_consent, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reader_informed_consent, tangled_rope).
narrative_ontology:human_readable(reader_informed_consent, "Reader Informed Consent in Information Systems").
narrative_ontology:topic_domain(reader_informed_consent, "information_systems/cognitive_autonomy/epistemic_integrity").

domain_priors:requires_active_enforcement(reader_informed_consent).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reader_informed_consent, platform_operators).
narrative_ontology:constraint_beneficiary(reader_informed_consent, advertising_networks).
narrative_ontology:constraint_victim(reader_informed_consent, readers_epistemic_autonomy).
narrative_ontology:constraint_victim(reader_informed_consent, cognitive_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS (SNARE) — The collective capacity for unmanipulated judgment cannot exit the attention extraction system. Readers are individually trapped by network effects (everyone is on the same platform). No alternative epistemic infrastructure. Systematic extraction with zero degrees of freedom — maximum experienced extraction.
constraint_indexing:constraint_classification(reader_informed_consent, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIVIDUAL READER (TANGLED ROPE) — Benefits from free access to information platform; constrained by switching costs, social dependency, and lack of epistemic alternatives. Information access is genuine coordination benefit; attention capture and behavioral modification are asymmetric extraction. Constrained exit: could theoretically leave but faces social isolation, information disadvantage, career impact.
constraint_indexing:constraint_classification(reader_informed_consent, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the constraint as pure coordination: enabling information discovery, connecting readers with content, facilitating communication. From the platform's epistemic standpoint, the system solves a genuine collective action problem. Arbitrage exit: can shift revenue models, implement algorithmic changes, compete for users through perceived trustworthiness. Net beneficiary with high degrees of freedom.
constraint_indexing:constraint_classification(reader_informed_consent, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL RIGHTS COALITION (TANGLED ROPE) — Organized actors (privacy advocates, regulators, civil society) recognize both coordination function and extraction. See the constraint as solvable through transparency mechanisms (disclosure), user control (granular consent), and structural alternatives (federated platforms, algorithmic auditing). Constrained exit: regulatory capture and economic power asymmetry limit reform speed. Some genuine coordination function (information access) alongside active enforcement (GDPR, consent mechanisms) that partly mitigates but does not eliminate extraction.
constraint_indexing:constraint_classification(reader_informed_consent, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSENT RITUAL APPARATUS (PITON) — The formal consent mechanisms (cookie banners, privacy policies, terms of service) are substantially performative. Readers do not meaningfully understand what they are consenting to; platforms deploy consent as legal/regulatory theater rather than genuine epistemic autonomy mechanism. Theater ratio is high (0.68): the apparatus consumes significant resource and attention but minimal actual verification of understanding or meaningful choice. Classification driven by theater gate, not high extraction chi — the ritual maintains institutional legitimacy without delivering its stated function.
constraint_indexing:constraint_classification(reader_informed_consent, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, attention scarcity and information asymmetry are inherent features of cognition and communication. Cognitive capacity is bounded; information is abundant. Filtering by algorithmic ranking or editorial curation is a natural law of how any scale-invariant information system must function. The constraint appears immutable. However: the beneficiary declarations (platform operators, advertising networks) and victim declarations (readers, epistemic commons) flag this as a false summit candidate. The 'natural' filtering is actually contingent institutional choice (surveillance-based ranking vs. user-control-based ranking).
constraint_indexing:constraint_classification(reader_informed_consent, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ALTERNATIVE PLATFORM ARCHITECTS (TANGLED ROPE) — Technologists building decentralized, user-controlled, or privacy-preserving information systems see the constraint as solvable through architecture (end-to-end encryption, local-first computation, user-owned data stores). Mobile exit: can build alternatives, though network effects create high switching costs. See the constraint as jointly coordination + extraction; the coordination function can be preserved while extraction is reduced. Moderately powerful but constrained by lock-in and capital requirements.
constraint_indexing:constraint_classification(reader_informed_consent, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reader_informed_consent_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reader_informed_consent, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reader_informed_consent, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reader_informed_consent, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reader_informed_consent, TR),
    TR >= 0.70.

:- end_tests(reader_informed_consent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Platform operators capture behavioral data, attention, and engagement metrics that are monetized through advertising. Readers receive valuable access to information and social connectivity. The extraction is substantial but not total — many readers derive genuine benefit from platform access. The asymmetry is real: the platform captures the data; the reader captures the content. Suppression (0.72): High. Multiple mechanisms: network effects trap readers (switching platforms isolates them socially and informationally); informational asymmetry (readers do not know how algorithms rank content); cognitive asymmetry (readers cannot meaningfully process privacy policies); behavioral modification operates below awareness (algorithmic ranking shapes preferences without explicit triggering). Suppression reflects that exit is formally possible but practically constrained. Theater ratio (0.68): High. Consent apparatus is substantially performative: privacy policies exceed typical reading comprehension; cookie banners create illusion of choice without meaningful control; regulatory compliance (GDPR) is visible but enforcement of stated preferences is incomplete. The rising trajectory reflects that as regulation increased, platforms invested in consent theater rather than genuine autonomy mechanisms. Theater has become more sophisticated, not less, as compliance requirements have grown.
 *
 * PERSPECTIVAL GAP:
 *   The gap between platform-operator rope and reader-snare classifications is the core diagnostic. Platforms genuinely coordinate information; readers genuinely experience extraction. Neither perspective is factually wrong — they describe different aspects of a hybrid structure. The false summit perspective risks collapsing this gap by naturalizing the extraction as inherent to coordination. The piton perspective reveals that consent mechanisms are designed to bridge the gap without closing it — maintaining the appearance of reader autonomy while preserving extraction. The alternative-architect perspective shows that the gap is not inevitable: coordination can be achieved with lower extraction through different architectural choices (federated models, user-owned data, algorithm-transparent ranking). This means the constraint is reformable tangled rope, not immutable mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective follows from power level, exit options, and beneficiary/victim position. Platform operators with institutional power and arbitrage exit derive low or negative d values — they experience the constraint as beneficial coordination. Individual readers with moderate power and constrained exit derive high d values — they experience the constraint as extraction. The powerless/trapped perspective (epistemic commons) derives maximum d — the abstract collective good has zero exit options and zero power. The analytical observer with analytical power and exit derives mid-range d reflecting the mixed coordination-extraction structure. The alternative architects with powerful positioning and mobile exit derive mid-to-low d reflecting that they perceive solutions. These directionality values drive the chi formula computation and ground the perspectival diversity in structural reality, not mere disagreement.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the tangled rope classification at the platform level, which captures both the genuine coordination function (information access) and the real extraction asymmetry (behavioral data harvesting). The false summit perspective flags the risk of naturalizing this as immutable. The consent ritual perspective (piton) shows that regulatory responses may be creating theater without reducing underlying extraction. The resolution is not that one perspective is 'correct' — rather, the presheaf over the observation site reveals: (1) coordination is real but insufficient to justify the extraction asymmetry; (2) extraction is hidden through consent theater and informational asymmetry; (3) alternatives exist that preserve coordination with lower extraction, proving the constraint is not natural law; (4) the constraint persists because it benefits identifiable actors (platform operators, advertising networks) who have power to shape regulatory responses. The diagnostic signal is the rising theater ratio across the measurement interval — compliance grows more theatrical, not more genuine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_cognitive_capacity,
    'Can readers meaningfully understand and exercise informed consent given the cognitive load of privacy policies and algorithmic complexity?',
    'Empirical testing: comprehension assessments of readers exposed to actual privacy policies; eye-tracking studies of consent interaction; correlation between stated preferences and revealed behavior post-consent',
    'If meaningful comprehension is rare (< 10%): consent is theater, not genuine autonomy mechanism. Classification shifts from tangled_rope to snare at reader perspective. If comprehension is achievable with good design (> 50%): tangled_rope classification holds and reform is structurally possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_cognitive_capacity, empirical, 'Whether readers can meaningfully understand and exercise informed consent').

omega_variable(
    behavioral_modification_detection,
    'Are readers'' behavioral patterns modified by algorithmic ranking and personalization independent of their explicit consent or awareness?',
    'A/B testing comparing reader behavior under transparent vs. opaque algorithmic ranking; longitudinal tracking of engagement patterns before/after personalization introduction; field experiments with opt-out mechanisms',
    'If modification is substantial and operates below awareness: extraction is hidden and consent is illusory. Suppression should be higher. If readers can detect and adjust to ranking: suppression is lower and exit options are more mobile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_modification_detection, empirical, 'Degree to which algorithmic personalization modifies behavior independent of awareness').

omega_variable(
    alternative_ecosystem_viability,
    'Are decentralized, user-controlled, or advertising-free information platforms technically and economically viable at scale?',
    'Performance and adoption metrics for alternative platforms (Mastodon, Bluesky, Signal, Wikipedia, Substack); cost analysis of user-owned data infrastructure; network effect modeling under heterogeneous platform preferences',
    'If viable alternatives exist: exit options for readers should be upgraded from trapped/constrained to mobile. The constraint should reclassify as less extractive. If alternatives are not viable: network lock-in is structural, and suppression ceiling remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_ecosystem_viability, empirical, 'Viability of decentralized and user-controlled information platforms').

omega_variable(
    regulatory_capture_mechanism,
    'Do platform operators influence regulatory consent requirements (GDPR, CCPA) to appear compliant while preserving actual extraction?',
    'Analysis of regulatory capture lobbying; comparison of technical enforcement of consent preferences vs. stated regulatory intent; measurement of actual data reduction post-GDPR implementation',
    'If capture is systematic: consent regulation becomes performative (piton mechanism). Theater ratio should be higher. If regulation achieves enforcement: consent is becoming a genuine epistemic autonomy mechanism, reducing effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Degree to which platforms capture consent regulation to preserve extraction').

omega_variable(
    false_summit_natural_law_claim,
    'Is the constraint a genuine natural law of scaled information systems, or a contingent institutional choice that benefits identifiable actors?',
    'Comparative analysis: information systems with different architectures (user-controlled ranking, federated models, local-first design) and their extraction profiles; historical analysis of how contemporary extraction mechanisms became ''normal''',
    'If natural law: mountain classification is correct. If contingent: mountain is false summit. Platform operators and advertising networks would reclassify from beneficiary to engineer of the false naturalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether attention extraction is inherent to information systems or contingent institutional choice').

omega_variable(
    identity_fusion_reader_lock,
    'Do readers become identity-locked into platforms through social identity, algorithmic identity mirroring, and community membership rather than through material barriers to exit?',
    'Qualitative analysis of reader exit narratives; studies of social identity fusion with platforms; measurement of perceived identity cost vs. material switching cost',
    'If identity-lock is primary mechanism: readers should be classified as identity_locked (not just trapped or constrained). This would show suppression is partly internalized cognitive capture rather than purely structural barrier. Exit would require identity transformation, not just switching cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_reader_lock, empirical, 'Role of identity fusion in reader lock-in vs. material exit barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reader_informed_consent, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ric_tr_t0, reader_informed_consent, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ric_tr_t3, reader_informed_consent, theater_ratio, 3, 0.55).
narrative_ontology:measurement(ric_tr_t6, reader_informed_consent, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(ric_be_t0, reader_informed_consent, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ric_be_t3, reader_informed_consent, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(ric_be_t6, reader_informed_consent, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ric_su_t0, reader_informed_consent, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ric_su_t3, reader_informed_consent, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(ric_su_t6, reader_informed_consent, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reader_informed_consent, information_standard).
narrative_ontology:affects_constraint(reader_informed_consent, algorithmic_amplification_epistemic_harm).
narrative_ontology:affects_constraint(reader_informed_consent, attention_economy_cognitive_labor).
narrative_ontology:affects_constraint(reader_informed_consent, surveillance_capitalism_structural_extraction).

% DUAL FORMULATION NOTE:
% Reader informed consent is downstream of platform architecture choices and the advertising-driven business model. The upstream constraints (algorithmic design, attention economy mechanics, surveillance capitalism structure) determine the extraction pressure; informed consent is the response mechanism (often performative) to that pressure. Each upstream constraint has distinct extractiveness reflecting the specific structural choice; informed consent represents the layer where readers encounter and attempt to exercise autonomy over those choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reader_informed_consent, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
