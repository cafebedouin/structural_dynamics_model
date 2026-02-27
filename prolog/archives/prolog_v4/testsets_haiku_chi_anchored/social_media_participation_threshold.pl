% ============================================================================
% CONSTRAINT STORY: social_media_participation_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_media_participation_threshold, []).

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
 *   constraint_id: social_media_participation_threshold
 *   human_readable: The 2025 Digital Participation Threshold
 *   domain: social/technological
 *
 * SUMMARY:
 *   By 2025, major social media platforms have become de facto mandatory
 *   infrastructure for social and informational participation in most
 *   developed societies. What began as optional entertainment and networking
 *   tools have become functionally essential for employment networks, civic
 *   information access, family coordination, emergency alerts, and social
 *   participation. The constraint operates as a hybrid
 *   extraction-coordination mechanism: platforms genuinely solve a
 *   coordination problem (connecting billions of people at low marginal cost)
 *   while simultaneously extracting value through attention capture,
 *   behavioral data harvesting, algorithmic manipulation, and behavioral
 *   targeting. The threshold effect is sharp — non-participation now imposes
 *   significant biographical costs across multiple domains simultaneously.
 *   The extractiveness has grown from 0.35 in 2018 to 0.58 in 2025, driven by
 *   both deepening integration into institutional infrastructure and
 *   increasing sophistication of behavioral extraction mechanisms. Theater
 *   ratio has increased from 0.42 to 0.55, reflecting the growing
 *   performative dimension: institutions maintain platform presence for
 *   legitimacy signaling rather than genuine communication, while users
 *   curate idealized self-presentations rather than authentic interaction.
 *
 * KEY AGENTS:
 *   - Platform Operators (Meta, Google, ByteDance, TikTok, X): Institutional/arbitrage beneficiaries — capture network effects, attention, behavioral data, and advertising revenue. See platforms as legitimately valuable coordination infrastructure.
 *   - Non-Users and Excluded Populations: Powerless/trapped victims — cannot access employment networks, civic information, family coordination, emergency alerts without platform participation. Digital divide deepens into participation divide.
 *   - Reluctant Participants (Majority): Moderate/constrained victims and beneficiaries — gain genuine access to networks but suffer attention extraction, surveillance, and algorithmic manipulation. Trapped in network effects.
 *   - Privacy-Conscious Professionals: Powerful/mobile agents — have resources and professional standing to negotiate selective participation or migrate between platforms. Can exercise some agency.
 *   - Advertising Ecosystem: Institutional/arbitrage beneficiaries — depends on behavioral targeting enabled by platform data extraction. Incentivizes platform suppression of privacy.
 *   - Legacy Institutional Gatekeepers (News, Government, Schools, Employers): Institutional/arbitrage actors maintaining performative platform presence. See own platform accounts as degraded but necessary.
 *   - Digital Rights and Interoperability Coalition: Organized/mobile agents — recognize extraction mechanisms and advocate for federated alternatives with genuine exit paths. Represent the scaffold perspective.
 *   - Analytical Observer: Civilizational perspective — risks naturalizing network effects as immutable law rather than contingent technological choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_media_participation_threshold, 0.58).
domain_priors:suppression_score(social_media_participation_threshold, 0.68).
domain_priors:theater_ratio(social_media_participation_threshold, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_media_participation_threshold, extractiveness, 0.58).
narrative_ontology:constraint_metric(social_media_participation_threshold, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(social_media_participation_threshold, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_media_participation_threshold, tangled_rope).
narrative_ontology:human_readable(social_media_participation_threshold, "The 2025 Digital Participation Threshold").
narrative_ontology:topic_domain(social_media_participation_threshold, "social/technological").

domain_priors:requires_active_enforcement(social_media_participation_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_media_participation_threshold, platform_operators).
narrative_ontology:constraint_beneficiary(social_media_participation_threshold, advertising_ecosystem).
narrative_ontology:constraint_victim(social_media_participation_threshold, non_users).
narrative_ontology:constraint_victim(social_media_participation_threshold, privacy_conscious_citizens).
narrative_ontology:constraint_victim(social_media_participation_threshold, informational_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-USER / EXCLUDED (SNARE) — Cannot access essential social coordination, employment networks, emergency alerts, or civic information without platform participation. Exit is not optional; the cost of non-participation now exceeds the cost of submission. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98.
constraint_indexing:constraint_classification(social_media_participation_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RELUCTANT PARTICIPANT (TANGLED ROPE) — Participates because social coordination genuinely requires it (network effects), but also bears costs: attention extraction, data surveillance, algorithmic manipulation, behavioral targeting. Benefits from network access while suffering extraction. d≈0.72, f(d)≈1.12, σ=1.0 → χ≈0.65.
constraint_indexing:constraint_classification(social_media_participation_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the constraint as pure coordination infrastructure. Captures data flows and attention flows as legitimate returns to infrastructure provision. Network effects create genuine coordination value that wouldn't exist without platforms. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(social_media_participation_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PRIVACY-CONSCIOUS PROFESSIONAL (TANGLED ROPE) — Has sufficient resources and professional standing to maintain limited platform presence while using alternative channels. Experiences constraint as hybrid: benefits from network access for career/social coordination, but can negotiate better privacy terms or migrate between platforms. d≈0.45, f(d)≈0.42, σ=1.0 → χ≈0.24.
constraint_indexing:constraint_classification(social_media_participation_threshold, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY INSTITUTIONAL GATEKEEPER (PITON) — Traditional institutions (newspapers, employers, government agencies, schools) maintain their own communication channels but increasingly see platform presence as performative compliance with expectations of digital engagement. Theater ratio=0.55 reflects that institutional platform accounts often serve legitimacy signaling rather than genuine communication function. The institution maintains presence through inertia and competitive pressure rather than effective reach.
constraint_indexing:constraint_classification(social_media_participation_threshold, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: DIGITAL RIGHTS MOVEMENT (TANGLED ROPE) — Organized agents (civil society, digital rights groups, interop advocates) recognize both the coordination value of platforms AND the extraction mechanisms. See alternative pathways (interoperability, open protocols, federated systems) as viable, though not yet mature. Benefits from organizing through platforms while suffering extraction. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.47.
constraint_indexing:constraint_classification(social_media_participation_threshold, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: INTEROPERABILITY / PROTOCOL COALITION (SCAFFOLD) — Technologists and policy advocates building federation protocols (ActivityPub, decentralized identity, open social graphs) see the platform threshold as a temporary coordination failure with a structured exit path. The extraction mechanism is designed to degrade as interoperability matures and network effects become composable across platforms. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.22. Has sunset clause: estimated 10-15 years for mature alternative infrastructure.
constraint_indexing:constraint_classification(social_media_participation_threshold, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, network effects create a structural mathematical reality: large networks have inherent coordination advantages that appear as an immutable law. Any communication medium will concentrate participation, creating a participation threshold for access. This perspective risks naturalizing what is actually a contingent technological and economic choice. ε=0.58, suppression=0.68 contradict the mountain classification — engine will detect false summit.
constraint_indexing:constraint_classification(social_media_participation_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_media_participation_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_media_participation_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_media_participation_threshold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_media_participation_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_media_participation_threshold, TR),
    TR >= 0.70.

:- end_tests(social_media_participation_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Platforms extract significant value through behavioral data, attention capture, and algorithmic manipulation, but the extraction is enabled by genuine network coordination benefits. The 0.58 value reflects that ~58% of the constraint's function is extraction rather than coordination. The sharp growth from 0.35 (2018) reflects both institutional integration (making non-participation costlier) and technological sophistication (dark patterns, infinite scroll, algorithmic engagement optimization). Suppression (0.68): High. Multiple barriers prevent meaningful exit: (1) Network effects — competitors cannot offer equivalent connectivity; (2) Employment integration — many employers require platform access; (3) Civic/informational integration — government, schools, emergency alerts increasingly platform-dependent; (4) Social integration — peer networks are platform-native, switching costs are extreme; (5) Behavioral lock-in — platforms deploy sophisticated retention mechanisms. The suppression is high but not absolute (0.68 not 0.95) because limited alternatives exist (private messaging, email, SMS) and some populations (high-income, tech-literate) have exit options. Theater ratio (0.55): Moderate. Platform behavior contains both genuine coordination (user networks are real) and performative signaling (curated self-presentation, institutional virtue-signaling, engagement theater). The 0.55 value reflects that slightly more than half of platform activity is functional network maintenance, while slightly less than half is performative presentation or algorithmic engagement gamification. Claimed type (tangled_rope): Correct. The constraint combines genuine coordination (beneficiaries can solve real network problems) with asymmetric extraction (victims bear attention costs while beneficiaries capture data value).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a sharp perspectival divide between high-power actors who can negotiate selective participation and powerless actors for whom non-participation is not viable. The platform operator sees pure coordination (Rope) — from their position, they are solving the legitimate problem of connecting billions of people. The non-user sees pure extraction (Snare) — for them, the coordination benefit is forced upon them, while all costs are real. The reluctant participant sees hybrid coordination-extraction (Tangled Rope) — they genuinely benefit from networks while suffering extraction. The privacy-conscious professional sees attenuated extraction (Tangled Rope with lower d) — they have power and mobility to reduce suppression. The digital rights coalition sees a temporary problem with a viable exit path (Scaffold) — federated protocols and interoperability can degrade extraction while preserving coordination. The institutional gatekeeper sees degraded ritual (Piton) — platform presence persists through inertia and competitive pressure despite low functional value. The analytical observer risks seeing network effects as a natural law (Mountain) — treating contingent technological/economic choices as immutable laws of coordination. The engine's false summit detector will catch this.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.10. Net beneficiary with negative effective extraction. Non-users: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — cannot exit without biographical cost. Reluctant participants: Victim + constrained → d≈0.72, f(d)≈1.12. High extraction; some network benefit but suppression is strong. Privacy-conscious professionals: Both beneficiary and victim + mobile → d≈0.45, f(d)≈0.42. Lower extraction because mobility reduces suppression. Digital rights coalition: Organized + mobile → d≈0.50, f(d)≈0.65. Moderate extraction with low suppression due to agency and alternatives. Institutional gatekeepers: Arbitrage (performative) → d≈0.08, f(d)≈-0.10. Piton classification comes from theater gate (0.55), not from high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing the coordination benefit (genuine: networks are valuable) from the extraction mechanism (behavioral targeting, attention capture, data surveillance). The constraint is not 'networks are impossible without extraction' (which would be mountain-tier false), but rather 'platforms have designed extraction into their coordination mechanism in ways that are removable.' The scaffold perspective shows this: interoperable protocols can preserve coordination benefit while removing extraction. The digital rights perspective shows this: regulations can degrade dark patterns while preserving network access. The constraint is legitimately tangled_rope — both functions are real — but the entanglement is technologically contingent, not structurally necessary. This resolves the mandatrophy by showing that the classification is stable across perspectives while the structural mechanisms (behavioral extraction, network effects, regulatory capture) are empirically resolvable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    essential_vs_convenience_boundary,
    'Where is the boundary between genuinely essential participation (employment, civic information, family coordination) versus convenient-but-optional engagement?',
    'Sector analysis: employment requirements by industry; government service accessibility; family communication patterns; emergency alert dependency; civic voting/participation requirements',
    'If essential category is <40% of claimed functions: constraint is weaker than snare classification suggests. If >70%: snare classification confirmed for majority of population.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(essential_vs_convenience_boundary, empirical, 'Boundary between essential and optional platform participation').

omega_variable(
    interoperability_maturity_timeline,
    'Can federated/interoperable protocols (ActivityPub, Bluesky, decentralized identity) mature to functional parity with centralized platforms before network lock-in becomes irreversible?',
    'Technical benchmarking: feature parity, user experience, security, scalability; adoption tracking for alternative protocols; institutional migration patterns',
    'If maturity achieved within 10 years: scaffold perspective confirmed. If >15 years or technically infeasible: scaffold is aspirational, constraint remains snare/tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_maturity_timeline, empirical, 'Feasibility of interoperable protocol maturity as exit path').

omega_variable(
    regulatory_intervention_binding,
    'Can regulation (interoperability mandates, data portability requirements, algorithmic transparency) force platform incumbents to reduce extraction without destroying network coordination?',
    'Policy analysis: EU Digital Markets Act enforcement; proposed US regulation; compliance costs; architectural feasibility; international coordination',
    'If binding regulation succeeds: extraction mechanisms can degrade without waiting for technical alternatives. Constraint transitions from snare to scaffold within regulatory timeline. If regulation fails/captured: extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_intervention_binding, preference, 'Whether regulation can degrade extraction while preserving coordination').

omega_variable(
    behavioral_addiction_mechanism,
    'Is the suppression (0.68) driven primarily by network effects (structural — legitimate coordination advantage) or by behavioral extraction (algorithmic dark patterns, attention capture)?',
    'Comparative analysis: network effects measurable via non-dark-pattern platforms; behavioral extraction measurable via platform design audits, user attention tracking, engagement manipulation studies',
    'If 70%+ suppression is behavioral: extraction mechanism is removable while preserving coordination. If 70%+ suppression is network effects: no alternative protocols will reduce the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_addiction_mechanism, empirical, 'Attribution of suppression to network effects vs behavioral manipulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_media_participation_threshold, 2018, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smpt_tr_t2018, social_media_participation_threshold, theater_ratio, 2018, 0.42).
narrative_ontology:measurement(smpt_tr_t2021, social_media_participation_threshold, theater_ratio, 2021, 0.48).
narrative_ontology:measurement(smpt_tr_t2025, social_media_participation_threshold, theater_ratio, 2025, 0.55).

% Extraction over time
narrative_ontology:measurement(smpt_be_t2018, social_media_participation_threshold, base_extractiveness, 2018, 0.35).
narrative_ontology:measurement(smpt_be_t2021, social_media_participation_threshold, base_extractiveness, 2021, 0.48).
narrative_ontology:measurement(smpt_be_t2025, social_media_participation_threshold, base_extractiveness, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_media_participation_threshold, information_standard).
narrative_ontology:affects_constraint(social_media_participation_threshold, algorithmic_amplification_bias).
narrative_ontology:affects_constraint(social_media_participation_threshold, attention_capture_dark_patterns).
narrative_ontology:affects_constraint(social_media_participation_threshold, behavioral_data_surveillance).
narrative_ontology:affects_constraint(social_media_participation_threshold, network_effect_lock_in).

% DUAL FORMULATION NOTE:
% The social media participation threshold is downstream of specific platform design choices (algorithms, behavioral targeting, engagement optimization). Each downstream constraint has its own ε reflecting the empirical contestability of that specific mechanism. The participation threshold integrates across all mechanisms: it is the global effect of accumulated platform extraction design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_media_participation_threshold, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
