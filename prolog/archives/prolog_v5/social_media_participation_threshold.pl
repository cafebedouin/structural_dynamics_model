% ============================================================================
% CONSTRAINT STORY: social_media_participation_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The 2025 digital participation threshold represents a structural
 *   transition point where social media platforms shifted from optional
 *   communication tools to de facto mandatory infrastructure for economic and
 *   social participation. This constraint operates across employment,
 *   education, government services, community organizing, and information
 *   distribution. Non-adopters face systematic exclusion; privacy-conscious
 *   users face binary choice between surveillance and marginalization; casual
 *   users experience genuine coordination benefits entangled with data
 *   extraction; platforms capture network-effect monopolies; and public
 *   institutions have built dependency on proprietary systems. The constraint
 *   exhibits snare characteristics (high extraction, high suppression,
 *   trapped exit) from the perspective of non-adopters and privacy-conscious
 *   populations, tangled-rope characteristics (genuine coordination plus
 *   extraction) from casual users, rope characteristics (pure coordination
 *   benefit) from platforms themselves, and potential scaffold
 *   characteristics (with organized advocacy for alternatives with sunset
 *   logic). The theater ratio has increased modestly as platforms develop
 *   sophisticated algorithmic governance that performs legitimacy while
 *   extracting attention and data.
 *
 * KEY AGENTS:
 *   - Non-Adopters: Primary victim (powerless/trapped) — face systematic exclusion from economic opportunity and social information; no meaningful exit option
 *   - Privacy-Conscious Users: Primary victim (powerless/trapped) — forced choice between data extraction and participation; limited alternative platforms with comparable reach
 *   - Casual Participants: Secondary victim-beneficiary (moderate/constrained) — experience genuine coordination benefits (family connection, community organizing) entangled with extraction; can reduce participation at social cost
 *   - Platform Corporations: Primary beneficiary (institutional/arbitrage) — capture attention, data assets, and network effects; see constraint as pure coordination problem; maximal exit options
 *   - Information Commons: Structural victim (moderate/trapped) — dependent on proprietary platforms for discourse and information distribution; extraction is high and asymmetric
 *   - Government and Institutional Services: Institutional actor (organized/constrained) — built dependency on social media for public service delivery; created piton through institutional inertia rather than strategic choice
 *   - Digital Rights Advocates: Organized actors (organized/constrained) — developing alternative platforms, regulatory frameworks, and interoperability standards as scaffold mechanisms with sunset logic
 *   - Analytical Observer: Civilizational observer (analytical/analytical) — risks false summit error by naturalizing network-effect concentration as immutable law rather than contingent policy outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_media_participation_threshold, 0.62).
domain_priors:suppression_score(social_media_participation_threshold, 0.68).
domain_priors:theater_ratio(social_media_participation_threshold, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_media_participation_threshold, extractiveness, 0.62).
narrative_ontology:constraint_metric(social_media_participation_threshold, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(social_media_participation_threshold, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_media_participation_threshold, snare).
narrative_ontology:human_readable(social_media_participation_threshold, "The 2025 Digital Participation Threshold").
narrative_ontology:topic_domain(social_media_participation_threshold, "social/technological").

domain_priors:requires_active_enforcement(social_media_participation_threshold).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_media_participation_threshold, platform_corporations).
narrative_ontology:constraint_victim(social_media_participation_threshold, non_adopters).
narrative_ontology:constraint_victim(social_media_participation_threshold, privacy_conscious_users).
narrative_ontology:constraint_victim(social_media_participation_threshold, economically_excluded_populations).
narrative_ontology:constraint_victim(social_media_participation_threshold, information_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ADOPTER (SNARE) — Individuals who do not maintain active social media presence face systematic exclusion from job postings, community information, emergency alerts, government services, and informal social coordination. Exit is effectively unavailable — withdrawal means material loss of economic opportunity and social participation. The constraint extracts from this agent maximally: they bear full cost of non-participation while beneficiaries capture the coordination premium.
constraint_indexing:constraint_classification(social_media_participation_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRIVACY-CONSCIOUS USERS (SNARE) — Users who object to data extraction, algorithmic profiling, and attention capture face binary choice: participate under extractive terms or exit entirely. Limited alternative platforms exist with comparable network reach. Exit options are severely constrained — attempting to maintain privacy while participating requires constant vigilance and acceptance of degraded service. The extraction is high: users pay in attention, data, and behavioral modification.
constraint_indexing:constraint_classification(social_media_participation_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CASUAL PARTICIPANTS (TANGLED ROPE) — Users who adopt platforms for genuine coordination benefits (family connection, community organizing, information sharing) experience genuine value alongside extraction. The constraint provides real coordination function: matching distant relatives, enabling local mutual aid, distributing time-sensitive information. But coordination is entangled with extraction: attention harvesting, algorithmic manipulation, and data monetization are bundled with legitimate functions. Exit is constrained but possible — users can reduce participation or switch platforms at some social cost.
constraint_indexing:constraint_classification(social_media_participation_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM CORPORATIONS (ROPE) — From the institutional beneficiary perspective, the platforms solve a genuine coordination problem: connecting billions of users across geographic and temporal distance at near-zero marginal cost. The platforms experience the constraint as pure coordination mechanism. Their exit options are maximal — they can modify platform design, pricing, features. Extraction runs toward them: captured user attention, data assets, and network effects produce extraordinary returns. The constraint appears as legitimate coordination from this perspective.
constraint_indexing:constraint_classification(social_media_participation_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INFORMATION COMMONS (SNARE) — Public discourse, news distribution, and informational coordination have migrated to proprietary platforms. The commons itself becomes dependent on private platform infrastructure for basic function. Platform algorithm modifications, moderation policies, and business decisions directly shape what information reaches public view. The information commons has no exit option and no negotiating power. Extraction is high and asymmetric: platforms control information flow with minimal accountability to the commons.
constraint_indexing:constraint_classification(social_media_participation_threshold, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: GOVERNMENT AND INSTITUTIONAL SERVICES (PITON) — Public institutions have increasingly shifted service delivery to social media (official announcements, permit applications, emergency alerts) without developing systematic alternatives. This creates institutional dependence on proprietary platforms for public functions. The theater ratio is high: formal regulatory structures appear to govern platform access to public services, but enforcement is minimal and platforms retain effective veto power. The degradation derives from institutional inertia — government built dependency rather than recognizing the structural risk. Exit pathways exist but are politically expensive.
constraint_indexing:constraint_classification(social_media_participation_threshold, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: DIGITAL RIGHTS ADVOCATES (SCAFFOLD) — Organized advocates for alternative platforms, data rights regulation, and algorithmic transparency see the participation threshold as a temporary coordination failure with structural exit mechanisms available: interoperability standards, federated platforms (Mastodon, Bluesky), regulatory frameworks (EU DMA, DSA), and public digital infrastructure investment. This perspective recognizes extraction but frames it as solvable via institutional redesign. The sunset logic is conditional: if interoperability and platform alternatives mature, the participation threshold loses enforcement power.
constraint_indexing:constraint_classification(social_media_participation_threshold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE SUMMIT RISK (MOUNTAIN?) — A civilizational-scale analysis might frame digital platform dependency as an immutable consequence of network-effect economics: any coordination mechanism at global scale will concentrate power in a small number of dominant platforms. This perspective risks naturalizing what is actually a contingent policy choice. The base metrics reveal this as a false summit: the extractiveness (0.62), suppression (0.68), and structural enforcement requirements indicate institutional design choices (algorithm design, moderation policies, network effects protection, regulatory capture), not laws of nature.
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
 *   Extractiveness (0.62): Moderate-high, increasing over the interval. The constraint begins with voluntary participation (ε ≈ 0.35) but has hardened as institutions migrate core functions to platforms. By 2025, non-adoption carries material economic and social penalties — exclusion from job postings, community information, emergency alerts, government services. The extraction is not total (platforms do provide genuine coordination) but is substantial and growing. The trajectory reflects institutional lock-in: as more actors depend on platform presence, the cost of non-participation rises exponentially. Suppression (0.68): High. Alternatives exist but are structurally inferior: federated platforms lack network effects, traditional coordination mechanisms (email, web) lack reach and discoverability, opting out means accepting marginalization. The suppression derives from network effects (concentrated in few dominant platforms) and institutional migration (services moved to platforms). Theater ratio (0.45): Moderate. The constraint operates with relatively low performative overhead — extraction is explicit and acknowledged by participants (users know they are providing attention and data in exchange for access). The theater rises slightly as platforms develop algorithmic governance that frames surveillance as 'personalization' and data extraction as 'service improvement,' but this is limited theater relative to other constraints (e.g., judicial process, academic peer review).
 *
 * PERSPECTIVAL GAP:
 *   The gap between snare and rope readings is maximal: non-adopters experience the constraint as extractive coercion with no exit; platforms experience it as voluntary coordination that enables connection at scale. The tangled rope reading (casual participants) occupies the middle: genuine coordination value mixed with extraction. The piton reading (government institutions) reveals institutional degradation: official oversight without real enforcement. The scaffold reading (digital rights advocates) provides the only exit mechanism: interoperability and alternatives with clear sunset. The false summit risk appears in the civilizational analysis: naturalizing network effects as immutable law rather than recognizing them as contingent on platform dominance strategies and regulatory choices. The perspectival richness of this constraint makes it diagnostically valuable for testing whether index-based classification can capture the full complexity of institutional extraction that looks like coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's power level, exit options, and structural relationship to the extraction flow. Non-adopters and privacy-conscious users are trapped (d → 0.95, f(d) ≈ 1.42): maximum experienced extraction. Casual participants are constrained but with some mobile options (d → 0.55, f(d) ≈ 0.75): moderate-high extraction. Platform corporations are institutional beneficiaries with arbitrage options (d → 0.05, f(d) ≈ -0.12): extraction flows toward them, appearing as low or negative cost (they gain). Information commons has no agency (d → 1.0, f(d) ≈ 1.42): maximum extraction. Government institutions are constrained (d → 0.65, f(d) ≈ 1.00): moderate extraction from their institutional perspective. Digital rights advocates are organized with constrained exit (d → 0.40, f(d) ≈ 0.40): low-moderate extraction. The analytical observer operates at the universal scope boundary (d → 0.72, f(d) ≈ 1.15): observational extraction (effort to understand and classify the constraint). The effective extractiveness (χ) for each perspective scales this base extractiveness (ε = 0.62) by f(d) and scope modifier σ(S). Non-adopter chi: 0.62 × 1.42 × 1.2 (global scope) ≈ 1.06 (capped at 1.0). Platform chi: 0.62 × (-0.12) × 1.2 ≈ -0.09 (negative — extraction flows toward them).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by showing that snare, tangled rope, and rope classifications coexist without contradiction — they are perspectival readings from different structural positions. The question 'is social media a snare or a rope?' has no binary answer; the constraint is a snare to non-adopters, tangled rope to casual participants, and rope to platforms. The false summit risk (civilizational analyst naturalizing network effects as immutable) is correctly identified by the engine: the structural data shows extractiveness, suppression, and active enforcement dynamics, not natural law properties (no accessibility collapse, resistance metrics don't support mountain). The mandatrophy is resolved by accepting that the constraint is genuinely a Snare-Tangled Rope-Rope presheaf over the observation site, with institutional solutions (regulation, interoperability, alternatives) available via the scaffold mechanism. The extraction is not immutable; it is architecturally contingent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interoperability_technical_feasibility,
    'Can interoperability standards (ActivityPub-style federated protocols) achieve parity with proprietary platforms for real-time coordination, content discovery, and network effects?',
    'Technical performance benchmarks: latency, scalability, feature parity. Adoption metrics for federated platforms. Cross-platform compatibility testing.',
    'If technically viable: scaffold sunset mechanism is structural (threshold dissolves as alternatives mature). If technically infeasible: participation threshold hardens into mountain-like immutability despite organizational origins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_technical_feasibility, empirical, 'Whether federated interoperability can achieve platform feature parity').

omega_variable(
    regulatory_enforcement_capacity,
    'Can national/supranational regulators enforce interoperability, data portability, or algorithmic transparency mandates against platform resistance?',
    'Case analysis: EU DMA enforcement, FTC antitrust actions, state-level legislation. Tracking platform compliance or non-compliance, penalties, and behavioral response.',
    'If enforceable: regulatory scaffold provides exit mechanism (victims can force platform API access, migrate data). If unenforceable: platforms maintain structural veto; threshold remains snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_enforcement_capacity, empirical, 'Whether regulators can enforce interoperability and data portability').

omega_variable(
    network_effects_substitutability,
    'Are network effects in social coordination substitutable — can a secondary platform achieve critical mass if primary platforms become unacceptable?',
    'Historical case studies (MySpace→Facebook transition, Twitter→Bluesky dynamics). Analysis of adoption barriers for alternative platforms. User switching cost measurements.',
    'If substitutable: threshold is contingent institutional arrangement (snare/tangled_rope). If irreplaceable: network effects are quasi-structural (threshold approaches mountain-like hardness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_substitutability, empirical, 'Whether social network effects permit meaningful competition and switching').

omega_variable(
    privacy_preserving_coordination_alternatives,
    'Can coordination functions (job matching, community organizing, emergency alerts) be provided via privacy-preserving alternatives without network-effect penalties?',
    'Technical research on differential privacy, encrypted coordination protocols, and decentralized matching algorithms. Pilot deployment and user preference studies.',
    'If viable: privacy-conscious exit becomes available (snare extraction reduces for victim group). If impossible: extraction of privacy attention is immutable (victims face permanent sacrifice for participation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(privacy_preserving_coordination_alternatives, empirical, 'Whether privacy-preserving alternatives can replace proprietary platform functions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_media_participation_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smpt_tr_t0, social_media_participation_threshold, theater_ratio, 0, 0.25).
narrative_ontology:measurement(smpt_tr_t5, social_media_participation_threshold, theater_ratio, 5, 0.35).
narrative_ontology:measurement(smpt_tr_t10, social_media_participation_threshold, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(smpt_be_t0, social_media_participation_threshold, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(smpt_be_t5, social_media_participation_threshold, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(smpt_be_t10, social_media_participation_threshold, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_media_participation_threshold, global_infrastructure).
narrative_ontology:affects_constraint(social_media_participation_threshold, algorithmic_content_filtering).
narrative_ontology:affects_constraint(social_media_participation_threshold, data_monetization_asymmetry).
narrative_ontology:affects_constraint(social_media_participation_threshold, network_effect_concentration).
narrative_ontology:affects_constraint(social_media_participation_threshold, digital_divide_exclusion).

% DUAL FORMULATION NOTE:
% The participation threshold constraint family decomposes into four related but structurally distinct constraints: (1) participation_threshold (this story) — the dependency on platforms for access to coordination and information; (2) algorithmic_content_filtering — the extraction via attention and behavioral modification; (3) data_monetization_asymmetry — the extraction via data and profile commodification; (4) network_effect_concentration — the structural mechanism that maintains platform monopoly. The participation threshold is upstream of all three: it establishes why platforms have coercive power in the first place. Each story has distinct ε, suppression, and theater ratios reflecting different aspects of the same underlying institutional arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
