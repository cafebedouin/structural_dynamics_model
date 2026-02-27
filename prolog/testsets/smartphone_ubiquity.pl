% ============================================================================
% CONSTRAINT STORY: smartphone_ubiquity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_smartphone_ubiquity, []).

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
 *   constraint_id: smartphone_ubiquity
 *   human_readable: The Smartphone Ubiquity Constraint
 *   domain: technological/social/economic
 *
 * SUMMARY:
 *   The smartphone ubiquity constraint describes the transition of the
 *   smartphone from a communication device into a 'place within which we
 *   live' — a portable digital home that mediates access to financial
 *   systems, employment markets, healthcare, civic participation, and social
 *   identity. This constraint exhibits snare characteristics at the user
 *   level (powerless and constrained users face systematic extraction of
 *   attention, location, behavioral data, and financial surplus through
 *   platforms they cannot exit) while appearing as coordination, regulation,
 *   or even natural law from other perspectives. The constraint is maintained
 *   through suppression mechanisms including ecosystem lock-in (iOS/Android
 *   duopoly), app store gatekeeping, behavioral addiction engineering, and
 *   the systematic digitalization of essential services. The beneficiaries
 *   (platform corporations, advertising ecosystem, surveillance apparatus)
 *   extract value through data monetization, attention capture, and
 *   behavioral manipulation. The victims include non-smartphone users
 *   (excluded from essential services), aware users (trapped despite
 *   understanding the extraction), and the digital underclass (dependent on
 *   platforms they cannot afford to opt out of). The theater ratio (0.55)
 *   reflects that the smartphone is genuinely useful (communication,
 *   information access, coordination) but this functionality is deeply
 *   entangled with and subordinated to extraction mechanisms. The constraint
 *   has intensified over the 16-year interval as essential services have
 *   moved exclusively to app-based delivery, increasing suppression from 0.40
 *   to 0.68 and extractiveness from 0.25 to 0.58.
 *
 * KEY AGENTS:
 *   - Platform Corporations (Apple, Google, Meta, Amazon, TikTok): Institutional/arbitrage beneficiaries — control OS ecosystems, app distribution, data pipelines, and surveillance infrastructure; experience the constraint as coordination solution
 *   - Digital Underclass: Powerless/trapped victims — lack device access or reliable connectivity; face systematic exclusion from employment, financial services, healthcare; cannot exit
 *   - Aware Users: Moderate/constrained victims — understand extraction mechanisms but cannot meaningfully exit because alternatives lack scale; trapped in dependent position
 *   - Regulatory Coalition: Organized/constrained actors — national/regional authorities (EU, US FTC, data protection agencies) attempting to constrain extraction while recognizing genuine coordination functions; perceive tangled_rope structure
 *   - Advertising Ecosystem: Institutional/arbitrage beneficiaries — data brokers, ad networks, marketing firms extracting behavioral surplus; downstream beneficiaries of platform ubiquity
 *   - Essential Service Providers: Institutional/constrained actors — healthcare, financial, employment systems that have made smartphone access mandatory for service delivery; economically dependent on platform infrastructure
 *   - Non-Smartphone Users: Powerless/trapped victims — elderly, economically excluded, privacy-conscious populations systematically shut out from digital-first services; excluded rather than extracted from
 *   - Surveillance Apparatus: Institutional/arbitrage beneficiaries — government agencies, law enforcement, security services leveraging smartphone ubiquity for population monitoring; symbiotic relationship with platform data infrastructure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(smartphone_ubiquity, 0.58).
domain_priors:suppression_score(smartphone_ubiquity, 0.68).
domain_priors:theater_ratio(smartphone_ubiquity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(smartphone_ubiquity, extractiveness, 0.58).
narrative_ontology:constraint_metric(smartphone_ubiquity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(smartphone_ubiquity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(smartphone_ubiquity, snare).
narrative_ontology:human_readable(smartphone_ubiquity, "The Smartphone Ubiquity Constraint").
narrative_ontology:topic_domain(smartphone_ubiquity, "technological/social/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(smartphone_ubiquity, platform_corporations).
narrative_ontology:constraint_beneficiary(smartphone_ubiquity, advertising_ecosystem).
narrative_ontology:constraint_beneficiary(smartphone_ubiquity, surveillance_apparatus).
narrative_ontology:constraint_victim(smartphone_ubiquity, non_smartphone_users).
narrative_ontology:constraint_victim(smartphone_ubiquity, attention_economy_targets).
narrative_ontology:constraint_victim(smartphone_ubiquity, digital_underclass).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIGITAL UNDERCLASS (SNARE) — Users without smartphone access or with severely limited access face systematic exclusion from employment, financial services, healthcare coordination, educational access, and civic participation. Exit options are severely constrained: digital participation is now a prerequisite, not an option. The constraint extracts attention, location data, behavioral metadata, and financial surplus through forced participation in exploitative platforms. High suppression: platforms set terms unilaterally, algorithm opacity prevents transparency, and dependence grows as digital pathways monopolize access.
constraint_indexing:constraint_classification(smartphone_ubiquity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AWARE USER (SNARE) — Users who understand the extraction mechanisms (data harvesting, algorithmic manipulation, attention capture) remain trapped because alternatives do not exist at sufficient scale. Constrained exit: switching devices or platforms is superficial because the ecosystem is dominated by two operating systems and three dominant platforms. Experienced extraction includes attention hijacking, behavioral manipulation, location tracking, and psychological dependency engineering. Awareness increases experienced extractiveness rather than enabling exit.
constraint_indexing:constraint_classification(smartphone_ubiquity, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY COALITION (TANGLED ROPE) — National regulators (EU GDPR framework, US FTC action, data protection agencies) perceive the smartphone ubiquity as a coordination problem requiring enforcement (GDPR, DMA, age restrictions) AND an asymmetric extraction regime they are trying to constrain. The coalition has genuine enforcement power but faces structural limitations: platforms operate globally, capital mobility circumvents jurisdiction, and regulators operate at national scale while extraction operates at global scale. This is tangled_rope because coordination benefits exist (standardized data practices improve interoperability and consumer protection) alongside persistent extraction (platform compliance theater obscures continued data exploitation).
constraint_indexing:constraint_classification(smartphone_ubiquity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM CORPORATION (ROPE) — The smartphone ubiquity is experienced as a coordination solution to the problem of monetizing attention and behavioral prediction at scale. Platforms frame the constraint as enabling voluntary participation: users 'choose' to use the service, extract value from connectivity, benefit from free tools. From the platform perspective, the constraint solves a genuine coordination problem: how to connect billions of users across geographies without centralized infrastructure cost. Net beneficiary with arbitrage options: the platform captures data rent, advertising surplus, and position in the digital economy. Exit for platforms is enabled (they can shift to web3, relocate jurisdiction, rebrand) — arbitrage defines their power position.
constraint_indexing:constraint_classification(smartphone_ubiquity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DISCOURSE OF DIGITAL INEVITABILITY (PITON) — The smartphone ubiquity is often naturalized as inevitable technological progress: 'everyone has a smartphone,' 'digital is the future,' 'you can't live without one.' This framing performs legitimacy work — it converts a contingent institutional arrangement (platform dominance, app-based service delivery, digital-first governance) into a law of nature. The theater ratio is moderate (0.55): real functionality exists (communication, information access, coordination) but is deeply entangled with extraction mechanisms (tracking, algorithmic manipulation, forced interface design). The discourse maintains the system through performative inevitability, but the underlying constraint shows signs of institutional atrophy — awareness of extraction mechanics is rising, regulatory pressure is mounting, and alternative models (decentralized protocols, privacy-first services, dumb phones) are emerging as viable counter-constraints. The piton classification reflects that the 'naturalness' narrative is degraded by visible alternatives.
constraint_indexing:constraint_classification(smartphone_ubiquity, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — A tempting but false analysis: smartphone ubiquity appears inevitable because human cognitive architecture is optimized for portable information access, social connection, and tool extension. From this view, the constraint is a natural law of technological evolution — any society reaching sufficient infrastructure will converge on ubiquitous mobile computing. However, the structural data contradicts the mountain classification. The beneficiaries are highly concentrated (five platform corporations). The suppression is engineered and maintained through locked ecosystems, app store gatekeeping, and surveillance capitalism practices — not inherent to technology. The victims are systematically excluded rather than naturally disadvantaged. The constraint is contingent on specific institutional arrangements (intellectual property regimes, spectrum allocation, capital concentration) that are policy choices, not laws of nature. This false summit is a canonical example of naturalization: converting an extractive regime into inevitable progress. The engine's false summit detector will flag this as a misclassification.
constraint_indexing:constraint_classification(smartphone_ubiquity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(smartphone_ubiquity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(smartphone_ubiquity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(smartphone_ubiquity, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(smartphone_ubiquity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(smartphone_ubiquity, TR),
    TR >= 0.70.

:- end_tests(smartphone_ubiquity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The smartphone platform ecosystem captures substantial economic and behavioral surplus: data monetization (valued at hundreds of billions annually), attention capture (average 4+ hours daily), behavioral prediction (used for manipulation and targeting), and mandatory participation in digital economy (workers cannot apply for jobs without smartphone). The extractiveness increased from 0.25 to 0.58 over the interval as smartphone access became prerequisite rather than convenience. The value is not higher (e.g., 0.70+) because some users derive genuine benefit (social connection, information access, coordination), and the constraint operates through apparent choice rather than pure coercion. Suppression (0.68): High. Lock-in mechanisms are substantial: iOS/Android duopoly prevents hardware/OS alternatives; app store gatekeeping prevents software alternatives; behavioral addiction engineering (notifications, social validation loops, infinite scroll) reduces psychological exit capacity; systematic digitalization of essential services (banking, healthcare, employment) makes non-participation economically impossible; lack of interoperability prevents data portability or cross-platform switching. The suppression reflects engineered constraints, not technological necessity. Theater ratio (0.55): Moderate. Real functionality exists — smartphones genuinely enable communication, information access, and coordination — but this is deeply intertwined with and subordinated to extraction mechanisms. The 'place within which we live' framing performs legitimacy work by emphasizing real utility while obscuring extraction. The theater has increased over time (from 0.35 to 0.55) as marketing rhetoric around smartphone utility has intensified even as extraction mechanisms have become more visible (location tracking, algorithm transparency failures, child safety violations). Claimed type: Snare, because (1) extractiveness > 0.46, (2) suppression > 0.60, (3) at least one victim group exists (digital underclass, aware users), and (4) effective extraction χ ≥ 0.66 for the powerless perspective.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates acute perspectival fragmentation. The platform corporation (institutional/arbitrage) experiences coordination — solving the problem of connecting billions across geographies and monetizing scale. The digital underclass (powerless/trapped) experiences pure extraction — systematic exclusion and data expropriation with no exit. The aware user (moderate/constrained) experiences snare — understanding the extraction mechanism but finding no viable alternative. The regulatory coalition (organized/constrained) perceives tangled rope — recognizing both genuine coordination functions and persistent extraction, implementing enforcement that creates compliance theater rather than true decoupling. The discourse of digital inevitability (piton perspective) naturalizes extraction as progress. The civilizational analytical observer encounters a false summit: the temptation to classify smartphone ubiquity as a natural law of technological evolution, but the structural data reveals contingent institutional arrangements (platform dominance, ecosystem lock-in, service digitalization policy) that are policy choices, not technological necessities. The perspectival gap is widest between the platform corporation (rope/coordination) and the digital underclass (snare/extraction): the same constraint is experienced as enabling coordination by the beneficiary and as systematic trap by the victim.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows the beneficiary/victim + exit options framework. The platform corporation (beneficiary + arbitrage exit) derives low d → negative f(d) → experiences negative χ (they benefit). The digital underclass (victim + trapped exit) derives high d → high f(d) → experiences maximum χ (0.95-1.0 effective extraction). The aware user (victim + constrained exit) derives d ≈ 0.65 → f(d) ≈ 1.0 (moderate-high extraction due to understanding without escape). The regulatory coalition (victim of platform power + constrained exit, but organized) derives d ≈ 0.45 → f(d) ≈ 0.55 (reduced experienced extraction due to organizational capacity). No overrides are needed: the structural data correctly produces the observed classification outcomes across perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The smartphone ubiquity constraint resolves mandatrophy by demonstrating that snare classification (from the powerless and constrained perspectives) is legitimate AND that other classification types are legitimate from different positions. The false resolution would be: 'It's either a snare (pure extraction) or a rope (pure coordination) — which is it?' The correct resolution is: 'For whom?' The platform corporation genuinely solves coordination problems and experiences rope. The digital underclass genuinely faces extraction traps and experiences snare. The regulatory coalition faces a hybrid where coordination and extraction are structurally entangled (tangled_rope). The piton perspective correctly identifies that discourse naturalizes contingent arrangements. The mountain perspective is a false summit — a tempting but incorrect analysis that converts policy choices into natural law. The constraint satisfies mandatrophy resolution criteria: (1) beneficiaries and victims are clearly identified with structurally asymmetric relationships; (2) the snare classification is robust at the powerless/trapped perspective but not universal; (3) the perspectival gap is explained by differences in power, exit options, and structural position; (4) each perspective's classification follows consistently from the indexed tuple; (5) alternative analyses (mountain, rope) are shown to be partial or false rather than equally valid. The mandatrophy is resolved through the presheaf structure: snare is correct for one class of observers; the apparent contradiction with rope/mountain/tangled_rope reflects observer position, not classification incoherence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_architectures_sufficiency,
    'Do decentralized smartphone architectures (GrapheneOS, /e/OS, protocol-based alternatives) constitute genuine functional substitutes or merely niche alternatives for security-conscious users?',
    'Adoption metrics: percentage of users choosing privacy-first architectures; functionality parity analysis between privacy-first and mainstream platforms; network effects measurement (whether privacy-first alternatives reach critical mass for universal compatibility)',
    'If genuine substitutes: snare classification is weakened — exit options become mobile rather than trapped/constrained, and suppression drops as platform lock-in becomes optional. If niche only: snare classification is reinforced — mainstream alternatives lack sufficient scale to constitute real exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_architectures_sufficiency, empirical, 'Whether decentralized smartphone architectures constitute viable alternatives').

omega_variable(
    mandatory_smartphone_dependence_threshold,
    'At what level of essential service digitalization does smartphone access become a prerequisite for civic/economic participation versus a convenience multiplier?',
    'Comparative analysis of required services delivery: healthcare (telehealth mandates), employment (online-only application systems), finance (mobile banking), governance (digital ID, tax filing); measurement of exclusion rates for non-smartphone users across sectors',
    'If threshold exceeded in majority of sectors: suppression increases to 0.75+, extractiveness remains high, snare classification is robust. If threshold not yet reached: suppression is lower (0.55-0.60), constraint is tangled_rope from more perspectives, regulatory escape routes exist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatory_smartphone_dependence_threshold, empirical, 'Threshold at which smartphone access becomes mandatory versus convenience').

omega_variable(
    platform_ecosystem_modularity_boundary,
    'Can smartphones as hardware/OS layer be decoupled from platform ecosystem (app stores, social networks, surveillance backends) or is the bundling architecturally necessary?',
    'Technical analysis of platform lock-in mechanisms: app sandboxing requirements, API restrictions, data portability barriers; comparison with historically unbundled technologies (PC hardware/software split); feasibility study of interoperable platform standards',
    'If decoupling is technically feasible: the smartphone hardware constraint (might be rope/mountain) can be separated from the platform ubiquity constraint (currently snare). If architectural bundling is necessary: snare classification persists because escape from platform extraction requires escape from smartphone access itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_ecosystem_modularity_boundary, empirical, 'Whether platforms can be decoupled from smartphone hardware').

omega_variable(
    regulatory_effectiveness_on_extraction,
    'Do regulatory constraints (GDPR, DMA, child safety regulations) reduce actual extraction or merely create compliance theater?',
    'Longitudinal measurement of data collection practices pre/post-regulation; analysis of regulatory loopholes (consent mechanisms, dark patterns, legitimate interest claims); measurement of platform revenue concentration post-regulation',
    'If effective: tangled_rope classification is robust, regulatory coalition''s organized power genuinely constrains extraction, theater_ratio drops. If theater only: snare classification is reinforced, regulatory perspective collapses back to rope (beneficiary coordination), extraction persists through compliance theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_effectiveness_on_extraction, empirical, 'Whether regulations reduce extraction or create compliance theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(smartphone_ubiquity, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smartphone_tr_t0, smartphone_ubiquity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(smartphone_tr_t8, smartphone_ubiquity, theater_ratio, 8, 0.45).
narrative_ontology:measurement(smartphone_tr_t16, smartphone_ubiquity, theater_ratio, 16, 0.55).

% Extraction over time
narrative_ontology:measurement(smartphone_be_t0, smartphone_ubiquity, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(smartphone_be_t8, smartphone_ubiquity, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(smartphone_be_t16, smartphone_ubiquity, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(smartphone_ubiquity, global_infrastructure).
narrative_ontology:boltzmann_floor_override(smartphone_ubiquity, 0.35).
narrative_ontology:affects_constraint(smartphone_ubiquity, digital_financial_inclusion).
narrative_ontology:affects_constraint(smartphone_ubiquity, algorithmic_attention_capture).
narrative_ontology:affects_constraint(smartphone_ubiquity, surveillance_capitalism).
narrative_ontology:affects_constraint(smartphone_ubiquity, digital_divide_expansion).
narrative_ontology:affects_constraint(smartphone_ubiquity, platform_ecosystem_lock_in).

% DUAL FORMULATION NOTE:
% The smartphone ubiquity constraint is composed of multiple nested constraints that should be decomposed for precise analysis: (1) hardware ubiquity (iOS/Android duopoly) — primarily a market concentration / snare at moderate intensity; (2) platform ecosystem lock-in (app stores, data silos) — extraction mechanism at high intensity; (3) service digitalization (mandatory app-based access to banking, healthcare, employment) — compression mechanism (externality-driven dependency) at high intensity; (4) behavioral addiction engineering (notification systems, infinite scroll, social validation loops) — attention extraction at high intensity. This story models the joint constraint at the system level. Precise analysis would decompose into four separate stories linked via affects_constraints, with distinct ε values for each layer. Current 0.58 ε represents the blended effect. Upstream stories (hardware duopoly ε ≈ 0.35-0.40, piton degradation) influence downstream (service digitalization ε ≈ 0.65, stronger snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(smartphone_ubiquity, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
