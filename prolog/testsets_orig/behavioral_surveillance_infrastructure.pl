% ============================================================================
% CONSTRAINT STORY: behavioral_surveillance_infrastructure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_behavioral_surveillance_infrastructure, []).

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
 *   constraint_id: behavioral_surveillance_infrastructure
 *   human_readable: Behavioral Surveillance Infrastructure as Extractive Constraint
 *   domain: digital_control/governance
 *
 * SUMMARY:
 *   Behavioral surveillance infrastructure represents one of the largest
 *   extractive constraints operating at global scale. The constraint
 *   encompasses the technological systems, economic incentives, regulatory
 *   frameworks, and cultural norms that enable continuous monitoring of human
 *   behavior across digital, financial, physical, and social domains. The
 *   extractiveness has accelerated over the past decade as mobile devices,
 *   internet-of-things networks, and algorithmic processing have made
 *   behavioral data collection ubiquitous and valuable. Early digitization
 *   (2010-2015) operated with lower extractiveness because data monetization
 *   was immature and users were less aware of tracking. Subsequent
 *   consolidation (2015-2020) increased extraction as platforms perfected
 *   behavioral profiling and targeting. Recent developments (2020-2025) have
 *   driven extractiveness further as surveillance integrated with state
 *   apparatus, health monitoring, and financial systems. The theater_ratio
 *   has also risen: privacy regulations, data protection laws, and consent
 *   frameworks create the appearance of user control while actual behavioral
 *   extraction continues at scale. The constraint exhibits all six
 *   classification types from different perspectives, with the analytical
 *   view converging on Tangled Rope — the infrastructure genuinely
 *   coordinates digital communication and enables services, but the
 *   extraction is severe enough that the coordination function no longer
 *   justifies the overhead.
 *
 * KEY AGENTS:
 *   - Surveilled Population: Primary victim (powerless/trapped) — inhabitants of digital infrastructure with no viable exit. Behavioral autonomy, attention, and conformity are extracted continuously. No coordination benefit compensates.
 *   - Surveillance Platform Operators: Primary beneficiary (institutional/arbitrage) — Meta, Google, TikTok, Amazon. Monetize behavioral data at scale; can shift business models or regulatory regimes; experience the constraint as essential coordination mechanism.
 *   - Data Monetization Firms: Beneficiary (institutional/arbitrage) — data brokers, ad networks, hedge funds using behavioral data for market advantage. Arbitrage options to different markets and regulatory regimes.
 *   - State Security Apparatus: Beneficiary (institutional/arbitrage) — national security, law enforcement, public health systems using surveillance for coordination. Can shape regulatory frameworks; experiences extraction as legitimate authority.
 *   - Behavioral Autonomy (abstract collective): Victim (powerless/trapped) — abstract collective good of human agency, dignity, and freedom. Cannot organize or exit. Bears full cost of behavioral manipulation, conformity pressure, and psychological harm.
 *   - Privacy Regulators: Institutional actor (institutional/constrained) — GDPR, CCPA, data protection authorities. Created to constrain surveillance but increasingly captured by platforms through lobbying and revolving-door employment. Theater-maintaining actors.
 *   - Organized Privacy Advocates: Secondary victim (organized/constrained) — encryption advocates, decentralization movements, privacy-first technologists. Technically skilled and ideologically committed but constrained by network effects and resource asymmetries. Building alternatives (Signal, Tor, Matrix) that operate in the shadow of dominant platforms.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as Tangled Rope: genuine coordination function alongside severe extraction. Acknowledges that surveillance infrastructure cannot be simply eliminated without losing coordination benefits, but also cannot continue at current extraction rates without eroding human agency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(behavioral_surveillance_infrastructure, 0.68).
domain_priors:suppression_score(behavioral_surveillance_infrastructure, 0.72).
domain_priors:theater_ratio(behavioral_surveillance_infrastructure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(behavioral_surveillance_infrastructure, extractiveness, 0.68).
narrative_ontology:constraint_metric(behavioral_surveillance_infrastructure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(behavioral_surveillance_infrastructure, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(behavioral_surveillance_infrastructure, snare).
narrative_ontology:human_readable(behavioral_surveillance_infrastructure, "Behavioral Surveillance Infrastructure as Extractive Constraint").
narrative_ontology:topic_domain(behavioral_surveillance_infrastructure, "digital_control/governance").

domain_priors:requires_active_enforcement(behavioral_surveillance_infrastructure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(behavioral_surveillance_infrastructure, surveillance_platform_operators).
narrative_ontology:constraint_beneficiary(behavioral_surveillance_infrastructure, data_monetization_firms).
narrative_ontology:constraint_beneficiary(behavioral_surveillance_infrastructure, behavioral_targeting_advertisers).
narrative_ontology:constraint_beneficiary(behavioral_surveillance_infrastructure, state_security_apparatus).
narrative_ontology:constraint_victim(behavioral_surveillance_infrastructure, surveilled_population).
narrative_ontology:constraint_victim(behavioral_surveillance_infrastructure, behavioral_autonomy).
narrative_ontology:constraint_victim(behavioral_surveillance_infrastructure, informational_privacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SURVEILLED INDIVIDUAL (SNARE) — Trapped by ubiquitous digital infrastructure. Exit from surveillance requires abandoning participation in digital communication, financial systems, transportation, employment, and social coordination. The cost of exit is social death. Suppression is maximal: every device, every platform, every transaction is a surveillance point. No legitimate coordination function benefits this agent. Pure extraction of behavioral data, attention, and conformity.
constraint_indexing:constraint_classification(behavioral_surveillance_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PLATFORM USER WITH EXIT CAPACITY (TANGLED ROPE) — Technically mobile but constrained by network effects and coordination value. Can switch platforms, use privacy tools, limit data sharing — but at high cost to social connectivity and economic participation. Genuine coordination function exists: platforms do enable communication and community. But surveillance enables extraction of behavioral data worth far more to operators than service value provided to users. Mixed: significant coordination benefit with asymmetric extraction layered on top.
constraint_indexing:constraint_classification(behavioral_surveillance_infrastructure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SURVEILLANCE PLATFORM OPERATOR (ROPE) — Experiences the surveillance infrastructure as pure coordination. The platform solves the collective action problem of how to connect billions of people and monetize attention. From this perspective, behavioral data extraction is the payment mechanism that makes the service possible — without it, the platform cannot fund itself. Operator has full arbitrage optionality: can exit to different business models, regulatory regimes, or markets. Extraction runs toward this agent as beneficiary.
constraint_indexing:constraint_classification(behavioral_surveillance_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ORGANIZED RESISTANCE (SNARE) — Privacy activists, encryption advocates, and decentralization movements are organized but face suppression. Their exit path (build decentralized alternatives, enforce strong privacy) is constrained by network effects and resource asymmetries. Even successful alternatives (Signal, Tor, Matrix) operate in the shadow of the dominant centralized platforms. The constraint on this agent is not just surveillance but the structural inability to build competing infrastructure at scale.
constraint_indexing:constraint_classification(behavioral_surveillance_infrastructure, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE SECURITY APPARATUS (ROPE) — Experiences surveillance infrastructure as essential coordination mechanism for security, public health, and law enforcement. From this perspective, behavioral data enables coordination at scale: identifying threats, tracking disease vectors, enforcing law. The state has arbitrage options and can shape regulatory frameworks to its benefit. For the state, extraction is experienced as legitimate authority, not coercive overhead.
constraint_indexing:constraint_classification(behavioral_surveillance_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE PRIVACY REGULATOR (PITON) — Institutional actor created to solve the surveillance problem but increasingly captures by the platforms it regulates. GDPR, CCPA, and similar frameworks create the *appearance* of control without substantive constraint on extraction. Theater_ratio is high: privacy policies, consent mechanisms, and data breach notifications are performative — they create the illusion of user agency while data flows continue unabated. Regulator has become inert machinery maintaining the fiction of privacy protection while surveillance scales.
constraint_indexing:constraint_classification(behavioral_surveillance_infrastructure, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scale, behavioral surveillance infrastructure exhibits genuine coordination function (connection, information access, emergency response coordination) alongside severe extraction (behavioral manipulation, autonomy erosion, inequality amplification). The engine classifies this as Tangled Rope: χ is high, but the constraint serves both coordination and extraction simultaneously. This is the structurally honest classification — acknowledging that the infrastructure is neither pure evil nor pure good, but a hybrid mechanism where extraction is enabled by coordination.
constraint_indexing:constraint_classification(behavioral_surveillance_infrastructure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(behavioral_surveillance_infrastructure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(behavioral_surveillance_infrastructure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(behavioral_surveillance_infrastructure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(behavioral_surveillance_infrastructure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(behavioral_surveillance_infrastructure, TR),
    TR >= 0.70.

:- end_tests(behavioral_surveillance_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts behavioral data with economic value to operators far exceeding the service value provided to users. The extraction is measured in multiple currencies — attention (advertising engagement), behavioral conformity (algorithmic optimization), financial value (data sales), and political influence (state cooperation). Over the measurement interval, extractiveness has nearly doubled as platforms perfected behavioral profiling and data monetization. Suppression (0.72): Very high. The mechanisms preventing exit are overwhelming. Digital infrastructure is unavoidable for employment, financial participation, social coordination, and public services. Every interaction leaves behavioral traces. Data collection is largely invisible (hidden in terms of service, executed in background processes). Alternative platforms lack network effects and market power. Privacy tools are technically complex and commercially disadvantaged. Legal protections are inadequate and regulatory enforcement is weak. Suppression is not violent coercion but structural inevitability — escape requires abandoning modern life. Theater ratio (0.58): Moderate-high and increasing. Privacy regulations create the appearance of user control (consent mechanisms, privacy policies, data deletion rights) without substantive constraint on extraction. GDPR transparency and CCPA choice provisions have increased regulatory theater without meaningfully reducing data flows. The performative layer (privacy policies reach 400+ pages; consent dialogs are cognitive dark patterns; data breach notifications are routine and consequence-free) has grown faster than actual protection. Theater has increased from 0.25 to 0.58 over the measurement interval as regulatory theater scaled.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap appears between the surveilled individual (powerless/trapped → Snare) and the platform operator (institutional/arbitrage → Rope). The individual sees constraint; the operator sees coordination. This gap is not a measurement error or interpretive ambiguity — it reflects genuine structural asymmetry. Both perspectives are analytically accurate from their respective positions. The gap reveals the core mandate-trophy: the constraint is simultaneously a valid coordination mechanism (connection, information access, emergency response) AND a severe extraction mechanism (behavioral data theft, attention mining, autonomy erosion). The tension cannot be resolved by choosing one perspective as correct — both are structurally true. Resolution requires either (a) restructuring the coordination function to eliminate the extraction (decentralized alternatives), (b) accepting the extraction as the price of coordination (regulatory acceptance), or (c) rejecting the coordination and its extraction simultaneously (technological defection). Current policy attempts (a) without success because decentralized alternatives lack network effects, maintains the fiction of (b) through regulatory theater, and does not seriously pursue (c) because coordination benefits are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural position relative to the extraction mechanism. The surveilled individual occupies d ≈ 0.95 — nearly full target. They are heavily victim-designated, trapped with no exit, and experience maximal extraction. Platform operators occupy d ≈ 0.05 — nearly full beneficiary. They are heavily beneficiary-designated, have arbitrage options, and extract from the system. Moderate users with exit capacity occupy d ≈ 0.60 — mixed. They benefit from platform coordination but bear surveillance costs and face high-cost exit. Regulatory actors face d ≈ 0.50 — symmetric. They are neither beneficiary nor target but captured by beneficiaries, experiencing mixed incentives. Privacy advocates face d ≈ 0.70 — moderately high. They are nominally part of the surveilled population but have specialized knowledge (exit options) and ideological commitment. The state apparatus occupies d ≈ 0.20 — beneficiary-skewed. It experiences surveillance as legitimate coordination authority and has exit options (can change regulatory regimes). These d values are not fixed — they vary by agent and time. As surveillance consolidates, constrained agents shift toward trapped (d increases). As privacy-preserving alternatives scale, exit costs decline (d decreases).
 *
 * MANDATROPHY ANALYSIS:
 *   The behavioral surveillance constraint exhibits mandatrophy at high severity (extractiveness 0.68). The mandatrophy is not 'is this extraction or coordination?' but 'how do we restructure coordination to eliminate the extraction?' The constraint serves genuine coordination functions: connecting people, enabling emergency response, funding digital services. But it extracts far more value than coordination requires. Current regulatory attempts resolve mandatrophy through theater (GDPR creates consent fiction, CCPA creates choice fiction) without addressing the underlying extraction. The analytical perspective (Tangled Rope) is the honest assessment: the infrastructure coordinates AND extracts. The mandatrophy resolves by acknowledging that the current operational form is indefensible — either the infrastructure must be restructured to separate coordination from extraction (decentralization, public ownership, non-profit models) or the extraction must be bounded through structural limits (data minimization, federated architectures, transparency with teeth). Theater-based regulation cannot resolve mandatrophy because it does not address the economic incentive structure driving extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_fiction_vs_structural_inevitability,
    'Is surveillance extraction avoidable through user consent and choice, or is it structurally inevitable given the economics of digital platforms?',
    'Empirical: test whether users with maximal privacy settings (opt-outs enabled, data deletion requested, encryption used) achieve materially lower extraction rates. Conceptual: whether any digital coordination at scale can function without behavioral data collection.',
    'If avoidable: regulation focusing on consent and transparency is appropriate. If inevitable: the constraint cannot be negotiated away — only structural alternatives (decentralization, public infrastructure, non-profit platforms) can escape it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_fiction_vs_structural_inevitability, empirical, 'Whether surveillance is avoidable through consent or structurally inevitable').

omega_variable(
    alternative_coordination_viability,
    'Can decentralized, privacy-preserving alternatives (Signal, Matrix, Mastodon, blockchain social graphs) scale to coordinate billions of users with equivalent functionality to centralized surveillance platforms?',
    'Empirical: comparative analysis of decentralized platform adoption rates, network effects, feature parity, and resource costs. Historical: analysis of past platform shifts (MySpace to Facebook, Slack to Teams).',
    'If viable: scaffold perspective applies — exit pathway exists and sunset is real. If not viable: surveilled population remains trapped; alternative is purely aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_viability, empirical, 'Whether privacy-preserving alternatives can scale to replace surveillance platforms').

omega_variable(
    identity_locked_vs_trapped_bindings,
    'To what extent is the surveilled population''s immobility due to trapped structural conditions (no viable alternatives, economic dependency) versus identity-locked psychological bindings (social identity fused with platform participation, internalized inevitability)?',
    'Qualitative: interview studies of exit intentions vs. actual behavior. Behavioral: sudden availability of truly equivalent alternative platform, tracking adoption rates and churn from dominant platforms.',
    'If trapped: material policy (breaking network effects, funding alternatives) is necessary. If identity-locked: information and norm-shifting campaigns may shift perception and enable exit. Most likely: both mechanisms bind simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_trapped_bindings, empirical, 'Whether immobility is structural or internalized').

omega_variable(
    state_vs_corporate_extraction_asymmetry,
    'Does state surveillance extraction differ structurally from corporate behavioral extraction, or are they convergent mechanisms with different administrative systems?',
    'Comparative: analyze state surveillance regimes (China, Russia) and corporate platforms (Meta, Google) for behavioral control mechanisms. Examine whether corporate platforms voluntarily adopt state-like extraction or resist.',
    'If different: regulation can separate corporate and state surveillance. If convergent: surveillance infrastructure itself is the constraint, regardless of administrator.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_vs_corporate_extraction_asymmetry, conceptual, 'Whether state and corporate extraction are structurally distinct').

omega_variable(
    extraction_quantification_uncertainty,
    'What is the true economic value of extracted behavioral data, and how does it compare to the service value provided to users?',
    'Empirical: market analysis of data broker pricing, advertising effectiveness studies, user willingness-to-pay for privacy. Accounting: full-cost analysis including externalities (psychological harm, polarization, autonomy erosion).',
    'If extraction value >> service value: justifies snare classification. If comparable: tangled rope with genuine coordination function is more accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_quantification_uncertainty, empirical, 'Quantifying true extraction value versus service provision').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(behavioral_surveillance_infrastructure, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bsi_tr_t0, behavioral_surveillance_infrastructure, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bsi_tr_t5, behavioral_surveillance_infrastructure, theater_ratio, 5, 0.42).
narrative_ontology:measurement(bsi_tr_t10, behavioral_surveillance_infrastructure, theater_ratio, 10, 0.58).
narrative_ontology:measurement(bsi_tr_t15, behavioral_surveillance_infrastructure, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(bsi_be_t0, behavioral_surveillance_infrastructure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bsi_be_t5, behavioral_surveillance_infrastructure, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(bsi_be_t10, behavioral_surveillance_infrastructure, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(bsi_be_t15, behavioral_surveillance_infrastructure, base_extractiveness, 15, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(behavioral_surveillance_infrastructure, global_infrastructure).
narrative_ontology:affects_constraint(behavioral_surveillance_infrastructure, algorithmic_filter_bubbles).
narrative_ontology:affects_constraint(behavioral_surveillance_infrastructure, attention_market_monopolies).
narrative_ontology:affects_constraint(behavioral_surveillance_infrastructure, behavioral_manipulation_at_scale).
narrative_ontology:affects_constraint(behavioral_surveillance_infrastructure, digital_identity_asymmetry).

% DUAL FORMULATION NOTE:
% Behavioral surveillance infrastructure is upstream of multiple extractive constraints in digital domains. Algorithmic filter bubbles depend on behavioral data; attention market monopolies depend on surveillance-enabled targeting; behavioral manipulation depends on behavioral profiles; digital identity asymmetry depends on centralized control of identity data. Each downstream constraint has its own ε value reflecting domain-specific extraction rates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(behavioral_surveillance_infrastructure, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
