% ============================================================================
% CONSTRAINT STORY: consumer_data_aggregation_oligopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consumer_data_aggregation_oligopoly, []).

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
 *   constraint_id: consumer_data_aggregation_oligopoly
 *   human_readable: Consumer Data Aggregation Oligopoly
 *   domain: digital_economy/platform_governance
 *
 * SUMMARY:
 *   The consumer data aggregation oligopoly represents a structurally pure
 *   extraction mechanism masked by coordination framing. A small number of
 *   platforms (Google, Meta, Amazon, Apple, and regional equivalents) have
 *   consolidated control over consumer behavioral data through network
 *   effects, switching costs, and lack of viable alternatives. This
 *   constraint exhibits multiple DR types across perspectives, revealing the
 *   diagnosis: it is a snare from the consumer's position, a rope from the
 *   platform's position, a piton from the regulatory ritual position, and a
 *   scaffold from the decentralization movement's position. The constraint's
 *   extractiveness has increased over 15 years as data collection depth has
 *   expanded (from behavioral to locational to biometric to social graph),
 *   regulatory theater has proliferated without material enforcement impact,
 *   and consumers have shifted from active opt-in to passive acceptance. The
 *   theater ratio reflects that privacy policies, cookie consents, and GDPR
 *   compliance mechanisms create a performative appearance of consumer
 *   control while actual data extraction accelerates.
 *
 * KEY AGENTS:
 *   - Consumers: Primary victims (powerless/trapped → identity_locked) — face total data extraction with no material exit; younger cohorts are identity-locked to platform participation
 *   - Data Aggregation Platforms: Primary beneficiaries (institutional/arbitrage) — capture extraction through network effects and switching costs; reframe extraction as coordination benefit
 *   - Small Competitors: Secondary victims (powerful/constrained) — cannot compete without platform data access; extraction is coordination cost with limited benefit
 *   - Advertisers and Financial Institutions: Secondary beneficiaries (organized/mobile) — benefit from targeting and risk assessment powered by aggregated data; have exit optionality but prefer aggregation
 *   - Regulators: Organized actors (organized/mobile) — simultaneously benefit from infrastructure and bear costs of rights protection; enforcement constrained by lobbying and cross-border coordination failures
 *   - Consent Theater System: Institutional inertia (institutional/arbitrage) — privacy policies, cookie banners, and compliance checkboxes persist despite non-functionality; primary function (informed consent) has atrophied
 *   - Data Cooperatives and Federation Advocates: Organized actors (organized/constrained) → analytical observer (analytical/analytical) — building scaffold structures with sunset logic; currently low market share but structurally viable alternatives
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices as information physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consumer_data_aggregation_oligopoly, 0.58).
domain_priors:suppression_score(consumer_data_aggregation_oligopoly, 0.68).
domain_priors:theater_ratio(consumer_data_aggregation_oligopoly, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consumer_data_aggregation_oligopoly, extractiveness, 0.58).
narrative_ontology:constraint_metric(consumer_data_aggregation_oligopoly, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(consumer_data_aggregation_oligopoly, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consumer_data_aggregation_oligopoly, snare).
narrative_ontology:human_readable(consumer_data_aggregation_oligopoly, "Consumer Data Aggregation Oligopoly").
narrative_ontology:topic_domain(consumer_data_aggregation_oligopoly, "digital_economy/platform_governance").

domain_priors:requires_active_enforcement(consumer_data_aggregation_oligopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consumer_data_aggregation_oligopoly, data_aggregation_platforms).
narrative_ontology:constraint_beneficiary(consumer_data_aggregation_oligopoly, advertisers).
narrative_ontology:constraint_beneficiary(consumer_data_aggregation_oligopoly, financial_institutions).
narrative_ontology:constraint_victim(consumer_data_aggregation_oligopoly, consumers).
narrative_ontology:constraint_victim(consumer_data_aggregation_oligopoly, small_competitors).
narrative_ontology:constraint_victim(consumer_data_aggregation_oligopoly, data_privacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSUMER (SNARE) — Consumers face total data extraction with minimal exit options. Participation in digital services requires data surrender. Privacy policies are incomprehensible cover for data sales. Withdrawal from digital platforms incurs catastrophic life costs (banking, employment, social access). Trapped at maximum structural extraction.
constraint_indexing:constraint_classification(consumer_data_aggregation_oligopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IDENTITY-LOCKED CONSUMER (SNARE) — Younger cohorts whose identity formation occurs within the platforms. The constraint is not just material extraction but epistemic — consent is fused with identity. The consumer cannot imagine privacy as a category or alternatives to surveillance as normal. Data surrender is identity-constituted, not just economically coerced.
constraint_indexing:constraint_classification(consumer_data_aggregation_oligopoly, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: DATA AGGREGATION PLATFORM (ROPE) — Experiences the constraint as pure coordination: collecting and distributing consumer signals enables matching between advertisers, lenders, and consumers. The platform sees genuine collective action benefit. Extraction is reframed as 'connecting consumers with relevant offers.' Net beneficiary with exit optionality.
constraint_indexing:constraint_classification(consumer_data_aggregation_oligopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SMALL COMPETITOR (TANGLED ROPE) — Cannot compete without access to aggregated consumer data. The oligopoly constrains market entry but also enables coordination (cannot do business outside the data platforms). High extraction cost, some access benefit, exit is theoretically mobile but practically constrained by network effects and switching costs.
constraint_indexing:constraint_classification(consumer_data_aggregation_oligopoly, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY COALITION (TANGLED ROPE) — Data protection regulators (GDPR authorities, FTC, national privacy commissioners) simultaneously benefit from data aggregation infrastructure (monitoring compliance, tax enforcement, security) and bear costs (defending privacy rights against platform extraction). Enforcement is active; exit from coordination is structurally possible but politically costly.
constraint_indexing:constraint_classification(consumer_data_aggregation_oligopoly, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSENT THEATER SYSTEM (PITON) — Privacy policies, cookie consents, and data-use disclosures are degraded institutional ritual. The theater persists despite dysfunction: consumers do not read or understand policies, meaningful consent is impossible, regulatory compliance occurs through checkbox theatre rather than genuine protection. Primary function (informed consent) has atrophied; constraint remains through institutional inertia and legal theater.
constraint_indexing:constraint_classification(consumer_data_aggregation_oligopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: DATA COOPERATIVES / DECENTRALIZATION (SCAFFOLD) — Emerging alternatives (data cooperatives, personal data stores, federation protocols, self-sovereign identity) represent temporary support structures that could enable exit from platform oligopoly. These mechanisms have explicit sunset logic: as they mature and scale, centralized data aggregation's extractive advantage declines. Current constraints are high (bootstrap phase); sunset timeframe is 15-30 years for sufficient maturity and adoption.
constraint_indexing:constraint_classification(consumer_data_aggregation_oligopoly, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / DATA PHYSICS (MOUNTAIN) — From a civilizational/universal perspective, information asymmetry about consumer behavior is inherent: aggregators necessarily know more about aggregate patterns than individuals know about themselves. This perspective naturalizes data oligopoly as an immutable law of information physics. However, the structural data contradicts the mountain classification — the engine will detect this as a false summit, revealing that 'information asymmetry is natural' masks a contingent institutional choice to concentrate data extraction rather than distribute it.
constraint_indexing:constraint_classification(consumer_data_aggregation_oligopoly, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consumer_data_aggregation_oligopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consumer_data_aggregation_oligopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consumer_data_aggregation_oligopoly, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consumer_data_aggregation_oligopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consumer_data_aggregation_oligopoly, TR),
    TR >= 0.70.

:- end_tests(consumer_data_aggregation_oligopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. Platforms extract significant behavioral, locational, and inferential data from consumers with minimal compensation. The measurement progression (0.35→0.48→0.58 over 15 years) reflects deepening data collection (from click behavior to biometric inference to social graph) and expanding monetization vectors (advertising→credit→health predictions). Early-period extraction was lower because data collection was narrower and consumers had more plausible deniability about data use. Suppression (0.68): High. Consumers face multiple suppression mechanisms: structural (no viable digital alternative platforms), epistemic (incomprehensible privacy policies), and identity-based (younger cohorts don't know non-surveillance digital world exists). Switching costs are astronomical for consumers deeply embedded in platform ecosystems. Theater ratio (0.55): Moderate-high. Regulatory theater (GDPR compliance, cookie consent, data subject access requests) creates performative control mechanisms while actual extraction continues. However, theater is not maximal (0.72+) because some real friction exists — GDPR enforcement is selective but real, some consumers do exercise rights, some alternatives are emerging. The theater has increased over the interval as regulatory compliance mechanisms proliferated without corresponding enforcement intensity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival divergence is driven entirely by beneficiary/victim status and exit options. The platform perspective (institutional/arbitrage/beneficiary) sees coordination; the consumer perspective (powerless/trapped/victim) sees extraction. These are not measurement differences — they reflect genuine structural asymmetry in how costs and benefits distribute. The identity-locked consumer's perspective (powerless/identity_locked/victim) is critical: it shows that even at biographical time horizon, constraints on identity-constituted agents may appear as ropes (the agent perceives changeability) while trapped agents perceive mountains (immutability). Here, the identity-locked consumer still classifies as snare because the constraint is so severe that even identity-fused agents recognize extraction, but their pathway to exit is cognitive frame-breaking, not structural barrier removal. The scaffold perspective (organized/constrained) reveals that the snare is not immutable — organized actors with some mobility can build alternatives — but the bootstrap phase is long (15-30 years) and current extraction is high.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the beneficiary/victim declarations and exit options. Data aggregation platforms are beneficiaries with arbitrage exit → low d → negative experienced extraction. Consumers are victims with trapped/identity_locked exit → high d → high f(d) → high χ. Regulators are both beneficiaries (from data infrastructure) and victims (of pressure to permit extraction) → moderate d. Small competitors are victims with constrained exit → high d but with some coordination benefit → moderate-high χ. The identity-locked consumer's directionality differs from the trapped consumer: trapped exit + victim status drives d≈0.95, while identity_locked exit + victim status with some structural mobility drives d≈0.85. The sigmoid f(d) transforms these into experienced extractiveness values, accounting for the agent's power level and temporal horizon. Institutional beneficiaries (platforms, advertisers) have canonical d values that produce negative χ (they experience subsidization). Powerless victims at global scope experience maximum χ (0.66-1.42 range).
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE RESOLUTION: This constraint avoids mandatrophy (false labeling of extraction as coordination) through the victim/beneficiary asymmetry. The platform's coordination narrative (connecting consumers with ads) is genuine as inter-platform coordination — the extraction occurs in the asymmetry between platform benefit and consumer cost. The GDPR and privacy regulation create the appearance of mandatrophy resolution (are we regulating extraction or coordinating data use?) but actually perpetuate it — regulatory compliance becomes a performance that domesticates extraction rather than eliminates it. The constraint resolves as pure snare because: (1) genuine coordination is available only to beneficiary-side agents (platforms and advertisers), (2) consumers have no real coordination role — they do not consensually provide data, they are prevented from opting out, (3) regulatory theater masks extraction without preventing it. The analytical observer who sees this as immutable information physics (mountain perspective) is committing false naturalization — the constraint is snare from the powerless perspective, which means it is imposed rather than inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_authenticity_threshold,
    'At what point does privacy policy complexity become a suppression mechanism rather than a disclosure mechanism?',
    'Readability analysis of actual policies; eye-tracking studies of user engagement with consent interfaces; comparison of policy complexity trends against regulatory requirements',
    'If suppression is intentional: classification stands as snare. If policies approach authentic disclosure: classification shifts toward tangled_rope with lower suppression component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_authenticity_threshold, empirical, 'Whether privacy policies function as disclosure or suppression').

omega_variable(
    identity_lock_penetration,
    'What fraction of consumer base is identity-locked (unable to imagine alternatives) versus materially trapped (unable to afford exit)?',
    'Qualitative research on consumer perception of data sharing necessity; generational cohort analysis; correlation between age/digital nativity and exit perception',
    'If high identity-lock: constraint''s intergenerational persistence is stronger; may require identity frame disruption rather than just policy change. If low identity-lock: most consumers are materially trapped; removal of material barriers could enable exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_penetration, empirical, 'Relative contribution of identity-lock versus material entrapment').

omega_variable(
    network_effect_irreversibility,
    'How much of the oligopoly''s moat derives from genuine network effects (more users = more valuable) versus switching costs and coordination failure (could disperse but doesn''t)?',
    'Analysis of alternative platforms (Mastodon, Signal, DuckDuckGo adoption curves); historical platform shifts (MySpace→Facebook); cost-benefit models of consumer switching in scenarios with coordinated migration',
    'If network effects are weak: constraint is coordination failure (Rope), not structural oligopoly (Snare). If network effects are strong: moat is deep; cooperative/federated alternatives face structural disadvantage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_irreversibility, empirical, 'Whether oligopoly moat is genuine network effects or coordination failure').

omega_variable(
    regulatory_capture_depth,
    'Does data regulation (GDPR, CCPA) serve to domesticate oligopoly extraction rather than meaningfully constrain it?',
    'Comparative analysis of enforcement intensity; correlation between regulation passage and actual consumer data control; assessment of whether regulations set compliance baseline that oligopolies internalize',
    'If regulation is captured: coalface perspective is false — regulatory enforcement perpetuates snare. If regulation is genuine constraint: coalface perspective is accurate — exit options exist at regulatory level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Whether regulation constrains oligopoly or captures regulation').

omega_variable(
    alternative_platform_viability,
    'Can federation protocols, data cooperatives, and personal data stores provide sufficient functionality and adoption to displace centralized aggregation within the scaffold timeframe?',
    'Technical feasibility studies of federated identity and data infrastructure; adoption curve modeling for decentralized alternatives; user experience parity assessment against centralized incumbents',
    'If viable: scaffold perspective is structural — sunset is real and timeline is realistic. If unviable: scaffold is aspirational; constraint persists as snare with no exit mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Whether decentralized alternatives are technically and economically viable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consumer_data_aggregation_oligopoly, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cdao_tr_t0, consumer_data_aggregation_oligopoly, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cdao_tr_t5, consumer_data_aggregation_oligopoly, theater_ratio, 5, 0.42).
narrative_ontology:measurement(cdao_tr_t10, consumer_data_aggregation_oligopoly, theater_ratio, 10, 0.55).
narrative_ontology:measurement(cdao_tr_t15, consumer_data_aggregation_oligopoly, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(cdao_be_t0, consumer_data_aggregation_oligopoly, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cdao_be_t5, consumer_data_aggregation_oligopoly, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cdao_be_t10, consumer_data_aggregation_oligopoly, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(cdao_be_t15, consumer_data_aggregation_oligopoly, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consumer_data_aggregation_oligopoly, resource_allocation).
narrative_ontology:affects_constraint(consumer_data_aggregation_oligopoly, digital_surveillance_infrastructure).
narrative_ontology:affects_constraint(consumer_data_aggregation_oligopoly, algorithmic_credit_scoring).
narrative_ontology:affects_constraint(consumer_data_aggregation_oligopoly, behavioral_advertising_targeting).

% DUAL FORMULATION NOTE:
% Consumer data aggregation is the upstream constraint enabling downstream extraction in credit scoring, advertising targeting, and behavioral prediction markets. The oligopoly structure (high concentration, network effects, switching costs) creates the data concentration. The measurement progression shows both deepening extraction (extractiveness rising) and increasing theater (consent mechanisms proliferating without impact). This constraint family spans economic extraction (pricing/targeting), epistemic extraction (behavioral inference), and identity extraction (social graph commodification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(consumer_data_aggregation_oligopoly, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
