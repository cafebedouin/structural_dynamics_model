% ============================================================================
% CONSTRAINT STORY: consent_fatigue_externality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consent_fatigue_externality, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: consent_fatigue_externality
 *   human_readable: Consent Fatigue as Privacy Law Externality
 *   domain: technology_governance/privacy_law/innovation_policy
 *
 * SUMMARY:
 *   Consent fatigue emerges as an unintended externality of the GDPR's
 *   consent-based privacy framework. Users face mandatory consent requests
 *   across dozens of platforms and services daily, each requiring active
 *   engagement. The cognitive load of processing repeated consent notices
 *   produces decision fatigue, reducing the quality of user choice.
 *   Simultaneously, data controllers have engineered dark patterns (cookie
 *   walls, pre-ticked boxes, manipulative UI) that exploit this fatigue to
 *   capture user permissions that would be rejected under conditions of full
 *   attention. The constraint exhibits tangled structure: GDPR consent
 *   mechanisms serve genuine coordination functions (enable data-sharing
 *   partnerships, allow users to opt out of processing they reject) while
 *   simultaneously enabling extraction (platforms capture value from
 *   fatigue-driven acceptance). The theater ratio has risen over the interval
 *   as consent becomes increasingly performative: users mechanically accept
 *   consent to access services, rendering the consent meaningless as a signal
 *   of preference. Suppression has increased as platforms deploy more
 *   sophisticated dark patterns and as the sheer volume of consent requests
 *   grows. The constraint operates across multiple institutional levels
 *   simultaneously: individual exhaustion (powerless users), corporate
 *   strategy (platforms weaponizing consent), regulatory enforcement
 *   (agencies trying to maintain compliance standards), innovation ecosystems
 *   (startups bearing disproportionate compliance costs), and alternative
 *   governance coalitions (technical standards communities building
 *   replacement systems). This is not a singular constraint from a contested
 *   kernel but a structural consequence of implementing consent-based privacy
 *   protection in networked service economies where data processing is
 *   ubiquitous and platform power is asymmetric.
 *
 * KEY AGENTS:
 *   - Exhausted Users: Primary victims (powerless/trapped) — bear the cognitive cost of consent fatigue; cannot exit digital economy without abandoning work, healthcare, government services, social access
 *   - Privacy-Conscious Consumers: Secondary victims (moderate/constrained) — can exit individual services at high cost (social isolation, platform switching); experience mixed coordination and extraction
 *   - Compliant Data Controllers: Primary beneficiary (institutional/arbitrage) — experience consent as genuine enabler of trust-based data use; have capacity to achieve compliance with minimal dark patterns
 *   - Rent-Extracting Platforms: Secondary beneficiary (powerful/arbitrage) — weaponize consent mechanisms through dark patterns; convert mandatory friction into extraction tool; bear compliance costs as price of extraction capacity
 *   - Regulatory Apparatus: Institutional actor (institutional/constrained) — maintains enforcement machinery despite evidence of theater; sees own process as degraded but persists through inertia
 *   - Innovation Coalition: Organized actors (organized/constrained) — experience mixed coordination (consent enables data partnerships) and extraction (compliance burden disproportionately burdens SMEs); have agency but face mandated compliance
 *   - Alternative Governance Coalition: Organized actors (organized/mobile) — building technical privacy controls (differential privacy, federated learning) as replacement for consent; see current constraint as temporary with sunset horizon
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing consent fatigue as inherent cognitive limit rather than contingent feature of regulatory and platform design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consent_fatigue_externality, 0.58).
domain_priors:suppression_score(consent_fatigue_externality, 0.62).
domain_priors:theater_ratio(consent_fatigue_externality, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consent_fatigue_externality, extractiveness, 0.58).
narrative_ontology:constraint_metric(consent_fatigue_externality, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(consent_fatigue_externality, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consent_fatigue_externality, tangled_rope).
narrative_ontology:human_readable(consent_fatigue_externality, "Consent Fatigue as Privacy Law Externality").
narrative_ontology:topic_domain(consent_fatigue_externality, "technology_governance/privacy_law/innovation_policy").

domain_priors:requires_active_enforcement(consent_fatigue_externality).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(consent_fatigue_externality, 'fd85d3ca-6e18-4334-a5ee-2190bf529d0f').
narrative_ontology:cs_kernel_codification('fd85d3ca-6e18-4334-a5ee-2190bf529d0f', fixed_text).
narrative_ontology:cs_authority_grounding('fd85d3ca-6e18-4334-a5ee-2190bf529d0f', lineage).
narrative_ontology:cs_interpretation_layer_present('fd85d3ca-6e18-4334-a5ee-2190bf529d0f').
narrative_ontology:cs_created_at('fd85d3ca-6e18-4334-a5ee-2190bf529d0f', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consent_fatigue_externality, data_controllers).
narrative_ontology:constraint_beneficiary(consent_fatigue_externality, high_complexity_services).
narrative_ontology:constraint_victim(consent_fatigue_externality, user_agency_commons).
narrative_ontology:constraint_victim(consent_fatigue_externality, innovation_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXHAUSTED USER (SNARE) — Users face mandatory consent friction on every platform, service, and data processing activity. Exit from the digital economy is structurally impossible (work, healthcare, government services, social participation all require digital access). The user experiences maximum suppression (consent is legally mandated) and cannot escape without abandoning essential services. The extraction is the value captured through degraded consent — users grant permissions they don't understand to maintain access, enriching data controllers.
constraint_indexing:constraint_classification(consent_fatigue_externality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRIVACY-CONSCIOUS CONSUMER (TANGLED ROPE) — Can exit individual services but at high cost (abandoning preferred platform, social isolation). The consent regime coordinates genuine privacy choice (users can opt out of some processing) alongside extraction (repeated fatigue-driven acceptance of conditions they would reject under full attention). Moderate power, constrained exit — typical bifurcated experience of participation with friction.
constraint_indexing:constraint_classification(consent_fatigue_externality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPLIANT DATA CONTROLLER (ROPE) — Interprets consent mechanisms as coordination that enables trust-based data use. For the compliant actor, GDPR consent is a genuine enabler: it provides legal basis, customer confidence, and market differentiation. This agent experiences the constraint as rope because their business model aligns with the regulatory intent — they benefit from transparent consent and face low compliance costs. Arbitrage exit available (can relocate processing, use different jurisdictions).
constraint_indexing:constraint_classification(consent_fatigue_externality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RENT-EXTRACTING PLATFORM (SNARE) — For actors with sufficient market power and engineering sophistication, consent fatigue is a tool: dark patterns, cookie walls, and manipulative UI design convert mandatory consent screens into extraction mechanisms. Users consent under cognitive load to access essential services. The platform experiences the constraint as snare: they bear the enforcement cost (legal compliance, audit burden) to maintain an extraction regime (high-friction consent capture). Powerful actors can weaponize the consent regime.
constraint_indexing:constraint_classification(consent_fatigue_externality, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY APPARATUS (PITON) — Privacy regulators (EDPB, CNIL, ICO) have built extensive enforcement machinery around consent mechanisms. The apparatus persists through institutional inertia despite accumulating evidence that consent-based compliance is theater: users don't read consent notices, banner fatigue reduces attention to near-zero, and dark patterns exploit the friction the regulation created. The theater ratio is high because enforcement activity (fines, warnings, guidance documents) creates performance of consent management without functional privacy protection. The regulatory function has atrophied (cannot prevent extraction through consent alone) but the apparatus endures.
constraint_indexing:constraint_classification(consent_fatigue_externality, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INNOVATION COALITION (TANGLED ROPE) — Startup ecosystems and SME consortia experience the constraint as mixed coordination and extraction. Consent compliance creates real coordination benefits (enables data-sharing partnerships, reduces customer friction relative to proprietary solutions). Simultaneously, the compliance burden (legal review, consent engineering, audit trails) extracts disproportionately from small actors who lack dedicated compliance teams. Organized but constrained: they have agency to influence standards but face barriers to exit (compliance is mandated; cannot opt out).
constraint_indexing:constraint_classification(consent_fatigue_externality, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, consent fatigue might be seen as an inherent limit to regulating consent-based data control: human attention is finite, and mandating consent for every data processing activity necessarily produces fatigue that degrades the quality of consent. This perspective sees the exhaustion as immutable (a constraint on human cognition), not a contingent feature of the regulatory regime. However, this naturalization obscures the role of dark patterns, service design, and platform power in producing fatigue — revealing the false summit structure.
constraint_indexing:constraint_classification(consent_fatigue_externality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: ALTERNATIVE GOVERNANCE COALITION (SCAFFOLD) — Emerging standards coalitions (privacy by design, federated learning, differential privacy, data minimization norms) are building alternative data governance pathways that reduce reliance on consent-based compliance. These coalitions see the current constraint as temporary: consent fatigue is a symptom of poor system design, not inherent to privacy protection. The scaffold is real and has a sunset — as technical standards mature (differential privacy tooling, privacy-enhancing computation), explicit consent-per-transaction becomes unnecessary. Mobile exit available (adopt alternative standards, migrate user expectations). Active enforcement through standards bodies and research communities.
constraint_indexing:constraint_classification(consent_fatigue_externality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consent_fatigue_externality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consent_fatigue_externality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consent_fatigue_externality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consent_fatigue_externality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consent_fatigue_externality, TR),
    TR >= 0.70.

:- end_tests(consent_fatigue_externality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint enables platforms to capture user permissions through fatigue-driven acceptance while maintaining legal compliance. The extraction is substantial but not maximal because compliant actors can and do achieve genuine consent, and because technical alternatives (privacy-by-design) are reducing reliance on consent in some domains. The 0.58 value reflects that extraction is real and increasing (measurement trajectory 0.45 → 0.58) but is constrained by regulatory oversight, technical alternatives, and user sophistication growth. Suppression (0.62): High. Users face mandatory consent friction to access essential services and cannot exit without abandoning digital participation. Dark patterns amplify structural suppression through manipulative UI design. Suppression is enforced through both regulatory mandate (consent is legally required) and platform power (services are conditionally gated on consent acceptance). Theater ratio (0.68): High. Regulatory enforcement activity (guidance, fines, warnings) creates the appearance of consent management without functional privacy protection. Users mechanically click through consent notices; platforms routinely violate GDPR consent requirements (bundled consent, insufficient granularity, dark patterns) with minimal enforcement consequences. The theater has increased over the interval as both regulatory complexity and platform sophistication in circumventing intent have grown. Extractiveness trajectory (0.45 → 0.52 → 0.58) shows steady accumulation of extraction as platforms refine dark pattern deployment and consent fatigue deepens. Suppression trajectory (0.55 → 0.60 → 0.62) shows increasing structural suppression as more services go online and as dark patterns become more sophisticated. Theater trajectory (0.52 → 0.60 → 0.68) shows increasing performative content as regulatory enforcement maintains the appearance of consent protection without functional outcomes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximum perspectival differentiation. The exhausted user experiences snare: trapped in mandatory consent regimes that degrade their choices. The compliant controller experiences rope: consent as genuine enabler of trust-based data use. The rent-extracting platform experiences snare from its own perspective: consent compliance costs are significant (legal, engineering, audit), but the platforms bear these costs because the extraction payoff (high-friction consent capture) exceeds compliance burden. The regulatory apparatus experiences piton: sees its own enforcement machinery as substantially performative but continues because alternatives haven't fully replaced it. The innovation coalition experiences tangled rope: genuine coordination benefits (consent enables partnerships) mixed with extraction (compliance burden disproportionately affects small actors). The alternative governance coalition experiences scaffold with a real sunset: sees consent fatigue as a temporary problem solved by technical privacy controls reaching maturity (10-15 year horizon). The analytical observer risks mountain (naturalizing fatigue as cognitive limit) but structural data reveals false summit (fatigue is primarily platform and regulatory design, not immutable). These perspectival gaps are not measurement artifacts — they reflect genuine differences in structural position, power, and exit capacity relative to this specific constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position: their power level, exit options, and beneficiary/victim status. Exhausted users have d ≈ 0.95 (full target: powerless, trapped, forced to accept consent); compliant controllers have d ≈ 0.10 (beneficiaries with low extraction experience); rent-extracting platforms have d ≈ 0.40 (powerful beneficiaries but must bear enforcement costs to maintain extraction capacity — intermediate d due to cost-benefit tradeoff); regulatory apparatus has d ≈ 0.72 (analytical about extraction but embedded in institutional inertia — moderate target position despite analytical power). The chi formula χ = ε × f(d) × σ(S) applies the sigmoid f(d) to these d values, producing: exhausted users experience χ ≈ 1.42 (high d → high f(d) → maximum extraction at global scope); compliant controllers experience χ ≈ -0.05 (low d → negative f(d) → negative/beneficial extraction — the constraint enables their business model); rent-extracting platforms experience χ ≈ 0.65 (moderate d → moderate f(d) → meaningful extraction capacity despite enforcement costs). Scope modifier σ(S) amplifies extraction at global scope (σ=1.2) — consent fatigue scales across all digital services globally, making the extraction mechanism harder to monitor or regulate. The perspectival gap between exhausted users (high χ, high experience of extraction) and compliant controllers (low/negative χ, low experience of extraction or benefit) is precisely what differentiates snare from rope: same structural constraint (GDPR consent regime), different experienced extractiveness due to power and exit differentiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that all eight types are legitimate perspectival readings. The false summit (mountain) is revealed through structural data: consent fatigue is not an inherent cognitive limit but a contingent feature of regulatory architecture and platform design. The snare (exhausted user) is the target's accurate structural reality. The rope (compliant controller) is the beneficiary's genuine experience. The piton (regulatory apparatus) is real: enforcement machinery persists despite degraded function. The tangled rope (innovation coalition, privacy-conscious consumer) accurately captures mixed coordination and extraction. The scaffold (alternative governance coalition) is real and has a sunset: technical standards are maturing and can reduce reliance on consent within 10-15 years. The constraint does not resolve to a single type — it resolves to a presheaf over the observation site where each perspective captures real structure from its position. The mandatrophy is resolved by recognizing that DR classification is fundamentally relational: the constraint appears differently to agents with different power, exit options, and structural relationships. No single type is 'correct' — the full perspectival array is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_quality_degradation_threshold,
    'At what frequency or complexity of consent requests does user attention collapse sufficiently that consent no longer represents meaningful choice?',
    'Empirical studies of consent comprehension vs. number/complexity of consent notices; eye-tracking analysis of consent banner engagement; A/B testing of consent UI design impacts on actual choice differentiation',
    'If threshold is low (3-5 notices): most consent regimes are already non-functional. If threshold is high (20+ notices): current fatigue is overstated. This determines whether the constraint is primarily a GDPR implementation failure (fixable via reform) or an inherent limitation of consent-based models.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_quality_degradation_threshold, empirical, 'Consent attention collapse threshold').

omega_variable(
    dark_pattern_prevalence_vs_compliance_intent,
    'Are consent dark patterns (cookie walls, manipulative UI, pre-ticked boxes) violations of GDPR intent or intended side effects of the regulatory architecture?',
    'Comparative analysis of regulatory guidance (EDPB Guidelines 05/2020) vs. enforcement actions; study of compliance outcomes when enforcement is strict vs. lenient; examination of whether GDPR amendments (AI Act, Digital Services Act) systematically tighten dark pattern prohibitions or remain permissive',
    'If dark patterns are violations: the constraint is regulatory failure (fixable via enforcement). If side effects: the constraint is structural to consent-based models (requires architectural change).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dark_pattern_prevalence_vs_compliance_intent, conceptual, 'Whether dark patterns are regulatory violations or structural side effects').

omega_variable(
    alternative_governance_viability,
    'Can privacy-enhancing technologies (differential privacy, federated learning, privacy-preserving analytics) actually replace consent-based governance at scale, or do they require consent as backstop for residual risks?',
    'Deployment studies of systems using purely technical privacy controls without explicit user consent; analysis of residual data flows and edge cases; adoption metrics for privacy-by-design frameworks; legal analysis of whether technical controls satisfy regulatory intent without consent',
    'If viable: the scaffold perspective is real and sunset is achievable (10-15 year horizon). If not viable: consent-based governance is permanent, and the constraint''s improvement requires reform within consent models, not replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_governance_viability, empirical, 'Viability of consent replacement via technical privacy controls').

omega_variable(
    false_summit_natural_law_ambiguity,
    'Is consent fatigue an inherent limit on human attention (natural law) or a contingent feature of platform design, service complexity, and dark patterns (constructed constraint)?',
    'Historical analysis of consent comprehension in simpler regulatory environments (pre-cookie-law eras); comparison of fatigue levels across jurisdictions with different UI design standards; intervention studies where platforms redesign consent flows for clarity vs. extraction',
    'If natural law: fatigue is inevitable and policy must accept high levels of degraded consent or shift to technical/paternalistic controls. If constructed: platform design choices and regulatory gaps are primary drivers, and better design can reduce fatigue substantially while preserving choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_ambiguity, conceptual, 'Natural law vs. constructed constraint framing of consent fatigue').

omega_variable(
    gdpr_kernel_interpretation_reading_gap,
    'Does the GDPR legal text create one coherent consent regime (one kernel, multiple readings) or multiple structurally incompatible compliance regimes (multiple kernels)?',
    'Comparative regulatory decisions (EDPB, national regulators, courts) on borderline cases (legitimate interest vs. consent, bundled consent validity, adequate consent granularity); analysis of whether disagreements center on interpretation of one text or on irreconcilable premises about privacy protection',
    'If one kernel: this story captures the unified constraint; regulatory harmonization can converge readings. If multiple kernels: the GDPR should decompose into separate constraint stories per interpretation regime. Current analysis assumes one kernel (formalized fixed_text authority_grounding with lineage interpretation) pending empirical verification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gdpr_kernel_interpretation_reading_gap, conceptual, 'GDPR as single kernel or multiple kernels').

omega_variable(
    suppression_mechanism_structural_vs_dark_pattern,
    'Is measured suppression (0.62) driven by structural barriers (mandatory consent for essential services, technical necessity) or by dark pattern design (manipulative UI, insufficient information)?',
    'Decomposition study: measure consent comprehension and choice differentiation when UI design follows best-practice clarity (light background, clear options, active choice) vs. dark patterns; test whether users with transparent consent show significantly lower fatigue and higher choice coherence',
    'If structural: suppression is inherent to consent-based governance. If dark-pattern-driven: suppression reflects regulatory failure and enforcement gaps, and can be reduced through design standards and enforcement tightening. This informs whether suppression should be treated as immutable or as modifiable through governance improvement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_dark_pattern, empirical, 'Suppression mechanism: structural or dark-pattern-driven').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consent_fatigue_externality, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cfat_tr_t0, consent_fatigue_externality, theater_ratio, 0, 0.52).
narrative_ontology:measurement(cfat_tr_t3, consent_fatigue_externality, theater_ratio, 3, 0.6).
narrative_ontology:measurement(cfat_tr_t6, consent_fatigue_externality, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(cfat_be_t0, consent_fatigue_externality, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cfat_be_t3, consent_fatigue_externality, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(cfat_be_t6, consent_fatigue_externality, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cfat_su_t0, consent_fatigue_externality, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cfat_su_t3, consent_fatigue_externality, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(cfat_su_t6, consent_fatigue_externality, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consent_fatigue_externality, resource_allocation).
narrative_ontology:affects_constraint(consent_fatigue_externality, dark_pattern_prevalence).
narrative_ontology:affects_constraint(consent_fatigue_externality, regulatory_capture_privacy_agencies).
narrative_ontology:affects_constraint(consent_fatigue_externality, platform_power_asymmetry_digital_markets).

% DUAL FORMULATION NOTE:
% The consent fatigue externality is downstream of GDPR implementation but represents a structurally distinct constraint from the original regulation. Separate stories address: (1) GDPR intent (consent coordination mechanism) with lower ε ≈ 0.25, and (2) GDPR implementation (consent fatigue as extraction mechanism) with ε ≈ 0.58. This story focuses on implementation externalities. The upstream constraint (GDPR purpose and intent) has its own extractiveness reflecting regulatory design; this story captures how that design produces unintended extraction through fatigue in deployed systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(consent_fatigue_externality, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
