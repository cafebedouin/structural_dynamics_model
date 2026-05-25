% ============================================================================
% CONSTRAINT STORY: synthetic_information_reliability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_synthetic_information_reliability, []).

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
 *   constraint_id: synthetic_information_reliability
 *   human_readable: Synthetic Information Reliability Constraint
 *   domain: information_systems/epistemic_infrastructure
 *
 * SUMMARY:
 *   The synthetic information reliability constraint models the structural
 *   extraction that emerges when synthetic content production outpaces
 *   verifiable authentication of that content. As generative systems (LLMs,
 *   synthetic media, deepfakes) proliferate, the information ecosystem faces
 *   irreducible uncertainty about source authenticity and content
 *   reliability. This creates asymmetric benefits: producers of synthetic
 *   content can scale communication and knowledge generation at marginal
 *   cost; consumers and the epistemic commons bear verification burden and
 *   contamination risk. The constraint exhibits all six DR types from
 *   different perspectives. From the ecosystem's view, it is a snare —
 *   irreversible contamination with no exit. From producers' view, it is rope
 *   — coordination that enables scale. From organized regulatory actors, it
 *   is a scaffold with sunset — cryptographic provenance and digital
 *   authentication standards offer genuine exit paths. From legacy
 *   verification institutions, it is a piton — performative authority with
 *   degraded actual function. The analytical observer risks seeing a mountain
 *   — information-theoretic inevitability — but structural data reveals this
 *   as naturalization of contingent institutional choices.
 *
 * KEY AGENTS:
 *   - Synthetic Content Producers: Primary beneficiary (institutional/arbitrage) — capture scaling benefits and marginal-cost economics during reliability gap window
 *   - Information Ecosystem: Primary victim (powerless/trapped) — epistemic commons suffers irreversible contamination as signal-to-noise ratio degrades
 *   - End Users/Consumers: Victim (powerless/trapped) — bear verification cost individually; asymmetric information disadvantage at point of consumption
 *   - Verification and Detection Services: Secondary beneficiary/victim (moderate/constrained) — profit from the gap they are hired to close; business model depends on uncertainty persistence
 *   - Regulatory and Standards Bodies: Organized actor (organized/constrained) — EU AI Act, C2PA, content authentication communities building alternative pathways
 *   - Legacy Verification Institutions: Institutional actor (institutional/arbitrage) — editorial boards, fact-checkers, journalistic authority maintaining performative function despite degraded capability
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent incentive structures as immutable information-theoretic limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(synthetic_information_reliability, 0.58).
domain_priors:suppression_score(synthetic_information_reliability, 0.62).
domain_priors:theater_ratio(synthetic_information_reliability, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(synthetic_information_reliability, extractiveness, 0.58).
narrative_ontology:constraint_metric(synthetic_information_reliability, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(synthetic_information_reliability, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(synthetic_information_reliability, tangled_rope).
narrative_ontology:human_readable(synthetic_information_reliability, "Synthetic Information Reliability Constraint").
narrative_ontology:topic_domain(synthetic_information_reliability, "information_systems/epistemic_infrastructure").

domain_priors:requires_active_enforcement(synthetic_information_reliability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(synthetic_information_reliability, synthetic_content_producers).
narrative_ontology:constraint_beneficiary(synthetic_information_reliability, scaling_beneficiaries).
narrative_ontology:constraint_victim(synthetic_information_reliability, information_ecosystem).
narrative_ontology:constraint_victim(synthetic_information_reliability, end_users).
narrative_ontology:constraint_victim(synthetic_information_reliability, knowledge_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMATION ECOSYSTEM (SNARE) — The epistemic commons cannot exit the reliability crisis. As synthetic content proliferates, distinguishability between authentic and generated information degrades irreversibly within the ecosystem. No mechanism exists to purify or recover signal after contamination. Maximum extraction from the collective knowledge base with no exit option.
constraint_indexing:constraint_classification(synthetic_information_reliability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: END USERS (SNARE) — Consumers face irreducible uncertainty. They cannot distinguish synthetic from authentic information at point of consumption without external verification infrastructure they do not control. Cost of verification is borne individually; benefits of efficient communication accrue to producers. Trapped in asymmetric information environment with no credible exit mechanism.
constraint_indexing:constraint_classification(synthetic_information_reliability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SYNTHETIC CONTENT PRODUCERS (ROPE) — Primary beneficiaries experience the constraint as enabling coordination: distributed generation of content at scale solves real communication and knowledge problems. The constraint (reliability gap) subsidizes their ability to produce at marginal cost. Net extraction flows toward this group; they experience the constraint as coordination that subsidizes their operation.
constraint_indexing:constraint_classification(synthetic_information_reliability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: VERIFICATION SERVICES (TANGLED ROPE) — Constrained by resource requirements and accuracy limits of detection technologies, but also benefit from the reliability gap they are hired to close. Real coordination function (detecting synthetic content), but extraction embedded: as verification improves, the market for verification expands; as detection becomes commoditized, the service layer extracts value from uncertainty itself. Cannot exit without degrading their own business model.
constraint_indexing:constraint_classification(synthetic_information_reliability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY BODIES (SCAFFOLD) — Organized actors (EU AI Act, Content Authentication Technical Community, digital provenance standards) view the reliability gap as a temporary coordination failure with built-in sunset. Synthetic content labeling, cryptographic provenance, and interoperable authentication standards are establishing alternative pathways that bypass the traditional trust-in-source model. Low effective extraction because regulators see both agency and exit path.
constraint_indexing:constraint_classification(synthetic_information_reliability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY INSTITUTIONS (PITON) — Traditional gatekeepers (editorial boards, fact-checkers, journalistic standards) maintain performative authority over synthetic content classification despite degraded actual function. The ritual persists through institutional inertia and credential monopolies rather than because verification processes work. Theater ratio (0.68) reflects the sustained performative claim to authority while actual functional verification capability lags reality. These institutions see their own process as degraded.
constraint_indexing:constraint_classification(synthetic_information_reliability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN VIEW) — From a civilizational/information-theoretic perspective, the reliability gap between synthetic and authentic information is treated as inherent to computational communication: any finite verification process can be defeated by sufficiently advanced synthesis. This perspective sees the constraint as an immutable property of information systems themselves. However, structural data contradicts the mountain classification — the engine's false summit detector will identify this as naturalization of what is actually a contingent institutional arrangement shaped by incentive structures and verification investment levels.
constraint_indexing:constraint_classification(synthetic_information_reliability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(synthetic_information_reliability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(synthetic_information_reliability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(synthetic_information_reliability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(synthetic_information_reliability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(synthetic_information_reliability, TR),
    TR >= 0.70.

:- end_tests(synthetic_information_reliability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint measures 0.28 at baseline (2020-21, early synthetic content scaling) and rises to 0.58 by endpoint (2025-26, deployment of advanced generative systems at scale). The extraction reflects the asymmetry between producers' ability to generate content at scale and consumers' inability to verify source and authenticity without external infrastructure. The rising trajectory indicates that as synthesis capability advances, verification burden increases faster than verification capability, widening the extraction gap. Suppression (0.62): High. Significant barriers include: (1) technical irreducibility — no finite verification process can prove non-synthesis; (2) epistemic barriers — consumers lack access to verification tools at point of consumption; (3) institutional barriers — legacy verification mechanisms (journalism, academia, editorial authority) are degrading faster than alternative infrastructure matures; (4) economic barriers — distributed verification is more expensive than centralized production. Theater ratio (0.68): High and rising. Legacy gatekeepers (fact-checkers, editorial boards, journalistic standards) maintain performative claims to authority over synthetic content classification despite demonstrably degraded actual verification function. The ritual persists through institutional inertia, credential monopolies, and lack of credible alternatives. As synthesis becomes more sophisticated, the gap between claimed verification capacity and actual capability widens, driving theater ratio higher. The theatrical component is the sustained authority claim despite reduced functional verification.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's extractive structure. Beneficiaries perceive coordination; victims perceive extraction. The divergence between producer (Rope) and ecosystem (Snare) perspectives is maximal — same structural phenomenon, opposite classifications. This gap is diagnostic: it indicates that the constraint maintains itself through asymmetric information (victims cannot see the full structure from their position) and power difference (beneficiaries can enforce the constraint despite victim opposition). The piton perspective indicates that legacy institutions maintain the reliability constraint not through function but through performative authority — they claim to solve the problem (verification) while actually maintaining it (by blocking alternative authentication infrastructure). The scaffold perspective indicates that this performative authority is not inevitable — regulatory bodies and standards communities are building exit paths through cryptographic provenance and distributed authentication, which would reduce theater and eventually collapse the extractive structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the extraction flow. Synthetic producers (beneficiaries) have arbitrage exit — they can leave synthetic content production and return to other domains if reliability infrastructure undermines their current model. They experience low effective extraction (χ) because they benefit from the gap. End users (victims) have trapped exit — they cannot access information without navigating the reliability gap. They experience high χ because the constraint extracts from them (verification cost, contamination risk) without compensation. Verification services (victims/beneficiaries hybrid) have constrained exit — they profit from verification but cannot exit because their business model depends on the gap persisting. They experience moderate χ. Regulatory bodies (organized victims) have constrained exit — they cannot escape responsibility for managing the constraint but can invest in alternatives (provenance infrastructure), giving them agency and reducing their experienced extraction. The analytical observer (analytical position) faces the oracle gap: their native instruments (information theory, computational limits) cannot see the institutional contingency that cross-position analysis reveals. The identity_locked status appears implicitly in the producer beneficiary group — their identity and market positioning have fused with 'synthetic as scalable', making exit conceptually difficult even as technical barriers to transition are being removed.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by demonstrating that the classification depends critically on observational position and exit structure, not on a single 'true' type. The same structural phenomenon (synthetic-authentic gap) classifies as Snare from the powerless ecosystem, Rope from the institutional producer, and Scaffold from the organized regulator. The mandatrophy would arise if we demanded a single type — but DR's indexical classification resolves it by showing that all six readings are legitimate from their respective positions. The false summit (mountain classification) is the analytical observer's naturalization of what is contingent institutional structure. Once recognized, the scaffold perspective becomes operative: provenance infrastructure offers genuine exit, which means the constraint is not a mountain (immutable) but a temporary coordination problem with built-in obsolescence. The theater ratio (0.68) indicates that legacy verification institutions are maintaining extractive claims through performative authority, not functional verification. As alternative infrastructure matures, theater and extraction should both decline. The measurement trajectory shows both theater and extractiveness rising, which indicates the constraint is in active disequilibrium and deteriorating — the gap between claimed and actual verification function is widening, a sign that legacy institutions are losing functional capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_budget_vs_sophistication_arms_race,
    'Can verification budget scale fast enough to match synthesis sophistication, or is the arms race structurally unwinnable?',
    'Empirical tracking of detection capability vs synthesis capability over 5-10 year horizon; measurement of error rates in synthetic detection systems as synthesis models improve; cost-benefit analysis of verification infrastructure scaling',
    'If verification scalable: constraint is temporary coordination problem (Scaffold from more perspectives, shorter sunset). If unwinnable arms race: constraint approaches immutable (Mountain-like extractiveness at scale), and regulatory solutions must shift from detection to provenance/attestation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(verification_budget_vs_sophistication_arms_race, empirical, 'Whether verification can keep pace with synthesis sophistication').

omega_variable(
    provenance_infrastructure_adoption_rate,
    'Will cryptographic content authentication and digital provenance standards achieve sufficient adoption to make synthetic-authentic distinction observable at scale?',
    'Tracking adoption of standards (C2PA, media provenance protocols) across major platforms and content generators; measurement of verification coverage (percentage of global information flows with cryptographic provenance); economic analysis of adoption incentives',
    'If adoption reaches critical mass (>60% of major platforms): scaffold sunset becomes operational — the reliability gap is solvable through infrastructure, not inherently extractive. If adoption stalls: constraint remains structural, and end-users remain trapped in reliability asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provenance_infrastructure_adoption_rate, empirical, 'Critical mass adoption of content provenance standards').

omega_variable(
    extraction_vs_coordination_decomposition,
    'How much of the reliability constraint''s ''extraction'' is genuine coordination cost (real burden of verification) vs rent-seeking behavior (artificial scarcity from actors blocking infrastructure)?',
    'Comparative analysis of verification costs in systems with cryptographic provenance vs systems relying on detection; measurement of institutional resistance to provenance standards adoption; cost accounting for true verification infrastructure vs defensive gatekeeping overhead',
    'If coordination cost dominates (ε_actual ~0.25): constraint is Rope with performative theater, not Tangled Rope. Beneficiary/victim distinction dissolves. If extraction dominates (ε_actual ~0.65): constraint is closer to Snare, and the regulatory scaffold is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_decomposition, empirical, 'True coordination cost vs institutional extraction in reliability gap').

omega_variable(
    identity_lock_in_producer_incentives,
    'Are synthetic content producers structurally dependent on reliability gaps (trapped), or identity-locked in business models that require them (cannot imagine alternatives)?',
    'Analysis of business model evolution as provenance infrastructure matures; whether producers transition to authenticated models or resist adoption; measurement of switching costs (technical vs identity/institutional)',
    'If trapped: producers will exit as infrastructure removes gaps. If identity-locked: resistance to provenance standards persists even after barriers are technically removed, because producer identity and market positioning have fused with synthetic-as-scalable narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_producer_incentives, conceptual, 'Whether producer reliance on reliability gaps is structural or identity-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(synthetic_information_reliability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(synth_info_tr_t0, synthetic_information_reliability, theater_ratio, 0, 0.42).
narrative_ontology:measurement(synth_info_tr_t3, synthetic_information_reliability, theater_ratio, 3, 0.55).
narrative_ontology:measurement(synth_info_tr_t6, synthetic_information_reliability, theater_ratio, 6, 0.65).
narrative_ontology:measurement(synth_info_tr_t9, synthetic_information_reliability, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(synth_info_be_t0, synthetic_information_reliability, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(synth_info_be_t3, synthetic_information_reliability, base_extractiveness, 3, 0.41).
narrative_ontology:measurement(synth_info_be_t6, synthetic_information_reliability, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(synth_info_be_t9, synthetic_information_reliability, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(synthetic_information_reliability, information_standard).
narrative_ontology:affects_constraint(synthetic_information_reliability, deepfake_detection_arms_race).
narrative_ontology:affects_constraint(synthetic_information_reliability, content_authentication_infrastructure).
narrative_ontology:affects_constraint(synthetic_information_reliability, epistemic_commons_contamination).

% DUAL FORMULATION NOTE:
% The synthetic information reliability constraint decomposes into three downstream constraints: (1) deepfake_detection_arms_race — the empirical verification challenge at the technical level; (2) content_authentication_infrastructure — the institutional/regulatory layer building provenance solutions; (3) epistemic_commons_contamination — the knowledge-system-level damage from reliability collapse. This constraint story focuses on the holistic extraction structure. The three downstream constraints have their own ε values reflecting their specific domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(synthetic_information_reliability, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
