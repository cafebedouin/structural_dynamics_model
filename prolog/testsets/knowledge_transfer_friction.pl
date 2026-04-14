% ============================================================================
% CONSTRAINT STORY: knowledge_transfer_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_knowledge_transfer_friction, []).

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
 *   constraint_id: knowledge_transfer_friction
 *   human_readable: Knowledge Transfer Friction Across Organizational and Epistemic Boundaries
 *   domain: organizational_behavior/epistemic_institutions
 *
 * SUMMARY:
 *   Knowledge transfer friction arises when the cost of acquiring and
 *   transmitting knowledge across organizational, professional, or epistemic
 *   boundaries exceeds what the inherent complexity of the knowledge itself
 *   requires. This constraint creates a hybrid coordination-extraction
 *   system: institutions require gatekeeping structures to maintain knowledge
 *   quality and professional standards (coordination function), but those
 *   same structures create artificial scarcity and restrict access to agents
 *   outside the institutional boundary (extraction function). The constraint
 *   exhibits all six DR types from different observation contexts, making it
 *   a diagnostic exemplar for how institutional extraction can masquerade as
 *   quality control. Theater ratio (0.55) reflects that credentialing systems
 *   increasingly perform institutional legitimacy maintenance rather than
 *   actual knowledge verification — degrees verify institutional affiliation,
 *   not capability; peer review gates prestige rather than ensuring quality.
 *   Over the 20-year interval, both theater ratio and extractiveness
 *   increased as institutional gatekeeping intensified despite the emergence
 *   of competing open-knowledge systems.
 *
 * KEY AGENTS:
 *   - Peripheral Knowledge Seekers: Primary victims (powerless/trapped) — bear full cost of friction; cannot access knowledge without institutional credentials or payment
 *   - Organizational Adapters: Secondary victims (moderate/constrained) — bridge multiple knowledge domains at personal cost; subsidize knowledge translation
 *   - Knowledge Gatekeeper Institutions: Primary beneficiaries (institutional/arbitrage) — control access through credentialing, publication norms, professional associations; capture rent from scarcity
 *   - Open Knowledge Coalition: Organized agents (organized/constrained) — Wikipedia, arXiv, GitHub, MOOCs, open-source communities; provide coordination function while fighting institutional resistance
 *   - Credentialing Systems: Institutional actors (institutional/arbitrage) — maintain performative authority over knowledge legitimacy; persist through inertia despite degraded utility
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional gatekeeping as inherent to knowledge itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(knowledge_transfer_friction, 0.52).
domain_priors:suppression_score(knowledge_transfer_friction, 0.48).
domain_priors:theater_ratio(knowledge_transfer_friction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(knowledge_transfer_friction, extractiveness, 0.52).
narrative_ontology:constraint_metric(knowledge_transfer_friction, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(knowledge_transfer_friction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(knowledge_transfer_friction, tangled_rope).
narrative_ontology:human_readable(knowledge_transfer_friction, "Knowledge Transfer Friction Across Organizational and Epistemic Boundaries").
narrative_ontology:topic_domain(knowledge_transfer_friction, "organizational_behavior/epistemic_institutions").

domain_priors:requires_active_enforcement(knowledge_transfer_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(knowledge_transfer_friction, knowledge_gatekeepers).
narrative_ontology:constraint_beneficiary(knowledge_transfer_friction, incumbent_institutions).
narrative_ontology:constraint_victim(knowledge_transfer_friction, knowledge_seekers).
narrative_ontology:constraint_victim(knowledge_transfer_friction, organizational_periphery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL KNOWLEDGE SEEKER (SNARE) — Trapped within institutional boundaries and gatekeeping structures. Cannot access tacit knowledge held by incumbents; cannot bypass established channels without career damage. Maximum extraction cost: must pay in time, credentials, or organizational loyalty to access knowledge that would be freely available in absence of friction. No viable exit option from the friction itself.
constraint_indexing:constraint_classification(knowledge_transfer_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZATIONAL ADAPTER (TANGLED ROPE) — Moderately constrained agents who bridge institutions but at cost. They benefit from access to multiple knowledge domains (coordination function) while bearing significant friction costs (extraction). Career advancement requires translating between implicit institutional cultures. Extraction is asymmetric: translators subsidize knowledge integration.
constraint_indexing:constraint_classification(knowledge_transfer_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: KNOWLEDGE GATEKEEPER INSTITUTION (ROPE) — Primary beneficiary. Institutional actors (universities, professional associations, corporate R&D) control knowledge access through credentialing, publication norms, and tacit skill transmission. The friction preserves their authority over knowledge legitimacy. They experience the constraint as coordination: structuring access prevents chaos and ensures quality control. Net beneficiary with arbitrage exit options.
constraint_indexing:constraint_classification(knowledge_transfer_friction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN KNOWLEDGE COALITION (TANGLED ROPE) — Organized agents (open-source communities, Wikipedia, open-access movements, MOOCs) simultaneously coordinate knowledge access while being constrained by incumbent institutional resistance. They provide coordination function (pooling tacit knowledge, democratizing access) while bearing extraction costs (fighting against institutional gatekeeping, resource scarcity, institutional dismissal of non-credentialed contributions).
constraint_indexing:constraint_classification(knowledge_transfer_friction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDENTIALING THEATER (PITON) — Traditional credentialing systems (degrees, certifications, peer review gatekeeping) persist through institutional inertia despite degraded functional utility. Theater ratio (0.55) reflects that formal credentials increasingly fail to predict actual knowledge or capability. The credentialing process is largely performative maintenance of institutional authority, not genuine knowledge verification. Persists because alternatives haven't fully displaced it.
constraint_indexing:constraint_classification(knowledge_transfer_friction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TACIT KNOWLEDGE VIEW (MOUNTAIN) — From a universal perspective, some friction is inherent to knowledge transfer: tacit knowledge (know-how, intuition, embodied practice) cannot be fully codified and transmitted. The gap between explicit and tacit is a structural feature of how knowledge propagates. However, the structural data contradicts the pure mountain classification — observed friction substantially exceeds what tacit knowledge limits alone would require, revealing that institutional gatekeeping amplifies what might otherwise be a modest coordination challenge.
constraint_indexing:constraint_classification(knowledge_transfer_friction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(knowledge_transfer_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(knowledge_transfer_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(knowledge_transfer_friction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(knowledge_transfer_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(knowledge_transfer_friction, TR),
    TR >= 0.70.

:- end_tests(knowledge_transfer_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Knowledge transfer friction imposes real costs on peripheral agents — time spent acquiring credentials, payment for access, career penalties for non-institutional knowledge. But extraction is not total: some knowledge transfers freely (open-source, volunteer teaching, mentorship); institutional pathways do confer real benefits (quality assurance, professional networks, social legitimation). The 0.52 value reflects that friction is partly extractive (institutional rent-seeking) and partly coordination cost (genuine quality maintenance). Suppression (0.48): Moderate. Barriers to knowledge access exist but are not total — workarounds include informal mentorship, open-access alternatives, self-study. Career risk of non-credentialed knowledge-seeking exists but is manageable. Theater ratio (0.55): Moderate-high and increasing. Credentialing systems increasingly perform institutional legitimacy rather than knowledge verification. A university degree verifies institutional affiliation more reliably than technical capability; peer review gates prestige more than quality; professional licensing certifies institutional membership more than competence. This ratio has increased over 20 years as specialized knowledge has become more complex, making verification by traditional gatekeepers less credible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perpendicularity across multiple institutional contexts. Gatekeeping institutions see Rope (coordination for quality assurance) — they genuinely solve the problem of knowledge legitimacy in a world where false claims propagate quickly. Peripheral seekers see Snare (pure extraction) — they must pay rent to institutions for access that would be freely available in absence of gatekeeping. Organizational adapters see Tangled Rope (mixed coordination and extraction) — they benefit from access to multiple knowledge domains and professional networks while bearing translation costs. Open knowledge movements see Tangled Rope (they coordinate knowledge access while being suppressed by institutional resistance). The piton perspective reveals credentialing theater — formal credentials persist through inertia despite declining functional utility. The analytical observer risks seeing a Mountain (tacit knowledge limits are immutable) but the structural data reveals this as false — observed friction substantially exceeds what inherent tacit limits require.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent reflects their structural position in the extraction flow. Gatekeeping institutions (d ≈ 0.05-0.15) experience low effective extraction because they are primary beneficiaries with arbitrage exit options — they control the friction mechanism. Peripheral seekers (d ≈ 0.95) experience maximum extraction because they are trapped: cannot exit without acquiring expensive credentials. Organizational adapters (d ≈ 0.55-0.65) experience moderate extraction because they are moderately constrained — they can acquire credentials or find informal pathways, but both are costly. Open knowledge coalitions (d ≈ 0.65-0.75) experience high extraction through resistance they face, but also moderate benefits through the coordination function they provide. The analytical observer (d ≈ 0.72) experiences the constraint at the civilizational scale where tacit knowledge limits create genuine friction independent of institutional gatekeeping.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in knowledge transfer friction is NOT 'which type is correct?' but 'which extraction mechanism is driving the friction?' The analytical observer's mountain (tacit knowledge is inherently hard to transfer) is partially true but substantially incomplete — it naturalizes institutional gatekeeping as if it were an inherent feature of knowledge rather than a contingent institutional choice. The resolution is perspectival: all six types are legitimate readings of the same structural data from different positions. The gatekeeper sees rope; the peripheral seeker sees snare; the adapter sees tangled rope; the coalition sees tangled rope with asymmetric suppression; the credentialing system sees piton (performing legitimacy through ritual); the analyst sees mountain (tacit limits). The constraint resolves mandatrophy by showing that friction serves multiple functions simultaneously — coordination (preventing knowledge pollution), extraction (concentrating professional status), and legitimation theater (maintaining institutional authority). No single type is 'the truth'; the presheaf over all observation positions IS the structural reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_vs_institutional_friction,
    'What proportion of observed knowledge transfer friction is inherent to tacit knowledge limits versus artificially imposed by institutional gatekeeping?',
    'Comparative analysis: measure transfer friction in open-knowledge environments (GitHub, arXiv preprints, Wikipedia communities) versus closed institutional contexts with identical tacit knowledge complexity. High correlation with institutional openness vs institutional closure would indicate institutional gatekeeping drives most friction.',
    'If institutional drives >70% of friction: Snare/Tangled Rope classification correct, organizational restructuring can reduce friction significantly. If tacit limits drive >50% of friction: friction has legitimate natural limit component, organizational change alone cannot eliminate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_vs_institutional_friction, empirical, 'Proportion of friction driven by tacit knowledge limits vs institutional gatekeeping').

omega_variable(
    extraction_mechanism_internalization,
    'Are peripheral agents internalizing the friction as legitimate credentialing necessity, or perceiving it as externally imposed extraction?',
    'Qualitative analysis of agent narratives about knowledge access barriers. Agents who frame barriers as ''necessary rigor'' or ''maintaining standards'' versus agents who frame barriers as ''unjust gatekeeping''. Survey of displaced/unsuccessful transfer attempts.',
    'If internalized as legitimate: friction may persist despite accessibility improvements because agents perceive barriers as justified. If perceived as extraction: organizational changes enabling access will encounter less resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_internalization, empirical, 'Whether peripheral agents internalize friction as legitimate or extractive').

omega_variable(
    decentralized_knowledge_viability,
    'Can decentralized, non-credentialed knowledge systems (open-source communities, online learning networks) scale to match institutional knowledge accessibility in specialized domains?',
    'Longitudinal comparison of knowledge quality, speed of innovation, and practitioner competence in decentralized versus credentialed systems across domains (software engineering, medical practice, scientific research). Market adoption rates.',
    'If decentralized systems prove viable: scaffold perspective confirmed — friction is temporary, alternative pathways are functional. If decentralized systems prove inadequate: institutional gatekeeping has genuine functional value, friction is partly justified coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralized_knowledge_viability, empirical, 'Viability of decentralized knowledge systems as alternatives to credentialed institutions').

omega_variable(
    identity_lock_in_knowledge_gatekeeping,
    'To what extent do institutional actors maintain gatekeeping because their professional identity is constituted through exclusive knowledge claims?',
    'Analysis of institutional resistance to knowledge democratization; whether resistance decreases when alternative identity structures (e.g., ''guide'' versus ''gatekeeper'', ''mentor'' versus ''authority'') are offered. Exit pathways for professional identity migration.',
    'If high identity lock: friction persists even when functional justification disappears; requires deep institutional identity restructuring. If low identity lock: friction responds to structural incentives and can be modified through policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_knowledge_gatekeeping, empirical, 'Role of professional identity fusion in maintaining knowledge gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(knowledge_transfer_friction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ktf_tr_t0, knowledge_transfer_friction, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ktf_tr_t10, knowledge_transfer_friction, theater_ratio, 10, 0.48).
narrative_ontology:measurement(ktf_tr_t20, knowledge_transfer_friction, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(ktf_be_t0, knowledge_transfer_friction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ktf_be_t10, knowledge_transfer_friction, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(ktf_be_t20, knowledge_transfer_friction, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(knowledge_transfer_friction, information_standard).
narrative_ontology:affects_constraint(knowledge_transfer_friction, professional_credentialing_cartel).
narrative_ontology:affects_constraint(knowledge_transfer_friction, academic_prestige_hierarchy).
narrative_ontology:affects_constraint(knowledge_transfer_friction, intellectual_property_barriers).

% DUAL FORMULATION NOTE:
% Knowledge transfer friction is upstream of specific credentialing systems and intellectual property regimes but represents a distinct structural constraint. The downstream constraints (credentialing cartels, prestige hierarchies, IP barriers) are specific institutional manifestations; this constraint captures the generic mechanism by which institutional boundaries create knowledge access friction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(knowledge_transfer_friction, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
