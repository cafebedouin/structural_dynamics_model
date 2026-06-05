% ============================================================================
% CONSTRAINT STORY: gdpr_right_to_be_forgotten
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_right_to_be_forgotten, []).

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
 *   constraint_id: gdpr_right_to_be_forgotten
 *   human_readable: GDPR Right to Be Forgotten
 *   domain: digital_rights/data_governance
 *
 * SUMMARY:
 *   The GDPR Right to Be Forgotten (RTBF) is a regulatory mandate that
 *   permits data subjects to request deletion or delisting of their personal
 *   information from digital systems. Enacted in 2016, it represents a
 *   fundamental claim: that individuals should retain control over their
 *   digital history and that information should not persist indefinitely in
 *   indexed form. The constraint is structurally complex because it
 *   simultaneously coordinates privacy interests (matching data subjects'
 *   preferences about their digital presence) and extracts from platforms
 *   (imposing compliance costs and limiting the comprehensiveness of indexed
 *   information systems). From different perspectives, the same constraint
 *   appears as liberation (for trapped data subjects), coordination (for
 *   privacy advocates), extraction (for search engines), loss (for
 *   historians), and theatrical compliance (for regulators). The
 *   extractiveness has increased over the measurement period from 0.32 to
 *   0.58 as platforms have developed strategic non-compliance techniques,
 *   consolidated their market power, and as the scope of delisting requests
 *   has expanded beyond initial predictions. Theater ratio has remained
 *   moderate (0.35–0.48) because the constraint involves genuine coordination
 *   labor (platforms must develop compliance infrastructure) alongside
 *   significant performative elements (delisting workflows that give
 *   appearance of control without addressing underlying surveillance
 *   indexing).
 *
 * KEY AGENTS:
 *   - Data Subjects: Primary beneficiary (powerless/trapped without RTBF; powerless/constrained when exercising rights) — seek to control their digital history and reputation
 *   - Search Engines (Google, Bing, etc.): Primary victim (powerful/mobile in global markets but trapped within EU jurisdiction) — bear compliance costs and face reduced indexing value
 *   - Privacy Advocates: Secondary beneficiary (institutional/arbitrage) — gain institutional legitimacy and market demand from RTBF implementation
 *   - Historical Researchers and Archivists: Secondary victim (powerless/trapped) — lose access to information necessary for historical research and accountability journalism
 *   - Data Protection Authorities (CNIL, ICO, BfDI): Regulators (institutional/constrained) — enforce RTBF and navigate contested cases balancing privacy against public interest
 *   - Technical Solutions Providers: Organized actors (organized/constrained) — develop decentralized and privacy-preserving alternatives that could render centralized RTBF obsolete
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing information immutability as law when it reflects contingent architectural choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_right_to_be_forgotten, 0.58).
domain_priors:suppression_score(gdpr_right_to_be_forgotten, 0.65).
domain_priors:theater_ratio(gdpr_right_to_be_forgotten, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_right_to_be_forgotten, extractiveness, 0.58).
narrative_ontology:constraint_metric(gdpr_right_to_be_forgotten, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gdpr_right_to_be_forgotten, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_right_to_be_forgotten, tangled_rope).
narrative_ontology:human_readable(gdpr_right_to_be_forgotten, "GDPR Right to Be Forgotten").
narrative_ontology:topic_domain(gdpr_right_to_be_forgotten, "digital_rights/data_governance").

domain_priors:requires_active_enforcement(gdpr_right_to_be_forgotten).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_right_to_be_forgotten, data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_right_to_be_forgotten, privacy_advocates).
narrative_ontology:constraint_victim(gdpr_right_to_be_forgotten, search_engines).
narrative_ontology:constraint_victim(gdpr_right_to_be_forgotten, content_indexers).
narrative_ontology:constraint_victim(gdpr_right_to_be_forgotten, historical_archives).
narrative_ontology:constraint_victim(gdpr_right_to_be_forgotten, public_information_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (SNARE) — An individual whose past conduct, financial history, medical data, or incriminating information is permanently indexed and searchable faces immutable extraction: their reputation and opportunity costs are permanently depressed by indexed information they cannot remove. They have no exit from the constraint — the information persists regardless of context, rehabilitation, or time elapsed. The suppression is total: delisting is difficult, alternatives do not exist, and power is asymmetrically distributed. From this perspective, the right to be forgotten is not an extraction mechanism but a liberation from one.
constraint_indexing:constraint_classification(gdpr_right_to_be_forgotten, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DATA SUBJECT EXERCISING RTBF (TANGLED ROPE) — Once the data subject initiates delisting requests, they experience the right to be forgotten as a coordination mechanism (they coordinate with platforms to manage information flow) mixed with asymmetric extraction: platforms have high compliance costs, face legal liability, and bear resource burdens to process delisting requests. The data subject experiences some benefit (reduced indexed data) and some cost (slow processing, partial compliance, search engines' strategic non-compliance). Exit options are constrained — the subject must navigate platforms' appeal processes and enforce rights through litigation.
constraint_indexing:constraint_classification(gdpr_right_to_be_forgotten, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRIVACY ADVOCACY COALITION (ROPE) — Advocacy organizations, data protection authorities, and privacy-by-design firms benefit from the right to be forgotten as a coordination mechanism: it creates demand for privacy services, establishes regulatory leverage, and defines a clear institutional role. From their perspective, RTBF is pure coordination with minimal extraction — it solves the collective action problem of balancing individual privacy against indexed information systems. Exit options include arbitrage: organizations can earn revenue and influence by advising on RTBF compliance.
constraint_indexing:constraint_classification(gdpr_right_to_be_forgotten, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SEARCH ENGINES (SNARE) — Google, Bing, and other search indexers experience the right to be forgotten as pure extraction: they must invest significant compliance costs (technical infrastructure, legal review, delisting workflows), face reputational damage when incorrectly denying requests, and suffer reduced indexing value as searchable content shrinks. They have no genuine exit option — operating in the EU requires compliance, and even non-EU operators face liability through EU users accessing their services. Suppression is high: they cannot refuse to implement RTBF without facing regulatory sanction. The extraction is asymmetric: costs fall on the indexers while benefits accrue to data subjects.
constraint_indexing:constraint_classification(gdpr_right_to_be_forgotten, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: HISTORICAL RESEARCHERS (SNARE) — Archivists, journalists, and historians face extraction through the right to be forgotten: information necessary for research, historical reconstruction, and accountability journalism is systematically delisted, making comprehensive historical analysis increasingly difficult. They cannot exit the constraint — they have no leverage to negotiate carve-outs, and the suppression is total: once information is delisted, recovery requires direct source access that is often unavailable. The extraction is permanent: each successful delisting request reduces the epistemic commons available to future research.
constraint_indexing:constraint_classification(gdpr_right_to_be_forgotten, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: DATA PROTECTION AUTHORITIES (TANGLED ROPE) — Regulators (CNIL, ICO, BfDI) implement RTBF enforcement as a coordination mechanism: they balance individual privacy rights against societal information access. But they also experience extraction: they must invest enforcement capacity, navigate contested cases (what counts as 'forgotten'?), and face pressure from both data subjects and platforms. They have some exit options (constrained): they can set enforcement priorities and interpret the right's boundaries, but they cannot opt out of implementation.
constraint_indexing:constraint_classification(gdpr_right_to_be_forgotten, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: TECHNICAL SOLUTIONS PROVIDERS (SCAFFOLD) — Companies developing privacy-enhancing technologies, federated search, and distributed archives see RTBF as a temporary coordination challenge with a sunset: decentralized search, local-first architecture, and user-controlled data stores are building alternative information retrieval systems that render centralized indexed deletion obsolete. Extraction is low because the market sees this as transitional. The sunset is the normalization of privacy-respecting defaults.
constraint_indexing:constraint_classification(gdpr_right_to_be_forgotten, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: INSTITUTIONAL THEATER VIEW (PITON) — From a civilizational timescale, the right to be forgotten is substantially performative: it creates visible compliance workflows (delisting request forms, regulatory documentation) that give the appearance of privacy control while leaving power asymmetries in place. Data subjects' exercised right is <5% of UK internet users; enforcement is spotty; platforms develop strategic non-compliance; and the core problem (centralized surveillance indexing) persists. The theatrical compliance mechanism (the right itself) masks the deeper structural issue. Theater ratio is moderate (0.48) because genuine coordination and real delisting occur, but the apparatus' functional scope is constrained.
constraint_indexing:constraint_classification(gdpr_right_to_be_forgotten, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / IMMUTABILITY VIEW (MOUNTAIN) — From a universal timescale, information deletion is mathematically intractable: once data is indexed, copied, and distributed across decentralized systems, removal becomes a coordination problem requiring global agreement on what counts as 'forgotten.' The right to be forgotten is thus a Sisyphean constraint — it appears as natural law because the task of perfect deletion is incomputable. However, this perspective risks naturalizing what is a contingent institutional choice: aggregated centralized indexing could be replaced by privacy-respecting architecture. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(gdpr_right_to_be_forgotten, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_right_to_be_forgotten_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gdpr_right_to_be_forgotten, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gdpr_right_to_be_forgotten, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_right_to_be_forgotten, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gdpr_right_to_be_forgotten, TR),
    TR >= 0.70.

:- end_tests(gdpr_right_to_be_forgotten_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, increasing over time. Initial implementation (2016–2020) saw extractiveness around 0.32 as platforms adapted and compliance became routinized. By 2024, extractiveness rose to 0.58 as platforms developed strategic non-compliance (requiring manual delisting requests, maintaining multiple copies, slow processing), data subject awareness increased, and the volume of delisting requests exposed the resource intensity of platform compliance. The constraint extracts from search engines and historical archives while benefiting data subjects and privacy organizations. Suppression (0.65): Moderate-high. Suppression mechanisms include: (1) asymmetric enforcement — data subjects lack resources to litigate non-compliance; (2) platform strategic non-compliance — delisting requests are slow, appeal processes are opaque, and information re-emerges through aggregators; (3) scope ambiguity — legitimate interest exceptions create loopholes for platforms; (4) third-party re-indexing — information deleted from Google reappears in specialized indices. Theater ratio (0.48): Moderate. The constraint involves genuine coordination (platforms develop compliance infrastructure, regulators establish delisting procedures) but significant theatrical elements (the appearance of control without addressing centralized surveillance indexing, performative 'right' that is inconsistently enforced, compliance workflows that consume resources without shifting power balance).
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence. The data subject sees the constraint as either liberation (snare) from permanent indexing or mixed coordination/extraction (tangled rope) when navigating the delisting process. Search engines see pure extraction (snare) — compliance costs with no compensating benefit from their perspective. Privacy advocates see pure coordination (rope) — the right creates demand for privacy services and establishes regulatory legitimacy. Historians see pure extraction (snare) — deletion of information necessary for research. Regulators see mixed coordination/extraction (tangled rope) — they must balance privacy rights against public interest while managing enforcement overhead. Technical intermediaries see a sunset mechanism (scaffold) — privacy-respecting architecture will eventually render centralized RTBF unnecessary. The piton perspective observes that despite the right's existence, actual control remains concentrated in platform hands. The mountain perspective risks naturalizing immutability as inherent when it reflects architectural choices. The perspectival gap reveals that the constraint simultaneously solves one coordination problem (individual privacy control) while creating another (historical information access) and transferring power between platforms (surveillance indexers) and regulators (enforcement infrastructure).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural position — who benefits, who bears costs, and what exit options exist. Data subjects benefit from delisting but face constrained exit options (must use platform workflows or litigate) → d ≈ 0.3–0.5 depending on exit choice. Search engines bear compliance costs with arbitrage options in non-EU markets → d ≈ 0.7 (high extraction). Privacy advocates benefit without bearing costs, have arbitrage options → d ≈ 0.15 (low extraction). Historians bear losses with no exit options → d ≈ 0.95 (maximum extraction). Regulators balance benefits and costs with constrained exit → d ≈ 0.5 (symmetric). The engine derives these from beneficiary/victim declarations and exit options. High d values → high f(d) → high χ for search engines and historians. Low d values → low/negative χ for privacy advocates and platforms earning arbitrage revenue.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION THROUGH PERSPECTIVAL PLURALISM: The mandatrophy question is 'Is RTBF a coordination mechanism or extraction?' The answer is both — it is tangled rope from the analytical view. RTBF coordinates privacy interests (data subjects and privacy advocates benefit from individual control mechanisms). RTBF extracts from platforms (compliance costs are asymmetric) and from historical archives (information access is constrained). The mandatrophy resolves by recognizing that the constraint simultaneously solves one coordination problem while creating others. From the data subject perspective, RTBF is liberatory (removes a snare). From the platform perspective, RTBF is extractive (adds compliance overhead). From the historian perspective, RTBF is destructive (removes coordination on shared historical knowledge). The system is not mislabeled — it is genuinely tangled: the coordination function (privacy control) and extraction function (compliance asymmetry, information suppression) are inseparable. No reframing makes RTBF pure coordination or pure extraction. The constraint's legitimacy depends on whether the coordination benefit (individual privacy control) outweighs the extraction cost (reduced historical access, compliance burden). This is a preference question, not a structural one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_forgotten,
    'What constitutes ''being forgotten'' — delisting from search indices only, or deletion from all digital storage systems?',
    'Case law analysis of ECJ rulings on RTBF scope; comparison of EU implementation vs non-EU countries'' approaches; tracking of information re-emergence across platforms post-delisting',
    'If delisting only: RTBF is coordination (rope) — information persists but is less accessible. If full deletion required: RTBF is unachievable (mountain, false summit). Current interpretation (delisting only) makes RTBF a tangled rope with significant platform extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_forgotten, conceptual, 'Whether RTBF means delisting or permanent deletion').

omega_variable(
    legitimate_interest_carve_out,
    'How should the ''legitimate interest'' and ''public interest'' exceptions be balanced against the right to be forgotten?',
    'Analysis of cases where RTBF is denied on public interest grounds; tracking of which actors (media, historians, law enforcement) successfully invoke exceptions; longitudinal audit of exception grant rates by regulator',
    'If exceptions are broad: RTBF becomes theater (piton) — the right exists but is frequently overridden. If exceptions are narrow: RTBF extraction on historical archives is severe (snare). Current implementation is contested; no consensus exists on where the boundary should sit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_interest_carve_out, preference, 'Balance between RTBF and legitimate interest exceptions').

omega_variable(
    reindexing_through_third_parties,
    'If a data subject successfully delists information from Google but third-party archives, news aggregators, or domain-specific search engines re-index it, has the right to be forgotten been exercised?',
    'Audit of post-delisting information persistence across platforms; tracking of complaint rates when information is re-indexed; analysis of platform cooperation in cross-service delisting',
    'If re-indexing breaks the right: the constraint requires global coordination (mountain, false summit) and is practically unachievable. If delisting from major search engines is sufficient: the right is functional but asymmetric (tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reindexing_through_third_parties, empirical, 'Whether third-party re-indexing violates the right to be forgotten').

omega_variable(
    power_asymmetry_in_enforcement,
    'Can data subjects effectively enforce RTBF against major platforms, or do asymmetric resources and legal access barriers create a de facto extraction mechanism?',
    'Analysis of RTBF dispute resolution timelines; tracking of cases where data subjects win vs platforms win; comparison of enforcement outcomes between wealthy vs economically constrained data subjects; audit of platform appeal rejection rates',
    'If enforcement is symmetric: RTBF is functional coordination (rope/tangled rope). If asymmetric: platforms can strategically non-comply, making RTBF a snare for powerless actors and piton for platforms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(power_asymmetry_in_enforcement, empirical, 'Effectiveness of data subject enforcement mechanisms').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent does awareness of the right to be forgotten (and its inconsistent enforcement) cause self-suppression — people limiting their online activity or data sharing due to fear of permanent indexing?',
    'Survey data on awareness and behavioral change; comparison of online activity levels pre- and post-RTBF awareness; analysis of information avoidance patterns correlated with RTBF publicity',
    'If significant: the suppression is partially internalized — people carry the fear of indexing even when technical options exist. The constraint''s effective suppression is higher than the platform-level measures alone suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Internalization of suppression through RTBF awareness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_right_to_be_forgotten, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rtbf_tr_t0, gdpr_right_to_be_forgotten, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rtbf_tr_t4, gdpr_right_to_be_forgotten, theater_ratio, 4, 0.42).
narrative_ontology:measurement(rtbf_tr_t8, gdpr_right_to_be_forgotten, theater_ratio, 8, 0.48).
narrative_ontology:measurement(rtbf_tr_t12, gdpr_right_to_be_forgotten, theater_ratio, 12, 0.48).

% Extraction over time
narrative_ontology:measurement(rtbf_be_t0, gdpr_right_to_be_forgotten, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(rtbf_be_t4, gdpr_right_to_be_forgotten, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(rtbf_be_t8, gdpr_right_to_be_forgotten, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(rtbf_be_t12, gdpr_right_to_be_forgotten, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_right_to_be_forgotten, identity_coordination).
narrative_ontology:affects_constraint(gdpr_right_to_be_forgotten, data_portability_right).
narrative_ontology:affects_constraint(gdpr_right_to_be_forgotten, consent_withdrawal_mechanisms).
narrative_ontology:affects_constraint(gdpr_right_to_be_forgotten, search_engine_gatekeeping).
narrative_ontology:affects_constraint(gdpr_right_to_be_forgotten, historical_information_access).

% DUAL FORMULATION NOTE:
% The GDPR right to be forgotten decomposes into multiple structurally distinct constraints: (1) individual delisting (data subject control over search indices, ε≈0.40) vs (2) platform compliance obligations (infrastructure and verification costs, ε≈0.72) vs (3) historical information access (archives and research access, ε≈0.65). These share a legal mandate but have different empirical status, different extraction mechanisms, and different stakeholder positions. This story treats RTBF as a tangled rope coordinating all three; upstream stories decompose the individual delisting and historical access problems separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_right_to_be_forgotten, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
