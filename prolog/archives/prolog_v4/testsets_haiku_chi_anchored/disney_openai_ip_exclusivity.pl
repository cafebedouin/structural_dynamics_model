% ============================================================================
% CONSTRAINT STORY: disney_openai_ip_exclusivity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_disney_openai_ip_exclusivity, []).

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
 *   constraint_id: disney_openai_ip_exclusivity
 *   human_readable: Exclusive IP Licensing for Generative AI Training (Disney/OpenAI)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   In a 2025 scenario, Disney invests $1 billion in OpenAI and grants
 *   exclusive access to its top-200 character IP library for training the
 *   Sora video generation model. This constraint exhibits classic
 *   tangled-rope structure: it simultaneously coordinates Disney and OpenAI
 *   toward a shared goal (accelerating Sora's character generation
 *   capability) while extracting value from competing AI developers and
 *   independent artists who cannot access the same training data. The
 *   exclusivity mechanism is the extraction lever — it suppresses
 *   alternatives by concentrating training resources. The constraint's
 *   evolution shows rising extractiveness (0.38 → 0.62 over 4 years) as
 *   Sora's performance advantage compounds, coupled with declining theater
 *   ratio (0.52 → 0.44) as the licensing agreement becomes functionally
 *   effective rather than performative. However, the theater ratio remains
 *   moderate because the fundamental enforcement problem — ensuring Disney's
 *   IP contribution is not reverse-engineered from model weights — is
 *   essentially unsolvable through legal contracts alone. The constraint's
 *   stability depends on three irreducible uncertainties: (1) attribution
 *   (can Disney's specific contribution be isolated?), (2) fair-use
 *   redefinition (will courts exempt AI training from copyright law?), and
 *   (3) open-source convergence (can competitors build equivalent systems
 *   without the exclusive data?).
 *
 * KEY AGENTS:
 *   - OpenAI (Institutional/Arbitrage): Primary beneficiary — receives $1B capital, exclusive training data, and market-dominant Sora capability; can exit by finding alternative character datasets (arbitrage).
 *   - Disney (Institutional/Constrained): Primary beneficiary + victim — benefits from $1B injection and licensing revenue, but suppresses own use of IP elsewhere; constrained by exclusive licensing agreement.
 *   - Competing AI Developers (Moderate/Constrained): Primary victims — excluded from top-200 IP library; face 2-5 year development lag for character-rich video models; constrained by Disney's licensing monopoly.
 *   - Independent Character Artists (Powerless/Trapped): Secondary victims — character designs implicitly harvested through Disney library; no consent, no compensation, no exit pathway.
 *   - Regulatory Bodies (Organized/Mobile): Observers building alternative pathways — fair-use doctrine, antitrust action, training-rights frameworks; have agency to reduce exclusivity's force.
 *   - Analytical Observer (Analytical/Analytical): Sees the constraint as tangled coordination-extraction; the deal's coordination benefit depends entirely on extraction from non-licensees.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(disney_openai_ip_exclusivity, 0.62).
domain_priors:suppression_score(disney_openai_ip_exclusivity, 0.68).
domain_priors:theater_ratio(disney_openai_ip_exclusivity, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(disney_openai_ip_exclusivity, extractiveness, 0.62).
narrative_ontology:constraint_metric(disney_openai_ip_exclusivity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(disney_openai_ip_exclusivity, theater_ratio, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(disney_openai_ip_exclusivity, tangled_rope).
narrative_ontology:human_readable(disney_openai_ip_exclusivity, "Exclusive IP Licensing for Generative AI Training (Disney/OpenAI)").
narrative_ontology:topic_domain(disney_openai_ip_exclusivity, "technological/economic").

domain_priors:requires_active_enforcement(disney_openai_ip_exclusivity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(disney_openai_ip_exclusivity, openai_shareholders).
narrative_ontology:constraint_beneficiary(disney_openai_ip_exclusivity, disney_executives).
narrative_ontology:constraint_beneficiary(disney_openai_ip_exclusivity, sora_product_users).
narrative_ontology:constraint_victim(disney_openai_ip_exclusivity, competing_ai_developers).
narrative_ontology:constraint_victim(disney_openai_ip_exclusivity, independent_artists).
narrative_ontology:constraint_victim(disney_openai_ip_exclusivity, character_licensing_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT CHARACTER ARTISTS (SNARE) — Cannot exit the constraint. Their character designs and stylistic innovations are implicitly harvested through Disney's top-200 IP library. No consent, no compensation, no alternative training pathways for competitive video models. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈1.04.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING AI DEVELOPERS (SNARE) — Face severe extraction. Excluded from the top-200 IP library that accelerates video generation quality. Cannot train Sora-equivalent models without: (a) building comparable character-rich datasets years behind, or (b) licensing the same IP at monopoly rates from Disney. d≈0.88, f(d)≈1.33, σ=1.2 → χ≈0.98.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: OPENAI (EXCLUSIVE LICENSEE) (ROPE) — Primary beneficiary. Solves critical coordination problem: acquiring high-quality, legally-cleared character training data at scale. The $1B investment structures a clean coordination mechanism — Disney provides access, OpenAI builds infrastructure, both capture value. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary through arbitrage (can exit by finding alternative data; Disney cannot exit by licensing to competitors without breach).
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISNEY (LICENSOR AND SHAREHOLDER) (TANGLED ROPE) — Hybrid. Coordination: secures $1B capital injection + revenue share + strategic partnership. Extraction: must suppress internal use of IP elsewhere (cannot license to Meta, Google, or other training partners), creating internal opportunity cost. Also benefits as OpenAI shareholder from Sora's market dominance. d≈0.45, f(d)≈0.43, σ=1.2 → χ≈0.32. Mixed incentives: coordination gain from capital + licensing revenue, extraction cost from suppression of alternatives.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AND STANDARDS BODIES (SCAFFOLD) — See this constraint as a temporary monopoly power grab that will decay as regulatory pressure mounts. Fair-use doctrine, AI training liability frameworks, and antitrust action (EU, FTC) are building alternative pathways that reduce exclusivity's value. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.34. Organized agents (courts, regulators) have agency and see a sunset: within 5-10 years, legal redefinition of training rights will erode Disney's exclusive leverage.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL ENTERTAINMENT STUDIO LICENSING MODEL (PITON) — The exclusive licensing framework is largely performative theater applied to a new domain. Traditional studio licensing (franchise agreements, theatrical distribution) persists as the assumed model for AI training, despite growing functional decay: (a) training data value is diffuse (cannot isolate Disney character contribution to Sora's performance), (b) licensing terms cannot prevent derivative training (once Disney IP is in model weights, no enforcement), (c) exclusive terms rapidly become obsolete as open-source models catch up. theater_ratio=0.44 reflects moderate functional decay. The licensing model persists through institutional habit, not because it effectively controls AI training. Supervisory structure (lawyers, contracts, IP departments) is theatrical overhead that cannot prevent the actual technical problems. d≈0.12, f(d)≈-0.02, σ=1.0 → χ≈-0.00.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational viewpoint, the constraint is a hybrid coordination-extraction mechanism designed to concentrate AI capability (coordination: Disney/OpenAI partnership accelerates Sora's development) while suppressing competitor access (extraction: non-licensees cannot build comparable systems). The analytical observer sees both functions operating simultaneously and structurally linked. d≈0.70, f(d)≈1.13, σ=1.2 → χ≈0.85. The constraint is neither pure coordination (because exclusivity harms ecosystem) nor pure extraction (because it does enable real capability). It is tangled: the coordination only works by extracting from competitors.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(disney_openai_ip_exclusivity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(disney_openai_ip_exclusivity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(disney_openai_ip_exclusivity, TR),
    TR >= 0.70.

:- end_tests(disney_openai_ip_exclusivity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The constraint extracts from competing developers by denying access to accelerated training data. Disney's top-200 IP is a rare, high-value asset (iconographic characters with decades of context), and exclusive access creates a 2-5 year performance gap for competitors. However, the extraction is not maximal (would be 0.75+) because: (a) open-source alternatives are rapidly advancing, (b) fair-use doctrine may eventually permit training on copyrighted data, (c) the character contribution to Sora's overall performance is technically inseparable from OpenAI's architecture, making enforcement difficult. The trajectory shows rising extractiveness because early exclusivity advantage compounds: Sora's market dominance creates higher switching costs for users, increasing the value of Disney's training data advantage. Suppression (0.68): High. Competing developers face severe barriers: they lack access to equivalent character IP, Disney's licensing rates for similar access would be prohibitive (estimated $500M+), and career/funding risk accompanies investing years in slower model development. Regulatory pathways (fair-use litigation, antitrust action) exist but are slow (5-10 year timelines). Theater ratio (0.44): Moderate. The licensing agreement has some functional content (Disney does provide access to labeled, high-quality IP data with legal clearance), but significant performative elements: licensing contracts cannot prevent competitors from independently creating Disney-like characters, cannot prevent reverse-engineering of Disney's stylistic contributions from weights, and assume that 'character IP' is a transferable asset (it is not — the asset is OpenAI's ability to generate Disney-like images, which is architecture, not data).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a full perspectival spectrum. OpenAI sees Rope (coordination problem solved: access to rare IP + capital + partnership). Disney sees Tangled Rope (mixed coordination benefit and extraction cost from suppression). Competing developers see Snare (extraction with no coordination benefit for them). Independent artists see Snare (their work is harvested without consent). Regulatory bodies see Scaffold (temporary monopoly being dismantled by litigation and redefinition). The traditional licensing model sees Piton (performative theater applied to an unsolvable technical problem). The analytical observer sees Tangled Rope (the coordination IS the extraction; you cannot separate them). The perspectival gap is driven by differential exit options: OpenAI has arbitrage (can find alternative data sources), Disney is constrained (cannot license to others), competitors are constrained (cannot access the exclusive data), artists are trapped (cannot prevent IP harvesting), and regulators are mobile (can rewrite the rules).
 *
 * DIRECTIONALITY LOGIC:
 *   OpenAI (Institutional/Arbitrage): Beneficiary + arbitrage exit → d≈0.08, f(d)≈-0.10. Net beneficiary. Disney (Institutional/Constrained): Beneficiary + victim (of own constraint) + constrained exit → d≈0.45, f(d)≈0.43. Mixed: benefits from capital/revenue but suppresses own alternatives. Competing developers (Moderate/Constrained): Victims + constrained exit → d≈0.88, f(d)≈1.33. High extraction: excluded from rare asset, cannot easily exit or rebuild. Independent artists (Powerless/Trapped): Victims + trapped exit → d≈0.92, f(d)≈1.40. Maximum extraction: work is used without consent or compensation, no organized exit. Regulators (Organized/Mobile): Analytical observers with agency → d≈0.50, f(d)≈0.65. Neutral position; they can shift the constraint's rules. Licensing model (Institutional/Constrained): Theater-dominant piton; d is irrelevant — the piton classification comes from functional decay (0.44 theater ratio) and inertial persistence, not from high effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy by distinguishing coordination from extraction through structural asymmetry. The Disney-OpenAI partnership creates genuine coordination (both gain capital, capability, revenue). But this coordination is inseparable from extraction because the partnership's benefit depends on excluding competitors — the 'coordination' only works because it extracts. The Tangled Rope type correctly captures this: there is a real coordination function (capital deployment, IP access, capability acceleration) AND a real extraction function (competitors denied access, artists denied consent, licensing monopoly). The mandate is resolved by recognizing that some constraints are STRUCTURALLY tangled — you cannot remove the extraction without destroying the coordination. This is not a measurement problem or an observer bias; it is the constraint's design. If Disney offered the same IP access to all AI developers at fair-market rates, the tangling would dissolve: OpenAI and competitors would both benefit (Rope), but Disney would lose exclusive leverage. The Tangled Rope classification stands because the constraint is deliberately designed to couple coordination and extraction. The mandatrophy is NOT resolved by reducing to a single type; it is resolved by accepting that this is genuinely a hybrid mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    training_data_attribution,
    'Can Disney''s contribution to Sora''s performance be isolated and measured? Or is the character IP value diffuse and inseparable from OpenAI''s architecture improvements?',
    'Ablation studies comparing Sora trained with vs without Disney IP; performance metrics on character generation tasks; reverse engineering analysis of model weights for Disney-specific features.',
    'If measurable: exclusivity deal has real enforcement (Disney can monitor compliance). If diffuse: enforcement is theater; Disney cannot verify that competitors aren''t achieving similar performance through public data, making the licensing agreement legally vulnerable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(training_data_attribution, empirical, 'Whether Disney''s IP contribution to Sora is isolable or diffuse').

omega_variable(
    fair_use_redefinition,
    'Will courts (US, EU) redeclassify AI training on copyrighted IP as fair use, rendering exclusive licensing agreements moot?',
    'Landmark litigation outcomes (Andersen v OpenAI, Getty Images v Stability AI, or EU fair-use precedents); legislative changes to Copyright Directive or DMCA; regulatory guidance from USPTO or relevant bodies.',
    'If fair use expands: exclusivity clause becomes unenforceable; Disney''s licensing advantage vanishes; constraint transitions from Tangled Rope to Scaffold with immediate sunset (litigation would establish sunset date). If fair use contracts: exclusivity maintains force; constraint remains Tangled Rope or upgrades toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fair_use_redefinition, empirical, 'Legal redefinition of AI training as fair use').

omega_variable(
    open_source_convergence,
    'Will open-source AI models (Llama, Mistral, or successors) achieve feature parity with Sora on character/video generation without proprietary Disney training data?',
    'Benchmarking open-source models against Sora on character design diversity, animation consistency, and style transfer; publication pace and architecture advances in open-source video generation; industry adoption of open-source alternatives.',
    'If convergence occurs within 2-3 years: Disney''s exclusive data loses economic value rapidly; competitors can build Sora-equivalents without licensing; constraint degrades to Piton (theater). If open-source lags >5 years: exclusivity maintains real leverage; constraint stabilizes as Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_convergence, empirical, 'Open-source model convergence with proprietary video generation').

omega_variable(
    internal_disney_opportunity_cost,
    'How much value does Disney forgo by restricting its own use of top-200 IP for training alternative models (Disney+, theme park AI, merchandise design)?',
    'Internal Disney financial analysis (if leaked); inference from licensing structure and revenue implications; comparison with hypothetical unrestricted scenario.',
    'If opportunity cost is high (>$500M NPV): Disney''s net benefit from the deal becomes marginal; constraint is extraction on Disney itself (self-imposed snare). If low: Disney''s coordination benefit remains dominant; constraint is genuine Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_disney_opportunity_cost, empirical, 'Disney''s internal opportunity cost from IP exclusivity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(disney_openai_ip_exclusivity, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(disney_openai_tr_t0, disney_openai_ip_exclusivity, theater_ratio, 0, 0.52).
narrative_ontology:measurement(disney_openai_tr_t2, disney_openai_ip_exclusivity, theater_ratio, 2, 0.48).
narrative_ontology:measurement(disney_openai_tr_t4, disney_openai_ip_exclusivity, theater_ratio, 4, 0.44).

% Extraction over time
narrative_ontology:measurement(disney_openai_be_t0, disney_openai_ip_exclusivity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(disney_openai_be_t2, disney_openai_ip_exclusivity, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(disney_openai_be_t4, disney_openai_ip_exclusivity, base_extractiveness, 4, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(disney_openai_ip_exclusivity, resource_allocation).
narrative_ontology:affects_constraint(disney_openai_ip_exclusivity, ai_training_data_access).
narrative_ontology:affects_constraint(disney_openai_ip_exclusivity, generative_model_capability_concentration).
narrative_ontology:affects_constraint(disney_openai_ip_exclusivity, copyright_enforcement_for_ml).

% DUAL FORMULATION NOTE:
% The Disney-OpenAI exclusivity constraint decomposes into three structural claims with different epsilon values: (1) resource allocation for capital/IP (epsilon~0.30, Rope focus), (2) competitive advantage through data concentration (epsilon~0.62, Tangled Rope focus), and (3) copyright/fair-use precedent setting (epsilon~0.75, Snare focus from competitors' perspective). This story addresses the data-concentration claim; the network links connect to upstream capital-allocation dynamics and downstream fair-use litigation impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(disney_openai_ip_exclusivity, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
