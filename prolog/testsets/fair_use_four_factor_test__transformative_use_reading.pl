% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__transformative_use_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__transformative_use_reading, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fair_use_four_factor_test__transformative_use_reading
 *   human_readable: Fair Use Four-Factor Test: Transformative-Use Reading
 *   domain: legal_theory/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   The transformative-use reading of the fair-use four-factor test is a
 *   doctrinal interpretation that prioritizes the first factor (purpose and
 *   character of use) over the fourth factor (market effect on the original
 *   work). Under this reading, when a secondary use adds 'new meaning,
 *   message, or expression' to the source material, the doctrine grants
 *   strong safe harbor even when market harm to licensing channels is
 *   substantial. This reading has become dominant in US copyright
 *   jurisprudence since Campbell v. Acuff-Rose Music (2014 Cariou v. Prince,
 *   Blanch v. Koons line) but remains contested against creator-centric
 *   readings (which treat fair use as narrow exception) and user-centric
 *   readings (which treat it as affirmative right). The transformative-use
 *   reading redistributes value from original copyright holders toward remix
 *   creators and platforms that host derivative works. It is NOT a mountain
 *   (natural law of culture) but a contingent legal construction with
 *   identifiable beneficiaries (remix culture, platforms, transformative
 *   secondary creators) and identifiable victims (copyright holders whose
 *   licensing revenue is subordinated). The constraint exhibits tangled-rope
 *   structure: genuine coordination function (clarifies which derivative uses
 *   are defensible without pre-licensing) combined with asymmetric extraction
 *   (original creators bear uncompensated cost; secondary creators and
 *   platforms capture new-meaning value).
 *
 * KEY AGENTS:
 *   - Original Copyright Holders (Creator-Centric Coalition): Primary victims under the transformative-use reading; licensing revenue subordinated when transformation is found (powerless/trapped in high-remix domains)
 *   - Remix Artists and Secondary Creators: Primary beneficiaries; transformative use provides safe harbor for derivative works (moderate/constrained — benefit from doctrine but face litigation risk and transformativeness threshold uncertainty)
 *   - User-Generated Content Platforms (YouTube, TikTok, Instagram, etc.): Institutional beneficiaries; transformative-use immunity enables hosting derivative works at massive scale without licensing infrastructure (institutional/arbitrage)
 *   - Performing Rights Organizations (ASCAP, BMI, SESAC): Organized intermediate actors; lose licensing capture on transformative uses while retaining revenue on non-transformative covers and licensed derivatives (organized/mobile)
 *   - Copyright Law Academic Community: Authority structure; scholars defending transformative-use doctrine as essential to cultural production and innovation (institutional/arbitrage)
 *   - Courts Applying Four-Factor Test: Implementing authority; final adjudicators of transformativeness; produce case-by-case decisions with doctrinal rationales (institutional/arbitrage)
 *   - Analytical Observer: Sees the constraint as a contingent legal construction, not a natural law (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.52).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.48).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Fair Use Four-Factor Test: Transformative-Use Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal_theory/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, 'cd4b0fe4-4e9e-4e7f-af52-8a14c427d3f9').
narrative_ontology:cs_kernel_codification('cd4b0fe4-4e9e-4e7f-af52-8a14c427d3f9', fixed_text).
narrative_ontology:cs_authority_grounding('cd4b0fe4-4e9e-4e7f-af52-8a14c427d3f9', extraction).
narrative_ontology:cs_interpretation_layer_present('cd4b0fe4-4e9e-4e7f-af52-8a14c427d3f9').
narrative_ontology:cs_reading_relation('cd4b0fe4-4e9e-4e7f-af52-8a14c427d3f9', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd4b0fe4-4e9e-4e7f-af52-8a14c427d3f9', fair_use_four_factor_test__user_centric_reading, influences).
narrative_ontology:cs_axiom('cd4b0fe4-4e9e-4e7f-af52-8a14c427d3f9', foundational, transformativeness_prioritized_over_market_harm).
narrative_ontology:cs_axiom_status(transformativeness_prioritized_over_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('cd4b0fe4-4e9e-4e7f-af52-8a14c427d3f9', transformativeness_prioritized_over_market_harm, deontological).
narrative_ontology:cs_axiom('cd4b0fe4-4e9e-4e7f-af52-8a14c427d3f9', foundational, cultural_value_of_recontextualization).
narrative_ontology:cs_axiom_status(cultural_value_of_recontextualization, holdable).
narrative_ontology:cs_axiom_grounding('cd4b0fe4-4e9e-4e7f-af52-8a14c427d3f9', cultural_value_of_recontextualization, empirically_contingent).
narrative_ontology:cs_reference_frame('cd4b0fe4-4e9e-4e7f-af52-8a14c427d3f9', balanced_four_factor_analysis).
narrative_ontology:cs_drift_state('cd4b0fe4-4e9e-4e7f-af52-8a14c427d3f9', contemporary_post_2000_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cd4b0fe4-4e9e-4e7f-af52-8a14c427d3f9', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_culture_practitioners).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, user_generated_content_platforms).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, transformative_secondary_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, original_copyright_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, licensing_revenue_streams).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORIGINAL COPYRIGHT HOLDER IN HIGH-REMIX DOMAIN (SNARE) — A creator in a domain where transformative reuse is common (music sampling, image remixing, fan fiction, meme culture) faces zero reliable licensing revenue from derivative works. The transformative-use reading subordinates market harm when 'new meaning' is added, making the copyright holder's ability to control or monetize derivative uses effectively null. Trapped exit — cannot prevent the reuse, cannot demand compensation, cannot exit the domain. Maximum experienced extraction.
constraint_indexing:constraint_classification(fair_use_four_factor_test__transformative_use_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SECONDARY CREATOR / REMIX ARTIST (TANGLED ROPE) — Benefits from the transformative-use doctrine: their derived works are legally defensible as fair use when they add new meaning. But also constrained by uncertainty about what threshold of 'transformativeness' courts will recognize, by the risk of litigation even with a strong fair-use defense, and by market pressures from licensing industries. Genuine coordination benefit (can create without pre-licensing) mixed with asymmetric extraction (original creator bears cost; secondary creator captures new-meaning value).
constraint_indexing:constraint_classification(fair_use_four_factor_test__transformative_use_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: USER-GENERATED CONTENT PLATFORM (ROPE) — Platforms like YouTube, TikTok, Twitch benefit from the transformative-use doctrine by hosting derivative works (covers, remixes, commentary, fan edits) without licensing every source. The doctrine enables coordination: it allows platforms to offer tools for creative reuse while maintaining legal defensibility. Arbitrage exit — platforms can choose alternative models (strict licensing, original-content-only) but remain where transformative-use immunity is strongest. Net beneficiary.
constraint_indexing:constraint_classification(fair_use_four_factor_test__transformative_use_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MUSIC LICENSING INDUSTRY (TANGLED ROPE) — Organized agents (performing rights organizations) benefit from licensing legitimate derivative works (covers, orchestral arrangements, authorized remixes) while losing market capture from sampled music, TikTok lip-syncs, and fan remixes classified as transformative. Coordination benefit exists: the doctrine clarifies which derivative uses require licensing (and thus generate revenue) vs. which do not (safe harbor). But extraction is also present: licensing revenue is subordinated to transformativeness analysis, reducing the industry's ability to monetize reuse. Mobile exit — the industry can lobby for statutory licensing, renegotiate artist agreements, or invest in watermarking technology.
constraint_indexing:constraint_classification(fair_use_four_factor_test__transformative_use_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COPYRIGHT LAW DOCTRINAL STABILITY / LEGACY IP THEORY (PITON) — The transformative-use doctrine has become a ceremonial frame for fair-use analysis: courts and scholars invoke 'transformativeness' as the primary criterion while simultaneously treating the other three factors as subordinate. The ritual persists through institutional inertia despite the doctrine's conceptual instability (what counts as 'new meaning'? courts disagree radically). Legacy IP theory sees this as degraded doctrine maintained through precedent and citation networks, not through coherent justification. Theater_ratio high because the doctrinal apparatus produces the appearance of principled analysis while the outcome often depends on the judge's intuitions about cultural value and remix legitimacy.
constraint_indexing:constraint_classification(fair_use_four_factor_test__transformative_use_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the transformative-use doctrine could be naturalized as an immutable principle: cultural production inherently requires building on prior works; therefore, strict copyright is incompatible with cultural evolution. This perspective sees the doctrine as reflecting an unchangeable law of culture. However, the structural data contradicts this mountain classification — the doctrine is a legal construction with beneficiaries (platforms, remix culture) and victims (copyright holders), not a natural law. False summit detection applies here.
constraint_indexing:constraint_classification(fair_use_four_factor_test__transformative_use_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__transformative_use_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fair_use_four_factor_test__transformative_use_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fair_use_four_factor_test__transformative_use_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, TR),
    TR >= 0.70.

:- end_tests(fair_use_four_factor_test__transformative_use_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.52): Moderate. The transformative-use doctrine does subordinate market harm when transformation is significant, but the doctrine itself includes a balancing framework — all four factors remain nominally in play. The doctrine does not eliminate licensing revenue but redistributes it by narrowing the scope of licensable uses. Extractiveness is higher than pure-coordination rope (0.35) because a real transfer occurs from original creators to secondary creators/platforms, but lower than snare (0.70+) because original creators retain some licensing revenue in non-transformative or marginally-transformative cases. The value depends on how broadly courts define transformativeness, which varies by circuit and by judge. SUPPRESSION (0.48): Moderate-high. Original creators face suppression through: (1) legal uncertainty about transformativeness thresholds (high litigation cost even for strong fair-use claims), (2) power asymmetry (individual creators vs. platforms in litigation), (3) cultural normalization of remix as legitimate expression (making copyright enforcement socially costly), (4) platform terms of service that assume transformative-use safe harbor. However, suppression is not total — copyright holders can still sue, can win cases involving minimal transformation, can pressure platforms through DMCA takedown notices. THEATER RATIO (0.55): Moderate. The doctrine is partly functional (distinguishes transformative from non-transformative uses) and partly performative (the 'new meaning' criterion is applied inconsistently; courts rely heavily on intuitions about cultural value). The theater has increased over time (t0=0.42 → t20=0.60) as the doctrine has become more entrenched and as judges have more routinely invoked transformativeness as the dominant factor while giving less genuine weight to the other three factors. The trend reflects a shift from balanced four-factor analysis toward a dominance hierarchy where transformativeness substitutes for genuine weighing.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps are maximal across the observed positions. Original copyright holders in high-remix domains see a Snare: the doctrine subordinates their market harm and denies them reliable licensing revenue. Secondary creators see Tangled Rope: they gain safe harbor for derivative works but face litigation risk and threshold uncertainty. Platforms see Rope: coordination function (clarity on which uses require licensing) with net benefit (immune from liability for user-uploaded content). PROs see Tangled Rope: coordination benefit (licensing market clarity) but extraction cost (subordinated licensing revenue). Legacy IP theory sees Piton: doctrinal degradation as transformativeness becomes shorthand for judicial intuitions about cultural value rather than principled legal analysis. The analytical observer risks seeing Mountain (cultural production requires recontextualization, therefore strict copyright is impossible) — but this naturalizes a contingent legal reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) differs radically across agent positions: Original copyright holders (powerless/trapped exit) experience d≈0.92 (nearly full targets), deriving maximum extraction via χ. Remix artists (moderate/constrained) experience d≈0.58 (mixed), deriving moderate extraction subordination. Platforms (institutional/arbitrage) experience d≈0.08 (net beneficiaries), deriving negative or minimal χ. PROs (organized/mobile) experience d≈0.45 (symmetric), deriving moderate extraction with some agency. The doctrine's structure ensures that d values concentrate extraction on powerless copyright holders while distributing benefits to institutional platforms and organized secondary creators. This is not an emergent property of the four-factor test but a structural feature of the transformative-use reading specifically — the creator-centric reading would produce different d values favoring copyright holders.
 *
 * MANDATROPHY ANALYSIS:
 *   The transformative-use reading resolves mandatrophy by accepting that the four-factor test produces genuinely different classifications depending on the observer's position and exit options. The constraint does NOT resolve to a single type but to a presheaf of types over different positions. The mandatrophy is epistemic: what counts as 'new meaning' for transformative-use purposes? Courts do not and cannot specify this with precision — the criterion is inherently contextual and judgment-dependent. The resolution mechanism is iterative jurisprudence: each litigated case refines the boundaries of transformative use, but no stable equilibrium is reached because the underlying tension (copyright owner incentives vs. remix culture incentives) cannot be resolved through legal definition alone. The doctrine persists because it provides institutional legitimacy to decisions favoring remix culture without requiring legislative reform of copyright scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformativeness_threshold_indeterminacy,
    'What degree or type of modification constitutes sufficient ''new meaning'' for transformative-use protection? Is the threshold a binary gate or a continuous spectrum?',
    'Meta-analysis of court decisions classifying uses as transformative vs. non-transformative; identification of the common structural features (new context, new audience, new critique) vs. borderline cases (minimal modification, stylistic variation, format shift without semantic change)',
    'If threshold is low (many uses qualify): doctrine becomes robust safe harbor for remix culture. If threshold is high (few uses qualify): doctrine becomes aspirational framing masking case-by-case judicial discretion. Classification may shift from tangled_rope toward snare as indeterminacy forces secondary creators into litigation risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformativeness_threshold_indeterminacy, empirical, 'What constitutes sufficient transformativeness for fair-use protection').

omega_variable(
    market_harm_measurement_gap,
    'Does subordinating market harm (fourth factor) when transformation is high actually reflect the economic reality of licensing markets, or does it rationalize uncompensated value transfer from original creators to secondary creators and platforms?',
    'Quantitative analysis: (1) actual licensing revenue foregone in high-transformation domains (music sampling, fan art, TikTok covers); (2) comparison with alternate-universe scenarios where licensing was mandatory; (3) evidence of market development in jurisdictions with stricter licensing requirements vs. transformative-use safe harbors',
    'If market harm is truly negligible in transformation cases: doctrine reflects legitimate copyright-culture balance. If significant licensing revenue is displaced: doctrine redistributes value from original creators to platforms and secondary creators without statutory compensation mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_measurement_gap, empirical, 'Economic reality of market harm in transformation cases').

omega_variable(
    platform_power_asymmetry_in_transformative_doctrine,
    'Does the transformative-use doctrine disproportionately benefit large platforms (YouTube, TikTok, Instagram) that can absorb litigation risk, relative to individual remix creators and small communities?',
    'Analysis of fair-use litigation patterns: (1) distribution of defendants (platforms vs. individuals); (2) resource asymmetry in legal defense; (3) settlement patterns showing individual creators silenced by threat of litigation even with strong fair-use arguments; (4) empirical impact on diverse creator participation by platform size and legal resources',
    'If asymmetry is severe: doctrine becomes a shield for platform power rather than genuine public-access tool. Classification may shift toward snare for individual creators while remaining rope/tangled_rope for institutional platforms. False beneficiary identification (platforms, not creators) may be required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_power_asymmetry_in_transformative_doctrine, empirical, 'Whether transformative-use doctrine benefits platforms more than creators').

omega_variable(
    reading_identity_underspecification,
    'This is one reading of the fair-use four-factor-test kernel. What structural commitments define the transformative-use reading as distinct from the creator-centric reading (which treats fair use as narrow exception) and the user-centric reading (which treats fair use as affirmative user right)?',
    'Doctrinal analysis: identify the reading''s core normative claim (transformativeness prioritized over property control), the epistemic premises that ground it (cultural value from recontextualization is paramount), and the authority structures that instantiate it (courts emphasizing transformativeness in factor analysis; copyright scholars defending the doctrine as culturally vital).',
    'If the reading''s axioms are properly distinguished: the three readings coexist as live jurisprudential options. If axioms collapse into shared commitments: the reading is not genuinely distinct from creator-centric or user-centric. Classification of the contest kernel itself depends on whether reading_relations should be forecloses, coexists_with, or influences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_underspecification, conceptual, 'Structural distinctness of transformative-use reading from sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fairuse_transform_theater_t0, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fairuse_transform_theater_t10, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 10, 0.53).
narrative_ontology:measurement(fairuse_transform_theater_t20, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 20, 0.6).

% Extraction over time
narrative_ontology:measurement(fairuse_transform_extract_t0, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fairuse_transform_extract_t10, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(fairuse_transform_extract_t20, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fairuse_transform_suppress_t0, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fairuse_transform_suppress_t10, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(fairuse_transform_suppress_t20, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, identity_coordination).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, sampling_licensing_market_enclosure).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, platform_derivative_content_immunity).

% DUAL FORMULATION NOTE:
% The transformative-use reading is one of three readings of the fair-use four-factor-test kernel. All three readings share the same statutory text (17 U.S.C. § 107) and the same factual domains (copyright disputes) but differ in their weighting of the four factors and their underlying commitment to creator incentives vs. public access vs. transformative culture. This story models the transformative-use reading specifically. Sibling stories model the creator-centric and user-centric readings. Network links indicate which constraint readings influence each other: the transformative-use reading directly affects downstream constraints on sampling markets and platform immunity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_four_factor_test__transformative_use_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
