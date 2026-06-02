% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__censorship_mechanism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__censorship_mechanism_reading, []).

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
 *   constraint_id: article17_erasure_right__censorship_mechanism_reading
 *   human_readable: Article 17 Right to Erasure as Strategic Censorship Mechanism
 *   domain: technology_governance/data_protection_law/competition_policy
 *
 * SUMMARY:
 *   Article 17 of the General Data Protection Regulation (GDPR) grants data
 *   subjects the right to erasure ('right to be forgotten') under specified
 *   conditions: when personal data is no longer necessary for its original
 *   purpose, when the subject withdraws consent, or when the data is
 *   unlawfully processed. This reading instantiates a structural critique:
 *   the same mechanism that coordinates legitimate privacy protection can be
 *   weaponized as a prior-restraint substitute by strategic actors seeking to
 *   suppress negative coverage, documentary evidence, or competitive
 *   intelligence. The reading treats erasure as a censorship mechanism where
 *   the asymmetry lies not in who holds the legal right (the subject does)
 *   but in who benefits from its exercise (strategic suppressors, not
 *   privacy-conscious individuals). The constraint exhibits tangled_rope
 *   structure: genuine privacy coordination exists (data subjects deserve
 *   exit from unwanted retention) alongside asymmetric suppression (bad-faith
 *   requesters extract documentary suppression; platforms extract liability
 *   reduction; journalists and archivists bear costs with no reciprocal
 *   benefit). The suppression trajectory shows intensification over the
 *   interval (t0=0.52 → t6=0.68) reflecting rising sophistication in
 *   weaponization and DPA enforcement burden. The extractiveness trajectory
 *   (t0=0.35 → t6=0.58) shows the constraint transitioning from pure
 *   coordination (early cases, straightforward privacy claims) to hybrid
 *   extraction (rising sophisticated bad-faith requests and erosion of public
 *   interest exceptions).
 *
 * KEY AGENTS:
 *   - Strategic Erasure Requesters: Primary beneficiaries (powerful/mobile) — weaponize the right to suppress negative coverage, competitive research, or documentary evidence; extract reputation protection and information asymmetry
 *   - Platform Operators: Secondary beneficiaries (institutional/arbitrage) — reduce content moderation costs and liability by complying with erasure requests; extract liability reduction while maintaining plausible coordination frame
 *   - Journalists / Investigative Media: Primary victims (moderate/constrained) — lose evidentiary trail and public-interest documentation; constrained by platform hosting and search result erasure; bear suppression costs with no offsetting benefit
 *   - Archivists / Historical Researchers: Primary victims (moderate/constrained) — institutional and independent archivists lose access to historical records; bear epistemic damage to public record; constrained by DPA compliance obligations
 *   - Data Protection Authorities (DPAs): Institutional enforcer (organized/constrained) — tasked with distinguishing legitimate privacy claims from strategic suppression; lack capacity for pattern detection; enforcement is costly and litigation-averse, defaulting to erasure compliance
 *   - Legitimate Data Subjects: Genuine beneficiaries (moderate/mobile) — those with legitimate privacy interests (medical data, financial information, identity-linked personal data); experience the right as pure coordination with no suppression cost
 *   - Speech Commons / Public Record Integrity: Victim (powerless/trapped) — abstract collective good bearing cost of retroactive erasure; cannot organize or exit; no institutional advocate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__censorship_mechanism_reading, 0.58).
domain_priors:suppression_score(article17_erasure_right__censorship_mechanism_reading, 0.68).
domain_priors:theater_ratio(article17_erasure_right__censorship_mechanism_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__censorship_mechanism_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__censorship_mechanism_reading, "Article 17 Right to Erasure as Strategic Censorship Mechanism").
narrative_ontology:topic_domain(article17_erasure_right__censorship_mechanism_reading, "technology_governance/data_protection_law/competition_policy").

domain_priors:requires_active_enforcement(article17_erasure_right__censorship_mechanism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__censorship_mechanism_reading, '1d05e842-5298-4821-86cd-e065f3047b4f').
narrative_ontology:cs_kernel_codification('1d05e842-5298-4821-86cd-e065f3047b4f', formalized).
narrative_ontology:cs_authority_grounding('1d05e842-5298-4821-86cd-e065f3047b4f', extraction).
narrative_ontology:cs_interpretation_layer_present('1d05e842-5298-4821-86cd-e065f3047b4f').
narrative_ontology:cs_reading_relation('1d05e842-5298-4821-86cd-e065f3047b4f', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d05e842-5298-4821-86cd-e065f3047b4f', article17_erasure_right__competitive_moat_reading, influences).
narrative_ontology:cs_axiom('1d05e842-5298-4821-86cd-e065f3047b4f', foundational, erasure_subject_to_weaponization).
narrative_ontology:cs_axiom_status(erasure_subject_to_weaponization, holdable).
narrative_ontology:cs_axiom_grounding('1d05e842-5298-4821-86cd-e065f3047b4f', erasure_subject_to_weaponization, empirically_contingent).
narrative_ontology:cs_axiom('1d05e842-5298-4821-86cd-e065f3047b4f', secondary, public_interest_exception_erodes_under_litigation_burden).
narrative_ontology:cs_axiom_status(public_interest_exception_erodes_under_litigation_burden, holdable).
narrative_ontology:cs_axiom_grounding('1d05e842-5298-4821-86cd-e065f3047b4f', public_interest_exception_erodes_under_litigation_burden, empirically_contingent).
narrative_ontology:cs_reference_frame('1d05e842-5298-4821-86cd-e065f3047b4f', privacy_protection_framework).
narrative_ontology:cs_drift_state('1d05e842-5298-4821-86cd-e065f3047b4f', contemporary_strategic_weaponization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1d05e842-5298-4821-86cd-e065f3047b4f', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, strategic_erasure_requesters).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, platforms_reducing_liability).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, journalists_archivists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, public_record_integrity).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, speech_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARCHIVIST/CUSTODIAN (SNARE) — Cannot refuse erasure requests without legal exposure; bears full suppression cost with no countervailing benefit. No exit from GDPR/DPA jurisdiction; erasure requests destroy evidentiary value of archived materials retroactively. Maximum extraction from perspective of those stewarding historical record.
constraint_indexing:constraint_classification(article17_erasure_right__censorship_mechanism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT JOURNALIST (SNARE) — Constrained by platform hosting, search results, and archival access; faces erosion of evidentiary trail they rely on for investigative reporting. No arbitrage exit (must publish within reach of requesters). Erasure requests eliminate sourcing materials and public interest documentation retroactively. High suppression cost; minimal coordination benefit.
constraint_indexing:constraint_classification(article17_erasure_right__censorship_mechanism_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: DATA SUBJECT / LEGITIMATE PRIVACY CLAIMANT (ROPE) — Genuine beneficiary of erasure right where personal data is truly irrelevant (medical records, financial data, genuine personal information). Experiences the right as pure coordination: enabling exit from unwanted data retention. Net beneficiary; low extraction; pure coordination function.
constraint_indexing:constraint_classification(article17_erasure_right__censorship_mechanism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STRATEGIC REQUESTER / BAD-FAITH ACTOR (TANGLED ROPE) — Weaponizes erasure right to suppress negative coverage, competitive intelligence, or documentary evidence. Experiences the right as both coordination mechanism (legitimate privacy framing) AND extraction tool (suppression of disfavored information). Extracts value from retroactive deletion while maintaining plausible legitimacy. Mobile exit (can shift jurisdictions, change identity) but does not need it — the right is asymmetrically in their favor.
constraint_indexing:constraint_classification(article17_erasure_right__censorship_mechanism_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM OPERATOR (TANGLED ROPE) — Benefits from erasure compliance as liability reduction (removes contentious content without appearing to censor) while maintaining plausible coordination function (respecting privacy rights). Experiences the right as hybrid: genuine privacy coordination in some cases, but also as extraction mechanism reducing moderation costs and content disputes. Arbitrage exit available (comply in EU, differ elsewhere) but leverages asymmetry strategically.
constraint_indexing:constraint_classification(article17_erasure_right__censorship_mechanism_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY ENFORCEMENT (PITON) — DPA enforcement of erasure compliance is largely theatrical: determining 'necessity' vs 'public interest exception' is fact-dependent, litigation is costly, and bad-faith requests are hard to identify in enforcement record. The apparatus certifies itself as protecting privacy while often enabling suppression. Theater persists due to institutional inertia and jurisdictional complexity; genuine verification of bad faith or public interest is rare in enforcement practice.
constraint_indexing:constraint_classification(article17_erasure_right__censorship_mechanism_reading, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: CIVILIZATIONAL/ANALYTICAL (MOUNTAIN) — From a civilizational horizon, privacy is an inalienable right and personal data erasure is an immutable commitment to human dignity. This perspective naturalizes erasure as a fundamental constraint on data retention, beyond policy negotiation. However, the structural data reveals this as a false summit: the constraints on erasure (public interest exceptions, necessity tests) are contingent policy choices, not laws of nature. The mountain framing obscures the choice architecture that enables weaponization.
constraint_indexing:constraint_classification(article17_erasure_right__censorship_mechanism_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__censorship_mechanism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article17_erasure_right__censorship_mechanism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article17_erasure_right__censorship_mechanism_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article17_erasure_right__censorship_mechanism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(article17_erasure_right__censorship_mechanism_reading, TR),
    TR >= 0.70.

:- end_tests(article17_erasure_right__censorship_mechanism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58, tangled rope threshold): The constraint coordinates genuine privacy protection for data subjects (ε_pure_coordination ≈ 0.05) while simultaneously enabling suppression for strategic requesters (ε_weaponized ≈ 0.72). The measured value reflects the actual practice: a minority of erasure requests are legitimate privacy claims; a growing minority are strategic suppression attempts; the majority fall in a gray zone (plausibly legitimate privacy, plausibly strategic). The DPA lacks capacity to distinguish, so compliance defaults to erasure, meaning the constraint functions primarily as suppression mechanism in practice despite its privacy-protection framing. The trajectory shows extractiveness rising from 0.35 (early period, straightforward cases) to 0.58 (current state, sophisticated requests, erosion of exceptions) as bad-faith actors learn to game the system. Suppression (0.68, high): The constraint suppresses alternatives available to journalists and archivists — they cannot simply ignore erasure requests (legal exposure), cannot appeal easily (litigation costs), cannot distinguish bad faith (DPA enforcement is opaque). Platform operators face high compliance costs but not suppression cost (they benefit). Legitimate privacy subjects face low suppression cost (the right works as intended). The weighted average is high because the constraint's primary effect is now suppression for those defending the speech commons. Theater (0.55, moderate): DPA enforcement of erasure involves plausible legitimacy ritual (necessity tests, public interest exceptions) but the ritual is largely performative — determining bad faith is epistemically hard, litigation is costly, so enforcement defaults to erasure compliance. The ritual matters (it's not pure exercise of power) but its verification function is weak.
 *
 * PERSPECTIVAL GAP:
 *   Seven perspectives reveal the full structural complexity. Legitimate data subjects (Perspective 3, Rope) experience pure coordination: the right solves their genuine privacy problem. Strategic suppressors (Perspective 4, Tangled Rope) experience hybrid coordination-extraction: they frame erasure as privacy exercise while extracting suppression. Journalists and archivists (Perspectives 1–2, Snare) experience pure extraction: the same right suppresses their work with no offsetting benefit. Platform operators (Perspective 5, Tangled Rope) experience it as liability reduction bundled with plausible privacy framing. DPA enforcers (Perspective 6, Piton) see it as performative: the ritual of necessity testing and exception-granting looks functional but actually defaults to erasure, making the apparatus itself theatrical. The civilizational view (Perspective 7, Mountain) risks naturalizing the erasure right as immutable privacy law when it is actually a policy choice with exploitable asymmetries. The perspectival gap is diagnostic: if all perspectives agreed on Snare, the constraint would be pure extraction; if all agreed on Rope, it would be pure coordination. The diversity of types reveals that the constraint's character depends entirely on which agent's interests dominate practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by structural position relative to the erasure mechanism. Legitimate data subjects (moderate/mobile) have d ≈ 0.15-0.25 (net beneficiary, low/negative extraction). Strategic suppressors (powerful/mobile) have d ≈ 0.70-0.80 (target the mechanism for suppression, high extraction). Journalists (moderate/constrained) have d ≈ 0.75-0.85 (trapped by erasure compliance, high extraction). Archivists (moderate/constrained) have d ≈ 0.80-0.90 (epistemically damaged, very high extraction). Platform operators (institutional/arbitrage) have d ≈ 0.10-0.20 (beneficiaries, arbitrage exit via compliance strategy). The engine derives d from these structural positions and applies sigmoid f(d) to determine experienced chi. Beneficiaries experience low χ; victims experience high χ. The platform perspective is complicated by arbitrage exit: they can geographically arbitrage (comply in EU, differ elsewhere) so their structural exit capacity reduces their apparent d, even though they benefit from erasure. This is captured by the directionality derivation: institutional + arbitrage → d ≈ 0.15 despite being beneficiary, because their exit capacity changes the power dynamic.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the tangled_rope classification captures the empirical hybrid structure: legitimate privacy coordination genuinely exists (data subjects benefit), but asymmetric suppression is now the dominant effect in practice (journalists, archivists, speech commons bear costs). The classification is not 'which type is correct?' but 'which function is currently dominant and for whom?' Legitimate privacy claims follow a Rope trajectory (coordination with no extraction). Strategic suppression requests follow a Snare trajectory (extraction with no coordination). The measured constraint averages across both populations, yielding tangled_rope. As the proportion of bad-faith requests rises (shown in the suppression trajectory), the constraint approaches pure Snare. As DPA capacity to detect and block bad-faith requests improves (counterfactual), the constraint approaches pure Rope. The current state is genuinely hybrid, and the tangled_rope type is the accurate classification for that state. The mandatrophy is resolved by recognizing that the constraint's structure enables both readings simultaneously — the same mechanism can coordinate privacy and suppress speech depending on who wields it. This is precisely what tangled_rope is designed to capture: genuine coordination function coexisting with asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bad_faith_detection_boundary,
    'What structural markers distinguish legitimate privacy claims from strategic suppression attempts using the same erasure mechanism?',
    'Pattern analysis of erosure requesters: prior requests, timing relative to negative coverage, identity masking, cross-jurisdictional patterns; comparison with genuinely sensitive personal data (medical, financial, identity-linked)',
    'If bad faith is structurally detectable: tangled_rope classification with clear victim/beneficiary distinction holds. If bad faith is epistemically opaque: the constraint is closer to snare (suppression mechanism wins by default, since verification failure defaults to erasure compliance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bad_faith_detection_boundary, empirical, 'Detectability of bad-faith erasure requests vs. legitimate privacy claims').

omega_variable(
    public_interest_exception_collapse,
    'Do DPA public interest exceptions (journalism, historical research, archival) actually constrain erasure in practice, or do they collapse under erasure request pressure due to litigation costs and burden of proof?',
    'Analysis of DPA case law and enforcement records: rate of public interest exception grants vs. erasure grants; cost of litigation vs. compliance cost; precedent strength of exceptions across jurisdictions',
    'If exceptions are robust: extractiveness drops to 0.35-0.40 (rope/scaffold hybrid with real exit for media). If exceptions collapse: extractiveness rises to 0.68+ (snare/tangled_rope with no real exit for victims). This is the pivot point between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_interest_exception_collapse, empirical, 'Enforceability of public interest exceptions to erasure rights').

omega_variable(
    reading_kernel_distinction,
    'Is Article 17 a fundamental privacy right (privacy_fundamental_reading) or a mechanism that can be weaponized for suppression (censorship_mechanism_reading)? Is this distinction resolvable or does it depend on which agents'' interests are privileged?',
    'Legislative intent analysis (GDPR drafting history, DPA guidance documents); empirical pattern of bad-faith requests and enforcement outcomes; counterfactual: would the same right be written identically if legislators had explicitly predicted strategic weaponization? If yes, the reading is choice-enabled rather than foreclosed.',
    'If the readings forecast, this reading (censorship_mechanism) remains live; the privacy_fundamental_reading describes the right''s intent. If empirical bad faith drives the distinction: this reading becomes the structural outcome, privacy_fundamental becomes the aspirational framing. If neither reading is foreclosed: both coexist; the constraint''s type depends on which reading the analyst adopts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Whether Article 17''s actual structure permits weaponization or the weaponization is contingent policy failure').

omega_variable(
    competitive_moat_interaction,
    'Does Article 17 function as a competitive advantage for large platforms (who can absorb compliance costs and weaponize erasure against competitors'' archives) vs. small platforms or archives? Does this create a competitive moat that reinforces the censorship mechanism?',
    'Empirical analysis of erasure compliance costs by platform size; correlation between platform market share and erasure request volume; analysis of archive/competitor targeting patterns; cost-benefit across scale',
    'If large platform advantage exists: extractiveness rises to 0.65+ (snare for competitors). If costs distribute evenly: extractiveness stays at 0.58 (tangled rope with symmetric burden). This maps to the competitive_moat_reading''s core concern.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competitive_moat_interaction, empirical, 'Differential compliance burden and competitive advantage from erasure weaponization').

omega_variable(
    archive_destruction_irreversibility,
    'Is erasure from public indexes and platforms sufficient to satisfy the right (practical erasure), or does the right require deletion from all archives, Internet Archive, and historical databases (epistemic erasure)? What is the scope of the right in practice vs. law?',
    'DPA guidance interpretation; analysis of enforcement outcomes (what counts as ''erased''); Internet Archive''s opt-out vs. legal deletion obligations; cross-jurisdictional variation in scope',
    'If practical erasure suffices: archives can retain records off-index (low suppression for historians). If epistemic erasure is required: archives must destroy historical records (high suppression, severe for public record integrity, extractiveness rises to 0.72+). This distinction drives whether the constraint is tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_destruction_irreversibility, empirical, 'Scope of erasure: practical vs. epistemic destruction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__censorship_mechanism_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art17_cens_theater_t0, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(art17_cens_theater_t3, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 3, 0.49).
narrative_ontology:measurement(art17_cens_theater_t6, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(art17_cens_extract_t0, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(art17_cens_extract_t3, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(art17_cens_extract_t6, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(art17_cens_suppress_t0, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(art17_cens_suppress_t3, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(art17_cens_suppress_t6, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__censorship_mechanism_reading, identity_coordination).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, platform_content_moderation_cost_shifting).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, digital_archive_destruction).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, journalistic_evidentiary_commons).

% DUAL FORMULATION NOTE:
% Article 17 decomposition: The privacy_fundamental_reading and censorship_mechanism_reading describe the same legal text but with different ε values and beneficiary/victim structures. The privacy_fundamental reading (ε ≈ 0.05, Rope) describes the coordination function when erasure is exercised by legitimate data subjects with genuine privacy interests. This reading (censorship_mechanism_reading, ε ≈ 0.58, Tangled Rope) describes the constraint under strategic weaponization and erosion of public interest exceptions. The two readings are structurally distinct because they have different victim sets and different primary functions. When the proportion of bad-faith requests is low, the constraint functions as privacy coordination (Rope). As bad-faith requests rise and exceptions erode, the constraint transitions toward suppression mechanism (Snare). The current measured state reflects the hybrid structure at intermediate bad-faith proportion, yielding tangled_rope. Both readings are live and upstream of the competitive_moat_reading, which treats Article 17 as a regulatory instrument that differentially benefits large platforms over small archives and competitors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article17_erasure_right__censorship_mechanism_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
