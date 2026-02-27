% ============================================================================
% CONSTRAINT STORY: wikipedia_notability_requirement_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wikipedia_notability_requirement_2026, []).

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
 *   constraint_id: wikipedia_notability_requirement_2026
 *   human_readable: Wikipedia Notability Requirement (2026)
 *   domain: social/technological
 *
 * SUMMARY:
 *   Wikipedia's notability requirement functions as a multi-layered
 *   constraint that simultaneously coordinates knowledge curation (preventing
 *   spam and low-quality articles) and extracts authority from emerging
 *   subjects, marginalized communities, and non-institutional knowledge
 *   producers. The constraint operates by requiring pre-existing media
 *   coverage in 'reliable sources' before a Wikipedia article can be created.
 *   This appears neutral on its surface but systematically privileges
 *   established institutions with existing media presence while excluding
 *   knowledge from non-English-speaking communities, emerging fields without
 *   mainstream media attention, and subjects important to marginalized groups
 *   but not to mass media audiences. The theater ratio (0.64) reflects that
 *   notability enforcement has become increasingly performative: Wikipedia
 *   editors spend significant effort in deletion debates that focus on
 *   citation counting rather than actual knowledge value, while the genuine
 *   curation function (article quality) is maintained through separate
 *   mechanisms (editorial review, vandalism control). The constraint exhibits
 *   all six DR types from different perspectives, making it a diagnostic
 *   exemplar of how institutional gatekeeping combines coordination and
 *   extraction.
 *
 * KEY AGENTS:
 *   - Established Institution (institutional/arbitrage): Primary beneficiary — corporations, governments, universities already have media coverage and experience notability requirement as straightforward coordination
 *   - Emerging Subject (powerless/trapped): Primary victim — new persons, events, concepts lack media presence and are locked out of Wikipedia despite potential importance
 *   - Marginalized Community (powerless/trapped): Secondary victim — non-English-speaking communities, Global South knowledge, indigenous knowledge systems systematically excluded by media-coverage metric
 *   - Wikipedia Curation Authority (institutional/arbitrage): Co-beneficiary — notability requirement concentrates editorial power and gatekeeping authority within community structures
 *   - Content Community Editors (moderate/constrained): Mixed — benefit from coordination function (preventing spam) but constrained by enforcement mechanism and gatekeeping authority concentration
 *   - Deletion Review Community (organized/constrained): Organized advocates for policy change; constrained by inability to override enforcement mechanism
 *   - Distributed Knowledge Platform Movement (organized/mobile): Building alternative systems with different curation metrics; represents structural exit path for sunset logic
 *   - Analytical Observer (analytical/analytical): Risk of naturalizing contingent institutional choice as inherent limitation on knowledge systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wikipedia_notability_requirement_2026, 0.52).
domain_priors:suppression_score(wikipedia_notability_requirement_2026, 0.68).
domain_priors:theater_ratio(wikipedia_notability_requirement_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wikipedia_notability_requirement_2026, tangled_rope).
narrative_ontology:human_readable(wikipedia_notability_requirement_2026, "Wikipedia Notability Requirement (2026)").
narrative_ontology:topic_domain(wikipedia_notability_requirement_2026, "social/technological").

domain_priors:requires_active_enforcement(wikipedia_notability_requirement_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wikipedia_notability_requirement_2026, established_institutions).
narrative_ontology:constraint_beneficiary(wikipedia_notability_requirement_2026, wikipedia_curation_authority).
narrative_ontology:constraint_victim(wikipedia_notability_requirement_2026, emerging_subjects).
narrative_ontology:constraint_victim(wikipedia_notability_requirement_2026, marginalized_communities).
narrative_ontology:constraint_victim(wikipedia_notability_requirement_2026, non_anglophone_knowledge).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING SUBJECT (SNARE) — A newly significant person, event, or concept with real-world importance cannot establish notability on Wikipedia without pre-existing media coverage. The subject is trapped: cannot use Wikipedia to build legitimacy, cannot exit the requirement without external institutional validation first. The notability gate operates as pure extraction — the subject must demonstrate importance elsewhere before Wikipedia will acknowledge it exists.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITY (SNARE) — Communities with limited access to major media outlets or publishing infrastructure cannot satisfy notability criteria even when they have significant cultural or historical importance. Notability requirement systematically excludes non-English-language sources and non-Western media institutions as 'reliable sources.' Trapped with no exit option — the constraint enforces cultural hierarchy through what appears to be neutral editorial policy.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: ESTABLISHED INSTITUTION (ROPE) — Major corporations, government bodies, universities, and established media outlets benefit from notability requirement as coordination mechanism. They can arbitrage between Wikipedia presence and external validation: their importance is already established in secondary sources, so notability requirement is a straightforward coordination problem (documenting what is already known) rather than extraction. Institutional actor with arbitrage exit sees this as pure coordination.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONTENT COMMUNITY (TANGLED ROPE) — Wikipedia editors and subject-matter experts face both coordination function (preventing spam and low-quality articles) and extraction (the requirement concentrates editorial power and creates gatekeeping authority). Experts are constrained by the notability enforcement mechanism but also benefit from the coordination function that maintains article quality. Significant active enforcement required to maintain the standard against both inclusion pressure and systemic bias.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL KNOWLEDGE ARCHIVE (PITON) — Wikipedia frames itself as a repository of established knowledge, not a primary source or discovery mechanism. The notability requirement is performative maintenance of this institutional role: it creates the appearance of rigorous curation while the actual function (knowledge preservation) has atrophied as Wikipedia becomes the primary knowledge interface for billions of users. Theater ratio reflects the gap between stated purpose (encyclopedia of established knowledge) and actual function (first-contact knowledge interface). Institutional inertia maintains the requirement despite its functional degradation.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DELETION REVIEW COMMUNITY (TANGLED ROPE) — Organized groups advocating for expanded notability (Wikidata, Wikimedia Foundation, subject-specific projects) see both coordination and extraction. The notability requirement provides coordination benefit (maintaining quality standards) but also enforces asymmetric authority: review standards are unevenly applied across subjects, creating extractive gatekeeping. Organized agents have constrained exit options — they can propose guideline changes but cannot override the enforcement mechanism directly.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: DISTRIBUTED KNOWLEDGE PLATFORM MOVEMENT (SCAFFOLD) — Alternative platforms (Wikidata structured data, subject wikis, semantic web projects) are building parallel knowledge systems that bypass notability gatekeeping. Organized actors see the notability requirement as a temporary coordination problem with a structured exit: as distributed platforms mature, Wikipedia's exclusive authority over what 'counts' as notable knowledge declines. Sunset logic applies — the constraint's extraction mechanism depends on Wikipedia's monopoly status, which is eroding.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some notability threshold is inherent to any knowledge system: finite editorial capacity means some prioritization rule is necessary. The notability requirement appears as an immutable constraint on knowledge organization itself. However, the structural data contradicts the mountain classification — this naturalizes what is actually a contingent institutional choice (using pre-existing media coverage as the metric) rather than a law of information systems. The engine's false summit detector will identify this as naturalization.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wikipedia_notability_requirement_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wikipedia_notability_requirement_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(wikipedia_notability_requirement_2026, TR),
    TR >= 0.70.

:- end_tests(wikipedia_notability_requirement_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The notability requirement does enforce asymmetric authority — Wikipedia editors and established institutions benefit from control over what counts as notable, while emerging subjects and marginalized communities bear the cost of exclusion. However, extractiveness is not maximal because some coordination benefit is genuine: preventing spam and extremely low-quality articles is a real function. The 0.52 value reflects the mixed nature. Suppression (0.68): High. Significant structural barriers prevent escape: emerging subjects cannot bypass the requirement by demonstrating importance on Wikipedia itself; they must establish importance in external media systems first. Non-English-language sources are systematically devalued. Career and funding incentives in academia and journalism concentrate media coverage toward establishment subjects. But suppression is not absolute — some subjects can accumulate media coverage and gain inclusion. Theater ratio (0.64): Moderate-high. Notability enforcement has become increasingly performative over the interval. Editors spend substantial time in deletion debates focused on citation counting (theater) while the actual curation function (quality control) occurs through separate mechanisms. The performative content has increased as Wikipedia's role shifted from encyclopedia of established knowledge to first-contact knowledge interface — the stated purpose (documenting already-notable subjects) diverges from actual function (serving as primary knowledge source).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates multiple coexisting classifications from identical base properties. Established institutions see pure coordination (Rope) — notability requirement aligns with their interests and solves a real gatekeeping problem. Emerging subjects see pure extraction (Snare) — they are systematically excluded with no exit option. Marginalized communities see snare with cultural/linguistic hierarchy (Snare) — the media-coverage metric enforces Western institutional dominance. Content editors see mixed coordination and extraction (Tangled Rope) — they benefit from quality maintenance but face extractive gatekeeping authority. The deletion review community sees the constraint as both coordination and enforcement asymmetry (Tangled Rope) — advocates for change lack direct authority. The distributed platform movement sees a temporary coordination problem with a structured exit (Scaffold) — alternative systems are building parallel knowledge authorities. The institutional knowledge archive sees its own degraded ritual (Piton) — the stated purpose (encyclopedia of established knowledge) has atrophied while the enforcement mechanism persists. The analytical observer risks naturalizing contingency as law (Mountain) — the false summit detector will identify this as misclassification. The perspectival gap is large because the constraint's extraction depends on Wikipedia's monopoly authority, which different agents experience with different exit velocities.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by structural position relative to the notability enforcement mechanism. Established institutions are beneficiaries with arbitrage options — they can satisfy notability criteria through existing media presence, deriving d ≈ 0.10 (low extraction experienced). Emerging subjects are victims with no exit options — they cannot satisfy the requirement without external validation, deriving d ≈ 0.90 (maximal extraction experienced). Marginalized communities are victims trapped by media-source hierarchy — they experience the constraint as cultural gatekeeping, deriving d ≈ 0.85. Content editors are constrained beneficiaries — they benefit from coordination function but face authority concentration, deriving d ≈ 0.55. Deletion review organizers face constrained options but have some policy influence, deriving d ≈ 0.60. Distributed platform advocates have mobile exit options through alternative systems, deriving d ≈ 0.40. The scaffold classification emerges from the organized actors with mobile exit and generational timeframe — they see structured alternatives appearing. The piton classification emerges from institutional perspective viewing the enforcement mechanism as degraded performance rather than genuine function. The mountain classification at the analytical/civilizational level would require the notability requirement to be inherent to knowledge curation itself — the structural data contradicts this by showing the specific metric (media coverage) is contingent and alternative systems are using different metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by showing that the notability requirement is genuinely tangled — it provides both coordination (preventing spam and low-quality inclusion) and extraction (concentrating authority and systematically excluding non-institutional knowledge). The snare perspective (emerging subject/trapped) is the most vulnerable agent class, but that does not mean the constraint is universally a snare. The rope perspective (established institution/arbitrage) is the primary beneficiary, but the constraint is not pure coordination — asymmetric extraction is built in. The power-law structure across perspectives reveals the extraction mechanism: the constraint's coordination benefit flows to agents who already have institutional authority, while extraction flows from agents without pre-existing media presence. This is not disguised coordination (which would appear as rope from most perspectives) nor is it pure extraction (which would show snare or snare-like from nearly all perspectives). The tangled_rope classification with organized enforcement reflects the actual structure: a coordination mechanism (quality gate) layered with systematic extraction (authority concentration and cultural hierarchy). The scaffold perspective (distributed platforms with sunset logic) and piton perspective (performative enforcement) further resolve the classification by showing that the constraint's function has partially degraded (piton) while its extraction mechanism persists, and structural alternatives are emerging that will eventually reduce its enforcement pressure (scaffold).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notability_metric_contingency,
    'Is the specific notability metric (media coverage in reliable sources) inherent to knowledge curation, or a contingent institutional choice?',
    'Comparative analysis of alternative knowledge systems (academic databases, Wikidata, subject wikis, distributed platforms) and their curation metrics. Analysis of historical Wikipedia guideline evolution.',
    'If contingent: notability requirement is tangled_rope/snare from all perspectives. If inherent: mountain classification is justified. Current evidence strongly suggests contingency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(notability_metric_contingency, conceptual, 'Whether notability metric is contingent or inherent to knowledge curation').

omega_variable(
    systemic_bias_quantification,
    'Does the notability requirement systematically exclude categories of knowledge (non-English sources, marginalized communities, emerging fields) at rates that exceed random variation?',
    'Statistical analysis of deletion rates by subject category, source language, institutional affiliation, and geographic origin. Longitudinal tracking of inclusion patterns for comparable subjects.',
    'If systematic exclusion confirmed: snare classification is dominant. If exclusion is random/proportional: tangled_rope classification better captures mixed coordination and extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_bias_quantification, empirical, 'Whether notability requirement systematically excludes categories of knowledge').

omega_variable(
    distributed_platform_maturity,
    'Will alternative knowledge platforms (Wikidata, subject wikis, semantic web projects) achieve sufficient maturity and adoption to reduce Wikipedia notability enforcement pressure within 10-20 years?',
    'Tracking adoption metrics, coverage depth, and citation rates of alternative platforms. Analysis of Wikipedia traffic share trends and emergence of dominant competing knowledge interfaces.',
    'If maturity confirmed: scaffold perspective is structural (real sunset). If alternative platforms remain niche: scaffold is aspirational, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_platform_maturity, empirical, 'Whether distributed platforms will mature enough to reduce Wikipedia''s notability enforcement').

omega_variable(
    editorial_capacity_constraint,
    'What is the actual limiting factor on Wikipedia''s editorial capacity — resource scarcity or organizational structure? Could the constraint be relaxed through better allocation rather than fundamental capacity limits?',
    'Analysis of Wikipedia''s actual editorial resource allocation, volunteer engagement patterns, and comparison with other large collaborative knowledge systems. Simulation of relaxed notability criteria with adequate automation and structural support.',
    'If structural/allocational: the constraint is extractive gatekeeping. If genuine resource scarcity: the constraint is legitimate coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(editorial_capacity_constraint, empirical, 'Whether notability limits are driven by resource scarcity or organizational structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wikipedia_notability_requirement_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wiki_notab_tr_t0, wikipedia_notability_requirement_2026, theater_ratio, 0, 0.48).
narrative_ontology:measurement(wiki_notab_tr_t5, wikipedia_notability_requirement_2026, theater_ratio, 5, 0.56).
narrative_ontology:measurement(wiki_notab_tr_t10, wikipedia_notability_requirement_2026, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(wiki_notab_be_t0, wikipedia_notability_requirement_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(wiki_notab_be_t5, wikipedia_notability_requirement_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(wiki_notab_be_t10, wikipedia_notability_requirement_2026, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wikipedia_notability_requirement_2026, information_standard).
narrative_ontology:affects_constraint(wikipedia_notability_requirement_2026, wikipedia_editing_authority).
narrative_ontology:affects_constraint(wikipedia_notability_requirement_2026, global_south_knowledge_access).
narrative_ontology:affects_constraint(wikipedia_notability_requirement_2026, emerging_field_epistemic_recognition).

% DUAL FORMULATION NOTE:
% The notability requirement decomposes into at least two structurally distinct constraints: (1) quality maintenance (coordination problem — preventing spam and extremely low-quality inclusion) and (2) authority concentration (extraction mechanism — controlling what counts as legitimate knowledge). The base story treats the unified constraint. Downstream stories would decompose the coordination and extraction components separately, showing how the mixed constraint produces different classification outcomes from different perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wikipedia_notability_requirement_2026, organized, 0.6).
constraint_indexing:directionality_override(wikipedia_notability_requirement_2026, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
