% ============================================================================
% CONSTRAINT STORY: sotu_1996_clinton_v_chip_television_requirement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1996_clinton_v_chip_television_requirement, []).

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
 *   constraint_id: sotu_1996_clinton_v_chip_television_requirement
 *   human_readable: V-Chip Mandate: Parental Filtering Technology Requirement (1996 Telecommunications Act)
 *   domain: regulatory/media_governance
 *
 * SUMMARY:
 *   The V-chip mandate, embedded in the 1996 Telecommunications Act and
 *   promoted by President Clinton in his State of the Union address,
 *   represents a deliberate shift in gatekeeping authority from centralized
 *   institutional actors (broadcasters, government) to distributed parental
 *   decision-making. The constraint exhibits characteristics of both Tangled
 *   Rope and Scaffold: genuine coordination function (enabling parental
 *   agency without government censorship) coexists with asymmetric extraction
 *   (manufacturers and broadcasters bear compliance burden). The mechanism is
 *   structurally dependent on a stable, culturally acceptable rating taxonomy
 *   (TV-Y through TV-MA) that categorizes all broadcast content. The theater
 *   ratio is moderate (0.48) because while the rating system involves some
 *   performative compliance, the V-chip technology itself has genuine
 *   functional capacity to block content. The constraint's classification
 *   varies dramatically by perspective: parents see coordination (Rope),
 *   manufacturers see extraction (Snare to Tangled Rope depending on scale),
 *   regulatory agencies see authority delegation (Rope), and pre-existing
 *   self-regulatory bodies see their own degradation (Piton). This is a
 *   diagnostic exemplar of how the same structural innovation can appear as
 *   coordination from beneficiary perspectives and as extraction from victim
 *   perspectives.
 *
 * KEY AGENTS:
 *   - Parents seeking media control (institutional/arbitrage) — primary beneficiaries; gain decentralized gatekeeping authority without centralized censorship
 *   - Television manufacturers large and small (powerful/constrained and powerless/trapped) — bear compliance costs; small manufacturers face higher proportional burden
 *   - Broadcasting networks and content producers (powerful/constrained) — must implement rating systems and labeling; lose monopoly control over content gatekeeping
 *   - Content rating system administrators (institutional/constrained) — bear burden of classifying all broadcast content; gain institutional legitimacy
 *   - Regulatory agencies (FCC, Congress) (institutional/arbitrage) — delegate gatekeeping authority; reduce political heat on censorship questions
 *   - Child advocacy and media reform coalition (organized/mobile) — organized beneficiaries; see V-chip as temporary bridge to improved media literacy norms
 *   - Pre-existing broadcast self-regulatory bodies (institutional/arbitrage) — displaced by V-chip mandate; their prior gatekeeping authority degrades to ceremonial status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1996_clinton_v_chip_television_requirement, 0.38).
domain_priors:suppression_score(sotu_1996_clinton_v_chip_television_requirement, 0.42).
domain_priors:theater_ratio(sotu_1996_clinton_v_chip_television_requirement, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1996_clinton_v_chip_television_requirement, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1996_clinton_v_chip_television_requirement, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sotu_1996_clinton_v_chip_television_requirement, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1996_clinton_v_chip_television_requirement, tangled_rope).
narrative_ontology:human_readable(sotu_1996_clinton_v_chip_television_requirement, "V-Chip Mandate: Parental Filtering Technology Requirement (1996 Telecommunications Act)").
narrative_ontology:topic_domain(sotu_1996_clinton_v_chip_television_requirement, "regulatory/media_governance").

domain_priors:requires_active_enforcement(sotu_1996_clinton_v_chip_television_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1996_clinton_v_chip_television_requirement, parents_media_control_seekers).
narrative_ontology:constraint_beneficiary(sotu_1996_clinton_v_chip_television_requirement, regulatory_agencies_delegating_authority).
narrative_ontology:constraint_victim(sotu_1996_clinton_v_chip_television_requirement, television_manufacturers).
narrative_ontology:constraint_victim(sotu_1996_clinton_v_chip_television_requirement, broadcasters_content_producers).
narrative_ontology:constraint_victim(sotu_1996_clinton_v_chip_television_requirement, content_rating_system_administrators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL TV MANUFACTURERS (SNARE) — Trapped by compliance mandates with no meaningful exit. Must implement V-chip technology or exit the US market entirely. Compliance costs are non-recoverable; no coordination benefit accrues to manufacturers. Extraction runs entirely toward regulatory burden with suppression of alternatives (cannot refuse without losing market access).
constraint_indexing:constraint_classification(sotu_1996_clinton_v_chip_television_requirement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MAJOR TV MANUFACTURERS (TANGLED ROPE) — Constrained but organized; can absorb compliance costs and negotiate implementation details. Genuine coordination function exists: the V-chip enables a market niche (parent-controlled TVs) that manufactures can differentiate on. Mixed extraction and coordination — they bear regulatory burden but gain market segmentation advantage.
constraint_indexing:constraint_classification(sotu_1996_clinton_v_chip_television_requirement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR BROADCASTING NETWORKS (SNARE) — Must rate all content; labeling requirements increase production friction. But networks have exit option (cable, syndication, consolidation) and market power. Extraction is asymmetric — regulatory requirements without proportional coordination benefit. The V-chip redistributes control away from network gatekeeping toward parental gatekeeping, reducing network monopoly power on content decisions.
constraint_indexing:constraint_classification(sotu_1996_clinton_v_chip_television_requirement, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PARENTS SEEKING MEDIA CONTROL (ROPE) — Primary beneficiaries. The V-chip provides genuine coordination function: enables decentralized parental decision-making without centralized government censorship. Parents gain agency and tools; no enforcement burden falls on them. Extraction flows toward this agent (regulatory burden on manufacturers is a subsidy to parental control). Net beneficiary position with significant arbitrage options (use V-chip, do not use, use alternative monitoring methods).
constraint_indexing:constraint_classification(sotu_1996_clinton_v_chip_television_requirement, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CHILD ADVOCACY COALITION (SCAFFOLD) — Organized agents see V-chip as a temporary bridge toward media literacy norms that will eventually make device-level filtering unnecessary. The mechanism is scaffolding because it assumes eventual behavioral change (parents becoming more conscious media consumers, industry self-regulation improving) that would make mandatory V-chip redundant. Sunset is implicit: as industry rating systems mature and media literacy improves, device-level mandates become unnecessary. Has exit options through advocacy for alternative mechanisms.
constraint_indexing:constraint_classification(sotu_1996_clinton_v_chip_television_requirement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: CONTENT RATING SYSTEM ADMINISTRATORS (TANGLED ROPE) — Bear significant burden of implementing, maintaining, and defending rating taxonomy. Must classify all broadcast content; face criticism from all sides (too strict, too lenient, culturally biased). But also gain institutional legitimacy and influence over what content signals are meaningful. Mixed coordination (enabling parental choice) and extraction (burden of rating all content, liability for rating accuracy). Constrained because they cannot exit the system once they enter — institutional credibility depends on continuity.
constraint_indexing:constraint_classification(sotu_1996_clinton_v_chip_television_requirement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: REGULATORY AGENCIES (ROPE) — Achieve coordination objective (enabling parental control without government censorship) by delegating gatekeeping to distributed agents (individual parents). Benefits from reduced political heat on censorship questions; implementation burden falls on manufacturers/broadcasters. Net beneficiary of regulatory authority delegation with arbitrage options (could regulate content directly, enforce rating compliance, or step back entirely).
constraint_indexing:constraint_classification(sotu_1996_clinton_v_chip_television_requirement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: PRE-EXISTING INDUSTRY SELF-REGULATION (PITON) — The V-chip mandate effectively displaces earlier self-regulatory mechanisms (NAB code, broadcast standards practices). These mechanisms persist in degraded form as theatrical compliance — they are maintained for legitimacy but real gatekeeping power has shifted to technological devices and parental controls. Theater ratio reflects that pre-mandate industry self-regulation was already primarily performative; the V-chip makes this explicit by transferring actual power elsewhere.
constraint_indexing:constraint_classification(sotu_1996_clinton_v_chip_television_requirement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, gatekeeping in broadcast media may appear as an immutable constraint: someone must decide what content reaches mass audience; that someone is either government, industry, or distributed parents. The shift from centralized to distributed gatekeeping is a structural invariant, not a contingent choice. This perspective risks naturalizing what is actually a political decision (delegating authority to parents rather than centralizing it) as an inevitable feature of media governance.
constraint_indexing:constraint_classification(sotu_1996_clinton_v_chip_television_requirement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1996_clinton_v_chip_television_requirement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1996_clinton_v_chip_television_requirement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1996_clinton_v_chip_television_requirement, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1996_clinton_v_chip_television_requirement, TR),
    TR >= 0.70.

:- end_tests(sotu_1996_clinton_v_chip_television_requirement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The mandate imposes genuine compliance costs on manufacturers and broadcasters (design, implementation, testing, labeling), but also creates coordination benefits (parents gain agency, clear rating taxonomy enables market segmentation). The extraction is asymmetric but not total — large manufacturers can absorb costs and gain market differentiation; small manufacturers face disproportionate burden. Suppression (0.42): Moderate. Broadcasters cannot easily exit (massive installed base), and manufacturers cannot refuse compliance without losing US market access. But suppression is not total — alternative gatekeeping methods exist (parental monitoring, subscription services with internal controls, public broadcasting with different rating standards). Manufacturers can lobby for implementation flexibility; broadcasters can adjust content strategy. Theater ratio (0.48): Moderate. The V-rating taxonomy involves some performative classification (debates over what constitutes TV-PG vs TV-14 content), but the technology itself has genuine blocking capacity. The theater is lower than in pure self-regulatory systems because technological enforcement is involved. The ratio increases slightly over the measurement interval (0.32 to 0.48) as rating disputes accumulate and implementation friction increases, but does not approach Piton territory (>0.70) because the device remains functionally meaningful.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Parents perceive Rope (pure coordination enabling their agency without government censorship). Large manufacturers perceive Tangled Rope (genuine market differentiation opportunity offset by compliance burden). Small manufacturers perceive Snare (regulatory extraction with no offsetting benefit). Broadcasters perceive Snare to Tangled Rope (loss of gatekeeping authority with labeling burden, but alternative content strategies available). Rating system administrators perceive Tangled Rope (institutional legitimacy gained through responsibility borne). Regulatory agencies perceive Rope (successful authority delegation achieving policy objectives). Child advocacy sees Scaffold (temporary mechanism enabling transition to better norms). Pre-existing self-regulatory bodies see Piton (ceremonial displacement). The analytical observer risks Mountain (naturalizing distributed gatekeeping as inevitable) but structural analysis reveals this as false summit — the shift from centralized to distributed authority is a political choice, not a law of nature. The perspectival gap reveals that 'parental control via technology' is simultaneously coordination from parents' perspective and extraction from small manufacturers' perspective, resolved by noting that these are genuinely different structural positions with different exit options and power levels.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to the extraction flow. Parents are beneficiaries with high exit options (arbitrage) — they can use V-chip, ignore it, or use alternatives — producing low d (full beneficiary). Large manufacturers are constrained beneficiaries (can absorb costs, gain market advantage) — producing moderate d. Small manufacturers are victims with trapped options (must comply or exit market) — producing high d (full target). Broadcasters are victims with constrained options (must rate and label; can lobby and adjust content) — producing moderate-high d. Rating administrators are mixed (gain legitimacy, bear burden) — producing mid-range d. Regulatory agencies are net beneficiaries (achieve delegation objective) — producing low d. The canonical fallback values are overridden by explicit structural declarations: manufacturers are declared as victims (bearing compliance burden) despite their powerful nominal status, because their exit options are severely constrained by market structure (must serve US market or lose scale economies). This differentiation between global power and constraint-specific power is essential for accurate perspectival classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that the classification depends on structural position. This is NOT mislabeling extraction as coordination — each type is legitimate from its perspective. The mandate genuinely creates coordination (parents gain agency; regulatory objectives achieved), AND it genuinely extracts (manufacturers bear costs; broadcasters lose gatekeeping). The resolution is: the constraint is simultaneously Rope from parents' perspective (coordinating parental agency) and Tangled Rope/Snare from manufacturers' perspective (enforcing compliance costs). The mandatrophy is resolved by accepting that different observers legitimately classify the same constraint differently because they occupy different structural positions. The false summit risk is in the analytical perspective — viewing V-chip as an immutable natural feature of media governance (gatekeeping must exist somewhere; the V-chip is where it naturally resides) rather than as a contingent political choice (authority was deliberately shifted from broadcasters to parents through regulatory mandate). The analytical observer's Mountain classification is a false summit that naturalizes what should be recognized as a structural innovation chosen for specific policy purposes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rating_taxonomy_stability,
    'Is the V-rating taxonomy (TV-Y, TV-Y7, TV-G, TV-PG, TV-14, TV-MA) sufficiently stable and culturally invariant to serve as a coordination mechanism, or does it embed contested value judgments about appropriate content?',
    'Longitudinal analysis of rating disputes, appeals, and changes; cross-cultural comparison of how same content is rated in different regions; correlation between parent expectations and actual rating assignments',
    'If stable and culturally neutral: V-chip is pure coordination (Rope from all perspectives). If contested and value-laden: V-chip embeds regulatory authority in rating decisions (Tangled Rope confirmed; underlying constraint is ''who defines appropriate content'').',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rating_taxonomy_stability, empirical, 'Stability and cultural invariance of V-rating taxonomy').

omega_variable(
    parental_agency_actualization,
    'Do parents actually use V-chip technology to exercise gatekeeping, or does the device remain unutilized, functioning primarily as regulatory theater?',
    'Household survey data on V-chip activation rates; comparison of demographic groups that use V-chip versus those that don''t; correlation between V-chip adoption and actual content consumption patterns',
    'If used actively: V-chip represents genuine distributed gatekeeping (Rope from parent perspective confirmed). If unused: V-chip is regulatory theater with minimal functional impact, reducing to Piton classification across most perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parental_agency_actualization, empirical, 'Whether V-chip technology is actually used by parents').

omega_variable(
    manufacturer_cost_pass_through,
    'Do TV manufacturers absorb V-chip compliance costs or pass them through to consumers? Does this affect market segmentation?',
    'Price comparison analysis (V-chip equipped vs non-equipped models, controlling for other features); market share analysis before/after mandate; manufacturer profit margin studies during implementation period',
    'If absorbed: extraction runs entirely toward manufacturers (Snare confirmed for small manufacturers; Tangled Rope for large). If passed through: extraction partially shifted to consumers, changing victim classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manufacturer_cost_pass_through, empirical, 'Whether V-chip costs are absorbed by manufacturers or passed to consumers').

omega_variable(
    gatekeeping_shift_completeness,
    'Does the V-chip represent a genuine shift in gatekeeping authority from industry to parents, or do broadcasters retain effective control through rating decisions and content production?',
    'Analysis of broadcaster response to V-chip mandate: do programming decisions change? Do broadcasters deliberately target unrated or lightly-rated content? Does rating creep occur (inflation of ratings to avoid blocking)?',
    'If genuine shift: regulatory delegation to parents is structural and real (Rope from parental perspective confirmed). If retention of broadcaster control: V-chip is a distributed enforcement mechanism for industry preferences (Snare from broadcaster perspective with modified parameters).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_shift_completeness, empirical, 'Whether gatekeeping authority truly shifts to parents or remains with broadcasters').

omega_variable(
    technological_lock_in_duration,
    'Is the V-chip mandate structurally temporary (a bridge to better solutions) or does it create technological lock-in that persists beyond its functional utility?',
    'Monitoring of alternative parental control mechanisms (internet-based, subscription service controls, AI content filtering); analysis of whether V-chip becomes obsolete before mandate expires or extends indefinitely',
    'If temporary bridge: Scaffold perspective is accurate; exit path is real. If locked-in: Scaffold reclassifies as Tangled Rope or Snare; the sunset clause becomes theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_lock_in_duration, conceptual, 'Whether V-chip mandate is structurally temporary or technologically locked in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1996_clinton_v_chip_television_requirement, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vchip_tr_t0, sotu_1996_clinton_v_chip_television_requirement, theater_ratio, 0, 0.32).
narrative_ontology:measurement(vchip_tr_t3, sotu_1996_clinton_v_chip_television_requirement, theater_ratio, 3, 0.42).
narrative_ontology:measurement(vchip_tr_t6, sotu_1996_clinton_v_chip_television_requirement, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(vchip_be_t0, sotu_1996_clinton_v_chip_television_requirement, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(vchip_be_t3, sotu_1996_clinton_v_chip_television_requirement, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(vchip_be_t6, sotu_1996_clinton_v_chip_television_requirement, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1996_clinton_v_chip_television_requirement, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1996_clinton_v_chip_television_requirement, broadcast_rating_system_implementation).
narrative_ontology:affects_constraint(sotu_1996_clinton_v_chip_television_requirement, parental_authority_over_media_consumption).
narrative_ontology:affects_constraint(sotu_1996_clinton_v_chip_television_requirement, technology_as_regulatory_enforcement).

% DUAL FORMULATION NOTE:
% The V-chip mandate can be decomposed into structurally distinct constraints: (1) The rating taxonomy constraint (classifying content; ε ≈ 0.22, mountain from analytical perspective because rating categories appear natural), (2) The device mandate constraint (requiring V-chip hardware; ε ≈ 0.38, tangled rope because coordination and extraction are genuinely mixed), and (3) The authority delegation constraint (shifting gatekeeping from broadcasters to parents; ε ≈ 0.35, rope/tangled rope depending on perspective). This single story models the device mandate as the primary constraint; rating taxonomy and authority delegation are analyzed as sub-mechanisms. Complete decomposition would create three separate stories with network links.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1996_clinton_v_chip_television_requirement, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
