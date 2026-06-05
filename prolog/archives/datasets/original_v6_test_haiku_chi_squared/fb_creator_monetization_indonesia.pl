% ============================================================================
% CONSTRAINT STORY: fb_creator_monetization_indonesia
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fb_creator_monetization_indonesia, []).

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
 *   constraint_id: fb_creator_monetization_indonesia
 *   human_readable: Facebook/Meta's Performance Bonus Monetization Program for Indonesian Creators
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Meta's Performance Bonus program in Indonesia represents a structural
 *   extraction mechanism embedded in the platform economy's dependency
 *   dynamics. Launched as a monetization pathway for Indonesian content
 *   creators, the program offers direct financial rewards (typically
 *   $100-500/month for high-engagement creators) tied to algorithmic
 *   performance metrics. For creators in a nation where median household
 *   income is approximately $200-250 per month, the bonus program creates
 *   powerful economic dependency. The constraint exhibits classic snare
 *   characteristics: creators face genuine earning limitations (traditional
 *   employment in Indonesia offers limited income growth; alternative income
 *   streams are scarce), high barriers to exit (audience migration costs,
 *   network effects lock-in, lack of alternative platforms at comparable
 *   scale), and suppression through opaque algorithmic criteria and
 *   unilateral policy changes. Simultaneously, the program coordinates
 *   creator behavior and platform engagement — but this coordination function
 *   is secondary to extraction. Meta benefits through sustained engagement,
 *   algorithm-optimized content production, audience data collection, and
 *   advertiser value concentration. The theater ratio (0.68) reflects the
 *   performative framing of 'performance bonus' as meritocratic reward when
 *   allocation is substantially driven by algorithmic opacity and Meta's
 *   unilateral design choices.
 *
 * KEY AGENTS:
 *   - Indonesian Content Creators (Monetized): Primary victims (powerless/trapped) — 10-50k enrolled creators generating content, bearing labor costs, dependent on algorithm-driven income
 *   - Meta Corporation: Primary beneficiary (institutional/arbitrage) — captures engagement metrics, audience data, advertising value, shareholder returns; can modify or terminate program unilaterally
 *   - Indonesian Content Ecosystem: Secondary victim (moderate/constrained) — non-enrolled creators, media organizations, alternative platforms disadvantaged by bonus program's preferential algorithm treatment
 *   - Creator Unions and Advocacy Groups: Organized but degraded (organized/constrained) — formal representation exists (Serikat Kreator Indonesia, Dewan Konten Indonesia) but lacks leverage; alternative platforms exist but cannot match Meta's reach
 *   - Indonesian Regulatory Authority (Kominfo/OJK): Potential intervener (institutional/constrained) — possesses regulatory mandate but faces capacity and political constraints; Digital Platform regulation still in draft stage
 *   - Analytical Observer: Systemic view (analytical/analytical) — sees platform dependency as structural vulnerability; risks naturalizing extraction as inevitable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fb_creator_monetization_indonesia, 0.58).
domain_priors:suppression_score(fb_creator_monetization_indonesia, 0.72).
domain_priors:theater_ratio(fb_creator_monetization_indonesia, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fb_creator_monetization_indonesia, extractiveness, 0.58).
narrative_ontology:constraint_metric(fb_creator_monetization_indonesia, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fb_creator_monetization_indonesia, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fb_creator_monetization_indonesia, snare).
narrative_ontology:human_readable(fb_creator_monetization_indonesia, "Facebook/Meta's Performance Bonus Monetization Program for Indonesian Creators").
narrative_ontology:topic_domain(fb_creator_monetization_indonesia, "technological/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fb_creator_monetization_indonesia, meta_shareholder_value).
narrative_ontology:constraint_beneficiary(fb_creator_monetization_indonesia, platform_engagement_metrics).
narrative_ontology:constraint_victim(fb_creator_monetization_indonesia, indonesian_content_creators).
narrative_ontology:constraint_victim(fb_creator_monetization_indonesia, creator_time_autonomy).
narrative_ontology:constraint_victim(fb_creator_monetization_indonesia, content_ecosystem_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDONESIAN CREATOR (SNARE) — Structurally trapped. Program offers monetization pathway unavailable through traditional employment or other platforms in Indonesia. Once enrolled, algorithm-driven bonus eligibility creates dependency: high suppression (0.72) from lack of alternative income sources, opaque eligibility criteria, and unilateral algorithm control. Creator bears cost of content production labor while Meta extracts engagement data and audience lock-in. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81. No meaningful exit; creator cannot replicate audience on alternative platforms without massive audience rebuilding.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONTENT ECOSYSTEM PLAYERS (TANGLED ROPE) — Other creators, advertisers, media organizations face constrained exit. The program coordinates engagement standards and creator monetization infrastructure (coordination function) while simultaneously extracting value through preferential algorithm treatment of bonus-enrolled creators vs non-enrolled competitors. Requires active enforcement: Meta's algorithm actively deprioritizes non-bonus content. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.46. Mixed experience: benefits from platform infrastructure but disadvantaged by extraction mechanism favoring bonused creators.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: META CORPORATION (ROPE) — Experiences program as pure coordination mechanism: allocates creator incentives, coordinates engagement behavior, standardizes content production norms. High exit options (arbitrage) — Meta can modify, suspend, or terminate program at any time; can redirect creators to other monetization models. Benefits accrue immediately through engagement data, audience stickiness, and shareholder value. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary; effective extraction is negative (subsidy to Meta's coordination objectives).
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (SNARE) — From civilizational/global scale, the program represents a structural extraction mechanism embedded in the platform economy's dependency dynamics. Indonesian creators face genuine earning constraints (median household income ~$200/month; bonus payments can reach $100-500/month for high-engagement creators). This creates structural vulnerability: creators optimize for algorithmic engagement rather than content quality or audience autonomy. The 'performance bonus' framing naturalizes extraction as meritocratic reward. d≈0.82, f(d)≈1.25, σ=1.2 → χ≈0.73. High effective extraction due to scope amplification (global platform, standardized mechanism) and analytical perspective's ability to see systemic lock-in.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: CREATOR UNIONS / ALTERNATIVE PLATFORMS (PITON) — Formal creator advocacy and alternative platforms (TikTok, YouTube, BeReal) exist but are degraded: they cannot replicate Meta's audience reach in Indonesia (Facebook+Instagram monthly active users: 139 million in population of 270 million; TikTok second at 59 million). Theater ratio (0.68) reflects performative alternative: unions and competing platforms stage resistance while lock-in persists. The existence of alternatives is partly theatrical — switching costs for audiences remain prohibitive. d≈0.60, f(d)≈0.75, σ=0.9 → χ≈0.41. Organized agents see the constraint as inertial — maintained by network effects, not innovation.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: POWERLESS / NATURAL LAW VIEW (false summit candidate) — From the perspective of low-income creators globally, platform dependency might appear as immutable economic law: 'This is how digital labor markets work.' The framing naturalizes extraction as inevitable. However, structural data contradicts mountain classification: ε=0.58 (not ≤0.25), suppression=0.72 (not ≤0.05), theater=0.68 (indicates performance), accessibility_collapse not declared. This is a FALSE SUMMIT — the 'inevitability' is contingent on Meta's design choices (algorithm opacity, bonus criteria volatility, geographic pricing discrimination). Natural law framing obscures the extractive institutional design.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fb_creator_monetization_indonesia_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fb_creator_monetization_indonesia, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fb_creator_monetization_indonesia, TR),
    TR >= 0.70.

:- end_tests(fb_creator_monetization_indonesia_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Base extraction reflects several structural factors: (1) Creators bear production labor costs while Meta captures engagement data and audience monetization rights. (2) The 'performance bonus' framing naturalizes extraction as meritocratic reward despite algorithmic opacity. (3) Payment amounts ($100-500/month) are substantial in Indonesian context but remain below creator earning potential if they retained audience ownership. The 0.58 value represents extraction higher than pure coordination (Rope) but lower than predatory lending or coercive labor (where ε approaches 0.75+). Suppression (0.72): High. Four suppression mechanisms operate: (a) Lack of alternative income sources — Indonesian labor market constraints and limited freelance/remote income streams. (b) Algorithmic opacity — bonus eligibility criteria are not fully transparent; creators cannot predict earnings reliably. (c) Network effects lock-in — audience migration to alternative platforms requires abandoning existing followers. (d) Unilateral policy control — Meta can change bonus criteria, payment rates, or eligibility without creator consent or advance notice. Theater ratio (0.68): Moderate-high. The 'performance bonus' framing is substantially performative: it presents algorithmic extraction as meritocratic reward, obscures platform dependency, and masks Meta's data extraction and audience control. However, theater is not maximal (not ≥0.85 as in pure piton) because real monetary transfers occur — the performance is not purely symbolic. Theater has increased over the measurement interval as algorithmic criteria have become more opaque and bonus volatility has increased.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a dramatic perspectival gap between creators and Meta. Indonesian creators experiencing the program as a Snare (trapped, high extraction) see an oppressive dependency mechanism — the program appears extractive precisely because their exit options are severely limited by income needs and audience lock-in. Meta perceives the program as Rope (coordination mechanism) — from Meta's institutional perspective, the program solves a coordination problem (how to incentivize high-engagement content while monetizing creator audiences), and Meta experiences the constraint as a net benefit (engagement, data, advertiser value). The creator ecosystem players occupy a middle position (Tangled Rope): they benefit from the platform infrastructure and engagement standards but are disadvantaged by preferential algorithm treatment of bonus-enrolled creators. The piton perspective (degraded alternatives) reflects the theater content: creator unions and competing platforms exist but cannot genuinely compete. The analytical observer's perspective risks naturalizing the extraction as inevitable economic law ('Platform dependency is how digital labor markets work') — but the structural data reveals this as a false summit. The constraint is contingent on Meta's design choices, not an immutable law of economics.
 *
 * DIRECTIONALITY LOGIC:
 *   Indonesian Creators: Victims + trapped → d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81. Very high extraction from creators' perspective. Creators cannot exit without significant income loss; they are structurally trapped by income dependency and network effects. Meta receives maximum extractive power because creators have minimal bargaining position. Meta Corporation: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Negative effective extraction (net benefit to Meta). Meta can modify or terminate the program at any time; experiences program as pure coordination benefit. Indonesian Content Ecosystem: Victim + constrained → d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.46. Moderate extraction. Non-enrolled creators can switch platforms or content strategies, but do so at cost. Creator Unions: Organized + constrained → d≈0.45, f(d)≈0.45, σ=0.9 → χ≈0.18. Low effective extraction due to organized status and some bargaining capacity, but constrained by Meta's market dominance. Analytical Observer: analytical → d≈0.82, f(d)≈1.25, σ=1.2 → χ≈0.73. High effective extraction from systemic perspective; observer sees structural lock-in and dependency dynamics that individual creators may not fully perceive.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED (extractiveness 0.58 < 0.70 threshold, but high suppression and snare classification warrant analysis). The constraint avoids confusion between coordination and extraction through clear victimization: Indonesian creators bear measurable costs (production labor, algorithmic dependency, income volatility, data extraction) while Meta captures measurable benefits (engagement metrics, audience data, advertiser revenue, shareholder value). The 'performance bonus' framing could mislead toward Rope classification if analyzed superficially — the program does coordinate engagement behavior and solve a real problem (creator monetization). However, the coordination function is a secondary effect; primary function is extraction. The mandatrophy is resolved by recognizing: (1) Beneficiaries (Meta) experience low/negative effective extraction (d≈0.08, χ≈-0.07). (2) Victims (creators) experience high extraction (d≈0.92, χ≈0.81). (3) Active enforcement is unnecessary because network effects and income dependency suppress alternatives (high suppression=0.72 without requiring direct coercion). The classification as Snare (not Tangled Rope) is robust because the extraction is structurally primary; coordination is incidental to Meta's profit maximization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithm_transparency_threshold,
    'At what level of algorithm transparency would the bonus program cease functioning as a snare (shift from extraction to rope)?',
    'Comparative analysis of platforms with transparent algorithmic bonus criteria (YouTube Partner Program partial transparency) vs opaque ones (Meta); measurement of creator switching rates when algorithm criteria become predictable; correlation between transparency and perceived fairness',
    'If transparency threshold ≤ 0.6 (60% specificity): Program reclassifies from Snare to Tangled Rope; creators gain constrained exit through predictability. If transparency threshold > 0.8: Program remains Snare; opacity is essential to extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithm_transparency_threshold, empirical, 'Whether algorithmic transparency would convert snare to rope').

omega_variable(
    alternative_platform_critical_mass,
    'What audience scale for alternative platforms (TikTok, YouTube, BeReal) would constitute genuine exit option for Indonesian creators (shift from trapped to constrained)?',
    'Historical analysis of creator switching patterns when alternative platform reach approaches Meta parity; survey of creator revenue requirements vs alternative platform earning capacity; measurement of actual switching costs (audience loss percentage) when creators migrate',
    'If critical mass < 40% of Meta''s reach: creators still trapped (current state). If critical mass 40-70%: creators shift to constrained exit; classification moves from Snare toward Tangled Rope. If critical mass > 70%: genuine multi-platform ecosystem; classification becomes Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_critical_mass, empirical, 'Critical reach threshold for alternative platforms to enable exit').

omega_variable(
    indonesian_regulatory_intervention_capacity,
    'Does Indonesian regulatory authority (Kominfo, OJK) possess technical and political capacity to mandate algorithmic criteria disclosure or bonus payment audits?',
    'Analysis of existing Indonesian digital regulation (Law No. 1 of 2000 on Electronic Documents, draft Digital Platform regulation); comparison with EU Digital Markets Act enforcement; assessment of Meta''s revenue share and political influence relative to regulatory capacity',
    'If capacity exists and political will materializes: suppression drops below 0.60; classification shifts from Snare toward Tangled Rope or Scaffold (if sunset clause added). If capacity absent or political will blocked: suppression remains high; Snare classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indonesian_regulatory_intervention_capacity, empirical, 'Whether Indonesian regulatory system can enforce algorithmic transparency').

omega_variable(
    creator_coalition_formation_dynamics,
    'Can Indonesian creators organize collective bargaining power sufficient to negotiate with Meta (shift from powerless to organized)?',
    'Analysis of existing creator unions and collectives in Indonesia (Dewan Konten Indonesia, Serikat Kreator Indonesia); assessment of coalition size thresholds and Meta''s historical response to creator organizing; measurement of leverage gained through collective action vs individual negotiation',
    'If coalition critical mass < 10% of monetized creators: organization fails; creators remain powerless. If 10-30%: coalition formation possible but weak leverage. If > 30%: sufficient critical mass for meaningful negotiation; power atom could shift from powerless to organized, reclassifying creator perspective from Snare toward Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creator_coalition_formation_dynamics, empirical, 'Feasibility of creator collective organization and bargaining power').

omega_variable(
    creator_income_dependency_elasticity,
    'What percentage of enrolled creators have Meta bonus income as ≥50% of total household income (measure of trap severity)?',
    'Survey of Indonesian creators on income composition; analysis of creator exit/persistence rates correlated with income dependency ratios; measurement of survival income threshold in Indonesian context (living wage estimates)',
    'If ≥70% of creators are highly dependent: structural trap is severe; Snare classification robust. If 30-70%: trap is moderate; some creators have exit capacity (constrained rather than trapped). If < 30%: bonus is supplemental income; classification shifts toward Rope or Scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creator_income_dependency_elasticity, empirical, 'What percentage of creator income depends on Meta bonus').

omega_variable(
    algorithm_stability_volatility,
    'What is the month-to-month volatility in bonus eligibility and payment amounts for equivalent creators?',
    'Panel study of creator earnings over 12 months; measurement of coefficient of variation in bonus payments for creators maintaining constant content production; analysis of bonus criteria changes and retroactive policy shifts',
    'If volatility coefficient > 0.4 (40% variance for equivalent work): unpredictability reinforces trap through inability to plan; Snare classification strengthened, suppression ≥0.70. If < 0.2: predictability enables planning; classification shifts toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_stability_volatility, empirical, 'Month-to-month stability of bonus eligibility and payments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fb_creator_monetization_indonesia, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fbcm_tr_t0, fb_creator_monetization_indonesia, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fbcm_tr_t3, fb_creator_monetization_indonesia, theater_ratio, 3, 0.55).
narrative_ontology:measurement(fbcm_tr_t6, fb_creator_monetization_indonesia, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(fbcm_be_t0, fb_creator_monetization_indonesia, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fbcm_be_t3, fb_creator_monetization_indonesia, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(fbcm_be_t6, fb_creator_monetization_indonesia, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fb_creator_monetization_indonesia, resource_allocation).
narrative_ontology:affects_constraint(fb_creator_monetization_indonesia, platform_algorithm_opacity).
narrative_ontology:affects_constraint(fb_creator_monetization_indonesia, audience_ownership_asymmetry).
narrative_ontology:affects_constraint(fb_creator_monetization_indonesia, creator_income_precarity).

% DUAL FORMULATION NOTE:
% The Facebook creator monetization constraint is downstream of three related structural constraints: (1) platform_algorithm_opacity (ε≈0.65, Mountain or Tangled Rope) — algorithmic criteria are inherently opaque; (2) audience_ownership_asymmetry (ε≈0.72, Snare) — creators do not own audience relationships; (3) creator_income_precarity (ε≈0.48, Tangled Rope) — global creator economy structure. The Indonesia-specific instantiation (fb_creator_monetization_indonesia) has ε=0.58, representing the intersection of global platform extraction mechanisms with local economic dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fb_creator_monetization_indonesia, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
