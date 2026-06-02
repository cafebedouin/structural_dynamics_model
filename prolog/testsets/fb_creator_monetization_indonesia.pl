% ============================================================================
% CONSTRAINT STORY: fb_creator_monetization_indonesia
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   human_readable: Meta Performance Bonus Monetization for Indonesian Creators
 *   domain: technological/economic/labor
 *
 * SUMMARY:
 *   Meta's Performance Bonus program in Indonesia represents a canonical
 *   hybrid constraint: it solves a genuine coordination problem (connecting
 *   dispersed creators with advertising markets) while simultaneously
 *   extracting labor value through algorithmic control and precarity. The
 *   program launched as a regional expansion of Meta's Creator Fund and Reels
 *   Play Bonus, offering Indonesian creators their first mainstream
 *   monetization pathway. However, the program's structure embeds extraction:
 *   creators are classified as independent contractors with no employment
 *   protections; algorithm opacity prevents creators from understanding
 *   income determinants; platform dependency is total because Meta controls
 *   both the audience distribution mechanism and the advertising supply;
 *   account suspensions are arbitrary and unappealable; and income volatility
 *   is high and unpredictable. The constraint exhibits all characteristics of
 *   tangled rope: (1) genuine coordination function—Meta solves the real
 *   problem of connecting Indonesian creators to global advertising; (2)
 *   asymmetric extraction—the income share favors Meta substantially; (3)
 *   active enforcement—algorithmic ranking and account management enforce
 *   creator compliance with Meta's policies. The theater ratio (0.65)
 *   reflects that Meta's monetization process involves significant
 *   performative elements: creators are told their content is being evaluated
 *   on 'engagement,' 'authenticity,' and 'community guidelines,' but these
 *   metrics are opaque, algorithmically determined, and subject to unilateral
 *   change. The suppression ratio (0.62) captures barriers to exit: lack of
 *   alternative income pathways, audience lock-in via network effects, skill
 *   specificity to Meta's platform, and informal institutional barriers
 *   (creator associations lack bargaining power). The extractiveness value
 *   (0.58) reflects that while the program does provide income to creators,
 *   the income share is asymmetric and the conditions of work (algorithmic
 *   control, precarity, lack of protections) exceed what would be expected in
 *   comparable labor arrangements.
 *
 * KEY AGENTS:
 *   - Indonesian Content Creators (Individual): Primary victims (powerless/trapped) — lack alternative monetization pathways; income depends entirely on Meta's platform; subject to arbitrary algorithmic suppression and account suspensions
 *   - Creator Associations and Guilds (Organized): Secondary victims (moderate/constrained) — have some agency and coordination capacity but limited bargaining power relative to Meta; constrained by network effects and audience lock-in
 *   - Meta Platform Corporation: Primary beneficiary (institutional/arbitrage) — extracts value through intermediation; controls algorithm, payment terms, and policy enforcement; has exit options (can redirect monetization to other regions or strategies)
 *   - Advertising Ecosystem: Secondary beneficiary (institutional/arbitrage) — benefits from creator supply and engagement metrics; has exit options (other platforms, direct creator deals)
 *   - Indonesian Labor Ministry: Weak institutional regulator (institutional/constrained) — has formal authority but constrained by Meta's scale and threat of program relocation; sees its regulatory capacity as eroded
 *   - Creator Cooperative Movement: Emerging alternative provider (organized/constrained) — building alternative platforms but constrained by network effects and capital requirements; represents the scaffold perspective
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (Meta's dominance, network effects) as laws of technology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fb_creator_monetization_indonesia, 0.58).
domain_priors:suppression_score(fb_creator_monetization_indonesia, 0.62).
domain_priors:theater_ratio(fb_creator_monetization_indonesia, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fb_creator_monetization_indonesia, extractiveness, 0.58).
narrative_ontology:constraint_metric(fb_creator_monetization_indonesia, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fb_creator_monetization_indonesia, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fb_creator_monetization_indonesia, tangled_rope).
narrative_ontology:human_readable(fb_creator_monetization_indonesia, "Meta Performance Bonus Monetization for Indonesian Creators").
narrative_ontology:topic_domain(fb_creator_monetization_indonesia, "technological/economic/labor").

domain_priors:requires_active_enforcement(fb_creator_monetization_indonesia).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fb_creator_monetization_indonesia, meta_platform).
narrative_ontology:constraint_beneficiary(fb_creator_monetization_indonesia, advertising_ecosystem).
narrative_ontology:constraint_victim(fb_creator_monetization_indonesia, indonesian_content_creators).
narrative_ontology:constraint_victim(fb_creator_monetization_indonesia, creator_labor_standards).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS CREATOR (SNARE) — Trapped by absence of alternative monetization pathways in Indonesian economy. The creator has no real exit: leaving Meta means forfeiting the primary available income stream. Suppression is high: algorithm opacity, arbitrary account suspensions, metric manipulation, vague content policies. The constraint extracts labor through algorithmic control while offering subsistence-level income. From the creator's biographical perspective, the constraint appears as pure extraction with a coercive mechanism (platform dependency) masquerading as opportunity.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ORGANIZED CREATOR COLLECTIVE (TANGLED ROPE) — Organized creators (guilds, associations, unions in formation) experience both genuine coordination and asymmetric extraction. The platform solves a real problem: connecting dispersed creators with audiences and advertisers. But the coordination comes with embedded extraction: opaque algorithms, unilateral policy changes, no collective bargaining power. These agents have some agency and some exit options (YouTube, TikTok, local platforms) but constrained by network effects and audience lock-in. The temporal horizon is generational because sustainable creator labor standards require institutional development beyond the immediate platform.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: META PLATFORM CORPORATION (ROPE) — Experiences the constraint as pure coordination infrastructure. The Performance Bonus program solves Meta's coordination problem: monetizing creator content attracts creators, creator content attracts advertisers, advertisers pay Meta. This is a functioning ecosystem. From Meta's perspective (immediate timescale, arbitrage exit options, global scope), the bottleneck is traffic and engagement. The monetization program is overhead to maximize creator output. Extraction flows toward Meta, but Meta perceives the constraint as enabling collective action.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERTISING ECOSYSTEM (ROPE) — Advertisers see the Performance Bonus program as a coordination solution: creators are incentivized to generate engaging content; Meta controls quality and brand safety through its algorithms and policies. The constraint solves the ad-tech coordination problem: matching advertiser budgets to creator inventory. Advertisers benefit from lower customer acquisition costs and can arbitrage across platforms. They have exit options (Google, TikTok, direct creator deals) and experience the Meta constraint as enabling, not extractive.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INDONESIAN LABOR STANDARDS ADVOCATES (TANGLED ROPE) — Civil society organizations, labor advocates, and government agencies recognize both coordination benefit and labor extraction. The program does provide income to creators who otherwise lack employment pathways. But it does so while fragmenting labor law: creators are classified as 'independent partners,' stripping them of employment protections (minimum wage, benefits, collective bargaining, workplace safety). The constraint coordinates labor supply for Meta's benefit while extracting labor protection standards. Advocates have some agency (regulations, tax codes, pressure campaigns) but constrained by Meta's scale and exit threat (relocate higher-paying programs to other regions).
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INDONESIAN LABOR MINISTRY (PITON) — Institutional actor with formal regulatory authority but eroding enforcement capacity. The ministry classifies creators under existing labor law, but the classification is performative: creators are 'independent contractors' on paper, with all the labor protections that implies (which is none). The regulatory mechanism persists through institutional habit despite not functioning: the ministry issues guidance, Meta ignores it, creators have no recourse. Theater is high because regulatory authority is asserted but not exercised. The ministry is trapped in a piton state — the constraint has outlived its regulatory function, maintained only because replacement mechanisms (new labor classifications, platform regulation, creator cooperatives) have not matured.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: CREATOR COOPERATIVE AND ALTERNATIVE PLATFORM MOVEMENT (SCAFFOLD) — Organized actors (platform cooperatives, creator collectives, local platforms) see Meta's monetization constraint as temporary and obsolete. The movement is building alternative pathways: Indonesian-local platforms with transparent algorithms, creator cooperatives that distribute advertising revenue directly, peer-to-peer funding mechanisms. The scaffold classification reflects that these alternatives have a concrete sunset clause: as cooperative platforms mature and network effects distribute, Meta's monopoly on creator monetization weakens. Current timescale is generational (10-20 years) because building institutional alternatives requires sustained coordination. This perspective sees the constraint as a problem-to-be-solved with a known exit pathway, not as immutable.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURALIZED NETWORK EFFECT VIEW (MOUNTAIN) — From the civilizational/universal vantage, the monetization bottleneck appears to follow from an immutable law: in digital platforms, network effects create winner-take-all outcomes that are inherent to the technology itself. Creators must go where audiences are; audiences go where creators are; Meta has the largest network; therefore, creator dependence on Meta is a law of nature, not a constraint artifact. However, the structural data contradicts this mountain classification — Meta's dominance in Indonesia is contingent on policy choices (algorithm design, monetization rules, account suspension policies), not technological necessity. The engine will classify this as a false summit, revealing the naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
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
 *   Extractiveness (0.58): Moderately high. The measurement trajectory shows rising extractiveness over the interval (0.35 → 0.48 → 0.58), reflecting increasing reliance on the platform as creators invest in optimization and audience building. The base value of 0.58 captures that while creators do receive income, the income share is substantially lower than what comparable labor arrangements would offer. An independent content creator selling directly to advertisers would capture 80-90% of advertising revenue; via Meta's platform, creators capture 45-55%, with Meta extracting 45-55%. The extractiveness also captures algorithmic control mechanisms that suppress income for non-compliant content, reducing predictability and creating dependency on Meta's changing metrics. Suppression (0.62): Moderately high. Barriers to exit include: (1) absence of alternative platforms with comparable reach in Indonesia (trapping); (2) audience lock-in (creators' audiences exist on Meta, not on creator's own channels); (3) skill specificity (creator income optimization requires knowledge of Meta's algorithm, which is opaque); (4) institutional barriers (creator associations lack collective bargaining power to negotiate terms with Meta); (5) threat of program relocation (if creators organize or pressure Meta, the program can be moved to other regions with less resistance). Theater ratio (0.65): High. Meta's monetization process involves substantial performance elements: (1) the 'engagement' and 'quality' metrics that determine income are presented as objective algorithmic assessments but are actually opaque and subject to policy changes; (2) creator behavior in response to metrics produces content that is optimized for algorithm, not for creator or audience preferences (dance trends, format constraints, notification-seeking); (3) the creator program itself is presented as an opportunity for 'empowerment' and 'economic inclusion,' masking the extraction mechanism. Theater has risen over the interval as creators have become more aware of algorithmic gaming and as policy changes (suspensions, metric shifts) have exposed the opaque evaluation process.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces dramatic perspectival divergence. Meta perceives the Performance Bonus program as pure coordination (Rope from institutional/arbitrage perspective) — the platform solves the problem of monetizing creator content and matching advertiser budgets to creator inventory. The organized creator collective perceives genuine coordination AND asymmetric extraction (Tangled Rope from moderate/constrained perspective) — the platform does connect them to advertising, but on unfavorable terms and without agency. The precarious individual creator perceives pure extraction with coercive trapping (Snare from powerless/trapped perspective) — the 'monetization opportunity' is actually the only available income pathway, and algorithmic suppression is indistinguishable from arbitrary punishment. The Indonesian labor ministry perceives its own regulatory authority as degraded (Piton from institutional/constrained perspective) — formal classification of creators as 'independent partners' is technically defensible under current law, but the category has outlived its functional purpose as creators have become economically dependent. The creator cooperative movement perceives a temporary problem with a known exit pathway (Scaffold from organized/constrained perspective) — Meta's monetization advantage is contingent on network effects, and as alternative platforms mature, the constraint will dissolve. The civilizational analytical observer risks perceiving network effects as immutable (Mountain from analytical/analytical perspective) — but the structural data reveals this as a false summit, showing that concentration is contingent on policy choices rather than technological necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to the extraction flow. Meta as beneficiary with arbitrage exit (can redirect monetization to other regions, other strategies) gets low d (~0.15), producing negative effective extractiveness chi from its perspective—Meta experiences the constraint as enabling, not extractive. Precarious creators as victims with trapped exit (no alternative income pathways) get high d (~0.92), producing high effective extractiveness chi—they experience maximum extraction. Organized creators as victims with constrained exit (can exit at high cost via audience relocation, skill transfer to other platforms) get moderate-high d (~0.65), producing moderate effective extractiveness. The Indonesian labor ministry as a regulator with constrained exit (has formal authority but limited enforcement capacity) gets moderate d (~0.55). The creator cooperative movement as organized agents with constrained exit but with a known sunset pathway gets moderate d (~0.60) but lower perceived extraction because the constraint is perceived as temporary. The analytical observer at civilizational timescale gets d (~0.73) reflecting that observation across multiple perspectives reveals ambiguity about whether the constraint is technologically necessary or contingent on policy.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that tangled rope is the structurally correct classification when evaluated from the 'global optimum' analytical perspective: Meta does solve a genuine coordination problem (connecting creators to advertising), AND the constraint exhibits asymmetric extraction (income share favors Meta) AND active enforcement (algorithmic ranking). The six perspectives show that all six types are legitimate from different structural positions, but the analytical observer at generational timescale (organized/constrained, national scope) sees tangled rope as the canonical form. The snare classification from the precarious creator's perspective is the precarious creator's legitimate experience, not a misclassification — it reveals that tangled rope hides within it a snare for agents with no exit options. The rope classification from Meta's perspective is not wrong; it is the perspective of the beneficiary with arbitrage options. The mandatrophy is resolved not by choosing one type, but by recognizing that the tangled rope classification is robust to directionality variation: all agents, whether beneficiary or victim, agree that coordination function exists AND asymmetric extraction exists AND active enforcement exists. The disagreement is about whether the coordination is sufficient to justify the extraction, not about whether both elements exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_suppression_vs_engagement_optimization,
    'Does Meta''s algorithmic ranking system suppress creator income through content suppression, or does it merely optimize for engagement in ways that disadvantage small creators?',
    'Comparative analysis of algorithm-suppressed content vs. organic decline. Measurement of creator reach penalties post-policy violation vs. organic reach decay over time. Documentation of explicit suppression rules vs. emergent effects of engagement-maximization.',
    'If algorithmic suppression is explicit: extraction mechanism is deliberate (snare classification stands). If suppression is emergent from engagement metrics: extraction is systemic but not intentional (tangled rope classification strengthens). If suppression is minimal and reach decline is organic: constraint moves toward rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_suppression_vs_engagement_optimization, empirical, 'Whether content suppression is explicit policy or emergent from engagement optimization').

omega_variable(
    income_sufficiency_threshold,
    'At what income level does the Performance Bonus program transition from supplement to primary livelihood? Does this threshold vary by creator segment or region?',
    'Income distribution analysis by creator tier, region, and content category. Longitudinal tracking of creators who exit the program vs. those who intensify engagement as income rises. Comparison of median Meta income vs. local minimum wage and cost of living.',
    'If most creators use Meta income as supplement: constraint is less extractive (moderate supplemental income + existing livelihood = lower d). If many creators depend on Meta income for primary livelihood: constraint is highly extractive (trapped exit option + dependency = higher d). Threshold variation indicates differential extraction by geography and creator type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(income_sufficiency_threshold, empirical, 'Income sufficiency threshold and variation by creator segment').

omega_variable(
    alternative_platform_viability,
    'Do Indonesian-local platforms or cooperative alternatives offer genuinely competitive monetization rates, or do they remain economically marginal due to advertiser preference for scale?',
    'Comparative monetization rates (CPM, cost-per-engagement) across platforms. Creator switching analysis: what percentage of creators achieve comparable income on alternative platforms? Network effects measurement: how much audience follows creators to alternatives vs. stays on Meta?',
    'If alternatives are genuinely viable: scaffold perspective is confirmed, exit is not trapped (creators can arbitrage). If alternatives are economically marginal: creators face constrained rather than mobile exit, strengthening snare classification. If viable but require scale (minimum audience size): exit is arbitrage-dependent, changing directionality for organized creators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Whether alternative platforms offer competitive monetization viability').

omega_variable(
    policy_classification_ambiguity,
    'Is the creator classification as ''independent partner'' technically accurate under Indonesian labor law, or does it misapply labor law categories to evade creator protections?',
    'Legal analysis of Indonesian labor code (UU No. 13 Tahun 2003) applied to creator circumstances. Comparison with analogous gig economy rulings (ride-share, delivery platforms) in Indonesian courts. Assessment of whether ''platform-coordinated work'' has been tested against employment definitions.',
    'If classification is technically accurate under current law: extraction mechanism is legal but represents a gap in labor law (policy preference, not constraint artifact). If classification misapplies law: constraint is extractive precisely because it evades labor protections (snare classification strengthens). If classification is untested: ambiguity allows both interpretations, sustaining the tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_classification_ambiguity, conceptual, 'Whether creator classification as ''independent partner'' aligns with Indonesian labor law').

omega_variable(
    suppression_internalization,
    'Do creators internalize Meta''s metrics as measures of content quality, or do they perceive the metrics as externally imposed constraints?',
    'Creator interviews and ethnographic analysis. Content surveys tracking creator stated intent (personal expression vs. algorithm optimization). Measurement of creator behavior change post-policy (do creators adapt to metrics, or resist?).',
    'If internalized: creators are identity-locked to Meta''s optimization logic, making exit even more difficult (exit would require abandoning their identity as ''successful creators''). If perceived as external: suppression remains identity-neutral constraint, and exit is more cognitively accessible (constrained rather than identity_locked). Internalization strengthens the snare classification for biographical timescale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether creators internalize algorithmic metrics or perceive them as external constraints').

omega_variable(
    network_effects_contingency,
    'Is Meta''s audience concentration (the reason creators cannot exit) a contingent outcome of policy choices (algorithm, monetization rules, competitor suppression), or a necessary feature of platform technology?',
    'Historical counterfactual analysis: what if Meta''s algorithm had been transparent? What if monetization had been shared more equitably? Comparative platform analysis: do all winner-take-all outcomes show the same degree of concentration, or is Meta''s concentration higher than technological necessity would dictate?',
    'If concentration is contingent: mountain classification is false summit (naturalization of policy choice). If concentration is necessary: mountain classification is defensible. If indeterminate: omega resolves to unresolvable conceptual ambiguity, sustaining the analytical mountain as a legitimate perspectival reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(network_effects_contingency, conceptual, 'Whether network effects concentration is technologically necessary or contingent on policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fb_creator_monetization_indonesia, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fbcreator_tr_t0, fb_creator_monetization_indonesia, theater_ratio, 0, 0.45).
narrative_ontology:measurement(fbcreator_tr_t3, fb_creator_monetization_indonesia, theater_ratio, 3, 0.58).
narrative_ontology:measurement(fbcreator_tr_t6, fb_creator_monetization_indonesia, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(fbcreator_be_t0, fb_creator_monetization_indonesia, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fbcreator_be_t3, fb_creator_monetization_indonesia, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(fbcreator_be_t6, fb_creator_monetization_indonesia, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fbcreator_su_t0, fb_creator_monetization_indonesia, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fbcreator_su_t3, fb_creator_monetization_indonesia, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(fbcreator_su_t6, fb_creator_monetization_indonesia, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fb_creator_monetization_indonesia, resource_allocation).
narrative_ontology:affects_constraint(fb_creator_monetization_indonesia, algorithmic_amplification_inequality).
narrative_ontology:affects_constraint(fb_creator_monetization_indonesia, gig_economy_labor_classification).
narrative_ontology:affects_constraint(fb_creator_monetization_indonesia, platform_monopoly_indonesia).

% DUAL FORMULATION NOTE:
% Meta's Performance Bonus monetization decomposes into three structurally distinct constraints: (1) resource_allocation coordination between creators and advertisers (what this story models), (2) algorithmic inequality in content distribution (downstream of this story — affects visibility and income), (3) labor classification ambiguity (upstream of this story — enables the monetization extraction). All three share extractiveness vectors but have distinct ε values. The monetization constraint is the primary mechanism through which algorithmic inequality translates into economic extraction; the labor classification ambiguity is the policy precondition that allows independent contractor status instead of employment protections.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fb_creator_monetization_indonesia, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
