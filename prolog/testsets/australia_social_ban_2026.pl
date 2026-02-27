% ============================================================================
% CONSTRAINT STORY: australia_social_ban_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_australia_social_ban_2026, []).

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
 *   constraint_id: australia_social_ban_2026
 *   human_readable: Australian Under-16 Social Media Ban (2025)
 *   domain: political/technological
 *
 * SUMMARY:
 *   The Australian Under-16 Social Media Ban (effective December 2025)
 *   represents a global first: legislation that shifts the burden of age
 *   verification onto platforms rather than parents or users. The constraint
 *   exhibits a fundamental structural tension between its beneficiary frame
 *   (child safety, parental authority restoration) and its victim structure
 *   (digital equity collapse, youth information access, platform
 *   profitability). The same legislative mechanism appears as pure
 *   coordination (government establishing clear rules), pure extraction
 *   (platforms losing revenue and teens losing social access), a degraded
 *   enforcement ritual (age verification theater), and a mixed
 *   coordination-extraction hybrid (parents gaining legitimacy while
 *   incurring enforcement labor). The theater_ratio trajectory (0.35 → 0.58)
 *   reflects that enforcement mechanisms depend increasingly on performative
 *   compliance signaling (platforms deploying visible age checks, government
 *   publicizing enforcement actions) rather than functional verification.
 *   This is a diagnostic case for jurisdictional divergence: while EU, UK,
 *   and US regulators debate online harms frameworks, Australia has enacted a
 *   bright-line rule, making it the first jurisdiction to test whether
 *   legislative suppression of platform access can be technically enforced at
 *   scale.
 *
 * KEY AGENTS:
 *   - Australian Government: Primary beneficiary (institutional/arbitrage) — establishes technological sovereignty, resolves parent advocacy pressure, demonstrates child protection leadership globally
 *   - Under-16 Users: Primary victim (powerless/trapped) — lose social coordination infrastructure, peer access, digital market signaling; cannot exit legally
 *   - Marginalized Youth: Secondary victim (powerless/trapped) — disproportionate loss of equity-providing infrastructure (peer support, mental health resources, labor market entry)
 *   - Social Media Platforms: Complex position (organized/constrained) — benefit from reduced liability and youth content moderation labor, but face compliance cost and revenue loss; constrained because abandoning Australian market carries geopolitical cost
 *   - Parents: Enforcement mediators (moderate/constrained) — gain legitimacy to restrict access but incur unpaid monitoring labor; constrained because social/legal expectations now hold them accountable for enforcement
 *   - Regulators: Performative enforcers (institutional/arbitrage) — maintain visible enforcement apparatus with modest functional verification capacity
 *   - Analytical Observer: Risk of naturalization — tends to frame the constraint as inevitable consequence of neurobiology rather than contingent policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(australia_social_ban_2026, 0.52).
domain_priors:suppression_score(australia_social_ban_2026, 0.68).
domain_priors:theater_ratio(australia_social_ban_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(australia_social_ban_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(australia_social_ban_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(australia_social_ban_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(australia_social_ban_2026, tangled_rope).
narrative_ontology:human_readable(australia_social_ban_2026, "Australian Under-16 Social Media Ban (2025)").
narrative_ontology:topic_domain(australia_social_ban_2026, "political/technological").

domain_priors:requires_active_enforcement(australia_social_ban_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(australia_social_ban_2026, australian_government).
narrative_ontology:constraint_beneficiary(australia_social_ban_2026, child_safety_advocates).
narrative_ontology:constraint_beneficiary(australia_social_ban_2026, parents_seeking_control).
narrative_ontology:constraint_victim(australia_social_ban_2026, social_media_platforms).
narrative_ontology:constraint_victim(australia_social_ban_2026, under_16_users).
narrative_ontology:constraint_victim(australia_social_ban_2026, digital_access_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDER-16 USER (SNARE) — Teenagers cannot exit the constraint without defiance or moving jurisdictions. No legitimate legal pathway to participate. Maximum experienced extraction: lost social capital, peer exclusion, inability to coordinate socially via platform networks, reduced access to youth-driven information distribution. Full suppression — behavioral alternatives (VPN, parent account spoofing, regional migration) carry legal/relational risk.
constraint_indexing:constraint_classification(australia_social_ban_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED YOUTH / DIGITAL EQUITY (SNARE) — Indigenous, rural, and low-income under-16s lose access to peer support networks, mental health resources, educational coordination, and labor market signaling that platforms provided. Disproportionate extraction relative to urban/affluent cohorts. No exit option for those who depended on platforms as primary social infrastructure.
constraint_indexing:constraint_classification(australia_social_ban_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SOCIAL MEDIA PLATFORMS (TANGLED ROPE) — Benefit from reduced regulatory fragmentation across Australian market (single jurisdiction, clear rules vs. patchwork EU/UK requirements). Also benefit from youth user reduction reducing content moderation labor and liability exposure. BUT face severe extraction: age-verification cost, algorithmic modification for Australian traffic, IP geoblocking infrastructure, legal liability for enforcement failures, profit loss from youth demographic. Constrained exit: cannot simply abandon Australian market (political pressure from tech regulation globally); must invest in compliance. Mixed benefit/burden structure.
constraint_indexing:constraint_classification(australia_social_ban_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: AUSTRALIAN GOVERNMENT (ROPE) — Pure coordination benefit: establishes clear rule-of-law for digital child protection, resolves political pressure from parent advocacy, demonstrates technological sovereignty. Extracts no resources from the regulated ecosystem. Low suppression cost relative to alternatives (EU fines model, UK online harms bill). Arbitrage exit: can adjust enforcement rigor, sunset if political winds shift, align with Five Eyes allies. Net coordinator, not extractor.
constraint_indexing:constraint_classification(australia_social_ban_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PARENTS (TANGLED ROPE) — Benefit from legitimized authority to restrict teen access without being the sole enforcer (state backup reduces parental blame for social exclusion). But also extract enforcement labor: monitoring, boundary-setting, managing peer-pressure arguments. Constrained exit: social/legal expectation to enforce the constraint; cannot simply permit teen use without legal/reputational cost. Mixed experience: coordination (shared rule) plus extraction (unpaid enforcement work).
constraint_indexing:constraint_classification(australia_social_ban_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ENFORCEMENT INSTITUTIONS / REGULATORS (PITON) — Age verification for platforms is largely performative: no single standard, verification accuracy ≤75%, spoofing trivial for determined teenagers, parent account sharing undetectable. Regulators perform enforcement through complaint mechanisms and platform audits, but the actual constraint relies on platform cooperation and parental supplementation. Theater ratio high because the regulatory apparatus must be seen to be enforcing even though the actual gate is weak. Piton classification reflects that enforcement is maintained through institutional inertia and political theater rather than functional verification capacity.
constraint_indexing:constraint_classification(australia_social_ban_2026, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / TECHNOLEGAL NATURALISM (MOUNTAIN) — Risk of treating the ban as an immutable consequence of developmental neuroscience (brain development, social comparison, addiction mechanics) rather than a contingent policy choice. This perspective risks false naturalization: the constraint appears to be written by neurobiology, not by Australian legislators choosing to shift verification burden onto platforms rather than fund alternative youth social infrastructure.
constraint_indexing:constraint_classification(australia_social_ban_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(australia_social_ban_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(australia_social_ban_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(australia_social_ban_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(australia_social_ban_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(australia_social_ban_2026, TR),
    TR >= 0.70.

:- end_tests(australia_social_ban_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significantly from under-16 users (social access, peer coordination, information flow) and from platforms (compliance cost, revenue loss from youth demographic, IP geoblocking infrastructure). But extraction is not maximal — platforms can operationalize age verification at incremental cost, and teenagers retain some alternative pathways (VPN, parent account, alternative platforms). The extractiveness trajectory (0.38 → 0.52) reflects increasing burden as compliance mechanisms mature and platforms internalize enforcement costs. Suppression (0.68): High. Multiple layers of suppression constrain alternatives: legal barriers to defiance (fines for parents/platforms), social pressure (peer normalization of ban), technical barriers (IP geoblocking for Australian users), and infrastructure capture (platforms must implement continent-wide age verification, making circumvention costly for users). Teenagers cannot easily exit through legitimate channels; platform defiance is legally risky for operators; parent non-enforcement carries social/legal cost. Theater ratio (0.58): Moderate-high. Age verification mechanisms are partially performative — platforms deploy visible verification workflows that lack technical rigor (accuracy ≤75%, spoofing trivial, parent account sharing undetectable). Regulators publicize enforcement audits and complaint mechanisms but rely on platform self-reporting and voluntary compliance. The regulatory apparatus must be seen to be working even though functional verification is weak. Theater increases over time (0.35 → 0.58) as enforcement becomes routinized and must sustain political legitimacy through visible action rather than measured outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates acute divergence between user experience (snare) and government experience (rope). The same mechanism — age verification and platform compliance — appears as complete extraction to trapped teenagers but as pure coordination to the government that designed it. Platforms occupy a hybrid position (tangled_rope) — they benefit from reduced youth-related liability but suffer revenue loss and infrastructure cost. Parents occupy a boundary: they gain legitimized authority (rope-like) but incur unpaid monitoring labor (tangled_rope). The gap is irreducible because it reflects a genuine asymmetry: the government can declare victory through legislative action and performative enforcement; the trapped victim cannot coordinate refusal across the jurisdiction. The piton perspective (regulators) highlights that enforcement is substantially theatrical — age verification is visible but not functionally robust, which allows government to claim success while actual constraint relies on platforms' profit motive and parents' social investment.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position relative to extraction flow. Under-16 users bear full extraction with zero offsetting benefit: d ≈ 1.0 (full target). Platforms are victims of compliance cost but beneficiaries of reduced liability — net victim position with some coordination benefit: d ≈ 0.65 (victim but not powerless). Government is pure beneficiary with arbitrage exit: d ≈ 0.05 (beneficiary). Parents benefit from legitimacy but bear enforcement labor: d ≈ 0.50 (symmetric). Regulators maintain visible authority with low functional cost: d ≈ 0.25 (beneficiary but constrained). The analytical observer at civilizational scope risks d ≈ 0.0 (full naturalization) unless careful to distinguish physical constraint from policy choice. The engine derives these d values from the beneficiary/victim declarations and exit options; they determine how each agent experiences the effective extractiveness χ = ε × f(d) × σ(S).
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The constraint's classification as 'tangled rope' (claimed_type) depends on resolving whether the coordination dimension (child safety, parental authority restoration, digital rule-of-law establishment) genuinely outweighs the extraction dimension (youth access loss, platform compliance cost, regulatory capture risk). Current extractiveness (0.52) suggests the classification is defensible (0.40 ≤ χ ≤ 0.90 for tangled rope at some perspectives). However, three scenarios would shift the classification: (1) If age verification proves infeasible and enforcement becomes purely theatrical (theater_ratio → 0.85+), platforms experience snare-type extraction rather than tangled rope, and the entire constraint degrades to piton (degraded enforcement ritual). (2) If youth alternative coordination infrastructure (Discord, gaming, school apps) proves functionally robust, the victim status of under-16s downgrades from snare to constrained/mobile, and the constraint approaches pure coordination (rope) rather than tangled rope. (3) If platform compliance cost exceeds revenue loss by >2x, platforms experience pure extraction without offsetting benefit, and the constraint approaches snare-tier severity for this agent. The mandatrophy is unresolved because these empirical conditions determine whether the constraint is genuinely hybrid or misclassified. Setting mandatrophy_resolved to false preserves this uncertainty pending omega variable resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    age_verification_technical_feasibility,
    'Can age verification technology achieve >85% accuracy without surveillance-grade identity collection or biometric databases?',
    'Deployed age-verification performance metrics; fraud detection rates; privacy impact assessment comparison with actual technology deployed',
    'If feasible: platforms bear moderate compliance cost (tangled rope confirmed). If not feasible: enforcement becomes theater (piton), actual constraint relies on parental/peer policing rather than technical gate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(age_verification_technical_feasibility, empirical, 'Technical feasibility of age verification without invasive identity collection').

omega_variable(
    youth_alternative_coordination,
    'Do under-16s develop alternative social coordination infrastructure (Discord, WhatsApp, gaming platforms, school-based apps) that functionally replace banned social media networks?',
    'Longitudinal survey of teen social network topology; measurement of peer communication lag and coordination failure rates pre/post-ban; ethnographic documentation of alternative platforms'' adoption',
    'If robust alternatives emerge: victim status of under-16s is ''downgraded'' from snare to tangled_rope (some exit via alternatives). If alternatives remain fragmented: snare classification confirmed — trapped agents cannot coordinate effectively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(youth_alternative_coordination, empirical, 'Whether under-16s develop functionally equivalent alternative social infrastructure').

omega_variable(
    platform_profit_impact_asymmetry,
    'Do social media platforms experience greater profit loss from Australian market isolation than from compliance cost, or does youth user reduction free resources that offset revenue loss?',
    'Quarterly earnings analysis pre/post-ban; geographic profit breakdown; content moderation cost reduction; advertising rate changes in Australian market segment',
    'If profit loss > compliance cost: platforms extract net from the constraint (snare-tier). If compliance cost > profit loss: platforms experience pure extraction (tangled rope confirmed). If profit loss < compliance cost: platforms benefit overall (rope-tier coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_profit_impact_asymmetry, empirical, 'Net financial impact of the ban on social media platform profitability').

omega_variable(
    parental_enforcement_labor_measurement,
    'How much unpaid parental labor (monitoring, boundary-setting, conflict management) does the constraint require, and is this labor equally distributed across socioeconomic strata?',
    'Time-use survey of parental enforcement activities; correlation with household income and parental employment status; measurement of mental health impacts (parental stress)',
    'If labor is evenly distributed: moderate extraction from parents (tangled rope confirmed). If labor concentrates on lower-income families: asymmetric extraction from already-constrained agents (snare-tier for that cohort).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parental_enforcement_labor_measurement, empirical, 'Distribution and magnitude of parental enforcement labor required by the ban').

omega_variable(
    mental_health_displacement_effect,
    'Does the ban reduce mental health crisis rates in under-16s (positive coordination outcome) or does it displace mental health support-seeking behavior toward less-moderated alternative platforms (negative externality)?',
    'Pre/post-ban emergency mental health utilization rates; analysis of crisis support availability on alternative platforms; longitudinal mental health surveys comparing Australian cohorts to control jurisdictions',
    'If reduction in crises: victim status downgraded (net safety gain). If displacement to worse platforms: victim status upgraded (net harm from constraint). If no change: constraint is performative for mental health (piton indicator).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mental_health_displacement_effect, empirical, 'Mental health effects of displacement from banned platforms to alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(australia_social_ban_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ausban_tr_t0, australia_social_ban_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ausban_tr_t3, australia_social_ban_2026, theater_ratio, 3, 0.5).
narrative_ontology:measurement(ausban_tr_t6, australia_social_ban_2026, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(ausban_be_t0, australia_social_ban_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ausban_be_t3, australia_social_ban_2026, base_extractiveness, 3, 0.47).
narrative_ontology:measurement(ausban_be_t6, australia_social_ban_2026, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(australia_social_ban_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(australia_social_ban_2026, digital_access_equity_global).
narrative_ontology:affects_constraint(australia_social_ban_2026, platform_regulatory_capture).
narrative_ontology:affects_constraint(australia_social_ban_2026, youth_mental_health_surveillance).

% DUAL FORMULATION NOTE:
% The Australian ban decomposes into multiple structural constraints: age verification infrastructure (technological), enforcement theater (regulatory), parental delegation (social), and youth access loss (equity). These are linked through network.affects_constraints because they share institutional coupling — regulatory failure on one dimension (age verification infeasibility) cascades to others (enforcement becomes theater, parental labor increases, equity loss accelerates). The primary constraint (australia_social_ban_2026) models the legislative mechanism; downstream constraints model specific failure modes and spillover effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(australia_social_ban_2026, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
