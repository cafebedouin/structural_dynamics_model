% ============================================================================
% CONSTRAINT STORY: content_moderation_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_content_moderation_asymmetry, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: content_moderation_asymmetry
 *   human_readable: Content Moderation Asymmetry in Digital Platforms
 *   domain: digital_governance/information_systems
 *
 * SUMMARY:
 *   Content moderation on global digital platforms exhibits a structural
 *   asymmetry: platforms claim coordination function (protecting users from
 *   harmful content, maintaining platform health) while simultaneously
 *   extracting value through asymmetric enforcement that concentrates power
 *   over speech in platform operators' hands. The same rule-enforcement
 *   infrastructure that coordinates around illegal material also suppresses
 *   legitimate speech that contradicts platform interests or threatens
 *   advertiser relationships. This constraint is a diagnostic exemplar of
 *   Tangled Rope structure: genuine coordination function exists (large-scale
 *   content management requires standards), but the coordination mechanism is
 *   weaponized for extraction through algorithmic opacity, opaque
 *   enforcement, and systematically imbalanced appeals. The extractiveness
 *   trajectory (0.32 → 0.62 over interval) reflects platform maturation: as
 *   platforms scale, algorithmic enforcement becomes more central to
 *   operations, transparency declines, and asymmetric enforcement
 *   intensifies. Theater ratio (0.48 → 0.68) tracks the gap between published
 *   community guidelines and actual enforcement opacity — platforms invest in
 *   the performative apparatus (publishing moderation reports) while
 *   algorithmic decisions remain black-boxed.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — control moderation infrastructure; extract value through asymmetric enforcement benefiting corporate advertisers and institutional accounts
 *   - Marginalized Content Creators: Primary victims (powerless/trapped) — content removed with minimal recourse; algorithmic decisions opaque; alternative platforms offer minimal reach
 *   - Independent Creators: Secondary victim (moderate/constrained) — experience mixed coordination (reach/monetization) and extraction (inconsistent enforcement, appeal barriers)
 *   - Corporate Brands and Verified Accounts: Beneficiary (institutional/arbitrage) — preferential moderation; human review on appeals; faster resolution
 *   - User Epistemic Commons: Diffuse victim (powerless/trapped) — collective information environment degraded by asymmetric enforcement; false positives and false negatives concentrate on low-power users
 *   - Platform Moderation Teams: Inter-institutional constraint (institutional/constrained) — enforce asymmetric extraction while experiencing resource constraints and policy contradictions
 *   - Digital Rights Coalition: Organized challenger (organized/constrained) — building regulatory and technological alternatives to platform-controlled moderation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes both legitimate coordination and extractive weaponization of the same infrastructure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(content_moderation_asymmetry, 0.58).
domain_priors:suppression_score(content_moderation_asymmetry, 0.68).
domain_priors:theater_ratio(content_moderation_asymmetry, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(content_moderation_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(content_moderation_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(content_moderation_asymmetry, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(content_moderation_asymmetry, tangled_rope).
narrative_ontology:human_readable(content_moderation_asymmetry, "Content Moderation Asymmetry in Digital Platforms").
narrative_ontology:topic_domain(content_moderation_asymmetry, "digital_governance/information_systems").

domain_priors:requires_active_enforcement(content_moderation_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(content_moderation_asymmetry, platform_operators).
narrative_ontology:constraint_beneficiary(content_moderation_asymmetry, corporate_advertisers).
narrative_ontology:constraint_beneficiary(content_moderation_asymmetry, institutional_verified_accounts).
narrative_ontology:constraint_victim(content_moderation_asymmetry, marginalized_content_creators).
narrative_ontology:constraint_victim(content_moderation_asymmetry, user_epistemic_commons).
narrative_ontology:constraint_victim(content_moderation_asymmetry, appeal_disenfranchised_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUSPENDED ACCOUNT HOLDER (SNARE) — User content removed with minimal appeal pathway; platform algorithm-mediated decisions are opaque and non-negotiable. No meaningful exit: alternative platforms have smaller reach; reputation damage persists across networks. Trapped within the platform's enforcement regime despite formal terms of service.
constraint_indexing:constraint_classification(content_moderation_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT CONTENT CREATOR (TANGLED ROPE) — Genuine coordination function: platform enables audience discovery and monetization that would not exist without moderation infrastructure. Simultaneously extracted through asymmetric enforcement: content moderation rules are applied inconsistently; algorithmic amplification favors institutional accounts; appeal mechanisms are resource-intensive. Medium extraction with genuine benefits creates the tangled rope structure.
constraint_indexing:constraint_classification(content_moderation_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CORPORATE BRAND / VERIFIED INSTITUTION (ROPE) — Benefits from preferential moderation: corporate accounts receive human review on appeals; content moderation delays are minimized; algorithmic amplification is consistently favorable. Experiences the moderation system as coordination: it stabilizes the information environment in ways that benefit institutional actors. Low experienced extraction; high value from the coordination function.
constraint_indexing:constraint_classification(content_moderation_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM ACCOUNTABILITY COALITION (SCAFFOLD) — Organized actors (civil society organizations, regulatory bodies, digital rights groups) see the moderation asymmetry as a temporary governance failure with a sunset clause. Transparency mandates, algorithmic auditing requirements, and mandatory appeal mechanisms are building alternative pathways to platform accountability. External pressure is incrementally raising enforcement standards and appeal fairness. Scaffold structure emerges from the sunset logic: regulatory frameworks (EU DSA, potential US legislation) establish declining timelines for platform compliance, after which extraction mechanisms lose legal force.
constraint_indexing:constraint_classification(content_moderation_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM MODERATION TEAM (TANGLED ROPE) — Inter-institutional perspective: the moderation team has genuine coordination function (managing scale of content, protecting from illegal material) but also enforces asymmetric extraction on behalf of platform leadership. Team members are constrained by resource limitations, policy contradictions, and conflicting directives. They experience the system as both necessary infrastructure and an instrument of extraction against lower-power users. Different exit costs from platform leadership (who can arbitrage away) vs moderation workers (constrained by labor market).
constraint_indexing:constraint_classification(content_moderation_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL MODERATION PLAYBOOK (PITON) — Community guidelines, enforcement mechanisms, and appeal processes are largely performative vestiges of earlier web governance models. Platforms maintain the theater (publishing guidelines, showing enforcement metrics) while actual enforcement is algorithmic and opaque. The traditional playbook persists through institutional inertia despite recognized failures; alternatives (algorithmic transparency, decentralized moderation, user-controlled curation) have not fully replaced it.
constraint_indexing:constraint_classification(content_moderation_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational perspective, content moderation at platform scale requires both coordination (shared standards to manage illegal/harmful content) and extraction (asymmetric enforcement that concentrates power). The constraint exhibits both functions structurally. Effective extraction chi is high due to global scope amplifying moderation asymmetries and algorithmic decisions affecting billions; the suppression mechanism is substantial because appeals are resource-gated and visibility is algorithm-determined. The classification is Tangled Rope, not Snare, because genuine coordination for harm prevention exists alongside extraction.
constraint_indexing:constraint_classification(content_moderation_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(content_moderation_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(content_moderation_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(content_moderation_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(content_moderation_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(content_moderation_asymmetry, TR),
    TR >= 0.70.

:- end_tests(content_moderation_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platform operators extract significant value through moderation asymmetries: they control visibility (algorithmic amplification favors institutional accounts), control appeals (resource-gated), and control standards (no transparency into decision logic). The extraction is substantial but not maximal because independent creators do receive genuine value from platform access and monetization infrastructure. Suppression (0.68): High. Multiple suppression mechanisms operate simultaneously: algorithmic opacity (users cannot predict what will be removed or amplified), appeal friction (complex processes with low reversal rates), and reputational damage (removals are visible; appeals are not). These barriers operate across layers — technical, procedural, and social. Theater ratio (0.64): Moderate-high. Platforms publish detailed moderation reports and community guidelines (the theater) while actual enforcement happens through opaque algorithms (the gap). The theater has increased as platforms face regulatory scrutiny, but it obscures rather than illuminates the actual enforcement mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between powerless/trapped and institutional/arbitrage contexts. The suspended user sees a Snare (extraction with no exit); the platform sees Rope (coordination). The independent creator sees Tangled Rope (mixed coordination and extraction); the corporate brand sees Rope (coordination with preferential treatment). The moderation team sees Tangled Rope (infrastructure and extraction simultaneously) with institutional constrained exit, differentiated from platform operators by labor-market constraints. The digital rights coalition sees Scaffold (temporary problem with regulatory sunset) because they perceive effective exit paths (transparency mandates, algorithmic auditing, decentralized alternatives) that give them agency over the constraint's future. The analytical observer sees Tangled Rope at civilizational scope, recognizing both coordination (harm reduction) and extraction (asymmetric enforcement) as structural features.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators (beneficiaries with arbitrage exit) derive low d values: they control the moderation system and can arbitrage to alternative business models if moderation becomes untenable. Marginalized users (victims with trapped exit) derive high d values: they depend on platform reach and have minimal alternatives; their suppression is maximal because algorithmic decisions are non-transparent and appeals are resource-gated. Independent creators (mixed victims/beneficiaries with constrained exit) derive mid-range d values reflecting mixed cost-benefit. Moderation teams present an inter-institutional perspective with constrained exit (labor market limited, but not identity-locked if they maintain professional distance from platform ideology). Corporate brands (beneficiaries with arbitrage exit) are differentiated from platform operators through institutional role, but both have similar low d values reflecting beneficiary status and exit optionality. The analytical observer (d≈0.72 from the canonical table) sees the full structure: neither full beneficiary nor full target, but positioned to analyze both.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by disaggregating moderation function from enforcement asymmetry. The genuine coordination problem (managing harmful content at platform scale) is real; the extraction (asymmetric enforcement) is also real. They are not competing hypotheses about a single constraint but rather two functions implemented through the same infrastructure. The classification is Tangled Rope (not Snare) because: (1) platforms genuinely do need moderation coordination, (2) independent creators receive real value from this infrastructure, (3) the extraction is enabled by but not required by the coordination function (regulation or platform redesign could separate them). The classification is Tangled Rope (not Rope) because: (1) enforcement is systematically asymmetric against lower-power users, (2) suppression mechanisms (opacity, appeal friction) concentrate power, (3) platform operators benefit disproportionately from the same system that harms users. The mandate-tropism is the claim that this is simply 'how platforms must work' or 'necessary to manage scale' — the analytical view rejects both pure extraction (Snare) and pure coordination (Rope) framings and identifies the structural entanglement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moderation_threshold_legitimacy,
    'What proportion of platform content removals target genuinely harmful material vs. legitimate speech that contradicts platform/advertiser interests?',
    'Audit of removed content by independent evaluators blinded to moderation rationale; comparison to harm-based legal standards (libel, incitement, threats); analysis of appeal reversal rates as proxy for mistaken removal',
    'If >80% are harm-based: extraction component is incidental to coordination function (classification shifts toward Rope). If <50% are harm-based: extraction dominates (classification shifts toward Snare). Mid-range reflects the Tangled Rope boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moderation_threshold_legitimacy, empirical, 'Ratio of harmful vs. legitimate-but-suppressed content in removals').

omega_variable(
    algorithmic_opacity_suppression_mechanism,
    'Does algorithmic opacity itself function as the primary suppression mechanism, or do explicit policy enforcement and appeal barriers account for most suppression?',
    'Comparison of suppression metrics between: (a) transparent platforms with visible moderation rules, (b) platforms with algorithmic ranking but visible removals, (c) opaque algorithm platforms. Measurement of appeal success rates under different transparency conditions.',
    'If algorithmic opacity is primary: suppression is internalized (users cannot predict what will be removed or amplified), raising extractiveness. If explicit policy is primary: suppression is structural but contestable, potentially lowering effective extraction. Affects exit_options classification for creator perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_opacity_suppression_mechanism, empirical, 'Whether suppression is algorithmic-opacity-driven or policy-driven').

omega_variable(
    regulatory_sunset_realism,
    'Will transparency mandates (EU DSA, potential US regulation) actually reduce moderation asymmetry, or will regulatory compliance itself become a new layer of theater?',
    'Post-DSA implementation data on appeal reversal rates, moderation latency, and creator satisfaction in EU vs other regions; analysis of whether regulatory compliance creates paperwork theater while asymmetric enforcement continues beneath the surface',
    'If regulation is effective: Scaffold perspective is validated, actual sunset clause reduces future extraction. If regulation becomes theater: the moderation asymmetry migrates to a deeper layer (Piton perspective dominates), and extraction persists under new cover story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_sunset_realism, empirical, 'Whether regulatory transparency mandates will materially reduce moderation asymmetry').

omega_variable(
    institutional_capture_of_moderation_teams,
    'To what extent are platform moderation workers identity-locked into the platform''s extraction logic versus structurally constrained by labor market/resource limitations?',
    'Interviews with moderators post-departure; analysis of unionization attempts and exit patterns; measurement of agreement with platform enforcement rationale among active vs former moderators',
    'If identity-locked: suppression operates through workers'' internalized alignment with platform goals; exits would require identity renegotiation (raises chi for constrained exit). If structurally constrained: suppression is external resource limitation; exits are blocked by labor market barriers (lowers chi relative to identity-lock). Affects directionality computation for the moderation team perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_moderation_teams, empirical, 'Whether moderation team suppression is identity-locked or structurally constrained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(content_moderation_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmod_tr_t0, content_moderation_asymmetry, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cmod_tr_t3, content_moderation_asymmetry, theater_ratio, 3, 0.56).
narrative_ontology:measurement(cmod_tr_t6, content_moderation_asymmetry, theater_ratio, 6, 0.64).
narrative_ontology:measurement(cmod_tr_t9, content_moderation_asymmetry, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(cmod_be_t0, content_moderation_asymmetry, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cmod_be_t3, content_moderation_asymmetry, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(cmod_be_t6, content_moderation_asymmetry, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(cmod_be_t9, content_moderation_asymmetry, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(content_moderation_asymmetry, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(content_moderation_asymmetry, 0.18).
narrative_ontology:affects_constraint(content_moderation_asymmetry, algorithmic_amplification_bias).
narrative_ontology:affects_constraint(content_moderation_asymmetry, platform_labor_exploitation).
narrative_ontology:affects_constraint(content_moderation_asymmetry, content_creator_dependency).

% DUAL FORMULATION NOTE:
% Content moderation asymmetry is upstream of algorithmic amplification bias (moderation rules determine which content gets surfaced) and platform labor exploitation (moderation teams enforce the asymmetric system). Decomposed separately because each has distinct ε values and beneficiary/victim structures. Moderation asymmetry (ε=0.58) coordinates around content safety while extracting through enforcement asymmetry. Algorithmic amplification bias (higher ε) extracts through visibility concentration. Platform labor exploitation (distinct ε) extracts through worker suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(content_moderation_asymmetry, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
