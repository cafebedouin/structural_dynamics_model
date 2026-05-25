% ============================================================================
% CONSTRAINT STORY: purity_policing_as_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_purity_policing_as_extraction, []).

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
 *   constraint_id: purity_policing_as_extraction
 *   human_readable: Purity Policing as Extraction in Platform Communities
 *   domain: platform_governance/content_moderation/community_norms
 *
 * SUMMARY:
 *   Purity policing in platform communities operates through a structural
 *   inversion: moral identity performance is achieved not through personal
 *   content curation (blocking, muting, filtering one's own feed) but through
 *   extracting the labor and presence of targeted creators via coordinated
 *   reporting campaigns. The constraint exhibits genuine coordination
 *   function (community boundary maintenance, norm enforcement) alongside
 *   asymmetric extraction (targeted creators bear disproportionate costs
 *   while harassment coordinators capture moral status and community
 *   influence). This is a canonical tangled_rope: the coordination and
 *   extraction are inseparable — the same reporting infrastructure that
 *   enables legitimate moderation also enables extractive purity campaigns.
 *   The constraint is downstream of two structural features: the locus of
 *   harm prevention (mountain — platforms structurally cannot prevent all
 *   harm, creating legitimate need for user-driven moderation) and harassment
 *   affordance architecture (rope — reporting tools are coordination
 *   infrastructure that can be weaponized). The extraction mechanism has
 *   intensified over the 6-year interval as coordinated harassment tactics
 *   have professionalized and platform engagement metrics have increasingly
 *   rewarded controversy.
 *
 * KEY AGENTS:
 *   - Targeted Creators: Primary victim (powerless/identity_locked) — professional identity fused with platform presence; exit requires abandoning audience, income, and career trajectory built over years
 *   - Harassment Coordinators: Primary beneficiary (organized/mobile) — capture moral status and community influence through successful campaigns; can exit to alternative platforms or identities with minimal cost
 *   - Community Moderators: Mixed position (moderate/constrained) — benefit from reporting infrastructure for legitimate moderation but also bear labor cost of processing bad-faith reports; constrained by platform tools and policies
 *   - Platform Safety Coalition: Organized agents (organized/mobile) — advocacy groups, researcher networks, and policy coalitions building transparency tools, appeal processes, and distributed governance models with sunset logic
 *   - Platform Corporation: Institutional beneficiary (institutional/arbitrage) — benefits from user-generated moderation labor and engagement metrics during controversy; can exit to alternative governance models with minimal cost
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination need and extractive weaponization; risks over-indexing on either
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(purity_policing_as_extraction, 0.58).
domain_priors:suppression_score(purity_policing_as_extraction, 0.68).
domain_priors:theater_ratio(purity_policing_as_extraction, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(purity_policing_as_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(purity_policing_as_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(purity_policing_as_extraction, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(purity_policing_as_extraction, tangled_rope).
narrative_ontology:human_readable(purity_policing_as_extraction, "Purity Policing as Extraction in Platform Communities").
narrative_ontology:topic_domain(purity_policing_as_extraction, "platform_governance/content_moderation/community_norms").

domain_priors:requires_active_enforcement(purity_policing_as_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(purity_policing_as_extraction, harassment_coordinators).
narrative_ontology:constraint_beneficiary(purity_policing_as_extraction, platform_engagement_metrics).
narrative_ontology:constraint_victim(purity_policing_as_extraction, targeted_creators).
narrative_ontology:constraint_victim(purity_policing_as_extraction, community_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(purity_policing_as_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

constraint_indexing:constraint_classification(purity_policing_as_extraction, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

constraint_indexing:constraint_classification(purity_policing_as_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(purity_policing_as_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

constraint_indexing:constraint_classification(purity_policing_as_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

constraint_indexing:constraint_classification(purity_policing_as_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(purity_policing_as_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(purity_policing_as_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(purity_policing_as_extraction, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(purity_policing_as_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(purity_policing_as_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Targeted creators bear significant costs (time defending against reports, emotional labor, audience loss, income disruption, potential platform removal) while harassment coordinators capture moral status and community influence at low personal cost. The extraction is not maximal because some reporting campaigns do identify genuine policy violations, and some creators successfully defend or migrate. The value reflects the asymmetry: coordinators externalize costs onto targets while internalizing benefits. Suppression (0.68): High. Barriers to exit include economic dependency (platform-specific audience and income), identity fusion (professional identity constituted through platform presence), reputational lock-in (leaving appears to validate accusations), and network effects (audience cannot easily migrate). Suppression is not total because some creators do successfully exit to alternative platforms or careers. Theater ratio (0.45): Moderate. Reporting and moderation processes have genuine functional content (some reports identify real violations, some moderation decisions are substantively justified) but also significant performative elements (reports filed for moral signaling rather than policy enforcement, moderation decisions driven by controversy management rather than policy application). The theater has increased as coordinated campaigns have become more sophisticated at gaming reporting systems.
 *
 * PERSPECTIVAL GAP:
 *   The targeted creator experiences a snare: high extraction, high suppression, identity-locked exit. Their professional identity is fused with platform presence — leaving requires not just losing income but abandoning the career identity built over years. The harassment coordinator experiences rope: they are solving a coordination problem (enforcing community norms) and can exit costlessly if the campaign fails or the platform becomes inhospitable. The community moderator experiences tangled_rope: they benefit from reporting infrastructure for legitimate moderation but bear the labor cost of processing bad-faith reports and face constraints from platform tools. The platform safety coalition sees scaffold: current extraction mechanisms are temporary, and transparency tools plus distributed governance will sunset the weaponization while preserving legitimate moderation. The platform corporation sees rope: user-generated moderation is cost-effective coordination infrastructure, and engagement metrics during controversy are a side benefit. The analytical observer sees tangled_rope: genuine coordination need (platforms cannot prevent all harm, users must participate in moderation) inseparably entangled with extractive weaponization (purity policing as moral status competition).
 *
 * DIRECTIONALITY LOGIC:
 *   Harassment coordinators are beneficiaries with mobile exit options — they capture moral status and community influence through successful campaigns and can costlessly exit to alternative platforms or identities if the current platform becomes inhospitable. This produces low directionality (d ≈ 0.20), yielding low or negative effective extraction (they experience the constraint as coordination). Targeted creators are victims with identity_locked exit — their professional identity is constituted through platform presence, making exit psychologically unthinkable even when structurally possible. This produces high directionality (d ≈ 0.89), yielding high effective extraction. Community moderators are in a mixed position: they appear in both beneficiaries (they use reporting infrastructure for legitimate moderation) and victims (they bear labor cost of bad-faith reports), with constrained exit (they can leave moderation roles but face reputational and community costs). This produces moderate directionality (d ≈ 0.55). Platform corporations are beneficiaries with arbitrage exit — they benefit from user-generated moderation labor and can costlessly shift to alternative governance models. The analytical observer uses the canonical analytical directionality (d ≈ 0.72).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the coordination function (community norm enforcement, boundary maintenance) and the extraction mechanism (purity policing as moral status competition) are structurally inseparable. The same reporting infrastructure that enables legitimate moderation also enables extractive campaigns. The tangled_rope classification captures this: it is neither pure coordination (rope) nor pure extraction (snare) but an irreducible hybrid. The perspectival gap is diagnostic: beneficiaries see rope (coordination), victims see snare (extraction), and the analytical observer sees tangled_rope (both). The scaffold perspective (platform safety coalition) represents a structural hypothesis: that improved governance tools can separate the coordination function from the extraction mechanism, creating a sunset for the weaponization while preserving legitimate moderation. Whether this hypothesis is correct is an empirical question captured in omega variable sunset_mechanism_viability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_threshold,
    'At what frequency does norm enforcement transition from legitimate boundary maintenance to extractive purity policing?',
    'Longitudinal analysis of reporting campaign outcomes: ratio of content violations found vs creator departures; correlation between campaign intensity and actual policy violations',
    'If threshold is low (e.g., 3+ coordinated reports = extraction): many legitimate moderation actions misclassified. If threshold is high (e.g., 20+ reports required): extractive campaigns persist unchallenged until severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_threshold, empirical, 'Frequency threshold distinguishing norm enforcement from extraction').

omega_variable(
    identity_lock_mechanism,
    'Is the targeted creator''s inability to exit structural (platform dependency for income/audience) or cognitive (professional identity fused with platform presence)?',
    'Post-departure trajectory analysis: do creators who leave maintain income/audience elsewhere, or does departure correlate with career exit? Survey data on identity fusion with platform.',
    'If structural: suppression is material (economic dependency). If cognitive: suppression is internalized (identity lock). Mixed cases require decomposition into separate constraint stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether creator exit barriers are structural or identity-based').

omega_variable(
    platform_complicity_degree,
    'Does platform architecture passively enable harassment or actively incentivize it through engagement metrics?',
    'A/B testing of algorithmic changes: does reducing visibility of coordinated reporting reduce harassment frequency? Analysis of engagement metrics during harassment campaigns.',
    'If passive: platform is rope (neutral infrastructure). If active: platform is tangled_rope or snare (extraction beneficiary). Determines whether platform_engagement_metrics should remain in beneficiaries list.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_complicity_degree, empirical, 'Degree of platform architectural complicity in extraction').

omega_variable(
    sunset_mechanism_viability,
    'Can improved moderation tools and community governance structures actually sunset the extraction mechanism, or do they merely shift its form?',
    'Longitudinal tracking of platforms that implemented transparency tools, appeal processes, and distributed moderation: did harassment campaigns decline or adapt? Comparison of harassment rates across governance models.',
    'If viable: scaffold perspective is structural (real sunset). If adaptation occurs: scaffold perspective is aspirational (extraction persists in new forms).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_mechanism_viability, empirical, 'Whether governance improvements can sunset extraction or only displace it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(purity_policing_as_extraction, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(purity_pol_tr_t0, purity_policing_as_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(purity_pol_tr_t3, purity_policing_as_extraction, theater_ratio, 3, 0.4).
narrative_ontology:measurement(purity_pol_tr_t6, purity_policing_as_extraction, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(purity_pol_be_t0, purity_policing_as_extraction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(purity_pol_be_t3, purity_policing_as_extraction, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(purity_pol_be_t6, purity_policing_as_extraction, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(purity_policing_as_extraction, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of locus_of_harm_prevention (mountain — platforms structurally cannot prevent all harm) and harassment_affordance_architecture (rope — reporting tools as coordination infrastructure). The upstream constraints establish the structural context; this constraint models the specific extraction mechanism that emerges when coordination infrastructure is weaponized for moral status competition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(purity_policing_as_extraction, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
