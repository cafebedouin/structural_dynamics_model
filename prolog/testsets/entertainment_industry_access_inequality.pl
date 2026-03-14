% ============================================================================
% CONSTRAINT STORY: entertainment_industry_access_inequality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_entertainment_industry_access_inequality, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: entertainment_industry_access_inequality
 *   human_readable: Entertainment Industry Access Inequality
 *   domain: media/entertainment/economics
 *
 * SUMMARY:
 *   The entertainment industry's access inequality creates a structural
 *   extraction mechanism where career entry is gatekept through information
 *   asymmetry, financial barriers, and network concentration. The constraint
 *   operates across talent discovery, audition systems, agent representation,
 *   and industry casting — all nominally open but functionally requiring
 *   economic privilege, geographic proximity to hubs, or family connections.
 *   The base extractiveness (0.62) reflects substantial asymmetric benefit
 *   capture by gatekeeping institutions during the critical career-building
 *   phase, while suppression (0.68) measures the material and epistemic
 *   barriers that prevent alternatives from functioning. Theater ratio (0.58)
 *   captures the industry's performance of 'open auditions' and 'diversity
 *   initiatives' while actual hiring remains concentrated. The constraint
 *   exhibits all six classification types from different perspectives,
 *   revealing how institutional gatekeeping naturalizes itself across
 *   multiple actor positions.
 *
 * KEY AGENTS:
 *   - Economically Marginalized Aspirants: Primary victims (powerless/trapped) — cannot afford unpaid internships, relocation, headshots, or networking; absolute structural barriers
 *   - Middle-Class Aspirants with Resources: Secondary victims (moderate/constrained) — can afford some access but face significant financial risk; benefit from coordination mechanisms
 *   - Industry Gatekeeper Institutions: Primary beneficiaries (institutional/arbitrage) — agents, studios, networks benefit from information asymmetry and network control; abundant exit options
 *   - Union and Advocacy Organizations: Organized pressure (organized/mobile) — see the system as performatively maintained; advocate for barrier reduction
 *   - Streaming Platform Disruptors: Powerful alternative pathways (powerful/mobile) — YouTube, TikTok, Discord building parallel discovery with lower barriers; potential sunset mechanism
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing gatekeeping as inherent to subjective talent evaluation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(entertainment_industry_access_inequality, 0.62).
domain_priors:suppression_score(entertainment_industry_access_inequality, 0.68).
domain_priors:theater_ratio(entertainment_industry_access_inequality, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(entertainment_industry_access_inequality, extractiveness, 0.62).
narrative_ontology:constraint_metric(entertainment_industry_access_inequality, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(entertainment_industry_access_inequality, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(entertainment_industry_access_inequality, snare).
narrative_ontology:human_readable(entertainment_industry_access_inequality, "Entertainment Industry Access Inequality").
narrative_ontology:topic_domain(entertainment_industry_access_inequality, "media/entertainment/economics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(entertainment_industry_access_inequality, established_industry_gatekeepers).
narrative_ontology:constraint_beneficiary(entertainment_industry_access_inequality, legacy_talent_agents).
narrative_ontology:constraint_beneficiary(entertainment_industry_access_inequality, family_connected_aspirants).
narrative_ontology:constraint_victim(entertainment_industry_access_inequality, marginalized_aspiring_workers).
narrative_ontology:constraint_victim(entertainment_industry_access_inequality, economically_disadvantaged_creatives).
narrative_ontology:constraint_victim(entertainment_industry_access_inequality, geographic_outsiders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY MARGINALIZED ASPIRANT (SNARE) — Cannot afford unpaid internships, relocation to industry hubs, professional headshots, or networking. Structural barriers to entry are absolute; exit from the constraint requires abandoning creative ambitions entirely. Maximum extraction — the aspiring worker bears full cost of access inequality without benefit.
constraint_indexing:constraint_classification(entertainment_industry_access_inequality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-CLASS ASPIRANT WITH RESOURCES (TANGLED ROPE) — Can afford some unpaid work and relocation but faces significant financial and career risk. Benefits from industry coordination mechanisms (standardized audition formats, agent networks) that do solve real matching problems. Extraction is real but surmountable with family support or personal savings — constrained, not trapped.
constraint_indexing:constraint_classification(entertainment_industry_access_inequality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INDUSTRY GATEKEEPER INSTITUTION (ROPE) — Experiences the access system as coordination: agents match talent to roles, scouts evaluate potential, networks identify capable creatives. The gatekeeper benefits from information asymmetry but also from functioning talent discovery. Exit options are abundant — can switch platforms, models, or geographic markets. Institutional arbitrage removes extraction from their perspective.
constraint_indexing:constraint_classification(entertainment_industry_access_inequality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: UNION AND ADVOCACY ORGANIZATION (PITON) — Organized groups (SAG-AFTRA, unions, access-focused nonprofits) see the barrier system as degraded and performatively maintained. Theater ratio is high because the industry performs 'diversity initiatives' and 'open casting' while actual hiring remains nepotistic. Mobile — can exit to alternative industry models or political organizing. The constraint persists through institutional inertia despite advocacy pressure.
constraint_indexing:constraint_classification(entertainment_industry_access_inequality, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: STREAMING PLATFORM DISRUPTOR (SCAFFOLD) — Tech platforms (YouTube, TikTok, Discord communities) are building alternative pathways with lower barriers and distributed discovery. These platforms have sunset logic relative to traditional gatekeeping — as digital-native audiences grow and creators build careers through direct fan relationships, the traditional studio-agent-audition pipeline loses relevance. Powerful actors with mobile exit options see the constraint as temporary.
constraint_indexing:constraint_classification(entertainment_industry_access_inequality, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, information asymmetry in talent evaluation is structurally inherent to entertainment: subjective judgment cannot be fully mechanized, and network effects create natural concentration of discovery power. This perspective sees access inequality as an unavoidable feature of any talent identification system. However, the structural data contradicts mountain classification — the base metrics show suppression and extraction driven by institutional gatekeeping, not by immutable natural limits.
constraint_indexing:constraint_classification(entertainment_industry_access_inequality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(entertainment_industry_access_inequality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(entertainment_industry_access_inequality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(entertainment_industry_access_inequality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(entertainment_industry_access_inequality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(entertainment_industry_access_inequality, TR),
    TR >= 0.70.

:- end_tests(entertainment_industry_access_inequality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The original research framing suggested this was pure nepotism (ε > 0.75), but the constraint has genuine coordination components: agent networks do match talent to roles, audition systems do provide information flow, industry infrastructure does solve real problems of scale. However, the coordination function is thoroughly captured by gatekeeping — benefits flow disproportionately to privileged aspirants. The 0.62 value reflects that the system has real coordination work alongside asymmetric extraction. Suppression (0.68): High. Multiple reinforcing barriers: (1) Structural — unpaid internships (economic filter), relocation requirements (geographic filter), professional development costs (capital filter). (2) Epistemic — gatekeepers claim talent evaluation is subjective art, not mechanizable (naturalizes discretionary gatekeeping). (3) Institutional — careers depend on gatekeeper approval, creating risk aversion and normative compliance. (4) Internalized — aspiring workers often internalize the gatekeeper's skepticism about their own viability. Theater ratio (0.58): Moderate-high. The industry performs open-casting rituals, diversity initiatives, merit-based selection; actual hiring concentrates on connected and privileged candidates. The performance has increased over the measurement interval as pressure for visible diversity has grown, while actual access barriers have stiffened (internship unpaid-ness, relocation costs, networking requirements).
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a full perspectival spectrum. The marginalized aspirant experiences pure Snare — absolute barriers, zero net benefit, no exit. The middle-class aspirant experiences Tangled Rope — can navigate barriers with family support, benefits from coordination mechanisms. The gatekeeper institution experiences Rope — their dominant frame is coordination and talent matching. The union/advocacy sees Piton — performative diversity initiatives masking persistent nepotism. The streaming platform sees Scaffold — alternative pathways with sunset logic reducing traditional gatekeeping's relevance. The civilizational analyst risks Mountain — assuming subjective evaluation is inherently gatekeeping-resistant. The perspectival gap is not about disagreement but about structural position: the gatekeeper benefits from information asymmetry while the outsider bears its cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: Marginalized aspirants are victims with trapped exit — d ≈ 0.92, f(d) ≈ 1.38 — maximum experienced extraction. Middle-class aspirants are victims with constrained exit (can pay but costs are high) — d ≈ 0.75, f(d) ≈ 1.08 — high but surmountable extraction. Industry gatekeepers are beneficiaries with arbitrage exit (can switch platforms, models, regions) — d ≈ 0.12, f(d) ≈ -0.01 — negative effective extraction (they benefit, not bear cost). Scope modifier σ(national) = 1.0 does not scale suppression. The chi formula produces: for powerless/trapped, χ ≈ 0.62 × 1.38 × 1.0 ≈ 0.86 (snare threshold); for institutional/arbitrage, χ ≈ 0.62 × (-0.01) × 1.0 ≈ -0.006 (rope). The structural data does not require directionality overrides — the derived d values correctly reflect the actual extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the six types represent legitimate readings from different structural positions. The snare perspective (powerless/trapped) is the primary structural reality. The rope perspective (institutional/arbitrage) is the gatekeeper's genuine experience — they are solving a real coordination problem (talent matching at scale), and their benefits are not, from their position, obviously parasitic. The tangled rope perspective (moderate/constrained) identifies the hybrid function for middle-class actors who both benefit from coordination and bear extraction costs. The piton perspective (organized/mobile) identifies the theater dimension — that performative diversity initiatives have increased while actual access has not. The scaffold perspective (powerful/mobile) identifies the sunset mechanism — that platform alternatives are genuinely reducing traditional gatekeeping's relevance. The mountain perspective is a false summit — the claim that subjective talent evaluation is inherently gatekeeping-resistant naturalizes what is actually a contingent institutional choice (gatekeepers could publish rankings, crowdsource evaluation, mechanize filters, etc., but do not because opacity serves their interests). The analytical observer's classification should be snare or tangled_rope, not mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    talent_evaluation_mechanization,
    'Is talent evaluation in entertainment inherently subjective, or does this claim naturalize gatekeeping convenience?',
    'Comparison of alt-platform discovery (TikTok, YouTube) with studio-gate discovery on same metrics: time-to-earning, career sustainability, creative control, diversity of outcomes',
    'If mechanization is possible: access inequality is contingent institutional choice (Snare confirmed). If evaluation is inherently subjective: natural information asymmetry justifies some gating (Mountain partially correct).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(talent_evaluation_mechanization, empirical, 'Whether subjective talent evaluation is mechanizable or inherently gatekeeping-resistant').

omega_variable(
    family_advantage_magnitude,
    'What proportion of industry entry is explained by family connection vs. genuine merit discovery?',
    'Demographic analysis of hired workers by family connection status; comparison of career trajectories from family-connected vs. independent entry; audit studies of hiring gatekeepers',
    'If family advantage > 40%: snare classification is understated (extractiveness > 0.62). If family advantage < 15%: moderate permeability, tangled_rope from many perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_advantage_magnitude, empirical, 'Proportion of industry entry attributable to family connection').

omega_variable(
    platform_sunset_credibility,
    'Are streaming platforms and social media genuinely replacing traditional gatekeeping, or are they creating new parallel tiers of exclusion?',
    'Longitudinal tracking of TikTok/YouTube creators: what proportion reach traditional studio deals; what proportion sustain living income without studio backing; whether social-media-native careers have comparable earnings/stability to studio-path careers',
    'If platforms are truly replacing gatekeeping: scaffold sunset is real, constraint is temporalizing. If new gatekeeping forms: constraint is morphing, not sunsetting (tangled_rope from platform perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_sunset_credibility, empirical, 'Whether streaming platforms are genuinely replacing traditional gatekeeping').

omega_variable(
    suppression_mechanism_internalization,
    'How much of the measured suppression (0.68) is structural (financial barriers, relocation costs) vs. internalized (aspirants believe they lack talent or don''t belong)?',
    'Pre/post-intervention surveys; tracking outcome differences between aspirants given financial support vs. aspirants given confidence/mentorship; analysis of rejection response patterns',
    'If suppression is >50% internalized: constraint''s effective suppression persists after barrier removal; identity-lock mechanisms are operative. If <30% internalized: financial barrier removal would dramatically alter classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Ratio of structural vs. internalized suppression in access barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(entertainment_industry_access_inequality, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(entaccess_tr_t0, entertainment_industry_access_inequality, theater_ratio, 0, 0.32).
narrative_ontology:measurement(entaccess_tr_t10, entertainment_industry_access_inequality, theater_ratio, 10, 0.45).
narrative_ontology:measurement(entaccess_tr_t20, entertainment_industry_access_inequality, theater_ratio, 20, 0.58).
narrative_ontology:measurement(entaccess_tr_t5, entertainment_industry_access_inequality, theater_ratio, 5, 0.38).

% Extraction over time
narrative_ontology:measurement(entaccess_be_t0, entertainment_industry_access_inequality, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(entaccess_be_t10, entertainment_industry_access_inequality, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(entaccess_be_t20, entertainment_industry_access_inequality, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(entaccess_be_t5, entertainment_industry_access_inequality, base_extractiveness, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(entertainment_industry_access_inequality, information_standard).
narrative_ontology:affects_constraint(entertainment_industry_access_inequality, cultural_representation_bias).
narrative_ontology:affects_constraint(entertainment_industry_access_inequality, creative_labor_precarity).
narrative_ontology:affects_constraint(entertainment_industry_access_inequality, geographic_cultural_concentration).

% DUAL FORMULATION NOTE:
% Entertainment access inequality decomposes into multiple structurally distinct constraints: talent evaluation gatekeeping (this story), labor precarity in unpaid internships (downstream), cultural representation (downstream), and geographic hub concentration (adjacent). This story focuses on information asymmetry and access barriers; labor precarity and representation bias have their own ε values and should be modeled separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
