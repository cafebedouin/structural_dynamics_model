% ============================================================================
% CONSTRAINT STORY: au_social_media_ban_u16
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_au_social_media_ban_u16, []).

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
 *   constraint_id: au_social_media_ban_u16
 *   human_readable: Australian Under-16 Social Media Ban
 *   domain: social_technological/regulatory
 *
 * SUMMARY:
 *   The Australian Under-16 Social Media Ban represents a conflict between
 *   child protection (a legitimate coordination problem) and digital autonomy
 *   (a structural right). The constraint exhibits the core mandatrophy: is
 *   the state using age-gating enforcement to solve a real developmental harm
 *   problem, or is it using child-protection rhetoric to justify
 *   technological control over youth access to peer coordination networks?
 *   The extractiveness value (0.52) reflects genuine mixed function — the ban
 *   does reduce exposure to algorithmic addiction and some documented harms,
 *   but it also suppresses peer network access, information discovery, and
 *   youth voice in digital spaces. The suppression value (0.65) reflects high
 *   coercive content: the ban is mandatory, affects a legally defined
 *   population without individual consent capacity, and criminalized at both
 *   user and platform level. The theater ratio (0.58) indicates that
 *   compliance signaling (age-gating infrastructure) partially substitutes
 *   for effective alternative provision (media literacy, parental guidance,
 *   youth-appropriate platform design). The ban creates an institutional
 *   bifurcation: global platforms must implement Australian-specific
 *   enforcement, while domestic educational alternatives remain underfunded.
 *
 * KEY AGENTS:
 *   - Under-16 Users: Primary victims (powerless/trapped) — bear full exclusion cost with no exit options; criminalized participation via fake accounts or VPNs
 *   - Child Development Advocates: Primary beneficiary (institutional/arbitrage) — frame the ban as protective; have flexibility to modify or sunset the law
 *   - Social Media Platforms: Secondary institutional actor (institutional/constrained) — face compliance costs and liability exposure, but also gain competitive moat from fixed-cost age-verification infrastructure
 *   - Youth Digital Rights Community: Secondary victim (moderate/constrained) — constrained by suppression of their advocacy platforms; benefits from stated protection intent
 *   - Age-Appropriate Platform Providers: Organized beneficiary (organized/mobile) — see the ban as creating market opportunity for alternatives; have exit options through platform development
 *   - Media Literacy Education System: Degraded alternative mechanism (institutional/arbitrage) — functionally displaced by the ban; persists through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — assesses whether biology justifies the policy choice or whether regulatory arbitrage will degrade enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(au_social_media_ban_u16, 0.52).
domain_priors:suppression_score(au_social_media_ban_u16, 0.65).
domain_priors:theater_ratio(au_social_media_ban_u16, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(au_social_media_ban_u16, extractiveness, 0.52).
narrative_ontology:constraint_metric(au_social_media_ban_u16, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(au_social_media_ban_u16, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(au_social_media_ban_u16, tangled_rope).
narrative_ontology:human_readable(au_social_media_ban_u16, "Australian Under-16 Social Media Ban").
narrative_ontology:topic_domain(au_social_media_ban_u16, "social_technological/regulatory").

domain_priors:requires_active_enforcement(au_social_media_ban_u16).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(au_social_media_ban_u16, child_development_advocates).
narrative_ontology:constraint_beneficiary(au_social_media_ban_u16, platform_liability_managers).
narrative_ontology:constraint_beneficiary(au_social_media_ban_u16, government_youth_protection_agencies).
narrative_ontology:constraint_victim(au_social_media_ban_u16, underage_users).
narrative_ontology:constraint_victim(au_social_media_ban_u16, open_internet_access_principle).
narrative_ontology:constraint_victim(au_social_media_ban_u16, platform_innovation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE UNDER-16 USER (SNARE) — Cannot exit the ban through legitimate means. Age-gating is technically enforced; fake accounts violate terms of service; international VPNs are unreliable and carry legal/safety risks. The user bears full cost of exclusion from social coordination, peer networks, and information access. No alternatives mandated by the state. Maximum suppression — participation is criminalized at both user and platform level.
constraint_indexing:constraint_classification(au_social_media_ban_u16, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: YOUTH DIGITAL RIGHTS COMMUNITY (TANGLED ROPE) — Constrained by legal barriers to advocacy and social coordination, but benefits from the constraint's stated protective intent. Advocates for youth autonomy face suppression of their organizational platforms (the very social media being banned). Can organize through traditional media and legal channels but faces resource barriers. Mixed extraction and coordination function.
constraint_indexing:constraint_classification(au_social_media_ban_u16, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GOVERNMENT YOUTH PROTECTION AGENCY (ROPE) — Primary beneficiary. The ban solves a coordination problem: how to reduce minors' exposure to algorithmic harms without requiring individual parental judgment at scale. Agency experiences the constraint as enabling its mandate, not extracting from it. Has arbitrage options (can modify enforcement, can sunset the law). Net beneficiary of the constraint structure.
constraint_indexing:constraint_classification(au_social_media_ban_u16, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: AGE-APPROPRIATE DIGITAL ALTERNATIVE PLATFORMS (SCAFFOLD) — Organized actors (youth-focused platforms, parental-control services, curated networks) see the ban as temporary coordination with a sunset: if age-appropriate alternatives mature and gain user adoption, the crude binary ban (allowed/not allowed) becomes unnecessary. The constraint creates market opportunity for alternatives. Organized agents have exit options (develop compliant platforms, lobby for refined thresholds). Has sunset logic if alternatives reach critical mass.
constraint_indexing:constraint_classification(au_social_media_ban_u16, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SOCIAL MEDIA PLATFORMS (TANGLED ROPE) — Constrained by compliance costs and liability exposure, but also benefits from regulation-driven competitive moat (age verification is a fixed cost that smaller competitors cannot absorb). Global platforms face conflicting mandates: Australian age-verification law, EU GDPR/DMA requirements, US free-speech litigation, and Chinese surveillance frameworks. Extraction runs in multiple directions simultaneously. Primary costs: age-verification infrastructure, reduced advertising revenue from under-16 segment, liability for enforcement failure. Primary benefits: reduced content moderation burden for youth harm, legal safe harbor, competitive advantage.
constraint_indexing:constraint_classification(au_social_media_ban_u16, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MEDIA LITERACY EDUCATION SYSTEM (PITON) — The stated rationale for the ban partly displaces responsibility from education institutions to legal enforcement. Media literacy programs, digital citizenship curricula, and parental guidance frameworks are the functional alternative to a blanket ban, but these approaches require sustained investment and institutional commitment. The ban creates theater around youth protection while educational infrastructure atrophies. Theater ratio reflects that compliance signaling (platform age-gating) substitutes for actual skill building. Degraded alternative mechanism persists through inertia.
constraint_indexing:constraint_classification(au_social_media_ban_u16, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: DEVELOPMENTAL NEUROSCIENCE VIEW (MOUNTAIN) — From a neuroscientific perspective, adolescent vulnerability to algorithmic addiction and social comparison has an irreducible biological basis: prefrontal cortex development (delayed until mid-20s), heightened dopamine sensitivity, and social-brain reorganization during puberty are not contingent on policy or technology design. A blanket ban naturalizes these biological constraints as universal law. However, the structural data reveal contingency: different jurisdictions achieve different developmental outcomes with different regulatory approaches (EU age minimums at 13-16 with consent; South Korea with education-based approaches; US with negligible restrictions). The mountain classification is a false summit — biology is necessary but not sufficient for the policy choice.
constraint_indexing:constraint_classification(au_social_media_ban_u16, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(au_social_media_ban_u16_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(au_social_media_ban_u16, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(au_social_media_ban_u16, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(au_social_media_ban_u16, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(au_social_media_ban_u16, TR),
    TR >= 0.70.

:- end_tests(au_social_media_ban_u16_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The ban extracts from under-16 users (full exclusion from mainstream social coordination), but the extraction is not maximal because the constraint has a legitimate protective function (documented algorithmic addiction harms, social comparison effects). The value reflects that extraction is coupled with stated benefit provision (child protection). Suppression (0.65): High. The ban is mandatory, age-defined, criminalized at user and platform level, and has no built-in appeal mechanism for individual circumstances. Users cannot negotiate exceptions; platforms cannot negotiate selective compliance. However, suppression is not absolute because workarounds (international VPNs, fake accounts) exist and are technically accessible to motivated users. Theater ratio (0.58): Moderate-high. The primary mechanism (age-gating infrastructure) is performance-oriented: platforms implement compliance systems to demonstrate lawfulness, but actual effectiveness depends on technical sophistication of verification and user motivation to evade. Media literacy and youth-appropriate design (alternative functional mechanisms) remain underfunded. Theater is rising (t=0 to t=5) as compliance becomes more formalized and actual protective function depends more on sustained investment in education/alternatives.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The powerless user sees snare (pure extraction). The institutional beneficiary sees rope (pure coordination). The organized alternative provider sees scaffold (temporary with sunset). The platforms see tangled rope (mixed). The degraded educational system sees itself as piton (inertial). The false mountain (developmental neuroscience view) naturalizes the policy when alternatives exist. The gap reflects fundamental misalignment: the constraint solves a real problem (algorithmic harm) but creates new problems (peer exclusion, privacy surveillance, regulatory arbitrage). No single perspective captures the full structure. Convergence would require either (a) a genuinely age-appropriate alternative that sunset-able, or (b) abandonment of the protection rationale in favor of explicit autonomy control.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from agent power, exit options, and beneficiary/victim status. Under-16 users: powerless + trapped → d ≈ 0.95 → f(d) ≈ 1.42 → high experienced extraction. Government agency: institutional + arbitrage → d ≈ 0.00 → f(d) ≈ -0.12 → negative experienced extraction (benefits). Platforms: institutional + constrained → d ≈ 0.40 → f(d) ≈ 0.40 → moderate extraction (compliance costs outweigh liability reduction). Youth advocates: moderate + constrained → d ≈ 0.65 → f(d) ≈ 1.00 → moderate extraction (suppression of advocacy infrastructure). Alternative platforms: organized + mobile → d ≈ 0.20 → f(d) ≈ 0.10 → low extraction (have exit options and market opportunity). Media literacy system: institutional + arbitrage → d ≈ 0.35 → f(d) ≈ 0.30 → low extraction (inertia, not active suppression, but functional displacement). The scope modifier σ(S) = 1.0 (national, not global) because Australian enforcement is legally sovereign but globally networked platforms constrain effectiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY. This constraint faces the core mandatrophy: framing a binary ban (allowed/not allowed) as a 'protection measure' when the actual mechanism is suppression of autonomy. The analytical resolution requires comparing three scenarios: (1) If the ban genuinely reduces documented developmental harm (algorithmic addiction, social comparison effects) AND media literacy/age-appropriate alternatives are funded at comparable scale: tangled_rope is the correct stable classification — extraction is justified as a side effect of necessary coordination. (2) If the ban reduces some harms but causes greater harms (social isolation, peer exclusion, regulatory arbitrage creating privacy infrastructure): the constraint should reclassify to snare — the protection rationale is false, and the actual function is autonomy control. (3) If enforcement degrades into regulatory arbitrage (widespread VPN adoption, platform compliance theater without actual enforcement): the constraint degrades to piton (maintained by institutional inertia despite functional failure). The current evidence suggests scenario 1 is partially true (documented algorithm harms are real) but scenario 2 is also partially true (social isolation harm data is emerging). Mandatrophy resolution requires 5+ years of developmental outcome data from Australian cohorts compared to other jurisdictions with different regulatory approaches.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    age_verification_technical_sufficiency,
    'Can age-verification systems reliably distinguish U16 from 16+ users without creating surveillance infrastructure or enabling identity fraud?',
    'Technical audits of proposed age-verification protocols (government ID scanning, facial recognition, device-based attestation); comparison with existing systems (payment processors, age-gated content platforms); measurement of false-positive/false-negative rates',
    'If verification is reliable and non-invasive: constraint is enforceable and primarily a coordination/snare hybrid. If verification requires surveillance or enables fraud: constraint becomes a snare for users (privacy cost) and platforms (liability cost), with theater masking failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(age_verification_technical_sufficiency, empirical, 'Technical reliability of age-verification mechanisms').

omega_variable(
    comparative_developmental_harm,
    'Does the developmental harm prevented by the ban (reduced algorithmic addiction exposure) exceed the developmental harm caused by social isolation and peer-network exclusion for U16 users?',
    'Longitudinal studies comparing cohorts in jurisdictions with/without similar bans; measurement of mental health outcomes, peer integration, and academic performance over 5+ years; analysis of alternative harm pathways (offline social pressures, offline substance use)',
    'If prevented harm > caused harm: constraint is justified as protective (rope/scaffold perspective is structural). If caused harm ≥ prevented harm: constraint is net extraction masked as protection (snare/piton perspective dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_developmental_harm, empirical, 'Comparative developmental outcomes with and without the ban').

omega_variable(
    regulatory_arbitrage_stability,
    'Will the Australian ban persist as a stable national policy, or will it degrade into regulatory arbitrage (users migrate to international VPNs, platforms create compliance theater while actual enforcement fails)?',
    'Measurement of VPN adoption rates among Australian minors post-implementation; comparison of platforms'' compliance costs to enforcement penalties; survey of platform internal enforcement budgets vs actual user age distribution in Australia',
    'If stable enforcement: constraint is tangled_rope (coordination + extraction). If regulatory arbitrage succeeds: constraint degrades into piton (theater maintained by institutional inertia despite functional failure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_stability, empirical, 'Stability of Australian regulatory enforcement against arbitrage').

omega_variable(
    alternative_platform_adoption_threshold,
    'At what user-adoption rate for age-appropriate alternatives does the crude binary ban become replaceable with refined threshold-based access?',
    'Analysis of existing age-appropriate platform adoption curves (Snapchat pre-2015, TikTok in regulated markets); measurement of network-effect lock-in; comparison with educational/parental-control platform uptake rates',
    'If alternatives reach 30%+ of U16 population: scaffold sunset becomes realistic within 5-10 years. If adoption stalls: scaffold classification is aspirational, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_adoption_threshold, empirical, 'Critical mass threshold for age-appropriate alternative platform adoption').

omega_variable(
    mandatrophy_resolution,
    'Is this constraint a genuine coordination mechanism (protecting minors from algorithmic harm) that requires extraction (suppressing access) as a side effect, or pure extraction (controlling youth digital autonomy) falsely framed as protection?',
    'Comparative analysis: (1) effectiveness of the ban in reducing documented algorithmic harms vs other regulatory approaches (UK Age Appropriate Design Code, EU Digital Services Act transparency requirements); (2) alignment of enforcement with stated protective goals vs misalignment indicating capture by parental-control industry; (3) measurement of actual youth harm reduction vs theater (compliance signaling without behavioral change)',
    'If genuine coordination: tangled_rope classification is stable. If pure extraction: constraint should reclassify to snare, and the state''s coordination rationale is false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_resolution, empirical, 'Whether protection rationale is genuine coordination or false legitimation of extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(au_social_media_ban_u16, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ausm_tr_t0, au_social_media_ban_u16, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ausm_tr_t2, au_social_media_ban_u16, theater_ratio, 2, 0.5).
narrative_ontology:measurement(ausm_tr_t5, au_social_media_ban_u16, theater_ratio, 5, 0.58).

% Extraction over time
narrative_ontology:measurement(ausm_be_t0, au_social_media_ban_u16, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ausm_be_t2, au_social_media_ban_u16, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(ausm_be_t5, au_social_media_ban_u16, base_extractiveness, 5, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(au_social_media_ban_u16, enforcement_mechanism).
narrative_ontology:affects_constraint(au_social_media_ban_u16, algorithmic_addiction_capture_u16).
narrative_ontology:affects_constraint(au_social_media_ban_u16, youth_privacy_surveillance_infrastructure).
narrative_ontology:affects_constraint(au_social_media_ban_u16, peer_network_access_constraint_adolescent).

% DUAL FORMULATION NOTE:
% The ban decomposes into three linked structural constraints: (1) algorithmic_addiction_capture_u16 (ε=0.35, rope) — the protective goal, legitimate coordination; (2) youth_privacy_surveillance_infrastructure (ε=0.62, snare) — the age-verification enforcement mechanism, creates new harms; (3) peer_network_access_constraint_adolescent (ε=0.58, tangled_rope) — the collateral cost of exclusion from coordination networks. This story models the ban itself (enforcement mechanism layer); the linked constraints model the layered extraction beneath.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(au_social_media_ban_u16, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
