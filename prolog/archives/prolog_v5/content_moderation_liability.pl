% ============================================================================
% CONSTRAINT STORY: content_moderation_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_content_moderation_liability, []).

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
 *   constraint_id: content_moderation_liability
 *   human_readable: Content Moderation Liability Constraint
 *   domain: internet_governance/legal_policy
 *
 * SUMMARY:
 *   The content moderation liability constraint arises from the legal and
 *   regulatory tension between platform responsibility for user-generated
 *   content and the practical impossibility of comprehensive moderation.
 *   Platforms face dual pressure: regulatory mandates to remove harmful
 *   content (child safety, terrorism, election interference) and liability
 *   exposure for speech they fail to moderate. This creates asymmetric
 *   extraction: large platforms absorb compliance costs through scale
 *   economics and legal resources, while content creators, small platforms,
 *   and civil society organizations bear suppression through both legal
 *   uncertainty and de facto platform enforcement. The constraint exhibits
 *   all six DR types depending on perspective. Theater ratio (0.68) reflects
 *   that much of the moderation infrastructure is performative:
 *   notice-and-takedown procedures provide legal defensibility rather than
 *   demonstrable harm prevention; transparency reports serve regulatory
 *   ritual rather than enabling accountability; appeals processes are often
 *   opaque. Extractiveness (0.58) indicates moderate-high extraction that has
 *   grown over the interval as regulatory pressure increased (DSA, Online
 *   Safety Bill, regulatory fragmentation). The rise from 0.32 to 0.58 over
 *   six years tracks the shift from permissive early internet governance to
 *   comprehensive liability regimes. Alternative architectures (decentralized
 *   networks, federated systems, community-governed platforms) are emerging
 *   with different moderation and liability models, supporting the scaffold
 *   perspective's claim of an eventual sunset clause.
 *
 * KEY AGENTS:
 *   - Individual Content Creators: Primary victims (powerless/trapped) — face legal liability and platform suppression with no meaningful due process or appeal mechanisms
 *   - Civil Society Organizations (NGOs, advocacy groups): Primary victims (powerless/trapped) — face dual extraction through platform liability and distribution control; cannot build independent infrastructure at scale
 *   - Small/Independent Platforms: Secondary victims (moderate/constrained) — bear compliance costs disproportionate to resources; limited legal resources to contest liability
 *   - Large Platforms (Meta, Google, X, TikTok): Primary beneficiaries (institutional/arbitrage) — scale economics in compliance, legal resources, and market dominance enable them to use liability framework as competitive moat
 *   - Regulatory Bodies (governments, legislators): Institutional actors (institutional/arbitrage) — benefit from liability framework through enforcement legitimacy and policy coordination; perform regulatory theater
 *   - Decentralized/Alternative Infrastructure Coalition: Organized actors (organized/mobile) — building federated and community-governed alternatives with different moderation models; have genuine exit pathways
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent legal choices as inevitable features of internet governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(content_moderation_liability, 0.58).
domain_priors:suppression_score(content_moderation_liability, 0.62).
domain_priors:theater_ratio(content_moderation_liability, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(content_moderation_liability, extractiveness, 0.58).
narrative_ontology:constraint_metric(content_moderation_liability, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(content_moderation_liability, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(content_moderation_liability, tangled_rope).
narrative_ontology:human_readable(content_moderation_liability, "Content Moderation Liability Constraint").
narrative_ontology:topic_domain(content_moderation_liability, "internet_governance/legal_policy").

domain_priors:requires_active_enforcement(content_moderation_liability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(content_moderation_liability, large_platforms).
narrative_ontology:constraint_beneficiary(content_moderation_liability, legal_compliance_infrastructure).
narrative_ontology:constraint_victim(content_moderation_liability, content_creators).
narrative_ontology:constraint_victim(content_moderation_liability, civil_society_organizations).
narrative_ontology:constraint_victim(content_moderation_liability, small_platforms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTENT CREATOR (SNARE) — Trapped by liability exposure and platform Terms of Service. Cannot exit the constraint without abandoning online speech platforms entirely. Faces suppression through both legal liability and platform enforcement mechanisms. No meaningful appeal process or due process. Maximum extraction from perspective of individual creator.
constraint_indexing:constraint_classification(content_moderation_liability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL SOCIETY (SNARE) — NGOs and advocacy groups face dual extraction: liability for platform hosting their content AND extraction via platform control over content distribution. Trapped by legal exposure in jurisdictions where they operate. Cannot build independent infrastructure at scale without massive resources. Suppression mechanisms prevent effective coordination or contestation.
constraint_indexing:constraint_classification(content_moderation_liability, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SMALL PLATFORM (TANGLED ROPE) — Faces significant liability exposure and compliance costs, but also benefits from coordination around moderation standards and legal frameworks. Constrained by resource limitations and legal uncertainty. Some exit available (geographic arbitrage, niche focus) but costly. Mixed experience of coordination (learning from industry standards) and extraction (disproportionate liability relative to resources).
constraint_indexing:constraint_classification(content_moderation_liability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: LARGE PLATFORM (ROPE) — Net beneficiary of liability framework through scale economics and legal resources. Experiences moderation as coordination mechanism: standardized policies, community guidelines, and legal compliance enable growth and market position. Regulatory arbitrage available through legal teams and compliance infrastructure. Extraction flows toward this agent.
constraint_indexing:constraint_classification(content_moderation_liability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COMPLIANCE THEATER (PITON) — Section 230 (US) and similar frameworks create performative compliance rituals: notice-and-takedown procedures, content moderation reports, transparency statements, regulatory submissions. Theater has risen over time as regulations multiply (DSA, Online Safety Bill, regulatory patchwork). The actual functional verification that content removal serves legitimate purposes is minimal. Regulatory inertia maintains the constraint despite degraded function.
constraint_indexing:constraint_classification(content_moderation_liability, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: DECENTRALIZED INFRASTRUCTURE COALITION (SCAFFOLD) — Organized actors (ActivityPub networks, blockchain platforms, community-owned servers, open-protocol projects) are building alternative content distribution systems with different liability models. Sees moderation liability as a temporary coordination problem being solved by architectural redesign. Low effective extraction because this coalition has genuine exit pathways and momentum. Sunset clause: as federated and decentralized systems mature, traditional platform liability becomes less relevant. Estimated timeline: 10-15 years for meaningful adoption.
constraint_indexing:constraint_classification(content_moderation_liability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — From a civilizational view, this perspective risks naturalizing the liability framework as an inevitable feature of internet governance: 'online platforms must moderate content; platforms must be liable for user speech; this tension is inherent to digital communication.' However, this naturalizes contingent legal and regulatory choices (US Section 230, EU DSA structure, platform liability regimes). The structural data contradicts the mountain classification — architectural alternatives exist and are being deployed. This is a false summit.
constraint_indexing:constraint_classification(content_moderation_liability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(content_moderation_liability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(content_moderation_liability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(content_moderation_liability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(content_moderation_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(content_moderation_liability, TR),
    TR >= 0.70.

:- end_tests(content_moderation_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value for large platforms through competitive advantage (smaller competitors bear higher relative compliance costs) and through the capture of content governance authority. However, extractiveness is not at snare levels (≥0.66) because genuine coordination benefits exist: platforms do moderate genuinely harmful content (child exploitation, terrorism, extreme violence), and this serves a real public function. The constraint is hybrid — it coordinates around harm prevention while simultaneously enabling extraction through scale economics. Suppression (0.62): Moderate-high. Significant barriers to contestation include: legal liability uncertainty (creators cannot know ex-ante which content is actionable), platform Terms of Service as unilateral constraints (no negotiation), algorithmic opacity (decisions are often unexplainable), and appeal processes that are opaque or non-existent. Suppression is not total because some jurisdiction-specific escape routes exist (jurisdictional arbitrage, regulatory safe harbors in some regions). Theater ratio (0.68): High. Notice-and-takedown procedures provide legal defensibility rather than demonstrable harm prevention. Transparency reports are ritual compliance documents. Appeals processes are largely performative. The rise from 0.35 to 0.68 over the interval reflects multiplication of regulatory requirements (GDPR, DSA, Online Safety Bill, platform-specific policies) without proportional increase in functional capacity. Theater increased faster than legitimate moderation function.
 *
 * PERSPECTIVAL GAP:
 *   The piton perspective identifies that moderation compliance infrastructure has become substantially performative. The gap between regulatory requirement complexity and actual harm prevention effectiveness has widened. Notice-and-takedown procedures, transparency reports, and appeals processes provide legal defensibility for platforms but do not demonstrably prevent harmful content at scale. The snare perspective (content creator) captures the asymmetry: creators face legal liability and suppression mechanisms with no due process or meaningful contestation. The rope perspective (large platform) captures that the same liability framework functions as a coordination mechanism for market control. The scaffold perspective captures that architectural alternatives (ActivityPub, Bluesky federation, blockchain platforms) are being deployed to bypass the entire liability constraint. The mountain perspective (false summit) risks naturalizing what is actually a policy choice: different jurisdictions have different liability regimes, and the empirical outcomes vary. This reveals that the 'inevitability' frame is ideological, not structural.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality from beneficiary/victim declarations and exit options. Beneficiaries (large_platforms, legal_compliance_infrastructure) with arbitrage exit receive low d values (~0.15-0.20), producing negative or minimal chi. Victims (content_creators) with trapped exit receive high d values (~0.90), producing amplified chi through f(d). This asymmetry is the core extraction mechanism: the same moderation regime protects large platforms and extracts from small actors. No directionality override is necessary — the structural data (beneficiary/victim + exit options) accurately produces the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by acknowledging that the liability framework serves BOTH coordination and extraction functions: (1) it genuinely coordinates around preventing harmful content (child exploitation, terrorism); (2) it simultaneously enables large platforms to extract competitive advantage through compliance cost scale economies. The tangled_rope classification is correct because both functions are real and non-decomposable. The perspectival gap shows that beneficiaries (large platforms) perceive coordination while victims (small platforms, creators) perceive extraction. The piton perspective shows that the coordination function is degrading (theater ratio rising faster than effectiveness). The scaffold perspective shows that architectural alternatives are emerging that could separate coordination function from extraction mechanism — the constraint is not inevitable but policy-contingent. The false mountain perspective reveals that naturalizing the liability framework ('this is how internet moderation must work') is ideology, not necessity. Regulatory regimes in different jurisdictions prove that alternatives are structurally possible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liability_vs_free_speech,
    'Is content moderation liability a necessary protection mechanism against harmful content or an extraction mechanism that suppresses legitimate speech?',
    'Longitudinal analysis of false positive rates (legitimate content removed), civil society impact, diversity of speech pre/post regulation, and jurisdictional comparison of speech outcomes under different liability regimes',
    'If primarily protective: constraint may reclassify toward Rope (coordination benefit dominates). If primarily extractive: classification as Snare/Tangled Rope sustained. Impact determines whether civil society classification as victim is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_vs_free_speech, empirical, 'Whether liability functions as speech protection or suppression mechanism').

omega_variable(
    scale_economics_inevitability,
    'Does the extractiveness of the constraint derive from inherent scalability limitations of decentralized platforms or from regulatory/legal choices that favor centralized architectures?',
    'Comparative analysis of compliance costs (absolute and per-user) across platform scales; technical capacity analysis of decentralized moderation; regulatory barrier analysis for alternative architectures',
    'If extractiveness is architecturally inevitable: mountain classification has merit. If extractiveness is policy-contingent: scaffold and rope classifications are more accurate, and policy redesign could reduce extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scale_economics_inevitability, empirical, 'Whether constraint extractiveness is technical or policy-determined').

omega_variable(
    notice_takedown_effectiveness,
    'Does the notice-and-takedown procedure (performative compliance theater) actually prevent harmful content proliferation or primarily serve evidentiary purposes in liability defense?',
    'Comparison of content removal timing relative to harm (child safety, election interference, violence incitement); analysis of appeals granted vs denied; tracking of re-upload rates post-removal; regulator enforcement records',
    'If genuinely protective: theater ratio should be lower (~0.40-0.50). If primarily evidentiary: piton classification confirmed with theater ratio ~0.70. Changes assessment of whether compliance infrastructure serves victims or benefits platforms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(notice_takedown_effectiveness, empirical, 'Whether notice-and-takedown serves harm prevention or liability defense').

omega_variable(
    jurisdictional_arbitrage,
    'Can platforms meaningfully practice jurisdictional arbitrage in moderation policy (apply different standards by region) or does global content flow force uniform high-suppression policy?',
    'Audit of platform moderation policies across jurisdictions; analysis of whether region-specific moderation actually functions; tracking of policy divergence and convergence over time',
    'If meaningful arbitrage exists: institutional actors have more exit options than analysis assumes, reducing d and chi. If global policy convergence dominates: extraction is more concentrated, supporting higher extractiveness estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_arbitrage, empirical, 'Extent of platform jurisdictional arbitrage in moderation').

omega_variable(
    decentralization_viability,
    'Can ActivityPub, blockchain platforms, and federated systems actually achieve the content moderation outcomes that make them viable alternatives (i.e., reduce harmful content without central extraction)?',
    'Empirical deployment analysis of existing decentralized platforms; measurement of harmful content rates, user experience, and sustainability; comparison of moderation burden vs centralized platforms',
    'If decentralized moderation is viable: scaffold sunset is real and timeline accelerates. If decentralized systems struggle with moderation at scale: scaffold classification is aspirational, timeline extends, and large platform position strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_viability, empirical, 'Viability of decentralized content moderation at scale').

omega_variable(
    suppression_mechanism_attribution,
    'Is measured suppression (0.62) primarily driven by legal/regulatory liability exposure, platform Terms of Service enforcement, or the technical impossibility of contesting algorithmic decisions?',
    'Decomposition analysis: measure suppression under different legal regimes (jurisdictions with high platform liability vs safe harbor); measure suppression for human-reviewed vs algorithmic takedowns; measure effectiveness of appeals processes',
    'If legal liability dominates: policy change reduces suppression. If ToS enforcement dominates: structural redesign required. If technical impossibility dominates: architectural change (transparency, appeals) required. Affects which interventions could reduce the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_attribution, empirical, 'Primary driver of suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(content_moderation_liability, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cml_tr_t0, content_moderation_liability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cml_tr_t3, content_moderation_liability, theater_ratio, 3, 0.52).
narrative_ontology:measurement(cml_tr_t6, content_moderation_liability, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(cml_be_t0, content_moderation_liability, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cml_be_t3, content_moderation_liability, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(cml_be_t6, content_moderation_liability, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(content_moderation_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(content_moderation_liability, platform_algorithmic_transparency).
narrative_ontology:affects_constraint(content_moderation_liability, digital_services_act_compliance).
narrative_ontology:affects_constraint(content_moderation_liability, section_230_safe_harbor).

% DUAL FORMULATION NOTE:
% Content moderation liability is the parent constraint affecting three downstream constraints: algorithmic transparency (more transparency required as liability exposure increases), DSA compliance (EU instantiation of the liability regime), and Section 230 safe harbor erosion (US legal contestation of the liability framework). The upstream constraint derives from the general tension between platform responsibility and content moderation impossibility at scale; downstream constraints instantiate this tension in specific jurisdictional and technical contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(content_moderation_liability, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
