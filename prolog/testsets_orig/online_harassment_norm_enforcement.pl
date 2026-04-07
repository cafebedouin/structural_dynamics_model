% ============================================================================
% CONSTRAINT STORY: online_harassment_norm_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_online_harassment_norm_enforcement, []).

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
 *   constraint_id: online_harassment_norm_enforcement
 *   human_readable: Online Harassment Norm Enforcement Systems
 *   domain: social/digital_governance
 *
 * SUMMARY:
 *   Online harassment norm enforcement creates a structural tension between
 *   legitimate community protection and the concentration of enforcement
 *   power in institutional actors (platforms) who use norm governance to
 *   simultaneously extract value (user data, advertiser confidence,
 *   regulatory legitimacy) and coordinate safety. The constraint exhibits
 *   Tangled Rope structure: genuine coordination of harassment prevention
 *   coexists with asymmetric extraction that benefits platforms and
 *   high-status users at the cost of targeted harassment recipients,
 *   marginalized communities, and volunteer moderators. The system's
 *   theater_ratio (0.64) reflects that moderation policy and enforcement are
 *   substantially performative — designed to signal safety to regulators and
 *   advertisers rather than to maximally reduce harm. Moderation policies
 *   emphasize 'civility' and 'respectful discourse' in ways that
 *   disproportionately suppress marginalized voices (e.g., anti-racist speech
 *   flagged as 'hate speech,' feminist speech flagged as 'toxicity'). The
 *   extractiveness trajectory (0.42→0.62 over 9 years) shows accumulation:
 *   platforms layer additional enforcement tools and policies nominally for
 *   safety but functionally increasing surveillance and control.
 *
 * KEY AGENTS:
 *   - Targeted Harassment Recipients: Primary victim (powerless/trapped) — no meaningful exit, high suppression, dependent on platform enforcement that operates with high false-negative rates
 *   - Marginalized Communities: Primary victim (organized/constrained) — derive some benefit from norm coordination but bear disproportionate costs from enforcement asymmetries that suppress their speech under civility frames
 *   - Community Moderators: Secondary victim (moderate/constrained) — unpaid labor with burnout costs but also gain reputation and skill development; quasi-coerced by platform design that outsources governance
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — benefit from liability reduction, advertiser confidence, regulatory compliance; can arbitrage enforcement stringency across jurisdictions
 *   - High-Status Public Figures: Secondary beneficiary (powerful/mobile) — receive disproportionate enforcement protection; can mobilize enforcement against critics; mobile exit if enforcement changes
 *   - Content Moderation Industry: Institutional actor (institutional/arbitrage) — maintains enforcement infrastructure through career dependence and regulatory compliance; theater-heavy (policies signal safety without necessarily delivering it)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent socio-technical arrangement as immutable law of digital communication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(online_harassment_norm_enforcement, 0.58).
domain_priors:suppression_score(online_harassment_norm_enforcement, 0.68).
domain_priors:theater_ratio(online_harassment_norm_enforcement, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(online_harassment_norm_enforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(online_harassment_norm_enforcement, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(online_harassment_norm_enforcement, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(online_harassment_norm_enforcement, tangled_rope).
narrative_ontology:human_readable(online_harassment_norm_enforcement, "Online Harassment Norm Enforcement Systems").
narrative_ontology:topic_domain(online_harassment_norm_enforcement, "social/digital_governance").

domain_priors:requires_active_enforcement(online_harassment_norm_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(online_harassment_norm_enforcement, platform_operators).
narrative_ontology:constraint_beneficiary(online_harassment_norm_enforcement, high_status_users).
narrative_ontology:constraint_beneficiary(online_harassment_norm_enforcement, enforcement_infrastructure_providers).
narrative_ontology:constraint_victim(online_harassment_norm_enforcement, targeted_harassment_recipients).
narrative_ontology:constraint_victim(online_harassment_norm_enforcement, marginalized_communities).
narrative_ontology:constraint_victim(online_harassment_norm_enforcement, low_status_speakers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED HARASSMENT RECIPIENT (SNARE) — Trapped between continued exposure to abuse and removing themselves from the platform entirely. The norm enforcement system is designed to protect but operates with massive false-negative rates (reported abuse often unaddressed) and creates a perverse incentive: visible retaliation and escalation sometimes receives faster response than initial harassment. Maximum experienced extraction — no meaningful exit, high suppression.
constraint_indexing:constraint_classification(online_harassment_norm_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMUNITY MODERATOR (TANGLED ROPE) — Constrained by unpaid labor expectations, burnout from exposure to abuse content, and lack of institutional authority. But also derives benefit: volunteer moderators gain reputation, skill development in conflict resolution, and social status within communities. Significant extraction embedded in genuine coordination of community safety — some agency but substantial costs.
constraint_indexing:constraint_classification(online_harassment_norm_enforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Benefits from norm enforcement as coordination mechanism: reduces liability exposure, improves advertiser confidence, sustains user engagement by maintaining minimum safety standards. Enforcement is a pure coordination function from this perspective — genuine public good that serves the platform's interests. Exit via arbitrage: can adjust enforcement stringency based on regulatory jurisdiction.
constraint_indexing:constraint_classification(online_harassment_norm_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-STATUS PUBLIC FIGURE (TANGLED ROPE) — Experiences enforcement as both protective and extractive. Benefits from rapid response to coordinated abuse campaigns (coordination function). But also derives extraction: enforcement asymmetry — their speech receives protection that ordinary users do not. Can mobilize enforcement disproportionately against critics, framing disagreement as harassment. Mobile exit option (can move to alternative platforms or create private spaces), but platform switching costs are lower for high-status figures than for ordinary users.
constraint_indexing:constraint_classification(online_harassment_norm_enforcement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CONTENT MODERATION INDUSTRY (PITON) — The infrastructure of moderation (automated systems, outsourced review centers, policy frameworks) persists through institutional momentum despite visible dysfunction. Many moderation policies are theater: rule categories that sound protective but are inconsistently applied, appeal processes that rarely overturn decisions, transparency reports that obscure rather than clarify enforcement patterns. Theater ratio reflects that much moderation activity is performative — designed to signal safety to regulators and advertisers rather than to actually reduce harm. The system maintains itself through career dependence and regulatory compliance rather than effectiveness.
constraint_indexing:constraint_classification(online_harassment_norm_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MARGINALIZED COMMUNITY ORGANIZING (TANGLED ROPE) — Organized actors (feminist groups, anti-racist organizing, LGBTQ+ networks) perceive enforcement as both beneficial (rapid response to coordinated pile-ons) and extractive (enforcement policies often disproportionately target marginalized voices under guise of 'civility'). Constrained by resource barriers and political vulnerability. Derives some benefit from norm coordination but bears disproportionate costs from enforcement asymmetries.
constraint_indexing:constraint_classification(online_harassment_norm_enforcement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, online disinhibition and norm violation are inherent features of digital communication at scale. The constraint appears as an immutable law: anonymous or pseudonymous communication + reduced social friction + network amplification effects = harassment at scale. This perspective naturalizes what the structural data reveals as a contingent socio-technical arrangement. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(online_harassment_norm_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(online_harassment_norm_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(online_harassment_norm_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(online_harassment_norm_enforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(online_harassment_norm_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(online_harassment_norm_enforcement, TR),
    TR >= 0.70.

:- end_tests(online_harassment_norm_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Platforms extract significant value through norm enforcement — user data collection justified as 'safety analysis,' advertiser premium for 'safe environments,' regulatory compliance reducing litigation risk. But the extraction is not maximal because genuine harassment does decrease (some coordination function is real) and some users (high-status figures, organized communities) derive real protective benefit. The trajectory from 0.42→0.62 reflects layering: initial enforcement was more coordination-focused; later tools added surveillance and control functions that increased extraction. Suppression (0.68): High. Significant barriers to exit: platforms have network effects (where users go, others follow); switching costs are high; no competitor offers equivalent functionality with lower suppression. Suppression is both structural (economic barriers) and internalized (users accept platform authority as legitimate norm-setter). Theater ratio (0.64): High. Moderation policy is substantially performative: appeal processes have low reversal rates (theater of 'fairness'), transparency reports obscure enforcement patterns (theater of 'accountability'), and policy changes are announced as 'safety improvements' while functionally increasing control. The theater has increased from 0.48→0.70 as platforms added compliance infrastructure responding to regulatory pressure — the expansion of moderation tools increased performative overhead without proportionally increasing actual harm reduction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. Platforms see pure coordination (Rope) — they are solving the legitimate problem of harassment reduction and maintaining safe spaces. Community moderators see mixed coordination and extraction (Tangled Rope) — they benefit from reputation but bear labor costs. Targeted harassment recipients see pure extraction (Snare) — the system fails to protect them while forcing them to bear psychological costs. High-status users see partial extraction with strong coordination benefits (Tangled Rope) — they benefit from enforcement protection while being able to weaponize enforcement against critics. The content moderation industry sees a degraded, theater-heavy system (Piton) — enforcement persists through regulatory compliance and career dependence rather than effectiveness. Marginalized communities see enforcement that is simultaneously protective and extractive (Tangled Rope) — harassment prevention is real but enforcement of civility norms suppresses their speech. The analytical observer risks seeing an immutable feature of digital communication (Mountain) — harassment at scale is an inherent property of online disinhibition — but the structural data reveals this as false naturalization: enforcement asymmetries, theater-heavy systems, and suppression mechanisms are contingent institutional choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural positions: Platforms (institutional/arbitrage) experience low d (~0.05-0.15) — they are net beneficiaries with exit options. Targeted harassment recipients (powerless/trapped) experience high d (~0.92) — full victims with no exit. Community moderators (moderate/constrained) experience moderate d (~0.58) — mixed victims and beneficiaries constrained by platform design. High-status users (powerful/mobile) experience moderate d (~0.45) — nominal victims of harassment but actual beneficiaries of enforcement asymmetry, with mobile exits. The sigmoid f(d) maps these to experienced extractiveness: high d yields high chi; low d yields negative chi (institutional actors perceive the constraint as pure benefit). The enforcement asymmetry creates perpendicular gradient in d values: for identical harassment, the target's d depends on their status — high-status targets get lower d (faster, more aggressive enforcement) than marginalized targets (higher d, slower response).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint genuinely exhibits Tangled Rope structure — there is authentic coordination (harassment reduction is real and benefits everyone) AND asymmetric extraction (enforcement asymmetries benefit platforms and high-status users, suppress marginalized speech). This is not a misclassification problem; it is the definitional case for Tangled Rope. The false summit (Mountain perspective) is exposed: online harassment is not a law of nature but a contingent outcome of platform design choices, enforcement policies, and labor structures. The constraint could be redesigned toward higher coordination (decentralized moderation, democratic policy-setting, paid moderator labor) and lower extraction (symmetric enforcement, algorithmic transparency, user control). The theater component (0.64) reflects that current moderation does some work but relies heavily on performative signals to satisfy regulators and advertisers rather than maximizing actual harm reduction. This suggests a companion constraint story: moderation-theater-as-regulatory-compliance (higher theater_ratio, lower actual harassment-reduction function) decomposed from this story's mixed coordination-extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_asymmetry_mechanism,
    'Is enforcement asymmetry (harassment of marginalized voices flagged less rapidly than harassment of powerful users) a structural feature of moderation systems or an emergent property of biased training data and human moderator demographics?',
    'Comparative analysis of enforcement response times across protected classes; audit of moderation training data and moderator demographic composition; controlled experiments with identical content posted by users of different status',
    'If structural: the constraint embeds extraction into the norm itself — ''harassment'' is defined to protect privileged speech disproportionately. If emergent from training: fixing moderator bias and data representativeness could reduce asymmetry. Classification would shift from Tangled Rope (embedded asymmetry) to Scaffold (fixable structural problem).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_mechanism, empirical, 'Causal origin of enforcement asymmetry across user types').

omega_variable(
    appeal_process_functionality,
    'Do appeal processes for moderation decisions actually reverse wrongful enforcement at rates sufficient to serve as meaningful exits for wrongly-flagged users?',
    'Analysis of appeal reversal rates by user demographic; time-to-resolution comparison between appeals and original enforcement; user satisfaction surveys post-appeal',
    'If appeal reversal rates < 5%: appeal process is theater, and exit_options should be ''trapped'' for all users. If reversal rates > 25%: appeals provide meaningful recourse, and exit_options should shift to ''constrained'' for non-marginalized users and ''identity_locked'' for marginalized users (same barriers, but internalized doubt about legitimacy of appeal)',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appeal_process_functionality, empirical, 'Efficacy of moderation appeals as meaningful recourse').

omega_variable(
    community_moderator_coercion_vs_voluntary,
    'Are volunteer community moderators genuinely voluntary contributors or functionally coerced by platform design that outsources governance work without compensation or authority?',
    'Analysis of moderator exit rates and burnout timelines; interviews on motivation (autonomy, mastery, purpose vs. obligation); comparison of moderator wellbeing against full-time moderation staff',
    'If genuine volunteers: Tangled Rope classification holds. If functionally coerced: reclassify as Snare for moderators (exit costs rise, suppression rises). The constraint may decompose into two: community-moderation-as-volunteering (lower extraction) and professional-moderation-as-labor-exploitation (higher extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_moderator_coercion_vs_voluntary, empirical, 'Voluntariness of unpaid community moderation labor').

omega_variable(
    norm_definition_circularity,
    'What establishes the definition of ''harassment'' in moderation policies — democratic deliberation, expert consensus, or platform profit optimization?',
    'Historical analysis of policy changes; comparison with peer platforms; stakeholder interviews on whose preferences shaped policy; analysis of correlation between enforcement changes and advertiser/regulatory pressure',
    'If democratic: the norm is legitimately enforced coordination. If expert: expertise claims should be transparent. If profit-driven: the ''norm'' is a disguised extraction mechanism, and classification should shift toward Snare for all non-beneficiary perspectives. This determines whether the constraint is Tangled Rope (mixed coordination and extraction) or primarily Snare with coordination cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_definition_circularity, conceptual, 'Origin and legitimacy of harassment norm definition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(online_harassment_norm_enforcement, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harass_tr_t0, online_harassment_norm_enforcement, theater_ratio, 0, 0.48).
narrative_ontology:measurement(harass_tr_t3, online_harassment_norm_enforcement, theater_ratio, 3, 0.56).
narrative_ontology:measurement(harass_tr_t6, online_harassment_norm_enforcement, theater_ratio, 6, 0.64).
narrative_ontology:measurement(harass_tr_t9, online_harassment_norm_enforcement, theater_ratio, 9, 0.7).

% Extraction over time
narrative_ontology:measurement(harass_be_t0, online_harassment_norm_enforcement, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(harass_be_t3, online_harassment_norm_enforcement, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(harass_be_t6, online_harassment_norm_enforcement, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(harass_be_t9, online_harassment_norm_enforcement, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(online_harassment_norm_enforcement, identity_coordination).
narrative_ontology:boltzmann_floor_override(online_harassment_norm_enforcement, 0.12).
narrative_ontology:affects_constraint(online_harassment_norm_enforcement, platform_algorithmic_amplification).
narrative_ontology:affects_constraint(online_harassment_norm_enforcement, content_moderation_labor_exploitation).
narrative_ontology:affects_constraint(online_harassment_norm_enforcement, social_media_engagement_maximization).

% DUAL FORMULATION NOTE:
% Online harassment norm enforcement decomposes into two structurally distinct constraints: (1) harassment-prevention-as-genuine-coordination (lower extractiveness, higher coordination function) and (2) moderation-theater-as-regulatory-compliance (higher theater ratio, performative safety signaling). This story captures the hybrid Tangled Rope form. Companion stories should address enforcement asymmetries as a separate extraction mechanism and the labor exploitation embedded in unpaid community moderation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(online_harassment_norm_enforcement, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
