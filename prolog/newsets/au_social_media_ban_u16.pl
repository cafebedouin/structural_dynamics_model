% ============================================================================
% CONSTRAINT STORY: au_social_media_ban_u16
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
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
 *   domain: social_technological/regulation
 *
 * SUMMARY:
 *   A proposed Australian federal law aims to ban social media access for
 *   children under 16, citing concerns over mental health and online harms.
 *   The law mandates that social media platforms implement robust age
 *   verification systems, placing the onus of enforcement on the companies
 *   under threat of significant fines. This creates a structural conflict
 *   between the state's claimed protective duty, the civil liberties of young
 *   people, the privacy implications of age verification, and the operational
 *   burdens on global technology platforms.
 *
 * KEY AGENTS:
 *   - Australian Youth (Under 16): Primary target (powerless/trapped) — bears the cost of lost access, autonomy, and privacy.
 *   - Australian Government: Primary beneficiary (institutional/arbitrage) — achieves a political goal and enacts a protective measure it frames as a public good.
 *   - Social Media Platforms: Enforcer/Secondary Victim (powerful/constrained) — forced to implement and bear the cost of a complex, controversial regulatory regime.
 *   - Civil Liberties Advocates: Organized opposition (organized/mobile) — view the law as an extractive infringement on fundamental rights.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(au_social_media_ban_u16, 0.55).
domain_priors:suppression_score(au_social_media_ban_u16, 0.75).
domain_priors:theater_ratio(au_social_media_ban_u16, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(au_social_media_ban_u16, extractiveness, 0.55).
narrative_ontology:constraint_metric(au_social_media_ban_u16, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(au_social_media_ban_u16, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(au_social_media_ban_u16, tangled_rope).
narrative_ontology:human_readable(au_social_media_ban_u16, "Australian Under-16 Social Media Ban").
narrative_ontology:topic_domain(au_social_media_ban_u16, "social_technological/regulation").

domain_priors:requires_active_enforcement(au_social_media_ban_u16).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(au_social_media_ban_u16, australian_government).
narrative_ontology:constraint_beneficiary(au_social_media_ban_u16, concerned_parents).
narrative_ontology:constraint_victim(au_social_media_ban_u16, australian_youth_u16).
narrative_ontology:constraint_victim(au_social_media_ban_u16, social_media_platforms).
narrative_ontology:constraint_victim(au_social_media_ban_u16, civil_liberties_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BANNED YOUTH (SNARE) — As the direct target with no political power or legal standing, the youth are trapped. The law extracts their autonomy, access to information, and social communities, while potentially compromising their privacy through verification systems. Circumvention (e.g., VPNs) is possible but carries risk and is actively combated. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(au_social_media_ban_u16, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GOVERNMENT (SCAFFOLD) — The government frames the law as a temporary protective measure (a scaffold) to shield a vulnerable demographic until they reach an age of greater maturity (16). From this view, it's a coordination effort for public good with a clear (age-based) sunset for any given individual. As the primary beneficiary with arbitrage exit (they can amend the law), they perceive negative extraction. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07.
constraint_indexing:constraint_classification(au_social_media_ban_u16, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORMS (TANGLED ROPE) — Global platforms are legally constrained to comply, facing massive fines. They experience the law as a hybrid. It's a coordination rule (rope) they must implement, but it also imposes significant compliance costs, user friction, and potential liability (snare), representing an extraction of resources. d≈0.60, f(d)≈0.85, σ=1.2 → χ≈0.56.
constraint_indexing:constraint_classification(au_social_media_ban_u16, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: CIVIL LIBERTIES ADVOCATE (SNARE) — Organized groups see the law as a pure extraction of rights (freedom of expression, access to information) and privacy, setting a dangerous precedent for state control over the internet. They view the child protection rationale as a pretext for a coercive mechanism. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.41. While χ is below the snare threshold, their classification is based on the high suppression (0.75) and the principle of rights extraction.
constraint_indexing:constraint_classification(au_social_media_ban_u16, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The system's default view. It recognizes the dual function: a genuine, state-led coordination attempt to mitigate perceived social harm (the rope function), combined with a highly coercive, rights-extracting mechanism imposed on a powerless demographic (the snare function). The high suppression and asymmetric cost distribution confirm the tangled rope classification. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(au_social_media_ban_u16, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(au_social_media_ban_u16_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(au_social_media_ban_u16, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(au_social_media_ban_u16, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(au_social_media_ban_u16, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(au_social_media_ban_u16_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55): High. The primary extraction is not financial but is levied against the autonomy, social capital, and informational access of an entire youth demographic. The costs of privacy intrusion via mandatory age verification are also significant. Suppression (0.75): High. The constraint is backed by the full force of federal law, with severe penalties for non-compliant platforms. While circumvention is possible, it is not a sanctioned or easy alternative, making the suppression of access the default state. Theater Ratio (0.40): Moderate. The law has a clear function (blocking accounts), but a significant performative aspect exists. It allows the government to be seen 'acting decisively' on a complex issue (youth mental health) where this ban is a blunt instrument. The known imperfections of age verification tech and the ease of circumvention for determined teens mean the law's actual effectiveness may be lower than its political signaling value.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The government perceives a protective Scaffold, a temporary measure to help children. The targeted youth experience a Snare, an arbitrary removal of their social world and rights. Platforms, caught in the middle, see a Tangled Rope—a costly, coercive mandate (snare) that also functions as a new set of rules for the road (rope). Civil liberties groups see only the Snare, viewing the protective claims as a pretext for control. The classification depends entirely on whether one is imposing the rule, subject to it, or forced to enforce it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Government, Concerned Parents) have a low directionality (d), leading to a perception of low or negative extraction (Rope/Scaffold). Victims (Youth, Platforms, Advocates) have a high directionality, leading to high effective extraction (χ) and a perception of a Snare or Tangled Rope. The youth, being powerless and trapped, experience the highest d-value and thus the most severe form of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a classic example of how the framework resolves mandatrophy. A naive analysis might label the law as simply 'protective' (Rope) or 'authoritarian' (Snare). Deferential Realism shows that both are valid perspectival truths. The analytical classification of Tangled Rope correctly captures the structure: a system with a genuine (though debatable) coordination goal (protecting children) that is implemented through highly coercive, extractive means. It avoids the error of accepting the beneficiary's framing at face value while also acknowledging the coordination component that a pure Snare classification would miss.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    net_welfare_impact,
    'Does the ban produce a net improvement in youth mental health, or does the harm from social isolation and driving activity to less-safe platforms outweigh the benefits?',
    'Longitudinal studies comparing mental health metrics of Australian youth cohorts pre- and post-ban against control groups in other countries.',
    'If net positive, strengthens the Scaffold/Rope perspectives. If net negative or neutral, strengthens the Snare/Piton perspectives, revealing the protective function as theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_welfare_impact, empirical, 'Net impact on youth mental health and well-being').

omega_variable(
    verification_privacy_tradeoff,
    'Can age verification be implemented effectively at scale without creating unacceptable privacy risks or new centralized databases of sensitive user data?',
    'Technical audits of deployed age verification systems, analysis of data breach incidents, and evaluation of privacy-preserving cryptographic methods.',
    'If privacy can be preserved, the extractive component is lower. If it cannot, the constraint functions as a data-harvesting Snare, regardless of its stated intent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_privacy_tradeoff, empirical, 'Feasibility of effective and private age verification').

omega_variable(
    censorship_precedent,
    'Will this law serve as a precedent for broader state control over internet access, extending to other age groups or different categories of ''harmful'' content?',
    'Tracking subsequent legislative proposals in Australia and other jurisdictions that cite this law as a model.',
    'If it leads to broader controls, its classification as a Snare becomes dominant. If it remains narrowly focused, the Scaffold/Tangled Rope views remain more plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(censorship_precedent, conceptual, 'Whether the ban sets a precedent for broader censorship').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(au_social_media_ban_u16, 2024, 2034).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(au_s_tr_t2024, au_social_media_ban_u16, theater_ratio, 2024, 0.2).
narrative_ontology:measurement(au_s_tr_t2029, au_social_media_ban_u16, theater_ratio, 2029, 0.35).
narrative_ontology:measurement(au_s_tr_t2034, au_social_media_ban_u16, theater_ratio, 2034, 0.4).

% Extraction over time
narrative_ontology:measurement(au_s_be_t2024, au_social_media_ban_u16, base_extractiveness, 2024, 0.4).
narrative_ontology:measurement(au_s_be_t2029, au_social_media_ban_u16, base_extractiveness, 2029, 0.5).
narrative_ontology:measurement(au_s_be_t2034, au_social_media_ban_u16, base_extractiveness, 2034, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(au_social_media_ban_u16, enforcement_mechanism).
narrative_ontology:affects_constraint(au_social_media_ban_u16, digital_identity_frameworks).
narrative_ontology:affects_constraint(au_social_media_ban_u16, online_content_moderation_policies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
