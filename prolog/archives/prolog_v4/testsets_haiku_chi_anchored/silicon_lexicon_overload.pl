% ============================================================================
% CONSTRAINT STORY: silicon_lexicon_overload
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_silicon_lexicon_overload, []).

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
 *   constraint_id: silicon_lexicon_overload
 *   human_readable: The Silicon Lexicon (Corporate Tech-Speak)
 *   domain: linguistic/technological/social
 *
 * SUMMARY:
 *   The Silicon Lexicon represents the crystallization of industry-specific
 *   jargon ('bandwidth,' 'alignment,' 'synergy,' 'pivot,' 'disrupt,'
 *   'leverage') into mandatory corporate communication protocols that extend
 *   far beyond their original technical domains. What began as efficient
 *   terminology for distributed systems, software engineering, and technical
 *   architecture has metastasized into a social gatekeeper in hiring,
 *   promotion, and team dynamics across non-technical domains (strategy, HR,
 *   operations, sales). The constraint exhibits simultaneous coordination
 *   function (genuine technical precision in narrow domains) and extraction
 *   function (social gatekeeping, exclusion of non-native speakers, reduction
 *   of linguistic clarity). The theater ratio (0.68) reflects that most
 *   jargon usage in corporate contexts is performative: employees deploy
 *   'bandwidth' not for technical precision but for status signaling and
 *   in-group membership. The extractiveness trajectory (0.18→0.38 over the
 *   interval) shows accelerating exclusion as jargon usage has cascaded from
 *   engineering teams to executive strategy, forcing broader populations into
 *   mandatory fluency in an arbitrary symbolic system.
 *
 * KEY AGENTS:
 *   - Tech Industry Insiders: Primary beneficiary (institutional/arbitrage) — secure status through jargon fluency, recruit peers with compatible communication styles, maintain insider/outsider boundary
 *   - Non-Native Speakers: Primary victim (powerless/trapped) — mandatory fluency requirement in hiring/promotion; no alternative communication pathways accepted in corporate environments
 *   - Technical Professionals: Secondary beneficiary/victim (moderate/constrained) — benefit from jargon's coordination function in technical work but also pay extraction cost when jargon extends to non-technical contexts
 *   - Linguistic Clarity: Collective victim (powerless/trapped) — cannot exit corporate communications; clarity and precision decline as technical jargon is deployed for obfuscation
 *   - Cross-Domain Communication: Collective victim (powerless/trapped) — business, academia, government, non-profits increasingly forced to adopt tech-industry jargon even when domain-inappropriate
 *   - Plain Language Movement: Organized challengers (organized/constrained) — advocates for clear writing (government plain language initiatives, accessibility organizations) constrained by corporate hiring power
 *   - Corporate HR Systems: Institutional enforcer (institutional/arbitrage) — maintains jargon gatekeeping through job descriptions, performance reviews, cultural fit assessments; sees jargon as degraded ritual (Piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(silicon_lexicon_overload, 0.38).
domain_priors:suppression_score(silicon_lexicon_overload, 0.62).
domain_priors:theater_ratio(silicon_lexicon_overload, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(silicon_lexicon_overload, extractiveness, 0.38).
narrative_ontology:constraint_metric(silicon_lexicon_overload, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(silicon_lexicon_overload, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(silicon_lexicon_overload, tangled_rope).
narrative_ontology:human_readable(silicon_lexicon_overload, "The Silicon Lexicon (Corporate Tech-Speak)").
narrative_ontology:topic_domain(silicon_lexicon_overload, "linguistic/technological/social").

domain_priors:requires_active_enforcement(silicon_lexicon_overload).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(silicon_lexicon_overload, tech_industry_insiders).
narrative_ontology:constraint_beneficiary(silicon_lexicon_overload, corporate_management_class).
narrative_ontology:constraint_victim(silicon_lexicon_overload, non_native_speakers).
narrative_ontology:constraint_victim(silicon_lexicon_overload, linguistic_clarity).
narrative_ontology:constraint_victim(silicon_lexicon_overload, cross_domain_communication).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED NON-NATIVE SPEAKER (SNARE) — Cannot exit corporate environments without abandoning professional advancement; faces mandatory fluency in an arbitrary symbolic system with no pedagogical foundation. High suppression (0.62): mandatory jargon in job postings, performance reviews, team meetings. No alternatives accepted in hiring/promotion. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(silicon_lexicon_overload, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TECHNICAL PROFESSIONAL (TANGLED ROPE) — Experiences dual function: jargon does provide efficient coordination within tech communities (rope element) but also functions as a barrier to non-specialists (extraction element). Constrained exit: must use silicon lexicon to signal expertise and secure promotions, but doing so limits communication with broader audiences. d≈0.58, f(d)≈0.77, σ=1.0 → χ≈0.29.
constraint_indexing:constraint_classification(silicon_lexicon_overload, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECH INDUSTRY INSIDERS (ROPE) — Beneficiaries of jargon-based coordination and status signaling. The lexicon solves genuine coordination problems within technical domains (distributed systems require precise terminology). Arbitrage exit: can shift domains or create new terminology. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.04. Net beneficiary through status enforcement and in-group signaling.
constraint_indexing:constraint_classification(silicon_lexicon_overload, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLAIN LANGUAGE MOVEMENT (TANGLED ROPE) — Organized agents (writing groups, accessibility advocates, education reformers) see the lexicon as a coordination problem with a potential sunset. The constraint has coordination function (efficient intra-domain communication) but extracts from broader society through exclusion. Constrained exit: must work within corporate systems while advocating for alternatives. d≈0.44, f(d)≈0.47, σ=1.2 → χ≈0.25. Coalition has agency to challenge but limited enforcement power to change corporate hiring.
constraint_indexing:constraint_classification(silicon_lexicon_overload, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CORPORATE HR SYSTEMS (PITON) — Maintains jargon requirements through performative compliance checking: job descriptions mandate 'synergy,' 'alignment,' 'bandwidth' usage; performance reviews assess 'cultural fit' (codified in lexicon patterns). Theater ratio (0.68): Most jargon terms perform sophisticated meaning but lack definitional rigor. 'Bandwidth' = capacity but evokes technical precision unwarranted for generic resource planning. HR systems persist in using jargon not because it optimizes hiring but through institutional inertia and copy-paste templating.
constraint_indexing:constraint_classification(silicon_lexicon_overload, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN-ASPIRATION) — Risks framing jargon as an immutable property of modern professional communication: 'all large organizations require shared terminology to function at scale.' However, structural data (ε=0.38, suppression=0.62, theater=0.68) contradicts mountain classification. The jargon's persistence is contingent on corporate hiring gatekeeping and media amplification, not on inherent structural necessity. This represents a false summit where institutional power naturalizes arbitrary linguistic choices.
constraint_indexing:constraint_classification(silicon_lexicon_overload, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silicon_lexicon_overload_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(silicon_lexicon_overload, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silicon_lexicon_overload, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(silicon_lexicon_overload, TR),
    TR >= 0.70.

:- end_tests(silicon_lexicon_overload_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high, increasing. The constraint does extract value through exclusion and status gatekeeping, but this is not as severe as pure predatory extraction (0.70+). The measure reflects that some jargon does solve real coordination problems in technical contexts (~20-30% of typical usage), but the majority (~70-80%) serves social gatekeeping. The trajectory (0.18→0.38) shows acceleration as jargon cascades from engineering to executive strategy to non-technical domains where it provides no coordination value. Suppression (0.62): High. Barriers to exit include: mandatory jargon in job postings and hiring criteria; performance reviews assessing 'cultural fit' (coded in jargon patterns); team communication in jargon-dense companies imposing adoption costs on newcomers; media and investor discourse amplifying jargon legitimacy. However, suppression is not total (0.80+) because some organizations explicitly reject jargon (design-focused firms, government agencies, non-profits) and some individuals can navigate corporate environments through code-switching or selective adoption. Theater ratio (0.68): High and increasing. Most jargon deployment in non-technical contexts is performative: using 'bandwidth' to discuss meeting time, 'synergy' to describe project collaboration, 'alignment' to mean agreement. These terms sound sophisticated but obscure rather than clarify meaning. The ratio increased from 0.42 to 0.68 as jargon spread from engineering (lower theater) to executive/cultural contexts (high theater).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries (tech insiders, management) experience the jargon as efficient intra-group coordination (Rope). Non-native speakers experience it as pure gatekeeping (Snare). Technical professionals see mixed function (Tangled Rope): jargon enables precision in their domain but becomes a barrier when they must communicate with non-technical stakeholders. The plain language movement sees a temporary problem with potential solutions (Scaffold): plain-language policies in government and some design-focused firms demonstrate that alternatives exist, and the constraint could have a sunset as broader awareness of jargon's costs grows. Corporate HR systems see their jargon enforcement as a degraded ritual (Piton): they persist in copy-pasting jargon-laden job descriptions not because it optimizes hiring but through template inertia and risk aversion (all competitors use it, so copying legitimacy is safe). The analytical observer risks naturalizing jargon as an inevitable feature of professional communication (false Mountain), but the structural data reveals this is contingent institutional choice, not inherent organizational necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Tech industry insiders: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Clear net beneficiary. They created the jargon and maintain gatekeeping through hiring/promotion. Non-native speakers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. They must adopt jargon to access corporate employment and have no alternative professional pathways in tech-dominated industries. Technical professionals: Victim + constrained (mixed beneficiary) → d≈0.58, f(d)≈0.77. They benefit from jargon's coordination function in their technical work but bear extraction cost when forced to use it in non-technical contexts. Plain language movement: Organized + constrained → d≈0.44, f(d)≈0.47. Coalition has agency and advocacy power but limited enforcement power to change corporate hiring practices. Linguistic clarity: Victim + trapped (abstract) → d≈0.95, f(d)≈1.42. The collective good (clear communication across society) has maximum extraction imposed on it with no voice or exit mechanism. Corporate HR: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. They benefit from jargon's status-signaling function and maintain gatekeeping, but the engine's Piton classification identifies this as a degraded benefit (inertial rather than functional).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE vs TYRANNY RESOLVED: The constraint's Tangled Rope classification avoids both false Mountain (naturalizing jargon as necessary) and false pure Snare (denying its coordination function). The mandatrophy is resolved by acknowledging that jargon does provide coordination benefits in narrow technical domains (genuine mandate for distributed systems terminology) but has been extracted from that domain and deployed as social gatekeeping (tyranny). The solution space is not to eliminate technical jargon entirely (would harm necessary precision in engineering) but to contain it: establish domain boundaries within which jargon is required and permitted, coupled with plain-language requirements for external/cross-functional communication. This is the Scaffold path: sunset the mandatory universal adoption of jargon while maintaining its domain-appropriate use. The increasing theater ratio (0.42→0.68) and extractiveness (0.18→0.38) show the constraint is drifting toward pure Snare as jargon spreads beyond its coordination-functional domains. Without intervention, the eventual classification will be Snare (high extraction, low coordination, high suppression). The plain language movement's perspective provides the sunset mechanism: government plain-language policies, accessibility-driven writing standards, and design-focused culture shifts are building alternative pathways that reduce reliance on arbitrary jargon for professional credibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_threshold,
    'What proportion of silicon lexicon terms solve genuine technical coordination problems versus signaling social dominance?',
    'Linguistic analysis comparing jargon density in domains with high technical precision requirements (systems engineering, cryptography) versus generic domains (strategy, culture). Measurement of task completion rates when jargon is replaced with plain language in controlled environments.',
    'If genuine coordination > 60%: classification shifts toward Rope. If social signaling > 60%: classification confirms Snare/extraction. If mixed (40-60%): Tangled Rope is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_necessity_threshold, empirical, 'Proportion of jargon that solves coordination versus signaling').

omega_variable(
    entry_barrier_causality,
    'Does corporate adoption of silicon lexicon directly cause exclusion of non-native speakers, or does it merely correlate with other hiring gatekeeping mechanisms?',
    'Comparative analysis: corporate cultures with strict jargon enforcement versus those with plain-language policies. Tracking hiring outcomes, retention rates, and promotion rates for non-native speakers in each group, controlling for technical background.',
    'If jargon is causal: suppression rating (0.62) is justified, extraction mechanism is direct. If correlation only: suppression may be overestimated, extraction operates through other mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entry_barrier_causality, empirical, 'Whether jargon directly causes or correlates with exclusion').

omega_variable(
    plain_language_performance_parity,
    'Can organizations maintain internal coordination efficiency and external communication quality while replacing silicon lexicon with plain language alternatives?',
    'Longitudinal case studies of organizations implementing plain-language policies (government agencies, some design-focused tech companies). Measurement of project delivery metrics, documentation clarity, cross-functional collaboration effectiveness, hiring diversity.',
    'If parity achieved: Scaffold perspective is validated (sunset is real). If performance degrades: Rope perspective dominates (genuine coordination value). If mixed (domain-dependent): Tangled Rope is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plain_language_performance_parity, empirical, 'Whether plain language can maintain coordination efficiency').

omega_variable(
    generational_lexicon_drift,
    'Is silicon lexicon self-limiting through semantic degradation, or does it continuously regenerate new jargon to maintain exclusivity?',
    'Longitudinal linguistic corpus analysis (tech industry publications, job postings, conference talks) across 10-year intervals. Tracking vocabulary entropy: do new terms replace old ones at a rate suggesting generational reset, or does accumulated jargon create cumulative exclusion?',
    'If self-limiting: constraint may degrade naturally (Piton outcome). If continuously regenerating: extraction mechanism is structural and persistent (Snare outcome).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_lexicon_drift, empirical, 'Whether jargon regenerates to maintain exclusivity or becomes self-limiting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(silicon_lexicon_overload, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lex_tr_t0, silicon_lexicon_overload, theater_ratio, 0, 0.42).
narrative_ontology:measurement(lex_tr_t5, silicon_lexicon_overload, theater_ratio, 5, 0.55).
narrative_ontology:measurement(lex_tr_t10, silicon_lexicon_overload, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(lex_be_t0, silicon_lexicon_overload, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(lex_be_t5, silicon_lexicon_overload, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(lex_be_t10, silicon_lexicon_overload, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(silicon_lexicon_overload, information_standard).
narrative_ontology:affects_constraint(silicon_lexicon_overload, regulatory_capture_through_complexity).
narrative_ontology:affects_constraint(silicon_lexicon_overload, credential_inflation_hiring_gatekeeping).

% DUAL FORMULATION NOTE:
% The Silicon Lexicon is upstream of regulatory capture and credential inflation: jargon enables and justifies those constraints by making them linguistically opaque. It also feeds credential inflation in tech hiring: the requirement to demonstrate 'cultural fit' (jargon fluency) becomes a quasi-credential that gates employment. These constraints are structurally distinct but causally coupled through jargon's role as a status signal and exclusion mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(silicon_lexicon_overload, analytical, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
