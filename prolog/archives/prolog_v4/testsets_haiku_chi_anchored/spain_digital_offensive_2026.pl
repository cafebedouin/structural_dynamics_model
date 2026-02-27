% ============================================================================
% CONSTRAINT STORY: spain_digital_offensive_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_spain_digital_offensive_2026, []).

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
 *   constraint_id: spain_digital_offensive_2026
 *   human_readable: Spanish Five-Point Digital Offensive
 *   domain: political/technological/legal
 *
 * SUMMARY:
 *   In 2024-2026, Spanish PM Pedro Sánchez declared social media a 'failed
 *   state' and announced a five-point digital offensive targeting platform
 *   accountability, content moderation, and information control. The
 *   offensive frames social media regulation as solving collective action
 *   problems (misinformation, electoral integrity, social cohesion), yet
 *   simultaneously concentrates information gatekeeping power in state
 *   institutions and traditional media. This constraint exhibits structural
 *   tension between genuine coordination needs (addressing algorithmic
 *   amplification, platform opacity) and extractive state power
 *   consolidation. The classification depends critically on directionality:
 *   whether the offensive targets genuine platform externalities
 *   (coordination problem) or uses externality framing to suppress digital
 *   dissent (extraction). Global platforms experience the offensive as
 *   simultaneous coordination demand and extraction pressure; creators
 *   experience escalating compliance costs with trapped exit options; the
 *   state apparatus experiences it as power restoration; traditional media
 *   experiences it as audience re-consolidation; organized digital rights
 *   movements see it as temporary institutional response with eventual sunset
 *   through EU harmonization; the compliance bureaucracy maintains
 *   increasingly theatrical moderation processes.
 *
 * KEY AGENTS:
 *   - Spanish State Apparatus: Primary beneficiary (institutional/arbitrage) — regains information control and agenda-setting power; benefits from institutional coordination framing
 *   - Traditional Media Establishment: Secondary beneficiary (institutional/constrained) — strengthened gatekeeping role; reduced competition from platform distribution
 *   - Global Social Media Platforms: Primary victim/target (powerful/arbitrage) — faces escalating compliance costs, moderation pressure, operational constraints; can arbitrage between jurisdictions but subject to enforcement threats
 *   - Digital Content Creators: Constrained victim (powerless/trapped) — dependent on platform distribution; face escalating compliance uncertainty and audience risk
 *   - Citizen Digital Autonomy: Abstract victim (powerless/trapped) — loss of unmediated expression channels; increased surveillance and content control
 *   - EU Digital Rights Coalition: Organized observer (organized/mobile) — advocating for transparency and user control; mobile within EU regulatory framework
 *   - Content Moderation Bureaucracy: Institutional actor (institutional/arbitrage) — maintains performative compliance theater; recognizes own degradation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(spain_digital_offensive_2026, 0.58).
domain_priors:suppression_score(spain_digital_offensive_2026, 0.68).
domain_priors:theater_ratio(spain_digital_offensive_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(spain_digital_offensive_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(spain_digital_offensive_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(spain_digital_offensive_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(spain_digital_offensive_2026, tangled_rope).
narrative_ontology:human_readable(spain_digital_offensive_2026, "Spanish Five-Point Digital Offensive").
narrative_ontology:topic_domain(spain_digital_offensive_2026, "political/technological/legal").

domain_priors:requires_active_enforcement(spain_digital_offensive_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(spain_digital_offensive_2026, spanish_state_apparatus).
narrative_ontology:constraint_beneficiary(spain_digital_offensive_2026, traditional_media_establishment).
narrative_ontology:constraint_victim(spain_digital_offensive_2026, social_media_platforms).
narrative_ontology:constraint_victim(spain_digital_offensive_2026, digital_content_creators).
narrative_ontology:constraint_victim(spain_digital_offensive_2026, citizen_digital_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED DIGITAL CREATOR (SNARE) — Content creators dependent on social media distribution face escalating compliance costs, moderation pressure, and regulatory uncertainty. Cannot exit platforms without abandoning audience. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(spain_digital_offensive_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GLOBAL SOCIAL MEDIA PLATFORM (TANGLED ROPE) — Experiences coordination demands (content moderation, regulatory compliance, local language support) coupled with extraction of compliance costs and operational constraints. Can arbitrage between jurisdictions but subject to escalating enforcement threats. d≈0.58, f(d)≈0.68, σ=1.1 → χ≈0.43.
constraint_indexing:constraint_classification(spain_digital_offensive_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: SPANISH STATE APPARATUS (ROPE) — Benefits from the offensive as coordination tool: establishes information control, regains agenda-setting power from decentralized social networks, builds capacity for strategic enforcement. Frames regulation as solving collective action problem (misinformation, electoral integrity, social cohesion). d≈0.12, f(d)≈0.06, σ=1.0 → χ≈0.03.
constraint_indexing:constraint_classification(spain_digital_offensive_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRADITIONAL MEDIA ESTABLISHMENT (ROPE) — Regulatory framework strengthens gatekeeping role of established newspapers, TV, radio against platform distribution. Benefits from audience re-consolidation and reduced competition from viral citizen content. Experiences as coordination (restoring journalistic authority). d≈0.18, f(d)≈0.14, σ=1.0 → χ≈0.08.
constraint_indexing:constraint_classification(spain_digital_offensive_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EU DIGITAL RIGHTS COALITION (SCAFFOLD) — Organized civil society actors (Mozilla, EDRI, digital rights orgs) see the Spanish offensive as a temporary institutional response to platform externalities with sunset logic: proper algorithmic transparency, decentralization, and user control mechanisms will eventually replace state-mandated content removal. Currently constrained by lobbying disadvantage but mobile within EU framework. d≈0.48, f(d)≈0.58, σ=1.1 → χ≈0.37.
constraint_indexing:constraint_classification(spain_digital_offensive_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: CONTENT MODERATION BUREAUCRACY (PITON) — The five-point offensive creates extensive compliance theater: notification systems, appeals processes, transparency reports, auditing procedures. Much of this is performative (appeals rarely succeed; moderation at scale cannot achieve stated goals). Maintained through institutional inertia despite recognizing its own degradation. theater_ratio=0.64. d≈0.22, f(d)≈0.18, σ=1.0 → χ≈0.11.
constraint_indexing:constraint_classification(spain_digital_offensive_2026, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spain_digital_offensive_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(spain_digital_offensive_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spain_digital_offensive_2026, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(spain_digital_offensive_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(spain_digital_offensive_2026, TR),
    TR >= 0.70.

:- end_tests(spain_digital_offensive_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The offensive extracts compliance costs from platforms and creators while consolidating state information control. However, extraction is not total (0.70+) because there are genuine coordination problems social media creates (algorithmic amplification, misinformation spread, electoral manipulation). The empirical measure reflects this mixture: the state captures real externalities in its framing, but uses them as justification for broader power consolidation. The initial measurement (0.42) at interval start reflects lower explicit enforcement; the trajectory to 0.58 reflects escalating regulatory action and compliance burden accumulation. Suppression (0.68): High. Content creators face material barriers to exit (audience dependency), platforms face jurisdiction lock-in (cannot simply leave Spanish market), and the moderation system itself suppresses alternative communication channels. However, suppression is not absolute (0.90+) because EU frameworks, digital alternatives, and organized resistance exist. Theater ratio (0.64): Moderate-high. The offensive generates substantial performative activity: compliance notifications, appeals processes, transparency reports, fact-checking audits. Much is theater because: moderation at scale cannot achieve stated quality goals; appeals rarely succeed; fact-checking is partisan-vulnerable; compliance burden is designed to signal action rather than solve problems. Theater has increased from 0.38 to 0.64 over 12 months as the bureaucracy scales compliance processes faster than actual problem-solving.
 *
 * PERSPECTIVAL GAP:
 *   The original research framing (Sánchez's 'failed state' rhetoric) presupposes the state apparatus perspective (Rope: coordination problem). But this naturalizes the state's structural position and ignores the victim perspectives. Digital creators see a Snare (trapped, escalating costs, no exit). Platforms see Tangled Rope (genuine coordination demands mixed with extraction pressure). The compliance bureaucracy sees Piton (increasingly performative theater maintaining inertial processes). The EU digital rights coalition sees Scaffold (temporary response with sunset via harmonization). The analytical observer, integrating all perspectives, sees tangled_rope as claimed type: the offensive exhibits both genuine coordination function (platforms do create externalities) AND asymmetric extraction (state consolidates control beyond necessity). This dual structure — not pure extraction, not pure coordination — is mandatrophy-resolved tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Spanish state apparatus: Beneficiary + arbitrage → d≈0.12, f(d)≈0.06. Low effective extraction; net beneficiary from power consolidation. Captures information control as coordination benefit. Traditional media: Beneficiary + constrained → d≈0.18, f(d)≈0.14. Benefits from regulation but constrained by EU frameworks and audience preferences. Global platforms: Victim/target + arbitrage → d≈0.58, f(d)≈0.68. Significant extraction: compliance costs, operational constraints, but powerful enough to arbitrage between jurisdictions (arbitrage exit keeps d below 0.75). Digital creators: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction: audience-dependent exit is illusory; compliance burden is forced. Citizen digital autonomy: Victim + trapped → d≈0.95, f(d)≈1.42. Abstract collective cannot organize; no exit from surveillance infrastructure. EU digital rights coalition: Organized observer + mobile → d≈0.48, f(d)≈0.58. Moderate directionality; coalition has agency and mobility within EU framework. Content moderation bureaucracy: Institutional + arbitrage → d≈0.22, f(d)≈0.18. Low directionality; institutional actors maintain piton through inertia.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint resolves mandatrophy by demonstrating structural duality. Extractiveness (0.58) exceeds rope threshold (0.35) because the state extracts control beyond coordination necessity. Suppression (0.68) meets tangled rope minimum (0.40) because creators and platforms face material barriers. But the constraint has genuine coordination function (platforms do amplify misinformation; electoral integrity faces real threats from algorithmic manipulation). This is not pure extraction masquerading as coordination (Snare with false coordination claim) because: platforms experience real moderation demands, creators benefit from some content standards, the state's coordination framing captures genuine externalities. It is tangled rope because: the state consolidates power beyond what coordination alone requires, beneficiaries (state, traditional media) capture asymmetric gains, victims (creators, platforms) bear disproportionate costs, enforcement is required to sustain the asymmetry. The analytical test: if all coordination benefits could be achieved through platform self-regulation + transparent algorithmic standards + user control, would the state still impose the five-point offensive? The answer reveals extraction beyond coordination necessity. Current evidence suggests yes — the offensive targets state agenda-setting power restoration, not just problem-solving. This is why tangled_rope (not scaffold) is the claimed type: the sunset condition is not clearly operationalized, and state institutions have incentive to prevent rather than enable the sunset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    misinformation_measurement_problem,
    'What constitutes ''misinformation'' actionable under the offensive, and who defines it?',
    'Comparative analysis of moderation decisions across competing claims; tracking of appeals success rates; independence audit of fact-checking methodology',
    'If definition is tight and independent: regulatory framework enables genuine coordination. If definition is partisan: framework enables state information control (extraction). Current ambiguity allows both interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(misinformation_measurement_problem, conceptual, 'Definition and measurement of actionable misinformation').

omega_variable(
    platform_exit_feasibility,
    'Can creators and users realistically exit mainstream platforms (Meta, TikTok, X) to alternative networks without audience collapse?',
    'Longitudinal tracking of migration rates; audience size correlation; network effects persistence on alternative platforms; income stability for creators post-migration',
    'If exit is feasible: creator constraint downgrade (trapped → constrained). If exit leads to audience collapse: exit is illusory, snare classification confirmed. Currently asymmetric by creator size.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_exit_feasibility, empirical, 'Feasibility of platform exit for creators and users').

omega_variable(
    enforcement_asymmetry_direction,
    'Does the offensive disproportionately target anti-government content vs pro-government disinformation, or is enforcement actually content-neutral?',
    'Detailed audit of moderation decisions by content direction (pro/anti-government); statistical analysis of appeal outcomes; tracking of platform compliance notices by claim type',
    'If enforcement is asymmetric: snare classification dominates; effective extraction against dissent. If enforcement is neutral: tangled_rope confirmed; genuine coordination problem. Currently opaque.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_asymmetry_direction, empirical, 'Directionality and symmetry of enforcement decisions').

omega_variable(
    eu_regulatory_harmonization,
    'Will the Digital Services Act and related EU frameworks eventually render Spain''s unilateral five-point offensive obsolete?',
    'Tracking of DSA implementation; comparison of Spanish requirements vs harmonized EU standards; pressure from EU for regulatory alignment; sunset clause emergence',
    'If EU harmonization occurs: scaffold perspective confirmed; sunset becomes real. If Spain maintains parallel system: extraction mechanism persists independently of broader trends.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_regulatory_harmonization, empirical, 'EU regulatory harmonization trajectory and obsolescence timeline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(spain_digital_offensive_2026, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spain_dig_tr_t0, spain_digital_offensive_2026, theater_ratio, 0, 0.38).
narrative_ontology:measurement(spain_dig_tr_t6, spain_digital_offensive_2026, theater_ratio, 6, 0.55).
narrative_ontology:measurement(spain_dig_tr_t12, spain_digital_offensive_2026, theater_ratio, 12, 0.64).

% Extraction over time
narrative_ontology:measurement(spain_dig_be_t0, spain_digital_offensive_2026, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(spain_dig_be_t6, spain_digital_offensive_2026, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(spain_dig_be_t12, spain_digital_offensive_2026, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(spain_digital_offensive_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(spain_digital_offensive_2026, eu_digital_services_act_compliance).
narrative_ontology:affects_constraint(spain_digital_offensive_2026, platform_content_liability_regimes).
narrative_ontology:affects_constraint(spain_digital_offensive_2026, algorithmic_transparency_mandates).

% DUAL FORMULATION NOTE:
% The Spanish five-point offensive is a specific national instantiation of a broader constraint family around platform regulation. The EU Digital Services Act represents a harmonization attempt with different structural properties (ε≈0.35, lighter suppression, stronger coordination logic). The offensive's higher extractiveness (0.58) reflects nationalist state consolidation rather than coordination problem-solving. These constraints are linked: the offensive is downstream of both genuine platform externalities AND state capacity recovery pressures; the DSA is upstream as a potential sunset mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(spain_digital_offensive_2026, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
