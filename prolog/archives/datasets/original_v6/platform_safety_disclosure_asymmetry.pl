% ============================================================================
% CONSTRAINT STORY: platform_safety_disclosure_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_safety_disclosure_asymmetry, []).

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
 *   constraint_id: platform_safety_disclosure_asymmetry
 *   human_readable: Platform Safety Disclosure Asymmetry
 *   domain: technology/governance/public_safety
 *
 * SUMMARY:
 *   Platform safety disclosure asymmetry creates a structural constraint
 *   where digital platforms withhold critical safety data from users,
 *   regulators, and researchers while maintaining control over both the
 *   extraction mechanism (data collection) and the suppression mechanism
 *   (data denial). The constraint operates across multiple institutional
 *   contexts simultaneously: users depend on platforms for communication and
 *   participation but cannot access the safety information needed to make
 *   informed decisions; regulators lack the technical documentation and
 *   algorithmic transparency required for standard-setting; researchers face
 *   legal and contractual barriers to independent verification of platform
 *   harms. The constraint exhibits the Tangled Rope signature: genuine
 *   coordination functions exist (platforms provide accessible services,
 *   researchers benefit from platform collaborations), but these are embedded
 *   within an asymmetric extraction structure where platforms capture value
 *   from user data while denying users visibility into safety risks. The
 *   extractiveness trajectory (0.35→0.58) reflects accumulating platform
 *   sophistication in selective disclosure: early platforms disclosed little
 *   but also provided fewer services (lower χ); modern platforms provide
 *   extensive services but simultaneously deploy more sophisticated
 *   data-hoarding mechanisms (higher χ through increased suppression despite
 *   maintained disclosure theater). The theater ratio increase (0.38→0.55)
 *   documents the rise of transparency theater: bug bounty programs,
 *   transparency reports, safety councils, and content moderation boards
 *   create the appearance of accountability without enabling independent
 *   verification of operational safety practices. This is the diagnostic
 *   signature of constraint drift toward institutionalized theater.
 *
 * KEY AGENTS:
 *   - End Users: Primary victims (powerless/trapped) — depend on platforms for digital participation but have zero access to safety documentation; bear risk without agency
 *   - Vulnerable Populations: Secondary victims (powerless/trapped) — children, marginalized communities targeted by algorithmic harms; minimum exit capacity; regulatory frameworks designed to protect them are undermined by disclosure asymmetry
 *   - Safety Researchers: Constrained agents (moderate/constrained) — benefit from platform scale and research collaborations but face legal suppression of findings (DMCA, contract liability, terms-of-service restrictions)
 *   - Journalists: Constrained agents (moderate/constrained) — similar legal and reputational constraints as researchers; dependent on confidential source relationships and platform data access
 *   - Regulatory Bodies: Organized but constrained (organized/constrained) — bear accountability for harms they cannot see; information asymmetry prevents standard-setting and enforcement
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — extract value from user participation while controlling information flow; benefit from de facto coordination on opacity strategies
 *   - Content Moderation Boards and Oversight Councils: Institutional theater (institutional/arbitrage) — create legitimacy appearance without enforcement power or access to operational data; maintain inertia through performance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_safety_disclosure_asymmetry, 0.58).
domain_priors:suppression_score(platform_safety_disclosure_asymmetry, 0.68).
domain_priors:theater_ratio(platform_safety_disclosure_asymmetry, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_safety_disclosure_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_safety_disclosure_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(platform_safety_disclosure_asymmetry, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_safety_disclosure_asymmetry, tangled_rope).
narrative_ontology:human_readable(platform_safety_disclosure_asymmetry, "Platform Safety Disclosure Asymmetry").
narrative_ontology:topic_domain(platform_safety_disclosure_asymmetry, "technology/governance/public_safety").

domain_priors:requires_active_enforcement(platform_safety_disclosure_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_safety_disclosure_asymmetry, platform_operators).
narrative_ontology:constraint_beneficiary(platform_safety_disclosure_asymmetry, institutional_stakeholders).
narrative_ontology:constraint_victim(platform_safety_disclosure_asymmetry, end_users).
narrative_ontology:constraint_victim(platform_safety_disclosure_asymmetry, regulatory_capacity).
narrative_ontology:constraint_victim(platform_safety_disclosure_asymmetry, researcher_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Users cannot exit without abandoning digital participation; they have no access to safety data about the platforms they depend on; suppression is structural (data withholding backed by terms of service and legal threat). Maximum extraction: users bear safety risk while platforms extract value from their participation, capture behavioral data, and control what safety information reaches them.
constraint_indexing:constraint_classification(platform_safety_disclosure_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: VULNERABLE POPULATIONS (SNARE) — Zero exit capacity; highest exposure to platform harms (algorithmic targeting, predation, harassment); zero access to safety documentation; regulatory frameworks specifically designed to protect these groups are undermined by asymmetric disclosure. Maximal snare signature: trapped, information-starved, targeted by extraction mechanisms.
constraint_indexing:constraint_classification(platform_safety_disclosure_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SAFETY RESEARCHERS AND JOURNALISTS (TANGLED ROPE) — Constrained by legal barriers (DMCA takedown, terms-of-service violations, contract liability) but also benefit from platforms' scale and research collaborations. Experience mixed coordination (research access enables discovery) and extraction (legal suppression of findings, career risk from disclosure). Significant agency but at real cost.
constraint_indexing:constraint_classification(platform_safety_disclosure_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY BODIES (TANGLED ROPE) — Organized but constrained by resource asymmetry, jurisdictional limits, and information asymmetry. Benefit from coordination with platforms (information sharing agreements, working groups) but also victimized by withholding of critical safety data needed for standard-setting. Asymmetric extraction: regulators bear accountability for harms they cannot see.
constraint_indexing:constraint_classification(platform_safety_disclosure_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PLATFORM OPERATORS (ROPE) — Primary beneficiary. Asymmetry is functional for them: disclose enough to appear cooperative (security reports, transparency reports, bug bounty programs) while withholding data that would reveal systemic harms or enable external accountability. The disclosure mechanism itself is coordinated: all platforms use similar opacity strategies, creating de facto coordination without explicit collusion.
constraint_indexing:constraint_classification(platform_safety_disclosure_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL THEATER (PITON) — Content moderation boards, transparency reports, and safety councils are largely performative. They create the appearance of accountability without enabling independent verification of platform safety practices. Theater ratio reflects that these mechanisms exist but lack enforcement power or access to operational data. Institutional inertia: the oversight architecture persists because stakeholders benefit from its legitimacy theater even though functional verification remains impossible.
constraint_indexing:constraint_classification(platform_safety_disclosure_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk naturalizing the constraint as inherent to platform economics: platforms must withhold data to protect proprietary algorithms, prevent abuse of shared information, and maintain security. From this view, the asymmetry is immutable — platforms cannot fully disclose without compromising their own safety mechanisms. However, this risks conflating 'difficult to disclose' with 'impossible to disclose,' legitimizing convenience as necessity. The engine's false summit detector should flag this perspective.
constraint_indexing:constraint_classification(platform_safety_disclosure_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_safety_disclosure_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_safety_disclosure_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_safety_disclosure_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_safety_disclosure_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_safety_disclosure_asymmetry, TR),
    TR >= 0.70.

:- end_tests(platform_safety_disclosure_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits classic extraction: platforms capture behavioral data, attention, and economic value from users while systematically denying users access to safety information that would enable exit or informed participation. The value is not extreme (platforms do provide real services and some limited transparency) but represents a significant asymmetry. The trajectory from 0.35 to 0.58 reflects platform sophistication in selective disclosure — disclosing enough to appear cooperative while withholding data that would enable independent verification of systemic harms. Suppression (0.68): High. Barriers to accessing platform safety data include: legal (DMCA takedown notices, copyright claims on data analysis), contractual (terms of service prohibiting reverse-engineering, researcher access restrictions), and technical (algorithmic opacity, proprietary system design). Suppression mechanisms are multiple and mutually reinforcing. Theater ratio (0.55): Moderate-high. Transparency reports, bug bounty programs, safety councils, and content moderation boards create the appearance of accountability while lacking enforcement power or access to operational safety metrics. The theater has increased over the interval as platforms have deployed more sophisticated legitimacy mechanisms in response to regulatory pressure.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximal perspectival divergence: platform operators see Rope (functional coordination on transparency standards), end users see Snare (pure extraction with no exit), researchers see Tangled Rope (mixed coordination and suppression), regulators see Tangled Rope at larger scale (institutional-level coordination with built-in information asymmetry), and the analytical observer risks naturalizing the structure as an immutable property of digital platform economics. The gap is not measurement error or observer subjectivity — it reflects real structural differences in how different agents experience the same constraint. The constraint is indeed tangled: genuine coordination functions exist (platforms provide services, research collaborations happen), but these are embedded within and subordinate to an extraction structure. The theater ratio documents the constraint's sophistication: it maintains legitimacy through performative accountability mechanisms precisely because underlying extraction is significant.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the disclosure flow. End users: trapped with no exit; full victims; d ≈ 0.95-1.0, producing maximum experienced extractiveness. Vulnerable populations: trapped with maximum targeting by extraction mechanisms (algorithmic amplification of harmful content, predatory behavior facilitation); d ≈ 0.95-1.0. Safety researchers: moderately constrained (alternatives exist via web scraping, user surveys, API access but at high cost and reduced quality); partial victims (access restrictions) and partial beneficiaries (research collaboration); d ≈ 0.72-0.78. Regulators: organized with resource constraints and jurisdictional limits; partial victims (information asymmetry) and partial beneficiaries (working relationships); d ≈ 0.55-0.65. Platform operators: arbitrage exit options (can reposition, change policy, negotiate with regulators); full beneficiaries; d ≈ 0.10-0.20. The directional range is wide, producing corresponding variation in experienced extractiveness (χ) across perspectives. Platform operators' low d dampens their experienced extraction into a coordination benefit (Rope). End users' high d amplifies their extraction into pure Snare. The asymmetry in d values IS the constraint's core structure.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint classifies as Tangled Rope across multiple perspectives, resolving the ambiguity that would arise if classified as pure extraction (Snare) or pure coordination (Rope). Tangled Rope classification correctly captures that the constraint BOTH coordinates (enabling user access to services, enabling researcher collaboration) AND extracts (asymmetric information, suppressed safety data, regulatory capture dynamics). The mandatrophy is resolved by explicitly declaring beneficiaries (platform operators, institutional stakeholders), victims (end users, regulatory capacity, researcher access), and the duality of the enforcement structure: the disclosure system enforces coordination (platforms are enforced to disclose something, creating transparency theater) while simultaneously enforcing asymmetry (suppression mechanisms prevent full disclosure, creating information advantage). The rise in theater ratio (0.38→0.55) confirms the mandatrophy resolution: as extraction pressure increases, theater increases to maintain legitimacy. If the constraint were pure coordination (Rope), theater would decline (coordination needs no performance). If the constraint were pure extraction (Snare), theater might vary but would not need to sustain coordination rhetoric. The actual pattern — theater rising as extractiveness rises — is the diagnostic signature of Tangled Rope struggling to maintain the coordination fiction against accumulating evidence of asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disclosure_scope_boundary,
    'What constitutes sufficient disclosure for safety accountability: aggregate trend data, individual-incident documentation, algorithmic decision thresholds, or full operational transparency?',
    'Comparative analysis of disclosure regimes (EU vs US vs China); correlation between disclosure scope and measurable safety outcomes; researcher capacity studies on what data enables independent verification',
    'If aggregate trends suffice: constraint may reclassify to Rope (genuine coordination on shared safety standards). If operational transparency required: constraint remains Snare/Tangled Rope (platforms cannot/will not disclose at required depth).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_scope_boundary, conceptual, 'What disclosure scope is sufficient for accountability').

omega_variable(
    exit_capacity_for_end_users,
    'Are end users genuinely trapped (no viable alternative to platform participation) or constrained (alternatives exist at high cost)?',
    'Measurement of alternative platform adoption rates, cost-benefit analysis of switching platforms, network effects quantification, necessity assessment for employment/education/civic participation',
    'If trapped: enduser perspectival d should be 0.95-1.0, maximizing chi. If constrained: d should be ~0.75-0.85, reducing chi slightly. Affects classification stability across time horizons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_capacity_for_end_users, empirical, 'Whether end users are trapped or constrained by platform dependency').

omega_variable(
    platform_coordination_mechanism,
    'Is the similarity in opacity strategies across platforms (all use partial transparency reports, all restrict research access similarly) evidence of explicit collusion, regulatory compliance convergence, or independent economic incentive alignment?',
    'Historical analysis of disclosure policy evolution; comparative timeline of transparency report adoption; DOJ/FTC investigation findings; researcher interviews on independent vs coordinated pressure',
    'If collusion: constraint exhibits coordination function (beneficiary perspective should reclassify to explicit Rope). If convergence: each platform individually extracts, but the aggregate effect is coordinated suppression — constraint remains Snare/Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_coordination_mechanism, empirical, 'Explicit vs independent platform coordination on disclosure asymmetry').

omega_variable(
    regulatory_capture_dynamics,
    'Do platform operators actively capture regulatory bodies through revolving-door staffing, technical expertise capture, or agenda-setting, or do regulators passively accept asymmetry due to resource constraints?',
    'Network analysis of regulator-platform staffing flows; policy lag vs technology deployment timeline; regulator budget vs platform R&D budget; regulatory comment period analysis for substantive vs performative input',
    'If active capture: regulatory perspective''s d should be overridden upward (they are partly victims of coordinated extraction by platforms). If passive acceptance: d remains moderate, reflecting organized agent status with constrained exit but real agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_dynamics, empirical, 'Active capture vs passive constraint of regulatory bodies').

omega_variable(
    researcher_access_substitutability,
    'Can alternative research methodologies (web scraping, user surveys, purchased API access, bug bounty program participation) enable sufficient safety verification despite platform withholding, or is platform-internal access irreplaceable?',
    'Comparative effectiveness study: findings from alternative methods vs findings from privileged-access researchers; gap analysis on harms undetectable via alternatives; researcher assessment of substitutability',
    'If substitutes work: researcher exit is less trapped than current classification suggests; reclassify from Snare to Tangled Rope. If irreplaceable: current classification holds; certain harms remain unverifiable without platform cooperation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(researcher_access_substitutability, empirical, 'Whether alternative research methodologies can substitute for platform data access').

omega_variable(
    suppression_mechanism_internalization,
    'Do researchers and regulators internalize platform framing (believing disclosure is genuinely impossible/dangerous) or explicitly perceive suppression as externally imposed?',
    'Qualitative research interviews with researchers/regulators on their mental model of disclosure constraints; comparison of stated barriers vs actual technical/legal requirements; longitudinal tracking of how framing changes with evidence',
    'If internalized: exit option should shift toward identity_locked for some researchers. If explicit: exit remains constrained (external barriers, not internal identity capture). Affects theater ratio interpretation — internalization increases effective theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is internalized or externally imposed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_safety_disclosure_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psd_tr_t0, platform_safety_disclosure_asymmetry, theater_ratio, 0, 0.38).
narrative_ontology:measurement(psd_tr_t5, platform_safety_disclosure_asymmetry, theater_ratio, 5, 0.48).
narrative_ontology:measurement(psd_tr_t10, platform_safety_disclosure_asymmetry, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(psd_be_t0, platform_safety_disclosure_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(psd_be_t5, platform_safety_disclosure_asymmetry, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(psd_be_t10, platform_safety_disclosure_asymmetry, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_safety_disclosure_asymmetry, information_standard).
narrative_ontology:affects_constraint(platform_safety_disclosure_asymmetry, algorithmic_opacity).
narrative_ontology:affects_constraint(platform_safety_disclosure_asymmetry, content_moderation_accountability).
narrative_ontology:affects_constraint(platform_safety_disclosure_asymmetry, data_minimization_norms).
narrative_ontology:affects_constraint(platform_safety_disclosure_asymmetry, researcher_access_barriers).

% DUAL FORMULATION NOTE:
% Platform safety disclosure asymmetry is the structural constraint that integrates multiple specific harms: algorithmic opacity (users cannot understand ranking), content moderation opacity (decisions opaque to users), researcher access barriers (systematic prevention of external verification), and data minimization failures (platforms collect beyond stated purposes). Each of these has its own constraint story with different ε values reflecting their specific empirical status. The disclosure asymmetry is upstream — it enables and sustains all these specific harms. Decomposition follows ε-invariance: algorithmic opacity (ε≈0.42) reflects epistemic limits on explainability; safety researcher access barriers (ε≈0.55) reflect legal/contractual suppression; content moderation outcomes opacity (ε≈0.45) reflects partial disclosure theater. Platform safety disclosure asymmetry (ε≈0.58) captures the meta-constraint that integrates these: the systematic architecture that sustains opacity across all specific domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_safety_disclosure_asymmetry, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
