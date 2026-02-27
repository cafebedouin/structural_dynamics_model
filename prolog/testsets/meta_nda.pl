% ============================================================================
% CONSTRAINT STORY: meta_nda
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meta_nda, []).

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
 *   constraint_id: meta_nda
 *   human_readable: Meta's Non-Disclosure Agreements for Undercover Testers
 *   domain: economic/platform_governance
 *
 * SUMMARY:
 *   Meta employs undercover testers to evaluate platform safety in simulated
 *   scenarios (simulated terrorist attacks, simulated school shootings)
 *   designed to identify vulnerabilities in content moderation,
 *   recommendation systems, and coordinated inauthentic behavior detection.
 *   These testers sign Non-Disclosure Agreements that prevent them from
 *   disclosing information about platform flaws, testing methodologies, or
 *   manipulative tactics discovered during testing. The NDA regime functions
 *   as a suppression mechanism: it prevents testers from warning researchers,
 *   policymakers, or the public about safety vulnerabilities or exploitable
 *   gaps. This constraint exhibits core snare characteristics: high
 *   suppression (legal liability for disclosure), high extractiveness
 *   (testers bear reputational and career costs of silence while Meta
 *   captures reputation-management benefits), and a power asymmetry
 *   (corporate legal enforcement vs. individual contract liability). However,
 *   the constraint is degrading: regulatory investigations increasingly
 *   override NDAs, external researchers discover similar vulnerabilities
 *   through independent testing, and emerging legislation (Digital Services
 *   Act, proposed U.S. platform transparency rules) will likely make
 *   safety-critical disclosures legally mandatory, establishing a scaffold
 *   sunset trajectory. The theater_ratio reflects that the NDA mechanism is
 *   partly performative — it prevents casual disclosure but does not prevent
 *   determined regulators, journalists, or whistleblowers from obtaining the
 *   same information.
 *
 * KEY AGENTS:
 *   - Undercover Testers: Primary victims (powerless/trapped) — contractually bound to silence; bear legal and career liability for disclosure
 *   - Meta Corporation: Primary beneficiary (institutional/arbitrage) — captures information asymmetry advantage; controls disclosure timing and framing
 *   - Public Epistemic Commons: Secondary victim (powerless/trapped) — cannot access safety-critical information about platform vulnerabilities
 *   - Regulatory Bodies (FTC, EU regulators, legislators): Organized stakeholders (organized/constrained) — have authority to demand disclosure but face jurisdictional and technical barriers
 *   - External Researchers and Journalists: Organized agents (organized/mobile) — can independently discover vulnerabilities; bypass NDA regime through external research
 *   - Emerging Regulatory Frameworks (DSA, proposed U.S. legislation): Structural sunset mechanism (organized/mobile) — building requirements for mandatory safety disclosure that will override NDAs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meta_nda, 0.58).
domain_priors:suppression_score(meta_nda, 0.72).
domain_priors:theater_ratio(meta_nda, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meta_nda, extractiveness, 0.58).
narrative_ontology:constraint_metric(meta_nda, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(meta_nda, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meta_nda, snare).
narrative_ontology:human_readable(meta_nda, "Meta's Non-Disclosure Agreements for Undercover Testers").
narrative_ontology:topic_domain(meta_nda, "economic/platform_governance").

domain_priors:requires_active_enforcement(meta_nda).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meta_nda, meta_corporation).
narrative_ontology:constraint_victim(meta_nda, undercover_testers).
narrative_ontology:constraint_victim(meta_nda, public_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDERCOVER TESTER (SNARE) — Contractually bound to silence about platform flaws, manipulative testing protocols, and safety gaps discovered during simulated attack scenarios. Cannot disclose to researchers, journalists, regulators, or the public without legal liability. Trapped by employment dependence and legal contract; no viable exit option. Bears full cost of enforced silence.
constraint_indexing:constraint_classification(meta_nda, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLIC EPISTEMIC COMMONS (SNARE) — Cannot access critical information about platform safety vulnerabilities, testing methodologies, or failure modes discovered by Meta's internal testers. Information asymmetry is structurally enforced. No organized constituency; no exit option. Bears full cost of suppressed knowledge.
constraint_indexing:constraint_classification(meta_nda, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: META CORPORATION (ROPE) — Experiences the NDA regime as coordination: protecting proprietary testing methodologies, preventing competitive disclosure, managing reputational risk during platform iteration. Has maximum exit options (can modify, terminate, or renegotiate agreements). Extraction runs toward this agent — they are the beneficiary.
constraint_indexing:constraint_classification(meta_nda, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY BODIES AND LEGISLATORS (TANGLED ROPE) — Have organizational power to demand disclosure and subpoena information, but constrained by: (1) jurisdiction limits on Meta's global operations, (2) technical expertise gaps in assessing platform vulnerabilities, (3) slow legislative/regulatory cycles vs. rapid platform iteration. Experience both extraction (suppressed evidence during investigations) and coordination function (NDAs do prevent competitive espionage and ensure quality of internal testing).
constraint_indexing:constraint_classification(meta_nda, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CORPORATE LEGAL NDA FRAMEWORK (PITON) — The broader system of NDAs in corporate environments is largely performative: it prevents public disclosure but does not prevent regulators from subpoenaing information, does not prevent internal whistleblowers, and increasingly does not prevent researchers from reverse-engineering platform behavior. The ritual of confidentiality persists through institutional inertia, but its actual functional capacity to suppress information has degraded. Theater ratio reflects that the legal mechanism maintains a performative barrier while alternative disclosure pathways (regulatory subpoena, research papers, media investigation) increasingly bypass it.
constraint_indexing:constraint_classification(meta_nda, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EMERGING DISCLOSURE STANDARDS (SCAFFOLD) — Digital Services Act (EU), upcoming U.S. platform regulation, and investor disclosure requirements are creating structural mechanisms that will force platform safety information to be disclosed to regulators, researchers, and potentially the public. These regulatory frameworks have a genuine sunset trajectory: as they mature, Meta's unilateral NDA regime loses enforceability. Low effective extraction because the sunset is already embedded in regulatory timelines (5-10 years for full DSA/U.S. compliance).
constraint_indexing:constraint_classification(meta_nda, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_nda_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meta_nda, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meta_nda, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meta_nda, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(meta_nda, TR),
    TR >= 0.70.

:- end_tests(meta_nda_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Meta captures significant benefits from the NDA regime: suppressed knowledge of platform flaws prevents competitive reverse-engineering, delays public criticism during vulnerability remediation, and maintains negotiating advantage with regulators who lack independent verification of safety gaps. However, extractiveness is not extreme (would be 0.75+) because: (1) regulators can subpoena information despite NDAs, (2) external researchers independently discover vulnerabilities, (3) whistleblower disclosures occur despite legal liability, (4) the regime is increasingly recognized as unjust, creating reputational costs to Meta. Suppression (0.72): High. Legal enforcement mechanism is strong (contracts are binding, violations carry legal liability). Alternatives for testers are limited (cannot disclose without liability; cannot maintain employment and criticize Meta publicly). However, suppression is not total (0.85+) because regulatory subpoena power and determined whistleblowers can overcome it. Theater ratio (0.45): Moderate-low. The NDA mechanism functions substantively (prevents casual disclosure, manages information flow timing) but increasingly performs its own limitation (external discovery paths exist, regulatory override is coming). The theater has been rising over the interval as regulatory pressure increases and the gap between public knowledge (from regulatory investigations) and NDA-bound knowledge (from testers) becomes visible.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a dramatic perspectival gap between Meta (who sees coordination and reputation management) and testers (who see pure extraction and legal liability). Meta's rope classification reflects their structural position: NDAs solve a real coordination problem (preventing competitive espionage, ensuring testing rigor). Testers' snare classification reflects theirs: they are trapped by legal liability with no exit and no benefit. Regulators see tangled_rope because they have some power (subpoena authority) but constrained exit (jurisdictional limits, expertise gaps). External researchers see a mostly-degraded constraint (piton) because they can bypass NDAs through independent testing, making the legal mechanism increasingly theatrical. The analytical observer at the regulatory/generational level sees an emerging scaffold because DSA and U.S. legislation will make the NDA unenforceable for safety-critical information within 5-10 years. This perspectival range (snare to rope to tangled_rope to piton to scaffold) demonstrates how the same structural mechanism appears differently depending on the observer's power, exit options, and time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the extraction flow. Undercover testers: d ≈ 0.95 (trapped victim with no exit, full target of extraction). Meta: d ≈ 0.05 (institutional beneficiary with arbitrage options, full beneficiary). Public epistemic commons: d ≈ 0.98 (powerless, unable to organize, bears cost of suppression, no exit). Regulators: d ≈ 0.60 (organized agents with subpoena power but constrained by jurisdictional limits and technical expertise gaps — mixed experience of extraction and coordination). External researchers: d ≈ 0.30 (can bypass NDA through independent research, so experience reduced extraction; benefit from Meta's public platform while avoiding NDA liability). The sigmoid f(d) transforms these structural positions into experienced extractiveness chi. High d values (victims) produce high f(d) ≈ 1.4, amplifying experienced extraction. Low d values (beneficiaries) produce f(d) ≈ -0.12, converting chi negative (net benefit to agent). Meta's global scope σ(S)=1.2 amplifies their effective extraction chi = 0.58 × f(d=0.05) × 1.2 ≈ -0.08 (net benefit); testers experience χ = 0.58 × f(d=0.95) × 1.2 ≈ 0.98 (high extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids the mandatrophy (false labeling of snare as rope or vice versa) by recognizing that Meta genuinely benefits from NDA-enforced coordination (preventing competitive espionage, managing disclosure timing) while simultaneously extracting from testers who have no viable exit. The snare classification is correct for the tester perspective because their power is powerless and exit is trapped — they experience pure extraction with coercion. The rope classification is correct for Meta's perspective because they genuinely coordinate testing rigor and prevent information leakage that would harm their competitive position. The tangled_rope classification for regulators is correct because they have both coordination function (investigating platform safety) and extraction (suppressed evidence during investigations). The mandatrophy is resolved by recognizing that 'coordination' and 'extraction' are not properties of the constraint itself but properties of the structural relationship between the constraint and the observer. Meta coordinates and extracts simultaneously from different perspectives. The engine's perspectival approach correctly models this: no single type is 'the true' classification; rather, the presheaf of perspectives reveals the full structural content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nda_legal_enforceability,
    'Do Meta''s NDAs with testers remain legally enforceable against regulatory subpoena, whistleblower disclosure, or government investigation?',
    'Precedent from regulatory investigations (FTC, Senate Commerce Committee, EU regulators); litigation outcomes when testers or regulators challenge NDA scope',
    'If enforceable: snare classification confirmed across all non-regulatory perspectives. If unenforceable against subpoena: extraction shifts from suppression to reputational damage management — snare becomes tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nda_legal_enforceability, empirical, 'Legal enforceability of NDAs against regulatory disclosure').

omega_variable(
    tester_substitute_discovery,
    'Can external researchers, journalists, or activists discover the same platform vulnerabilities through independent testing without being contractually bound to silence?',
    'Comparative analysis of independently discovered platform flaws vs. those disclosed by insiders; timeline comparison of public discovery vs. internal testing discovery',
    'If external discovery is rapid and complete: NDA extraction mechanism is degraded (piton). If external discovery is slow/incomplete: NDA suppression is more effective (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tester_substitute_discovery, empirical, 'Efficacy of external discovery vs. internal testing').

omega_variable(
    regulatory_override_trajectory,
    'Will emerging regulatory frameworks (DSA, proposed U.S. platform legislation) legally override corporate NDAs for safety-critical information disclosure?',
    'Legislative text analysis; EU implementation guidance on DSA transparency requirements; U.S. Congressional testimony on regulatory authority over platform disclosures',
    'If override occurs: scaffold sunset is real — NDAs become unenforceable within 5-10 years. If no override: extraction mechanism persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_override_trajectory, empirical, 'Regulatory override of corporate NDAs in future legislation').

omega_variable(
    tester_whistleblower_rates,
    'What fraction of Meta''s undercover testers ultimately disclose information to regulators, media, or researchers despite NDAs?',
    'Tracking of documented whistleblower disclosures; analysis of investigation reports citing internal tester testimony; surveys of former testers',
    'If whistleblower rate exceeds 20%: suppression is degraded (effective extraction reduces). If under 5%: suppression is highly effective (snare confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tester_whistleblower_rates, empirical, 'Actual disclosure rate among undercover testers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meta_nda, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meta_nda_tr_t0, meta_nda, theater_ratio, 0, 0.35).
narrative_ontology:measurement(meta_nda_tr_t3, meta_nda, theater_ratio, 3, 0.4).
narrative_ontology:measurement(meta_nda_tr_t6, meta_nda, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(meta_nda_be_t0, meta_nda, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(meta_nda_be_t3, meta_nda, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(meta_nda_be_t6, meta_nda, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meta_nda, information_standard).
narrative_ontology:affects_constraint(meta_nda, facebook_content_moderation_opacity).
narrative_ontology:affects_constraint(meta_nda, platform_safety_disclosure_asymmetry).

% DUAL FORMULATION NOTE:
% Meta's NDA regime is downstream of broader platform governance and information asymmetry dynamics. It represents a specific institutional mechanism for suppressing safety information that is part of a larger constraint family involving platform opacity, regulatory capture, and epistemic asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
