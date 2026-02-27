% ============================================================================
% CONSTRAINT STORY: dldr_information_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dldr_information_policy, []).

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
 *   constraint_id: dldr_information_policy
 *   human_readable: Don't Like, Don't Read (DLDR) Information Policy
 *   domain: technological/social
 *
 * SUMMARY:
 *   The 'Don't Like, Don't Read' (DLDR) policy represents a fundamental shift
 *   in information curation responsibility from platforms to readers.
 *   Originally framed as protecting reader autonomy and freedom of access,
 *   DLDR has become a structural mechanism for platforms to avoid the costs
 *   and legal exposure of content moderation while maintaining plausible
 *   deniability about harmful material in their archives. The constraint
 *   exhibits the characteristic structure of a tangled rope: it coordinates
 *   around a genuine problem (how to preserve diverse viewpoints without
 *   platform censorship) while simultaneously extracting by shifting curation
 *   labor and harm exposure onto readers, particularly vulnerable
 *   populations. The theater ratio has increased over the interval (0.35 →
 *   0.58) as platforms have invested in performative neutrality rhetoric
 *   despite increasingly employing behind-the-scenes content algorithms. The
 *   extractiveness has also increased (0.28 → 0.52) as platforms have layered
 *   additional data collection and engagement optimization on top of the
 *   baseline DLDR framework. The constraint is downstream of broader
 *   questions about platform accountability and the possibility of
 *   algorithmic curation that respects both user autonomy and harm
 *   prevention.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — shift curation and liability costs to readers while maintaining legal cover
 *   - Content Creators: Secondary beneficiary (moderate/constrained) — avoid responsibility for offensive material while retaining publication platform
 *   - Vulnerable Readers: Primary victim (powerless/trapped) — bear exposure costs without exit option or filtering capacity
 *   - Archive Epistemic Integrity: Primary victim (powerless/trapped) — abstract collective good that cannot organize; bears cost of unvetted content remaining in record
 *   - Digital Rights Coalition: Organized agents (organized/constrained) — advocates for user-controlled filtering without removal; see sunset pathway through technological maturation
 *   - Archival Institutions: Institutional actors (institutional/arbitrage) — invoke DLDR as preservation ethic while increasingly employing content warnings and curation in practice (piton characteristics)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing platform cost-shifting as immutable free speech principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dldr_information_policy, 0.52).
domain_priors:suppression_score(dldr_information_policy, 0.48).
domain_priors:theater_ratio(dldr_information_policy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dldr_information_policy, extractiveness, 0.52).
narrative_ontology:constraint_metric(dldr_information_policy, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dldr_information_policy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dldr_information_policy, tangled_rope).
narrative_ontology:human_readable(dldr_information_policy, "Don't Like, Don't Read (DLDR) Information Policy").
narrative_ontology:topic_domain(dldr_information_policy, "technological/social").

domain_priors:requires_active_enforcement(dldr_information_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dldr_information_policy, platform_operators).
narrative_ontology:constraint_beneficiary(dldr_information_policy, content_creators).
narrative_ontology:constraint_victim(dldr_information_policy, vulnerable_readers).
narrative_ontology:constraint_victim(dldr_information_policy, archive_epistemic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE READER (SNARE) — Unable to exit the archive system; bears the full cost of unfiltered or harmful content exposure. Has no alternative sources for the archived material and no mechanism to report or remove offensive content. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.88. Maximum extraction.
constraint_indexing:constraint_classification(dldr_information_policy, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ARCHIVE EPISTEMIC INTEGRITY (SNARE) — Abstract collective good that cannot organize or exit. Bears cost of unvetted content remaining in historical record without context or curation. Lacks mechanism to enforce standards or correct the record. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.88.
constraint_indexing:constraint_classification(dldr_information_policy, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CONTENT CREATOR (TANGLED ROPE) — Benefits from DLDR by avoiding responsibility for offensive material while retaining publication rights. Constrained by inability to edit or remove past works; must accept that readers will encounter their content unfiltered. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.35. Mixed: benefits from coordination (preservation) but also from extraction escape (no curation burden).
constraint_indexing:constraint_classification(dldr_information_policy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Primary beneficiary. DLDR transfers curation costs to readers, reducing platform operational expenses and legal liability. Frames this as reader autonomy and freedom. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.06. Net beneficiary; sees constraint as pure coordination.
constraint_indexing:constraint_classification(dldr_information_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DIGITAL RIGHTS COALITION (SCAFFOLD) — Organized advocates (content moderation nonprofits, platform transparency organizations) see DLDR as a temporary coordination gap that can be solved through better design: machine-learning tagging, user-controlled filtering without removing content, transparent archival standards. d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.25. Sunset mechanism: as filtering technology matures and platform accountability norms strengthen, DLDR gives way to hybrid models (content visible but tagged, filtered by user choice, with platform responsibility).
constraint_indexing:constraint_classification(dldr_information_policy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ARCHIVAL INSTITUTION (PITON) — Traditional libraries and historical societies invoke DLDR as preservation ethic ('we don't curate, we preserve') but increasingly recognize this as performative. Institutional archives increasingly employ content warnings, subject tagging, and context notes despite invoking DLDR principle. theater_ratio=0.58 approaches piton threshold; historical archival institutions are degraded copies of their stated ideal (pure preservation) maintained through institutional inertia.
constraint_indexing:constraint_classification(dldr_information_policy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FREE SPEECH VIEW (MOUNTAIN) — From a civilizational perspective, DLDR can be framed as an immutable principle: readers have always had the right to choose what they read, and this is a natural extension of that freedom. Filtering is responsibility of the consumer, not the archive. However, this naturalizes what is actually a contingent institutional arrangement: the shift of curation costs onto readers during the digital transition is a policy choice, not a law of nature.
constraint_indexing:constraint_classification(dldr_information_policy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dldr_information_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dldr_information_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dldr_information_policy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dldr_information_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dldr_information_policy, TR),
    TR >= 0.70.

:- end_tests(dldr_information_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. DLDR transfers significant curation labor and harm exposure costs to readers, disproportionately affecting vulnerable populations. The extraction is not total (reading is voluntary in principle, though platform switching costs are high). The trajectory from 0.28 → 0.52 reflects that platforms have increasingly weaponized DLDR rhetoric to justify minimizing human moderation despite possessing technological capacity for filtering. Suppression (0.48): Moderate. Readers technically have choice to avoid content, but the suppression is real: platforms provide no tagging, algorithmic filtering, or user-controlled visibility controls; readers must either consume all content or exit the platform entirely. Content creators and platform operators have suppressed alternative feedback loops (reader reporting, community curation). Theater ratio (0.58): Moderate-high. Platforms increasingly invoke DLDR as ethical principle (neutrality, freedom) while simultaneously employing algorithmic curation, engagement optimization, and content ranking that violates DLDR's stated principle. The theater has increased as the gap between stated policy (neutrality) and actual practice (algorithmic filtering) has widened. The 0.58 value reflects that a meaningful portion of platform behavior is performative neutrality claim rather than functional preservation.
 *
 * PERSPECTIVAL GAP:
 *   DLDR demonstrates a perspectival inversion typical of tangled rope constraints. The platform operator sees pure coordination (Rope) — solving the legitimate problem of preserving diverse content without censorship. The vulnerable reader sees pure extraction (Snare) — forced exposure to harmful content without exit. The content creator sees mixed experience (Tangled Rope) — benefits from publication platform but constrained by inability to edit or revise. The digital rights coalition sees a temporary problem with a sunset (Scaffold) — better filtering technology and platform accountability norms can replace DLDR within a generational timeframe. The archival institution sees its own degraded practice (Piton) — invoking DLDR as principle while increasingly employing content warnings and curation. The free speech absolutist observer risks seeing an immutable law (Mountain) — reader choice is natural and inalienable — but the structural data reveals this as a false summit: the cost-shifting to readers is contingent on platform business models that prioritize engagement and liability minimization over user welfare.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary; extraction is real but framed as enabling freedom. Content creators: Beneficiary + constrained → d≈0.55, f(d)≈0.75. Mixed: benefit from publication platform but constrained by inability to curate their own archive. Vulnerable readers: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction; no exit option and bear full harm cost. Archive epistemic integrity: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction; abstract victim. Digital rights coalition: Organized + constrained → d≈0.40, f(d)≈0.40. Low effective extraction; coalition has agency and sees path forward (technological sunset). Archival institutions: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification emerges from theater gate (0.58 approaches piton threshold) and from observed gap between stated principle and actual practice. Free speech observer: analytical → d≈0.72, f(d)≈1.15. Mountain perspective is perspectival naturalizing; false summit detector should flag.
 *
 * MANDATROPHY ANALYSIS:
 *   DLDR resolves the mandatrophy by revealing the structural opposition between platform efficiency and reader welfare. The tangled rope classification captures that this is neither pure coordination (platform cost reduction is real extraction, not shared benefit) nor pure extraction (there is a genuine coordination function: preserving diverse viewpoints without censorship). The mandatrophy is resolved by recognizing that the beneficiaries (platform, content creators) experience the constraint as coordination (Rope), while victims (vulnerable readers, epistemic integrity) experience it as extraction (Snare), and the gap reflects real structural asymmetry: platforms have exit options (could implement filtering) while vulnerable readers do not. The Scaffold perspective (digital rights coalition) points toward resolution: user-controlled filtering without removal, transparent curation standards, and algorithmic accountability would preserve the coordination function while eliminating the extraction. The false summit (free speech mountain) is caught by noting that the principle 'readers choose what they read' naturalizes a contingent platform design choice: platforms could provide filtering and users would still choose; DLDR is not natural law but policy architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_threshold_definition,
    'What threshold of content harm justifies platform intervention, and who decides?',
    'Cross-cultural comparative analysis of legal standards for harmful content; correlation between platform intervention and actual reader harm outcomes vs avoided harm',
    'If threshold is high (extreme violence only): DLDR is valid coordination. If threshold is low (offensive language): DLDR becomes extraction mechanism, constraint reclassifies as Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_threshold_definition, conceptual, 'Definition of harm threshold for content intervention').

omega_variable(
    vulnerability_access_tradeoff,
    'Does DLDR genuinely protect vulnerable readers'' autonomy, or does it systematize their exposure to harmful content?',
    'Empirical study of vulnerable population content consumption patterns on DLDR-governed platforms vs platforms with content filtering; measurement of harms reported by vulnerable users',
    'If autonomy protection is real: Tangled Rope classification confirmed. If vulnerability is systematized: reclassifies as Snare from vulnerable reader perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_access_tradeoff, empirical, 'Whether DLDR protects or exposes vulnerable readers').

omega_variable(
    platform_liability_shift,
    'Does DLDR shift legal/moral liability from platform to reader, or does it create a false liability division where platform remains accountable?',
    'Legal analysis of DLDR''s actual effect on platform liability in major jurisdictions; comparison of outcomes in litigation where DLDR was invoked as defense vs rejected',
    'If liability actually shifts: DLDR is functional coordination. If liability remains with platform: DLDR is performative liability laundering (strengthens piton classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_liability_shift, empirical, 'Whether DLDR actually shifts liability or merely performs it').

omega_variable(
    technological_alternative_availability,
    'Do mature content-filtering technologies exist that could replace DLDR without censorship, and why hasn''t the industry adopted them?',
    'Inventory of deployed filtering technologies in major platforms; analysis of adoption barriers (cost, liability exposure, design complexity, cultural resistance)',
    'If alternatives exist and are withheld for cost/liability reasons: extraction mechanism confirmed (Snare). If alternatives are genuinely immature: DLDR is legitimate coordination gap (Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_alternative_availability, empirical, 'Availability and adoption barriers of content-filtering alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dldr_information_policy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dldr_tr_t0, dldr_information_policy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dldr_tr_t5, dldr_information_policy, theater_ratio, 5, 0.45).
narrative_ontology:measurement(dldr_tr_t10, dldr_information_policy, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(dldr_be_t0, dldr_information_policy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dldr_be_t5, dldr_information_policy, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(dldr_be_t10, dldr_information_policy, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dldr_information_policy, information_standard).
narrative_ontology:affects_constraint(dldr_information_policy, platform_content_moderation).
narrative_ontology:affects_constraint(dldr_information_policy, reader_autonomy_vs_protection).
narrative_ontology:affects_constraint(dldr_information_policy, archive_preservation_standards).

% DUAL FORMULATION NOTE:
% DLDR as information policy is downstream of broader questions about platform accountability and algorithmic curation. Upstream constraints (platform_content_moderation, archive_preservation_standards) have their own ε values reflecting the technical and institutional feasibility of alternatives; DLDR has ε=0.52 reflecting the extraction that emerges when these constraints are not resolved.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dldr_information_policy, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
