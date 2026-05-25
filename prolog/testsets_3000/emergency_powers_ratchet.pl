% ============================================================================
% CONSTRAINT STORY: emergency_powers_ratchet
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emergency_powers_ratchet, []).

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
 *   constraint_id: emergency_powers_ratchet
 *   human_readable: The Permanent Crisis Scaffold
 *   domain: political/legal/social
 *
 * SUMMARY:
 *   The emergency powers ratchet is a systemic pattern where temporary legal
 *   authorities enacted during acute crises (war, pandemic, economic
 *   collapse, terrorism threat) persist and become normalized as permanent
 *   fixtures of governance. The constraint exhibits the classic structure of
 *   tangled rope: it solves a genuine collective action problem (crisis
 *   response requires speed and coordination unavailable through normal
 *   legislative process) while simultaneously enabling asymmetric extraction
 *   (executive authority expands permanently, legislative oversight remains
 *   constrained, civil liberties undergo structural degradation). The theater
 *   ratio has increased from 0.35 to 0.76 over the interval, reflecting that
 *   legal review mechanisms (court hearings, legislative certifications,
 *   constitutional amendments) have become increasingly performative — the
 *   formal rituals of oversight persist and elaborate while actual brake
 *   mechanisms atrophy. Extractiveness has ratcheted from 0.28 to 0.58,
 *   indicating that the initial emergency response (genuinely temporary,
 *   genuinely limited) has calcified into normalized executive authority. The
 *   constraint lacks a true sunset clause: nominal expiration dates are
 *   routinely renewed by automatic extension, legislative inattention, or
 *   redeclaration of emergency conditions. The mandate paradox here is
 *   whether the constraint is a scaffold (temporary structure with a real
 *   exit mechanism yet to be built) or a snare (temporary label concealing
 *   permanent extraction). The empirical resolution depends on whether sunset
 *   mechanisms are actually enforced and whether judicial deference to
 *   executive judgment returns to baseline post-crisis. Early data suggests
 *   degraded functionality: sunset clauses are renewed automatically, courts
 *   maintain elevated deference indefinitely, and the theater ratio is rising
 *   rather than declining.
 *
 * KEY AGENTS:
 *   - Executive Authority: Primary beneficiary (institutional/arbitrage) — captures expanded unilateral power during crisis; controls renewal/termination mechanism; benefits from coordination efficiency during acute phase but extracts permanently
 *   - Security Apparatus: Structural beneficiary (institutional/arbitrage) — surveillance, detention, and operational authorities granted during emergency persist indefinitely; gains permanent mandate expansion
 *   - Legislative Body: Mixed victim and weak beneficiary (organized/constrained) — constrained during active emergency (often prorogued or bypassed); benefits minimally from coordination (executives act unilaterally); loses oversight capacity
 *   - Civil Liberties Community: Primary victim (moderate/constrained) — faces normalized restrictions (surveillance, movement controls, detention authority); constrained by emergency legal framework; cannot exit or reorganize within compromised legal system
 *   - Constitutional Court: Institutional actor playing piton role (institutional/arbitrage) — maintains performative review (elaborate legal doctrine, formal hearings) but exercises minimal actual brake; deference to executive becomes permanent
 *   - Organized Reform Coalition: Organized agent (organized/mobile) — civil liberties organizations, constitutional scholars, good-governance NGOs building alternative pathways (supermajority requirements, mandatory sunset votes, citizen review boards); sees constraint as temporary design failure with real exit strategy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emergency_powers_ratchet, 0.58).
domain_priors:suppression_score(emergency_powers_ratchet, 0.68).
domain_priors:theater_ratio(emergency_powers_ratchet, 0.76).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emergency_powers_ratchet, extractiveness, 0.58).
narrative_ontology:constraint_metric(emergency_powers_ratchet, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(emergency_powers_ratchet, theater_ratio, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emergency_powers_ratchet, tangled_rope).
narrative_ontology:human_readable(emergency_powers_ratchet, "The Permanent Crisis Scaffold").
narrative_ontology:topic_domain(emergency_powers_ratchet, "political/legal/social").

domain_priors:requires_active_enforcement(emergency_powers_ratchet).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emergency_powers_ratchet, executive_authority).
narrative_ontology:constraint_beneficiary(emergency_powers_ratchet, security_apparatus).
narrative_ontology:constraint_beneficiary(emergency_powers_ratchet, administrative_agencies).
narrative_ontology:constraint_victim(emergency_powers_ratchet, constrained_civil_liberties).
narrative_ontology:constraint_victim(emergency_powers_ratchet, legislative_oversight_capacity).
narrative_ontology:constraint_victim(emergency_powers_ratchet, judicial_review_mechanisms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED CITIZEN (SNARE) — Subject to emergency powers with no effective exit. Once normalized, emergency restrictions (surveillance, detention authority, curfews, movement controls) become permanent features of daily life. The citizen cannot reorganize or challenge the constraint within the legal system it has reshaped. Maximum extraction experienced — trapped exit, powerless position.
constraint_indexing:constraint_classification(emergency_powers_ratchet, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPPOSITION POLITICIAN (TANGLED ROPE) — Constrained by emergency legal framework but also benefits from the coordination function: emergency powers allow rapid legislative action on genuine crises (pandemic, war), creating efficiency gains that even opposition figures rely on during acute emergencies. Yet extraction persists because the beneficiary (executive) controls the sunset mechanism. Mixed experience: genuine coordination need during crisis, but asymmetric control over termination.
constraint_indexing:constraint_classification(emergency_powers_ratchet, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE AUTHORITY (ROPE) — Experiences emergency powers as pure coordination: enables rapid response to genuine crises without legislative gridlock. During the acute phase, the executive uses arbitrage options (can invoke alternatives like martial law, can dissolve legislature in extremis). The constraint solves the executive's collective action problem. Net beneficiary with exit — experiences low effective extraction.
constraint_indexing:constraint_classification(emergency_powers_ratchet, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM MOVEMENT (SCAFFOLD) — Organized agents (civil liberties organizations, constitutional scholars, reform coalitions) see emergency powers as a temporary structural failure with a visible sunset mechanism. Constitutional amendments, sunset clauses with mandatory review periods, and supermajority requirements for extension are all designed to create exit pathways. Low effective extraction for organized agents with coherent exit strategy and generational time horizon. Theater is high (ritual reviews, formal hearings) but declining as norms mature.
constraint_indexing:constraint_classification(emergency_powers_ratchet, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGAL JUSTIFICATION SYSTEM (PITON) — Courts and legal doctrine maintain a performative review of emergency powers through habeas corpus, proportionality analysis, and necessity tests. But the actual check has degraded: courts consistently defer to executive threat assessments, rubber-stamp extensions, and apply minimal scrutiny. The legal review ritual persists through institutional inertia despite low functional capacity. Theater ratio very high (elaborate legal arguments, formal record creation) with minimal actual oversight. The constraint exists because the justification system hasn't been replaced, not because it functions.
constraint_indexing:constraint_classification(emergency_powers_ratchet, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilization-scale temporal view, emergency powers might appear as an immutable feature of governance: crises are recurring, executives always need rapid response authority, and the ratcheting mechanism is 'natural' to political structures. However, this perspective risks naturalizing what is actually a contingent institutional design failure. The false summit detector should flag this: the ratchet is not an inherent law of political thermodynamics but an outcome of specific legal doctrines and institutional incentives.
constraint_indexing:constraint_classification(emergency_powers_ratchet, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_powers_ratchet_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emergency_powers_ratchet, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_powers_ratchet, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(emergency_powers_ratchet, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(emergency_powers_ratchet, TR),
    TR >= 0.70.

:- end_tests(emergency_powers_ratchet_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits real extraction: executive authority expands, legislative oversight degrades, civil liberties become constrained. But the extraction is not maximal (0.70+) because genuine coordination benefits exist during the acute crisis phase, and some democracies have successfully constrained emergency power ratcheting through institutional design. The 0.58 value reflects that the extraction is substantial and persistent but not totalizing — organized agents can still mobilize for constitutional reform, courts retain nominal review authority, and some emergency powers do eventually expire. Suppression (0.68): Moderate-high. Multiple barriers prevent exit: legal framework restricts challenge mechanisms, courts defer to executive judgment, renewal happens through legislative inattention rather than active deliberation, and prolonged emergency eventually normalizes restrictions as baseline conditions. However, suppression is not maximal (0.85+) because constitutional amendment remains formally available, organized reform coalitions do exist, and some polities have successfully imposed hard sunset requirements. Theater ratio (0.76): High. Legal review mechanisms are substantially performative: constitutional court hearings generate elaborate proportionality doctrine while consistently approving extensions; legislative review votes on emergency renewal happen after de facto authorization has already proceeded; sunset clauses are drafted as if meaningful while typically renewed through automatic extension procedures. The ratio has increased over the interval as the gap between formal oversight complexity and actual brake strength has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a sharp perspectival divide along power axis and exit options. The executive authority (institutional/arbitrage) perceives pure coordination during acute phase and maintains arbitrage options to pivot to alternatives (martial law, constitutional amendment). The opposition politician (moderate/constrained) perceives genuine mixed coordination need during crisis but also extraction from controlled sunset mechanism. The constrained citizen (powerless/trapped) perceives only extraction — emergency restrictions become their permanent legal reality with no means of challenge. The organized reform coalition (organized/mobile) perceives a temporary institutional design failure with a visible exit pathway (constitutional amendment, supermajority requirements, mandatory citizen review). The legal justification system (institutional/arbitrage) perceives its role as coordination enabler but functions as extraction cover. The analytical observer risks naturalizing the ratchet as inherent to political systems. The perspectival gaps reflect real structural differences in power, exit options, and temporal horizons — not subjective disagreement about the same facts but different agents experiencing measurably different constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from power level, exit options, and structural relationship to the constraint. The executive authority benefits from the constraint and has arbitrage exit options (can declare martial law, can invoke other emergency mechanisms) — low d, negative chi. The opposition politician is constrained by the framework but benefits from crisis coordination during acute emergencies — moderate d, moderate chi. The citizen has no exit (trapped) and bears the full cost of normalized restrictions — high d, high chi. The organized reform coalition has mobile exit options (can mobilize for constitutional amendment, can leverage public opinion) — moderate-low d. The legal system appears to benefit (preserves judiciary role, enables coordination) but actually experiences extraction through institutional capture (deference becomes permanent, oversight capacity atrophies) — moderate-high d despite nominal beneficiary position. The piton classification for the legal system reflects that the constraint extracts (degrades actual review capacity) while the system appears to benefit (maintains formal authority). The mountain perspective risks d=0.72 (analytical observer position) but the engine's false summit detector should identify this as naturalization of contingent institutional design.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunset_enforceability,
    'Are formal sunset clauses and mandatory review requirements actually enforced by legislatures, or do they function as performative theater while de facto permanent powers persist?',
    'Historical analysis of emergency power expiration: What percentage of emergency powers with sunset clauses are actually allowed to expire without automatic renewal? Comparison with jurisdictions lacking sunset mechanisms.',
    'If enforced: scaffold perspective valid — organized agents have real exit mechanism. If theater: sunset is performative ratchet cover, reclassifying constraint from scaffold toward snare. Changes whether the constraint has genuine termination pathway.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_enforceability, empirical, 'Enforceability of legal sunset clauses on emergency powers').

omega_variable(
    crisis_frequency_threshold,
    'At what frequency does ''temporary emergency'' become ''permanent conditional authority''? Is there an empirical threshold where continuous crisis invocation transforms the legal status of emergency powers?',
    'Longitudinal analysis of crisis intervals: If crises occur > 1 per decade, does legal doctrine shift to normalize emergency powers? Comparative institutional analysis across polities with different crisis frequencies.',
    'If threshold exists below current crisis frequency: emergency powers are structurally permanent in modern governance. If threshold is theoretical: crisis frequency variation doesn''t determine permanence — institutional design does.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_frequency_threshold, empirical, 'Frequency threshold converting temporary emergency powers to permanent authority').

omega_variable(
    deferential_review_necessity,
    'Does judicial deference to executive threat assessment during crises serve a genuine coordination function (courts lack information to second-guess security judgments) or does it mask extraction (deference becomes permanent even post-crisis)?',
    'Comparative analysis of court review intensity during vs after crisis declaration. Do strictness of judicial review return to baseline once emergency officially ends? Track proportionality doctrine application before/during/after emergency invocation.',
    'If genuine necessity: coordination explanation valid, court deference is rational institutional specialization. If mask for extraction: deference never resets, making ''emergency'' review the new baseline — reclassifies constraint as snare with legal camouflage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deferential_review_necessity, empirical, 'Whether judicial deference during emergencies returns to baseline post-crisis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emergency_powers_ratchet, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emerg_tr_t0, emergency_powers_ratchet, theater_ratio, 0, 0.35).
narrative_ontology:measurement(emerg_tr_t5, emergency_powers_ratchet, theater_ratio, 5, 0.58).
narrative_ontology:measurement(emerg_tr_t10, emergency_powers_ratchet, theater_ratio, 10, 0.76).

% Extraction over time
narrative_ontology:measurement(emerg_be_t0, emergency_powers_ratchet, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(emerg_be_t5, emergency_powers_ratchet, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(emerg_be_t10, emergency_powers_ratchet, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emergency_powers_ratchet, enforcement_mechanism).
narrative_ontology:affects_constraint(emergency_powers_ratchet, legislative_gridlock_snare).
narrative_ontology:affects_constraint(emergency_powers_ratchet, surveillance_normalization).
narrative_ontology:affects_constraint(emergency_powers_ratchet, martial_law_precedent).

% DUAL FORMULATION NOTE:
% The emergency powers ratchet can be decomposed into distinct constraints: (1) the acute crisis coordination problem (genuine temporary need for rapid executive action, ε≈0.20, Rope); (2) the institutional ratchet mechanism (structural tendency for temporary authorities to persist, ε≈0.58, Tangled Rope); (3) the legal justification theater (court deference becoming permanent, ε≈0.65, Piton). These three stories are linked: the first creates the legitimacy for the second, which uses the third as its enforcement mechanism. The present story addresses constraint #2 (the ratchet mechanism itself) at the institutional level of analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(emergency_powers_ratchet, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
