% ============================================================================
% CONSTRAINT STORY: moltbook_breach_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_moltbook_breach_2026, []).

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
 *   constraint_id: moltbook_breach_2026
 *   human_readable: The Moltbook Database Exposure
 *   domain: technological/social
 *
 * SUMMARY:
 *   The Moltbook database exposure represents a structural snare where
 *   platform dominance enables Moltbook to extract value from its own breach
 *   while users bear the privacy cost permanently. The constraint is not
 *   merely the technical exposure but the lock-in mechanism that prevents
 *   exit as a remedial response. Users have no practical alternative to
 *   Moltbook despite the breach because the network effects and feature
 *   integration make switching costs prohibitive. This creates a pathological
 *   situation where the primary victim (user privacy) has no exit option, the
 *   secondary victim (regulatory authority) has constrained alternatives, and
 *   the primary beneficiary (Moltbook) retains arbitrage optionality through
 *   reputation recovery and lobbying. The theater ratio has increased from
 *   0.38 to 0.55 as the incident response focused on performative compliance
 *   (regulatory settlements, third-party audits, policy updates) while the
 *   underlying architectural vulnerability remains. Extractiveness has
 *   increased from 0.42 to 0.58 as the full scope of data compromise became
 *   apparent and as the regulatory response revealed itself to be constrained
 *   by Moltbook's prior regulatory relationships.
 *
 * KEY AGENTS:
 *   - Exposed Users: Primary victims (powerless/trapped) — bear permanent privacy loss with no exit mechanism, biographical timescale exposure risk
 *   - Moltbook Platform: Primary beneficiary-extractor (institutional/arbitrage) — maintains platform dominance despite breach, extracts value from lock-in, retains reputation recovery optionality
 *   - Regulatory Authorities: Secondary victim (moderate/constrained) — must pursue enforcement while constrained by jurisdiction, investigation pace, and platform dominance; face suppressed alternatives (real-time audit, immediate deletion, binding penalties)
 *   - Data Protection Coalition: Organized advocate (organized/constrained) — leverage through litigation and norm-setting but constrained by funding and industry-funded counter-messaging
 *   - Security-Theater Complex: Institutional actor (institutional/arbitrage) — incident response services, auditors, consultants benefit from breach response; perpetuate performative protocols
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — reveals that platform consolidation has created structural vulnerability at civilization scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(moltbook_breach_2026, 0.58).
domain_priors:suppression_score(moltbook_breach_2026, 0.68).
domain_priors:theater_ratio(moltbook_breach_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(moltbook_breach_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(moltbook_breach_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(moltbook_breach_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(moltbook_breach_2026, snare).
narrative_ontology:human_readable(moltbook_breach_2026, "The Moltbook Database Exposure").
narrative_ontology:topic_domain(moltbook_breach_2026, "technological/social").

domain_priors:requires_active_enforcement(moltbook_breach_2026).
% --- Structural relationships ---
narrative_ontology:constraint_victim(moltbook_breach_2026, user_privacy).
narrative_ontology:constraint_victim(moltbook_breach_2026, agent_autonomy).
narrative_ontology:constraint_victim(moltbook_breach_2026, platform_trust).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPOSED USER (SNARE) — Users have no meaningful exit: they cannot delete their data retroactively, cannot prevent future exposure from cached copies, and face permanent risk of downstream identity exploitation. Trapped in a system where their biographical data is now permanently compromised. Zero agency, maximum extraction of privacy value.
constraint_indexing:constraint_classification(moltbook_breach_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGULATORY AUTHORITY (SNARE) — Constrained by jurisdiction limits, slow investigation timelines, and Moltbook's platform dominance. Must pursue enforcement while the company controls its own forensic narrative. Suppressed alternatives: real-time audit access, immediate data deletion mandates, or meaningful penalties that aren't settled quietly.
constraint_indexing:constraint_classification(moltbook_breach_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MOLTBOOK PLATFORM (SNARE) — Paradoxically, Moltbook extracts value from its own exposure: 1) reputational cost is diffuse and recoverable (scandal fatigue); 2) data breach creates dependency for forensic services and security vendors; 3) threat of regulation is negotiable via lobbying; 4) user lock-in is so high that churn is minimal. Arbitrage options preserve exit value despite reputational damage.
constraint_indexing:constraint_classification(moltbook_breach_2026, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DATA PROTECTION COALITION (TANGLED ROPE) — Civil society, advocacy groups, and privacy advocates have genuine leverage through litigation, regulatory testimony, and norm-setting (EU GDPR enforcement, US state-level privacy bills). But they are constrained by funding and legal access. The coalition both forces coordination (privacy-by-design mandates) and bears extraction (excludes from industry funding, faces industry-funded counter-messaging). Mixed extraction and coordination burden.
constraint_indexing:constraint_classification(moltbook_breach_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SECURITY-THEATER COMPLEX (PITON) — Incident response protocols, bug-bounty programs, and third-party audits are largely performative: they signal competence without preventing the structural exposure (backend database with insufficient access controls). The theater has become the primary function — regulatory compliance checkboxes, consultant fees, press release cycles. The underlying security architecture remains degraded, and the theater persists through institutional inertia and fee-for-service incentives.
constraint_indexing:constraint_classification(moltbook_breach_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational scale, the exposure reveals that platform consolidation has created a structural vulnerability: user dependency is so high that exit is not genuinely available even when breach occurs. The constraint is not the exposure itself but the lock-in that prevents migration as a response. Moltbook's dominance means the platform's exposure becomes civilization-scale risk that users cannot unilaterally resolve.
constraint_indexing:constraint_classification(moltbook_breach_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moltbook_breach_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(moltbook_breach_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moltbook_breach_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(moltbook_breach_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(moltbook_breach_2026, TR),
    TR >= 0.70.

:- end_tests(moltbook_breach_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The breach extracts permanent privacy value from users who cannot exit or recover. But Moltbook's extraction is not as severe as a debt-trap snare (0.70+) because the platform's value proposition still provides some coordination benefit (communication, information aggregation), and some users may accept the privacy-extraction tradeoff. The increase from 0.42 to 0.58 reflects mounting evidence of data compromise scope and regulatory response weakness. Suppression (0.68): High. Users cannot delete exposed data retroactively, cannot prevent cached copies, cannot switch platforms at reasonable cost, and cannot access their own forensic data (Moltbook controls the narrative). Regulatory alternatives are suppressed: no real-time audit access, no immediate deletion mandates, no penalties large enough to trigger behavior change. Theater ratio (0.55): Moderate-high. Incident response has become increasingly performative: regulatory settlements establish compliance checkboxes, third-party audits provide assurance theater, press releases signal commitment to users. The underlying architectural exposure (backend database with insufficient access controls) remains degraded. Theater has increased as response mechanisms have proliferated without addressing root cause.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the powerless user (snare from all angles) and the institutional platform (snare classification but with arbitrage that preserves extraction benefit). From the user's biographical perspective, the breach is catastrophic and permanent — powerless/trapped/snare. From Moltbook's institutional perspective, the breach is a reputational cost, a regulatory negotiation, and an opportunity to consolidate market position through larger competitors' withdrawal — snare classification but with asymmetric extraction that favors Moltbook. The regulatory authority and data protection coalition occupy intermediate positions: they have organized leverage but are constrained by jurisdiction, legal pace, and industry dominance. The security-theater perspective reveals that incident response has become decoupled from actual security improvement — performative protocols perpetuate while architectural vulnerability persists. The analytical perspective reveals that platform consolidation itself is the deeper constraint — users cannot exit even when exposed because the coordination benefits of Moltbook are too valuable relative to alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position and exit capacity. Exposed users (powerless/trapped) have d ≈ 0.95: they bear maximum extraction with zero exit options. Moltbook (institutional/arbitrage) has d ≈ 0.10: the platform is a net beneficiary of the constraint (lock-in value exceeds reputational cost) and retains arbitrage optionality (lobbying, reputation recovery, market consolidation). Regulatory authorities (moderate/constrained) have d ≈ 0.70: they are constrained victims with limited exit (must enforce despite resource limits and jurisdiction boundaries). Data protection coalition (organized/constrained) has d ≈ 0.50: mixed burden and leverage — constrained funding but genuine litigation and norm-setting power. Security-theater complex (institutional/arbitrage) has d ≈ 0.15: net beneficiary of the incident response (consulting fees, audit contracts) despite ostensible victim status. The analytical observer (analytical/analytical) has d ≈ 0.72: sees the full structure but has no enforcement power and faces reputational cost for criticizing dominant platform.
 *
 * MANDATROPHY ANALYSIS:
 *   The snare classification is strengthened across all high-extraction perspectives by the lock-in mechanism: users cannot exit as a response to breach (powerless), regulatory authorities cannot impose binding penalties (constrained), and the platform itself is the beneficiary of the constraint (arbitrage preserved). The tangled rope classification for the organized coalition reflects genuine tension between their leverage (litigation, norm-setting) and their constraints (funding, industry opposition) — they do provide a coordination function (privacy norms evolution) while bearing extraction (exclusion from industry funding, counter-messaging). The piton classification for the security-theater complex is correct: the theater has become the primary function (regulatory compliance, assurance signaling) while the underlying security architecture remains degraded. The analytical snare prevents mislabeling this as a coordination problem or temporary scaffold — the lock-in is structural, and the extraction benefits the platform permanently. No sunset clause is credible because Moltbook's market dominance makes exit impossible for users and alternative platforms implausible at civilizational scale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forensic_narrative_control,
    'Did Moltbook''s control of forensic investigation delay or distort the true scope of the exposure?',
    'Independent forensic audit comparing internal investigation timeline to external forensic reconstruction; analysis of deleted logs and access patterns before disclosure',
    'If true scope was significantly larger: extractiveness increases to 0.65+ (platform deliberately obscured). If scope matches disclosure: extractiveness holds at 0.58 (incompetence rather than malice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forensic_narrative_control, empirical, 'Whether Moltbook''s forensic control obscured the true breach scope').

omega_variable(
    regulatory_capture_depth,
    'To what extent did Moltbook''s prior regulatory relationships (lobbying, revolving door) determine the penalty and enforcement response?',
    'Analysis of FTC/state AG settlement terms vs precedent; comparison of penalties to similar-scale breaches at non-dominant platforms; tracking of post-breach regulatory staffing movements',
    'If high capture: suppression increases to 0.75+ (regulatory alternatives genuinely suppressed). If low capture: suppression reflects technical/resource barriers rather than corruption.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Extent of regulatory capture in Moltbook enforcement response').

omega_variable(
    lock_in_irreversibility,
    'Can users meaningfully exit Moltbook as a result of the breach, or is platform switching cost prohibitive even for breached cohorts?',
    'Cohort analysis of user retention/churn by data sensitivity; comparison of switching costs (data portability, network effects, feature lock-in) to breach severity; follow-up surveys on user exit intentions vs actual behavior',
    'If switching cost > breach cost: lock-in creates permanent snare (users remain despite exposure). If switching cost < breach cost: exit options become constrained rather than trapped, changing powerless agent to moderate classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lock_in_irreversibility, empirical, 'Whether user exit from Moltbook is reversible').

omega_variable(
    breach_prevention_technical_feasibility,
    'Was the exposure technically preventable with current security tools (access controls, encryption, segmentation) or does it reflect architectural choices prioritizing feature development over protection?',
    'Technical reconstruction: comparison of Moltbook''s database architecture to industry-standard hardening practices; identification of specific access control gaps; analysis of cost/effort required to prevent',
    'If preventable: extractiveness increases (deliberate risk-taking for speed/profit). If not preventable with current tech: snare classification may degrade to tangled_rope (system-wide coordination problem rather than pure extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(breach_prevention_technical_feasibility, empirical, 'Technical feasibility of preventing the database exposure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(moltbook_breach_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(molt_tr_t0, moltbook_breach_2026, theater_ratio, 0, 0.38).
narrative_ontology:measurement(molt_tr_t3, moltbook_breach_2026, theater_ratio, 3, 0.48).
narrative_ontology:measurement(molt_tr_t6, moltbook_breach_2026, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(molt_be_t0, moltbook_breach_2026, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(molt_be_t3, moltbook_breach_2026, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(molt_be_t6, moltbook_breach_2026, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(moltbook_breach_2026, information_standard).
narrative_ontology:affects_constraint(moltbook_breach_2026, platform_lock_in_extraction).
narrative_ontology:affects_constraint(moltbook_breach_2026, regulatory_capture_mechanisms).
narrative_ontology:affects_constraint(moltbook_breach_2026, security_theater_proliferation).

% DUAL FORMULATION NOTE:
% The Moltbook exposure decomposes into three related constraints: (1) the technical database exposure (incident-level, high but recoverable extractiveness); (2) the platform lock-in that prevents user exit even after breach (structural, permanent extractiveness); (3) the regulatory capture that constrains enforcement response (institutional, enabling continued extraction). Each story has distinct epsilon values reflecting empirical vs structural severity. The database exposure alone would be snare with lower extractiveness (0.45+); the lock-in mechanism increases extractiveness to 0.58+ by removing exit capacity. Network links enable contamination propagation analysis: if lock-in degrades, platform extractiveness decreases; if regulatory capture deepens, user exit becomes further suppressed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(moltbook_breach_2026, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
