% ============================================================================
% CONSTRAINT STORY: access_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_access_arbitrage, []).

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
 *   constraint_id: access_arbitrage
 *   human_readable: Access Arbitrage: Reporters Pay in Framing for Institutional Access
 *   domain: media/political_science
 *
 * SUMMARY:
 *   This constraint models the dynamic of 'access journalism,' where
 *   reporters and news organizations require access to elite institutions
 *   (e.g., The White House, The Fed) to function. To maintain this access,
 *   they face structural pressure to frame coverage in ways that are not
 *   overly critical of the institution. This creates a quid pro quo, where
 *   the 'payment' for access is a reduction in journalistic adversarialism.
 *   The system has a genuine coordination function—disseminating official
 *   information to the public—but it is coupled with an extractive function:
 *   the institution gains narrative control at the expense of public
 *   accountability.
 *
 * KEY AGENTS:
 *   - Elite Institutions: Primary beneficiary (institutional/arbitrage) — Gatekeepers of access who benefit from favorable narrative framing.
 *   - The Public: Primary victim (powerless/trapped) — Receives filtered information, leading to a deficit in democratic accountability.
 *   - Access Journalists / News Organizations: Hybrid role (organized/constrained) — Benefit from the prestige and content that access provides, but are victims of the pressure to self-censor or soften coverage.
 *   - Journalistic Integrity: Abstract victim — The core principle of holding power to account is structurally undermined.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(access_arbitrage, 0.6).
domain_priors:suppression_score(access_arbitrage, 0.75).
domain_priors:theater_ratio(access_arbitrage, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(access_arbitrage, extractiveness, 0.6).
narrative_ontology:constraint_metric(access_arbitrage, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(access_arbitrage, theater_ratio, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(access_arbitrage, tangled_rope).
narrative_ontology:human_readable(access_arbitrage, "Access Arbitrage: Reporters Pay in Framing for Institutional Access").
narrative_ontology:topic_domain(access_arbitrage, "media/political_science").

domain_priors:requires_active_enforcement(access_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(access_arbitrage, elite_institutions).
narrative_ontology:constraint_victim(access_arbitrage, the_public).
narrative_ontology:constraint_victim(access_arbitrage, journalistic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PUBLIC (SNARE) — Receives information filtered through the access constraint. Lacks viable alternatives for high-level institutional news and cannot exit the information ecosystem. Bears the full cost of diminished accountability. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.85.
constraint_indexing:constraint_classification(access_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE ELITE INSTITUTION (ROPE) — Experiences the constraint as a pure coordination tool for message dissemination. Can grant or revoke access to different reporters/outlets at will (arbitrage). Benefits from the favorable framing. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07. Negative extraction indicates a net subsidy.
constraint_indexing:constraint_classification(access_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ACCESS JOURNALIST (TANGLED ROPE) — Simultaneously benefits from exclusive access (coordination) and pays a cost in journalistic autonomy (extraction). Exit is possible but career-damaging (constrained). Experiences the system as a necessary, coercive bargain. d≈0.60, f(d)≈0.85, σ=1.0 → χ≈0.51.
constraint_indexing:constraint_classification(access_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the genuine coordination function (information dissemination) and the severe, asymmetric extraction (loss of critical framing). The high suppression and extraction values confirm a hybrid classification. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.83.
constraint_indexing:constraint_classification(access_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(access_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(access_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(access_arbitrage, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(access_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(access_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.60) is high, representing the significant value of narrative control transferred from the public interest to the institution. Suppression (0.75) is also high, as institutions can and do punish critical outlets by revoking access, a severe penalty for a major news organization. The theater ratio (0.50) reflects the performative nature of many institutional press events (e.g., press briefings), which often function more as message control sessions than genuine forums for accountability. The system requires active enforcement by institutional press offices who cultivate relationships and manage access.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark. The institution sees a simple coordination mechanism (Rope) for managing its public communications. The public, unable to exit the system and bearing the full cost of weakened oversight, experiences a Snare. The journalist, caught in the middle, sees the reality of the coercive bargain: a Tangled Rope that provides career-sustaining benefits (access) while extracting a professional cost (autonomy).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural positions. The Elite Institution is a clear beneficiary with arbitrage exit options, yielding a low 'd' value and negative effective extraction (Rope). The Public is a victim with trapped exit options, yielding a high 'd' value and high positive extraction (Snare). The Access Journalist is both a beneficiary (of access) and a victim (of compromised integrity) with constrained exit, placing them in the middle and resulting in a Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a canonical example of a Tangled Rope, resolving a potential mandatrophy. A naive analysis might label the system a pure Snare, ignoring the real coordination value that access provides. Conversely, the institutional narrative would frame it as a pure Rope, ignoring the coercive extraction of favorable coverage. The Tangled Rope classification correctly identifies the hybrid nature of the constraint, acknowledging both its function and its asymmetric cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_structure,
    'Is the favorable framing a conscious, quid-pro-quo decision by reporters, or an emergent structural effect of source cultivation and cognitive capture?',
    'Ethnographic studies of newsrooms, off-the-record interviews with journalists, and analysis of career trajectories based on coverage tone.',
    'If conscious, the constraint is a clearer Snare from the journalist''s perspective. If emergent, it solidifies the Tangled Rope classification, highlighting the system''s coercive nature over individual intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_structure, empirical, 'Distinguishing between conscious intent and emergent structural effects in journalistic framing.').

omega_variable(
    public_harm_quantification,
    'How can the harm to public discourse and democratic accountability (the extraction) be empirically measured?',
    'Comparative content analysis of access-driven vs. independent reporting on the same events, correlated with public opinion shifts and measures of institutional trust.',
    'Strong empirical evidence of harm would increase the base extractiveness (ε) score, potentially shifting the analytical classification from Tangled Rope to Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_harm_quantification, empirical, 'Quantifying the negative externalities of access journalism on the public.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(access_arbitrage, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1980, access_arbitrage, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(acce_tr_t2000, access_arbitrage, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(acce_tr_t2020, access_arbitrage, theater_ratio, 2020, 0.5).

% Extraction over time
narrative_ontology:measurement(acce_be_t1980, access_arbitrage, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(acce_be_t2000, access_arbitrage, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(acce_be_t2020, access_arbitrage, base_extractiveness, 2020, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(access_arbitrage, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
