% ============================================================================
% CONSTRAINT STORY: sotu_1965_johnson_federal_preclearance_litigation_bypass
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1965_johnson_federal_preclearance_litigation_bypass, []).

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
 *   constraint_id: sotu_1965_johnson_federal_preclearance_litigation_bypass
 *   human_readable: Federal Preclearance Litigation Bypass in Voting Rights Enforcement (1965)
 *   domain: governance/voting_rights
 *
 * SUMMARY:
 *   The federal preclearance mechanism established by the 1965 Voting Rights
 *   Act eliminates the structural inefficiency of requiring citizens to
 *   litigate voting denials through courts — a process that creates 3-7 year
 *   delays while discrimination continues. By enabling direct federal action
 *   to strike down discriminatory practices and implement voter registration
 *   without prolonged lawsuits, the mechanism coordinates voting access
 *   enforcement across jurisdictions with persistent non-compliance.
 *   Disenfranchised voters benefit from immediate federal intervention; state
 *   and local officials lose the ability to delay compliance through
 *   protracted litigation but retain a sunset path: achieving a clean
 *   compliance record allows bailout from preclearance. The constraint
 *   exhibits Rope classification — genuine coordination benefit without
 *   significant extraction toward federal authorities — but shows modest
 *   drift toward higher theater over time as the mechanism becomes
 *   institutionalized. The extractiveness increase from 0.08 to 0.28 reflects
 *   gradual scope expansion beyond immediate voter access into broader
 *   electoral administration, creating the omega for scope creep. The theater
 *   increase from 0.25 to 0.42 reflects that preclearance submissions become
 *   increasingly bureaucratic as officials learn to navigate approval
 *   pathways, adding procedural complexity without corresponding voter access
 *   gains.
 *
 * KEY AGENTS:
 *   - Disenfranchised Voters: Primary beneficiary (powerless/mobile) — gain immediate voting access without years of litigation delay
 *   - Federal Enforcement Apparatus: Institutional beneficiary (institutional/mobile) — gain direct authority to enforce voting access; experience low extraction
 *   - State and Local Election Officials: Temporary target (powerful/constrained) — lose litigation delay tactic; face sunset mechanism that provides exit path through compliance
 *   - Civil Rights Organizations: Organized beneficiary (organized/mobile) — gain low-cost monitoring and enforcement tool via federal preclearance petition
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent enforcement design as structural necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1965_johnson_federal_preclearance_litigation_bypass, 0.28).
domain_priors:suppression_score(sotu_1965_johnson_federal_preclearance_litigation_bypass, 0.38).
domain_priors:theater_ratio(sotu_1965_johnson_federal_preclearance_litigation_bypass, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1965_johnson_federal_preclearance_litigation_bypass, extractiveness, 0.28).
narrative_ontology:constraint_metric(sotu_1965_johnson_federal_preclearance_litigation_bypass, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(sotu_1965_johnson_federal_preclearance_litigation_bypass, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1965_johnson_federal_preclearance_litigation_bypass, rope).
narrative_ontology:human_readable(sotu_1965_johnson_federal_preclearance_litigation_bypass, "Federal Preclearance Litigation Bypass in Voting Rights Enforcement (1965)").
narrative_ontology:topic_domain(sotu_1965_johnson_federal_preclearance_litigation_bypass, "governance/voting_rights").

narrative_ontology:has_sunset_clause(sotu_1965_johnson_federal_preclearance_litigation_bypass).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1965_johnson_federal_preclearance_litigation_bypass, disenfranchised_voters).
narrative_ontology:constraint_beneficiary(sotu_1965_johnson_federal_preclearance_litigation_bypass, federal_enforcement_mechanism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED VOTER (ROPE) — Immediate access to voting without waiting 3-7 years for litigation to resolve. The preclearance mechanism solves a pure coordination problem: getting voters registered and protected from discriminatory practices. The voter's exit option is mobile (they can move to non-preclearance jurisdictions, though at high cost). Experienced extractiveness is low — the mechanism directly benefits this agent.
constraint_indexing:constraint_classification(sotu_1965_johnson_federal_preclearance_litigation_bypass, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: FEDERAL ENFORCEMENT APPARATUS (ROPE) — The preclearance requirement is a coordination mechanism that clarifies which voting practices can proceed vs. which require federal approval before implementation. No extraction occurs toward federal government — the mechanism is a coordination tool that solves the problem of synchronizing voting access across jurisdictions with asymmetric state compliance. Low effective extraction because enforcement agencies have mobile exit (enforcement funds and authority can be directed elsewhere) and genuine coordination benefit.
constraint_indexing:constraint_classification(sotu_1965_johnson_federal_preclearance_litigation_bypass, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE AND LOCAL ELECTION OFFICIALS (SCAFFOLD) — Temporarily constrained by preclearance requirement; cannot implement voting changes without federal approval. However, this perspective sees a sunset: as voting discrimination decreases and officials achieve compliance norms, preclearance requirements should expire. The constraint is temporary enforcement with a built-in exit path (compliance triggers removal). Extraction is moderate because officials retain some agency and can see a path to sunset.
constraint_indexing:constraint_classification(sotu_1965_johnson_federal_preclearance_litigation_bypass, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: CIVIL RIGHTS ORGANIZATIONS (ROPE) — The preclearance mechanism provides organized civil rights groups with a coordination tool: they can petition federal authorities directly rather than funding protracted litigation. Low-cost coordination for monitoring and enforcing voting access. Organizations have mobile exit and genuine benefit from the enforcement structure. Effective extraction is near zero.
constraint_indexing:constraint_classification(sotu_1965_johnson_federal_preclearance_litigation_bypass, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / STRUCTURAL VIEW (MOUNTAIN) — From a civilizational perspective, the preclearance mechanism solves a fundamental coordination problem: how to enforce constitutional voting rights across jurisdictions with persistent non-compliance. The underlying constraint it addresses — the constitutional guarantee of voting access — is a natural law (mountain). Preclearance is the enforcement mechanism that turns the natural law into operational reality. However, this perspective risks naturalizing what is actually a contingent institutional design choice.
constraint_indexing:constraint_classification(sotu_1965_johnson_federal_preclearance_litigation_bypass, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1965_johnson_federal_preclearance_litigation_bypass_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1965_johnson_federal_preclearance_litigation_bypass, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1965_johnson_federal_preclearance_litigation_bypass, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(sotu_1965_johnson_federal_preclearance_litigation_bypass_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The preclearance mechanism coordinates voting access by replacing slow litigation with faster federal administrative review. Initial extractiveness is minimal (0.08) — the mechanism directly solves the coordination problem without asymmetric benefits. However, extractiveness drifts upward over time as federal authorities expand preclearance scope beyond immediate voting access into broader electoral administration (redrawing, voter ID standards, polling location changes), creating secondary extraction not inherent to the coordination function. By 1995, some state officials and local communities experience preclearance as a broader constraint on electoral autonomy, not just voting access. Suppression (0.38): Moderate. State and local officials cannot delay through litigation, but they retain substantial agency — they can implement changes that pass preclearance, and they have a clear exit path (bailout through compliance). Suppression is not high because the constraint explicitly includes a sunset mechanism. Theater ratio (0.42): Moderate. Initial theater is low (0.25) — preclearance serves its primary function of faster enforcement. Over time, theater increases (0.42) as preclearance becomes bureaucratized: jurisdictions learn optimization strategies (minimal changes designed to pass review rather than substantive compliance), federal reviewers develop procedural templates, and the system becomes a ritualized exchange rather than substantive voting rights enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the disenfranchised voter (who sees immediate access and low extraction) and the state/local official (who sees temporary constraint with an exit path via compliance). The analytical observer risks collapsing this gap by naturalizing the preclearance mechanism as inherent to constitutional voting rights, when in fact preclearance is a contingent institutional design choice. An alternative mechanism — rapid court review with strict timelines — could achieve similar coordination benefits without federal administrative expansion. The gap between the immediate beneficiary perspective (low extraction, high benefit) and the bureaucratized institutional perspective (moderate extraction, high theater) emerges over time as preclearance becomes institutionalized. This gap is diagnostically important: if theater ratio continues rising toward 0.60+, the constraint drifts toward Piton (performative compliance without functional voting protection).
 *
 * DIRECTIONALITY LOGIC:
 *   The preclearance mechanism is fundamentally beneficiary-focused: it extracts nothing from federal authorities (they gain enforcement power, which they perceive as a benefit), and it solves a genuine coordination problem (how to protect voting access across non-compliant jurisdictions). State and local officials are the constrained parties, but they have mobile exit options (implement compliant practices, achieve bailout; alternatively, litigate to overturn preclearance, which some southern jurisdictions attempted). The beneficiary-only structure (no victim group declared) indicates a Rope classification, not Snare. The scaffold perspective captures the sunset logic: preclearance is designed to be temporary, expiring as compliance increases. The mountain perspective risks naturalizing a contingent institutional design (federal preclearance) as a necessary structural feature (constitutional voting rights). The directionality is clean: federal authorities and disenfranchised voters benefit; state/local officials lose a delay tactic but gain a exit path; no agent is trapped or extracted from without agency.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing the coordination function (solving the litigation delay problem) from the extraction question (does federal preclearance expand beyond voting access?). The Rope classification holds as long as preclearance scope remains limited to voting access enforcement and the sunset mechanism functions. The omega on scope creep is critical: if federal authorities use preclearance to control electoral administration beyond voting access, the constraint reclassifies toward Tangled Rope (coordination + extraction) or toward Snare (extraction masquerading as coordination). The omega on bailout mechanism is also critical: if preclearance persists indefinitely despite declining discrimination, the constraint becomes Piton (institutional inertia) or Tangled Rope (if persistence serves federal political leverage). The measured drift in extractiveness (0.08 → 0.28) and theater (0.25 → 0.42) provides empirical hooks for mandatrophy resolution: the system itself is signaling scope expansion and bureaucratization that may shift the constraint's character over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    litigation_delay_quantification,
    'What is the true distribution of litigation delays under the pre-preclearance system, and how much voter harm occurs during these delays?',
    'Historical case-by-case analysis of voting rights litigation timelines pre-1965 vs. post-1965; voter registration impact during delay periods; correlation between delay duration and total votes suppressed',
    'If delays average < 1 year: preclearance may be over-engineered for the coordination problem it solves. If delays average > 5 years: preclearance is solving a genuinely severe coordination failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(litigation_delay_quantification, empirical, 'Quantification of voter harm during litigation delays').

omega_variable(
    federal_approval_delay_substitution,
    'Does the preclearance approval process simply substitute federal administrative delays for state litigation delays, without net improvement in voter access speed?',
    'Time-series analysis of preclearance submission-to-approval intervals vs. pre-preclearance litigation timelines; voter registration speed under federal preclearance vs. presumed speed under hypothetical immediate court orders',
    'If federal approvals are faster: preclearance solves the coordination problem. If federal approvals are similarly slow: the constraint may be a Tangled Rope (coordination + extraction) rather than pure Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_approval_delay_substitution, empirical, 'Whether federal preclearance approval substitutes for court delays').

omega_variable(
    sunset_mechanism_robustness,
    'Is the preclearance sunset mechanism (removal when state achieves compliance track record) actually triggered, or does preclearance persist indefinitely even as discrimination rates decline?',
    'Longitudinal analysis of Voting Rights Act Section 5 bailout provisions; count of jurisdictions achieving preclearance bailout; correlation between discrimination metrics and bailout success',
    'If bailout is real and achievable: scaffold classification confirmed. If preclearance persists despite compliance: constraint reclassifies toward Piton (degraded institutional inertia) or toward Tangled Rope if federal authorities use preclearance as leverage for other federal goals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_mechanism_robustness, empirical, 'Whether preclearance sunset mechanism functions as designed').

omega_variable(
    federal_authority_scope_creep,
    'Does federal preclearance authority remain limited to voting access enforcement, or does the mechanism become a tool for broader federal control over state electoral administration?',
    'Analysis of preclearance denials by category (voter access vs. electoral administration vs. political outcomes); correlation between preclearance denials and federal policy preferences unrelated to voting access',
    'If scope remains narrow: Rope classification stands. If scope expands: preclearance reclassifies toward Tangled Rope (coordination + broader extraction) or toward Snare (extraction masquerading as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_authority_scope_creep, empirical, 'Whether federal preclearance authority experiences scope creep').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1965_johnson_federal_preclearance_litigation_bypass, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(preclear_theater_1965, sotu_1965_johnson_federal_preclearance_litigation_bypass, theater_ratio, 0, 0.25).
narrative_ontology:measurement(preclear_theater_1980, sotu_1965_johnson_federal_preclearance_litigation_bypass, theater_ratio, 15, 0.35).
narrative_ontology:measurement(preclear_theater_1995, sotu_1965_johnson_federal_preclearance_litigation_bypass, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(preclear_extractiveness_1965, sotu_1965_johnson_federal_preclearance_litigation_bypass, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(preclear_extractiveness_1980, sotu_1965_johnson_federal_preclearance_litigation_bypass, base_extractiveness, 15, 0.22).
narrative_ontology:measurement(preclear_extractiveness_1995, sotu_1965_johnson_federal_preclearance_litigation_bypass, base_extractiveness, 30, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1965_johnson_federal_preclearance_litigation_bypass, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1965_johnson_federal_preclearance_litigation_bypass, voting_rights_act_coverage_formula).
narrative_ontology:affects_constraint(sotu_1965_johnson_federal_preclearance_litigation_bypass, section_4b_preclearance_conditions).

% DUAL FORMULATION NOTE:
% The preclearance litigation bypass is distinct from the Voting Rights Act's broader coverage formula and preclearance conditions. This story focuses on the temporal coordination function (eliminating litigation delay). Upstream constraints (coverage formula, which jurisdictions are covered) have their own ε values. Downstream constraints (what counts as compliance for bailout) also differ structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
