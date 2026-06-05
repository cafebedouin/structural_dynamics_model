% ============================================================================
% CONSTRAINT STORY: dutch_minority_govt_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dutch_minority_govt_2026, []).

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
 *   constraint_id: dutch_minority_govt_2026
 *   human_readable: Dutch Minority Government External Support Agreement (2026)
 *   domain: political/governmental
 *
 * SUMMARY:
 *   Following the fragmented 2025 Dutch election, the far-right PVV emerged
 *   as the largest party but cannot command a majority coalition — no
 *   mainstream parties will enter formal coalition with the PVV due to
 *   ideological distance and international pressure. To govern, the PVV
 *   formed a minority government and secured external parliamentary support
 *   from one or more support partners (likely a combination of center-right
 *   and centrist parties) who agreed to abstain or conditionally vote for
 *   government measures in exchange for policy concessions and consultation
 *   rights. This creates a Tangled Rope constraint: the support partner
 *   provides genuine coordination (enabling government formation and passage
 *   of budgets/legislation) while simultaneously extracting policy
 *   concessions, ministerial positions, and/or budgetary commitments. The
 *   constraint exhibits the full perspectival range: the support partner
 *   experiences it as coercive (Snare) due to defection costs; the PVV
 *   government experiences it as beneficial coordination (Rope); opposition
 *   parties see both coordination opportunity and extraction (Tangled Rope);
 *   Dutch parliamentary norms see it as temporary stabilization (Scaffold);
 *   European stability expectations see it as degraded but maintained
 *   performance (Piton); and a global analytical observer risks naturalizing
 *   the constraint as inherent electoral inevitability (false Mountain).
 *
 * KEY AGENTS:
 *   - PVV Parliamentary Caucus: Primary beneficiary (powerful/arbitrage) — forms government and controls cabinet; has arbitrage exit via new elections if dissatisfied with support terms
 *   - Support Partner (Center-Right/Centrist Coalition): Primary victim and agent of extraction (organized/constrained) — constrained by electoral arithmetic; withdrawal causes government collapse and likely worse electoral outcomes
 *   - Opposition Bloc (Left-Green-Socialist Parties): Secondary agent (organized/constrained) — have leverage through amendments and blocking coalitions but constrained by fragmentation; experience mixed coordination and extraction
 *   - Dutch Parliamentary Procedures: Institutional actor (institutional/constrained) — confidence votes and budget review cycles create formal venues for support partner leverage
 *   - EU Commission and International Partners: Analytical observer (analytical/analytical) — concern about PVV governance quality; support agreement provides reassurance of centrist constraint on PVV autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dutch_minority_govt_2026, 0.52).
domain_priors:suppression_score(dutch_minority_govt_2026, 0.65).
domain_priors:theater_ratio(dutch_minority_govt_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dutch_minority_govt_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(dutch_minority_govt_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dutch_minority_govt_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dutch_minority_govt_2026, tangled_rope).
narrative_ontology:human_readable(dutch_minority_govt_2026, "Dutch Minority Government External Support Agreement (2026)").
narrative_ontology:topic_domain(dutch_minority_govt_2026, "political/governmental").

domain_priors:requires_active_enforcement(dutch_minority_govt_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dutch_minority_govt_2026, pvv_parliamentary_caucus).
narrative_ontology:constraint_beneficiary(dutch_minority_govt_2026, coalition_formation_negotiators).
narrative_ontology:constraint_victim(dutch_minority_govt_2026, support_partner_parliamentary_independence).
narrative_ontology:constraint_victim(dutch_minority_govt_2026, policy_coherence_across_spectrum).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXTERNAL SUPPORT PARTNER (SNARE) — Constrained by legislative arithmetic: withdrawal collapses government and triggers early elections, risking worse outcomes. Cannot exit without severe political cost. Faces continuous extraction through veto power exploitation and policy concessions. d≈0.80, f(d)≈1.20, σ=1.0 → χ≈0.62.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: PVV-LED GOVERNMENT (ROPE) — Experiences the support agreement as pure coordination: obtains parliamentary majority through negotiated terms. Arbitrage exit available (call elections, seek different coalition). Benefits from parliamentary stability and policy implementation. d≈0.10, f(d)≈0.05, σ=1.0 → χ≈0.03. Near-zero effective extraction; net beneficiary.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: OPPOSITION BLOC (TANGLED ROPE) — Organized actors (left-right opposition) benefit from leverage in legislative amendments and agenda-setting (coordination function) but also suffer extraction: support agreement limits their blocking power and forces them into reactive positioning. Constrained exit due to electoral math. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DUTCH PARLIAMENTARY DEMOCRACY (SCAFFOLD) — The support agreement is explicitly temporary (indexed to confidence votes, budget negotiations, election schedule). It solves a immediate gridlock (coordination function) via sunset: once electoral fragmentation deepens beyond recovery or consensus realigns, the agreement ends. Dutch consensual norms (polder model tradition) provide self-limiting structure. theater_ratio moderate; sunset clause implicit in support mechanism. d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.26.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EUROPEAN PARLIAMENTARY STABILITY NORMS (PITON) — The support agreement maintains performative appearance of parliamentary legitimacy while functioning as a contingency mechanism. European norms expect stable governments; the agreement theater creates that appearance despite fragile underlying arithmetic. theater_ratio=0.58 reflects performative confidence votes and policy scripts. Function (preventing electoral chaos) has degraded compared to majority coalition governments; maintained through institutional inertia and electoral law constraints. d≈0.50, f(d)≈0.65, σ=1.1 → χ≈0.38.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ELECTORAL ARITHMETIC (MOUNTAIN-CANDIDATE) — From a civilizational/global view, Dutch electoral proportionality (D'Hondt method) creates structural inevitability: fragmented results mathematically require either grand coalitions or external support mechanisms. The constraint appears natural-law-like — inherent to the electoral system design. However, base properties (ε=0.52, suppression=0.65, theater=0.58) contradict mountain classification: this is contingent on specific historical party configurations and electoral choices, not on mathematical necessity. Engine flags as false summit.
constraint_indexing:constraint_classification(dutch_minority_govt_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dutch_minority_govt_2026_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dutch_minority_govt_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dutch_minority_govt_2026, TR),
    TR >= 0.70.

:- end_tests(dutch_minority_govt_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The support partner extracts significant policy concessions (typically 30-40% of cabinet positions, budget influence, veto rights on key legislation). But the extraction is not overwhelming because the support partner also has genuine coordination function — they enable government formation, which both sides need. The value reflects that roughly half of the support agreement's content is quid-pro-quo coordination, half is extraction asymmetry. Suppression (0.65): High. The support partner faces severe constraints: (1) legislative arithmetic forces continued participation (defection triggers elections); (2) reputational cost of being seen as propping up a controversial party; (3) internal party pressure from members who oppose supporting the PVV. These barriers suppress alternative negotiating outcomes. Theater ratio (0.58): Moderate. The agreement includes performative elements (confidence votes, policy speeches emphasizing consensus) but maintains functional legislative activity. The theater has increased from initial support agreement (0.42 at formation) as the government has become defensive and procedurally ritualistic. The increase tracks with extraction accumulation — as support partner leverage grows, more of the process becomes scripted negotiation and less is organic legislative work.
 *
 * PERSPECTIVAL GAP:
 *   The PVV government sees the support agreement as routine political coordination (Rope perspective) — they obtained parliamentary majority and can govern. The support partner sees it as coercive (Snare perspective) — trapped by electoral arithmetic, forced to tolerate a party they opposed, extracting concessions because defection is catastrophic. Opposition parties see opportunity and constraint (Tangled Rope) — they can amend government bills and force concessions from a weakened coalition, but they are also locked out by the support partner's veto. Dutch democratic norms see temporary stabilization (Scaffold) — the agreement is indexed to confidence votes and budgets, providing a natural sunset. European observers see degraded democracy (Piton) — the performative confidence votes create appearance of legitimacy while masking fragile coalition arithmetic. A naive analytical observer risks seeing electoral necessity (false Mountain) — claiming proportional representation mathematically requires support agreements — but the actual constraint is contingent on this specific party configuration.
 *
 * DIRECTIONALITY LOGIC:
 *   PVV Government: Beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Net beneficiary; can threaten new elections if dissatisfied with support terms. Support Partner: Victim + constrained → d≈0.80, f(d)≈1.20. Severe extraction; constrained by electoral arithmetic and reputational risk. Opposition Bloc: Mixed victim/beneficiary + constrained → d≈0.55, f(d)≈0.75. Can lever amendments but cannot veto government measures. Dutch Parliamentary Norms: Institutional + constrained → d≈0.45, f(d)≈0.50. Norms enable support mechanism but are also strained by it. EU/International: Analytical observer + arbitrage → d≈0.30, f(d)≈0.15. Can influence support partner choice through diplomatic pressure but not directly participate.
 *
 * MANDATROPHY ANALYSIS:
 *   The support agreement resolves the mandatrophy by distinguishing genuine coordination (government formation, budget passage, legislative majority) from asymmetric extraction (policy concessions, cabinet positions, ongoing veto leverage). The Tangled Rope classification is appropriate: both functions are real. The support partner genuinely needs to participate in governance (coordination); the PVV government genuinely extracts concessions in return (extraction). The perspectival gap shows how the same structural arrangement appears as Rope to beneficiaries and Snare to victims. The mandatrophy is resolved by acknowledging that BOTH readings are correct — the constraint IS a hybrid. The support agreement prevents mislabeling this as pure coordination (Rope) which would ignore the extraction, or pure extraction (Snare) which would ignore the coordination function. The theater ratio increase (0.42 → 0.58) shows early signs of Goodhart drift: as the support partner's leverage grows, more of the process becomes performative script rather than functional governance. This tracks the extraction accumulation and suggests the constraint could degrade toward Piton or Snare if the support mechanism persists beyond its natural sunset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    support_partner_defection_threshold,
    'What policy concession or external shock would trigger defection of the support partner?',
    'Tracking of policy negotiations; analysis of partner''s red lines; coalition stress-testing under budget crises or immigration controversies',
    'If threshold is low: support agreement is fragile (behaves like Snare). If threshold is high: government has real stability (Rope-like). Determines whether next 12-24 months see collapse or consolidation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(support_partner_defection_threshold, empirical, 'Defection threshold for support partner').

omega_variable(
    extraction_mechanism_formality,
    'Is the extraction mechanism (veto power, policy concessions) formally written in the support agreement or implicit in repeated negotiation?',
    'Analysis of published support agreement text; comparison to actual policy outcomes; measurement of support partner influence on budgets, appointments, legislation',
    'If formal: transparent Tangled Rope with measurable beneficiary/victim structure. If implicit: degrades to contingent Snare (support partner cannot publicly defend concessions as negotiated). Affects legitimacy and sustainability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_mechanism_formality, empirical, 'Formality of extraction mechanism in support agreement').

omega_variable(
    electoral_realignment_timeline,
    'What electoral timeline would force renegotiation or collapse of the support agreement?',
    'Polling trends; analysis of party coalition capacity; modeling of next election scenarios',
    'If renegotiation likely within 18 months: agreement is Scaffold with real sunset. If stable for 3+ years: functions as quasi-permanent Tangled Rope. Sunset timing determines classification longevity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_realignment_timeline, empirical, 'Timeline to electoral realignment forcing renegotiation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dutch_minority_govt_2026, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dutch_minority_tr_t0, dutch_minority_govt_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dutch_minority_tr_t6, dutch_minority_govt_2026, theater_ratio, 6, 0.52).
narrative_ontology:measurement(dutch_minority_tr_t12, dutch_minority_govt_2026, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(dutch_minority_be_t0, dutch_minority_govt_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dutch_minority_be_t6, dutch_minority_govt_2026, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(dutch_minority_be_t12, dutch_minority_govt_2026, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dutch_minority_govt_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(dutch_minority_govt_2026, eu_migration_policy_constraints).
narrative_ontology:affects_constraint(dutch_minority_govt_2026, dutch_fiscal_sustainability).

% DUAL FORMULATION NOTE:
% The support agreement is downstream of the 2025 electoral fragmentation constraint but represents a distinct structural phenomenon. Electoral fragmentation (ε≈0.30, more stable/natural) creates the condition; the support agreement mechanism (ε≈0.52, more extractive/contingent) is the institutional response. They are linked but structurally distinct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dutch_minority_govt_2026, organized, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
