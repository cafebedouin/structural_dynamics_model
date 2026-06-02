% ============================================================================
% CONSTRAINT STORY: paradigm_shift_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paradigm_shift_mechanism, []).

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
 *   constraint_id: paradigm_shift_mechanism
 *   human_readable: Paradigm Shift Mechanism: Knowledge Coordination and Extractive Lock-In
 *   domain: epistemology/institutional_science
 *
 * SUMMARY:
 *   The paradigm shift mechanism describes how scientific and intellectual
 *   communities enforce incumbent paradigms while simultaneously claiming to
 *   pursue objective truth. This constraint exhibits the defining feature of
 *   Tangled Rope: it serves genuine coordination functions (shared
 *   methodology, cumulative knowledge building, institutional stability)
 *   while simultaneously enabling extraction (barrier to entry for
 *   challengers, concentration of resources and prestige on incumbents,
 *   suppression of alternatives). The mechanism operates through multiple
 *   institutional layers — peer review, funding allocation, hiring and
 *   promotion criteria, journal editorial gatekeeping — that individually
 *   appear meritocratic but collectively create a self-reinforcing lock-in
 *   structure. Paradigm challengers face a sequential filter: their work must
 *   clear peer review (dominated by incumbents), secure funding (allocated by
 *   incumbent-heavy committees), get hired/promoted (evaluated by incumbent
 *   peers), and achieve sufficient publication and citations to gain
 *   influence (facing citation bias from incumbent networks). The
 *   constraint's extractiveness (0.58) reflects that incumbents capture
 *   disproportionate resources and career advancement during the paradigm's
 *   reign, while challengers must overcome multiplicative barriers. The
 *   theater ratio (0.68) reflects that much of the paradigm's enforcement is
 *   performative: the appearance of scientific rigor and open evaluation
 *   masks systematic filtering of alternative frameworks. The measurements
 *   show increasing theater and extractiveness over the interval, indicating
 *   that as paradigms mature and institutional infrastructure solidifies, the
 *   enforcement mechanism becomes more theater-heavy and more extractive.
 *
 * KEY AGENTS:
 *   - Early-Career Paradigm Challengers: Primary victims (powerless/trapped) — face journal rejection, funding denial, hiring discrimination. Career prospects entirely dependent on paradigm acceptance.
 *   - Mid-Career Paradigm Challengers: Secondary victims (moderate/constrained) — have accumulated some reputation; can survive some resistance but face opportunity costs and momentum loss.
 *   - Established Paradigm Incumbents: Primary beneficiaries (institutional/arbitrage) — receive citation advantage, funding priority, hiring preference. Can exit paradigm without career damage.
 *   - Funding Gatekeeping Institutions: Secondary beneficiaries (organized/constrained) — maintain institutional legitimacy through paradigm-aligned evaluation. Constrained by need to appear open to innovation.
 *   - Peer Review System: Institutional actor (institutional/arbitrage) — enforces paradigm through rejection gates; maintains appearance of meritocratic evaluation while performing paradigm policing.
 *   - Analytical Observer: Meta-level (analytical/analytical) — recognizes hybrid coordination-extraction structure; can see both genuine scientific need for paradigm stability and extractive barriers to challenge.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paradigm_shift_mechanism, 0.58).
domain_priors:suppression_score(paradigm_shift_mechanism, 0.62).
domain_priors:theater_ratio(paradigm_shift_mechanism, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paradigm_shift_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(paradigm_shift_mechanism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(paradigm_shift_mechanism, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paradigm_shift_mechanism, tangled_rope).
narrative_ontology:human_readable(paradigm_shift_mechanism, "Paradigm Shift Mechanism: Knowledge Coordination and Extractive Lock-In").
narrative_ontology:topic_domain(paradigm_shift_mechanism, "epistemology/institutional_science").

domain_priors:requires_active_enforcement(paradigm_shift_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paradigm_shift_mechanism, paradigm_incumbent_researchers).
narrative_ontology:constraint_beneficiary(paradigm_shift_mechanism, funding_gatekeepers).
narrative_ontology:constraint_victim(paradigm_shift_mechanism, paradigm_challenger_researchers).
narrative_ontology:constraint_victim(paradigm_shift_mechanism, field_epistemic_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER PARADIGM CHALLENGER (SNARE) — Cannot exit without abandoning career prospects. Faces journal rejection, funding denial, and professional ostracism. The paradigm enforcement mechanism extracts opportunity cost and career capital. No realistic alternative pathway exists within the same field at the same time horizon.
constraint_indexing:constraint_classification(paradigm_shift_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER PARADIGM CHALLENGER (TANGLED ROPE) — Faces significant career costs but has accumulated sufficient reputation to survive some paradigm resistance. Benefits from the mainstream paradigm's institutional resources while challenging it. Can exit by returning to mainstream work but at cost of lost momentum. Mixed extraction and coordination: the constraint both enables (through shared infrastructure, publication venues, citation networks) and constrains (through peer skepticism and funding bias).
constraint_indexing:constraint_classification(paradigm_shift_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ESTABLISHED PARADIGM INCUMBENT (ROPE) — Experiences the paradigm as pure coordination. Shared frameworks enable theory building, methodology standardization, and cumulative knowledge. Citation networks and funding concentration serve them. Can arbitrage to new problems within the paradigm or switch fields entirely without career damage. Net beneficiary with maximal exit options.
constraint_indexing:constraint_classification(paradigm_shift_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FUNDING GATEKEEPING INSTITUTION (TANGLED ROPE) — Benefits from paradigm stability (reduces funding evaluation complexity, aligns with peer-review consensus). Also enforces paradigm lock-in through grant allocation. Faces pressure from new paradigm challengers but constrained by risk-aversion and institutional legitimacy requirements. Must maintain appearance of supporting innovation while structurally defending incumbency.
constraint_indexing:constraint_classification(paradigm_shift_mechanism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PEER REVIEW SYSTEM (PITON) — Primary function (quality assurance) has degraded to secondary function (paradigm policing). High theater ratio reflects that review performs paradigm gatekeeping through rejection of anomalies and challengers. The system persists through inertia despite reduced epistemic effectiveness. Maintained by institutional path-dependency rather than functional necessity.
constraint_indexing:constraint_classification(paradigm_shift_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes that paradigm lock-in serves genuine coordination functions (shared methodology, cumulative research, institutional stability) while simultaneously enabling extraction (barrier to entry for challengers, concentration of resources and prestige). The constraint is not purely extractive nor purely coordinative — it is a hybrid that uses coordination mechanisms to enable extraction. This is the canonical tangled rope structure.
constraint_indexing:constraint_classification(paradigm_shift_mechanism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paradigm_shift_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(paradigm_shift_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(paradigm_shift_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(paradigm_shift_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(paradigm_shift_mechanism, TR),
    TR >= 0.70.

:- end_tests(paradigm_shift_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting significant but not total asymmetry. Incumbent researchers capture career and resource advantages during the paradigm's reign. However, the extraction is not total because: (a) the paradigm does provide genuine coordination benefits that benefit all participants including some challengers; (b) successful paradigm shifts do eventually occur, creating windows where challengers gain influence; (c) some institutional variation exists (some fields/institutions are more or less resistant to challenges). The value reflects that the extractive mechanism is substantial and structural but not irreversible. Suppression (0.62): Moderate-high. Multiple barriers exist: publication bias (negative results and framework-challenging papers face higher rejection rates), funding concentration (review committees are incumbent-heavy), hiring and promotion criteria (favoring work within established frameworks), citation bias (incumbent networks cite their own work more readily), and social suppression (challenging the paradigm invites professional skepticism and career risk). These barriers are significant and structural but not total — some challengers do publish, some do receive funding, some do advance their careers. Barriers can be overcome with sufficient effort and resources, but at high cost. Theater ratio (0.68): High, reflecting that paradigm gatekeeping is substantially performative. Peer review appears to evaluate scientific merit objectively while actually filtering for paradigm alignment. Funding committees appear to assess intellectual merit while actually weighting proposals by paradigm consistency. Hiring and promotion appear to evaluate scientific contributions while actually assessing career paths within the paradigm. The performance of meritocratic evaluation masks the function of paradigm enforcement. The measurements show theater increasing over time as paradigm maturity increases the need for performative legitimation.
 *
 * PERSPECTIVAL GAP:
 *   The paradigm shift mechanism exhibits maximum perspectival divergence across the index space. The early-career challenger sees a Snare (pure extraction with no escape). The established incumbent sees a Rope (pure coordination with maximal benefits). The mid-career challenger sees Tangled Rope (mixed extraction and coordination; some agency but significant costs). The funding gatekeeper sees Tangled Rope but from the opposite direction (must maintain coordination function while managing paradigm lock-in for legitimacy). The peer review system sees itself as Rope (quality assurance coordination) while actually functioning as Piton (performative gatekeeping through inertia). The analytical observer sees the true structure: Tangled Rope throughout, with the perspectival gaps revealing that what appears as natural epistemic gatekeeping to incumbents is structural extraction from the view of challengers. The gap between incumbent/observer and challenger perspectives is the defining diagnostic of a tangled rope constraint — the constraint uses coordination mechanisms (shared methodology, institutional structure, cumulative knowledge) to enable extraction (barrier to entry, resource concentration, prestige asymmetry).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim positions and exit options. Early-career challengers are victims with trapped exit (high d ≈ 0.92, f(d) ≈ 1.38). Established incumbents are beneficiaries with arbitrage exit (low d ≈ 0.08, f(d) ≈ -0.10). Mid-career challengers are victims with constrained exit (moderate-high d ≈ 0.70, f(d) ≈ 0.98). Funding gatekeepers are beneficiaries with constrained exit (moderate d ≈ 0.45, f(d) ≈ 0.55) — they benefit from paradigm stability but are constrained by legitimacy requirements. The peer review system is a beneficiary with arbitrage exit (low d ≈ 0.05, f(d) ≈ -0.12) — it reinforces paradigm while maintaining institutional flexibility. The analytical observer has d ≈ 0.72 (analytical exit), f(d) ≈ 1.15. The chi formula χ = ε × f(d) × σ(S) produces effective extractiveness: for early-career challengers at global scope (σ = 1.2), χ ≈ 0.58 × 1.38 × 1.2 ≈ 0.96 (maximum experienced extraction); for incumbents, χ ≈ 0.58 × (-0.10) × 1.2 ≈ -0.07 (net subsidy/benefit). The scope amplification (global σ = 1.2) reflects that paradigm lock-in is a field-wide or discipline-wide phenomenon — it affects all agents globally, and institutional coordination at this scale increases the effective extraction experienced by those at the bottom of the hierarchy.
 *
 * MANDATROPHY ANALYSIS:
 *   The paradigm shift mechanism resolves mandatrophy through hybrid classification. It is NOT purely extractive (Snare) because genuine coordination benefits exist — shared methodology, cumulative knowledge, institutional stability, and research infrastructure are real and valuable. It is NOT purely coordinative (Rope) because asymmetric extraction is structural and intentional — resource concentration on incumbents is a feature, not a bug, of how paradigms defend themselves. The tangled rope classification captures both: the constraint genuinely coordinates scientific practice while simultaneously extracting from challengers. The key insight is that the coordination function is what enables the extraction. By creating a shared framework, accumulated infrastructure, and institutional legitimacy, the paradigm makes alternative frameworks appear less viable — you can't build on a framework nobody else uses, can't get funding for research outside the paradigm, can't publish in high-impact venues with non-paradigm work, can't advance your career by challenging the shared assumptions. The extraction is hidden inside the coordination. The mandatrophy is resolved by recognizing that true paradigm shifts require challengers to extract themselves from the coordination benefits (lose access to infrastructure, networks, credibility) as the price of building alternatives. This is not a flaw in the classification — it is the structural reality of paradigm change. The paradigm shift mechanism is a constraint that must be overcome, not dissolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paradigm_lock_in_definition,
    'Is the paradigm enforcement mechanism a natural epistemic lag (time required for evidence accumulation) or an institutional barrier (career risk, funding bias, publication gates)?',
    'Comparative analysis of paradigm-shift timelines across fields with different institutional structures; correlation between institutional gatekeeping density and paradigm rigidity; historical case studies of successful and failed paradigm challenges',
    'If purely epistemic lag: mechanism is closer to Mountain (natural structural property). If primarily institutional: mechanism is Snare or Tangled Rope (extractive lock-in). Classification could shift by 2-3 types depending on decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradigm_lock_in_definition, empirical, 'Epistemic lag vs institutional barrier distinction').

omega_variable(
    challenger_suppression_internalization,
    'Do paradigm challengers perceive suppression as externally imposed (career barriers, funding denial) or internalized (self-censorship, identity lock with marginal position)?',
    'Qualitative research with paradigm challengers; comparison of suppression levels in interviews vs structural measurement; analysis of whether suppression persists post-exit from field or paradigm',
    'If externally imposed: exit_options should be trapped or constrained. If internalized: identity_locked becomes appropriate, potentially changing early-career perspective from Snare to Rope or Tangled Rope. Directionality derivation affected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(challenger_suppression_internalization, empirical, 'Suppression mechanism: structural vs internalized').

omega_variable(
    paradigm_replacement_timing,
    'What threshold of anomalies, alternative-paradigm publications, or paradigm-challenger success triggers a paradigm shift? Is there a critical mass point?',
    'Historical analysis of 15+ major paradigm shifts across fields; identification of turning points; correlation with critical mass thresholds of alternative-paradigm research output',
    'If critical mass exists: paradigm lock-in is temporary (Scaffold type more appropriate). If no threshold: lock-in can persist indefinitely (Snare type confirmed). Coalition power dynamics may activate if challenger population exceeds critical mass.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(paradigm_replacement_timing, empirical, 'Critical mass threshold for paradigm replacement').

omega_variable(
    funding_concentration_causality,
    'Does funding concentration toward incumbent paradigm cause paradigm lock-in or reflect epistemic consensus? Can we disentangle institutional allocation bias from meritocratic prioritization?',
    'Natural experiments: comparison of funding distribution before/after paradigm shifts; analysis of funding allocation in fields with different institutional structures; study of challenger funding sources and success rates',
    'If causal toward lock-in: extractiveness increases and suppression is structural. If reflective of consensus: extractiveness decreases and suppression is epistemic gatekeeping of low-quality work. Classification hinges on this.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(funding_concentration_causality, empirical, 'Funding concentration causality: bias vs consensus').

omega_variable(
    open_science_paradigm_resilience,
    'Do preprints, open data, and decentralized publication reduce paradigm lock-in by providing alternative validation pathways, or do incumbent paradigm gatekeepers simply migrate to these venues?',
    'Longitudinal study of paradigm-challenger success rates on arXiv/bioRxiv vs traditional journals; tracking of which challengers succeed with/without traditional publication; analysis of citation patterns and impact in new venues',
    'If open science enables: lock-in has sunset clause (Scaffold increasingly appropriate). If gatekeepers migrate: mechanism persists (Snare/Tangled Rope confirmed). May indicate that the constraint is not the publication mechanism but the epistemic authority structure itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_science_paradigm_resilience, empirical, 'Whether open science reduces paradigm lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paradigm_shift_mechanism, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paradigm_tr_t0, paradigm_shift_mechanism, theater_ratio, 0, 0.45).
narrative_ontology:measurement(paradigm_tr_t25, paradigm_shift_mechanism, theater_ratio, 25, 0.58).
narrative_ontology:measurement(paradigm_tr_t50, paradigm_shift_mechanism, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(paradigm_be_t0, paradigm_shift_mechanism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(paradigm_be_t25, paradigm_shift_mechanism, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(paradigm_be_t50, paradigm_shift_mechanism, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paradigm_shift_mechanism, information_standard).
narrative_ontology:affects_constraint(paradigm_shift_mechanism, peer_review_gatekeeping).
narrative_ontology:affects_constraint(paradigm_shift_mechanism, funding_allocation_bias).
narrative_ontology:affects_constraint(paradigm_shift_mechanism, citation_network_concentration).

% DUAL FORMULATION NOTE:
% The paradigm shift mechanism is upstream of multiple domain-specific constraints: peer review gatekeeping (which performs paradigm enforcement through rejection), funding allocation bias (which concentrates resources on incumbents), and citation network concentration (which amplifies incumbent voice). This constraint story models the general mechanism. Domain-specific stories model how the mechanism operates in particular fields (quantum physics, biology, economics, etc.) with field-specific ε values and institutional variations. All network members should link back to this parent constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
