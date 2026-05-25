% ============================================================================
% CONSTRAINT STORY: epistemic_charity_norm
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_charity_norm, []).

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
 *   constraint_id: epistemic_charity_norm
 *   human_readable: Epistemic Charity Norm in Intellectual Discourse
 *   domain: epistemology/social_coordination
 *
 * SUMMARY:
 *   The epistemic charity norm — the expectation that intellectuals should
 *   interpret opposing arguments in their strongest possible form before
 *   critique — functions simultaneously as a coordination mechanism and an
 *   extraction apparatus. In its pure form, it solves a genuine collective
 *   action problem: the temptation to strawman opponents is ever-present, and
 *   a shared norm against this behavior enables good-faith intellectual
 *   exchange. But the norm's application is radically asymmetric across
 *   institutional hierarchies. Charitable interpretation is demanded of those
 *   arguing against dominant frameworks, while the same demand is not applied
 *   to defenders of establishment positions. This asymmetry extracts from
 *   dissidents and marginal voices while protecting institutional
 *   interpreters. Over the interval measured (40 time units, representing
 *   roughly 40 years of academic discourse), the theater ratio has nearly
 *   doubled (0.35 → 0.65), indicating that the performative element of
 *   demonstrating one's own charity has increasingly dominated the actual
 *   epistemic function of preventing strawmanning. The constraint exemplifies
 *   how coordination mechanisms can degrade into theatrical gatekeeping when
 *   power asymmetries intersect with evaluation systems that reward the
 *   appearance of intellectual humility.
 *
 * KEY AGENTS:
 *   - Marginalized Voices: Primary victim (powerless/trapped) — denied charity while required to extend it; cannot exit without forfeiting epistemic credibility in dominant institutions
 *   - Dissident Intellectuals: Secondary victim (moderate/constrained) — experience selective gatekeeping; can conform to dominant positions at significant career cost
 *   - Institutional Interpreters: Primary beneficiary (institutional/arbitrage) — benefit from coordination function while exercising selective exemption from charity demands
 *   - Progressive Coalition: Organized agents (organized/constrained) — working to build alternative epistemological frameworks with symmetric charity and lower theater; see sunset pathway through epistemic justice development
 *   - Academic Reputation System: Institutional actor (institutional/arbitrage) — maintains the norm as a mark of epistemic virtue; sees own function as degraded (piton perspective) but sustains through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies the coordination function and assesses whether asymmetric application is separable from the mechanism itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_charity_norm, 0.52).
domain_priors:suppression_score(epistemic_charity_norm, 0.48).
domain_priors:theater_ratio(epistemic_charity_norm, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_charity_norm, extractiveness, 0.52).
narrative_ontology:constraint_metric(epistemic_charity_norm, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(epistemic_charity_norm, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_charity_norm, tangled_rope).
narrative_ontology:human_readable(epistemic_charity_norm, "Epistemic Charity Norm in Intellectual Discourse").
narrative_ontology:topic_domain(epistemic_charity_norm, "epistemology/social_coordination").

domain_priors:requires_active_enforcement(epistemic_charity_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_charity_norm, charitable_interpreters).
narrative_ontology:constraint_beneficiary(epistemic_charity_norm, institutional_discourse_gatekeepers).
narrative_ontology:constraint_victim(epistemic_charity_norm, marginal_voices).
narrative_ontology:constraint_victim(epistemic_charity_norm, dissident_intellectuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED VOICE (SNARE) — Caught in asymmetric charity: charitable interpretation is withheld from their arguments while demanded from interpreters of mainstream views. Cannot exit discourse without forfeiting credibility. Bears full cost of the norm's selective application.
constraint_indexing:constraint_classification(epistemic_charity_norm, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISSIDENT INTELLECTUAL (TANGLED ROPE) — Benefits from coordination function (genuine intellectual humility, preventing strawmanning of mainstream views) but experiences extraction through selective gatekeeping. Can exit by conforming, but at significant career cost. Mixed experience of coordination and asymmetric extraction.
constraint_indexing:constraint_classification(epistemic_charity_norm, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL INTERPRETER (ROPE) — Academic institutions benefit from the charity norm as a coordination mechanism. It enables good-faith intellectual exchange, reduces strawmanning, and builds epistemic trust. Experiences constraint as pure coordination with no meaningful extraction — can always apply charity selectively as needed.
constraint_indexing:constraint_classification(epistemic_charity_norm, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROGRESSIVE COALITION (SCAFFOLD) — Organized movements for inclusive epistemology (epistemic justice frameworks, participatory action research) see the selective charity norm as a temporary institutional barrier with a sunset clause. Developing alternative epistemological frameworks that embed charity symmetrically rather than gatekeeping it. Low effective extraction because the coalition perceives an exit pathway through institutional transformation.
constraint_indexing:constraint_classification(epistemic_charity_norm, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC REPUTATION SYSTEM (PITON) — The charity norm persists through institutional inertia as a mark of intellectual sophistication and good faith. But the function has degraded: it now primarily serves as a theater of epistemic virtue, allowing gatekeepers to perform good faith while withholding charity from threats to established hierarchies. High theater ratio reflects that the performative element (demonstrating one's own sophistication through charitable reading) often dominates actual epistemic function (improving collective knowledge).
constraint_indexing:constraint_classification(epistemic_charity_norm, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From the analytical vantage, the epistemic charity norm is fundamentally a coordination mechanism: it solves the collective action problem of intellectual strawmanning and hostile hermeneutics. The norm enables good-faith intellectual exchange across deep disagreements. The extraction component is contingent on selective application, not inherent to the mechanism itself. This suggests the constraint can be rebalanced toward pure coordination by enforcing symmetric application.
constraint_indexing:constraint_classification(epistemic_charity_norm, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_charity_norm_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_charity_norm, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_charity_norm, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_charity_norm, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(epistemic_charity_norm, TR),
    TR >= 0.70.

:- end_tests(epistemic_charity_norm_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The norm creates genuine epistemic benefits (reduced strawmanning, more robust intellectual exchange) but at an asymmetrically distributed cost. Defenders of dominant positions rarely face the demand to charitably interpret dissidents; dissidents always face the demand to charitably interpret dominance. This asymmetry creates net extraction on the margin, though not as severe as a pure snare (0.70+) because the norm does have genuine coordination value. Suppression (0.48): Moderate. Marginalized voices face significant barriers to exit (career consequences for rejecting the norm, loss of credibility if seen as non-charitable) but these are surmountable — some agents do exit by building alternative epistemic communities. The suppression is real but not total. Theater ratio (0.65): High and rising. The norm has increasingly become a performance of intellectual sophistication rather than a mechanism for improving epistemic exchange. Academics cite the norm to perform epistemic virtue, often while selectively exempting favored positions from the charity requirement. The rising theater ratio (0.35 → 0.65 over the interval) indicates that performative compliance has increasingly dominated actual epistemic function.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal here. The institutional interpreter sees a beneficial coordination mechanism (Rope) — their experience of the norm is that it prevents strawmanning and enables good-faith exchange, and they can apply it selectively as needed. The marginalized voice sees pure extraction (Snare) — they are required to interpret institutional positions charitably while denied reciprocal charity, and cannot exit without epistemic delegitimation. The dissident intellectual sees mixed coordination and extraction (Tangled Rope) — they benefit from a more robust intellectual conversation when the norm is applied symmetrically, but experience extraction when it is applied asymmetrically. The progressive coalition sees a solvable problem with an exit pathway (Scaffold) — epistemic justice frameworks that embed symmetric respect and accountability could replace the gatekeeping function. The academic reputation system sees its own degradation (Piton) — the system maintains the norm through inertia, recognizing that its function has atrophied. The analytical observer risks seeing an immutable epistemic requirement (Mountain — 'good-faith exchange requires charity') but the structural data reveals this as a false summit: the immutability disappears when charity is applied symmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   The marginalized voice experiences high d (close to 1.0, full target) due to powerless status and trapped exit options — they bear costs of the norm while unable to leverage its benefits. Institutional interpreters experience low d (close to 0.0, beneficiary) due to institutional power and arbitrage exit options — they can selectively apply the norm as needed. Dissident intellectuals experience moderate d (0.55-0.65) due to moderate power and constrained exit — they can resist some applications but at career cost. The analytical observer experiences high d (0.72-0.80, analytical/analytical position) because the observation itself requires seeing the norm's structural asymmetry, which is invisible from within positions that benefit from it. The directionality asymmetry explains why the same norm is experienced as snare by the powerless and rope by the institutional — their structural position relative to extraction flow differs radically.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by decomposing the epistemic charity norm into two distinct structural claims: (1) the coordination function (genuine epistemic value in preventing strawmanning) and (2) the asymmetric application (selective gatekeeping). Claim 1 maps to Rope — pure coordination, no asymmetric extraction. Claim 2 is a separate constraint (institutional gatekeeping with identity-lock on marginal voices) that maps to Snare. The tangled rope classification emerges from the current state: both mechanisms are active simultaneously, the coordination function is real and valuable, but it is embedded in an extractive gatekeeping apparatus that selects who gets to benefit from the coordination. The resolution pathway is NOT to eliminate the charity norm but to enforce symmetric application: expand the coalition that benefits from coordination while removing the gatekeeping extraction. This is exactly the sunset trajectory the scaffold perspective identifies — epistemic justice frameworks are building alternatives that preserve the coordination function without the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charity_application_asymmetry_mechanism,
    'Is the asymmetric application of charity a feature of the norm itself or a contingent abuse of a symmetric mechanism?',
    'Historical analysis of how the norm is articulated vs how it is applied; documentation of explicit exceptions and gatekeeping criteria; comparison across institutional contexts with different power distributions',
    'If feature: the norm is inherently extractive (Snare classification valid for all perspectives). If contingent abuse: the norm is pure coordination (Rope classification valid for all perspectives) and the extraction is a separate constraint (institutional gatekeeping).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charity_application_asymmetry_mechanism, conceptual, 'Whether charitable interpretation asymmetry is integral to the norm or a separable abuse').

omega_variable(
    marginal_voice_identity_lock,
    'Are marginalized voices constrained by material barriers (career risk, publication gatekeeping) or identity-locked (inability to imagine epistemic legitimacy outside the charity-dispensing system)?',
    'Comparative analysis of exit trajectories: agents who exit completely vs those who remain constrained; interview data on whether perceived barriers are material or internalized; measurement of whether agents who exit maintain epistemic confidence',
    'If identity-locked: the constraint operates through cognitive capture and educational pipeline effects (deeper binding than material constraints). If constrained: material barriers are primary, and structural reforms (removing gatekeeping) would materially increase exit capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_voice_identity_lock, empirical, 'Whether barrier to marginalized voices is structural or identity-based').

omega_variable(
    epistemic_justice_alternative_sufficiency,
    'Do epistemic justice frameworks provide a structurally adequate replacement for the charity norm, or do they require charity as a foundation?',
    'Analysis of epistemic justice implementations in communities without access to charity gatekeepers; documentation of alternative mechanisms for preventing strawmanning and hostile hermeneutics; assessment of whether justice-based norms achieve epistemic quality equivalent to charity-based norms',
    'If sufficient: scaffold perspective confirmed — sunset pathway is real and reachable. If insufficient: the charity norm may be a necessary coordination mechanism despite its extraction component, suggesting a true Tangled Rope structure that cannot be decomposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_justice_alternative_sufficiency, empirical, 'Whether epistemic justice frameworks can replace the charity norm').

omega_variable(
    theater_ratio_inflation_driver,
    'Is the rising theater ratio (0.52 → 0.65) driven by increasing complexity of intellectual exchange or by increasing gatekeeping performance?',
    'Measurement of actual epistemic outcomes (citations of charitably-interpreted work, quality of intellectual innovation, replication of key claims) vs performative outcomes (citations for charity demonstrations, peer reputation for sophisticated reading); historical comparison of how substantive disagreements are handled',
    'If complexity-driven: theater ratio reflects legitimate coordinated management of difficult exchange. If gatekeeping-driven: theater ratio indicates the piton degradation — the function has atrophied and the performative element now dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_inflation_driver, empirical, 'What drives the increasing theater ratio in the charity norm').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_charity_norm, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epchar_tr_t0, epistemic_charity_norm, theater_ratio, 0, 0.35).
narrative_ontology:measurement(epchar_tr_t20, epistemic_charity_norm, theater_ratio, 20, 0.48).
narrative_ontology:measurement(epchar_tr_t40, epistemic_charity_norm, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(epchar_be_t0, epistemic_charity_norm, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(epchar_be_t20, epistemic_charity_norm, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(epchar_be_t40, epistemic_charity_norm, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_charity_norm, information_standard).
narrative_ontology:affects_constraint(epistemic_charity_norm, institutional_gatekeeping).
narrative_ontology:affects_constraint(epistemic_charity_norm, epistemic_justice_framework).
narrative_ontology:affects_constraint(epistemic_charity_norm, intellectual_credibility_assignment).

% DUAL FORMULATION NOTE:
% The epistemic charity norm is downstream of broader gatekeeping mechanisms in academic institutions and upstream of specific identity-locked constraints on marginal voices. The constraint family includes: (1) epistemic_charity_norm (ε=0.52, Tangled Rope) — the norm itself with mixed coordination and extraction; (2) institutional_gatekeeping (ε=0.68, Snare) — the machinery that selectively applies charity; (3) epistemic_justice_framework (ε=0.15, Rope) — alternative coordination mechanisms with lower theater and symmetric application. The three are linked: charity norm enables but does not require gatekeeping; epistemic justice frameworks provide exit pathways by replacing the charity-gatekeeping pair with symmetric accountability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(epistemic_charity_norm, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
