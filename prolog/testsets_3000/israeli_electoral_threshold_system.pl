% ============================================================================
% CONSTRAINT STORY: israeli_electoral_threshold_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israeli_electoral_threshold_system, []).

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
 *   constraint_id: israeli_electoral_threshold_system
 *   human_readable: Israeli Electoral Threshold System
 *   domain: political_systems/electoral_governance
 *
 * SUMMARY:
 *   The Israeli electoral threshold — currently 3.25% of votes required for
 *   Knesset representation — functions simultaneously as a mechanism to
 *   prevent parliamentary fragmentation and as a barrier that systematically
 *   excludes small parties, emerging movements, and certain demographic-based
 *   political projects. The threshold has been incrementally raised from 1%
 *   (1948) to 1.5% (1992) to 2% (1996) to 3.25% (2014), with each increase
 *   justified as necessary to prevent instability while simultaneously
 *   excluding specific political competitors. The constraint exhibits hybrid
 *   coordination-extraction structure: it does enable coalition building and
 *   government formation (coordination function), but it achieves this partly
 *   through systematic exclusion (extraction mechanism). The extractiveness
 *   trajectory shows an increasing trend from 0.38 to 0.58 over the 16-year
 *   measurement period, reflecting growing awareness and political debate
 *   about the threshold's role in marginalizing specific constituencies. The
 *   theater ratio remains relatively low (0.35 to 0.48) compared to other
 *   institutional constraints, indicating that the mechanism's function is
 *   relatively transparent — the threshold works as stated, even if its
 *   stated purpose masks its actual political consequence.
 *
 * KEY AGENTS:
 *   - Large Established Parties (Likud, Labor): Primary beneficiaries (institutional/arbitrage) — threshold ensures they maintain coalition kingmaker positions; can always participate in coalition formation
 *   - Small Ideological Parties (religious parties, Arab parties, far-left/far-right): Primary victims (powerless/trapped) — systematic exclusion despite significant voter bases; no exit pathway if below threshold
 *   - Emerging Political Movements (sector-based movements, protest-based parties): Secondary victims (moderate/constrained) — face high barrier to entry; must accumulate critical mass before threshold can be cleared
 *   - Coalition Kingmakers (4-6 seat parties): Mixed (moderate/constrained) — benefit from negotiating leverage but extracted from through disproportionate coalition demands
 *   - Diaspora Political Communities (organized): Secondary victims (organized/constrained) — organized movements for representation face threshold barriers that inflate cost of voice
 *   - Electoral Commission: Institutional administrator (institutional/arbitrage) — maintains system through legal framework; sees threshold as degraded mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes hybrid coordination-extraction structure; identifies threshold as neither pure mechanism nor pure gatekeeper
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israeli_electoral_threshold_system, 0.58).
domain_priors:suppression_score(israeli_electoral_threshold_system, 0.65).
domain_priors:theater_ratio(israeli_electoral_threshold_system, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israeli_electoral_threshold_system, extractiveness, 0.58).
narrative_ontology:constraint_metric(israeli_electoral_threshold_system, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(israeli_electoral_threshold_system, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israeli_electoral_threshold_system, tangled_rope).
narrative_ontology:human_readable(israeli_electoral_threshold_system, "Israeli Electoral Threshold System").
narrative_ontology:topic_domain(israeli_electoral_threshold_system, "political_systems/electoral_governance").

domain_priors:requires_active_enforcement(israeli_electoral_threshold_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israeli_electoral_threshold_system, large_established_parties).
narrative_ontology:constraint_beneficiary(israeli_electoral_threshold_system, coalition_kingmakers).
narrative_ontology:constraint_victim(israeli_electoral_threshold_system, small_ideological_parties).
narrative_ontology:constraint_victim(israeli_electoral_threshold_system, emerging_political_movements).
narrative_ontology:constraint_victim(israeli_electoral_threshold_system, marginalized_demographic_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED POLITICAL MOVEMENT (SNARE) — A party unable to clear the threshold faces total representation collapse despite significant voter base. No alternative pathway to political voice; coalitions cannot help. Maximum extraction — loses all parliamentary seats and institutional leverage. Suppression is structural: threshold is enforced by law with no discretionary workaround.
constraint_indexing:constraint_classification(israeli_electoral_threshold_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINAL COALITION PARTY (TANGLED ROPE) — Party clearing the threshold but commanding few seats (4-6) experiences mixed coordination and extraction. Genuinely coordinates coalition formation — a necessary function. But also extracted from: kingmaker position forces disproportionate concessions (control of specific ministries, veto power on certain legislation). Constrained exit: cannot easily shift voter base to larger party without identity dissolution. High suppression but not total.
constraint_indexing:constraint_classification(israeli_electoral_threshold_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR COALITION PARTNER (ROPE) — Large party experiences the threshold as coordination mechanism. Threshold ensures coalition building is necessary (prevents single-party majority in most election cycles), creating coordination function. Net beneficiary through coalition formation predictability and leverage. Arbitrage exit: can always form coalitions or remain opposition. Experiences constraint as enabling, not extractive.
constraint_indexing:constraint_classification(israeli_electoral_threshold_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DIASPORA POLITICAL COMMUNITIES (TANGLED ROPE) — Organized diaspora movements seeking representation in Israeli politics face threshold barriers that extract disproportionate leverage from mobilized communities while also coordinating their input into coalition politics. Constrained: cannot simply exit Israeli political system if diaspora interests in Israel remain central. Theater is moderate: threshold is presented as technical requirement but functions as political gatekeeper. Mixed benefit-cost structure: coordination occurs (diaspora has leverage) but extraction occurs (threshold inflates cost of voice).
constraint_indexing:constraint_classification(israeli_electoral_threshold_system, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL COMMISSION (PITON) — Institution charged with administering the threshold sees it as degraded procedural mechanism. Threshold was designed to prevent parliamentary fragmentation; modern usage reflects political gatekeeping divorced from original function. Theater ratio (0.48) reflects that much administrative effort goes into enforcing a boundary that major parties now use for advantage rather than stability. The commission maintains the system through inertia and legal precedent, not because it serves its stated coordination function.
constraint_indexing:constraint_classification(israeli_electoral_threshold_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The threshold coordinates coalition building (genuine function) while extracting representation from parties unable to clear the bar. Both mechanisms are real. The constraint creates parliamentary stability (coordination benefit) at the cost of systematic exclusion (extraction cost). This is not a natural law or pure extraction, but a hybrid mechanism that cannot claim innocence on either count.
constraint_indexing:constraint_classification(israeli_electoral_threshold_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israeli_electoral_threshold_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israeli_electoral_threshold_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israeli_electoral_threshold_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israeli_electoral_threshold_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(israeli_electoral_threshold_system, TR),
    TR >= 0.70.

:- end_tests(israeli_electoral_threshold_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The threshold directly prevents representation of parties with 2-3% of vote share. In a recent cycle, approximately 15-20% of votes cast may not achieve representation (varies by electoral fragmentation). However, this is not extraction in the pure sense because the threshold does serve its stated coordination function — it prevents 15-20 party coalitions and enables government formation. The 0.58 value reflects that coordination benefit does not erase the representation cost. The upward trend (0.38 → 0.58) reflects political polarization increasing the cost: as the electorate fragments into more distinct constituencies, the threshold excludes increasingly organized and coherent groups rather than marginal slivers. Suppression (0.65): High. There is no discretionary pathway around the threshold; it is enforced uniformly. However, parties can strategically ally before elections (circumventing the threshold through joint lists), which provides limited exit for organized movements. Suppression is structural and legal but not absolute. Theater ratio (0.48): Moderate. The threshold is justified as fragmentation prevention (stated function), but political debate increasingly recognizes it as gatekeeping mechanism (actual function). The constraint is relatively transparent — observers know what it does and how — but the gap between stated purpose and actual effect is growing. The theater is not theatrical performance (like pretended judicial independence) but rather the gap between justification and consequence.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is primarily about what the constraint appears to be depending on your structural position relative to it. From above the threshold, it is an enabling coordination mechanism (Rope). From below it, it is an exclusionary extraction mechanism (Snare). From exactly at the threshold, it is a mixed constraint (Tangled Rope) that both enables and extracts. The analytical observer must recognize both mechanisms are real, not that one perspective is 'correct' and others are false. The gap also reveals an intentionality asymmetry: large parties recognize the threshold's role in preventing coalition formation (coordination function) and consciously maintain it for this reason, while also deriving benefit from its gatekeeping function (extraction). Small parties recognize the gatekeeping function and experience it as intentional exclusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Large beneficiary parties with arbitrage exit (can form coalitions or remain opposition) experience low effective extraction (d ≈ 0.15): they derive institutional benefit from coalition necessity without bearing suppression costs. Small victim parties with trapped exit (no representation if below threshold, no discretionary pathway) experience high effective extraction (d ≈ 0.88): they bear full suppression and exclusion cost. Marginal coalition parties with constrained exit experience moderate extraction (d ≈ 0.62): they clear the threshold but pay kingmaker extraction through disproportionate coalition leverage demands. The directionality asymmetry is structural: the same constraint produces negative χ for beneficiaries (net benefit) and positive χ for victims (net extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the threshold is genuinely a Tangled Rope: it coordinates coalition formation (real function, real benefit) while extracting representation (real cost, real harm). The temptation to misclassify comes from two directions: (1) defenders classify it as pure Rope (just coordination, no extraction) because it serves a genuine function; (2) critics classify it as pure Snare (just extraction, no coordination) because it systematically excludes. Neither classification is correct. The threshold creates both coordination and extraction simultaneously. The mandatrophy resolution requires accepting that the coordination benefit for large parties is real AND the extraction cost for small parties is real. The constraint is not innocent of its gatekeeping function, and the gatekeeping function does not eliminate its coordination service.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_level_incrementalism,
    'Has the threshold (currently 3.25%) been incrementally raised to exclude specific political competitors rather than to achieve general fragmentation prevention?',
    'Historical analysis of threshold adjustments: correlation between raised thresholds and exclusion of specific party ideologies; interviews with legislative committees justifying increases; comparative analysis with other democracies'' threshold levels and their stated rationales',
    'If incremental targeting: classification shifts toward pure Snare (intentional extraction with coordination cover). If alignment with genuine fragmentation concerns: Tangled Rope classification holds as hybrid mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_level_incrementalism, empirical, 'Whether threshold increases target specific competitors').

omega_variable(
    coalition_necessity_counterfactual,
    'Would Israeli coalition politics be substantially more unstable at lower threshold (1.5% or 2%) based on comparative electoral systems analysis?',
    'Comparative study of multi-party democracies with varying thresholds (Germany 5%, Israel 3.25%, Netherlands 0.67%); stability metrics (government durability, legislative fragmentation); coalition formation time analysis',
    'If stability substantially worse: coordination function is genuine, extraction is necessary cost. If stability comparable: threshold is primarily extractive mechanism dressed in stability rhetoric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coalition_necessity_counterfactual, empirical, 'Whether threshold is necessary for coalition stability').

omega_variable(
    representation_concentration_driver,
    'Is the 3.25% threshold the primary driver of representation concentration to 6-8 major parties, or do underlying party system dynamics (ideological polarization, demographic bifurcation) drive concentration regardless of threshold level?',
    'Time-series analysis of party system fragmentation before/after threshold changes (1992: 1.5% → 2% → 3.25% in 2014); modeling of voting behavior under counterfactual threshold levels; international comparison of threshold effects on party system concentration',
    'If threshold is primary driver: removing or lowering it could significantly increase representation diversity. If underlying dynamics are primary: threshold adjustment alone would not substantially improve inclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(representation_concentration_driver, empirical, 'Whether threshold is primary driver of party concentration').

omega_variable(
    intentionality_extraction_asymmetry,
    'Do large parties systematically benefit from threshold maintenance while small parties face extraction, or is this an artifact of electoral luck and demographic distribution?',
    'Historical record of party positions on threshold amendments: large parties'' statements supporting maintenance vs small parties'' stated positions; voting patterns in Knesset on threshold-related legislation; public discourse analysis showing awareness of extraction mechanism',
    'If systematic intentionality and awareness: extraction is deliberate, classification solidifies as Snare with coordination facade. If artifact: Tangled Rope holds as genuine hybrid without malevolent agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_extraction_asymmetry, conceptual, 'Whether threshold extraction is systematic or accidental').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israeli_electoral_threshold_system, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iets_tr_t0, israeli_electoral_threshold_system, theater_ratio, 0, 0.35).
narrative_ontology:measurement(iets_tr_t8, israeli_electoral_threshold_system, theater_ratio, 8, 0.42).
narrative_ontology:measurement(iets_tr_t16, israeli_electoral_threshold_system, theater_ratio, 16, 0.48).

% Extraction over time
narrative_ontology:measurement(iets_be_t0, israeli_electoral_threshold_system, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(iets_be_t8, israeli_electoral_threshold_system, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(iets_be_t16, israeli_electoral_threshold_system, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israeli_electoral_threshold_system, enforcement_mechanism).
narrative_ontology:affects_constraint(israeli_electoral_threshold_system, coalition_kingmaker_leverage).
narrative_ontology:affects_constraint(israeli_electoral_threshold_system, arab_party_representation_barriers).
narrative_ontology:affects_constraint(israeli_electoral_threshold_system, right_left_bloc_polarization).

% DUAL FORMULATION NOTE:
% The electoral threshold coordinates coalition formation (primary constraint) while creating systematic representation barriers (secondary constraint). These could be decomposed into separate stories: threshold_coalition_coordination (ε ≈ 0.25, Rope) and threshold_representation_exclusion (ε ≈ 0.68, Snare). The current story treats them as unified because they are mechanically inseparable — one system produces both effects. Network links identify downstream constraints that depend on threshold structure: coalition kingmaker leverage would be much lower without threshold; Arab party representation barriers are directly created by threshold; bloc polarization is amplified by threshold's forcing function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(israeli_electoral_threshold_system, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
