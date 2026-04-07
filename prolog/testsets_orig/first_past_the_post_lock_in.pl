% ============================================================================
% CONSTRAINT STORY: first_past_the_post_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_past_the_post_lock_in, []).

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
 *   constraint_id: first_past_the_post_lock_in
 *   human_readable: First Past the Post Electoral System Lock-In
 *   domain: political_economy/institutional_design
 *
 * SUMMARY:
 *   First-past-the-post (FPTP) electoral systems create a structural lock-in
 *   that functions simultaneously as a coordination mechanism and an
 *   extraction regime. The system solves the collective action problem of
 *   aggregating dispersed voter preferences into governing coalitions (rope
 *   function), but simultaneously suppresses minority party representation
 *   and enforces strategic voting that constrains voter preference expression
 *   (snare function). The constraint exhibits high theater (0.64) because
 *   much of the system's legitimacy derives from procedural fairness
 *   narratives—auditable vote counting, transparent constituency rules, equal
 *   ballots—that obscure the structural outcome: binary coalition formation.
 *   Over the past 80 years, extractiveness has increased from 0.35 to 0.58 as
 *   third-party movements have proliferated while the mechanical barriers to
 *   their viability have remained constant, forcing ever-larger portions of
 *   the electorate into strategic voting. The lock-in persists through path
 *   dependency: major parties benefit from the status quo during their time
 *   in power, creating symmetric incentives against reform despite rotating
 *   majority positions. Alternative voting systems (ranked-choice,
 *   proportional representation) have demonstrated feasibility in multiple
 *   jurisdictions, yet institutional inertia and incumbent advantage maintain
 *   FPTP in large democracies.
 *
 * KEY AGENTS:
 *   - Disenfranchised Third-Party Voters: Primary victims (powerless/trapped) — face wasted-vote penalty and spoiler stigma; cannot exit binary choice structure
 *   - Policy Minority Movements: Secondary victims (moderate/constrained) — experience dilution of platform positions within major coalitions; high exit costs prevent independent party building
 *   - Incumbent Major Parties: Primary beneficiaries (institutional/arbitrage) — use system to maintain single-party control and suppress competition; can realign coalition strategy without exiting structure
 *   - Opposition Parties: Ambiguous (institutional/constrained) — both beneficiaries when in power and victims when out of power; trapped by binary equilibrium that benefits majority but not minority status
 *   - Electoral Reform Coalition: Organized agents (organized/mobile) — see alternative pathways (ranked-choice, proportional) as feasible and demonstrate exit potential; provide countervailing pressure to lock-in
 *   - Electoral Commission Apparatus: Institutional administrators (institutional/arbitrage) — maintain performative legitimacy infrastructure while system outcomes are determined by structural incentives rather than procedural quality
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing binary equilibrium as mathematical necessity (Duverger's Law, Arrow's Impossibility) rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_past_the_post_lock_in, 0.58).
domain_priors:suppression_score(first_past_the_post_lock_in, 0.68).
domain_priors:theater_ratio(first_past_the_post_lock_in, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_past_the_post_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(first_past_the_post_lock_in, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(first_past_the_post_lock_in, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_past_the_post_lock_in, tangled_rope).
narrative_ontology:human_readable(first_past_the_post_lock_in, "First Past the Post Electoral System Lock-In").
narrative_ontology:topic_domain(first_past_the_post_lock_in, "political_economy/institutional_design").

domain_priors:requires_active_enforcement(first_past_the_post_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_past_the_post_lock_in, major_parties).
narrative_ontology:constraint_beneficiary(first_past_the_post_lock_in, incumbent_winners).
narrative_ontology:constraint_beneficiary(first_past_the_post_lock_in, agenda_gatekeepers).
narrative_ontology:constraint_victim(first_past_the_post_lock_in, third_parties).
narrative_ontology:constraint_victim(first_past_the_post_lock_in, policy_minorities).
narrative_ontology:constraint_victim(first_past_the_post_lock_in, voter_preference_precision).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED THIRD-PARTY VOTER (SNARE) — Structurally unable to exit the two-party equilibrium without losing all bargaining power. Voting for preferred candidate yields zero legislative representation; strategic voting for least-bad major party is extraction camouflaged as choice. Suppression is maximal: wasted vote penalty, spoiler stigma, and vote-splitting dynamics lock voters into binary choice despite multiparty preference landscape.
constraint_indexing:constraint_classification(first_past_the_post_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POLICY MINORITY MOVEMENT (TANGLED ROPE) — Movements aligned with policies unpopular to either major coalition (e.g., strict drug legalization, radical land-value taxation, non-interventionist foreign policy) experience both coordination function and extraction. FPTP aggregates them into major platforms (coordination), but minority positions are diluted or inverted to serve majority coalition logic (extraction). Exit cost is high: building alternative party requires decades of organizing and faces structural barriers (ballot access, debate thresholds, ranked-choice transitions).
constraint_indexing:constraint_classification(first_past_the_post_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT MAJOR PARTY (ROPE) — Experiences FPTP as pure coordination mechanism. The system organizes dispersed voter preferences into governing coalitions and enforces single-party control, which solves the collective action problem of legislation. Party leadership has arbitrage options (adjust platform, realign coalition, run different candidates) without exiting the institutional structure. The system's suppression of alternatives benefits incumbent parties directly.
constraint_indexing:constraint_classification(first_past_the_post_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ELECTORAL REFORM COALITION (SCAFFOLD) — Organized agents (ranked-choice advocates, proportional representation movements, open-list reformers) see FPTP as a temporary institutional form with documented alternative pathways and sunset potential. Mobile exit: jurisdictions in Canada, New Zealand, parts of Australia have transitioned away from FPTP. Suppression is moderate because coalition has visibility and institutional allies. Theater is moderate — reform feels technically possible even though path dependency is high.
constraint_indexing:constraint_classification(first_past_the_post_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CAPTURED OPPOSITION PARTY (TANGLED ROPE) — Opposition parties coordinate voters against incumbents (genuine coordination function) but are structurally trapped by the same two-party equilibrium that benefits majority coalitions. They benefit from FPTP when in power and suffer when out of power, but cannot exit the binary structure without losing all leverage. Constrained exit: reforming to proportional representation would mean ceding current opposition status to multiple microparties. They are both victims and beneficiaries, making extraction asymmetric but coordination genuine.
constraint_indexing:constraint_classification(first_past_the_post_lock_in, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ELECTORAL COMMISSION APPARATUS (PITON) — Administrative infrastructure of FPTP (ballot design, vote counting, constituency delimitation) is substantially performative. The machinery performs legitimacy-conferring functions (auditable counting, procedural fairness) while enforcing structural outcomes (two-party concentration) that are independent of the voting mechanism itself. Theater ratio is high because electoral administrators see their process as degraded — the real outcome (party concentration) is determined by system incentives, not by administrative quality. Maintains itself through institutional inertia rather than functional necessity.
constraint_indexing:constraint_classification(first_past_the_post_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, binary coalition formation is inherent to plurality voting systems: any voting mechanism that selects a single outcome from voter preferences necessarily induces binary clustering around the two largest preference modes. Arrow's Impossibility Theorem and Condorcet Paradox frame this as a natural law of democratic aggregation. However, this perspective risks naturalizing what is a contingent institutional choice — proportional and ranked-choice systems demonstrate that alternative mathematical structures are feasible.
constraint_indexing:constraint_classification(first_past_the_post_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_past_the_post_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(first_past_the_post_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(first_past_the_post_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_past_the_post_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(first_past_the_post_lock_in, TR),
    TR >= 0.70.

:- end_tests(first_past_the_post_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The system concentrates parliamentary representation into two major parties while voter preference landscape is more dispersed. This concentration benefits winners substantially (single-party government) while suppressing minority representation. The acceleration from 0.35 to 0.58 over 80 years reflects increasing polarization of voter preferences without proportional increases in party competition—extractiveness has grown because the gap between preference distribution and representational output has widened. Suppression (0.68): High. Multiple mechanisms enforce binary choice: wasted vote penalty (votes for third parties yield zero seats), spoiler stigma (voting third party blames you for majority defeat), and strategic voting necessity (voting your preference risks worst outcome). These are not trivial barriers—they function as structural suppression of preference expression. Theater ratio (0.64): Moderate-high. Procedural legitimacy narratives (fair counting, transparent rules, equal ballots) obscure mechanical outcomes. The system's legitimacy claims rest on procedural fairness, yet procedural quality is orthogonal to representational outcomes. Electoral administrators perform legitimacy-conferring functions while system structure determines outcomes independently of administrative quality.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence driven by power asymmetry and exit options. The beneficiary (major party with institutional power and arbitrage exits) experiences coordination. The victim (powerless voter with trapped exit) experiences extraction. The intermediate actors (policy movements with constrained exits, reform coalitions with mobile exits) experience hybrid forms. The analytical observer risks naturalizing institutional choices (binary representation) as mathematical limits (voting paradoxes).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by structural position: who benefits, who bears costs, and what exit options exist. Beneficiary major parties have d ≈ 0.10 (full beneficiary status with arbitrage exits)—they experience low or negative effective extraction because the system's suppression benefits them directly. Disenfranchised voters have d ≈ 0.95 (full target status with trapped exits)—they experience maximum effective extraction because they cannot exit without forfeiting all bargaining power. Policy minority movements have d ≈ 0.70 (partial victim status with constrained exits)—high but not maximal extraction because they have some voice within major platforms and some (costly) exit options through third-party organizing. Reform coalitions have d ≈ 0.45 (mixed position with mobile exits)—moderate extraction because they have demonstrated alternative pathways and exit is theoretically available even if costly. The sigmoid f(d) converts these d values into effective power modifiers that produce the perspectival gap: beneficiaries see rope (low χ), victims see snare (high χ), intermediates see tangled rope (moderate χ).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that both rope and snare classifications are structurally accurate from different perspectives. The mandatrophy arises from the system's dual nature: it is simultaneously a high-efficiency coordination mechanism (major parties organized into governing coalitions) and an extraction regime (suppression of minority representation and strategic voting compulsion). The coordination function is real—FPTP does aggregate dispersed preferences and solve the problem of government formation—but it does so through an extractive mechanism that concentrates representation asymmetrically. The analytical observer's natural law perspective (Mountain) is a false summit: binary equilibrium is attributed to mathematical necessity (Duverger's Law, Arrow's Theorem) when it is actually a contingent institutional choice that alternative voting systems have demonstrated can be altered. The resolution: FPTP is genuinely a Tangled Rope with dominant extraction properties (snare-like experience for many agents) but real coordination function (rope-like function for institutional actors). The system's theater ratio (0.64) indicates that much of its legitimacy derives from procedural fairness narratives that obscure this dual nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duvergers_law_necessity,
    'Is Duverger''s Law (mechanical+psychological effects driving two-party equilibrium) an invariant feature of plurality voting or a contingent path-dependent outcome that could be reversed?',
    'Controlled natural experiments comparing jurisdictions that switched voting systems (New Zealand 1993, Canada referendums) and tracking reversion behavior; empirical measurement of mechanical vs psychological effect magnitudes',
    'If invariant: FPTP lock-in is nearly immutable (mountain properties increase). If contingent: alternative voting systems can durably maintain multiparty competition (scaffold properties confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(duvergers_law_necessity, empirical, 'Whether Duverger''s Law effects are invariant or contingent').

omega_variable(
    coordination_vs_extraction_boundary,
    'What portion of the two-party system''s stability represents genuine coordination (efficient legislative coalescence) vs pure extraction (suppression of minority representation)?',
    'Comparative measurement of legislative efficiency (bill passage rate, legislative cycles) and minority representation satisfaction (policy congruence surveys) across FPTP vs proportional systems with similar electoral composition',
    'If coordination >> extraction: FPTP is a high-efficiency rope with unfortunate side effects. If extraction >> coordination: FPTP is a snare disguised as necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Proportion of outcomes due to coordination vs extraction').

omega_variable(
    reform_path_dependency,
    'Is FPTP lock-in mechanical (any alternative system must overcome voting paradoxes) or institutional (party incentives prevent reform even when alternatives are technically superior)?',
    'Analysis of reform attempts: costs, obstacles, party positioning; comparison with success cases (New Zealand, Ireland); modeling of reform barrier composition',
    'If mechanical: reform requires decades and external shock (landscape change in electoral mathematics). If institutional: reform could occur with coordinated party strategy shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_path_dependency, empirical, 'Whether lock-in is mathematical or institutional').

omega_variable(
    strategic_voting_prevalence,
    'What percentage of voters in FPTP systems cast strategic (least-bad major party) votes vs sincere (preferred candidate) votes, and how does this ratio change during crisis periods?',
    'Exit polls, preference surveys, ranked-choice simulation; tracking of strategic voting estimates over electoral cycles and during institutional legitimacy crises',
    'If strategic voting < 20%: extraction is modest, coordination is dominant (rope classification supported). If strategic voting > 50%: extraction is severe, snare classification supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_voting_prevalence, empirical, 'Prevalence of strategic voting behavior').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_past_the_post_lock_in, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fptp_tr_t0, first_past_the_post_lock_in, theater_ratio, 0, 0.48).
narrative_ontology:measurement(fptp_tr_t40, first_past_the_post_lock_in, theater_ratio, 40, 0.58).
narrative_ontology:measurement(fptp_tr_t80, first_past_the_post_lock_in, theater_ratio, 80, 0.64).
narrative_ontology:measurement(fptp_tr_t20, first_past_the_post_lock_in, theater_ratio, 20, 0.54).

% Extraction over time
narrative_ontology:measurement(fptp_be_t0, first_past_the_post_lock_in, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fptp_be_t40, first_past_the_post_lock_in, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(fptp_be_t80, first_past_the_post_lock_in, base_extractiveness, 80, 0.58).
narrative_ontology:measurement(fptp_be_t20, first_past_the_post_lock_in, base_extractiveness, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_past_the_post_lock_in, enforcement_mechanism).
narrative_ontology:affects_constraint(first_past_the_post_lock_in, gerrymandering_lock_in).
narrative_ontology:affects_constraint(first_past_the_post_lock_in, two_party_duopoly).
narrative_ontology:affects_constraint(first_past_the_post_lock_in, strategic_voting_equilibrium).

% DUAL FORMULATION NOTE:
% FPTP lock-in is upstream of gerrymandering effects (which exploit FPTP's single-seat-per-district structure) and two-party duopoly (which emerges from FPTP's mechanical and psychological effects). Strategic voting equilibrium is a downstream behavioral consequence of FPTP's structural incentives. All three related constraints share the coordination_vs_extraction ambiguity (omega_id: coordination_vs_extraction_boundary).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_past_the_post_lock_in, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
