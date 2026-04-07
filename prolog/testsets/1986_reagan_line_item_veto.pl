% ============================================================================
% CONSTRAINT STORY: 1986_reagan_line_item_veto
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1986_reagan_line_item_veto, []).

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
 *   constraint_id: 1986_reagan_line_item_veto
 *   human_readable: Presidential Line-Item Veto Authority (Reagan Era)
 *   domain: governance/executive_power
 *
 * SUMMARY:
 *   The presidential line-item veto is a proposed mechanism to grant the
 *   executive selective power to eliminate individual appropriations items
 *   from spending bills without vetoing the entire legislation. Reagan
 *   administration advocates frame it as a check against 'waste' and
 *   'measures that could not pass on their own merit,' modeling it on
 *   authority used by 43 state governors. The constraint exhibits genuine
 *   structural hybridity: it solves a real coordination problem (presidential
 *   deadlock over logrolling-laden bills) while simultaneously extracting
 *   power from legislative coalitions and dispersing it toward the executive.
 *   The line-item veto is neither pure coordination (Rope) nor pure
 *   extraction (Snare), but a genuine Tangled Rope — it requires active
 *   enforcement (presidential selectivity in veto application) and
 *   simultaneously benefits some agents (the executive, fiscal hawks) while
 *   imposing asymmetric costs on others (legislative minorities,
 *   small-jurisdiction coalitions). The theater ratio (0.35) reflects
 *   moderate performative content: the constraint operates on a real
 *   budgetary mechanism (appropriations), not a purely ceremonial one, but
 *   part of the political claim ('checking waste') exceeds the actual
 *   structural effect (shifting power, not eliminating waste).
 *
 * KEY AGENTS:
 *   - Executive Branch: Primary beneficiary (institutional/arbitrage) — gains selective power without full-bill veto cost; can implement fiscal preferences through item-by-item negotiation
 *   - Legislative Minority: Primary victim (powerless/trapped) — loses leverage point (logrolling coalitions); measures secured through negotiation can be stripped unilaterally
 *   - Small-Jurisdiction Constituencies: Secondary victim (powerless/trapped) — historically protected through logrolling; lose structural mechanism to secure federal investment in local infrastructure and facilities
 *   - Fiscal Discipline Advocates: Beneficiary (organized/constrained) — see constraint as temporary improvement to budget transparency and removal of hidden logrolling costs
 *   - Congressional Majority: Mixed (organized/constrained) — gains discipline enforcement on their own caucus (coordination benefit) but loses coalition flexibility and internal leverage
 *   - Large-State Delegations: Mixed (powerful/mobile) — retain negotiating power with executive but face tighter internal discipline and supermajority coordination requirements for override
 *   - Separation-of-Powers Doctrine: Institutional principle (institutional/arbitrage) — sees line-item veto as degradation of structural tension between branches; persists through inertia rather than function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1986_reagan_line_item_veto, 0.52).
domain_priors:suppression_score(1986_reagan_line_item_veto, 0.48).
domain_priors:theater_ratio(1986_reagan_line_item_veto, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1986_reagan_line_item_veto, extractiveness, 0.52).
narrative_ontology:constraint_metric(1986_reagan_line_item_veto, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(1986_reagan_line_item_veto, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1986_reagan_line_item_veto, tangled_rope).
narrative_ontology:human_readable(1986_reagan_line_item_veto, "Presidential Line-Item Veto Authority (Reagan Era)").
narrative_ontology:topic_domain(1986_reagan_line_item_veto, "governance/executive_power").

domain_priors:requires_active_enforcement(1986_reagan_line_item_veto).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1986_reagan_line_item_veto, executive_branch).
narrative_ontology:constraint_beneficiary(1986_reagan_line_item_veto, fiscal_discipline_advocates).
narrative_ontology:constraint_victim(1986_reagan_line_item_veto, legislative_minority_protection).
narrative_ontology:constraint_victim(1986_reagan_line_item_veto, local_appropriations_coalition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGISLATIVE MINORITY (SNARE) — Minority party members and backbench legislators face maximum extraction without exit. Line-item veto removes their leverage point (logrolling coalitions that require broad support). A measure they secured through negotiation can be eliminated unilaterally by the President. Suppression is structural: Congress can override, but only with supermajority discipline that the minority cannot enforce. The constraint removes their ability to embed preferred measures inside omnibus bills.
constraint_indexing:constraint_classification(1986_reagan_line_item_veto, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESSIONAL MAJORITY (TANGLED ROPE) — The majority party experiences coordination benefit (line-item veto removes cover for wasteful spending that embarrasses them) alongside extraction (they lose the ability to use logrolling to bind coalition members who might otherwise defect). They retain override power but face supermajority costs. The constraint enforces fiscal discipline on their own caucus, which is partially desired and partially resented.
constraint_indexing:constraint_classification(1986_reagan_line_item_veto, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE BRANCH (ROPE) — Primary beneficiary experiencing pure coordination gain. Line-item veto enables the President to implement fiscal policy preferences without confrontation over entire bills. The executive can now negotiate from a position of selective acceptance rather than binary sign/veto. The constraint solves a genuine coordination problem: how to reject wasteful measures without creating legislative deadlock.
constraint_indexing:constraint_classification(1986_reagan_line_item_veto, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SMALL-JURISDICTION CONSTITUENCIES (SNARE) — Rural, small-state, and economically disadvantaged constituencies lose structural protection. Their representatives historically secured local benefits (infrastructure, military base funding, research facilities) through coalition participation and logrolling. Line-item veto allows the President to strip these items while maintaining the bill. These constituencies have no alternative mechanism to secure federal investment — the logrolling coalition was their only leverage point.
constraint_indexing:constraint_classification(1986_reagan_line_item_veto, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: LARGE-STATE CONGRESSIONAL DELEGATIONS (TANGLED ROPE) — States with powerful delegations experience mixed effects. They retain sufficient scale to negotiate directly with the executive (mobile exit option), but their internal coalition discipline tightens. They can credibly threat override (supermajority power concentrated in large-state members), creating a coordination mechanism but also extracting loyalty from their own members.
constraint_indexing:constraint_classification(1986_reagan_line_item_veto, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANTI-DEFICIT COALITION (SCAFFOLD) — Fiscal hawks and budget-transparency advocates see line-item veto as a temporary structural improvement with a sunset. Their hope is that removing the hidden costs of logrolling will increase public awareness of actual spending distribution, enabling more explicit legislative choices. Suppression is moderate because the coalition has organizational capacity. But this is a temporary solution: if presidential line-item veto authority persists, it becomes normal power distribution rather than a correction.
constraint_indexing:constraint_classification(1986_reagan_line_item_veto, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: SEPARATION-OF-POWERS DOCTRINE (PITON) — Constitutional law sees line-item veto as a degraded institutional principle. The traditional account maintains that the President must accept or reject bills in their entirety — this entire-bill rule is what enables Congress to force presidential choice between compromise and deadlock. Line-item veto weakens this structural tension, making the President less dependent on legislative cooperation. The doctrine persists as a formal principle even as line-item veto undermines it, hence piton (theater_ratio 0.35 reflects moderate performative content — separation-of-powers rhetoric remains even as the actual separation shifts).
constraint_indexing:constraint_classification(1986_reagan_line_item_veto, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, line-item veto is a hybrid that simultaneously coordinates (removes legislative deadlock over waste) and extracts (concentrates power toward the executive, away from dispersed coalition-building). The constraint is not immutable — 43 states deploy it successfully, suggesting it is a contingent governance choice rather than a natural law of executive-legislative relations. The analytical perspective reveals that whether line-item veto is 'good governance' or 'executive power grab' depends entirely on the observer's position in the extraction flow.
constraint_indexing:constraint_classification(1986_reagan_line_item_veto, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1986_reagan_line_item_veto_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1986_reagan_line_item_veto, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1986_reagan_line_item_veto, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(1986_reagan_line_item_veto, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(1986_reagan_line_item_veto, TR),
    TR >= 0.70.

:- end_tests(1986_reagan_line_item_veto_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint concentrates power toward the executive and away from logrolling coalitions. The original Reagan framing minimizes this by emphasizing 'waste elimination,' but the structural effect is clear: legislative minorities and small constituencies lose their primary leverage mechanism. The 0.52 value reflects that the extraction is real but not absolute — Congress retains override authority (supermajority) and retains the ability to refuse to pass bills. If override supermajority discipline fails (omega_1), extractiveness would rise to 0.65+. Suppression (0.48): Moderate. Structural barriers to override include supermajority requirements and legislative time scarcity. But suppression is not total — Congress can theoretically override on every single item if it maintains discipline. The 0.48 value reflects that the constraint creates real barriers but not immobility. Theater ratio (0.35): Moderate-low. The constraint operates on a genuine mechanism (appropriations), so the theater is not dominant. However, part of the political justification ('eliminating waste') is rhetorical cover for the actual effect (shifting power). If the constraint is deployed to systematically veto small-jurisdiction items (omega_3), theater would rise as the 'waste elimination' framing becomes transparently partisan.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival disagreement occurs between the executive (Rope) and the legislative minority (Snare). The executive experiences the constraint as solving a coordination problem — they gain the ability to implement fiscal preferences without threatening the entire budget. The legislative minority experiences the same structural change as pure extraction — they lose their primary mechanism (logrolling participation) to secure measures. These are the same constraint producing opposite classifications because the agents occupy opposite structural positions in the extraction flow. The resolution is not 'which classification is correct?' but 'the constraint IS a Tangled Rope because it combines genuine coordination function (removing deadlock) with asymmetric extraction (dispersing power toward executive, away from coalitions).'
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality (d ≈ 0.10): Executive branch has full arbitrage exit — they can negotiate selectively or refuse to spend, experiencing low directionality toward the constraint. Victim directionality (d ≈ 0.90): Legislative minorities and small constituencies have trapped exit — no alternative mechanism to secure appropriations, experiencing maximum directionality as targets of extraction. Supermajority override option (d ≈ 0.55): Congressional actors experience moderate directionality — they can theoretically override but face supermajority coordination costs. Fiscal hawks (d ≈ 0.40): They are beneficiaries of the constraint (it removes hidden waste) but face constrained exit if the constraint is deployed in partisan fashion (omega_3). The directionality derivation chain reflects that 'constrained exit + beneficiary status' produces lower d than 'constrained exit + victim status,' creating measurable perspectival gaps through the f(d) sigmoid function.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is NOT resolved (mandatrophy_resolved: false) because the constraint's extraction function is still contested and context-dependent. The constraint is claimed as pure coordination ('eliminating waste') but operates as Tangled Rope because asymmetric power shifts are embedded in the mechanism. The irresolvable question: Is presidential line-item veto a neutral tool (Rope) or an extraction mechanism (Snare/Tangled Rope)? The answer depends on downstream usage (omega_3: does the President deploy it to systematically disadvantage opposition areas?). The constraint cannot resolve its own mandatrophy without additional enforcement — an independent accounting of veto patterns would be required to determine whether extraction is incidental (true Tangled Rope) or systematic (closer to Snare). Until that analysis exists, the constraint lives in genuine hybridity: it IS a Tangled Rope, but agents disagree on whether it is Rope-primary or Snare-primary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_override_supermajority_discipline,
    'Can Congress consistently achieve supermajority discipline to override presidential line-item vetoes on economically important measures?',
    'Historical data from state-level line-item veto usage: override rates for line items vs full-bill vetoes; analysis of whether override coalitions hold or dissolve',
    'If override discipline is high: suppression remains moderate, constraint remains Tangled Rope. If override discipline fails: suppression rises to 0.65+, constraint reclassifies to Snare across more perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_override_supermajority_discipline, empirical, 'Whether Congress can achieve supermajority veto override discipline').

omega_variable(
    hidden_logrolling_reemergence,
    'If line-item veto authority is granted, does logrolling migrate to non-appropriations legislation, or does it genuinely disappear?',
    'Comparative analysis of legislative mechanisms before and after line-item veto adoption at state level; tracking of bill complexity, rider insertion, and coalition formation patterns',
    'If logrolling remerges: the constraint removes a visible mechanism but doesn''t solve the underlying coordination problem — extractiveness remains high but visibility drops (theater increases). If logrolling genuinely diminishes: the constraint does reduce hidden extraction (theater drops, extractiveness drops to 0.35-0.40).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hidden_logrolling_reemergence, empirical, 'Whether logrolling reemerges in non-appropriations legislation').

omega_variable(
    executive_line_item_pattern_bias,
    'Does presidential line-item veto usage correlate with partisan geography or budgetary principle?',
    'Analysis of state-level line-item veto usage: distribution of vetoed items by geography (small-state vs large-state), party of executive vs legislature, fiscal ideology of veto justifications',
    'If usage is partisan/geographic: line-item veto is a mechanism for extracting resources from opposition areas (extractiveness rises to 0.60+, suppression rises). If usage is principled (consistent budget rules): constraint remains true Tangled Rope balancing coordination and fairness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(executive_line_item_pattern_bias, empirical, 'Whether line-item veto usage is partisan/geographic or principled').

omega_variable(
    small_constituency_adjustment_mechanism,
    'What alternative mechanism protects small-jurisdiction interests if logrolling is unavailable?',
    'Institutional design analysis: can direct executive negotiation, formula-based appropriations, or centralized competitive grant processes replace logrolling without creating new extraction mechanisms?',
    'If no alternative exists: the constraint permanently disempowers small constituencies, reclassifying from Snare to a degraded state (modified Mountain — immutable dispowerment). If alternatives emerge: constraint can stabilize as a true Tangled Rope with new equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(small_constituency_adjustment_mechanism, conceptual, 'Alternative protection mechanisms for small-constituency interests').

omega_variable(
    constitutional_delegation_scope,
    'Is line-item veto authority constitutional as a statutory delegation, or does it require a constitutional amendment?',
    'Constitutional law analysis and Supreme Court review of delegation doctrine (post-Schechter, post-Mistretta); historical precedent for presidential appropriations management authority',
    'If constitutional: the constraint is durable and enforceable. If unconstitutional: the constraint is a spectacular failure — Congress grants authority the President cannot exercise, creating theater without function (piton reclassification with theater rising to 0.75+).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_delegation_scope, conceptual, 'Constitutional status of statutory line-item veto delegation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1986_reagan_line_item_veto, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(1986_tr_t0, 1986_reagan_line_item_veto, theater_ratio, 0, 0.25).
narrative_ontology:measurement(1986_tr_t3, 1986_reagan_line_item_veto, theater_ratio, 3, 0.28).
narrative_ontology:measurement(1986_tr_t6, 1986_reagan_line_item_veto, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(1986_be_t0, 1986_reagan_line_item_veto, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(1986_be_t3, 1986_reagan_line_item_veto, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(1986_be_t6, 1986_reagan_line_item_veto, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1986_reagan_line_item_veto, enforcement_mechanism).
narrative_ontology:affects_constraint(1986_reagan_line_item_veto, legislative_logrolling_dependency).
narrative_ontology:affects_constraint(1986_reagan_line_item_veto, congressional_majority_coalition_formation).

% DUAL FORMULATION NOTE:
% Line-item veto is downstream of logrolling-dependent legislative processes and upstream of shifts in majority coalition discipline. Separate stories would address (1) the structural logic of logrolling itself (how it solves coalition problems) and (2) the mechanisms by which line-item veto alters coalition formation. This story focuses on the veto mechanism as a hybrid constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
