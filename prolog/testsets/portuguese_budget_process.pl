% ============================================================================
% CONSTRAINT STORY: portuguese_budget_process
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_portuguese_budget_process, []).

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
 *   constraint_id: portuguese_budget_process
 *   human_readable: Portuguese Constitutional Budget Process Constraint
 *   domain: political_economy/fiscal_governance
 *
 * SUMMARY:
 *   The Portuguese budget process embodies a constitutional constraint that
 *   distributes fiscal authority asymmetrically between the executive and
 *   parliament. The Portuguese Constitution (Article 108) grants the
 *   government monopoly over budget proposal and amendment, while parliament
 *   retains formal voting authority but cannot initiate fiscal measures. This
 *   creates a structural hybrid: genuine coordination necessity during
 *   coalition periods (supermajorities are required to pass budgets, forcing
 *   coalition negotiation) alongside extractive executive dominance (the
 *   executive controls both initial design and amendment scope). The
 *   constraint exhibits different classifications across observer positions:
 *   opposition parties experience it as a snare (trapped in binary choice:
 *   accept executive budget or force constitutional dissolution), coalition
 *   partners experience tangled rope (coordinating with government while
 *   accepting subordinate amendment authority), the executive experiences
 *   rope (budget design is their core coordination function), and reform
 *   advocates see a scaffold (constitutional amendments and EU integration
 *   creating pressure for structural change). Theater has increased over the
 *   measurement interval as parliamentary debate has become more performative
 *   relative to genuine fiscal deliberation, reflecting the constraint's
 *   degradation toward a piton structure.
 *
 * KEY AGENTS:
 *   - Executive Branch (Government): Primary beneficiary (institutional/arbitrage) — monopoly over budget proposal, amendment authority, veto power over parliamentary modifications
 *   - Minority Parliamentary Opposition: Primary victim (powerless/trapped) — constitutionally unable to initiate alternative budgets; forced choice between accepting executive proposal or triggering dissolution
 *   - Supermajority Coalition Partners: Secondary beneficiary/victim (organized/constrained) — genuine power to reject budgets but constrained by coalition dissolution risk and electoral dependency; negotiate modifications within executive-designed scope
 *   - Fiscal Transparency Advocates: Victim (moderate/constrained) — executive proposal monopoly reduces parliamentary scrutiny capacity and limits public deliberation on fiscal alternatives
 *   - Constitutional Reform Coalition: Analytical observer (organized/constrained) — EU requirements and democratic norms pressure toward expanded parliamentary budgetary authority
 *   - Budget Administration Technical Apparatus: Institutional actor (institutional/arbitrage) — maintains formal parliamentary review process that is largely performative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(portuguese_budget_process, 0.52).
domain_priors:suppression_score(portuguese_budget_process, 0.58).
domain_priors:theater_ratio(portuguese_budget_process, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(portuguese_budget_process, extractiveness, 0.52).
narrative_ontology:constraint_metric(portuguese_budget_process, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(portuguese_budget_process, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(portuguese_budget_process, tangled_rope).
narrative_ontology:human_readable(portuguese_budget_process, "Portuguese Constitutional Budget Process Constraint").
narrative_ontology:topic_domain(portuguese_budget_process, "political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(portuguese_budget_process).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(portuguese_budget_process, executive_branch).
narrative_ontology:constraint_beneficiary(portuguese_budget_process, incumbent_government).
narrative_ontology:constraint_victim(portuguese_budget_process, minority_parliament_representatives).
narrative_ontology:constraint_victim(portuguese_budget_process, fiscal_transparency).
narrative_ontology:constraint_victim(portuguese_budget_process, long_term_budgetary_reform).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY PARLIAMENTARY OPPOSITION (SNARE) — Trapped within constitutional constraints that heavily favor executive budgetary authority. Cannot exit the budget process; bound by constitutional requirement to vote. No effective veto power despite formal participation. Maximum extraction: forced acceptance of executive-authored budgets or face constitutional dissolution and electoral loss.
constraint_indexing:constraint_classification(portuguese_budget_process, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUPERMAJORITY COALITION PARTNERS (TANGLED ROPE) — Organized political groups with formal coalition arrangements. Experience coordination function (need to align fiscal priorities with governing coalition) alongside asymmetric extraction (executive retains final authority and controls agenda). Constrained by coalition dissolution risk and electoral dependency. Mixed relationship: genuine coordination necessity paired with extractive agenda control by executive.
constraint_indexing:constraint_classification(portuguese_budget_process, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE EXECUTIVE BRANCH (ROPE) — Institutional beneficiary that designs and enforces the budget process itself. Experiences the constraint as coordination mechanism: drafting, presenting, and defending budgets is their core function. Exit options are maximal (can rewrite process within constitutional bounds, can negotiate coalition terms). Net beneficiary from the budget architecture.
constraint_indexing:constraint_classification(portuguese_budget_process, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM ADVOCATES (SCAFFOLD) — Analytical observers and reform-oriented institutional actors see the budget process as a temporary institutional arrangement with a built-in sunset: constitutional amendments expanding parliamentary budgetary authority, EU fiscal integration requirements, and shifting democratic norms create pressure for structural reform. Theater is high (performative votes, symbolic parliamentary debate) but the constraint is seen as having finite duration and clear exit pathways through formal amendment.
constraint_indexing:constraint_classification(portuguese_budget_process, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 5: TECHNICAL BUDGET ADMINISTRATION (PITON) — Career civil servants and budget technicians maintain formal budget review processes that are largely performative. Constitutional requirements for parliamentary committee review and budgetary debate persist, but the real budget decisions have been made by executive and coalition partners before parliamentary consideration. The technical apparatus continues through institutional inertia despite low functional control.
constraint_indexing:constraint_classification(portuguese_budget_process, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL REALIST VIEW (MOUNTAIN) — From a civilizational perspective, parliamentary budget authority requires stable supermajorities to overcome executive veto and proposal power. This is presented as an immutable constitutional law: single-chamber legislatures with executive budgetary proposal monopoly cannot control spending without supermajority coalition. However, the mountain classification is a false summit — many parliamentary democracies achieve tighter budget control with similar constitutional structures through different norms and procedural design.
constraint_indexing:constraint_classification(portuguese_budget_process, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(portuguese_budget_process_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(portuguese_budget_process, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(portuguese_budget_process, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(portuguese_budget_process, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(portuguese_budget_process, TR),
    TR >= 0.70.

:- end_tests(portuguese_budget_process_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The government's monopoly over budget proposal and amendment authority creates asymmetric extraction, but the necessity of coalition agreement in most periods (2015–2019, 2019–2022) provides real negotiating leverage for coalition partners. This moderates pure extraction. The value has increased over the interval (0.38→0.52) reflecting the 2022–2026 supermajority government's ability to exclude coalition negotiation entirely, increasing extractive capacity. Suppression (0.58): Significant but not maximal. Opposition parties face constitutional constraints (cannot initiate budgets) and procedural barriers (limited amendment window, executive control of agenda), but retain formal voting authority and can force public debate. Coalition partners can reject budgets at the cost of dissolution. The suppression reflects structural constraints rather than coercive enforcement. Theater (0.65): Moderate-high and rising. Parliamentary budget committees conduct formal review, but real decisions occur in government and coalition negotiations before parliamentary consideration. The increase from 0.58 to 0.65 reflects growing disconnect between formal parliamentary procedure and actual budget determination. Claimed type (Tangled Rope): The constraint exhibits both coordination function (coalition budgeting requires government proposal and parliamentary approval) and asymmetric extraction (executive controls proposal scope and amendment authority). Both mechanisms are structurally present.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's hybrid nature. The executive sees pure coordination — drafting and defending budgets is legitimate governance. The opposition sees snare — constitutionally unable to propose alternatives. Coalition partners see tangled rope — they coordinate with government but operate within executive-designed scope. The analytical observer sees either a scaffold (constitutional amendments weakening executive dominance) or a mountain (false summit claiming that executive proposal monopoly is an immutable feature of parliamentary democracies). The gap between snare and rope perspectives is particularly diagnostic: it shows that political power matters more than formal constitutional authority. Coalition partners' ability to reject budgets gives them exit options that opposition lacks, shifting them from trapped to constrained, and from snare to tangled rope. This perspectival gap is identical to regulatory capture: the institutional actors (coalition partners, government) experience coordination and negotiation; the powerless actors (opposition, fiscal transparency) experience extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural position relative to the budget constraint. The executive derives d≈0.10 (beneficiary with arbitrage options — can redesign budget authority rules within constitutional bounds). Coalition partners derive d≈0.45 (both benefits and costs — access to fiscal authority paired with subordinate amendment power). Opposition derives d≈0.92 (victim with trapped exit — cannot initiate budgets, cannot exit parliamentary system, cannot reject without triggering dissolution and electoral loss). Fiscal transparency derives d≈0.85 (victim of reduced public deliberation capacity, constrained exit options). These directionality values feed into the chi formula χ = ε × f(d) × σ(S), where the sigmoid f(d) amplifies extraction experienced by trapped agents (opposition) and dampens extraction for beneficiaries (government).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by distinguishing coordination from extraction across observer positions. The government's perspective (rope) legitimately describes budget design as coordination — the process of drafting, defending, and implementing fiscal policy is genuinely coordinative. The opposition's perspective (snare) equally legitimately describes the same constraint as pure extraction — they have no say in design and cannot reject. The tangled rope classification at the coalition partner level is the diagnostic signal: both mechanisms are present in a single structural arrangement. The constitutional realist mountain view ('executive proposal monopoly is inherent to parliamentary systems') is a false summit — many parliaments achieve tighter budget control with similar or stronger executive proposal powers through different procedural norms. The scaffold view is not aspirational: EU fiscal integration and constitutional amendment pressure are real structural forces. The theater ratio's increase (0.58→0.65) signals that the constraint is degrading toward piton: the formal parliamentary process persists but controls less actual fiscal outcome. The mandatrophy is thus resolved by showing that all six types are valid perspectival readings, but the movement toward piton and the existence of real scaffold dynamics distinguish this from a pure snare that would be mischaracterized as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coalition_stability_versus_extraction,
    'Is executive budgetary dominance primarily an extraction mechanism or a necessary coordination cost of coalition governance?',
    'Comparative analysis of budget outcomes across different coalition structures; measurement of fiscal divergence between executive proposal and final enacted budget; assessment of whether coalition partners exercise substantive modification power',
    'If extraction-dominant: tangled rope classification holds with high χ. If coordination-necessary: classification should shift toward rope with lower χ, indicating that executive authority is a legitimate coordination mechanism rather than asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_stability_versus_extraction, empirical, 'Whether executive dominance is extractive or coordinatively necessary').

omega_variable(
    parliamentary_veto_capacity,
    'Can parliamentary opposition meaningfully modify budgets through procedural obstruction or do constitutional provisions make executive proposals effectively veto-proof?',
    'Historical analysis of budget amendments during minority government periods; measurement of modification rates and amendment adoption rates; assessment of whether opposition amendments reach committee or floor votes',
    'If opposition has veto capacity: snare classification is incorrect; minority status becomes constrained rather than trapped. If veto-proof: snare classification confirmed; suppression metric reflects genuine inability to block executive proposals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parliamentary_veto_capacity, empirical, 'Whether parliamentary opposition holds meaningful veto power').

omega_variable(
    eu_fiscal_integration_timeline,
    'How quickly will EU fiscal integration requirements force substantive changes to national budget processes, and will such requirements weaken or strengthen executive dominance?',
    'Monitoring of EU fiscal governance evolution (NextGenerationEU, European Fiscal Board proposals); assessment of whether EU constraints redistribute budgetary authority toward parliament or toward technocratic institutions',
    'If EU integration accelerates parliament''s role: scaffold sunset is real and accelerating. If EU integration strengthens executive technocratic authority: constraint may shift from tangled rope toward snare as both parliament and executive lose control to EU requirements.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(eu_fiscal_integration_timeline, empirical, 'Timeline and direction of EU fiscal governance impact on Portuguese budget process').

omega_variable(
    minority_government_frequency,
    'Is the Portuguese political system structurally converging toward majority coalitions or minority governments? How does this frequency distribution affect the average extractiveness experienced by parliament?',
    'Long-term analysis of coalition formation patterns (1987–2026); measurement of minority government duration and frequency; correlation of government type with budgetary modifications and constitutional amendments',
    'If converging toward coalitions: tangled rope classification reflects the typical case. If converging toward minority governments: snare classification becomes the modal case, and the average extractiveness increases across the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_government_frequency, empirical, 'Structural frequency of minority vs. majority governments in Portuguese politics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(portuguese_budget_process, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pb_tr_t0, portuguese_budget_process, theater_ratio, 0, 0.58).
narrative_ontology:measurement(pb_tr_t5, portuguese_budget_process, theater_ratio, 5, 0.62).
narrative_ontology:measurement(pb_tr_t10, portuguese_budget_process, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(pb_be_t0, portuguese_budget_process, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(pb_be_t5, portuguese_budget_process, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(pb_be_t10, portuguese_budget_process, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(portuguese_budget_process, enforcement_mechanism).
narrative_ontology:affects_constraint(portuguese_budget_process, eu_fiscal_governance_compliance).
narrative_ontology:affects_constraint(portuguese_budget_process, portuguese_coalition_formation_dynamics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(portuguese_budget_process, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
