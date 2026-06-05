% ============================================================================
% CONSTRAINT STORY: sotu_1989_bush_line_item_veto_proposal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1989_bush_line_item_veto_proposal, []).

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
 *   constraint_id: sotu_1989_bush_line_item_veto_proposal
 *   human_readable: Presidential Line-Item Veto Authority (1989 Bush Proposal)
 *   domain: governance/constitutional_executive_power
 *
 * SUMMARY:
 *   The line-item veto proposal emerges from structural tension in the
 *   appropriations process: Congress controls the purse via appropriations
 *   bills but the President holds the all-or-nothing veto, creating two
 *   opposing leverage points. Presidents (from Reagan onward) sought to shift
 *   to item-level veto, framing it as fiscal discipline mechanism. Congress
 *   resisted, defending committee authority and distributive bargaining. The
 *   constraint exhibits dual nature: genuine coordination function (enabling
 *   President to strike items without rejecting entire bills, reducing
 *   wasteful spending) paired with asymmetric power extraction (shifting
 *   item-level authority from Congress to executive). The theater ratio
 *   (0.48) reflects moderate performative content — 'fiscal discipline'
 *   framing obscures real objective of executive power expansion, yet some
 *   genuine budget coordination does occur. Extractiveness rises from 0.35
 *   (initial proposal) to 0.52 (after state implementations demonstrate
 *   partisan veto patterns), showing accumulating evidence of executive power
 *   rather than pure fiscal discipline.
 *
 * KEY AGENTS:
 *   - Executive Branch: Primary beneficiary (institutional/arbitrage) — gains item-level veto authority and selective power over appropriations without losing ability to sign bills
 *   - Congressional Committee Authority: Primary victim (powerful/constrained) — loses control over item-level appropriations trades, constrained by executive veto threat
 *   - Backbench Legislators: Secondary victim (powerless/trapped) — lose distributive bargaining leverage over district-specific spending items; cannot exit without abandoning legislative effectiveness
 *   - Fiscal Reform Coalition: Organized coalition (organized/constrained) — advocates for spending discipline see line-item veto as temporary reform mechanism; expect sunset as budgeting process reforms mature
 *   - Congressional Leadership: Mixed position (organized/mobile) — retain negotiating leverage with executive and can condition support on other priorities; experience mixed coordination and extraction
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing power reallocation as separation-of-powers necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1989_bush_line_item_veto_proposal, 0.52).
domain_priors:suppression_score(sotu_1989_bush_line_item_veto_proposal, 0.58).
domain_priors:theater_ratio(sotu_1989_bush_line_item_veto_proposal, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1989_bush_line_item_veto_proposal, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1989_bush_line_item_veto_proposal, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_1989_bush_line_item_veto_proposal, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1989_bush_line_item_veto_proposal, tangled_rope).
narrative_ontology:human_readable(sotu_1989_bush_line_item_veto_proposal, "Presidential Line-Item Veto Authority (1989 Bush Proposal)").
narrative_ontology:topic_domain(sotu_1989_bush_line_item_veto_proposal, "governance/constitutional_executive_power").

domain_priors:requires_active_enforcement(sotu_1989_bush_line_item_veto_proposal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1989_bush_line_item_veto_proposal, executive_branch).
narrative_ontology:constraint_beneficiary(sotu_1989_bush_line_item_veto_proposal, fiscal_discipline_advocates).
narrative_ontology:constraint_victim(sotu_1989_bush_line_item_veto_proposal, congressional_committee_authority).
narrative_ontology:constraint_victim(sotu_1989_bush_line_item_veto_proposal, legislative_distributive_bargaining).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BACKBENCH LEGISLATOR (SNARE) — Junior members lose negotiating leverage over district-specific appropriations. Cannot exit the constraint without abandoning legislative effectiveness; committee seniority and distributive bargaining are the primary mechanism through which backbenchers deliver constituent services. Line-item veto shifts all item-level power to the executive, leaving backbenchers with no recourse. Maximum extraction from the perspective of a member whose value to their district depends on winning budget items.
constraint_indexing:constraint_classification(sotu_1989_bush_line_item_veto_proposal, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMITTEE CHAIR (TANGLED ROPE) — Powerful position but constrained by executive veto threat. Committee chairs retain negotiating leverage over bills as wholes but lose authority over item-level trades. They benefit from the budgeting coordination function — appropriations committees allocate resources across competing priorities — but also bear extraction as item-level control shifts to the executive. Mixed coordination and extraction.
constraint_indexing:constraint_classification(sotu_1989_bush_line_item_veto_proposal, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE BRANCH (ROPE) — Gains selective veto authority without losing the ability to sign bills. Item-level veto is a pure coordination gain from the executive perspective: it enables the President to pass the appropriations bill while striking items deemed wasteful, eliminating the all-or-nothing veto threat that Congress exploits. The executive experiences this as solving a collective action problem, not extraction.
constraint_indexing:constraint_classification(sotu_1989_bush_line_item_veto_proposal, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FISCAL REFORM COALITION (SCAFFOLD) — Organized advocates for spending discipline (OMB, GAO, fiscal hawks) see line-item veto as a temporary mechanism to curb distributive bargaining excess during periods of high deficits. The sunset logic is implicit: if spending discipline reforms succeed and appropriations process reforms take hold, the need for line-item veto diminishes. Coalition members are constrained by political feasibility but see an exit path through structural reform of the budgeting process.
constraint_indexing:constraint_classification(sotu_1989_bush_line_item_veto_proposal, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONGRESSIONAL LEADERSHIP (TANGLED ROPE) — Leadership has high-level negotiating power with the executive and can condition support for other initiatives on resistance to line-item veto. They experience both the coordination function (budget bills do get passed, coordination happens) and the extraction (item-level power shifts away). Leadership is mobile — they can choose legislative strategies, form coalitions, leverage other political issues — but the structural reality is mixed: they retain some control but cede some authority.
constraint_indexing:constraint_classification(sotu_1989_bush_line_item_veto_proposal, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SEPARATION OF POWERS VIEW (MOUNTAIN) — From a constitutional theory perspective, the line-item veto is sometimes framed as an immutable feature of separated government: if Congress has power over the purse, the executive must have means to contest it, and item-level veto is the natural logical counterpart. However, the Constitution explicitly grants Congress appropriations power and the President the option of signing or rejecting bills in whole — no enumeration of item-level veto exists. The mountain classification is a false summit: the 'natural law' framing naturalizes a contested constitutional power reallocation.
constraint_indexing:constraint_classification(sotu_1989_bush_line_item_veto_proposal, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1989_bush_line_item_veto_proposal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1989_bush_line_item_veto_proposal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1989_bush_line_item_veto_proposal, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1989_bush_line_item_veto_proposal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sotu_1989_bush_line_item_veto_proposal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint shifts item-level appropriations authority from Congress to the executive, reducing Congressional committee power and backbench distributive leverage. However, extractiveness is not maximal (0.66+) because Congress retains the ability to override vetoes (with supermajority), and empirically, legislatures adapt by restructuring appropriations bills. The 0.52 reflects that the power shift is real and asymmetric but not absolute — Congress can develop counter-leverage. Suppression (0.58): Moderate-high. Significant barriers to Congressional resistance include: presidential popularity, deficit politics favoring executive discipline framing, backbencher isolation (individual members cannot defend their items against executive veto), and strategic difficulty of organizing supermajority override. However, suppression is not total because Congressional leadership can condition support on other issues and can frame item-level veto as unconstitutional power grab. Theater ratio (0.48): Moderate. The 'fiscal discipline' framing is substantially performative — partisan veto patterns in state contexts show executives using item-level veto for political targeting, not efficiency. But genuine budget coordination does occur — Presidents do sometimes use item-level veto to strike what they judge as wasteful items, not purely for partisan advantage. The ratio of performative to functional content is moderate.
 *
 * PERSPECTIVAL GAP:
 *   The original research group (executive) sees rope — a coordination mechanism enabling efficient budget vetoes. Fiscal discipline advocates see scaffold — a temporary reform tool with sunset as budgeting reform matures. Committee chairs see tangled rope — mixed coordination (bills do get passed) and extraction (item-level power shifts). Congressional leaders see tangled rope differently (organized/mobile) — they retain high-level leverage and can condition support on other priorities. Backbenchers see snare — they are trapped in a committee system where item-level veto eliminates their primary leverage mechanism. The analytical observer risks mountain — framing item-level veto as a natural corollary of separated government — but the structural data shows this is false summit: the Constitution does not enumerate item-level veto, and its presence depends on legislative/constitutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's d value derives from the agent's structural relationship to the power shift. Beneficiaries (executive) have arbitrage options — they can negotiate across multiple issues, leverage public opinion, extract concessions on unrelated legislation. Victims (backbenchers) are trapped — they depend on committee distributive bargaining and have no substitute source of district value. Intermediate actors (committee chairs, Congressional leadership) have constrained or mobile options, respectively, placing them between the extremes. The suppression metric (0.58) reflects real barriers to Congressional resistance: deficit politics, presidential popularity, difficulty organizing supermajority overrides, and strategic isolation of individual backbenchers. The extractiveness metric (0.52) reflects that the power shift is real but not absolute — Congress can adapt, override, or condition support.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE VALIDATION: The constraint satisfies all three gates for tangled rope classification. First, it has a genuine coordination function: Presidents can strike items without rejecting entire bills, enabling more efficient budget negotiation and reducing all-or-nothing veto standoffs. Second, it has asymmetric extraction: item-level authority shifts from Congress to the executive, reducing Congressional committee power and distributive leverage. Third, it requires active enforcement: the executive must actively use item-level veto; Congress must actively resist or condition support. The mandatrophy is resolved by recognizing that the same structural mechanism that enables coordination (selective item veto) also enables extraction (shifting power away from Congress). The constraint is not a snare disguised as rope; it is genuinely hybrid. The theater ratio (0.48) is low enough that the coordination function is real, not merely performative cover for extraction. The empirical question of whether executives use item-level veto for fiscal discipline (Rope) or political targeting (Snare) remains unresolved — see omegas 2 and 4.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_authority_ambiguity,
    'Does the President possess inherent constitutional authority to execute line-item veto, or is it a pure statutory/legislative power that requires Constitutional amendment?',
    'Supreme Court adjudication (Clinton v. City of New York, 1998) resolved as statutory authority only — Congress cannot delegate item-level veto to the executive without constitutional amendment. The constraint''s nature changed from a governance coordination mechanism to a constitutional power struggle.',
    'If executive authority: constraint is constitutional separation-of-powers coordination mechanism (Rope, with mountain elements). If legislative delegation only: constraint is a statutory power reallocation requiring supermajority support (Snare from Congressional perspective, requiring active legislative coalition).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_authority_ambiguity, conceptual, 'Whether line-item veto is constitutional executive power or requires amendment').

omega_variable(
    fiscal_discipline_mechanism_effectiveness,
    'Does item-level veto actually reduce wasteful spending, or do executive veto patterns reflect other political objectives unrelated to fiscal discipline?',
    'Empirical analysis of veto patterns in jurisdictions that implemented line-item veto (43 US states, 1990s-2010s). Correlation between veto use and measures of spending efficiency, pork-barrel reduction, deficit decline.',
    'If effective at reducing waste: tangled rope classification confirmed — genuine coordination function paired with power shift. If veto pattern reflects executive political preferences (targeting opposition districts, rewarding allies): constraint is pure extraction (Snare), with ''fiscal discipline'' as theatrical justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_discipline_mechanism_effectiveness, empirical, 'Whether line-item veto reduces wasteful spending or reflects executive political preferences').

omega_variable(
    legislative_adaptation_response,
    'How do legislatures adapt their appropriations strategies when facing item-level veto authority? Do they shift to omnibus bills, lump-sum appropriations, or other mechanisms that restore legislative leverage?',
    'Comparative analysis of appropriations structures in states with vs. without line-item veto. Measurement of bill complexity, spending earmark concentration, executive veto override rates.',
    'If legislatures successfully adapt: suppression is lower than assessed (0.58 → 0.35-0.40) — Congress retains counter-leverage. If legislative adaptation fails: suppression is higher (0.58 → 0.70+) — executive gains durable control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_adaptation_response, empirical, 'Legislative adaptation strategies in response to line-item veto authority').

omega_variable(
    partisan_asymmetry_in_veto_use,
    'Does line-item veto disproportionately target appropriations favored by the opposing party, making it a partisan extraction mechanism rather than a fiscal discipline tool?',
    'Historical veto data from states and federal contexts. Regression analysis of veto targets against partisan composition of legislators requesting items, presidential party affiliation, electoral competitiveness.',
    'If veto is partisan-asymmetric: suppression rises (constraints oppositions'' legislative effectiveness), extractiveness rises (asymmetric power shift), constraint reclassifies toward Snare. If veto pattern is fiscally neutral: constraint remains Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partisan_asymmetry_in_veto_use, empirical, 'Whether line-item veto use exhibits partisan bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1989_bush_line_item_veto_proposal, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liv_tr_t0, sotu_1989_bush_line_item_veto_proposal, theater_ratio, 0, 0.38).
narrative_ontology:measurement(liv_tr_t2, sotu_1989_bush_line_item_veto_proposal, theater_ratio, 2, 0.43).
narrative_ontology:measurement(liv_tr_t4, sotu_1989_bush_line_item_veto_proposal, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(liv_be_t0, sotu_1989_bush_line_item_veto_proposal, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(liv_be_t2, sotu_1989_bush_line_item_veto_proposal, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(liv_be_t4, sotu_1989_bush_line_item_veto_proposal, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1989_bush_line_item_veto_proposal, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1989_bush_line_item_veto_proposal, congressional_appropriations_process).
narrative_ontology:affects_constraint(sotu_1989_bush_line_item_veto_proposal, executive_veto_override_supermajority).
narrative_ontology:affects_constraint(sotu_1989_bush_line_item_veto_proposal, budget_deficit_constraint).

% DUAL FORMULATION NOTE:
% The line-item veto proposal exists in dual formulation: constitutional power reallocation (whether the executive can exercise item-level veto at all) and structural governance constraint (how item-level veto shifts appropriations leverage). The constitutional constraint has ε near 0 (either the power exists or it does not) — resolved by Clinton v. City of New York (1998) as statutory delegation only. The governance constraint has ε=0.52 (the structural power shift conditional on authorization) — this story analyzes the governance constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1989_bush_line_item_veto_proposal, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
