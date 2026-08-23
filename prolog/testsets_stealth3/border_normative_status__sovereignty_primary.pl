% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: Foundational State Authority to Exclude Non-Members (Collective Self-Determination Reading)
 *   domain: political philosophy/international law/migration studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading - sovereignty_primary - of the
 *   contested kernel border_normative_status: the claim that territorial
 *   boundaries are legitimate instruments of collective self-determination
 *   and that states hold foundational authority to exclude non-members. The
 *   kernel decomposes into three separate constraint stories because the
 *   colloquial label 'border control' covers structurally incompatible claims
 *   with different epsilon values: this reading (exclusion as foundational
 *   and default-legitimate), freedom_primary (movement as fundamental right;
 *   exclusion requiring extraordinary justification), and
 *   qualified_sovereignty (control retained but proportionate). Per the
 *   epsilon-invariance principle this file authors only the
 *   sovereignty-primary reading, over the FIXED referent of the standing
 *   global exclusion arrangement, assessed by the reading's OWN LIGHTS: hence
 *   low epsilon (the reading endorses the arrangement's core as legitimate
 *   self-determination, not predation) alongside structural data that
 *   honestly records real cost-bearing - declared victims, a vast enforcement
 *   apparatus, and accumulating extraction layered onto the coordination
 *   function (detention contracting, externalization markets). Claim and
 *   metrics are authored independently: where the reading's endorsement
 *   diverges from the engine's per-seat computations, that divergence is the
 *   datum the corpus exists to take. KEY AGENTS (by structural relationship):
 *   - receiving_state_citizenries: Primary beneficiary (organized/mobile) -
 *   the demos the boundary constitutes; - destination_state_governments:
 *   Agenda-setter and principal capturer (institutional/constrained) -
 *   administers the exclusion function and collects its political returns; -
 *   border_enforcement_industries: Concentrated secondary beneficiary
 *   (powerful/arbitrage) - monetizes enforcement volume across jurisdictions;
 *   - excluded_would_be_migrants: Primary target (powerless/trapped) - bears
 *   exclusion's costs at the line; - detained_and_deportable_residents:
 *   Target inside the machinery (powerless/trapped); -
 *   transnational_split_families: Diffuse target (moderate/constrained); -
 *   transit_state_governments: Inter-institutional absorber
 *   (institutional/constrained) - hosts outsourced enforcement under
 *   conditionality; - international_human_rights_bodies: Excluded
 *   remonstrator (institutional/constrained) - documents without leverage; -
 *   normative_theorists_of_membership: Analytical observer
 *   (analytical/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.22).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.65).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.22).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Foundational State Authority to Exclude Non-Members (Collective Self-Determination Reading)").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political philosophy/international law/migration studies").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, '2259331c-152b-41bc-bb60-718b0268bcb8').
narrative_ontology:cs_kernel_codification('2259331c-152b-41bc-bb60-718b0268bcb8', distributed).
narrative_ontology:cs_authority_grounding('2259331c-152b-41bc-bb60-718b0268bcb8', lineage).
narrative_ontology:cs_interpretation_layer_present('2259331c-152b-41bc-bb60-718b0268bcb8').
narrative_ontology:cs_reading_relation('2259331c-152b-41bc-bb60-718b0268bcb8', border_normative_status__freedom_primary, forecloses).
narrative_ontology:cs_reading_relation('2259331c-152b-41bc-bb60-718b0268bcb8', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('2259331c-152b-41bc-bb60-718b0268bcb8', foundational, foundational_collective_self_determination_right).
narrative_ontology:cs_axiom_status(foundational_collective_self_determination_right, holdable).
narrative_ontology:cs_axiom_grounding('2259331c-152b-41bc-bb60-718b0268bcb8', foundational_collective_self_determination_right, deontological).
narrative_ontology:cs_axiom('2259331c-152b-41bc-bb60-718b0268bcb8', secondary, justice_scope_bounded_by_membership).
narrative_ontology:cs_axiom_status(justice_scope_bounded_by_membership, holdable).
narrative_ontology:cs_axiom_grounding('2259331c-152b-41bc-bb60-718b0268bcb8', justice_scope_bounded_by_membership, conventional).
narrative_ontology:cs_reference_frame('2259331c-152b-41bc-bb60-718b0268bcb8', westphalian_plenary_exclusion_baseline).
narrative_ontology:cs_drift_state('2259331c-152b-41bc-bb60-718b0268bcb8', contemporary_human_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2259331c-152b-41bc-bb60-718b0268bcb8', '').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, receiving_state_citizenries).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, destination_state_governments).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, border_enforcement_industries).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_would_be_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, detained_and_deportable_residents).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, transnational_split_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, transit_state_governments).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, transit_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of destination-state political communities. The boundary secures their collective control over territory, labor-market access, and the composition of the electorate; they fund enforcement through taxation and supply the electoral majorities that sustain control policies. Dissatisfied members can emigrate, protest, or vote the policy out; the boundary binds outsiders far more tightly than them.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, receiving_state_citizenries, beneficiary,
    organized, generational, mobile, national).

% Governments of destination states write visa rules, command border agencies, negotiate externalization agreements with transit and origin states, and announce enforcement statistics. Visible control yields electoral credit, expanded executive discretion over policing and detention, and leverage over neighboring governments. Surrendering the exclusion function would mean giving up a core instrument of statecraft, so administrations defend and expand it regardless of party.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, destination_state_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__sovereignty_primary, destination_state_governments, beneficiary).

% Contractors supplying surveillance platforms, biometric systems, detention facilities, removal transport, and guard staffing to border and interior ministries. Revenue scales with enforcement appropriations; firms bid on procurements across many countries at once, so a budget cut or election result in any single state rarely threatens the overall portfolio.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, border_enforcement_industries, beneficiary,
    powerful, biographical, arbitrage, global).

% People outside the territory seeking entry for work, family reunification, or safety who are refused visas or turned back at the line. They absorb the boundary's costs directly: foregone lifetime earnings, smuggler debts, desert and sea-crossing mortality, and years of waiting on narrowing legal channels. Remaining home or risking irregular passage are the practical options; lawful admission is priced or rationed beyond reach.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, excluded_would_be_migrants, payer,
    powerless, biographical, trapped, global).

% Non-citizens already inside the territory - overstayers, rejected asylum seekers, long-settled residents without secure status - exposed to workplace raids, detention of days to years, and removal proceedings. Return sends many to countries they left decades ago; while proceedings run they cannot work lawfully, travel, or plan a household.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, detained_and_deportable_residents, payer,
    powerless, immediate, trapped, national).

% Households straddling the line: one member a citizen or permanent resident, another denied a visa or removed. They finance parallel lives in two jurisdictions, litigate reunification for years, and absorb separations neither partner chose. Their mixed status gives them some voice - the citizen member can vote and petition - but the outcome depends on officials they did not elect.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, transnational_split_families, payer,
    moderate, biographical, constrained, continental).

% Governments along the main routes that host enforcement outsourced by destinations: financed patrol forces, processing hubs, and containment camps on their soil, with aid and trade preferences tied to interception performance. Cooperation brings revenue and diplomatic favor; refusal risks funding loss, visa retaliation, and isolation. Their own borderlands pass under operational control negotiated elsewhere.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, transit_state_governments, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(border_normative_status__sovereignty_primary, transit_state_governments, beneficiary).

% Treaty-monitoring bodies, regional courts, and refugee agencies charged with protecting people in motion. They document pushbacks, publish findings, and issue non-binding judgments; states acknowledge the reports selectively and enforcement continues. These bodies hold no enforcement lever of their own and sit outside the ministerial meetings where exclusion policy is actually set.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, international_human_rights_bodies, excluded,
    institutional, generational, constrained, global).

% Political philosophers and legal scholars arguing over who may exclude whom and on what grounds. They produce the self-determination defenses, open-border critiques, and proportionality frameworks that all sides deploy; their influence runs through citation, curricula, and elite argument rather than votes or budgets.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, normative_theorists_of_membership, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__sovereignty_primary, destination_state_governments).
narrative_ontology:fixing_cost_class(border_normative_status__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the membership-composition problem every mass democracy faces: fixes who counts as part of the people entitled to decide common affairs, who shares the jurisdiction's obligations and protections, and whose claims the political process must answer. Provides a determinate demos for elections, welfare pools, military service, and redistribution.
% TRANSFER_FUNCTION: Moves access to territory, residence, and labor markets from non-members to member-controlled allocation; moves the risks of movement onto migrants themselves (smuggler debt, route mortality, detention time); moves enforcement spending from taxpayers to contractors; moves political capital to governments credited with control; moves development and security aid to transit states in exchange for interception performed on destinations' behalf.
% ABSENT_VOICES: Would-be migrants hold no seat in the legislatures that write the rules excluding them; deportable residents cannot vote in any jurisdiction governing their fate; transit-state populations live with camps and patrol forces decided in foreign capitals; future cohorts inherit membership rules made without them. Rights bodies remonstrate from outside the enforcement channel, and their objections enter the record without entering the decision.
% DISAPPEARANCE_RATIONALE: If foundational exclusion authority vanished overnight, migration corridors would reroute within months; wages, rents, and school enrollments in destination regions would shift with the new arrivals; welfare-state financing built on bounded contribution pools would need redesign; citizenship would migrate from inherited privilege toward portable status. The interstate order is organized around the expectation that states can close their edges - removing that capacity rearranges labor markets, fiscal architecture, and the meaning of political membership simultaneously.
% FOUNDING_PROBLEM: After the collapse of empires and the wars of the early twentieth century, new and restored states had to determine who belonged: who owed military service, paid taxes, received poor relief, and voted. Determining membership became the precondition for administering anything else, and controlling entry became the guarantee that the membership once determined stayed determined.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: democratic theorists working on the boundary problem - including prominent critics of this very reading - concede that a demos-defining problem exists and persists even while rejecting the exclusionary solution to it; historical sociologists of state formation document membership determination as a founding administrative task of modern states; frontline practitioners in refugee agencies attest daily to the consequences of unresolved membership rules. No corroborating source depends on exclusion's continuance for its professional standing.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).
:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.22 because the referent is the standing exclusion arrangement and the assessing seat is the sovereignty-primary reading itself: by its lights the core operation (determining who joins the demos) is legitimate collective self-determination, with residual extraction acknowledged where enforcement outruns defense of the boundary - detention contracting, externalization fees, smuggling-economy rents the apparatus incubates. Suppression is 0.65 as a RAW structural property, deliberately unscaled and deliberately NOT lowered by the reading's endorsement: whatever one thinks of its legitimacy, the constraint is maintained by enormous active coercive machinery - patrol forces, biometric screening, detention estates, removal logistics - and suppression measures the machinery, not approval of it. Theater is 0.44: a large and growing share of enforcement activity is performative (announced deportation totals, wall constructions timed to campaigns, interception statistics inflated for headlines) reflecting what Andreas called border games - states performing a control they only partly achieve - while screening and processing functions remain real. Accessibility collapse is low (0.35): understanding the constraint does not close its alternatives - free-movement zones exist and operate inside the system (Schengen, ECOWAS, Mercosur residencies), proving bounded exclusion is a constructible variable, not a law of nature. Resistance is substantial (0.55): irregular crossing itself, sanctuary and legal-aid networks, strategic litigation, Global South diplomacy against visa asymmetry, and sustained scholarly critique. The three tracked series share ONE eight-point grid (1914/1930/1948/1965/1985/2001/2015/2026) so every metric is authored at every examined time; trajectories are ratchet-shaped, not cyclic - a mid-century relaxation (guest-worker permeability) followed by secular tightening (compensatory closure after Schengen, post-2001 securitization, post-2015 externalization). The oscillation tracks geopolitical shocks rather than an engineered reinforcement schedule, so it is treated as environmental, not as intermittent reinforcement. Boltzmann typing as identity_coordination carries a standing alert: identity narratives ('this is simply who we are') are the classic cover for laundered extraction, and the coupling signature to watch is institutional power operating at global scope against powerless targets - precisely this constraint's shape - so complexity tolerance for boundary maintenance must not excuse the asymmetric cost structure the victim declarations record. Receipt-surface notes: gain_flow names destination_state_governments because the constraint's political gains (electoral credit, executive discretion, leverage over neighbors) demonstrably accrue there; contractor revenue is second-order, drawn from state budgets, and citizenry benefit is diffuse and incidental - receipt is not the same fact as beneficiary role. fixing_cost is prohibitive independently: for the governments who could dismantle the exclusion function, doing so dissolves the demos-bounding instrument their administrative and fiscal order rests on; the cost class is judged on that evidence alone.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically differently. From the trapped, powerless payer seats - would-be migrants facing global-scope exclusion, detainees inside the machinery - effective extraction is amplified toward its maximum: full-target directionality, no exit, verification of their situation difficult at scale. From the beneficiary seats the same structure subsidizes: citizenries receive constituted membership, governments collect authority, contractors collect fees. The sovereignty-primary reading's own seat sits with the beneficiaries - which is exactly why its authored epsilon is low while the structural record shows trapped victims paying through the same boundary. Inter-institutionally, transit-state governments occupy a squeezed middle: victim-classified by the victim declaration (they bear externalized enforcement on their soil) yet compensated through conditioned aid, a dual position the flat derivation understates. Among nominally same-level actors - citizenries of different destination states - wealth, asylum-system generosity, and demographic dependency differentiate exit options and experienced costs despite equal formal sovereignty; a Gulf monarchy's kafala-mediated exclusion and a Nordic welfare state's humanitarian-channel exclusion are the same nominal constraint with different lived distributions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for receiving_state_citizenries (mobile exit pushes them toward the beneficiary end), destination_state_governments (full beneficiary-plus-administrator), and border_enforcement_industries (arbitrage-grade exit puts them at the extreme beneficiary end). Victim declarations drive high directionality for the three migrant classes, with trapped exit pushing excluded_would_be_migrants and detained_and_deportable_residents nearest the full-target end; transnational_split_families sit slightly less extreme because the citizen member retains partial voice (constrained, not trapped). Transit_state_governments derive a high d from their victim declaration that their conditioned-aid receipts partially offset - a known residual imprecision. Directionality overrides were deliberately NOT used: the override mechanism keys on power_atom, and this story contains four institutional-power seats with genuinely opposed relationships (two governments as setters/beneficiaries, transit states as absorbed payers, rights bodies as excluded remonstrators) - a single power-atom override would contaminate all four simultaneously, so the structural declarations are left to carry the differentiation and the transit-state offset is documented here and in the externalization omega instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - determining who belongs after imperial collapse - is live, so nothing here turns on a dead mandate. The classification discipline cuts both ways for this reading. Against the freedom-primary temptation: the genuine coordination function (demos constitution for democratic and fiscal operation) must not be scored as pure extraction merely because its victims are visible; the reading's endorsement encodes a real judgment that some cost-bearing is the price of collective autonomy. Against the sovereignty temptation: the victim declarations refuse to let the coordination story absorb the entire ledger - excluded migrants pay through the very boundary that constitutes the members' benefit, enforcement is actively maintained, and extraction has accumulated on top of coordination for a century. Holding both facts in one structure is exactly what the hybrid category exists for, and the reading's own concession that outsiders bear real, justified-but-real costs is what makes the hybrid claim honest rather than apologetic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading - sovereignty_primary - of the kernel border_normative_status; what structurally changes under the sibling readings, and where exactly is the dispute located?',
    'Comparative classification of the sibling stories (border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty) authored over the identical referent: differences in victim sets, enforcement legitimacy, and reading-indexed epsilon isolate the disputed element.',
    'Under freedom_primary the excluded become rights-violation victims rather than justified cost-bearers, epsilon rises sharply, and enforcement machinery reads as the illegitimate object itself; under qualified_sovereignty enforcement becomes conditionally legitimate and epsilon sits between the poles. Classification of this file is stable only within its own reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: which reading of the border-normative-status kernel this story is, and what siblings would change.').

omega_variable(
    bounded_membership_naturality,
    'Is bounded political membership a near-universal feature of human social organization (making exclusion quasi-natural and mountain-adjacent), or a constructed institution maintained because identifiable actors benefit from it?',
    'Comparative anthropology and history of mobility regimes: societies with porous membership, federations dissolving internal boundaries, and the demonstrated constructibility of free-movement zones inside the current system.',
    'If constructed, the beneficiary declarations activate false-summit dynamics - the reading''s naturalizing rhetoric (''peoples have always controlled membership'') becomes cover for identifiable winners; if genuinely near-universal, part of the measured structure approaches natural-law certification and the extraction critique weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bounded_membership_naturality, conceptual, 'Naturality versus construction of bounded membership, and who the construction serves.').

omega_variable(
    citizen_displacement_accounting,
    'The sovereignty-primary reading treats displacement pressure on working-class citizens (wage competition, housing costs, service load) as externality or non-issue; what happens to the structure if those harms are admitted into the ledger?',
    'Distributional incidence analysis conducted under the reading''s own welfare standards: who bears measurable costs of openness scenarios, and whether the reading''s stated concern for member welfare compels counting them.',
    'Counting citizen displacement makes receiving_state_citizenries dual-positioned (beneficiary and payer), flattens their directionality away from the beneficiary end, and gives the reading internal reasons for enforcement levels it currently justifies only externally - potentially converting parts of the citizenry from beneficiaries into targets of the same apparatus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_displacement_accounting, preference, 'Whether the reading''s own value commitments require counting costs it currently books as non-issues.').

omega_variable(
    externalized_extraction_relocation,
    'Does externalized enforcement (financed interception in transit states) reduce the constraint''s extraction or merely relocate it from the destination frontier to transit territories and offshore camps?',
    'Track mortality, detention conditions, and cost incidence along corridors before and after externalization agreements (EU-Turkey style arrangements, Pacific deterrence models); compare victim outcomes at equivalent migration pressure.',
    'Relocated extraction keeps effective extraction high while moving the victim set toward transit-state populations and offshore detainees - changing which seats compute as trapped and possibly adding transit populations to the victim ledger this reading currently leaves implicit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalized_extraction_relocation, empirical, 'Whether outsourcing enforcement relocates or reduces the costs the arrangement imposes.').

omega_variable(
    enforcement_demand_loop,
    'Is the century-long enforcement buildup driven by actual migration pressure and security function, or by domestic electoral demand for visible control that feeds on its own performance?',
    'Time-series comparison of enforcement appropriations against measured irregular-flow volumes and against electoral calendars; identify phases where spending rose while flows fell.',
    'If demand-driven, the theater component compounds and enforcement subsystems drift toward inertial performance - apparatus maintained for display, function atrophied - pulling the overall structure toward degraded maintenance even while the core normative claim stays live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_demand_loop, empirical, 'Function-versus-display driver of the enforcement ratchet visible in the suppression series.').

omega_variable(
    migrant_coalition_potential,
    'Can excluded migrants convert numerosity into coalition power - strike actions, sanctuary networks, diaspora remittance leverage, eventual franchise - sufficient to move their power atom above powerless?',
    'Historical study of successful migrant-labor mobilizations (farmworker campaigns, sans-papiers movements, remittance-conditioned diplomacy) and the conditions under which states conceded.',
    'Effective coalition power lowers the trapped seats'' effective extraction without changing the arrangement''s structure, and would signal that the constraint''s persistence depends on keeping the victim class fragmented - a diagnostic marker separating durable hybrids from enforced snares.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(migrant_coalition_potential, empirical, 'Coalition-formation potential of a numerically large but individually powerless victim class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 1914, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1914, border_normative_status__sovereignty_primary, theater_ratio, 1914, 0.1).
narrative_ontology:measurement(bord_tr_t1930, border_normative_status__sovereignty_primary, theater_ratio, 1930, 0.18).
narrative_ontology:measurement(bord_tr_t1948, border_normative_status__sovereignty_primary, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(bord_tr_t1965, border_normative_status__sovereignty_primary, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(bord_tr_t1985, border_normative_status__sovereignty_primary, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(bord_tr_t2001, border_normative_status__sovereignty_primary, theater_ratio, 2001, 0.36).
narrative_ontology:measurement(bord_tr_t2015, border_normative_status__sovereignty_primary, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(bord_tr_t2026, border_normative_status__sovereignty_primary, theater_ratio, 2026, 0.44).

% Extraction over time
narrative_ontology:measurement(bord_be_t1914, border_normative_status__sovereignty_primary, base_extractiveness, 1914, 0.1).
narrative_ontology:measurement(bord_be_t1930, border_normative_status__sovereignty_primary, base_extractiveness, 1930, 0.13).
narrative_ontology:measurement(bord_be_t1948, border_normative_status__sovereignty_primary, base_extractiveness, 1948, 0.12).
narrative_ontology:measurement(bord_be_t1965, border_normative_status__sovereignty_primary, base_extractiveness, 1965, 0.1).
narrative_ontology:measurement(bord_be_t1985, border_normative_status__sovereignty_primary, base_extractiveness, 1985, 0.15).
narrative_ontology:measurement(bord_be_t2001, border_normative_status__sovereignty_primary, base_extractiveness, 2001, 0.18).
narrative_ontology:measurement(bord_be_t2015, border_normative_status__sovereignty_primary, base_extractiveness, 2015, 0.21).
narrative_ontology:measurement(bord_be_t2026, border_normative_status__sovereignty_primary, base_extractiveness, 2026, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1914, border_normative_status__sovereignty_primary, suppression_requirement, 1914, 0.15).
narrative_ontology:measurement(bord_su_t1930, border_normative_status__sovereignty_primary, suppression_requirement, 1930, 0.32).
narrative_ontology:measurement(bord_su_t1948, border_normative_status__sovereignty_primary, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(bord_su_t1965, border_normative_status__sovereignty_primary, suppression_requirement, 1965, 0.22).
narrative_ontology:measurement(bord_su_t1985, border_normative_status__sovereignty_primary, suppression_requirement, 1985, 0.42).
narrative_ontology:measurement(bord_su_t2001, border_normative_status__sovereignty_primary, suppression_requirement, 2001, 0.56).
narrative_ontology:measurement(bord_su_t2015, border_normative_status__sovereignty_primary, suppression_requirement, 2015, 0.63).
narrative_ontology:measurement(bord_su_t2026, border_normative_status__sovereignty_primary, suppression_requirement, 2026, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, identity_coordination).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% The colloquial label 'border control' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints - three readings of the kernel border_normative_status, each authored as its own story over the SAME standing referent (the existing global arrangement of territorial exclusion) with different reading-indexed epsilon: sovereignty_primary (this file; epsilon approximately 0.22 by its own lights, since the reading deems the arrangement's core legitimate), freedom_primary (same referent read as an impermissible rights restriction; epsilon authored high by that reading), and qualified_sovereignty (proportionality-conditioned control; epsilon intermediate). Family links carry contamination propagation: erosion of this reading's doctrinal acceptance enlarges the qualified reading's bargaining space and shifts the foreclosure relation toward freedom_primary. Historically this reading is upstream - its Westphalian lineage supplied the doctrinal foundation the qualified reading modifies and the freedom reading rejects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
