% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__extraction_snare_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: statutory_debt_ceiling__extraction_snare_reading
 *   human_readable: Statutory Debt Ceiling as Weaponized Extraction Mechanism
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   Under this reading, the U.S. statutory debt ceiling operates as a
 *   weaponized extraction mechanism enabling a legislative minority to extort
 *   policy concessions from the majority coalition by threatening sovereign
 *   default. The constraint is presented to the public as a fiscal brake on
 *   spending (the coordination_scaffold_reading and
 *   constitutional_nullity_reading framings); in structural reality (under
 *   this reading), it is deployed strategically by factions lacking majority
 *   legislative support to force outcomes they could not secure through
 *   normal order. The minority's leverage derives from the catastrophic
 *   nature of default (cessation of all federal payments within days, credit
 *   market dysfunction, international consequences) and the public appearance
 *   that default must be avoided at any cost, making the minority's threat
 *   credible even though both sides would lose from actual default. The
 *   extraction operates through repeated crises (1995-96, 2011, 2013,
 *   2018-19, 2021-23, 2024-present) in which the minority extracts
 *   concessions, then the process repeats with the ceiling raised. The
 *   theater ratio rises over time because the majority increasingly performs
 *   capitulation they do not intend to sustain (raising the ceiling without
 *   adopting all demanded policies) while the minority performs
 *   non-capitulation (claiming victory even when compromise occurs). The
 *   suppression ratio reflects the operational suppression of Treasury
 *   discretion and the subordination of federal beneficiaries' payment rights
 *   to the hostage logic.
 *
 * KEY AGENTS:
 *   - legislative_minority_faction — institutional power with arbitrage exit (can shift between parties); extracts policy concessions as the constraint's operator
 *   - treasury_operations — institutional power with trapped exit; pays the operational cost of the constraint through forced triage
 *   - federal_beneficiaries — powerless with trapped exit; bear the distributed cost of payment delays
 *   - sovereign_credit_market — powerful with mobile exit; extract elevated spreads and reassess risk during every crisis episode
 *   - majority_coalition — organized power with constrained exit; forced to choose between capitulation and default
 *   - courts — institutional power with constrained exit; observe and decline intervention despite constitutional objections
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.81).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.89).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling as Weaponized Extraction Mechanism").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, '3d3207ca-0911-4323-ace3-3795a94fd0a4').
narrative_ontology:cs_kernel_codification('3d3207ca-0911-4323-ace3-3795a94fd0a4', formalized).
narrative_ontology:cs_authority_grounding('3d3207ca-0911-4323-ace3-3795a94fd0a4', extraction).
narrative_ontology:cs_interpretation_layer_present('3d3207ca-0911-4323-ace3-3795a94fd0a4').
narrative_ontology:cs_reading_relation('3d3207ca-0911-4323-ace3-3795a94fd0a4', statutory_debt_ceiling__constitutional_nullity_reading, forecloses).
narrative_ontology:cs_reading_relation('3d3207ca-0911-4323-ace3-3795a94fd0a4', statutory_debt_ceiling__coordination_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('3d3207ca-0911-4323-ace3-3795a94fd0a4', foundational, statutory_ceiling_enables_minority_veto).
narrative_ontology:cs_axiom_status(statutory_ceiling_enables_minority_veto, holdable).
narrative_ontology:cs_axiom_grounding('3d3207ca-0911-4323-ace3-3795a94fd0a4', statutory_ceiling_enables_minority_veto, empirically_contingent).
narrative_ontology:cs_axiom('3d3207ca-0911-4323-ace3-3795a94fd0a4', foundational, default_threat_extractively_optimal_for_minority).
narrative_ontology:cs_axiom_status(default_threat_extractively_optimal_for_minority, holdable).
narrative_ontology:cs_axiom_grounding('3d3207ca-0911-4323-ace3-3795a94fd0a4', default_threat_extractively_optimal_for_minority, instrumental).
narrative_ontology:cs_reference_frame('3d3207ca-0911-4323-ace3-3795a94fd0a4', statutory_authority_as_hostage_mechanism).
narrative_ontology:cs_drift_state('3d3207ca-0911-4323-ace3-3795a94fd0a4', contemporary_fiscal_politics_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3d3207ca-0911-4323-ace3-3795a94fd0a4', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_operations).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_beneficiaries).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, sovereign_credit_market).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, international_creditors).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, majority_coalition).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, executive_branch).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, international_creditors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commands sufficient votes in one chamber to prevent raising the ceiling without extracting policy concessions unrelated to fiscal limits: spending cuts to programs the majority favors, regulatory rollback, judicial appointments, tax provisions. Uses the default threat (cessation of all federal payments within days) to force capitulation on demands orthogonal to the debt question itself. Maintains this power through party discipline and the structural asymmetry that defaulting harms everyone (making compromise appear mandatory from the outside, though the minority's preferred outcome is secured through the threat alone).
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction, agenda_setter,
    organized, biographical, arbitrage, national).

% Must operate within the statutory ceiling and faces periodic crises when receipts and obligations exceed it. Treasury Secretary has no authority to unilaterally raise the ceiling or to prioritize payments among mandatory obligations — any breach of the statute is violation. Subject to cascading operational constraints: must make triage decisions about which federal obligations to delay (Social Security, veterans benefits, payroll, interest, etc.), knowing that any prioritization choice creates political liability and market signaling costs.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_operations, payer,
    institutional, immediate, trapped, national).

% Recipients of mandatory benefits (Social Security, Medicare, veterans disability, federal payroll) face payment delays when the ceiling binds. The delays are neither their contractual responsibility nor within their control; they bear the cost of the minority's hostage strategy in the form of missed healthcare payments, pension delays, and income uncertainty. No exit mechanism exists; they cannot redirect their benefits or seek alternative federal programs.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_beneficiaries, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, federal_beneficiaries, payer).

% Prices U.S. sovereign debt based on perceived default risk. Each debt-ceiling crisis generates explicit default scenarios, rating-agency warnings, and elevated term premiums. The market extracts a measurable cost (higher borrowing rates on future issuance) from every brinksmanship episode. Though the market has exit options (allocate capital elsewhere), the U.S. Treasury's special role in global finance and the absence of true alternatives during crisis episodes create temporary immobility — creditors must hold through the ceiling standoff even as they signal elevated risk and exact compensation.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, sovereign_credit_market, payer,
    powerful, generational, mobile, global).

% Commands a majority of both chambers (in principle) but lacks a supermajority in at least one. Faces a binary choice: accept the minority's unrelated policy demands or allow default. The majority's preferred legislative agenda (absent the ceiling crisis) cannot advance while the hostage scenario is active. Politically, the majority faces reputational damage from both default (if it occurs) and capitulation to unrelated demands; either outcome is characterized as failure.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, majority_coalition, payer,
    organized, biographical, constrained, national).

% Has no statutory authority to raise the ceiling unilaterally or to resolve the default threat. Can negotiate with both factions and recommend legislation but cannot act independently. Faces political pressure from both sides: the minority demands executive action to meet its demands, the majority demands executive action to circumvent the crisis. The executive's actual power is constrained to negotiation and reputational leverage, making it functionally hostage to the crisis it cannot resolve.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, executive_branch, payer,
    powerful, biographical, constrained, national).

% Retain jurisdiction over challenges to the ceiling's constitutionality (14th Amendment Section 4 supremacy, due process, takings, etc.) but have historically declined to intervene, citing the political question doctrine and legislative remedy availability. Can observe the default threat unfold but treats judicial restraint as the appropriate institutional stance. Their potential intervention (ruling the ceiling unconstitutional) would collapse the constraint entirely but carries institutional costs (legislative/executive pushback on separation-of-powers grounds).
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, courts, observer,
    institutional, generational, constrained, national).

% Benefit from elevated spreads during crisis episodes (risk premium compensation); pay the cost of default risk through portfolio concentration and hedging requirements. Have exit options (move capital to other sovereigns) but the scale of U.S. Treasury holdings and the lack of true substitutes create sticky immobility. Observe the minority's hostage strategy as a recurring feature of U.S. fiscal governance and adjust pricing accordingly.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, international_creditors, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, international_creditors, payer).

% Assess sovereign credit risk based on default probability and recovery. Each ceiling crisis generates downgrade warnings and methodological revisions. The agencies' assessments feed market pricing and borrowing costs. Their analytical independence is compromised by the political nature of the crisis (they cannot fully price default risk as a probability when it is being deployed as a hostage mechanism rather than emerging from fiscal unsustainability), but their role remains to signal the cost to creditors.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, rating_agencies, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__extraction_snare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The constraint performs no coordination function in this reading. An alternative reading (coordination_scaffold_reading) frames the ceiling as a mechanism enabling Treasury to plan periodic borrowing and preventing indefinite automatic spending; this reading rejects that function as the operative constraint's purpose and observes instead that the ceiling's primary function is enabling minority veto.
% TRANSFER_FUNCTION: Transfers policy concessions, budget cuts, regulatory rollback, and judicial appointment commitments from the majority coalition to the minority faction as the price of raising the ceiling and avoiding default. Transfers also flow from federal beneficiaries (delayed payments, income uncertainty) to creditors (elevated spreads) and to the political process (majority credibility loss on both fronts — capitulation or default).
% ABSENT_VOICES: Default-scenario economists, international creditors, federal beneficiaries, and constitutional scholars questioning the ceiling's constitutionality under the 14th Amendment Section 4 are systematically excluded from the hostage negotiation. Economists' analyses showing default costs are treated as background noise in the political theater. International creditors have only pricing mechanisms, no seat at the table. Federal beneficiaries have no organized voice — their injury is felt only in delayed payments. Constitutional challengers are told judicial remedies exist but judges decline to use them. The absent-voices set is large and structurally excluded.
% DISAPPEARANCE_RATIONALE: If the statutory debt ceiling vanished overnight (legislatively repealed), the fiscal governance structure would reorganize sharply: the minority's hostage mechanism would evaporate; the majority would regain control of its legislative agenda; Treasury would return to normal operations; federal beneficiaries would receive uninterrupted payments; creditors would reprice U.S. sovereign debt downward (reduced default risk premium); and the political equilibrium would shift entirely — the fiscal constraint would have to be negotiated through substantive budget-process legislation rather than through periodic default crises. The constraint's disappearance would unmake the minority's veto power.
% FOUNDING_PROBLEM: The founding problem (under this reading's interpretation) was NOT to create a fiscal brake on spending, but rather to create a procedural hurdle that the minority could weaponize when it lacked majority support for its preferred policies. The constraint was designed in 1917 as a one-time borrowing authorization; its mutation into a recurring hostage mechanism emerged through political practice (repeated ceiling crises starting in the 1990s and accelerating after 2010) as factions discovered that it could be used to extract concessions outside the normal legislative process.
% FOUNDING_PROBLEM_CORROBORATION: Budget historians and political economists (Congressional Research Service, GAO, academic fiscal analysts) have documented that the founding problem (if one existed) was resolved by the 1950s—the ceiling became a routine formality raised automatically with budgets until the 1990s. The transformation into a hostage mechanism is attested by political commentators across the spectrum and by Treasury officials who characterize ceiling crises as unprecedented in their operational severity. Independent analyses by Brookings, the Committee for a Responsible Federal Budget, and academic economists document the shift from routine procedure to weaponized threat. The benefiting minority and its allied media echo the framing that the ceiling constrains spending; this claim is contradicted by the fact that all crises resolve with the ceiling raised to whatever level matches recent spending, meaning the ceiling constrains nothing—it only delays and threatens.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__extraction_snare_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading's high extractiveness (0.81) reflects the fact that the constraint systematically redistributes fiscal power from the legislative majority to an organized minority faction through repeated hostage crises. The suppression ratio (0.89) captures both the operational suppression of Treasury discretion and the suppression of federal beneficiaries' payment expectations. The theater ratio (0.62) reflects the large performative component: both sides conduct public negotiations, announce victories, and frame the outcome as principled, while the underlying dynamic is pure extraction through threat. The accessibility_collapse (0.73) represents that while alternatives to the debt ceiling exist (constitutional amendment via 14th Amendment Section 4, legislatively repealing the ceiling, treating the ceiling as unconstitutional), they are politically inaccessible during the crisis window — the default threat creates urgency that forecloses deliberate legislative reconsideration. The resistance (0.71) is high because the majority actively resists the minority's demands, creditors signal risk through pricing, economists and budget experts criticize the mechanism, and beneficiary groups (if organized) would mobilize against it — but this resistance is insufficient to prevent the minority's extraction because the default threat is catastrophic enough that the majority capitulates rather than risk it. The temporal measurements show a clear trajectory: from 1917-1995 the ceiling was a routine formality with minimal extractive character; from 1995 onward it emerged as a hostage mechanism as partisan polarization made the minority willing to tolerate higher default risk; the slope accelerated post-2010 as the Tea Party wing of the Republican coalition discovered that the hostage mechanism could extract substantial concessions.
 *
 * PERSPECTIVAL GAP:
 *   From the minority faction's seat: the constraint is a fiscal governance tool they successfully deploy to extract policy concessions. From the majority seat: it is a catastrophic threat they are forced to manage. From Treasury's seat: it is an operational impossibility (cannot maintain normal payments within a binding statutory constraint). From the credit market's seat: it is a source of recurring default risk that extracts compensation. From federal beneficiaries' seat: it is a non-agent-facing injury with no recourse. The engine's per-seat computation will reflect these divides — the constraint may compute as rope-adjacent from the minority's perspective (they benefit, others coordinate) while computing as snare from the majority and beneficiary perspectives (extraction with suppression).
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative minority has low directionality (d near 0.0 = beneficiary end) because they are the constraint's operator, they extract the concessions, and they have arbitrage exit (they can shift between parties or threaten to form a new coalition). The treasury and federal beneficiaries have high directionality (d near 1.0 = target end) because they pay the operational cost and have trapped exit — Treasury cannot ignore the statute, beneficiaries cannot redirect their benefits. The majority coalition has mid-to-high directionality (d ~0.65-0.75) because they bear the cost of capitulation or default but retain some negotiating power. The credit market has mid directionality (d ~0.55-0.65) because they extract elevated spreads but face concentrated exposure that limits exit during crisis windows. Directionality overrides are not needed for this constraint because the structural derivation accurately captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly rejects the mandatrophy resolution: the founding problem (if one existed) is dead, but the constraint persists and indeed grows more extractive. The constraint's original mandate (1917) was to authorize one-time war borrowing; that mandate is long obsolete. The constraint then evolved into a routine formality (1945-1995) where its mandate was to enable efficient Treasury operations — a genuine coordination function. But that mandate, too, is now dead: the modern constraint's primary function is enabling minority hostage crises, not efficient Treasury operations. Periodically raising the ceiling to match actual spending proves that the ceiling constrains nothing — it only delays and threatens. The mandatrophy is complete: the constraint persists through political inertia and minority faction benefit, not through any live coordination mandate. The theater ratio (0.62) captures this: the majority and Treasury perform as if the ceiling is a constraint they respect, while the reality is that they raise it whenever it binds. The performance of constraint sustains a constraint that has no function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    default_threat_credibility,
    'Is the legislative minority''s default threat credible, or is it bluff that would collapse under pressure?',
    'Historical observation: has the minority faction ever allowed default to occur, or does capitulation always occur before actual default? Structural analysis: what are the minority''s exit costs if default occurs (reputational, financial markets, constituent harm)? Behavioral game theory: under what cost/benefit profile would the threat become non-credible?',
    'If the threat is credible (both sides would genuinely incur high costs from default but the minority''s costs are lower than the majority''s), the snare classification holds. If the threat is incredible bluff, the constraint computes as extractive theater (piton-adjacent) rather than active snare — the beneficiaries would be maintaining an illusion of power rather than exercising real hostage leverage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(default_threat_credibility, empirical, 'Whether the default threat is a real mechanism or a credibility collapse waiting to occur.').

omega_variable(
    constitutional_authority_conflict,
    'Does the statutory debt ceiling conflict with the 14th Amendment Section 4 prohibition on default, and if so, which authority supersedes?',
    'Judicial interpretation: SCOTUS ruling on the constitutional conflict; congressional action to amend or repeal the ceiling; executive interpretation of constitutional primacy; political equilibrium between the three readings held by different factions.',
    'If courts rule the ceiling unconstitutional, the constraint disappears and the extraction mechanism collapses entirely — the nullity_reading becomes operant. If courts decline to intervene (political question doctrine), the extraction_snare_reading remains the operative constraint. If Congress repeals the ceiling legislatively, the constraint vanishes. The three readings compete for institutional authority; whichever gains institutional backing (legislative, judicial, or executive) determines which constraint becomes real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_authority_conflict, conceptual, 'Whether the ceiling''s statutory authority overrides constitutional prohibition on default, or vice versa.').

omega_variable(
    minority_faction_stability,
    'Is the minority faction a stable coalition capable of maintaining hostage discipline, or does it fragment when default risk rises above threshold?',
    'Observable: does the minority hold unified position through escalating crises, or do defectors emerge as default risk nears? Structural: what alignment of incentives keeps the minority together? What would cause it to splinter?',
    'If the minority faction is stable, the extraction mechanism persists and the snare classification holds. If the faction fragments as default risk rises, the hostage threat becomes non-credible and the constraint degrades to piton or theater. The temporal stability of the extraction depends on the minority''s internal cohesion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_faction_stability, empirical, 'Whether the minority faction has sufficient internal discipline to maintain default-threat credibility.').

omega_variable(
    federal_beneficiary_mobilization,
    'Could federal beneficiaries (Social Security recipients, veterans, federal payroll) organize collectively to demand ceiling elimination or constitutional override?',
    'Political mobilization: do beneficiary groups articulate this issue as a core threat? Electoral pressure: do politicians supporting the ceiling face beneficiary-group opposition? Legislative outcome: does beneficiary mobilization produce legislative change?',
    'If beneficiaries mobilize as a constituency, their powerless status (now unmobilized) could shift to organized status, raising their exit options and reducing suppression. The constraint would face resistance from below that it currently encounters only from above. The extraction mechanism would become politically unsustainable if beneficiaries'' injury is made visible and organized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_beneficiary_mobilization, empirical, 'Whether the currently powerless federal beneficiaries can organize into a political force that shifts the constraint''s equilibrium.').

omega_variable(
    kernel_reading_contest,
    'Which reading (nullity, coordination, extraction) will institutionally prevail? Will the kernel resolve toward one dominant reading, or remain contested across institutional seats?',
    'SCOTUS ruling on constitutionality (nullity_reading ↑); legislative repeal (extraction_snare_reading collapses); judicial decline to intervene and political stabilization of hostage mechanism (extraction_snare_reading ↑); or continued institutional fragmentation with different actors authoring different readings.',
    'If the nullity_reading institutionalizes (constitutional override), this constraint vanishes. If the coordination_scaffold_reading prevails (ceiling treated as real fiscal limit that Congress respects), the extraction mechanism withers. If the extraction_snare_reading prevails (hostage mechanism becomes normalized political practice), the constraint calcifies as a permanent extractive mechanism and the theater ratio stabilizes at high levels (performance of constraint rather than function of constraint).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which institutional authority will resolve the kernel contest, and which reading will become operant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1917, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1917, 0.08).
narrative_ontology:measurement(stat_tr_t1975, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement(stat_tr_t1995, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1995, 0.32).
narrative_ontology:measurement(stat_tr_t2010, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(stat_tr_t2018, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2018, 0.58).
narrative_ontology:measurement(stat_tr_t2024, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2024, 0.62).

% Extraction over time
narrative_ontology:measurement(stat_be_t1917, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1917, 0.15).
narrative_ontology:measurement(stat_be_t1975, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1975, 0.18).
narrative_ontology:measurement(stat_be_t1995, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(stat_be_t2010, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(stat_be_t2018, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2018, 0.72).
narrative_ontology:measurement(stat_be_t2024, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1917, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1917, 0.25).
narrative_ontology:measurement(stat_su_t1975, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1975, 0.28).
narrative_ontology:measurement(stat_su_t1995, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(stat_su_t2010, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(stat_su_t2018, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2018, 0.82).
narrative_ontology:measurement(stat_su_t2024, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2024, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__extraction_snare_reading, 0.12).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__constitutional_nullity_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, federal_spending_constraint_doctrine).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, congressional_budget_process_authority).

% DUAL FORMULATION NOTE:
% The statutory_debt_ceiling kernel gives rise to three distinct constraint stories: (1) extraction_snare_reading (this constraint) — the ceiling weaponized by legislative minority as hostage mechanism; (2) coordination_scaffold_reading — the ceiling as procedural boundary enabling Treasury operations; (3) constitutional_nullity_reading — the ceiling as constitutionally void constraint superseded by 14th Amendment Section 4. Each reading has distinct ε (nullity~0.0, coordination~0.35, extraction~0.81), distinct beneficiaries/victims, distinct type. The three stories are linked via network.affects_constraints; whichever reading gains institutional authority determines which constraint becomes operant. The nullity_reading and extraction_snare_reading coexist in legal/political discourse; the coordination_scaffold_reading is the default operational framing until hostage crisis activates the extraction_snare_reading. All three stories reference the same formal statute; their structural differences derive from different causal models of how the statute functions in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
