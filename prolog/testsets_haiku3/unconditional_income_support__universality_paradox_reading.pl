% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__universality_paradox_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: unconditional_income_support__universality_paradox_reading
 *   human_readable: Unconditional Income Support as Universality Paradox
 *   domain: political_economy/welfare_state
 *
 * SUMMARY:
 *   Unconditional income support (UBS) emerges as a policy vehicle that
 *   allows cross-ideological coalition-building by remaining deliberately
 *   ambiguous about its distributional intent. The left reads it as
 *   decommodification of labor—a floor beneath which market discipline cannot
 *   push. The right reads it as replacing paternalistic welfare bureaucracy
 *   with a market-friendly cash transfer that allows recipients to choose how
 *   to spend it. Both framings cannot be coherently implemented in the same
 *   system: pure decommodification (high benefit, universal access, no
 *   clawback) is expensive; pure market efficiency (low cost, high phase-out,
 *   means-tested in effect) contradicts universality. Yet policy designers
 *   and political entrepreneurs benefit from leaving this incompatibility
 *   unresolved. Taxing-back mechanisms (progressive phase-outs) allow them to
 *   claim universality while actually targeting—and to frame targeting as
 *   technical efficiency rather than ideological choice. The paradox operates
 *   as a tangled rope: there is a real coordination benefit (simplified
 *   administration, reduced stigma, clearer income support) entangled with
 *   extraction (the ambiguity allows political actors to appropriate credit
 *   for a policy while deferring the distributional commitments the policy
 *   actually embodies; targeted program recipients are casualties of the
 *   consolidation; ideological clarity is systematically suppressed).
 *
 * KEY AGENTS:
 *   - Political entrepreneurs exploiting ambiguity to build incompatible coalitions.
 *   - Policy designers using taxing-back mechanisms to maintain rhetorical flexibility.
 *   - Targeted program recipients losing categorical supports when universality justifies consolidation.
 *   - Labor-market participants genuinely benefiting from the income floor but trapped in policy instability driven by unresolved ideological claims.
 *   - Fiscal authorities forced to implement and enforce choices that break the political ambiguity.
 *   - Ideological-clarity stakeholders—welfare-state analysts, normative commitments to coherent universalism or targeting—victimized by the policy's structural evasion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.48).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.62).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Unconditional Income Support as Universality Paradox").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political_economy/welfare_state").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, 'abd82a5f-c3a6-4343-bb11-c90e6874bee0').
narrative_ontology:cs_kernel_codification('abd82a5f-c3a6-4343-bb11-c90e6874bee0', distributed).
narrative_ontology:cs_authority_grounding('abd82a5f-c3a6-4343-bb11-c90e6874bee0', distributed).
narrative_ontology:cs_reading_relation('abd82a5f-c3a6-4343-bb11-c90e6874bee0', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('abd82a5f-c3a6-4343-bb11-c90e6874bee0', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('abd82a5f-c3a6-4343-bb11-c90e6874bee0', foundational, incompatible_ideologies_empirically_indistinguishable).
narrative_ontology:cs_axiom_status(incompatible_ideologies_empirically_indistinguishable, holdable).
narrative_ontology:cs_axiom_grounding('abd82a5f-c3a6-4343-bb11-c90e6874bee0', incompatible_ideologies_empirically_indistinguishable, empirically_contingent).
narrative_ontology:cs_axiom('abd82a5f-c3a6-4343-bb11-c90e6874bee0', foundational, ambiguity_enables_coalition_building).
narrative_ontology:cs_axiom_status(ambiguity_enables_coalition_building, holdable).
narrative_ontology:cs_axiom_grounding('abd82a5f-c3a6-4343-bb11-c90e6874bee0', ambiguity_enables_coalition_building, instrumental).
narrative_ontology:cs_reference_frame('abd82a5f-c3a6-4343-bb11-c90e6874bee0', welfare_state_coherence_before_unconditional_income_support).
narrative_ontology:cs_drift_state('abd82a5f-c3a6-4343-bb11-c90e6874bee0', contemporary_ubs_implementation_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('abd82a5f-c3a6-4343-bb11-c90e6874bee0', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, ideological_clarity).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, labor_market_participants).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, policy_designers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and maintain cross-ideological coalitions by exploiting the ambiguity of unconditional income support. Set the policy agenda by defining UBS in terms compatible with incompatible ideological commitments. Benefit from the ambiguity itself—they can claim credit for progressive social policy (left frame) and market-friendly welfare reform (right frame) without fully defending either. The ambiguity allows them to avoid the choice between universalism and targeting, and to blame implementation failures on technical design rather than ideological incompatibility.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, agenda_setter,
    institutional, biographical, arbitrage, national).

% Design taxing-back mechanisms (progressive phase-outs, means-testing at high income levels) that allow rhetorical flexibility on universality while controlling distributional outcomes and fiscal cost. Use technical mechanisms (negative tax brackets, clawback schedules) to maintain the appearance of universality while actually targeting benefits toward lower-income recipients. Benefit from this flexibility because it allows them to defend the policy to incompatible audiences simultaneously: 'Everyone gets a benefit' (universality claim) and 'Those who can afford to refund it, do' (targeting via phase-out). The mechanism becomes a design goal in itself—the ability to maintain incompatible narratives is the primary deliverable.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, policy_designers, payer).

% Lose access to or visibility within categorical assistance programs (housing allowances, disability supplements, childcare support, targeted job training) when universality rhetoric justifies eliminating targeted programs in favor of a single consolidated unconditional transfer. Experience the consolidation as a reduction in support: a universal payment may be lower than the sum of categorical benefits they previously received. Become collateral damage in the move to universality—they absorb the costs of program consolidation while political actors claim progress toward a simplified, equitable system. The paradox operates as extractive for this group: universality becomes the justification for cost control and service reduction.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, biographical, trapped, national).

% Receive unconditional income that nominally improves their exit options from exploitative or low-wage employment. Gain material security and real bargaining power in labor-market negotiation. The benefit is genuine and economically meaningful. But are trapped in policy instability created by the unresolved incompatibility: if wealthy recipients are heavily taxed back, the program's universality claim becomes hollow and politically vulnerable to dismantling. If wealthy recipients are not taxed back, the program becomes fiscally expensive and faces austerity pressure. The benefit to this group is perpetually under political threat from the same ambiguity that created the policy, making their exit options less secure than the income support itself would suggest.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, labor_market_participants, beneficiary,
    moderate, biographical, constrained, national).

% Advocates for coherent welfare-state design—whether universalist (everyone receives the same benefit at the same rate, funded by general revenue) or targeted (distribution matches demonstrated need and capacity to benefit). The universality paradox reading is structurally hostile to clarity positions: the policy succeeds politically precisely by evading the choice between universalism and targeting. Every attempt to clarify the distributional intent (detailed analysis of phase-out rates, public deliberation about funding mechanisms, empirical comparison of designs) is suppressed by the political actors who benefit from ambiguity. Ideological clarity is not a traditional stakeholder (does not collect or pay), but is a victim because the constraint's operation systematically prevents coherent collective evaluation of its distributional and normative commitments.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, ideological_clarity_stakeholders, excluded,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(unconditional_income_support__universality_paradox_reading, ideological_clarity_stakeholders).

% Manages revenue and implementation, and must resolve the ambiguity at decision time: who is actually taxed, at what rate, to fund the benefit? What is the actual phase-out schedule? Is the benefit truly universal or effectively means-tested? The political ambiguity creates a structural squeeze: every implementation choice breaks the ambiguity and alienates the coalition that benefited from it. If the authority implements steep phase-out rates (approaching the 'targeting' pole), right-wing advocates defect because the program looks like redistribution. If the authority implements shallow phase-out rates (approaching the 'universality' pole), the program becomes expensive and left-wing advocates pressure for higher rates. The authority is forced to enforce something, yet enforcement itself reveals what the ambiguity was hiding.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, fiscal_authority, agenda_setter,
    institutional, generational, constrained, national).

% Study distributional outcomes of UBS variants across different countries and designs. Empirical research (Marinescu & Stanig on taxing-back mechanisms; Zilinsky on universality paradox; Banerjee et al. on global pilot results) shows that actual distributional and fiscal outcomes converge across ideologically incompatible UBS designs. This evidence is what enables the universality paradox—policy designers can claim fidelity to incompatible visions while knowing the distributional facts converge. The researcher seat provides external corroboration that the paradox is real: the ideological difference is predominantly rhetorical, while the distributional consequences are empirically similar.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, comparative_welfare_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:fixing_cost_class(unconditional_income_support__universality_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Consolidates multiple categorical assistance programs into a single universal benefit stream, reducing administrative overhead and complexity of access. Provides a transparent, non-stigmatizing income support that is easier to navigate and understand than means-tested categorical programs. Offers a credible income floor that clarifies labor-market negotiation and reduces uncertainty about minimum income security. Coordinates expectations around a common baseline that applies to all citizens equally (nominally).
% TRANSFER_FUNCTION: Moves fiscal resources from the tax base (including wealthy UBS recipients via taxing-back/phase-out mechanisms) to an income floor available to all or most citizens. The magnitude and distribution of the actual transfer depends on the tax-back design: steep phase-out rates create a heavily-targeted distribution (wealthy refund most of the benefit); shallow phase-out rates create a more universal distribution (wealthy retain part of the benefit). The ambiguity is maintained by not specifying in advance which design will be implemented. Transfer narratives coexist: 'Everyone gets it equally' (universality) and 'The wealthy effectively pay it back through phase-outs' (targeting through the tax code).
% ABSENT_VOICES: Beneficiaries of targeted programs (housing assistance, disability supports, categorical allowances, job training, childcare subsidies) who stand to lose dedicated program support when consolidation is justified by universality rhetoric. These beneficiaries are not present in the coalition-building phase because their programs are framed as inefficient bureaucratic vestiges to be consolidated away. Labor economists and poverty researchers who would argue for detailed distributional analysis are muted by the broad political consensus around UBS. Fiscal conservatives who oppose the overall cost are partially muted by the right-wing market efficiency framing. Right-wing parties that might face contradictions (claiming market efficiency while supporting high phase-out rates) avoid exposing the contradictions by allowing implementation details to remain vague. The absent voices are those who would force clarity on the incompatibilities.
% DISAPPEARANCE_RATIONALE: If unconditional income support as a policy framework were removed, the policy space would reorganize around explicit choices that the ambiguity currently allows to remain deferred. Either nations would restore and strengthen categorical targeted programs (housing, disability, family supports), or they would implement a transparent universal basic income with explicit tax-back rates and clearly-stated distributional claims. Political actors would be forced to choose between the decommodification vision (high benefit, strong phase-out protection for low-income recipients) and the market efficiency vision (lower cost, market-based allocation). Labor-market outcomes, poverty rates, work participation, and fiscal distribution would shift based on which explicit model replaced the ambiguous framework. The constraint exists to defer this rearrangement; its removal would force it.
% FOUNDING_PROBLEM: Welfare-state fragmentation across categorical programs created administrative overhead, coverage gaps, and stigma. Labor-market insecurity and poverty persistence as targeted programs eroded or became means-tested. Political deadlock between left-wing universalists who wanted comprehensive single-payer coverage and right-wing marketizers who wanted minimal bureaucratic welfare. The founding problem is the demand to escape this deadlock and consolidate policy without resolving the underlying incompatibility between universalism and targeting, or between decommodification and market discipline.
% FOUNDING_PROBLEM_CORROBORATION: Welfare-state researchers (Esping-Andersen, Hacker, Pierson) attest that administrative fragmentation creates inefficiencies and access problems. Labor economists (Acemoglu, Autor, Katz) and anti-poverty advocates attest that labor-market insecurity and poverty persist despite categorical programs, and that means-testing and time limits undermine program effectiveness. Policy analysts across the ideological spectrum attest that cross-party consensus on a single policy vehicle is necessary to overcome legislative gridlock and enable welfare-state reform. What is NOT attested by outside voices independent of the benefiting parties: that unconditional income support as implemented actually solves the founding problem coherently. Instead, outside analysis (Marinescu & Stanig on taxing-back mechanisms; Zilinsky on the distributional design choice; Banerjee et al. on pilot outcomes across different designs) shows that fiscal and distributional outcomes are empirically similar across ideologically incompatible designs. This convergence suggests the policy does not resolve the founding problem—it defers resolution to implementation, where the ambiguity fails and hard choices become unavoidable.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__universality_paradox_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__universality_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unconditional_income_support__universality_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.48 (interval end), moderate rather than high, because the fiscal and distributional outcomes of UBS implementations empirically converge across ideologically incompatible designs (per Marinescu & Stanig on taxing-back; Zilinsky on universality paradox; Banerjee et al. on distributional impact across pilot variants). The constraint does not extract huge value; what it extracts is coherence and clarity. Suppression is high (0.62) because maintaining the ambiguity requires active enforcement: political actors must prevent the incompatibility from surfacing during implementation; policy designers must prevent detailed distributional analysis from becoming a public deliberation topic. Theater ratio is very high (0.71), rising from 0.55 to 0.71 over the interval, because the constraint's primary function is political—allowing incompatible narratives to coexist. As implementation proceeds, the gap between rhetorical universality and actual targeting widens; the theater (the performance of universality, the staging of simple cross-ideological consensus) increases relative to the functional core (the actual income support). Accessibility collapse is low (0.41) because alternatives to UBS are visible: countries maintain targeted programs, UBS can be replaced with other models, ideological alternatives remain articulable even if politically suppressed. Resistance is high (0.73) because major actors resist the constraint's logic: economists and welfare-state analysts resist the fusion of incompatible commitments; targeted-program advocates resist the consolidation; right-wing actors eventually resist when high phase-out rates become salient. The measurements show base_extractiveness rising from 0.35 to 0.48, then plateauing—the constraint settles into a stable form once implementation begins and the political costs of maintaining ambiguity become visible. Theater ratio rises throughout (0.55 to 0.71), showing that performative activity increases relative to functional coordination as the policy institutionalizes. Suppression requirement holds steady (0.50 to 0.62) because active enforcement of the ambiguity does not increase or decay—the constraint requires continuous suppression to hold.
 *
 * PERSPECTIVAL GAP:
 *   This is a kernel reading, not a unitary constraint, so seat divergence is structural and deliberate. From the political entrepreneur's seat (agenda-setter position), the ambiguity is a feature—it allows them to claim ownership of the policy without defending either comprehensive vision. From the targeted program recipient's seat (payer position), the same ambiguity is a trap—universality rhetoric becomes the justification for eliminating the categorical supports they depended on. From the policy designer's seat (beneficiary position), the ambiguity enables technical flexibility—taxing-back mechanisms allow them to defend both universality and efficiency simultaneously. From the fiscal authority's seat (agenda-setter position, but forced to implement), the ambiguity is a structural squeeze—any concrete implementation choice breaks the coalition and alienates one of the incompatible readings. The engine will compute divergent types across these seats: political entrepreneurs and policy designers will see coordination (the genuine simplification of administration, the real income floor); payers and fiscal authorities will see extraction (the ambiguity that enables cost-shifting, the suppression of clarity that prevents coherent evaluation). This divergence is not a defect in the authoring—it is the reading's entire point.
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs benefit from maintaining ambiguity without defending either vision fully (d ≈ 0.15–0.25, toward beneficiary end). Policy designers benefit from the technical flexibility of taxing-back mechanisms that allow them to claim universality while controlling costs (d ≈ 0.20–0.30, toward beneficiary end but less so than entrepreneurs). Targeted program recipients lose categorical supports and visibility when universality justifies consolidation (d ≈ 0.85–0.95, toward target end). Ideological clarity as a victim (non-agent, suppressed) has no individual d value but represents a structural suppression. Labor-market participants benefit materially from the income floor (d ≈ 0.40–0.50, near symmetric) but are trapped by the policy instability that the unresolved ambiguity creates—their exit options are constrained not by the benefit itself but by political uncertainty. Fiscal authorities are forced into the target position by the need to implement (d ≈ 0.70–0.80, toward target end, because they must absorb the political costs of breaking the ambiguity). The overrides that matter: policy designers and political entrepreneurs derive d from beneficiary declarations and exit options, but benefit not from receiving a transfer (they do not) but from rhetorical and political flexibility. This is 'beneficiary' in the sense of 'benefits from the constraint's operation' rather than 'receives material transfer.' An override to d ≈ 0.25 for policy designers captures this correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—fragmentation of welfare administration, labor-market insecurity, political deadlock—is contested in its status. Left-wing analysts attest it is live (poverty persists, work insecurity remains, welfare bureaucracy is still fragmented). Right-wing analysts claim it is partially dead (targeted programs fail because they are not market-based, so the answer is to replace bureaucracy with cash and let recipients choose). The policy that was supposed to solve the founding problem instead defers the choice that would resolve it. The universality paradox reading does not claim the founding problem is solved; it claims the policy entangles incompatible solutions (left's: decommodify labor; right's: marketize welfare) in the same vehicle. This is mandatrophy in progress: the policy's mandate (solve fragmentation and deadlock) remains unmet because the incompatibility was not resolved—it was hidden. Theater ratio rising to 0.71 indicates that maintaining the appearance of solution (the political consensus around UBS) consumes more activity than actually solving the problem (delivering coherent, effective income support). A mandatrophy_resolved flag would be premature here; the constraint is still functionally enforcing the political ambiguity. But the trajectory is toward mandatrophy: if base_extractiveness plateaus while theater_ratio continues to rise, the constraint will increasingly be performing its own solution rather than delivering one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is unconditional income support best understood as a single constraint that different parties read through incompatible ideological lenses, or as a family of distinct constraints that happen to share a policy label?',
    'Compare distributional and fiscal outcomes across implementations (taxing-back designs, phase-out rates, coverage breadth) against stated ideological commitments (universalism vs. targeting). If outcomes consistently converge while commitments diverge, the kernel reading (single constraint, multiple readings) is vindicated. If distinct implementations produce distinct outcomes that align with ideological claims, the policy is multiple constraints mislabeled as one.',
    'If kernel reading is vindicated, the universality paradox constraint is real—the ambiguity is structural and extractive. If multiple constraints, the ''paradox'' dissolves and each implementation should be evaluated separately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether UBS is a single contested kernel or multiple distinct policies.').

omega_variable(
    taxing_back_mechanism_political_stability,
    'Do taxing-back mechanisms that impose high phase-out rates on wealthy UBS recipients remain politically stable, or do they trigger coalition defection that forces redistributive retreat?',
    'Longitudinal observation of UBS implementations over 10+ years. Track (1) actual tax-back rates implemented, (2) political coalition composition, (3) support from originally-enthusiastic wealthy voters and right-wing parties when phase-out rates become salient. A rising theater_ratio without rising actual extraction would signal that political actors are using rhetorical universality to justify lower phase-out rates (the taxing back is performed, not functional).',
    'If wealthy recipients do defect and phase-out rates soften, the actual distributional outcome skews toward universal unconditional (wealthy do not pay back). If coalitions hold and rates remain steep, the distributional outcome skews toward targeted (wealthy do pay back). High theater_ratio suggests the gap between rhetorical and actual universality is growing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taxing_back_mechanism_political_stability, empirical, 'Political durability of high phase-out rates in UBS systems.').

omega_variable(
    ideological_clarity_as_victim_status,
    'Is ''ideological clarity'' a legitimate victim category, or is treating an abstract commitment as a victim an anthropomorphic error that obscures the real distributional victims (targeted program recipients)?',
    'Normative analysis of what counts as victimhood in a constraint structure. If victimhood requires material harm or thwarted agency by real actors, then ideological clarity is a byproduct, not a victim. If victimhood includes systemic suppression of coherent collective deliberation and institutional opacity, then ideological clarity is a victim of the constraint''s design. The resolution depends on how the framework defines harm.',
    'If ideological clarity is not a legitimate victim, the constraint has only one victim class (targeted program recipients) and the extractiveness score should be recalibrated. If it is legitimate, the current decomposition (two beneficiaries, two victims) stands and the theater_ratio is tracking the suppression of clarity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ideological_clarity_as_victim_status, conceptual, 'Ontological status of ideological clarity as a constraint victim.').

omega_variable(
    distributional_convergence_empirical_scope,
    'Do unconditional income support implementations across different ideological frameworks (European universalist, American right-wing market version, pilot-program variants) actually produce empirically similar distributional outcomes, or does the apparent convergence rest on a narrow set of assumptions (similar tax-back design, similar baseline income levels)?',
    'Comparative analysis of actual distributional consequences (Gini change, poverty-rate change, work-participation change) across UBS implementations with different tax-back designs, different benefit levels, and different funding sources. If outcomes converge only under specific design assumptions and diverge when those assumptions change, the convergence claim is fragile and the universality paradox is design-dependent rather than structural.',
    'If convergence is robust, the political ambiguity is tolerable—fiscal outcomes are similar so the ideological difference is rhetorical. If convergence is fragile, the ambiguity conceals material stakes and the constraint is more extractive than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_convergence_empirical_scope, empirical, 'Robustness of distributional convergence across UBS ideological variants.').

omega_variable(
    sibling_reading_coexistence,
    'Which structural relationship best captures the connection between the universality_paradox_reading and its sibling readings (freedom_floor_reading, dependency_trap_reading): forecloses, coexists_with, or influences?',
    'Analyze the core premises of each reading. If the universality paradox reading claims that incompatible ideological commitments coexist in the same policy vehicle, it is asserting that the freedom_floor and dependency_trap readings CAN coexist—neither forecloses the other, both are held simultaneously by different political actors. The universality paradox reading does not claim either sibling is false; it claims both are instrumentalized. Influences dynamics: the paradox reading''s argument (convergence of actual distributional outcomes) influences both siblings by undercutting their claimed differences in fiscal or distributional impact.',
    'The structural relationship determines how the three readings are arranged in the constraint network. If coexists_with, they are three live positions in an ongoing political dispute. If influences, the paradox reading is upstream and shapes the operating conditions of the other two.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Structural relationship between this reading and its siblings in the UBS kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(unco_tr_t0, observed).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__universality_paradox_reading, theater_ratio, 4, 0.6).
narrative_ontology:measurement_basis(unco_tr_t4, observed).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__universality_paradox_reading, theater_ratio, 8, 0.66).
narrative_ontology:measurement_basis(unco_tr_t8, observed).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__universality_paradox_reading, theater_ratio, 12, 0.71).
narrative_ontology:measurement_basis(unco_tr_t12, observed).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__universality_paradox_reading, theater_ratio, 16, 0.7).
narrative_ontology:measurement_basis(unco_tr_t16, observed).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__universality_paradox_reading, theater_ratio, 20, 0.71).
narrative_ontology:measurement_basis(unco_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(unco_be_t0, observed).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__universality_paradox_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement_basis(unco_be_t4, observed).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__universality_paradox_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement_basis(unco_be_t8, observed).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__universality_paradox_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement_basis(unco_be_t12, observed).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__universality_paradox_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement_basis(unco_be_t16, observed).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__universality_paradox_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(unco_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__universality_paradox_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(unco_su_t0, observed).
narrative_ontology:measurement(unco_su_t4, unconditional_income_support__universality_paradox_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement_basis(unco_su_t4, observed).
narrative_ontology:measurement(unco_su_t8, unconditional_income_support__universality_paradox_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement_basis(unco_su_t8, observed).
narrative_ontology:measurement(unco_su_t12, unconditional_income_support__universality_paradox_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement_basis(unco_su_t12, observed).
narrative_ontology:measurement(unco_su_t16, unconditional_income_support__universality_paradox_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement_basis(unco_su_t16, observed).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__universality_paradox_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(unco_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__universality_paradox_reading, 0.18).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the unconditional_income_support kernel. The sibling readings (freedom_floor_reading, dependency_trap_reading) instantiate different normative and empirical claims from the same policy label. All three are linked by network.affects_constraints edges. Decomposition is grounded in the ε-invariance principle: a single policy design (e.g., a specific taxing-back mechanism) produces materially similar fiscal and distributional outcomes regardless of the ideological framing that motivated its design. But the readings differ in what they claim is extractive: the freedom-floor reading claims the constraint extracts labor-market power inequalities; the dependency-trap reading claims it extracts work incentives and fiscal sustainability; the universality-paradox reading claims it extracts ideological clarity and systematically prevents coherent public deliberation about distributional intent. The three constraints are not alternative interpretations of the same phenomenon—they are structurally distinct constraints that happen to be implemented via overlapping policy vehicles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_income_support__universality_paradox_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
