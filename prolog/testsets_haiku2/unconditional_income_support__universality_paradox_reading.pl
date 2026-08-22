% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Unconditional Income Support: Universality Paradox Reading
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel 'unconditional
 *   income support'—specifically, the universality paradox reading. The
 *   universality paradox claims that unconditional income support (UIS)
 *   operates as a politically ambiguous Trojan horse: it attracts
 *   cross-ideological coalitions (libertarian anti-bureaucracy, egalitarian
 *   decommodification, centrist fiscal efficiency) by remaining vague about
 *   its implementation architecture. The readings are
 *   incompatible—libertarians intend a high tax-back rate (functionally a
 *   negative income tax), while egalitarians intend a zero or low tax-back
 *   rate (functionally a basic income)—but fiscal equivalence research shows
 *   that these designs produce similar distributional outcomes when the
 *   tax-back rate is adjusted. The constraint's operation extracts value for
 *   political entrepreneurs and policy designers by allowing them to build
 *   coalitions without resolving the normative contradictions, while
 *   suppressing ideological clarity and harming targeted program recipients
 *   who lose benefits in the universalization. The tension between low
 *   extractiveness (fiscal outcomes similar across designs) and high theater
 *   and suppression (the political ambiguity is actively maintained to hold
 *   the coalition together) is the core structural dynamic.
 *
 * KEY AGENTS:
 *   - Political entrepreneurs: right-libertarian and progressive coalition leaders who exploit the ambiguity to assemble temporary majority coalitions
 *   - Policy designers: technical economists who engineer the tax-back rate to claim fidelity to multiple readings simultaneously
 *   - Targeted program recipients: current beneficiaries of means-tested programs who lose when universality is used to justify benefit consolidation and cuts
 *   - Ideological clarity: non-agent entity representing the epistemic possibility of evaluating whether UIS delivers on each reading's normative premises
 *   - Coalition members: both progressive and libertarian organized actors who benefit from access to the policy vehicle but may experience implementation inconsistent with their reading
 *   - Centrist policymakers: officials with authority to choose implementation parameters and thus to manage the ambiguity
 *   - Research economists: observers producing evidence on fiscal equivalence that constrains but does not resolve the political ambiguity
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
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Unconditional Income Support: Universality Paradox Reading").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, 'dd8340e8-1f6c-4e70-a257-b0d4c2b772c8').
narrative_ontology:cs_kernel_codification('dd8340e8-1f6c-4e70-a257-b0d4c2b772c8', distributed).
narrative_ontology:cs_authority_grounding('dd8340e8-1f6c-4e70-a257-b0d4c2b772c8', distributed).
narrative_ontology:cs_reading_relation('dd8340e8-1f6c-4e70-a257-b0d4c2b772c8', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd8340e8-1f6c-4e70-a257-b0d4c2b772c8', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('dd8340e8-1f6c-4e70-a257-b0d4c2b772c8', foundational, fiscal_equivalence_masks_normative_conflict).
narrative_ontology:cs_axiom_status(fiscal_equivalence_masks_normative_conflict, holdable).
narrative_ontology:cs_axiom_grounding('dd8340e8-1f6c-4e70-a257-b0d4c2b772c8', fiscal_equivalence_masks_normative_conflict, empirically_contingent).
narrative_ontology:cs_axiom('dd8340e8-1f6c-4e70-a257-b0d4c2b772c8', secondary, ambiguity_enables_coalition_stability).
narrative_ontology:cs_axiom_status(ambiguity_enables_coalition_stability, holdable).
narrative_ontology:cs_axiom_grounding('dd8340e8-1f6c-4e70-a257-b0d4c2b772c8', ambiguity_enables_coalition_stability, instrumental).
narrative_ontology:cs_reference_frame('dd8340e8-1f6c-4e70-a257-b0d4c2b772c8', neutral_design_space).
narrative_ontology:cs_drift_state('dd8340e8-1f6c-4e70-a257-b0d4c2b772c8', post_implementation_parameter_specification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dd8340e8-1f6c-4e70-a257-b0d4c2b772c8', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers_with_flexibility).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, ideological_clarity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, progressive_coalition_members).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, libertarian_coalition_members).
narrative_ontology:constraint_vindicates(unconditional_income_support__universality_paradox_reading, fiscal_equivalence_across_designs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coalition builders who exploit the ambiguity of unconditional income support to assemble cross-ideological majorities. A right-wing entrepreneur frames UIS as a way to eliminate means-testing bureaucracy and replace inefficient targeted programs with a single transfer (libertarian reading). A left-wing entrepreneur frames the same policy as decommodifying basic needs and removing labor-market coercion (egalitarian reading). Both cite the same policy vehicle but have incompatible implementation architectures in mind. They benefit because the ambiguity allows them to defer resolving the contradictions until after the policy is enacted—when the 'taxing-back' mechanism becomes salient.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, beneficiary).

% Technical economists and policy staff who design the implementation architecture. They benefit from the ambiguity because it allows them to choose the 'taxing-back' rate (the rate at which benefits are withdrawn as income rises) to engineer nearly any distributional outcome while maintaining the rhetorical unity of 'unconditional universality.' A 100% tax-back makes UIS functionally identical to a negative income tax; a 0% tax-back makes it a true universal basic income. The flexibility to modulate this parameter allows designers to claim fidelity to multiple incompatible readings simultaneously.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers_with_flexibility, beneficiary,
    institutional, generational, mobile, national).

% Recipients of existing means-tested programs (housing assistance, food stamps, disability) who face the possibility of being folded into a UIS system with lower total benefits. The universality of UIS is used rhetorically to justify cuts to targeted programs on the grounds that 'everyone gets the same payment now'—but because the UIS level is set lower than the prior combined benefits, net recipients of targeted programs lose. Their exit option is limited: they cannot individually opt out of the policy change, and political opposition is fragmented because the policy claims to serve all groups equally.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, biographical, trapped, national).

% The epistemic possibility of evaluating whether UIS actually delivers on the promised normative commitments of different reading traditions. The ambiguity suppresses this—stakeholders cannot coherently assess whether the implementation honors their own reading's premises because the policy vehicle is engineered to be consistent with multiple readings. Clarity would require specifying the tax-back rate, the initial benefit level, and the rationale for those choices—but doing so would foreclose one or more of the competing readings, breaking the coalition.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, ideological_clarity, payer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(unconditional_income_support__universality_paradox_reading, ideological_clarity).

% Left-liberal actors (labor unions, anti-poverty organizations, progressive legislators) who see UIS as a vehicle for decommodification and reduced labor-market coercion. They benefit from the coalition that forms around the ambiguous UIS proposal because it brings right-libertarian support for eliminating means-testing, expanding the tax base that seems politically feasible. However, their success in passing UIS may result in an implementation (high tax-back, lower benefit level) that delivers neither decommodification nor coercion reduction—they have gained a universalist rhetorically, but not the freedom-floor substantively.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, progressive_coalition_members, beneficiary,
    organized, biographical, constrained, national).

% Right-libertarian actors (some think tanks, anti-regulation conservatives, budgetary hawks) who see UIS as a way to eliminate the welfare bureaucracy, means-testing, and paternalistic discretion. They benefit from the coalition that forms because it brings progressive support for large-scale redistribution, expanding the political coalition in a way that seems more fiscally sustainable than incrementally expanding targeted programs. However, their success may result in an implementation (low or zero tax-back, high benefit level) that delivers decommodification they did not intend—or conversely, a high tax-back design that is functionally indistinguishable from the negative income tax they argued against.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, libertarian_coalition_members, beneficiary,
    organized, biographical, constrained, national).

% Elected officials and appointed technocrats who navigate between the two coalitions and have the authority to choose the tax-back rate, benefit level, and phase-in architecture. They benefit from the ambiguity because it allows them to claim fidelity to both the universalist and efficiency framings, and to adjust the parameters in response to fiscal pressure or political opportunity without being accused of betraying the core commitment. Their flexibility is the mechanism that allows the ambiguity to persist even after enactment.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, centrist_policymakers, agenda_setter,
    institutional, biographical, mobile, national).

% The binding budget constraint that forces resolution of the ambiguity at implementation time. UIS cannot be truly unconditional, universal, and infinitely generous simultaneously. Once the policy is passed, the parameters must be chosen: benefit level, tax-back rate, and phase-in schedule. These choices embed distributional consequences that will be inconsistent with at least some of the readings. Fiscal pressure is the mechanism that transforms the ambiguous policy vehicle into a specific distributional outcome.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, fiscal_constraints, payer,
    analytical, immediate, analytical, national).
narrative_ontology:stakeholder_non_agent(unconditional_income_support__universality_paradox_reading, fiscal_constraints).

% Academic and policy economists who study UIS implementations and produce evidence on labor-supply effects, distributional outcomes, and fiscal equivalence. Their analyses show that across wide parameter ranges, different UIS designs (negative income tax, basic income, guaranteed minimum income) produce similar fiscal and distributional outcomes when the tax-back rate is adjusted. This evidence constrains but does not resolve the political ambiguity—it shows that the readings are fiscally equivalent but does not adjudicate between them normatively.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, research_economists, observer,
    institutional, generational, analytical, global).

% Critics of UIS from outside the two major coalitions—labor-demand economists who worry about inflation or wage pressure, communitarians who worry about erosion of reciprocal obligation, feminists who worry about unpaid care work being devalued—are largely excluded from the UIS design conversation because their critiques do not fit neatly into the universalist framing. Including their voices would require specifying implementation details that break the ambiguity.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, excluded_critique_voices, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:fixing_cost_class(unconditional_income_support__universality_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine coordination problem in welfare-state design: how to reduce the administrative burden and stigma of means-testing, and how to simplify the tax-transfer system by replacing multiple targeted programs with a single universal payment. The coordination function is real—most welfare states struggle with complexity, work disincentives baked into means-testing cliffs, and poverty traps. A unified, unconditional payment addresses these coordination failures.
% TRANSFER_FUNCTION: Moves income from taxpayers to recipients of the basic income transfer, with the rate and structure of redistribution determined by the benefit level, the tax-back rate (the rate at which benefits are withdrawn as income rises), and the phase-in architecture. The same policy vehicle can implement very different transfers depending on parameter choices: at 100% tax-back and low benefit level, it is a negative income tax with limited universality; at 0% tax-back and high benefit level, it is a true basic income with substantial universality. The ambiguity allows different parties to believe they are implementing different transfers with the same policy vehicle.
% ABSENT_VOICES: Critics of UIS from outside the two major coalitions are largely excluded: labor economists worried about inflation, communitarians concerned about reciprocal obligation, feminists arguing that unconditional income supports unpaid care work without valuing it, disability advocates worried that universality erases specific accessibility needs, and voices from the Global South worried about international distributional implications. Including these voices would require technical specificity about implementation that would break the political ambiguity.
% DISAPPEARANCE_RATIONALE: If unconditional income support were never enacted, the political economies of welfare states would remain organized around means-tested targeted programs, with their attendant administrative burden, stigma, and poverty traps. The political coalitions that formed around UIS would not have a shared vehicle for advancing their agendas. The distributional outcomes would differ based on which legacy targeted programs persisted versus which were reformed independently. The constraint's disappearance would force explicit choices about tax rates, benefit levels, and eligibility that cannot now be deferred.
% FOUNDING_PROBLEM: Multiple welfare-state design problems converge: (1) the administrative burden and perverse incentives of means-testing; (2) the labor-market inefficiency of benefit cliffs; (3) the political unpopularity of means-tested programs; (4) the ideological desire of different political traditions (libertarian, egalitarian, centrist) to replace the existing welfare system with something simpler and more universal. These problems are real, but they support multiple different solutions with incompatible normative premises.
% FOUNDING_PROBLEM_CORROBORATION: Multiple sources outside the benefiting parties attest the founding problems are live: OECD and World Bank reports document the administrative burden and poverty traps in means-tested systems; labor economists document work-disincentive effects of means-testing cliffs; political scientists document the political unpopularity of means-tested welfare relative to universal programs. However, these same sources also document that different UIS designs (negative income tax vs. basic income) solve these problems differently and with different normative implications, and that the 'universality paradox' (that fiscal outcomes converge even as normative readings diverge) is not universally agreed—some researchers argue that the distributional differences across designs matter more than the fiscal equivalence.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   The extractiveness is authored at 0.48 because the constraint's operation produces distributional outcomes that are substantively similar across the competing implementations (per taxing-back literature). However, the theater ratio is high (0.71) and rising through the pre-implementation interval (0.45 to 0.71 across the interval), because the constraint's primary function is political: to hold together incompatible coalitions by avoiding specification of the very parameters (benefit level, tax-back rate) that would resolve the ambiguity. The suppression score is moderate-high (0.62) because maintaining the ambiguity requires suppressing critiques from outside the two major coalitions (labor economists worried about inflation, feminists concerned about unpaid care, disability advocates worried about erasing specific needs). The accessibility_collapse is moderate (0.45): alternatives to UIS (incremental means-tested program reform, targeted wage subsidies, job guarantees) remain technically accessible, but the political momentum behind UIS makes them harder to articulate during the coalition-building phase. The resistance score is high (0.72): the constraint faces substantial resistance from targeted program recipients who would lose, from ideological purists on both sides who object to the compromise, and from economists and social critics who object to the methodological dodginess of the fiscal-equivalence claim. The measurements show the theater ratio rising sharply during the pre-implementation window (0.45 to 0.71) as the policy is debated and ambiguity is actively maintained, then stabilizing once the parameters are set. Extractiveness and suppression rise more gradually, then stabilize post-implementation when the ambiguity resolves into a specific design.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (political entrepreneurs, policy designers) experience UIS as a coordination mechanism and a source of political capital and flexibility. From their vantage, the ambiguity is a feature: it allows them to assemble coalitions that would otherwise be hostile, and to defer the hard distributional choices. The victim seats (targeted program recipients, ideological clarity advocates) experience UIS as suppression: their specific needs are subsumed into universality, their ability to evaluate the policy against their own normative commitments is thwarted. The coalition-member seats (progressive and libertarian organized actors) occupy an ambiguous middle ground: they benefit from access to the policy vehicle and the political momentum it creates, but they risk that implementation will betray their reading. The engine computes this divergence as different directionalities emerging from different structural positions: beneficiaries near d=0.2-0.3 (collects political capital, maintains ambiguity), victims near d=0.8-0.9 (loses specific programs, suppressed voice), coalition members near d=0.5-0.6 (genuine gains offset by implementation risk).
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs benefit from the ambiguity (d near beneficiary end, ~0.15-0.25) because it allows them to assemble coalitions without resolving contradictions—they collect political capital and policy authority from the ambiguity itself. Policy designers benefit (d ~0.2-0.3) because flexibility in the tax-back rate allows them to claim fidelity to multiple readings and to adjust parameters in response to fiscal pressure. Targeted program recipients are targets (d ~0.85-0.95): they lose specific, customized benefits when folded into a universal payment set at a lower level; they have trapped exit (cannot opt out of the policy change); their interests are not represented in the design coalition. Ideological clarity is harmed (treated as a non-agent victim, d ~1.0): the ambiguity is an epistemic suppression—it prevents coherent evaluation of whether the policy delivers on any reading's premises. Progressive and libertarian coalition members occupy d ~0.5-0.65: they benefit from the coalition and the policy vehicle (genuine coordination gain), but they risk that implementation will deviate from their reading (genuine coordination loss). The disproportionate representation of right-libertarian and centrist designers in the policy implementation phase biases the tax-back rate upward (toward negative-income-tax design), creating asymmetric extraction from the progressive coalition members.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids misclassification as pure rope (genuine coordination without asymmetry) because the ambiguity itself is the coordination mechanism, and the ambiguity is actively suppressed to maintain the coalition. A genuine coordination mechanism would make the trade-offs and synergies explicit, allowing all parties to evaluate their net position. Instead, the ambiguity allows political entrepreneurs to claim that incompatible readings are simultaneously supported by the same policy—this is not coordination; it is deferral of distributive conflict. The classification as tangled_rope (hybrid coordination/extraction) is structurally sound: there is a genuine coordination function (simplifying the tax-transfer system, reducing means-testing burden and stigma), and there is asymmetric extraction (political entrepreneurs and policy designers extract value from the ambiguity; targeted program recipients lose; ideological clarity is suppressed). The active enforcement requirement (requires_active_enforcement: true) captures the fact that the ambiguity must be actively defended—policy designers must choose the tax-back rate in a way that claims to satisfy both readings, legislators must avoid explicit specification of the parameters during the coalition-building phase, and centrist officials must resist pressure from both wings to commit to implementation details that would foreclose one reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_equivalence_robust,
    'Does fiscal equivalence across UIS designs actually hold across empirical parameter ranges and real institutional contexts, or is it an artifact of theoretical models with unrealistic assumptions?',
    'Systematic analysis of existing UIS pilots (Kenya, Finland, Ontario, Stockton) and natural experiments; comparison of distributional outcomes across programs with different tax-back rates and benefit levels; validation of labor-supply elasticity estimates in the real-world studies versus economic models.',
    'If fiscal equivalence is robust, the universality paradox holds and different readings lead to genuinely equivalent outcomes despite normative contradictions. If it fails—if labor-supply or price responses diverge sharply across designs—then the readings are not fiscally equivalent, the ambiguity conceals real distributional tradeoffs, and the constraint is pure extraction masquerading as coordination, not tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_equivalence_robust, empirical, 'Whether the fiscal equivalence claim that grounds the universality paradox is empirically robust.').

omega_variable(
    implementation_bias_toward_extraction,
    'Will the centrist policymakers and policy designers who set the tax-back rate and benefit level systematically bias toward designs that extract value for higher-income earners or preserve administrative convenience, rather than honoring the coalition members'' normative commitments?',
    'Institutional analysis of actual implementation pathways; comparison of design parameters chosen in different political contexts (countries with stronger labor movements, social-democratic traditions); observation of which coalition members'' preferences are honored in parameter-setting versus which are compromised.',
    'If designers systematically bias toward negative-income-tax designs (high tax-back rate, low benefit level), the constraint operates as extraction from progressive coalition members and targeted program recipients. If designers balance across readings, the constraint operates as genuine (if tension-ridden) coordination. If designers are captured by right-libertarian preferences, the suppression score and theater ratio both rise sharply post-implementation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_bias_toward_extraction, empirical, 'Structural bias in implementation toward designs that benefit policy designers and centrist officials over coalition members.').

omega_variable(
    ambiguity_resilience,
    'Can the political ambiguity actually be maintained post-implementation, or does the specificity of the chosen parameters inevitably break the coalition and force explicit ideological reckoning?',
    'Observation of post-implementation political dynamics in jurisdictions that enact UIS; measurement of coalition stability and continued rhetorical unity after parameters are fixed; tracking of how quickly excluded voices re-enter the policy conversation once the ambiguity is resolved.',
    'If ambiguity collapses immediately post-implementation, the constraint transitions from tangled_rope (pre-enactment, ambiguity active) to snare (post-enactment, extraction revealed). If ambiguity persists through rhetoric and public-facing justification while parameters are set, the constraint maintains tangled_rope status. The measurements show theater ratio stabilizing post-implementation; if it actually falls sharply, ambiguity has collapsed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ambiguity_resilience, empirical, 'Whether political ambiguity can survive implementation or inevitably collapses under the weight of parameter specificity.').

omega_variable(
    reading_incompatibility_logical,
    'Are the three readings (universality_paradox, freedom_floor, dependency_trap) logically incompatible such that no single framework could hold all three, or do they represent permissible differences in emphasis and priority that could coexist?',
    'Formal analysis of the axioms and premises of each reading; determination of whether they make contradictory empirical claims (that can be falsified by evidence) or contradictory normative claims (that reflect irreconcilable value priorities); assessment of whether a policymaker could rationally hold all three simultaneously or whether holding one requires rejecting the others.',
    'If the readings are logically incompatible, they foreclose each other and the universality_paradox reading should use ''forecloses'' relation to both siblings, not ''coexists_with''. If the readings are normatively incompatible but empirically compatible (they make different predictions about the same parameter), they coexist as competing framings of the same policy. This determines the reading_relations in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_incompatibility_logical, conceptual, 'Logical structure of the relationship between the three readings: are they incompatible, or permissibly different in emphasis?').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression of excluded voices (labor economists, feminists, disability advocates, Global South voices) structural (they are actively kept out of policy conversations) or internalized (they have internalized the universality framing and self-suppress their critiques)?',
    'Ethnographic study of policy design processes; interviews with excluded voices about their decision-making; natural experiment from jurisdictions where excluded voices were systematically included in design conversations versus excluded; measurement of post-exclusion-removal persistence of suppression.',
    'If suppression is structural, removing the exclusion mechanism should reduce suppression substantially. If suppression is partially internalized, the measurement would persist even after formal inclusion (voices would need to be re-educated on the legitimacy of their critiques). This affects the trajectory of suppression post-implementation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Mechanisms of suppression of excluded voices: structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(unco_tr_t0, projected).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__universality_paradox_reading, theater_ratio, 4, 0.56).
narrative_ontology:measurement_basis(unco_tr_t4, projected).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__universality_paradox_reading, theater_ratio, 8, 0.65).
narrative_ontology:measurement_basis(unco_tr_t8, projected).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__universality_paradox_reading, theater_ratio, 12, 0.7).
narrative_ontology:measurement_basis(unco_tr_t12, projected).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__universality_paradox_reading, theater_ratio, 16, 0.71).
narrative_ontology:measurement_basis(unco_tr_t16, projected).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__universality_paradox_reading, theater_ratio, 20, 0.71).
narrative_ontology:measurement_basis(unco_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(unco_be_t0, projected).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__universality_paradox_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement_basis(unco_be_t4, projected).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__universality_paradox_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement_basis(unco_be_t8, projected).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__universality_paradox_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement_basis(unco_be_t12, projected).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__universality_paradox_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement_basis(unco_be_t16, projected).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__universality_paradox_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(unco_be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__universality_paradox_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(unco_su_t0, projected).
narrative_ontology:measurement(unco_su_t4, unconditional_income_support__universality_paradox_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement_basis(unco_su_t4, projected).
narrative_ontology:measurement(unco_su_t8, unconditional_income_support__universality_paradox_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement_basis(unco_su_t8, projected).
narrative_ontology:measurement(unco_su_t12, unconditional_income_support__universality_paradox_reading, suppression_requirement, 12, 0.61).
narrative_ontology:measurement_basis(unco_su_t12, projected).
narrative_ontology:measurement(unco_su_t16, unconditional_income_support__universality_paradox_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement_basis(unco_su_t16, projected).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__universality_paradox_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(unco_su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__universality_paradox_reading, 0.18).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'unconditional_income_support'. The universality_paradox_reading analyzes UIS as politically ambiguous—incompatible normative readings converge on similar fiscal outcomes via the tax-back mechanism. The freedom_floor_reading analyzes UIS as autonomy-enabling decommodification (zero or low tax-back, high universality). The dependency_trap_reading analyzes UIS as incentive-distorting subsidy (high tax-back, low universality, upward redistribution). The three readings instantiate different constraints with different beneficiary/victim structures, different epsilon values, and different types. They are linked by network.affects_constraints to model how the implementation of one reading constrains or forecloses the others, and how evidence about fiscal equivalence affects the empirical status of each reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_income_support__universality_paradox_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
