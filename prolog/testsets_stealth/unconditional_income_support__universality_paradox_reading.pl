% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Unconditional Income Support — Universality Paradox Reading (Cross-Ideological Trojan Horse)
 *   domain: political economy/social policy/welfare state theory
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the kernel
 *   unconditional_income_support: the universality paradox reading, under
 *   which the vehicle's operative structure is political ambiguity — a
 *   deliberately unresolved policy object whose cross-ideological appeal
 *   depends on no faction ever fixing what it actually is. The standing
 *   arrangement under contest (the ε referent, assessed by this reading's own
 *   lights) is the income-support vehicle as it operates in political
 *   discourse: championed everywhere, specified nowhere, with implementation
 *   paths that taxing-back research shows converge on similar fiscal
 *   outcomes. Because the material stakes converge, ε is LOW (0.30): the
 *   extraction this reading registers is political and epistemic, not fiscal
 *   — coalition value harvested by brokers, evaluative capacity and
 *   targeted-program security paid by others. Constraint family: the
 *   colloquial label decomposes into three structurally distinct readings per
 *   the ε-invariance principle — freedom_floor_reading (household-autonomy
 *   beneficiaries, lower ε), dependency_trap_reading (work-capacity victims,
 *   higher ε), and this file. Each is a separate story linked via
 *   network.affects_constraints; ε is never averaged or hedged across them.
 *   KEY AGENTS (by structural relationship): political_entrepreneurs —
 *   agenda-setting beneficiary (institutional/mobile), harvests coalition
 *   value from maintained ambiguity; policy_designers — secondary beneficiary
 *   (moderate/mobile), converts tax-back mechanics into rhetorical
 *   flexibility; targeted_program_recipients — primary target
 *   (powerless/trapped), whose programs are the bargaining chip;
 *   deliberative_public — target of the epistemic cost
 *   (moderate/constrained); market_liberal_endorsers and
 *   egalitarian_endorsers — dual-positioned coalition members (organized;
 *   mobile vs constrained exit respectively); future_claimants — excluded
 *   seat; welfare_state_researchers — analytical observer. Interval mapping:
 *   T=0 is 1970 (Family Assistance Plan era), T=56 is 2026.
 *
 * KEY AGENTS:
 *   - political_entrepreneurs: agenda-setting beneficiary (institutional/mobile) — controls the vehicle's framing, collects coalition value, exits easily to the next vehicle
 *   - policy_designers: beneficiary with agenda-setting secondary role (moderate/mobile) — produces the tax-back machinery that keeps both rhetorical descriptions available
 *   - targeted_program_recipients: primary target (powerless/trapped) — bear the dilution of needs-specific support into universalist rhetoric
 *   - deliberative_public: target of the epistemic cost (moderate/constrained) — cannot evaluate what the coalition refuses to specify
 *   - market_liberal_endorsers: dual-positioned coalition member (organized/mobile) — collects membership, pays in legitimating a rival reading
 *   - egalitarian_endorsers: dual-positioned coalition member (organized/constrained) — collects membership, pays with their best alternative platform being cannibalized
 *   - future_claimants: excluded seat (powerless/trapped) — bound by choices made in their absence
 *   - welfare_state_researchers: analytical observer (analytical/analytical) — produce the convergence findings every faction cites selectively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.3).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.5).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Unconditional Income Support — Universality Paradox Reading (Cross-Ideological Trojan Horse)").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political economy/social policy/welfare state theory").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, '60687bdf-94c1-432a-88d1-5ddff8d034b5').
narrative_ontology:cs_kernel_codification('60687bdf-94c1-432a-88d1-5ddff8d034b5', distributed).
narrative_ontology:cs_authority_grounding('60687bdf-94c1-432a-88d1-5ddff8d034b5', distributed).
narrative_ontology:cs_reading_relation('60687bdf-94c1-432a-88d1-5ddff8d034b5', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('60687bdf-94c1-432a-88d1-5ddff8d034b5', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('60687bdf-94c1-432a-88d1-5ddff8d034b5', foundational, implementation_paths_converge_fiscally).
narrative_ontology:cs_axiom_status(implementation_paths_converge_fiscally, holdable).
narrative_ontology:cs_axiom_grounding('60687bdf-94c1-432a-88d1-5ddff8d034b5', implementation_paths_converge_fiscally, empirically_contingent).
narrative_ontology:cs_axiom('60687bdf-94c1-432a-88d1-5ddff8d034b5', foundational, cross_ideological_appeal_requires_unresolved_specification).
narrative_ontology:cs_axiom_status(cross_ideological_appeal_requires_unresolved_specification, holdable).
narrative_ontology:cs_axiom_grounding('60687bdf-94c1-432a-88d1-5ddff8d034b5', cross_ideological_appeal_requires_unresolved_specification, empirically_contingent).
narrative_ontology:cs_reference_frame('60687bdf-94c1-432a-88d1-5ddff8d034b5', underspecified_coalition_vehicle).
narrative_ontology:cs_drift_state('60687bdf-94c1-432a-88d1-5ddff8d034b5', contemporary_post_pilot_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('60687bdf-94c1-432a-88d1-5ddff8d034b5', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, deliberative_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, market_liberal_endorsers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, egalitarian_endorsers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, market_liberal_endorsers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, egalitarian_endorsers).
narrative_ontology:constraint_vindicates(unconditional_income_support__universality_paradox_reading, taxing_back_convergence_findings).
narrative_ontology:constraint_vindicates(unconditional_income_support__universality_paradox_reading, constructive_ambiguity_coalition_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Champion the universal income vehicle across party platforms, primaries, and movement conferences. Their working asset is the breadth of the tent: the same sentence rallies market liberals, egalitarians, and administrative simplifiers because none of them is forced to hear the others' version. They control which financing specifications reach the agenda and routinely defer them. Exit is easy — the coalition-brokering skill ports to whatever vehicle comes next.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, beneficiary).

% Draft the tax-back schedules, phase-out curves, and pilot protocols that let the same vehicle be described as fiscally restrained to one audience and transformative to another. Rhetorical flexibility is their professional product; each new costing exercise refreshes it without committing anyone. They circulate between think tanks, ministries, and pilot evaluation teams.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, policy_designers, agenda_setter).

% Rely on disability payments, housing assistance, and in-work top-ups sized to specific needs. Universalist rhetoric treats their programs as redundant complexity awaiting consolidation into a flat grant; every coalition statement that everyone receives the same check prices their extra needs out of the conversation. They cannot exit the political process in which their programs serve as bargaining chips.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, immediate, trapped, national).

% Encounter the vehicle as a slogan endorsed by opposing ideologies and cannot tell which distributive outcome an endorsement commits them to. Evaluating it would require resolving financing and tax-back design that the coalition declines to fix. Disengagement is the only available exit, and it deepens the advantage of brokers who thrive on vagueness.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, deliberative_public, payer,
    moderate, generational, constrained, national).

% Endorse the vehicle as a replacement for bureaucratic means-testing and a contraction of the administrative state. They collect coalition membership and agenda relevance; they pay by lending legitimacy to a vehicle their coalition partners read as a large new entitlement. Alternative vehicles — voucherization, program sunsets — remain open to them if they walk.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, market_liberal_endorsers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, market_liberal_endorsers, payer).

% Endorse the vehicle as an unconditional security floor free of stigma and means-test humiliation. They collect coalition membership too, but their natural alternative — expanding targeted programs — is exactly what universalist rhetoric devalues, so leaving the vehicle costs them their strongest existing platform.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, egalitarian_endorsers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, egalitarian_endorsers, payer).

% People who will need income support after the next recession, illness, or displacement. No seat in the current coalition represents their claims; design choices made under present-day ambiguity will bind them, and they have no way to attend the conversation that sets those choices.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, future_claimants, excluded,
    powerless, generational, trapped, national).

% Run the taxing-back microsimulations, pilot evaluations, and comparative welfare-state histories showing net fiscal outcomes converging across implementation designs. They publish outside the coalition and bear none of its costs; every faction cites their findings selectively.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, welfare_state_researchers, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:fixing_cost_class(unconditional_income_support__universality_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem in welfare reform politics: advocates scattered across incompatible ideologies need a single shared object to coordinate on before any reform can move. Unconditional income support supplies that object, letting market-liberal, egalitarian, and administrative-simplification factions campaign together without first resolving their differences.
% TRANSFER_FUNCTION: Moves political value — coalition membership, agenda control, media attention, rhetorical cover — from the general evaluative public and from targeted-program constituencies toward coalition brokers and designers. Materially it moves little: once tax-back financing is accounted, net transfers converge across rival designs, so the fiscal flow is modest and roughly design-invariant.
% ABSENT_VOICES: Future claimants, targeted-program specialists, and anyone demanding specification-before-endorsement are outside the coalition conversation. Specification-demanders are not merely absent but actively discouraged: insisting on a concrete financing path breaks the tent, so the price of admission is suspension of the very question evaluation requires.
% DISAPPEARANCE_RATIONALE: If the vehicle vanished overnight, the cross-ideological reform space would reorganize: market liberals would return to means-testing critique and privatization proposals, egalitarians to targeted expansion, and the entrepreneurs would rebuild around whichever successor vehicle tolerated the most ambiguity. Pilot infrastructure and taxing-back research would persist, attached to narrower designs.
% FOUNDING_PROBLEM: Assemble a welfare-reform coalition spanning incompatible ideologies without pre-resolving distributive philosophy — the problem crystallized when dissatisfaction with bureaucratic means-testing cut across left and right but no single design satisfied both.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: political-science literature on policy ambiguity and coalition formation, historiography of the Nixon Family Assistance Plan and the negative-income-tax experiments, and testimony of opposition politicians who concede the vehicle's unusual breadth. No attesting source sits inside the entrepreneur or designer beneficiary seats alone.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__universality_paradox_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__universality_paradox_reading_tests).
:- end_tests(unconditional_income_support__universality_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.30) because the vehicle's fiscal footprint converges across designs: taxing-back microsimulations show net-transfer distributions that are roughly invariant to whether the delivery mechanism is a demogrant, a negative income tax, or a phased credit. What varies is packaging, and packaging is where the extraction lives — hence a low material ε paired with a genuinely asymmetric political structure. Suppression (0.50) is real but discursive: the constraint's persistence requires actively deflecting specification pressure (agenda control, 'details later', pilot-not-policy framing), and the suppression_requirement series traces that enforcement burden rising as costing offices, pilots, and referenda made vagueness harder to hold. Theater_ratio (0.55) reflects a growing share of activity — white papers, referendum campaigns, pilot announcements — that performs commitment while deferring implementation; it crosses 0.5 late in the interval, the Goodhart-drift signature worth watching. Accessibility_collapse is low (0.30): understanding the Trojan-horse mechanism closes no alternatives — targeted expansion, sectoral bargains, and rival vehicles all remain available, which is precisely why this is not a mountain claim. Resistance (0.55) comes from targeted-program defenders, fiscal scorers, and specification-demanders. All three temporal series share one grid (T=0,12,26,40,48,52,56) so no metric is sampled against another's end-state; suppression_requirement is tracked because the story's narrative specifically traces enforcement-capacity change (the maturing machinery of ambiguity maintenance), not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the political_entrepreneur seat the arrangement computes near pure coordination: a coalition machine they built, run, and profit from, with costs borne elsewhere. From the targeted_program_recipient seat the same structure computes as enforced extraction: their concrete security is the raw material of other people's coalition. The deliberative_public seat sits between — they receive a coordination good (a possible reform) they are structurally prevented from evaluating. The dual-positioned endorser seats split on exit: the market-liberal endorsers can walk to rival vehicles (mobile), the egalitarian endorsers cannot (their alternative is the thing being cannibalized), so identical nominal roles yield different effective positions. The engine computes these per-seat classifications from the authored power/exit/role data; the claimed_type does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Political_entrepreneurs and policy_designers sit near the beneficiary pole (low d): the ambiguity subsidizes them, and their mobility means arbitrage-grade exit from any given instantiation. The endorser seats derive as mixed — declared beneficiary with a payer secondary role pulls d upward from pure subsidy, and the egalitarian seat's constrained exit traps them nearer symmetry than their role alone implies. Targeted_program_recipients derive near the full-target pole (high d): they bear the transfer of program security, with trapped exit amplifying effective extraction despite the low base ε. Deliberative_public derives moderately high: a real cost (evaluative capacity) borne with constrained exit. Future_claimants hold the excluded role — commentary-grade absence feeding the consensus-provenance check, never a correction-grade override. Scope is national throughout: verification of what the vehicle 'really is' is hard precisely at the scale where the coalition operates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — assembling a cross-ideological reform coalition without pre-resolving distributive philosophy — is still live, so no mandatrophy is declared. The tangled_rope classification does double preventive work: against the rope mislabel (which would ignore that recipients and the deliberative public pay for the coordination) and against the snare mislabel (which would ignore the genuine coordination service and the convergent-fiscal finding that keeps material extraction low). The risk trajectory runs toward piton, not snare: if the founding problem ever dies — ideologies converging, or the vehicle's ambiguity finally priced — the expected decay mode is theatrical maintenance (endorsements without vehicles, pilots without policy), and the theater_ratio series crossing 0.5 in the final third of the interval is the early signature of that decay path. Watching that series is how a future analyst distinguishes living ambiguity from embalmed ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates only the universality_paradox_reading of the kernel unconditional_income_support; how would classification change under the sibling readings freedom_floor_reading and dependency_trap_reading?',
    'Generate and compare the two sibling stories against the same interval and structural data, classifying each reading separately rather than averaging across them.',
    'Under freedom_floor_reading the beneficiary set shifts to autonomy-seeking households and ε falls toward the coordination floor; under dependency_trap_reading ε rises sharply with work-capacity victims added. Only this reading yields the low-material-ε tangled_rope profile authored here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of a contested policy kernel; sibling readings are separate constraints, not measurement settings on this one.').

omega_variable(
    disagreement_location_material_vs_political,
    'Where do the three readings actually disagree — on the vehicle''s material effects (transfers, labor-supply incentives) or on its political function (coalition maintenance through ambiguity)?',
    'Locate each sibling''s foundational axiom and determine which structural element it contests; test whether empirical material-effect data could in principle settle the dispute.',
    'If disagreement is located in political function, the readings coexist as competing frames over one vehicle indefinitely; if in material effects, labor-supply and net-transfer evidence could collapse the contest and force reclassification of the whole family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_material_vs_political, conceptual, 'Location of the inter-reading disagreement within the kernel''s structure.').

omega_variable(
    taxing_back_convergence_robustness,
    'Is the fiscal and distributional convergence across implementation designs robust across financing regimes, or an artifact of the tax-back parameters studied so far?',
    'Extend microsimulation across financing variants (flat surtax, progressive phase-out, revenue-source substitutions) and test sensitivity of the resulting net-transfer distributions.',
    'If convergence fails, some designs carry materially higher ε, the ambiguity becomes expensive rather than cheap, and this reading''s low-extractiveness claim weakens toward the dependency_trap profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taxing_back_convergence_robustness, empirical, 'Robustness of the convergent-fiscal-outcome finding that anchors this reading''s low ε.').

omega_variable(
    ambiguity_intentionality,
    'Is the vehicle''s ambiguity strategically maintained by political entrepreneurs, or an emergent residue of sincere normative disagreement among endorsers?',
    'Trace specification episodes historically: when concrete financing proposals surfaced, did brokers deflect, and did coalition cohesion track specificity?',
    'Strategic maintenance supports attributing extraction to the political_entrepreneurs seat as agenda-setter capture; emergent ambiguity redistributes the story toward structural coalition dynamics and lowers individual-seat culpability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_intentionality, conceptual, 'Whether the Trojan-horse quality is designed or emergent.').

omega_variable(
    targeted_program_counterfactual,
    'Would targeted programs serving low-income households have expanded further absent the universalist vehicle absorbing reform energy and legitimacy?',
    'Compare welfare-state trajectories across jurisdictions with and without strong universalist vehicles, controlling for fiscal capacity and partisan composition.',
    'A confirmed crowding-out counterfactual raises the effective extraction borne by targeted_program_recipients well above the authored low ε; refutation keeps ε low and confines the harm mostly to the epistemic register.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(targeted_program_counterfactual, empirical, 'Counterfactual status of the harm to targeted-program constituencies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(unco_tr_t0, observed).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__universality_paradox_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(unco_tr_t12, observed).
narrative_ontology:measurement(unco_tr_t26, unconditional_income_support__universality_paradox_reading, theater_ratio, 26, 0.36).
narrative_ontology:measurement_basis(unco_tr_t26, observed).
narrative_ontology:measurement(unco_tr_t40, unconditional_income_support__universality_paradox_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(unco_tr_t40, observed).
narrative_ontology:measurement(unco_tr_t48, unconditional_income_support__universality_paradox_reading, theater_ratio, 48, 0.48).
narrative_ontology:measurement_basis(unco_tr_t48, observed).
narrative_ontology:measurement(unco_tr_t52, unconditional_income_support__universality_paradox_reading, theater_ratio, 52, 0.53).
narrative_ontology:measurement_basis(unco_tr_t52, observed).
narrative_ontology:measurement(unco_tr_t56, unconditional_income_support__universality_paradox_reading, theater_ratio, 56, 0.55).
narrative_ontology:measurement_basis(unco_tr_t56, observed).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement_basis(unco_be_t0, observed).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__universality_paradox_reading, base_extractiveness, 12, 0.18).
narrative_ontology:measurement_basis(unco_be_t12, observed).
narrative_ontology:measurement(unco_be_t26, unconditional_income_support__universality_paradox_reading, base_extractiveness, 26, 0.2).
narrative_ontology:measurement_basis(unco_be_t26, observed).
narrative_ontology:measurement(unco_be_t40, unconditional_income_support__universality_paradox_reading, base_extractiveness, 40, 0.23).
narrative_ontology:measurement_basis(unco_be_t40, observed).
narrative_ontology:measurement(unco_be_t48, unconditional_income_support__universality_paradox_reading, base_extractiveness, 48, 0.26).
narrative_ontology:measurement_basis(unco_be_t48, observed).
narrative_ontology:measurement(unco_be_t52, unconditional_income_support__universality_paradox_reading, base_extractiveness, 52, 0.29).
narrative_ontology:measurement_basis(unco_be_t52, observed).
narrative_ontology:measurement(unco_be_t56, unconditional_income_support__universality_paradox_reading, base_extractiveness, 56, 0.3).
narrative_ontology:measurement_basis(unco_be_t56, observed).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__universality_paradox_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(unco_su_t0, observed).
narrative_ontology:measurement(unco_su_t12, unconditional_income_support__universality_paradox_reading, suppression_requirement, 12, 0.2).
narrative_ontology:measurement_basis(unco_su_t12, observed).
narrative_ontology:measurement(unco_su_t26, unconditional_income_support__universality_paradox_reading, suppression_requirement, 26, 0.28).
narrative_ontology:measurement_basis(unco_su_t26, observed).
narrative_ontology:measurement(unco_su_t40, unconditional_income_support__universality_paradox_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement_basis(unco_su_t40, observed).
narrative_ontology:measurement(unco_su_t48, unconditional_income_support__universality_paradox_reading, suppression_requirement, 48, 0.42).
narrative_ontology:measurement_basis(unco_su_t48, observed).
narrative_ontology:measurement(unco_su_t52, unconditional_income_support__universality_paradox_reading, suppression_requirement, 52, 0.47).
narrative_ontology:measurement_basis(unco_su_t52, observed).
narrative_ontology:measurement(unco_su_t56, unconditional_income_support__universality_paradox_reading, suppression_requirement, 56, 0.5).
narrative_ontology:measurement_basis(unco_su_t56, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, identity_coordination).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'unconditional income support' per the ε-invariance principle: one policy vehicle, three structurally distinct readings, each with its own ε, beneficiary/victim structure, and type. This (universality paradox) story links to both siblings. Flow within the family: the freedom_floor reading supplies the normative case quoted in coalition materials (upstream, higher empirical confidence in its autonomy findings); the dependency_trap reading supplies the standing opposition case; this reading explains why both circulate simultaneously without colliding — the vehicle's ambiguity is what lets incompatible claims ride together. ε differs by wide margin across the family (low here, lower under freedom_floor, substantially higher under dependency_trap), which is the signature that these are different constraints sharing a label, not one constraint viewed from angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
