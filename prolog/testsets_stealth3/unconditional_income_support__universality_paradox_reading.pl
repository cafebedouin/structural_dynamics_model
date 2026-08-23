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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: unconditional_income_support__universality_paradox_reading
 *   human_readable: Unconditional Income Support as Politically Ambiguous Vehicle (Universality-Paradox Reading)
 *   domain: political economy/social policy/welfare state theory
 *
 * SUMMARY:
 *   Unconditional income support — any regular cash transfer unconditioned on
 *   behavior (universal basic income, negative income tax, citizen's
 *   dividend) — operates politically as an ambiguous container: the same
 *   label is sold to progressives as poverty abolition, to libertarians as
 *   welfare-bureaucracy replacement, and to technocrats as administrative
 *   simplification. The ambiguity is load-bearing: resolving the design
 *   question (flat universal grant funded by broad taxation versus tapered
 *   transfer clawed back through the tax code) would split the coalition, so
 *   the vehicle's viability depends on never resolving it. Implementation
 *   research finds the rival paths converge on similar net fiscal and
 *   distributional outcomes once tax-backs are modeled, which lowers the
 *   material stakes of the deferred choice and lets the ambiguity persist
 *   cheaply. Costs land elsewhere: universality rhetoric is used to justify
 *   consolidating or cutting targeted programs whose recipients would
 *   net-lose under a flat grant, and public evaluation capacity degrades as
 *   the same proposal is described incompatibly to different audiences. KEY
 *   AGENTS (by structural relationship): - political_entrepreneurs:
 *   agenda-setting beneficiary (powerful/arbitrage) — maintains the
 *   ambiguity, collects coalition rents - policy_designers: beneficiary
 *   (institutional/mobile) — tax-back mechanics enabling rhetorical
 *   universality - targeted_program_recipients: primary payer
 *   (powerless/trapped) — bear consolidation losses -
 *   middle_income_taxpayers: near-symmetric beneficiary/payer
 *   (organized/constrained) - targeted_program_defenders: excluded
 *   (organized/constrained) — absent from design rooms - ideological_clarity:
 *   non-agent payer (agent: false) — epistemic commons, excluded from
 *   derivation - distributional_researchers: analytical observer — document
 *   the convergence Claim/metric independence: claimed_type tangled_rope
 *   reflects the structure this reading asserts — genuine coalition
 *   coordination entangled with asymmetric extraction; the metrics are
 *   authored descriptively (low-moderate epsilon, rising suppression
 *   requirement, oscillating theater) without tuning to any predicted engine
 *   output. This story instantiates one reading of the
 *   unconditional_income_support kernel; committer structure is recorded in
 *   kernel_context and the kernel_reading_contestation omega.
 *
 * KEY AGENTS:
 *   - political_entrepreneurs: agenda-setting beneficiary (powerful/arbitrage) — champions the vehicle across audiences, maintains design ambiguity, collects coalition support
 *   - policy_designers: beneficiary (institutional/mobile) — craft tax-back mechanisms enabling rhetorical universality with administrative targeting
 *   - targeted_program_recipients: primary payer (powerless/trapped) — bear crowding-out when universality rhetoric justifies consolidating targeted benefits
 *   - middle_income_taxpayers: near-symmetric beneficiary/payer (organized/constrained) — receive headline grants, fund them through taxes, net position near zero
 *   - targeted_program_defenders: excluded (organized/constrained) — absent from the coalition rooms where designs are sketched
 *   - ideological_clarity: non-agent payer (agent: false) — the epistemic commons degraded by strategic ambiguity; listed for completeness, excluded from derivation
 *   - distributional_researchers: analytical observer (analytical/analytical) — document fiscal convergence across design families
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.32).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.42).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Unconditional Income Support as Politically Ambiguous Vehicle (Universality-Paradox Reading)").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political economy/social policy/welfare state theory").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, 'cb4c8e87-b73f-4312-82e8-982b558db107').
narrative_ontology:cs_kernel_codification('cb4c8e87-b73f-4312-82e8-982b558db107', formalized).
narrative_ontology:cs_authority_grounding('cb4c8e87-b73f-4312-82e8-982b558db107', expertise).
narrative_ontology:cs_interpretation_layer_present('cb4c8e87-b73f-4312-82e8-982b558db107').
narrative_ontology:cs_reading_relation('cb4c8e87-b73f-4312-82e8-982b558db107', unconditional_income_support__freedom_floor_reading, influences).
narrative_ontology:cs_reading_relation('cb4c8e87-b73f-4312-82e8-982b558db107', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('cb4c8e87-b73f-4312-82e8-982b558db107', foundational, design_ambiguity_is_load_bearing).
narrative_ontology:cs_axiom_status(design_ambiguity_is_load_bearing, holdable).
narrative_ontology:cs_axiom_grounding('cb4c8e87-b73f-4312-82e8-982b558db107', design_ambiguity_is_load_bearing, empirically_contingent).
narrative_ontology:cs_axiom('cb4c8e87-b73f-4312-82e8-982b558db107', foundational, fiscal_convergence_neutralizes_path_choice).
narrative_ontology:cs_axiom_status(fiscal_convergence_neutralizes_path_choice, holdable).
narrative_ontology:cs_axiom_grounding('cb4c8e87-b73f-4312-82e8-982b558db107', fiscal_convergence_neutralizes_path_choice, empirically_contingent).
narrative_ontology:cs_reference_frame('cb4c8e87-b73f-4312-82e8-982b558db107', contested_proposal_space).
narrative_ontology:cs_drift_state('cb4c8e87-b73f-4312-82e8-982b558db107', post_pilot_evaluation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('cb4c8e87-b73f-4312-82e8-982b558db107', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, ideological_clarity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, middle_income_taxpayers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, middle_income_taxpayers).
narrative_ontology:constraint_vindicates(unconditional_income_support__universality_paradox_reading, fiscal_convergence_thesis).
narrative_ontology:constraint_vindicates(unconditional_income_support__universality_paradox_reading, big_tent_coalition_feasibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Champion unconditional cash transfers to multiple audiences at once: to progressives as poverty relief, to libertarians as welfare-bureaucracy replacement, to technocrats as administrative simplification. Fundraise, build coalitions, and keep the specific design question — flat grant versus tax-back taper — deliberately open, because resolving it would force each camp to confront the parts of the package it dislikes. Their asset is the breadth of the tent; their returns are donations, profile, and agenda influence.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, agenda_setter,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, beneficiary).

% Economists and policy technicians in ministries, central banks, and think tanks who draft the delivery mechanics: negative income tax schedules, clawback rates, integration with existing tax codes. The tax-back architecture lets a scheme be described as universal while operating as targeted — the same instrument supports 'everyone gets a check' rhetoric and 'only net losers pay' accounting. Their reward is influence over the design space and publication of consequential work.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary,
    institutional, generational, mobile, national).

% Households relying on means-tested benefits — housing vouchers, disability top-ups, food assistance — whose grants exceed what a flat universal payment would replace. When universal schemes are pitched as replacements for the 'patchwork,' their specific, higher-value supports are the ones consolidated away. They have no exit from the budget process and little voice in the rooms where replacement designs are drawn.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, biographical, trapped, national).

% Earners above the poverty line who would receive the headline payment and fund it through their taxes. Under tax-back designs their net position is close to zero — the check comes back as withholding — but the visible check and the invisible clawback are psychologically distinct, which is what makes the pitch work on them. They can vote the scheme up or down but cannot easily evaluate what it actually does to them.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, middle_income_taxpayers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, middle_income_taxpayers, payer).

% Advocacy organizations, caseworkers, and program administrators defending the existing means-tested stack. They are conspicuously absent from the bipartisan convenings, philanthropy-funded pilots, and cross-party working groups where universal schemes get sketched; they encounter the designs only once announced, in opposition mode. Their leverage is defensive: mobilizing recipients and allies against consolidation proposals.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_defenders, excluded,
    organized, biographical, constrained, national).

% Non-agent entry kept for narrative completeness: the possibility of coherent public evaluation of cash-transfer policy. Strategic ambiguity spends it down — the same proposal is described as austere libertarianism to one audience and expansive progressivism to another, so no common account of what is being debated accumulates. Listed because the reading identifies degraded evaluation capacity as a cost of the arrangement; excluded from structural derivation because it is not an actor.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, ideological_clarity, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(unconditional_income_support__universality_paradox_reading, ideological_clarity).

% Academic economists and evaluation teams running microsimulations and pilot assessments (negative income tax experiments, resource-dividend analyses, guaranteed-income pilots). Their findings — that net fiscal and distributional outcomes converge across design families once tax-backs are modeled — are the empirical backbone of this reading. They collect no rents and bear no costs; they publish.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, distributional_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:fixing_cost_class(unconditional_income_support__universality_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assembles a pro-cash-transfer coalition spanning constituencies that agree on no single design: it lets camps endorse a label ('unconditional income support') while each projects its preferred mechanism onto it, deferring the flat-grant-versus-tax-back choice that would split them.
% TRANSFER_FUNCTION: Moves money from general revenue to households as unconditional payments (net flows concentrated at the bottom once clawbacks operate, near-zero for middle earners); moves budget share and political attention from targeted programs toward the universal vehicle; moves coalition support, donations, and agenda influence to the entrepreneurs who carry the label.
% ABSENT_VOICES: Defenders of the targeted safety net and the recipients themselves are missing from the design rooms — bipartisan convenings, philanthropy-funded pilots, cross-party commissions — where replacement architectures are first sketched. They meet the proposals as accomplished facts and object afterward, from outside.
% DISAPPEARANCE_RATIONALE: If the vehicle vanished overnight, the cross-ideological coalition dissolves into design-specific camps, pilot programs wind down, targeted programs lose the crowding-out pressure currently justified by universality rhetoric, and cash-transfer politics re-polarizes along the flat-grant/tax-back line the ambiguity currently suspends.
% FOUNDING_PROBLEM: Income insecurity persisting despite a mature means-tested welfare state: take-up gaps, stigma, bureaucratic burden, and poverty traps that targeted programs demonstrably leave unsolved — the problem negative-income-tax and universal-basic-income proposals were built to address.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: official poverty and material-deprivation statistics, take-up research by non-advocacy academics, and recipient testimony collected by service providers — none of whom sit in the entrepreneur/designer beneficiary seats. No beneficiary-only attestation is relied on.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__universality_paradox_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is low-moderate (0.32 at interval end): the material transfers are modest and converge across designs, honoring the reading's low-epsilon expectation, but the vehicle does move real resources asymmetrically — coalition rents accrue to entrepreneurs at no material cost to them, and universality rhetoric has been used to justify consolidating targeted benefits whose recipients net-lose. Suppression (0.42) is structural rather than coercive: the constraint operates by constraining the policy-discourse space — keeping the design question open, framing targeted programs as redundant 'patchwork' — not by punishing exit. Theater ratio (0.52) is high because a large share of the vehicle's activity is performative coalition maintenance: summits, pilot announcements, futurist panels — activity whose product is the appearance of momentum. Accessibility collapse is low (0.28): understanding the constraint — seeing that the paths converge — dissolves the ambiguity's value and opens evaluation options rather than closing them. Resistance is substantial (0.55): both ideological flanks actively press for resolution, and safety-net defenders fight consolidation proposals. Measurements share one grid (t=0,2,4,6,8,10 over 2016-2026). The theater series oscillates: the mid-interval dip (0.55 to 0.41) tracks the period when cash transfers were actually enacted at scale and performance gave way to operation; the subsequent rise tracks the return to discourse as legislative momentum stalled. The oscillation is driven by an exogenous shock rather than manufactured intermittently by the constraint, though each hype cycle re-opens the ambiguity that entrepreneurs monetize — a weak intermittent-reinforcement flavor worth watching. Base metrics were measured at the interval end (late-cycle stall phase), when theater is elevated and enforcement effort (suppression_requirement 0.30 to 0.42) has grown to hold the coalition against resolution pressure from both flanks.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently. From the entrepreneur seat the vehicle is a coalition technology they built and profit from: the ambiguity is an asset, and resolution is a threat — their computed extraction is damped toward subsidy. From the recipient seat the same structure operates as a slow threat to benefits calibrated to needs a flat grant cannot meet: high directionality, trapped exit. Policy designers experience rhetorical flexibility as professional opportunity with negligible personal cost. Middle-income taxpayers sit near-symmetric — the check and the clawback roughly cancel — which is precisely why their tolerance is purchasable with presentation rather than money. The excluded defender seat sees the crowding-out dynamic before it appears in budgets. Same-level divergence: entrepreneurs and safety-net defenders hold comparable organizational power but opposite directionalities — the constraint-specific factor is that the vehicle's ambiguity taxes defenders' terrain while subsidizing entrepreneurs' coalition space. Coalition note: recipients are individually powerless, and their natural coalition vehicle (defender organizations) is structurally excluded from the design rooms, which suppresses the coalition channel the powerlessness would otherwise open.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation. political_entrepreneurs and policy_designers are declared beneficiaries with strong exits (arbitrage, mobile): low d, effective extraction damped toward subsidy — they collect coalition rents and rhetorical flexibility without bearing the vehicle's costs. targeted_program_recipients are declared victims with trapped exit: high d, amplified effective extraction — they bear consolidation losses and cannot leave the budget process. ideological_clarity is declared a victim but authored as a non-agent (agent: false), so it is excluded from derivation and feeds no d-to-chi computation; it marks a real epistemic cost the reading attributes to the arrangement without asserting an actor bears it. middle_income_taxpayers carry dual beneficiary/payer roles with constrained exit: near-symmetric d, small net position either way. No directionality overrides are authored: the structural derivation from these declarations matches the qualitative relationships, and overrides are reserved for cases the derivation gets wrong.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions. Against rope-mislabeling: the vehicle does coordinate genuinely (it assembles a cash-transfer coalition no single camp could build alone), but the same structure carries asymmetric extraction — entrepreneur rents, crowding-out pressure on targeted recipients — sustained by actively maintained ambiguity; that dual character is tangled_rope, not rope. Against snare-mislabeling: material extraction is low and converges across designs; there is no predatory core comparable to debt-trap or coerced-labor snares, and the chief gains are positional (coalition space) rather than pecuniary concentration. The receipt surface sharpens this: gains demonstrably accrue to political_entrepreneurs (captured seat) and fixing is prohibitive for the seat that could fix it — a snare-flavored receipt cell sitting under a tangled_rope structure. That divergence is diagnostic, not contradictory: the ambiguity's gains concentrate in entrepreneurial hands even though the extracted substance is thin, and forcing the snare label would overstate the material harm. Mandatrophy: the founding problem (income insecurity despite a mature targeted welfare state) remains live and independently corroborated, so the arrangement is not a mandate outliving its function; the live risk is the reverse — ambiguity collapsing into a resolved design would convert the vehicle into an ordinary program or kill it, ending the tangled configuration. Watch the ambiguity_durability_under_enactment omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the unconditional_income_support kernel; would instantiating a sibling reading (freedom_floor_reading or dependency_trap_reading) change the structural verdict?',
    'Author and compile the sibling files; compare victim sets, epsilon referents, and computed types across the family.',
    'The floor reading would relocate victims to labor-market coercers and assess epsilon on coercion removal; the trap reading would relocate victims to taxpayers and future workers and raise epsilon. This file''s tangled_rope verdict holds only for the ambiguity-vehicle referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the unconditional-income-support kernel.').

omega_variable(
    fiscal_convergence_robustness,
    'Is the fiscal convergence underlying this reading — net distributional outcomes similar across flat-grant and tax-back designs — robust at national scale, or does it break under behavioral response, take-up differentials, or political modification?',
    'National microsimulation with behavioral margins; comparison of enacted instances (resource dividends, child allowances, pandemic checks) against modeled convergence.',
    'If convergence breaks, the deferred design choice becomes materially consequential, the ambiguity stops being cheap, and epsilon rises as the mask conceals divergent outcomes rather than similar ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_convergence_robustness, empirical, 'Robustness of the convergence finding that neutralizes path choice.').

omega_variable(
    crowding_out_materiality,
    'Does universality rhetoric actually cause consolidation or cuts to targeted programs, or is crowding-out so far a rhetorical threat without budgetary realization?',
    'Budget trajectories in jurisdictions adopting universal components; difference-in-differences on targeted-line funding around universal-scheme adoption.',
    'If no material crowding-out, targeted_program_recipients drop from the victim set and the constraint drifts toward a purer coordination reading; if realized, the payer seat''s extraction is confirmed and likely understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crowding_out_materiality, empirical, 'Whether the victim declaration for targeted-program recipients is budgetarily real.').

omega_variable(
    ambiguity_durability_under_enactment,
    'Can the design ambiguity survive actual enactment, or does passing a specific scheme necessarily resolve the label and split the coalition?',
    'Observe coalition behavior after enacted instances: post-pandemic-check coalition cohesion, resource-dividend politics, any full-scale adoption.',
    'If enactment necessarily resolves the ambiguity, the constraint is inherently transitional — sunset-like dynamics attach and the tangled configuration is a phase, not a steady state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_durability_under_enactment, conceptual, 'Whether the load-bearing ambiguity is enactable or strictly propositional.').

omega_variable(
    clarity_cost_bearer_status,
    'Is degraded evaluation capacity a genuine structural cost attributable to identifiable bearers, or an analyst''s aesthetic complaint with no seat?',
    'Deliberative-quality indicators: survey knowledge of what proposed schemes would actually do to respondents, framing coherence across outlets, correction rates on design descriptions.',
    'If no identifiable bearer, the ideological_clarity victim entry should be retired and the victim set reduced to targeted_program_recipients; if measurable, the epistemic cost belongs in the structural account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(clarity_cost_bearer_status, conceptual, 'Standing of the non-agent victim entry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uips_universality_paradox_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(uips_universality_paradox_tr_t0, observed).
narrative_ontology:measurement(uips_universality_paradox_tr_t2, unconditional_income_support__universality_paradox_reading, theater_ratio, 2, 0.47).
narrative_ontology:measurement_basis(uips_universality_paradox_tr_t2, observed).
narrative_ontology:measurement(uips_universality_paradox_tr_t4, unconditional_income_support__universality_paradox_reading, theater_ratio, 4, 0.41).
narrative_ontology:measurement_basis(uips_universality_paradox_tr_t4, observed).
narrative_ontology:measurement(uips_universality_paradox_tr_t6, unconditional_income_support__universality_paradox_reading, theater_ratio, 6, 0.44).
narrative_ontology:measurement_basis(uips_universality_paradox_tr_t6, observed).
narrative_ontology:measurement(uips_universality_paradox_tr_t8, unconditional_income_support__universality_paradox_reading, theater_ratio, 8, 0.49).
narrative_ontology:measurement_basis(uips_universality_paradox_tr_t8, observed).
narrative_ontology:measurement(uips_universality_paradox_tr_t10, unconditional_income_support__universality_paradox_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement_basis(uips_universality_paradox_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(uips_universality_paradox_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(uips_universality_paradox_be_t0, observed).
narrative_ontology:measurement(uips_universality_paradox_be_t2, unconditional_income_support__universality_paradox_reading, base_extractiveness, 2, 0.24).
narrative_ontology:measurement_basis(uips_universality_paradox_be_t2, observed).
narrative_ontology:measurement(uips_universality_paradox_be_t4, unconditional_income_support__universality_paradox_reading, base_extractiveness, 4, 0.27).
narrative_ontology:measurement_basis(uips_universality_paradox_be_t4, observed).
narrative_ontology:measurement(uips_universality_paradox_be_t6, unconditional_income_support__universality_paradox_reading, base_extractiveness, 6, 0.29).
narrative_ontology:measurement_basis(uips_universality_paradox_be_t6, observed).
narrative_ontology:measurement(uips_universality_paradox_be_t8, unconditional_income_support__universality_paradox_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement_basis(uips_universality_paradox_be_t8, observed).
narrative_ontology:measurement(uips_universality_paradox_be_t10, unconditional_income_support__universality_paradox_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement_basis(uips_universality_paradox_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(uips_universality_paradox_su_t0, unconditional_income_support__universality_paradox_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(uips_universality_paradox_su_t0, observed).
narrative_ontology:measurement(uips_universality_paradox_su_t2, unconditional_income_support__universality_paradox_reading, suppression_requirement, 2, 0.33).
narrative_ontology:measurement_basis(uips_universality_paradox_su_t2, observed).
narrative_ontology:measurement(uips_universality_paradox_su_t4, unconditional_income_support__universality_paradox_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement_basis(uips_universality_paradox_su_t4, observed).
narrative_ontology:measurement(uips_universality_paradox_su_t6, unconditional_income_support__universality_paradox_reading, suppression_requirement, 6, 0.37).
narrative_ontology:measurement_basis(uips_universality_paradox_su_t6, observed).
narrative_ontology:measurement(uips_universality_paradox_su_t8, unconditional_income_support__universality_paradox_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement_basis(uips_universality_paradox_su_t8, observed).
narrative_ontology:measurement(uips_universality_paradox_su_t10, unconditional_income_support__universality_paradox_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(uips_universality_paradox_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, identity_coordination).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, targeted_safety_net_programs).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'unconditional income support' decomposes into three structurally distinct constraints per the epsilon-invariance principle — freedom_floor_reading (epsilon assessed on the coercion-removal referent), dependency_trap_reading (epsilon assessed on the incentive-distortion referent), and this universality_paradox_reading (epsilon assessed on the ambiguity-vehicle referent, low because fiscal outcomes converge across designs). Each has its own beneficiaries, victims, and type; members are linked via network.affects_constraints. Upstream/downstream: the two pole readings supply the normative commitments whose non-resolution constitutes this reading's constraint; this reading in turn bears on targeted_safety_net_programs, whose budget fate is the material surface where universality rhetoric does its crowding-out work.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
