% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Statutory Debt Ceiling as Weaponized Default-Threat Boundary (Extraction-Snare Reading)
 *   domain: constitutional/political-economy/fiscal-governance
 *
 * SUMMARY:
 *   Under this reading, the statutory debt ceiling operates as a weaponized
 *   boundary: because the limit sits outside the budget process and governs
 *   obligations already enacted, a legislative minority large enough to block
 *   an increase — one chamber's caucus, or a Senate minority paired with
 *   filibuster rules — can hold the full faith and credit of the United
 *   States hostage and extract policy concessions from the governing majority
 *   under threat of default. Costs land on bondholders (risk premia, the 2011
 *   and 2023 downgrades), program beneficiaries (payment-triage threats),
 *   agencies and contractors, and the majority's agenda; gains accrue to the
 *   wielding faction and the constituencies whose priorities ride on its
 *   demands. This file is ONE READING of the statutory_debt_ceiling kernel:
 *   the coordination_scaffold_reading and constitutional_nullity_reading are
 *   separate constraints (separate files) instantiated from the same
 *   statutory text, linked through network.affects_constraints. Per the
 *   epsilon-referent rule, epsilon here is authored for the standing
 *   arrangement — the ceiling as actually operated — assessed by this
 *   reading's own lights; it is not averaged across sibling readings and not
 *   authored for any endorsed alternative. KEY AGENTS (by structural
 *   relationship): - minority_faction_legislators: Primary beneficiary
 *   (organized/constrained) — wields the ceiling as leverage, collects
 *   concessions - concession_receiving_policy_interests: Secondary
 *   beneficiary (organized/mobile) — receives the policy terms attached to
 *   demands - governing_majority_legislators: Primary target
 *   (powerful/constrained) — bears concession costs and default-risk blame -
 *   presidential_administration: Target-negotiator
 *   (institutional/constrained) — concedes terms, manages the deadline -
 *   treasury_secretary_and_department: Operational target
 *   (institutional/constrained) — executes emergency measures under a frozen
 *   legal ceiling - treasury_bondholders: Financial target (organized/mobile)
 *   — absorbs risk premia and mark-to-market losses -
 *   federal_program_beneficiaries: Captive target (powerless/trapped) —
 *   payment delays fall on them - federal_agencies_and_contractors:
 *   Organizational target (organized/constrained) - foreign_treasury_holders:
 *   Excluded risk-bearer (organized/constrained) — no seat in the process -
 *   credit_rating_agencies: Analytical observer (institutional/analytical) —
 *   prices the standoff
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.8).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.68).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling as Weaponized Default-Threat Boundary (Extraction-Snare Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional/political-economy/fiscal-governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, '8cbc5f4e-fafd-4870-98f1-f05df544538a').
narrative_ontology:cs_kernel_codification('8cbc5f4e-fafd-4870-98f1-f05df544538a', formalized).
narrative_ontology:cs_authority_grounding('8cbc5f4e-fafd-4870-98f1-f05df544538a', lineage).
narrative_ontology:cs_interpretation_layer_present('8cbc5f4e-fafd-4870-98f1-f05df544538a').
narrative_ontology:cs_reading_relation('8cbc5f4e-fafd-4870-98f1-f05df544538a', statutory_debt_ceiling__coordination_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('8cbc5f4e-fafd-4870-98f1-f05df544538a', statutory_debt_ceiling__constitutional_nullity_reading, coexists_with).
narrative_ontology:cs_axiom('8cbc5f4e-fafd-4870-98f1-f05df544538a', foundational, sovereign_default_threat_is_coercive_leverage).
narrative_ontology:cs_axiom_status(sovereign_default_threat_is_coercive_leverage, holdable).
narrative_ontology:cs_axiom_grounding('8cbc5f4e-fafd-4870-98f1-f05df544538a', sovereign_default_threat_is_coercive_leverage, empirically_contingent).
narrative_ontology:cs_axiom('8cbc5f4e-fafd-4870-98f1-f05df544538a', secondary, minority_seat_share_understates_veto_power).
narrative_ontology:cs_axiom_status(minority_seat_share_understates_veto_power, holdable).
narrative_ontology:cs_axiom_grounding('8cbc5f4e-fafd-4870-98f1-f05df544538a', minority_seat_share_understates_veto_power, empirically_contingent).
narrative_ontology:cs_reference_frame('8cbc5f4e-fafd-4870-98f1-f05df544538a', standalone_statutory_debt_limit).
narrative_ontology:cs_drift_state('8cbc5f4e-fafd-4870-98f1-f05df544538a', contemporary_brinkmanship_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8cbc5f4e-fafd-4870-98f1-f05df544538a', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, minority_faction_legislators).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, concession_receiving_policy_interests).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, governing_majority_legislators).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, presidential_administration).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_secretary_and_department).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_bondholders).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_program_beneficiaries).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_agencies_and_contractors).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__extraction_snare_reading, minority_veto_point_leverage_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A disciplined bloc within one chamber, large enough under procedural rules to block passage of a debt-limit increase but far short of a majority. They announce refusal to raise the limit unless specified policy changes are enacted, set deadlines, and negotiate terms as the Treasury's cash date approaches. When settlements close, the policy changes they demanded are enacted. Their exit from the tactic runs through primary electorates and coalition allies who reward the posture; abandoning it carries electoral cost.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, minority_faction_legislators, beneficiary,
    organized, biographical, constrained, national).

% Organized constituencies whose priorities are attached to debt-limit demands: spending caps shaped to their preference, program carve-outs, policy riders. They lobby for the tactic's use and receive enacted concessions when settlements close. If the tactic stopped yielding, they could redirect lobbying effort to ordinary appropriations channels.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, concession_receiving_policy_interests, beneficiary,
    organized, biographical, mobile, national).

% Hold chamber control and responsibility for seeing that enacted obligations are honored. Each standoff forces a choice between conceding policy terms they opposed and absorbing default risk attributable to their stewardship. Their own voters punish both capitulation and chaos, narrowing maneuver room. They could schedule a clean ceiling-only vote but lack reliable unity to pass one.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, governing_majority_legislators, payer,
    powerful, biographical, constrained, national).

% Negotiates the settlement, sequences emergency cash measures, and decides whether to test legally contested workarounds. Concedes policy terms under deadline pressure while managing market communication. Its alternatives — invoking the Fourteenth Amendment, issuing premium bonds, minting high-value coins — carry legal uncertainty and political cost, so it treats them as last resorts rather than plans of record.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, presidential_administration, payer,
    institutional, biographical, constrained, national).

% Must keep paying obligations under a frozen legal ceiling: suspends investments in government accounts, shifts accounting between funds, forecasts the exact date cash runs out, and prepares contingency plans for prioritizing which payments to make. Bears operational strain and personal legal exposure each episode. Statute bars issuing debt past the limit, and the legally contested alternatives remain untested.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_secretary_and_department, payer,
    institutional, biographical, constrained, national).

% Hold the instruments whose timely payment the ceiling threatens. During standoffs they absorb price declines and rising insurance costs on default risk; after the 2011 episode the sovereign rating fell and yields carried a persistent risk premium. They can rebalance toward other assets, but the depth and liquidity of the Treasury market make wholesale exit slow and costly.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_bondholders, payer,
    organized, biographical, mobile, global).

% Retirees, veterans, and households relying on scheduled federal payments. Standoffs threaten delayed checks and disrupted services, and leaked contingency planning contemplates choosing which obligations to miss. They depend on the payments and cannot arrange substitutes.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_program_beneficiaries, payer,
    powerless, immediate, trapped, national).

% Departments and vendors facing payment interruptions, cash-management distortions, and furlough-adjacent planning during standoffs. Operations continue on paper but disbursements stall; contracts and grants hang on the settlement calendar.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_agencies_and_contractors, payer,
    organized, biographical, constrained, national).

% Foreign governments and international investors holding large Treasury positions. They absorb the risk premium and volatility of each episode but have no vote, no standing in the negotiation, and no channel into the congressional process beyond market behavior.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, foreign_treasury_holders, excluded,
    organized, generational, constrained, global).

% Assess sovereign creditworthiness and act on it — the 2011 and 2023 downgrades followed standoffs. Their judgments feed back into borrowing costs and into each side's negotiating calculus. They analyze from outside the legislative process.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, credit_rating_agencies, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__extraction_snare_reading, minority_faction_legislators).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__extraction_snare_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The statute provides a single recurring vote at which Congress formally reauthorizes the accumulated debt issued under previously enacted spending and tax law, replacing per-issue approval of each borrowing with one aggregate checkpoint. That delegation-plus-checkpoint structure is the coordination service the arrangement performs.
% TRANSFER_FUNCTION: Moves policy concessions from the governing majority and the sitting administration to the faction that blocks the increase and to the constituencies behind its demands; moves default risk and its costs — rate premiums, market losses, payment-delay exposure — onto bondholders, program beneficiaries, agencies, contractors, and the broader economy.
% ABSENT_VOICES: Foreign holders of Treasury debt absorb each episode's risk premium with no vote and no seat; future taxpayers inherit the elevated borrowing costs; program beneficiaries learn of payment-triage contingencies only after plans leak. None is represented at the table where the terms are set.
% DISAPPEARANCE_RATIONALE: If the ceiling vanished overnight, Treasury would roll maturing debt automatically as finance ministries do in most advanced democracies; the recurring negotiation calendar, the emergency-measures choreography, and the concession stream would disappear, and borrowing decisions would return to the ordinary appropriations process. Every named seat's arrangements change: the faction loses its leverage instrument, the administration loses the crisis cycle, and the payee seats lose the risk overlay.
% FOUNDING_PROBLEM: The 1917 Second Liberty Bond Act was written to stop Congress from approving each individual bond issue during wartime financing: it delegated issuance authority to Treasury within a statutory aggregate, trading per-issue micromanagement for a single limit. The 1939 revision consolidated the separate limits into one ceiling for the same administrative reason.
% FOUNDING_PROBLEM_CORROBORATION: Fiscal historians and the Congressional Research Service's institutional histories attest the per-issue-approval streamlining purpose, and Treasury's archival records of the Liberty Bond Acts corroborate it — sources outside the modern arrangement's benefiting parties. Contemporary defenders cite fiscal discipline and deliberation instead, purposes the 1917 statute's text and history do not contain; no beneficiary attests to the founding problem itself.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__extraction_snare_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.80) because the ceiling's operative output under divided government is a concession stream decoupled from any fiscal function: the limit does not restrain spending (obligations are already enacted) — it prices the threat to honor them. Suppression (0.68) reflects enforced closure of alternatives: clean ceiling-only votes fail, Fourteenth Amendment routes are deterred by legal uncertainty, and each settled episode reaffirms the tactic. Theater ratio (0.32) is moderate: emergency-measure choreography and deadline ritual grow more performative, but the mechanism remains functionally extractive rather than inertial — well below the piton band. Accessibility collapse (0.58): alternatives exist on paper (abolition by simple statute, or the Danish practice of treating new debt authorization as implicit in the budget) but collapse in practice once the weapon's deterrent shadow falls on anyone who might abolish it. Resistance (0.62): majorities have attempted clean votes and suspensions, administrations have floated constitutional workarounds, and markets punish brinkmanship — real resistance that has so far failed to dislodge the structure. The three measurement series share one time grid (1917–2026, ten points each). The trajectory is a ratchet with partial post-crisis decay (1995→2002 and 2011→2019 dips) rather than smooth drift: each spike partially recedes but resets at a higher floor, and the oscillation itself is part of the mechanism — intermittent reinforcement, where each resolved crisis rewards the tactic and lowers the threshold for its next use. Coordination type is declared as enforcement_mechanism (floor 0.10): the checkpoint function is real but small relative to measured extraction, which sits far above the inherent-coordination-cost band.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the minority-faction seat the ceiling is a leverage instrument that converts procedural position into policy wins without winning elections — a workable tool from inside. From the governing-majority and administration seats the same structure is a gun to the head: concede or own the default. From the bondholder seat it is a recurring volatility tax; from the program-beneficiary seat, a threat to subsistence payments issued by their own government. The engine computes these per-seat classifications from the structural data; the divergence between the beneficiary seat's tool-like experience and the payer seats' coerced experience is the perspectival signature that separates a snare from a shared coordination burden.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (minority_faction_legislators, concession_receiving_policy_interests) drive low directionality for those seats — the arrangement subsidizes them. Victim declarations drive high directionality, amplified where exit is worst: program beneficiaries are trapped (nearest the full-target end), the majority, administration, Treasury, and agencies are constrained, and bondholders are mobile-but-costly (damped by exit). Scope is national for domestic seats and global for bondholders and foreign holders; larger scope scales effective extraction upward for targets. No directionality overrides were needed: declared roles plus exit options reproduce the true structural relationships, so the derivation chain stands unmodified.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — replacing per-issue bond approval with a single administrative checkpoint — died decades ago, yet the arrangement persists and its extraction has risen. The natural mislabel is piton: an old form kept alive past its function. But the theater ratio (0.32) is well below piton range, and the gains demonstrably accrue to a named seat — the minority faction captures the concessions — so the structure is maintained because someone profits, not from inertia alone; that is capture, not atrophy. The snare classification prevents the opposite error as well: reading the arrangement as pure coordination (the scaffold sibling) would erase the asymmetric transfer that is its operative output under divided government. Mandatrophy here resolves as repurposing: the mandate is dead, the mechanism was converted into a leverage instrument, and the conversion pays its operators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the extraction_snare_reading of the statutory_debt_ceiling kernel; would the coordination_scaffold_reading or constitutional_nullity_reading of the same statutory text classify differently, and where exactly does the disagreement sit?',
    'Compare the sibling stories'' epsilon and beneficiary/victim structures against this one: the scaffold reading authors the same text with epsilon near the coordination floor and no hostage beneficiaries; the nullity reading relocates validity to the Fourteenth Amendment and dissolves the statutory constraint. The disagreement is located in the operative function attributed to the identical statutory language.',
    'If the scaffold reading is adopted, effective extraction collapses toward coordination cost and the type moves toward rope/scaffold; if the nullity reading prevails, the constraint migrates from statute to constitutional conflict and may cease to bind at all. This file''s high-extraction verdict holds only within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of the statutory_debt_ceiling kernel; sibling readings instantiate different constraints from the same text.').

omega_variable(
    design_deviation_vs_continuity,
    'Is the ceiling''s weaponized operation a deviation from its administrative design or continuous with what the 1917/1939 statutes always enabled?',
    'Historical analysis of pre-1995 ceiling episodes — routine bipartisan raises with no concession extraction — versus the post-1995 pattern of demand-backed vetoes, correlated with unified versus divided government.',
    'If deviation, ending the weaponized usage restores a low-extraction checkpoint (scaffold territory); if continuous, the kernel itself is extractive and the sibling scaffold reading is a cover-story artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(design_deviation_vs_continuity, empirical, 'Whether extraction is a design feature or an emergent exploit of the statutory form.').

omega_variable(
    executive_counter_leverage_sufficiency,
    'Do the executive''s untested alternatives — Fourteenth Amendment invocation, premium bonds, high-value coin — constitute real exit that would deflate the hostage mechanism if ever used?',
    'Observe a standoff in which the administration refuses concessions and executes an alternative; litigation outcomes and market reaction would establish whether the alternative is viable.',
    'If alternatives are executable, the minority''s threat loses credibility and effective extraction falls sharply toward a tangled-rope residual; if they are legally or financially unusable, the target side sits nearer trapped and the snare reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_counter_leverage_sufficiency, empirical, 'Whether target-side exit options are real or merely theatrically available.').

omega_variable(
    payee_coalition_formation_failure,
    'The payees — bondholders, program beneficiaries, agencies, the governing majority — jointly command overwhelming resources, and removal is mechanically a one-vote statutory repeal; why has no durable coalition formed to remove the constraint?',
    'Collective-action analysis of abolition attempts: sponsorship patterns, concentrated-versus-diffuse cost asymmetry, and whether each prospective fixer privately values retaining the weapon for its own future minority periods.',
    'If each potential fixer rationally hoards the weapon for future use, the constraint is self-perpetuating regardless of aggregate harm and the snare reading is stable; if coordination failure is incidental, a single committed majority could abolish it at low cost and the persistence story collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(payee_coalition_formation_failure, empirical, 'Why a cheaply removable constraint with diffuse victims persists — coalition failure or mutual deterrence among would-be fixers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 1917, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sdc_esr_tr_t1917, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1917, 0.04).
narrative_ontology:measurement_basis(sdc_esr_tr_t1917, observed).
narrative_ontology:measurement(sdc_esr_tr_t1960, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement_basis(sdc_esr_tr_t1960, observed).
narrative_ontology:measurement(sdc_esr_tr_t1990, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1990, 0.09).
narrative_ontology:measurement_basis(sdc_esr_tr_t1990, observed).
narrative_ontology:measurement(sdc_esr_tr_t1995, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement_basis(sdc_esr_tr_t1995, observed).
narrative_ontology:measurement(sdc_esr_tr_t2002, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2002, 0.14).
narrative_ontology:measurement_basis(sdc_esr_tr_t2002, observed).
narrative_ontology:measurement(sdc_esr_tr_t2011, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2011, 0.24).
narrative_ontology:measurement_basis(sdc_esr_tr_t2011, observed).
narrative_ontology:measurement(sdc_esr_tr_t2013, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2013, 0.27).
narrative_ontology:measurement_basis(sdc_esr_tr_t2013, observed).
narrative_ontology:measurement(sdc_esr_tr_t2019, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2019, 0.22).
narrative_ontology:measurement_basis(sdc_esr_tr_t2019, observed).
narrative_ontology:measurement(sdc_esr_tr_t2023, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2023, 0.3).
narrative_ontology:measurement_basis(sdc_esr_tr_t2023, observed).
narrative_ontology:measurement(sdc_esr_tr_t2026, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2026, 0.32).
narrative_ontology:measurement_basis(sdc_esr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(sdc_esr_be_t1917, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1917, 0.05).
narrative_ontology:measurement_basis(sdc_esr_be_t1917, observed).
narrative_ontology:measurement(sdc_esr_be_t1960, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1960, 0.07).
narrative_ontology:measurement_basis(sdc_esr_be_t1960, observed).
narrative_ontology:measurement(sdc_esr_be_t1990, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1990, 0.16).
narrative_ontology:measurement_basis(sdc_esr_be_t1990, observed).
narrative_ontology:measurement(sdc_esr_be_t1995, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1995, 0.46).
narrative_ontology:measurement_basis(sdc_esr_be_t1995, observed).
narrative_ontology:measurement(sdc_esr_be_t2002, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2002, 0.34).
narrative_ontology:measurement_basis(sdc_esr_be_t2002, observed).
narrative_ontology:measurement(sdc_esr_be_t2011, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2011, 0.71).
narrative_ontology:measurement_basis(sdc_esr_be_t2011, observed).
narrative_ontology:measurement(sdc_esr_be_t2013, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2013, 0.74).
narrative_ontology:measurement_basis(sdc_esr_be_t2013, observed).
narrative_ontology:measurement(sdc_esr_be_t2019, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2019, 0.55).
narrative_ontology:measurement_basis(sdc_esr_be_t2019, observed).
narrative_ontology:measurement(sdc_esr_be_t2023, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2023, 0.77).
narrative_ontology:measurement_basis(sdc_esr_be_t2023, observed).
narrative_ontology:measurement(sdc_esr_be_t2026, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2026, 0.8).
narrative_ontology:measurement_basis(sdc_esr_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(sdc_esr_su_t1917, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1917, 0.05).
narrative_ontology:measurement_basis(sdc_esr_su_t1917, observed).
narrative_ontology:measurement(sdc_esr_su_t1960, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1960, 0.06).
narrative_ontology:measurement_basis(sdc_esr_su_t1960, observed).
narrative_ontology:measurement(sdc_esr_su_t1990, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1990, 0.11).
narrative_ontology:measurement_basis(sdc_esr_su_t1990, observed).
narrative_ontology:measurement(sdc_esr_su_t1995, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement_basis(sdc_esr_su_t1995, observed).
narrative_ontology:measurement(sdc_esr_su_t2002, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2002, 0.3).
narrative_ontology:measurement_basis(sdc_esr_su_t2002, observed).
narrative_ontology:measurement(sdc_esr_su_t2011, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2011, 0.58).
narrative_ontology:measurement_basis(sdc_esr_su_t2011, observed).
narrative_ontology:measurement(sdc_esr_su_t2013, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2013, 0.61).
narrative_ontology:measurement_basis(sdc_esr_su_t2013, observed).
narrative_ontology:measurement(sdc_esr_su_t2019, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2019, 0.5).
narrative_ontology:measurement_basis(sdc_esr_su_t2019, observed).
narrative_ontology:measurement(sdc_esr_su_t2023, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2023, 0.65).
narrative_ontology:measurement_basis(sdc_esr_su_t2023, observed).
narrative_ontology:measurement(sdc_esr_su_t2026, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2026, 0.68).
narrative_ontology:measurement_basis(sdc_esr_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the debt ceiling' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints instantiated from the same statutory text — a coordination checkpoint (coordination_scaffold_reading, low epsilon), a constitutionally suspect formality (constitutional_nullity_reading, validity contest), and a weaponized extraction mechanism (this file, high epsilon). This file is the high-epsilon member. Weaponized episodes erode the scaffold reading's factual predicate — each crisis disrupts the very Treasury operations the checkpoint allegedly facilitates — and supply the motivating cases for the nullity debate without settling it; both sibling relations are documented in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
