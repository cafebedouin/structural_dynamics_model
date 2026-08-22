% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__coordination_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__coordination_scaffold_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: statutory_debt_ceiling__coordination_scaffold_reading
 *   human_readable: Statutory Debt Ceiling as Procedural Coordination Scaffold (Coordination-Scaffold Reading)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   statutory_debt_ceiling: the coordination-scaffold reading, under which
 *   the aggregate statutory debt limit is a procedural coordination device —
 *   Congress sets one borrowing boundary by ordinary legislation and the
 *   Treasury executes all debt management inside it, sparing both branches
 *   the per-issuance micromanagement that governed federal finance before
 *   1917. On this reading the mechanism's adjustments are meant to be
 *   routine, its binding force is self-executing rather than
 *   apparatus-driven, and no group is systematically targeted by its
 *   operation; its costs are friction (adjustment-delay risk,
 *   extraordinary-measures overhead, episodic uncertainty premia) rather than
 *   designed transfer. CONSTRAINT FAMILY NOTE: the colloquial label 'the debt
 *   ceiling' decomposes, per the epsilon-invariance principle, into three
 *   linked stories. This reading authors low epsilon (0.22) because it
 *   assesses the standing arrangement as functioning delegation; the sibling
 *   extraction_snare_reading authors high epsilon over the same referent,
 *   reading the identical statute as a weaponized boundary enabling minority
 *   extraction under default threat; the sibling
 *   constitutional_nullity_reading authors the arrangement as
 *   constitutionally void under Fourteenth Amendment Section 4, i.e., as no
 *   valid constraint at all. The three files share the kernel and differ in
 *   epsilon, beneficiary/victim structure, and type; they are linked through
 *   network.affects_constraints rather than merged. Claim/metric independence
 *   is preserved: the scaffold claim and the low-but-nonzero metrics are each
 *   authored from their own evidence, and any computed divergence belongs to
 *   the engine.
 *
 * KEY AGENTS:
 *   - - congressional_fiscal_authorizers: agenda setter (institutional/mobile) — writes and adjusts the aggregate borrowing boundary by ordinary legislation; holds the repeal option at all times
 *   - - us_treasury: primary beneficiary with secondary payer position (institutional/trapped) — collects operational autonomy, bears execution risk and extraordinary-measures burden inside the limit
 *   - - bond_market_participants: beneficiary (organized/arbitrage) — receives predictable centralized issuance; prices episodic deadline risk and can reallocate globally
 *   - - broad_taxpayers: diffuse beneficiary (powerless/trapped) — gains financing efficiency, bears the debt service the boundary authorizes
 *   - - payment_dependent_households: excluded voice (powerless/trapped) — depend on uninterrupted federal payments, hold no seat in adjustment negotiations
 *   - - gao_fiscal_analysts: analytical observer (institutional/analytical) — audits the mechanism and recommends replacement designs; holds no vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__coordination_scaffold_reading, 0.22).
domain_priors:suppression_score(statutory_debt_ceiling__coordination_scaffold_reading, 0.2).
domain_priors:theater_ratio(statutory_debt_ceiling__coordination_scaffold_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__coordination_scaffold_reading, scaffold).
narrative_ontology:human_readable(statutory_debt_ceiling__coordination_scaffold_reading, "Statutory Debt Ceiling as Procedural Coordination Scaffold (Coordination-Scaffold Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__coordination_scaffold_reading, "constitutional_law/political_economy/fiscal_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__coordination_scaffold_reading, 'b6637b91-3171-4278-ad55-ce0081b11932').
narrative_ontology:cs_kernel_codification('b6637b91-3171-4278-ad55-ce0081b11932', fixed_text).
narrative_ontology:cs_authority_grounding('b6637b91-3171-4278-ad55-ce0081b11932', lineage).
narrative_ontology:cs_interpretation_layer_present('b6637b91-3171-4278-ad55-ce0081b11932').
narrative_ontology:cs_reading_relation('b6637b91-3171-4278-ad55-ce0081b11932', statutory_debt_ceiling__extraction_snare_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6637b91-3171-4278-ad55-ce0081b11932', statutory_debt_ceiling__constitutional_nullity_reading, forecloses).
narrative_ontology:cs_axiom('b6637b91-3171-4278-ad55-ce0081b11932', foundational, aggregate_limit_validly_channels_borrowing_power).
narrative_ontology:cs_axiom_status(aggregate_limit_validly_channels_borrowing_power, holdable).
narrative_ontology:cs_axiom_grounding('b6637b91-3171-4278-ad55-ce0081b11932', aggregate_limit_validly_channels_borrowing_power, conventional).
narrative_ontology:cs_axiom('b6637b91-3171-4278-ad55-ce0081b11932', foundational, treasury_executes_autonomously_within_legislated_aggregate).
narrative_ontology:cs_axiom_status(treasury_executes_autonomously_within_legislated_aggregate, holdable).
narrative_ontology:cs_axiom_grounding('b6637b91-3171-4278-ad55-ce0081b11932', treasury_executes_autonomously_within_legislated_aggregate, conventional).
narrative_ontology:cs_axiom('b6637b91-3171-4278-ad55-ce0081b11932', secondary, periodic_adjustment_preserves_accountability).
narrative_ontology:cs_axiom_status(periodic_adjustment_preserves_accountability, holdable).
narrative_ontology:cs_axiom_grounding('b6637b91-3171-4278-ad55-ce0081b11932', periodic_adjustment_preserves_accountability, instrumental).
narrative_ontology:cs_reference_frame('b6637b91-3171-4278-ad55-ce0081b11932', aggregate_delegation_routine_adjustment).
narrative_ontology:cs_drift_state('b6637b91-3171-4278-ad55-ce0081b11932', contemporary_post_2011_brinkmanship_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b6637b91-3171-4278-ad55-ce0081b11932', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, us_treasury).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, congressional_fiscal_authorizers).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, bond_market_participants).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, broad_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__coordination_scaffold_reading, us_treasury).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% House Ways and Means, Senate Finance, and party leadership write and adjust the aggregate borrowing limit by ordinary legislation. The limit gives them a single recurring decision point over total federal debt in place of approval of each issuance. Recent adjustments have sometimes required extended negotiation before enactment; the authority to raise, lower, suspend, or replace the limit remains wholly theirs at all times.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, congressional_fiscal_authorizers, agenda_setter,
    institutional, biographical, mobile, national).

% Runs federal borrowing day to day: auction scheduling, instrument mix, maturity structure, and cash management, all inside the legislated aggregate. When outstanding principal approaches the limit it deploys accounting measures — suspending certain investments, declaring debt issuance suspension periods — to keep meeting obligations while awaiting adjustment. It cannot borrow past the limit and cannot decline its payment obligations; its room for maneuver lives entirely inside the boundary Congress sets.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, us_treasury, beneficiary,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__coordination_scaffold_reading, us_treasury, payer).

% Buy and hold Treasury securities in the deepest sovereign debt market in the world. A single aggregate authorization gives them predictable, centralized issuance and standardized instruments; episodic adjustment standoffs add tail risk they price through bill yields and insurance spreads around anticipated deadlines. They can reallocate portfolios globally at low cost.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, bond_market_participants, beneficiary,
    organized, biographical, arbitrage, global).

% Carry the interest service on the debt the limit authorizes and gain from the lower transaction costs of streamlined, centralized financing. They hold no direct seat in adjustment negotiations; their exposure arrives through appropriations and debt-service lines.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, broad_taxpayers, beneficiary,
    powerless, generational, trapped, national).

% Count on the uninterrupted flow of federal payments — Social Security, military pay, vendor invoices, tax refunds. During adjustment standoffs they carry the risk of delayed or missed payments, though they take no part in the negotiations and have no organized voice in them.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, payment_dependent_households, excluded,
    powerless, immediate, trapped, national).

% Audit the borrowing-limit mechanism and have repeatedly recommended replacing the aggregate limit with a prior-approval framework under which debt increases are voted alongside the budget decisions that necessitate them. They publish and testify but hold no vote; their designs await legislative adoption.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, gao_fiscal_analysts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__coordination_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__coordination_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the financing collective-action problem: one legislated aggregate boundary replaces hundreds of per-issuance acts, letting Treasury time auctions, choose instruments, and manage maturities continuously while Congress retains a single recurring control point over total debt.
% TRANSFER_FUNCTION: In steady state it moves decision rights, not value: day-to-day borrowing discretion passes from Congress to Treasury inside a boundary Congress reserves to itself. Around adjustment votes, small amounts of bargaining leverage and agenda time change hands temporarily; this reading treats that episodic churn as friction cost, not as the arrangement's designed transfer.
% ABSENT_VOICES: Payment-dependent households would object — they carry the tail risk of interrupted federal payments during adjustment standoffs and have no organizational seat. Future taxpayers bear the service on what the boundary authorizes and appear only through advocacy proxies. GAO and CBO analysts object from inside the room (testimony, published replacement designs) but hold no vote; rating agencies signal externally without participating.
% DISAPPEARANCE_RATIONALE: Treasury's borrowing authority is legally defined relative to the limit; overnight removal would force immediate re-legislation of borrowing authority (unlimited authority or a new mechanism), repricing across the bill market as default-risk expectations reset, and loss of the single recurring control point the appropriating committees plan around. The world rearranges because the arrangement is load-bearing infrastructure, not decoration.
% FOUNDING_PROBLEM: Before 1917, Congress approved each federal bond issue individually — hundreds of separate statutes during wartime financing, each with its own terms and floor debate. The aggregate ceiling was built to authorize borrowing in bulk: Congress sets the total, Treasury manages the structure.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: GAO reports (2011, 2015) attest both that the pre-1917 per-issue regime was unworkable and that the aggregate limit has outlived its original justification, recommending replacement by a prior-approval framework; fiscal historians of Treasury debt management document the micromanagement burden the 1917 reform removed. No participant in current debates proposes restoring per-issue approval.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__coordination_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__coordination_scaffold_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statutory_debt_ceiling__coordination_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).
:- end_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22 at interval end) because this reading finds no designed transfer: the boundary allocates decision rights rather than moving value, and its measured costs are friction — delay risk around adjustment deadlines, the administrative overhead of extraordinary measures, small uncertainty premia in bill yields. Suppression is low (0.20) and deliberately carried as a scalar only: the enforcement picture is static across the whole interval (the limit binds by operation of law; no enforcement apparatus grows or decays), so no suppression_requirement series is authored. Theater is low with a visible bump at 2011 (performative votes and deadline choreography) that partially recedes afterward. Accessibility collapse is low-moderate (0.30): replacement designs (prior-approval frameworks, Gephardt-rule restoration, outright repeal) remain live and legislatable, so understanding the mechanism does not close the option set. Resistance is moderate (0.35): episodic political friction at adjustment votes plus a durable cross-partisan intellectual current (GAO, CBO-adjacent economists) pressing replacement. All series run on one shared eight-point grid (1917, 1939, 1953, 1967, 1979, 1995, 2011, 2025) with every tracked metric authored at every point. Receipt surface: each named seat was checked for capture of the arrangement's gains — authorizers collect control, Treasury collects autonomy, markets collect predictability, taxpayers collect financing efficiency; the friction component dissipates as market noise and evaporating leverage, so gain_flow is authored 'diffuse' as an affirmative finding, not a default. fixing_cost is authored 'cheap': complete replacement designs exist off the shelf and enactment requires only ordinary legislation; the binding obstacle is blame allocation, not institutional capacity.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently and the engine computes that divergence from the structural data. From the authorizing committees' seat the arrangement is a retained control instrument — a single recurring decision point they can rewrite at will (mobile exit, beneficiary-side directionality). From Treasury's seat it is an operating envelope: wide autonomy inside the line, hard truncation at it, with the secondary payer position surfacing as execution risk and extraordinary-measures burden whenever the line nears. From the bond market's seat it is market infrastructure whose occasional deadline drama is a priced nuisance. From payment-dependent households' seat it is an unconsented tail risk attached to checks they depend on — they sit outside the conversation entirely (excluded, not payer: this reading finds no systematic victim class, and their episodic exposure is friction the arrangement imposes without their participation). The GAO seat sees an anachronism awaiting replacement. Same-level divergence is visible between the two institutional seats: authorizers and Treasury hold comparable institutional power, but the authorizers' ability to rewrite the rule and Treasury's inability to decline execution give them opposite effective exit positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for all four named beneficiary groups: authorizers (near the beneficiary end — they set and can dissolve the boundary), Treasury (low with upward pressure from its secondary payer position — autonomy received, execution risk borne), bond markets (nearest the beneficiary end — arbitrage-grade exit plus pure coordination gain), and taxpayers (moderately low — real efficiency benefit, partly offset by the service burden on what the boundary authorizes). No victims are declared because this reading identifies no group systematically borne-upon by the arrangement's design; the excluded households feed the absence analysis (absent_voices, Q4) rather than the directionality computation, consistent with the R3 ruling that authored absences are commentary-grade. No directionality overrides are used: the derivation chain from roles, power, and exit options reproduces the relationships described here without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the unworkable burden of congressional approval for each individual debt issue — is dead, and it was killed by the mechanism itself: the aggregate limit solved per-issuance micromanagement permanently, and no serious proposal revisits it. Yet the arrangement persists and remains load-bearing: Treasury's statutory borrowing authority is defined relative to the limit, and its removal would force immediate re-legislation of borrowing authority. This is the classic scaffold posture — support erected for a construction phase, retained after the structure stands — and it is why the reading expects the R5 mismatch (dead founding problem crossed with world-rearranging dependence) to fire a zombie/transition flag. On this reading that flag is not a refutation but the transition signal scaffolds exist to emit: the GAO prior-approval design is the intended replacement structure, and the mechanism's justification now rests on the transition to it, not on the founding problem. Mandatrophy is therefore declared resolved in base_properties, matching the dead founding-problem status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexical_status,
    'This constraint is one reading (coordination_scaffold_reading) of the kernel statutory_debt_ceiling; which reading a story instantiates determines its entire beneficiary/victim structure and epsilon — what exactly do the sibling readings change?',
    'Cross-reading comparison of the three linked family files: extraction_snare_reading declares default-threatened program beneficiaries as victims and posits a capturing legislative minority; constitutional_nullity_reading removes the arrangement''s validity altogether. Reading selection is indexed by the committer frame, not recovered from the statute''s text.',
    'If the snare reading is adopted, this file''s low epsilon and empty victim set are replaced by high epsilon with declared victims and active enforcement; if the nullity reading is adopted, the constraint ceases to bind at all and the classification question becomes one about a void instrument.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexical_status, conceptual, 'Committer-frame provenance: one of three readings of the debt-ceiling kernel; sibling readings alter victim sets, epsilon, and validity.').

omega_variable(
    disagreement_axis_function_vs_validity,
    'Is the inter-reading disagreement located on the mechanism''s function (coordination versus extraction, an empirical dispute compatible with agreed validity) or on its constitutional validity (binding law versus Section 4 nullity, a legal dispute that dissolves the mechanism)?',
    'Doctrinal analysis separating the two axes: function disputes are resolvable by observing adjustment behavior under counterfactual institutional designs; validity disputes turn on Fourteenth Amendment Section 4 jurisprudence and are resolvable only by adjudication or constitutional clarification.',
    'Function-axis disagreement leaves this reading and the snare sibling coexisting as live assessments of one valid instrument; validity-axis disagreement activates the foreclosure edge this file declares toward the nullity sibling, since a validly-operative coordination account cannot coexist with a voidness account in one legal framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_axis_function_vs_validity, conceptual, 'Locates the kernel contest: function axis (empirical) versus validity axis (legal).').

omega_variable(
    brinkmanship_aberration_or_structure,
    'Are the 1995-2023 adjustment standoffs aberrant politics incident to the mechanism, or do they reveal a hostage-bearing structure inherent in any aggregate boundary requiring discrete renewal?',
    'Comparative institutional evidence: peer systems that abolished ceiling analogues (Denmark) or never had them (the United Kingdom''s supply-and-approve cycle) — if they exhibit neither per-issuance micromanagement nor recurrent brinkmanship, the standoffs are contingent politics; if boundary-renewal designs across systems reliably produce deadline hostage dynamics, the structure invites them.',
    'An aberration finding stabilizes this reading as the mechanism''s true character; a structural finding degrades this reading toward the extraction_snare_reading sibling and would justify re-authoring epsilon upward with declared victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brinkmanship_aberration_or_structure, empirical, 'Whether observed brinkmanship is exogenous pathology or endogenous to boundary-renewal design.').

omega_variable(
    extraordinary_measures_interpretive_drift,
    'Do Treasury''s extraordinary-measures interpretations (debt issuance suspension periods, investment suspensions) constitute legitimate interpretation of the statutory authorities, or de facto amendment that lets the interpretive layer absorb what should surface as legislative revision?',
    'Legal-historical audit of Treasury legal opinions and GAO review of extraordinary-measures usage: compare the statutory text''s grant of authority against the practices built on it, and test whether Congress ratifies or merely tolerates the interpretations.',
    'A legitimate-interpretation finding supports the reading''s healthy interpretive layer; a de-facto-amendment finding indicates codification strain beneath the scaffold and would push the drift assessment from practice_drift toward codification_collapse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraordinary_measures_interpretive_drift, empirical, 'Status of the interpretive layer that absorbs deadline pressure without surfaced revision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__coordination_scaffold_reading, 1917, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1917, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1917, 0.05).
narrative_ontology:measurement(stat_tr_t1939, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1939, 0.06).
narrative_ontology:measurement(stat_tr_t1953, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1953, 0.08).
narrative_ontology:measurement(stat_tr_t1967, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(stat_tr_t1979, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1979, 0.07).
narrative_ontology:measurement(stat_tr_t1995, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(stat_tr_t2011, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2011, 0.24).
narrative_ontology:measurement(stat_tr_t2025, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(stat_be_t1917, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1917, 0.08).
narrative_ontology:measurement(stat_be_t1939, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1939, 0.09).
narrative_ontology:measurement(stat_be_t1953, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1953, 0.11).
narrative_ontology:measurement(stat_be_t1967, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1967, 0.13).
narrative_ontology:measurement(stat_be_t1979, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1979, 0.14).
narrative_ontology:measurement(stat_be_t1995, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1995, 0.17).
narrative_ontology:measurement(stat_be_t2011, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2011, 0.21).
narrative_ontology:measurement(stat_be_t2025, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2025, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(statutory_debt_ceiling__coordination_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__coordination_scaffold_reading, resource_allocation).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, extraction_snare_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the debt ceiling' decomposes into three epsilon-invariant stories sharing the kernel statutory_debt_ceiling. This file (coordination_scaffold_reading) is the upstream member: it authors the arrangement as valid, low-extraction procedural coordination (epsilon approximately 0.22), and its account of routine delegated execution is the baseline against which the downstream siblings define themselves — extraction_snare_reading accepts validity but contests function (weaponized boundary, high epsilon, declared victims), while constitutional_nullity_reading contests validity itself (Section 4 voidness). The upstream reading influences both siblings because its operational account supplies the facts each sibling reinterprets; the relation to the nullity sibling is additionally a foreclosure edge (authored in cs_structure.reading_relations) because a validly-operative coordination framework and a constitutional-voidness framework cannot both hold within one legal commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
