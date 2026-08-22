% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__narrow_scope_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Section 2(b) Doctrine-Bounded Scope (Narrow Reading)
 *   domain: legal/technological (software licensing, intellectual property, open source governance)
 *
 * SUMMARY:
 *   A mixed-source software economy runs on a boundary line drawn by GPL
 *   Section 2(b) as construed under traditional copyright doctrine: works
 *   that are direct derivatives of GPL code — modified copies distributed
 *   onward — must themselves carry the GPL and ship complete corresponding
 *   source; combinations that leave the GPL code intact and communicate
 *   across interfaces, plugin slots, or process boundaries fall outside the
 *   license's reach, so the combining side keeps its own code closed. This
 *   story instantiates that doctrine-bounded reading as the standing
 *   arrangement: a reciprocal, bounded source-disclosure obligation on a
 *   defined minority of integrators, and preserved integration latitude for
 *   everyone else, backed by copyright liability and episodic enforcement,
 *   administered through an industrializing compliance layer, and stabilized
 *   by tracking doctrine courts already apply elsewhere. Constraint-family
 *   note (epsilon deltas over the same kernel text): this file carries the
 *   narrow reading at epsilon approximately 0.38; the strong-coupling sibling
 *   authors the same text as reaching all coupling forms, with a materially
 *   higher epsilon and a redrawn victim set; the enforcement-vacuum sibling
 *   authors effective epsilon as a function of which community holds
 *   enforcement capacity. The links run through network.affects_constraints.
 *   KEY AGENTS (by structural relationship): - proprietary_software_firms:
 *   Primary beneficiary (powerful/arbitrage) — integrates GPL components
 *   behind the doctrine-bounded line - direct_derivative_distributors:
 *   Primary target (organized/constrained) — owes source for modifications,
 *   holds the reciprocal grant - gpl_stewards: Agenda setter
 *   (institutional/identity-locked) — drafts, maintains, selectively enforces
 *   - mixed_ecosystem_users: Diffuse beneficiary (powerless/mobile) -
 *   ip_adjudicators: Analytical observer (institutional/national) — the
 *   unsettled forum
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.38).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.27).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.27).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Section 2(b) Doctrine-Bounded Scope (Narrow Reading)").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "legal/technological (software licensing, intellectual property, open source governance)").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__narrow_scope_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, '223ff554-3fbc-4396-a246-ba7f3f95ddb8').
narrative_ontology:cs_kernel_codification('223ff554-3fbc-4396-a246-ba7f3f95ddb8', fixed_text).
narrative_ontology:cs_authority_grounding('223ff554-3fbc-4396-a246-ba7f3f95ddb8', practice).
narrative_ontology:cs_interpretation_layer_present('223ff554-3fbc-4396-a246-ba7f3f95ddb8').
narrative_ontology:cs_reading_relation('223ff554-3fbc-4396-a246-ba7f3f95ddb8', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('223ff554-3fbc-4396-a246-ba7f3f95ddb8', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('223ff554-3fbc-4396-a246-ba7f3f95ddb8', foundational, derivative_work_boundary_is_doctrine_bounded).
narrative_ontology:cs_axiom_status(derivative_work_boundary_is_doctrine_bounded, holdable).
narrative_ontology:cs_axiom_grounding('223ff554-3fbc-4396-a246-ba7f3f95ddb8', derivative_work_boundary_is_doctrine_bounded, conventional).
narrative_ontology:cs_axiom('223ff554-3fbc-4396-a246-ba7f3f95ddb8', secondary, non_derivative_coupling_preserves_proprietary_rights).
narrative_ontology:cs_axiom_status(non_derivative_coupling_preserves_proprietary_rights, holdable).
narrative_ontology:cs_axiom_grounding('223ff554-3fbc-4396-a246-ba7f3f95ddb8', non_derivative_coupling_preserves_proprietary_rights, conventional).
narrative_ontology:cs_reference_frame('223ff554-3fbc-4396-a246-ba7f3f95ddb8', traditional_derivative_work_doctrine).
narrative_ontology:cs_drift_state('223ff554-3fbc-4396-a246-ba7f3f95ddb8', contemporary_cloud_coupling_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('223ff554-3fbc-4396-a246-ba7f3f95ddb8', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, proprietary_software_firms).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, mixed_ecosystem_users).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, direct_derivative_distributors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, direct_derivative_distributors).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__narrow_scope_reading, traditional_derivative_work_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and maintain the license text, publish interpretive answers, hold copyrights on key works, and choose which suspected violations to pursue. They can revise the license for future grants and relicense code they control, but their authority exists only because thousands of projects voluntarily adopt the text; stepping back from custodianship would dissolve a role the institution and its community have become. Enforcement capacity is thin relative to the installed base, so pursuit is selective and settlement-driven.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, gpl_stewards, agenda_setter,
    institutional, generational, identity_locked, global).

% Ship commercial products that bundle or connect to GPL components while keeping their own source closed, relying on the line that intact GPL code communicating across interfaces is not itself a modified work. They budget compliance spend, negotiate when claims land anyway, and hold credible exits: permissive-licensed substitutes, purchased dual licenses, funded rewrites. Their engineering roadmaps treat the boundary as dependable infrastructure.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, proprietary_software_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Distribute altered GPL programs or libraries — patched kernels, extended frameworks — and therefore owe complete corresponding source for their alterations under the same license. They received the underlying code without payment and build standing work on it; unwinding accumulated GPL dependencies mid-product is expensive, so the obligation operates as a standing toll paid in disclosure. Some treat publication as contribution; others comply minimally.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, direct_derivative_distributors, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__narrow_scope_reading, direct_derivative_distributors, beneficiary).

% Obtain working software stacks assembled from GPL cores plus proprietary extensions, typically at zero price. They have no seat in license drafting, interpretation, or enforcement, and learn of boundary disputes only when a component they depend on is relabeled or withdrawn. Switching tools is individually feasible, collectively rare.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, mixed_ecosystem_users, beneficiary,
    powerless, immediate, mobile, global).

% Courts and copyright offices before which the coupling-versus-modification question would ultimately be settled. Cases presenting it have settled, been dismissed, or been avoided on procedural or standing grounds, leaving the doctrine-bounded default operative in commerce. They observe the arrangement; they have not yet constituted it.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, ip_adjudicators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__narrow_scope_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__narrow_scope_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Publishes a doctrine-anchored boundary telling integrators which combinations trigger source-sharing: direct modifications of GPL works enter the commons; intact GPL code communicating across API calls, plugin slots, or process boundaries stays outside the obligation. This lets commercial engineering proceed against free components without case-by-case scope negotiation.
% TRANSFER_FUNCTION: Moves complete corresponding source for modifications from everyone who distributes altered GPL works to the general public; leaves integration latitude — the ability to couple without sharing — with whoever connects rather than modifies. No money moves; disclosure duties and retention rights do.
% ABSENT_VOICES: Upstream authors who selected the GPL to compel sharing beyond literal derivatives have no formal seat once code is released: their remedies are advocacy, license-version changes for future grants, or network-triggered licenses for new code — not participation in how adopted code's boundary is read. Embedded-deployment end users are similarly voiceless. Both would object that the operative line is narrower than licensor intent.
% DISAPPEARANCE_RATIONALE: Without the doctrine-bounded reciprocity convention, mixed-source practice splits: integrators either face expanded claims over every coupled build or face eroding sharing norms altogether; procurement, acquisition diligence, and component sourcing all reprice. The current division of labor between commons cores and proprietary edges depends on the boundary staying where doctrine puts it.
% FOUNDING_PROBLEM: Prevent proprietors from absorbing free code and its improvements into proprietary products: Section 2(b) was written so that distributed derivatives remain under the GPL, keeping the commons self-replenishing while ordinary reuse continues.
% FOUNDING_PROBLEM_CORROBORATION: Independent accounts of pre-copyleft erosion — proprietary forks of early permissively licensed networking releases — and scholarly treatments of copyleft design attest the founding problem was real. Continued corporate compliance expenditure on GPL exposure attests that firms themselves treat it as live; the strongest external evidence is adversarial, since the parties best positioned to deny the problem budget against it. No corroboration comes from parties who profit from weak copyleft.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__narrow_scope_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__narrow_scope_reading_tests).
:- end_tests(gpl_copyleft_scope__narrow_scope_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.38: the obligation binds a defined minority (distributors of modified GPL works), is reciprocal (they received the code at zero price), and stops at a doctrine line — extraction is real but bounded, hence moderate. Suppression 0.27 is structural, carried by the copyright-liability backdrop and episodic enforcement rather than continuous coercive machinery; it is a raw property, unscaled by scope or power in the engine's arithmetic. Theater 0.29 and rising: compliance has industrialized (scanning, attestations, audit vendors) faster than enforcement has materialized, so a growing share of boundary-related activity is precautionary performance. Accessibility collapse 0.42: exits persist everywhere — permissive-licensed stacks, dual-licensing offers, funded rewrites — so understanding the constraint does not foreclose alternatives. Resistance 0.32: boundary-testing by integrators, negotiated settlements, and quiet minimal compliance. All three series run on one shared six-point grid; the suppression_requirement series is authored because this story specifically tracks enforcement-capacity history — buildup through the early enforcement-campaign era, then decay as marquee litigation failed to settle the boundary. Claim and metrics are independent authored facts: I claim rope because the arrangement coordinates reuse against reciprocity with bounded asymmetry; the metrics describe its actual moderate, drifting operation, and any divergence between the computed per-seat types and this claim is the measurement the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   From the proprietary-firm seat the boundary is load-bearing infrastructure for lawful commerce; from the direct-derivative-distributor seat the same boundary is a standing toll paid in source disclosure; from the steward seat it is a defensive perimeter smaller than the one they meant to build; from the adjudicator seat it is a question successfully avoided. Same structure, four different constraints — the engine computes per-seat classifications from power, exit, and directionality rather than averaging them into one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. proprietary_software_firms and mixed_ecosystem_users appear only as beneficiaries, and the firms' arbitrage-grade exit pins them near the beneficiary pole. direct_derivative_distributors appear in victims, which the derivation reads toward the target pole amplified by constrained exit — but the same seat holds a secondary beneficiary position: the reciprocal grant of the underlying code they build on. Uncorrected, the derivation would overstate their d; the override (organized -> 0.72) encodes 'paying recipient.' gpl_stewards derive a low d as agenda-setting incidental beneficiaries; their exit lock is institutional identity fusion — the organization has become its custodial function — which is a property of their seat, not of their directionality. Receipt check for gain_flow: the principal extracted good is modification source code, and it lands in the unseated public commons; no named seat captures it, and the monetized leakage (settlements, audit fees) is second-order and likewise unseated — 'diffuse' is authored affirmatively after checking every seat, not defaulted.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as a snare would misdescribe a bounded reciprocal obligation as predation; reading it as frictionless coordination would miss the compliance-industrial drift visible in the theater series. The classification holds both errors out: extraction is real but confined to a set that received the code in exchange, and the founding mandate — keeping the commons self-replenishing — is still served, so no obsolescence flag fires. On the receipt surface, fixing_cost is authored 'prohibitive' because any binding clarification of the boundary reopens the interpretive contest no seat can win unilaterally: stewards cannot bind already-granted distributed copyrights, and adjudicators have declined the merits question. This pairs mechanically with gain_flow 'diffuse' in the receipt grid; the pairing is an honest joint judgment, not a template, and the arrangement is not inertial — its function is live, its theater is a monitored symptom, and what the temporal series watches for is compliance performance substituting for actual reciprocity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positionality,
    'This constraint instantiates the narrow_scope_reading of kernel gpl_copyleft_scope; how would the structural picture change under the strong_copyleft_reading sibling, and where exactly do the readings diverge?',
    'Definitive appellate treatment of whether dynamic linking, plugin loading, or aggregation constitutes a derivative work; until such a ruling the readings coexist as competing interpretive stances held by different communities.',
    'Under the strong reading the beneficiary seats flip toward target (integration latitude becomes infringing conduct), epsilon rises materially, and enforcement intensity becomes load-bearing — plausibly tangled_rope or snare per seat. The disagreement is located entirely at the derivative-work boundary element, not at the license''s existence or its reciprocity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positionality, conceptual, 'Committer structure: one reading of a contested scope kernel; the sibling reading would redraw the victim set.').

omega_variable(
    enforcement_capacity_distribution,
    'Does effective scope track the doctrinal text at all, or only whichever interpretive community holds enforcement capacity in a given ecosystem?',
    'Compare realized enforcement outcomes and negotiated compliance across steward-aligned versus industry-dominated ecosystems over the interval.',
    'If capacity dominates, this reading''s epsilon is an artifact of industry weight: in steward-aligned pockets the same coupling patterns bind. Grounds the influences edge to the enforcement-vacuum sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_distribution, empirical, 'Sibling-driven uncertainty: effective constraint as enforcement-capacity-dependent plurality.').

omega_variable(
    marginal_coupling_derivativity,
    'Where does traditional doctrine actually place specific coupling patterns — static linking, dynamic linking, separate-process communication, plugin architectures — on the derivative-work spectrum?',
    'An appellate merits ruling on a squarely presented linking case, or settled doctrinal guidance from a major jurisdiction.',
    'Resolves which integrator populations sit inside the obligation; a broader line converts part of the beneficiary seat into payers and lifts extraction; a tighter line shrinks the payer set further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_coupling_derivativity, empirical, 'Object-level boundary question this reading presumes rather than settles.').

omega_variable(
    saas_network_use_strain,
    'Does a distribution-triggered obligation survive the shift to hosted delivery, where modified GPL code is run for customers but never distributed?',
    'Track network-triggered license adoption by upstream projects seeking to reach hosted use, and the incidence of hosted deployments of distribution-keyed GPL components.',
    'Widespread hosted avoidance would hollow the payer set from below as obligations become routable around; upstream migration to network-triggered terms would change the arrangement''s composition for future grants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saas_network_use_strain, empirical, 'Cloud-era stress on a distribution-keyed boundary.').

omega_variable(
    compliance_theater_functionality,
    'Is industrialized license compliance reducing actual violations, or substituting attestations and scans for conformity?',
    'Correlate compliance-program maturity with audited violation incidence and remediation depth across firms.',
    'If substitution dominates, the theater_ratio understates drift and the reciprocity core beneath the compliance surface is thinner than it appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_theater_functionality, empirical, 'Whether the growing compliance layer performs or protects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_narrow_scope_tr_t0, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0, 0.11).
narrative_ontology:measurement_basis(gpl_narrow_scope_tr_t0, observed).
narrative_ontology:measurement(gpl_narrow_scope_tr_t6, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement_basis(gpl_narrow_scope_tr_t6, observed).
narrative_ontology:measurement(gpl_narrow_scope_tr_t12, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(gpl_narrow_scope_tr_t12, observed).
narrative_ontology:measurement(gpl_narrow_scope_tr_t18, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(gpl_narrow_scope_tr_t18, observed).
narrative_ontology:measurement(gpl_narrow_scope_tr_t24, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement_basis(gpl_narrow_scope_tr_t24, observed).
narrative_ontology:measurement(gpl_narrow_scope_tr_t30, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(gpl_narrow_scope_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(gpl_narrow_scope_be_t0, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(gpl_narrow_scope_be_t0, observed).
narrative_ontology:measurement(gpl_narrow_scope_be_t6, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement_basis(gpl_narrow_scope_be_t6, observed).
narrative_ontology:measurement(gpl_narrow_scope_be_t12, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement_basis(gpl_narrow_scope_be_t12, observed).
narrative_ontology:measurement(gpl_narrow_scope_be_t18, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 18, 0.38).
narrative_ontology:measurement_basis(gpl_narrow_scope_be_t18, observed).
narrative_ontology:measurement(gpl_narrow_scope_be_t24, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement_basis(gpl_narrow_scope_be_t24, observed).
narrative_ontology:measurement(gpl_narrow_scope_be_t30, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(gpl_narrow_scope_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl_narrow_scope_su_t0, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0, 0.13).
narrative_ontology:measurement_basis(gpl_narrow_scope_su_t0, observed).
narrative_ontology:measurement(gpl_narrow_scope_su_t6, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 6, 0.21).
narrative_ontology:measurement_basis(gpl_narrow_scope_su_t6, observed).
narrative_ontology:measurement(gpl_narrow_scope_su_t12, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 12, 0.28).
narrative_ontology:measurement_basis(gpl_narrow_scope_su_t12, observed).
narrative_ontology:measurement(gpl_narrow_scope_su_t18, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 18, 0.3).
narrative_ontology:measurement_basis(gpl_narrow_scope_su_t18, observed).
narrative_ontology:measurement(gpl_narrow_scope_su_t24, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 24, 0.28).
narrative_ontology:measurement_basis(gpl_narrow_scope_su_t24, observed).
narrative_ontology:measurement(gpl_narrow_scope_su_t30, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 30, 0.27).
narrative_ontology:measurement_basis(gpl_narrow_scope_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'GPL scope' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints over one kernel text: this file (narrow, doctrine-bounded reading — bounded reciprocal obligation, rope-shaped profile), gpl_copyleft_scope__strong_copyleft_reading (all coupling counts as derivative — broader captured set, higher epsilon, enforcement-heavy), and gpl_copyleft_scope__enforcement_vacuum_reading (effective constraint determined by enforcement-capacity distribution). The epsilon values differ because they are reading-indexed over the same referent text, not because the text changed; the upstream doctrinal anchor vindicated here is cited by the strong reading as insufficient and by the vacuum reading as indeterminate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__narrow_scope_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
