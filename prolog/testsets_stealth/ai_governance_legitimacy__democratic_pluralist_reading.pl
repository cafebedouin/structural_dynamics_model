% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__democratic_pluralist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__democratic_pluralist_reading, []).

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
 *   constraint_id: ai_governance_legitimacy__democratic_pluralist_reading
 *   human_readable: Democratic Pluralist Reading of AI Governance Legitimacy
 *   domain: political/theological/technological
 *
 * SUMMARY:
 *   A contested normative arrangement: the claim that AI governance is
 *   legitimate only when it issues from inclusive democratic deliberation and
 *   consent of the governed, with no tradition — religious or technocratic —
 *   holding interpretive monopoly. Under this reading the encyclical's
 *   dignity claims are admitted as serious contributions while its claim to
 *   authoritative interpretation is denied; the Magisterium participates as
 *   one voice among many. Per the epsilon-invariance principle this file
 *   instantiates ONE clean reading of the ai_governance_legitimacy kernel:
 *   the sibling readings (magisterial_subsidiarity,
 *   technocratic_optimization, market_libertarian) are separate constraints
 *   with their own epsilon, victim sets, and types, linked through
 *   network.affects_constraints. The claim/metric gap is deliberate: the
 *   reading CLAIMS scaffold while the authored metrics describe moderate
 *   extraction and a rising consultative layer — the engine measures that
 *   divergence; the claim is not reconciled to the metrics. KEY AGENTS (by
 *   structural relationship): - democratic_institutions: Primary
 *   agenda-setter and institutional beneficiary (institutional/constrained) —
 *   administers the legitimacy procedure, collects authority and oversight
 *   budgets - democratic_electorates: Sovereign source
 *   (organized/constrained) — authorizes the procedure through elections -
 *   civil_society_organizations: Secondary beneficiary (organized/mobile) —
 *   staffs and supplies the deliberative content - minority_rights_holders:
 *   Protected-but-exposed seat (powerless/constrained) — rights shield and
 *   majority-vote exposure on the same structure -
 *   deliberatively_excluded_groups: Structural victim (powerless/trapped) —
 *   bound by rules made in forums they cannot enter -
 *   authoritarian_regime_populations: External victim (powerless/trapped) —
 *   inherit standards shaped elsewhere without consent -
 *   magisterial_tradition_holders: Demoted rival authority
 *   (institutional/identity_locked) — voice retained, monopoly denied -
 *   ai_developers_and_technocrats: Regulated contributor (powerful/arbitrage)
 *   — procedural costs offset partly by social license -
 *   deliberative_democracy_scholars: Analytical observer
 *   (analytical/analytical) — sees the full structure, collects nothing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__democratic_pluralist_reading, 0.38).
domain_priors:suppression_score(ai_governance_legitimacy__democratic_pluralist_reading, 0.3).
domain_priors:theater_ratio(ai_governance_legitimacy__democratic_pluralist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__democratic_pluralist_reading, scaffold).
narrative_ontology:human_readable(ai_governance_legitimacy__democratic_pluralist_reading, "Democratic Pluralist Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__democratic_pluralist_reading, "political/theological/technological").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:has_sunset_clause(ai_governance_legitimacy__democratic_pluralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__democratic_pluralist_reading, 'ac11b447-de47-4330-a51f-8fe32c537a7d').
narrative_ontology:cs_kernel_codification('ac11b447-de47-4330-a51f-8fe32c537a7d', distributed).
narrative_ontology:cs_authority_grounding('ac11b447-de47-4330-a51f-8fe32c537a7d', practice).
narrative_ontology:cs_interpretation_layer_present('ac11b447-de47-4330-a51f-8fe32c537a7d').
narrative_ontology:cs_reading_relation('ac11b447-de47-4330-a51f-8fe32c537a7d', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac11b447-de47-4330-a51f-8fe32c537a7d', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac11b447-de47-4330-a51f-8fe32c537a7d', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('ac11b447-de47-4330-a51f-8fe32c537a7d', foundational, no_interpretive_monopoly_over_ai_principles).
narrative_ontology:cs_axiom_status(no_interpretive_monopoly_over_ai_principles, holdable).
narrative_ontology:cs_axiom_grounding('ac11b447-de47-4330-a51f-8fe32c537a7d', no_interpretive_monopoly_over_ai_principles, deontological).
narrative_ontology:cs_axiom('ac11b447-de47-4330-a51f-8fe32c537a7d', foundational, binding_ai_rules_require_deliberative_consent).
narrative_ontology:cs_axiom_status(binding_ai_rules_require_deliberative_consent, holdable).
narrative_ontology:cs_axiom_grounding('ac11b447-de47-4330-a51f-8fe32c537a7d', binding_ai_rules_require_deliberative_consent, deontological).
narrative_ontology:cs_axiom('ac11b447-de47-4330-a51f-8fe32c537a7d', secondary, encyclical_enters_as_one_voice_among_many).
narrative_ontology:cs_axiom_status(encyclical_enters_as_one_voice_among_many, holdable).
narrative_ontology:cs_axiom_grounding('ac11b447-de47-4330-a51f-8fe32c537a7d', encyclical_enters_as_one_voice_among_many, conventional).
narrative_ontology:cs_reference_frame('ac11b447-de47-4330-a51f-8fe32c537a7d', inclusive_public_reason_order).
narrative_ontology:cs_drift_state('ac11b447-de47-4330-a51f-8fe32c537a7d', contemporary_ai_acceleration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ac11b447-de47-4330-a51f-8fe32c537a7d', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, deliberatively_excluded_groups).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, authoritarian_regime_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, democratic_electorates).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, magisterial_tradition_holders).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, ai_developers_and_technocrats).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, magisterial_tradition_holders).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, ai_developers_and_technocrats).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures, constitutional courts, and electoral bodies operationalize the reading: they convene consultations, enact AI statutes, and review rival governance arrangements for compatibility with rights and process. Authority, jurisdiction, and oversight budgets flow to them as the designated legitimacy channel. Their exit is constrained because their claim to decide depends on continuing to embody the procedure — abandoning it would dissolve their own authorization.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, beneficiary).

% Citizens authorize and dismiss governments, vote in referendums, and supply the deliberative demand that gives the reading its force. They gain assurance that AI rules answer to someone they can remove, and bear the arrangement's costs indirectly through prices, taxes, and slower deployment. Emigration is the only exit, and it is costly.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, democratic_electorates, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, democratic_electorates, beneficiary).

% Advocacy groups, academic centers, and NGOs gain standing consultative roles, funding channels, and agenda influence; they staff advisory boards and supply much of the deliberative content. Exit is comparatively easy — they can redirect to other issues or jurisdictions — which keeps their position a benefit rather than a trap.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, beneficiary,
    organized, biographical, mobile, global).

% Rights protections, anti-discrimination review, and recourse procedures shield groups most exposed to biased or opaque AI systems. But where majorities coordinate, the same deliberative process can outvote them, and litigation is slow and costly; protection and exposure ride the same structure.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders, payer).

% Non-citizens affected by exported AI systems, linguistically isolated communities, the digitally disconnected, and the not-yet-born bear rules made in forums they cannot enter. Their objection is foreseeable but unrepresented; no exit route leads into the deliberative demos, and their interests surface only through proxy advocates.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, deliberatively_excluded_groups, excluded,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, deliberatively_excluded_groups, payer).

% Populations under authoritarian governments are governed by AI systems whose design and deployment answer to no deliberative process at all. Standards shaped by democratic-pluralist processes propagate through global supply chains and technical norms they inherit without consent, while their own states deny them the participatory remedy the reading prescribes.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, authoritarian_regime_populations, payer,
    powerless, generational, trapped, continental).

% The Catholic Magisterium retains a protected voice — its dignity claims are admitted as serious contributions — but loses the claim to authoritative interpretation it holds under its own reading of the same questions. Exit is identity-locked: relinquishing its teaching office would dissolve the institution itself, so it participates while contesting the terms of participation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, magisterial_tradition_holders, payer,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, magisterial_tradition_holders, beneficiary).

% Developers and technical experts submit deployment decisions to deliberated rules rather than professional judgment alone: impact assessments, consultations, and conformity procedures slow them and subordinate optimization goals to publicly chosen values. In exchange they acquire a social license and predictable rules that unaccountable expertise never secured; firms can also arbitrage, shifting launches toward jurisdictions with lighter process.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, ai_developers_and_technocrats, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, ai_developers_and_technocrats, beneficiary).

% Political theorists and governance researchers examine whether the arrangement's operation matches its justification: measuring inclusion, tracking capture of consultative seats, and comparing outcomes across jurisdictions governed under rival readings. They collect no rents and bear no compliance costs.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, deliberative_democracy_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__democratic_pluralist_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, contestable procedure for making binding AI rules where no tradition commands universal assent: elections select decision-makers, legislative and regulatory process aggregates positions, judicial review polices rights and process, and public reason forces justifications into terms every comprehensive doctrine can evaluate.
% TRANSFER_FUNCTION: Moves final decision authority over AI governance from any single tradition — magisterial, technocratic, or market — to the demos and its accountable institutions; moves compliance and deliberation costs onto developers, deployers, and participating organizations roughly in proportion to their exposure; moves voice and agenda influence toward organized civil society and away from unaccountable expertise and hierarchy.
% ABSENT_VOICES: Those outside the deliberative demos — authoritarian-regime populations, non-citizens bearing exported-system externalities, the digitally disconnected, future generations — would object that consent of the governed draws its boundary around those already inside. Dissenting traditions would object that the procedure itself prejudges their authority claims by admitting them only as participants. Neither group holds a seat; their objections surface mainly through the observer literature and occasional diplomatic protest.
% DISAPPEARANCE_RATIONALE: If the democratic-pluralist legitimacy regime vanished overnight, AI governance would fragment among the rival readings with no agreed arbiter: magisterial, technocratic, and market frameworks would compete openly for jurisdiction, the participatory infrastructure built under this reading (citizens' assemblies, consultative seats, rights-review dockets) would lose its authorizing basis, and populations currently shielded by rights review would face arrangements unconstrained by process criteria.
% FOUNDING_PROBLEM: How to legitimate binding rules for a transformative technology in societies where no single tradition — religious, technocratic, or commercial — commands general assent, without sliding into sectarian imposition, expert capture, or pure market release: the post-war democratic-constitutional settlement applied to machine intelligence.
% FOUNDING_PROBLEM_CORROBORATION: Rival readings corroborate the problem while disputing this solution: the magisterial tradition attests that technology must be subordinated to accountable moral principles (its own reading exists because the problem is live), market libertarians attest the legitimacy question while answering it with voluntary exchange, and authoritarian states implicitly concede it by constructing rival legitimacy derivations. International soft-law processes invoke the problem but sit close to the benefiting parties, so the decisive corroboration is adversarial: the existence of every sibling reading is testimony that the founding problem remains unresolved.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__democratic_pluralist_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__democratic_pluralist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__democratic_pluralist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__democratic_pluralist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).
:- end_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.38 at interval end): the arrangement imposes real procedural costs on developers, demotes rival authorities from interpretive monopoly to participation, and generates exclusion externalities on those outside the demos — but its costs are publicly justified, contestable, and far below snare levels. Suppression (0.30) is authored as a raw structural property, unscaled by power or scope (only extractiveness is scaled, by directionality and scope, in the engine): enforcement runs through consent-compatible machinery — elections, judicial review, civil-liberties limits — with residual coercive force where review strikes down rival arrangements and where rights limits bind traditions' internal exercise of authority. Theater (0.25) reflects a growing consultative layer — ethics boards and advisory councils with advisory-only remits performing inclusion — atop load-bearing electoral and judicial functions. Accessibility_collapse (0.35) is low because rival readings remain legally arguable and institutionally embodied; understanding the constraint does not eliminate alternatives. Resistance (0.45) is sustained: accelerationist impatience, market opposition to mandates, magisterial objection to demotion, and outright authoritarian rejection shape implementation without displacing the arrangement. The measurement series run on one shared time grid (points 0,2,4,6,8,10) so every tracked metric is authored at every examined time point; all three trajectories rise monotonically as soft-law aspiration hardened into binding instruments with penalty regimes and consultative apparatus over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute differently. From the institutional seats the arrangement is the precondition of legitimate common action — the only procedure that lets binding rules issue where metaphysical consensus is unavailable. From the developer seat it is friction and subordination; from the magisterial seat it is a demotion that preserves voice while confiscating authority; from the excluded seats it is a consent mechanism whose boundary leaves them outside it. Same structure, opposite phenomenology; the engine computes per-seat classifications from the structural data, and the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for civil_society_organizations (mobile exit damps further) and democratic_institutions; victim declarations drive high directionality for deliberatively_excluded_groups and authoritarian_regime_populations, amplified by trapped exit and powerless position — these seats approach the full-target end despite the reading's inclusive self-description, which is the sharpest divergence between the reading's lights and its structural operation. Dual-positioned seats sit intermediate: minority_rights_holders (shield and exposure on one structure), ai_developers_and_technocrats (costs offset by social license, arbitrage exit damping effective extraction), magisterial_tradition_holders (near-symmetric: retained protected voice offsets the lost monopoly, but identity-lock — the institution has fused with its teaching office such that exit would dissolve it — pushes effective extraction somewhat above the raw cost share). No directionality overrides were needed: the beneficiary/victim declarations plus exit options carry the derivation, and the magisterial seat's near-symmetric position is documented here rather than forced through an override keyed to a shared power atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification guards against two mislabels. Mislabeling as rope (pure coordination) would hide the asymmetric costs: demoted traditions and excluded populations pay for a coordination they cannot shape. Mislabeling as snare would ignore the genuine, publicly justified coordination function and the arrangement's own transitional theory — its institutional forms (assemblies, consultative seats, transitional oversight) are declared as scaffolding toward mature democratic self-governance, hence the authored sunset clause. The founding problem (legitimating AI rules amid value pluralism) remains live, so no mandatrophy-resolved declaration is made; the forward risk the temporal series monitors is consultative seats hardening into a permanent intermediary class after the transition succeeds, which would show up as theater_ratio climbing past 0.5 and the sunset clause going unexercised.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel ai_governance_legitimacy — the democratic-pluralist reading. Would instantiating a sibling reading (magisterial_subsidiarity, technocratic_optimization, market_libertarian) yield a different victim set, epsilon, and type for the same governance domain?',
    'Author each sibling as its own constraint story and compare computed classifications; track which legitimacy derivation binding instruments actually cite over time.',
    'If the magisterial reading prevails, victims shift toward actors deviating from doctrine-conform governance and epsilon re-anchors on doctrinal deviation; if the technocratic reading prevails, victims shift to those harmed by unaccountable optimization and enforcement migrates to performance metrics. The disagreement is located in the source-of-legitimacy premise, not in downstream instrumentation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one of four rival readings of the AI-legitimacy kernel.').

omega_variable(
    demos_boundary_ambiguity,
    'Who counts as the governed whose consent legitimates AI governance, given that AI systems'' effects cross the borders of any deliberative demos?',
    'Comparative analysis of extraterritorial AI regimes (Brussels-effect adoption, model export controls) and of representation mechanisms extended to non-citizens and future generations.',
    'If the demos must include all significantly affected parties, the exclusion externalities currently priced as moderate become core violations and epsilon rises sharply; if bounded-demos consent is accepted, current exclusions are tolerable transition costs of the scaffold phase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demos_boundary_ambiguity, conceptual, 'Boundary of the consenting demos versus the population bearing AI externalities.').

omega_variable(
    deliberative_capacity_under_synthetic_media,
    'Can inclusive public reason function in an information environment saturated with synthetic media, microtargeting, and attention-extracting AI — or does the constraint''s own subject matter erode its enabling condition?',
    'Longitudinal measurement of deliberative-process quality (polarization indices, misinformation penetration in consultation inputs, stability of citizens''-assembly outputs) as AI-generated content scales.',
    'If deliberative capacity degrades, the arrangement drifts toward piton — legitimacy language performed over a consent mechanism that no longer functions — and theater_ratio should climb past 0.5.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_capacity_under_synthetic_media, empirical, 'Whether AI erodes the deliberation the reading depends on.').

omega_variable(
    consultative_seat_capture,
    'Is the participatory infrastructure genuinely transitional toward mature democratic self-governance, or are consultative seats hardening into a permanent intermediary class collecting standing, funding, and agenda rents?',
    'Track turnover, funding dependence, and agenda-setting power of civil-society consultative seats across successive AI governance cycles; test whether sunset and review clauses are exercised or routinely renewed.',
    'If seats capture, the scaffold reclassifies toward tangled_rope — coordinated inclusion extracting rents for intermediaries — and the sunset clause is revealed as decorative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consultative_seat_capture, empirical, 'Scaffold-versus-permanent-apparatus question for the participatory layer.').

omega_variable(
    minority_shield_or_exposure,
    'Does the reading''s rights apparatus function as a shield for minority rights holders, or does majoritarian deliberation expose them wherever judicial review is weak?',
    'Compare outcomes for algorithmic-harm complainants across jurisdictions with strong versus weak judicial review and rights charters.',
    'Where exposure dominates, the minority_rights_holders seat flips from net beneficiary to net payer, raising measured extraction and pushing the arrangement toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_shield_or_exposure, empirical, 'Directionality ambiguity of the minority-protection seat.').

omega_variable(
    rival_arrangement_suppression_status,
    'When judicial review strikes down rival governance arrangements (sectarian impositions, deregulated deployments), is that suppression of rival readings or neutral arbitration among them?',
    'Doctrinal analysis of review standards: whether courts apply viewpoint-neutral process criteria or substantive criteria that privilege one reading''s values.',
    'If substantive, the suppression metric understates the constraint''s coercive maintenance of its own reading, and the coexists_with relations harden toward open rivalry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rival_arrangement_suppression_status, conceptual, 'Whether enforcement arbitrates among readings or entrenches this one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__democratic_pluralist_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ai_g_tr_t2, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 2, 0.12).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement(ai_g_tr_t6, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 6, 0.19).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ai_g_be_t2, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 2, 0.24).
narrative_ontology:measurement(ai_g_be_t4, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(ai_g_be_t6, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 6, 0.31).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(ai_g_su_t2, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 2, 0.15).
narrative_ontology:measurement(ai_g_su_t4, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 4, 0.18).
narrative_ontology:measurement(ai_g_su_t6, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 6, 0.21).
narrative_ontology:measurement(ai_g_su_t8, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 8, 0.26).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 10, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__democratic_pluralist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'AI governance legitimacy' covers four structurally distinct legitimacy derivations — democratic-deliberative, magisterial-subsidiary, technocratic-optimizing, market-voluntarist — each with its own epsilon, beneficiary/victim structure, and enforcement machinery. This file authors only the democratic-pluralist member. When institutionally ascendant, this reading changes the operating environment of the others (rival readings must litigate within public reason to gain traction), but no member logically eliminates another; the family link records the coupling without merging the constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
