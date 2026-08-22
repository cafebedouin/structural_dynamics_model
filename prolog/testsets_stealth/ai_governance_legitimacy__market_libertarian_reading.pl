% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__market_libertarian_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ai_governance_legitimacy__market_libertarian_reading
 *   human_readable: Market-Libertarian Reading of AI Governance Legitimacy
 *   domain: theological/political/economic
 *
 * SUMMARY:
 *   This story instantiates the market_libertarian_reading of the
 *   ai_governance_legitimacy kernel. The standing arrangement under contest
 *   is the settlement in which AI governance legitimacy flows from property
 *   rights in models, data, and compute and from voluntary exchange, with
 *   collective mandates ruled categorically illegitimate. The reading claims
 *   this settlement as pre-political — hence the mountain claim with
 *   emerges_naturally — while the authored structural record shows
 *   identifiable beneficiaries (capital holders, autonomous professionals)
 *   and identifiable cost-bearers (monopsony workers, locked-in users,
 *   communities unable to coordinate). The story is therefore authored as a
 *   false-summit candidate: the naturality claim and the beneficiary
 *   structure stand side by side, and the engine measures whether the former
 *   survives the latter. Family note: the sibling readings
 *   (magisterial_subsidiarity, technocratic_optimization,
 *   democratic_pluralist) are separate files with their own epsilon values
 *   over their own referents. This file's epsilon (0.28) is authored only for
 *   this reading's referent — the market settlement itself, carrying the
 *   residual costs the reading itself concedes at the margins (monopsony,
 *   asymmetry, externalities) — and is not averaged against siblings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.28).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.4).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "Market-Libertarian Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological/political/economic").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__market_libertarian_reading).
domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, 'f39c4feb-5e1b-4f36-9d1a-bc5e9952e248').
narrative_ontology:cs_kernel_codification('f39c4feb-5e1b-4f36-9d1a-bc5e9952e248', distributed).
narrative_ontology:cs_authority_grounding('f39c4feb-5e1b-4f36-9d1a-bc5e9952e248', practice).
narrative_ontology:cs_interpretation_layer_present('f39c4feb-5e1b-4f36-9d1a-bc5e9952e248').
narrative_ontology:cs_reading_relation('f39c4feb-5e1b-4f36-9d1a-bc5e9952e248', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f39c4feb-5e1b-4f36-9d1a-bc5e9952e248', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f39c4feb-5e1b-4f36-9d1a-bc5e9952e248', ai_governance_legitimacy__technocratic_optimization_reading, influences).
narrative_ontology:cs_axiom('f39c4feb-5e1b-4f36-9d1a-bc5e9952e248', foundational, voluntary_exchange_exclusive_legitimacy).
narrative_ontology:cs_axiom_status(voluntary_exchange_exclusive_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f39c4feb-5e1b-4f36-9d1a-bc5e9952e248', voluntary_exchange_exclusive_legitimacy, deontological).
narrative_ontology:cs_axiom('f39c4feb-5e1b-4f36-9d1a-bc5e9952e248', foundational, collective_mandates_illegitimate_coercion).
narrative_ontology:cs_axiom_status(collective_mandates_illegitimate_coercion, holdable).
narrative_ontology:cs_axiom_grounding('f39c4feb-5e1b-4f36-9d1a-bc5e9952e248', collective_mandates_illegitimate_coercion, deontological).
narrative_ontology:cs_axiom('f39c4feb-5e1b-4f36-9d1a-bc5e9952e248', secondary, exit_mechanisms_constitute_dignity_protection).
narrative_ontology:cs_axiom_status(exit_mechanisms_constitute_dignity_protection, holdable).
narrative_ontology:cs_axiom_grounding('f39c4feb-5e1b-4f36-9d1a-bc5e9952e248', exit_mechanisms_constitute_dignity_protection, instrumental).
narrative_ontology:cs_reference_frame('f39c4feb-5e1b-4f36-9d1a-bc5e9952e248', pre_political_exchange_order).
narrative_ontology:cs_drift_state('f39c4feb-5e1b-4f36-9d1a-bc5e9952e248', contemporary_ai_concentration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f39c4feb-5e1b-4f36-9d1a-bc5e9952e248', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, incumbent_ai_laboratories).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, venture_capital_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, ai_startup_founders).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_technologists).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, monopsony_platform_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, low_bargaining_power_users).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, coordination_failure_communities).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, pre_political_property_rights).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, spontaneous_order_coordination).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, consumer_sovereignty_welfare_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the largest AI research and deployment organizations. Own trained model weights, proprietary datasets, and compute fleets as legally protected assets and license access on negotiated terms. Benefit from the absence of collective mandates over deployment decisions and from enforceable rights against unauthorized copying. Can shift assets, incorporation, and research operations across jurisdictions when rules turn unfavorable.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, incumbent_ai_laboratories, beneficiary,
    institutional, generational, arbitrage, global).

% Supply the capital that funds AI startups and scale-ups in exchange for equity. Returns depend on strong ownership rights in models and data and on freedom to structure contracts without mandated obligations. Reallocate portfolios across sectors and countries quickly; no jurisdictional tie binds them.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, venture_capital_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% Found and run AI companies. Gain market entry on the strength of their products and contracts rather than licenses or approvals, and keep the upside under ownership rights. Relocate, reincorporate, or sell when a regulatory environment tightens.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, ai_startup_founders, beneficiary,
    powerful, biographical, mobile, global).

% Scarce engineering and research talent commanding high compensation and wide employer choice. Prefer workplaces and products free from collective scheduling, output mandates, or licensing gates. Move between employers, countries, and independent work with little friction.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_technologists, beneficiary,
    powerful, biographical, mobile, global).

% Perform labeling, moderation, content, and gig tasks for AI-producing platforms that dominate demand for their labor in their locality. Terms arrive take-it-or-leave-it under individual contracts; wages and conditions reflect thin outside options. Switching platforms rarely changes the terms on offer.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, monopsony_platform_workers, payer,
    powerless, immediate, constrained, regional).

% Use AI-mediated services under standardized terms of service, supplying behavioral data and attention as the price of participation. Cannot negotiate terms individually; leaving a dominant service carries social and practical costs, and available substitutes offer similar terms.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, low_bargaining_power_users, payer,
    powerless, biographical, constrained, global).

% Bear localized consequences of AI deployment — labor displacement, environmental load from compute facilities, erosion of local information ecosystems — with no recognized standing to negotiate collectively, because binding collective demands are treated as outside legitimate governance altogether. Moving away from the externalities is not feasible at reasonable cost.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, coordination_failure_communities, payer,
    powerless, generational, trapped, regional).

% Courts and commercial arbitration bodies that define and enforce ownership of models, data, and compute and police the contracts built on them. Extend existing property and contract doctrine to novel AI assets case by case. Bound by precedent and jurisdiction; they administer the settlement rather than redesign it.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, property_rights_adjudicators, agenda_setter,
    institutional, generational, constrained, global).

% Carry the social-doctrine tradition insisting that technology serve the common good under solidarity and the universal destination of goods. Their proposals for binding obligations on AI developers are ruled out of the legitimacy conversation at the outset, classified as coercion rather than contribution. Leaving the tradition is not a live option for them; the commitment constitutes the community.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, catholic_social_teaching_advocates, excluded,
    organized, civilizational, identity_locked, global).

% Study how theological and political traditions allocate authority over technology. Trace which claims about legitimacy travel from economics into theology and back, and who bears the costs of each settlement. Hold no stake in any settlement prevailing.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, political_theology_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__market_libertarian_reading, incumbent_ai_laboratories).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__market_libertarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides secure, transferable ownership of models, data, and compute and enforces contracts among unequal parties, so that investment, specialization, and deployment decisions coordinate through prices and agreement rather than central planning or licensing.
% TRANSFER_FUNCTION: Moves decision authority over AI development to holders of capital and property; moves returns from deployed AI toward those holders; moves risks and spillovers — wage pressure, data appropriation, local externalities — onto workers, users, and communities with the least bargaining power.
% ABSENT_VOICES: Social-doctrine advocates participate in public debate but are excluded from legitimacy inside this reading — their solidarity demands arrive pre-classified as coercion. Also missing: people who cannot appear as market actors at all (children, dependent adults, future generations bearing long-run externalities), and ecosystem interests with no bidding capacity.
% DISAPPEARANCE_RATIONALE: If the property-and-consent settlement vanished overnight, ownership of model weights, datasets, and compute would fall open to contest; investment would pause until a successor legitimacy principle — mandate, deliberation, or doctrine — allocated control; contracting, licensing, and arbitration practice would be rebuilt around whichever principle prevailed. Current AI production does not persist without it.
% FOUNDING_PROBLEM: Coordinating complex economic activity — here, AI development — without submitting it to centralized command, and shielding individuals from arbitrary collective authority; a settlement forged historically against central planning and absolutist allocation.
% FOUNDING_PROBLEM_CORROBORATION: Historians of economic thought corroborate the founding problem's reality in its original setting (the socialist-calculation and planning debates). The social-doctrine tradition corroborates the decentralizing half (subsidiarity) while rejecting the conclusion that solidarity obligations are coercion. Labor economists and competition scholars outside the beneficiary set attest that the salient threat in contemporary AI markets is private concentration rather than state command, disputing the founding problem's current framing. No source outside the beneficiary set attests that the problem, as this reading frames it, remains the operative one for AI governance.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__market_libertarian_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, ExtMetricName, E),
    domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28) because by this reading's own lights exchange is presumptively mutual-benefit; the residual sits in monopsony wage setting, data appropriation under non-negotiable terms, and uncompensated local externalities. Suppression (0.40) reflects enforcement through contract law, arbitration clauses, and the categorical delegitimation of mandate-based alternatives — real coercive machinery, lighter than a licensing regime. Theater (0.22) is low but rising: the 'innovation flourishes unencumbered' claim increasingly functions as rhetorical maintenance while actual market structure concentrates. Accessibility_collapse (0.58) is honestly intermediate: within the reading's own framework alternatives collapse nearly completely (mandates are defined out as coercion by fiat), yet rival readings persist institutionally, so collapse is far short of a natural law's near-total closure. Resistance (0.62) is substantial and organized: the social-doctrine tradition, democratic regulators, and labor organizing all contest the settlement. The temporal series run on one shared grid; extractiveness and suppression rise together as enforcement machinery (IP litigation over weights, arbitration expansion) matures alongside concentration. Coalition note: the three victim seats are individually powerless, but a worker-user-community coalition is the obvious countervailing power — and the settlement's defining move, delegitimating collective mandates, is precisely what impedes that coalition from forming. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats should compute differently. From the founder, investor, and laboratory seats the settlement is the coordination order they built, staff, and profit within — secure expectations, frictionless contracting. From the monopsony worker and trapped-community seats the same structure operates as a set of costs they cannot decline or renegotiate. The adjudicator seat experiences the settlement as neutral doctrine administration, extending old categories to new assets without revisiting the settlement itself. The excluded advocate seat is identity_locked: exit from the social-doctrine commitment is not a live option, so their opposition is permanent and constitutive rather than strategic. The engine computes per-seat classifications from the structural data; the authored mountain claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: venture investors (arbitrage-grade exit) sit nearest the beneficiary end, followed by laboratories (asset mobility across jurisdictions), founders and technologists (mobile labor and incorporation). Victim declarations drive high directionality: coordination_failure_communities (trapped, generational exposure) sit nearest the full-target end, with monopsony workers and locked-in users close behind. Property_rights_adjudicators sit near symmetric — they administer the settlement without collecting its principal gains. No directionality_overrides were needed: the beneficiary/victim declarations plus exit options already produce the correct ordering, and the excluded advocate seat is commentary-grade by design (an authored absence must not drive classification).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating complex activity without central command — remains live in its general form but is contested for AI specifically, where critics locate the salient threat in private concentration rather than state command. The status-by-verdict pairing (contested x world_rearranges) flags no zombie: the arrangement's function has not atrophied, it actively governs, so the piton path is not in play. The live misclassification risk runs the other way: mountain immunity would launder active, enforced, asymmetrically burdensome coordination as natural law and block contamination analysis entirely. The false-summit evaluation exists for exactly this shape — a naturality claim with a named beneficiary class — and the omega variables carry the naturality question openly rather than prejudging it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_property_rights,
    'Are property rights in models, data, and compute a pre-political natural order (as this reading claims) or a constructed legal convention that identifiable holders benefit from presenting as natural?',
    'Comparative legal-historical analysis of how intangible-asset entitlements emerged and vary across jurisdictions, joined to innovation-outcome data: if entitlement boundaries track drafting and lobbying history rather than any invariant feature, and innovation outcomes track enforcement intensity, the constructed-convention account strengthens.',
    'If constructed, the mountain claim fails and the settlement evaluates as enforced coordination with asymmetric gains; if the naturality claim withstands scrutiny, the mountain classification stands and the declared beneficiaries become anomalous rather than decisive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_property_rights, conceptual, 'Naturality of the property-rights foundation versus the constructed-entitlement account.').

omega_variable(
    monopsony_endogeneity,
    'Are the monopsony labor outcomes and data asymmetries defects external to the settlement (market imperfections the reading disavows) or endogenous products of the settlement''s own dynamics?',
    'Econometric tracing of concentration formation: whether winner-take-most patterns follow from the settlement''s protected features (network effects, data feedback loops, enforceable exclusivity) or from distortions the settlement does not authorize.',
    'If endogenous, the residual costs attributed to ''imperfection'' belong to the arrangement itself and effective extraction rises well above the authored 0.28; if exogenous, the low-epsilon reading survives intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monopsony_endogeneity, empirical, 'Whether the costs borne by weak-bargaining-power seats are internal to the settlement.').

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the ai_governance_legitimacy kernel; how would classification shift under a sibling reading''s framework, and where exactly is the disagreement located?',
    'Family-level comparison across the four reading files: locate the disputed element (source of legitimacy — consented exchange, doctrinal conformity, expert optimization, or democratic deliberation) and observe which structural facts each reading counts as costs imposed on unwilling parties.',
    'Under the magisterial reading the same terrain shows different victims (those harmed by unaccountable deployment) and higher measured burden; under the technocratic reading ethical costs appear as optimization trade-offs. Cross-reading verdicts are indexical; only the linked family supports comparison.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, preference, 'Committer-frame indexicality: one reading of a four-reading kernel.').

omega_variable(
    mandate_suppression_mechanism,
    'Is the suppression of collective mandates over AI structural (arbitration clauses, preemption, commercial-speech protections) or internalized (developers and publics treating mandates as presumptively illegitimate)?',
    'Post-adoption trajectories in jurisdictions that enacted binding AI obligations: if organized resistance persists after legal barriers fall, the internalized component is confirmed; if compliance normalizes quickly, suppression was mostly structural.',
    'If substantially internalized, effective suppression exceeds the structural measure and persists even where enforcement recedes — raising the cost of any transition away from the settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_suppression_mechanism, empirical, 'Structural versus internalized suppression of mandate-based alternatives.').

omega_variable(
    subsidiarity_selective_adoption,
    'The reading adopts subsidiarity (decentralization) while rejecting solidarity (binding common-good obligations): is the selective adoption stable, or does subsidiarity''s logic within Catholic Social Doctrine commit its holder to solidarity''s premises?',
    'Doctrinal analysis within the social-doctrine tradition, which treats the principles as inseparable; observe whether the reading''s decentralization claims survive contact with the tradition''s own authoritative interpretation.',
    'If inseparable, the reading''s partial endorsement is unstable: it drifts toward either the full magisterial sibling or toward abandoning the decentralization argument, changing which coordination claims it can legitimately make.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidiarity_selective_adoption, conceptual, 'Stability of the reading''s selective adoption of the encyclical''s principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_gov_mktlib_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(ai_gov_mktlib_tr_t0, observed).
narrative_ontology:measurement(ai_gov_mktlib_tr_t4, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement_basis(ai_gov_mktlib_tr_t4, observed).
narrative_ontology:measurement(ai_gov_mktlib_tr_t8, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement_basis(ai_gov_mktlib_tr_t8, observed).
narrative_ontology:measurement(ai_gov_mktlib_tr_t12, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement_basis(ai_gov_mktlib_tr_t12, observed).
narrative_ontology:measurement(ai_gov_mktlib_tr_t16, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement_basis(ai_gov_mktlib_tr_t16, observed).
narrative_ontology:measurement(ai_gov_mktlib_tr_t20, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(ai_gov_mktlib_tr_t20, observed).
narrative_ontology:measurement(ai_gov_mktlib_tr_t24, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(ai_gov_mktlib_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(ai_gov_mktlib_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(ai_gov_mktlib_be_t0, observed).
narrative_ontology:measurement(ai_gov_mktlib_be_t4, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement_basis(ai_gov_mktlib_be_t4, observed).
narrative_ontology:measurement(ai_gov_mktlib_be_t8, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement_basis(ai_gov_mktlib_be_t8, observed).
narrative_ontology:measurement(ai_gov_mktlib_be_t12, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement_basis(ai_gov_mktlib_be_t12, observed).
narrative_ontology:measurement(ai_gov_mktlib_be_t16, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement_basis(ai_gov_mktlib_be_t16, observed).
narrative_ontology:measurement(ai_gov_mktlib_be_t20, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement_basis(ai_gov_mktlib_be_t20, observed).
narrative_ontology:measurement(ai_gov_mktlib_be_t24, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 24, 0.28).
narrative_ontology:measurement_basis(ai_gov_mktlib_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_gov_mktlib_su_t0, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(ai_gov_mktlib_su_t0, observed).
narrative_ontology:measurement(ai_gov_mktlib_su_t4, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement_basis(ai_gov_mktlib_su_t4, observed).
narrative_ontology:measurement(ai_gov_mktlib_su_t8, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement_basis(ai_gov_mktlib_su_t8, observed).
narrative_ontology:measurement(ai_gov_mktlib_su_t12, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement_basis(ai_gov_mktlib_su_t12, observed).
narrative_ontology:measurement(ai_gov_mktlib_su_t16, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement_basis(ai_gov_mktlib_su_t16, observed).
narrative_ontology:measurement(ai_gov_mktlib_su_t20, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement_basis(ai_gov_mktlib_su_t20, observed).
narrative_ontology:measurement(ai_gov_mktlib_su_t24, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement_basis(ai_gov_mktlib_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__democratic_pluralist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'AI governance legitimacy' decomposes into four structurally distinct constraints — one per reading of the kernel — each with its own epsilon, beneficiary/victim structure, and classification. This file is the market_libertarian member: low authored epsilon over the market settlement as referent, with the naturality claim carried as a live omega rather than settled. The upstream/downstream structure runs through shared terrain: this reading's laissez-faire operation concentrates AI capability, which reshapes the operating environment of the technocratic sibling (influences edge), while the magisterial and democratic_pluralist siblings remain live rival frameworks held by other parties (coexistence edges). All four files link one another via affects_constraints; no single file's epsilon is comparable to another's without the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
