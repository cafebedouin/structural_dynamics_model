% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__utilitarian_hybrid_reading, []).

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
 *   constraint_id: software_source_status__utilitarian_hybrid_reading
 *   human_readable: Utilitarian Hybrid Reading of Software Source Status (Welfare-Maximizing Mixed Licensing Criterion)
 *   domain: economic/technological/political
 *
 * SUMMARY:
 *   The standing arrangement under contest is the global mixed licensing
 *   ecosystem: open commons dominate infrastructure (kernels, compilers,
 *   internet protocols, foundational libraries) while proprietary licensing
 *   dominates specialized, enterprise, and consumer software — with courts,
 *   procurement bodies, and corporate strategy evaluating the boundary
 *   between them by aggregate-welfare reasoning. This story instantiates the
 *   utilitarian_hybrid_reading of the software_source_status kernel:
 *   licensing arrangements are to be selected and judged by whichever model
 *   maximizes aggregate welfare in the context at hand, with no categorical
 *   answer. Its structural delta against the sibling readings is the absence
 *   of a categorical victim set, context-relative optimization, and explicit
 *   acceptance of mixed ecosystems. Per the epsilon-invariance family rule,
 *   the sibling readings (freedom_imperative_reading,
 *   pragmatic_development_reading, property_rights_reading) are separate
 *   constraint stories with their own epsilon values, victim sets, and
 *   classifications; this file links them via network.affects_constraints.
 *   The claimed type and the metrics are authored independently: the reading
 *   is claimed as rope (a pluralist decision criterion with low coercive
 *   overhead), while the metrics describe the standing mixed arrangement as
 *   this reading itself assesses it — largely endorsed, with documented
 *   pockets of welfare-losing extraction and soft exclusion of non-welfarist
 *   claims.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: Primary contextual beneficiary (powerful/arbitrage) — gains endorsement for exclusivity wherever welfare analysis succeeds
 *   - open_source_foundations: Primary contextual beneficiary (institutional/identity_locked) — gains legitimacy and resourcing for the commons wherever public-goods arguments succeed
 *   - software_end_users: Net beneficiary with diffuse costs (moderate/constrained) — gains welfare-optimized provisioning, pays for analytical failures as lock-in
 *   - independent_developers: Dual-positioned contextual beneficiary/payer (moderate/constrained) — model choice open, non-market motivations discounted
 *   - freedom_imperative_advocates: Excluded voice (organized/identity_locked) — claims admissible only after translation into welfare terms
 *   - competition_authorities: Agenda setter (institutional/analytical) — administers the criterion's institutional embedding in adjudication
 *   - welfare_economists: Analytical observer (analytical/analytical) — supplies the criterion's operating instrument and sees the whole structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.26).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.32).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Utilitarian Hybrid Reading of Software Source Status (Welfare-Maximizing Mixed Licensing Criterion)").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "economic/technological/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, 'ec0feec3-4481-4908-8fd5-ec6879b92646').
narrative_ontology:cs_kernel_codification('ec0feec3-4481-4908-8fd5-ec6879b92646', distributed).
narrative_ontology:cs_authority_grounding('ec0feec3-4481-4908-8fd5-ec6879b92646', expertise).
narrative_ontology:cs_interpretation_layer_present('ec0feec3-4481-4908-8fd5-ec6879b92646').
narrative_ontology:cs_reading_relation('ec0feec3-4481-4908-8fd5-ec6879b92646', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('ec0feec3-4481-4908-8fd5-ec6879b92646', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec0feec3-4481-4908-8fd5-ec6879b92646', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('ec0feec3-4481-4908-8fd5-ec6879b92646', foundational, aggregate_welfare_is_licensing_criterion).
narrative_ontology:cs_axiom_status(aggregate_welfare_is_licensing_criterion, holdable).
narrative_ontology:cs_axiom_grounding('ec0feec3-4481-4908-8fd5-ec6879b92646', aggregate_welfare_is_licensing_criterion, instrumental).
narrative_ontology:cs_axiom('ec0feec3-4481-4908-8fd5-ec6879b92646', foundational, no_categorical_licensing_model).
narrative_ontology:cs_axiom_status(no_categorical_licensing_model, holdable).
narrative_ontology:cs_axiom_grounding('ec0feec3-4481-4908-8fd5-ec6879b92646', no_categorical_licensing_model, empirically_contingent).
narrative_ontology:cs_reference_frame('ec0feec3-4481-4908-8fd5-ec6879b92646', welfare_optimal_mixed_licensing).
narrative_ontology:cs_drift_state('ec0feec3-4481-4908-8fd5-ec6879b92646', contemporary_platform_concentration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ec0feec3-4481-4908-8fd5-ec6879b92646', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, open_source_foundations).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, software_end_users).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, independent_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, software_end_users).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, independent_developers).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, consumer_welfare_standard).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, context_dependent_licensing).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, welfare_economics_methodology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell software under proprietary licenses, recovering R&D through exclusivity, support contracts, and controlled distribution. The welfare criterion gives them a standing obligation to justify exclusivity with aggregate-benefit arguments — innovation incentives, quality, security — and a standing opportunity: where the argument succeeds, their exclusivity is endorsed. Exit is flexible: they can shift product lines between proprietary and open models as the analysis favors, or locate IP holdings in favorable jurisdictions.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, proprietary_software_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Steward shared software infrastructure under open licenses — foundations, maintainer collectives, license-conservancy bodies. The welfare criterion legitimizes their commons wherever network-effect and public-goods arguments succeed, and channels grant, procurement, and corporate-contribution resources toward them. Exit is limited by identity: the stewardship role is constitutive of what these bodies are, and abandoning it would dissolve the organization's purpose rather than relocate it.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, open_source_foundations, beneficiary,
    institutional, generational, identity_locked, global).

% Use software under whichever license the welfare argument endorses in each domain. They gain from welfare-optimized provisioning — open infrastructure where analysis favors it, polished proprietary tools where it does not — and pay where welfare analysis fails to prevent lock-in, price discrimination, or data extraction. Leaving any single product is often costly; leaving the reach of the criterion is not a choice available to them at all.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, software_end_users, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, software_end_users, payer).

% Choose licenses for their own work under the criterion's guidance. They gain a legitimate path to either model — commons contribution or commercial exclusivity — and bear the costs of license-compliance complexity, dual-licensing negotiation, and the discounting of non-market motivations such as autonomy and craft freedom that do not translate cleanly into measurable welfare. Moving between camps is open to them; stepping outside the evaluative framework is not.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, independent_developers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, independent_developers, payer).

% Hold that software freedom is a moral requirement in its own right rather than an input to a welfare calculation. Under a welfare-governed conversation their claims are heard only insofar as they convert into measurable benefits, which they regard as a category error. They organize, publish, and contest procurement and standards processes from the margins; their commitments are constitutive of who they are, so exit from the position is unavailable short of abandoning it.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, freedom_imperative_advocates, excluded,
    organized, generational, identity_locked, global).

% Adjudicate licensing arrangements — mergers, tying, refusals to license, platform exclusivity — under consumer-welfare and total-welfare standards. They administer the criterion's institutional embedding: which arguments count, what evidence satisfies them, which arrangements are challenged. They bear the cost of analytical error and of capture accusations, and can alter the constraint's reach through doctrine and enforcement priorities.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, competition_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Supply the analytical apparatus the criterion runs on — demand estimation, deadweight-loss measurement, innovation-incentive modeling. Their expertise is the operating instrument of the framework; they collect professional standing from its adoption and bear reputational cost when their models mispredict. From this seat the whole structure — who argues, who wins, whose values translate — is visible.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, welfare_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__utilitarian_hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(software_source_status__utilitarian_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, context-relative decision criterion for choosing between proprietary and open licensing across heterogeneous software domains: courts, procurement bodies, firms, and standards groups can evaluate arrangements on common terms instead of by categorical allegiance to either model.
% TRANSFER_FUNCTION: Moves legitimacy and decision weight toward whichever licensing arrangement demonstrates larger aggregate welfare in a given context: exclusivity rents flow to proprietary vendors where innovation-incentive arguments succeed, infrastructure resources flow to open commons where public-goods arguments succeed, and the cost of proof falls on whichever party seeks endorsement of its arrangement.
% ABSENT_VOICES: Deontological software-freedom advocates would object that their claims are admissible only after translation into welfare terms, which they reject as a category error; future users and non-market users whose preferences are hard to aggregate are structurally underweighted; unpaid maintainers who produce much of the commons the welfare calculus counts are rarely seated in the adjudicating processes.
% DISAPPEARANCE_RATIONALE: Antitrust doctrine, public procurement, corporate licensing strategy, and standards negotiation all run on welfare criteria. Overnight removal would force reversion to categorical rights-talk or raw market power as the arbiter: licensing adjudication would fragment into the open-versus-proprietary conflict the criterion was built to adjudicate, and every arrangement currently endorsed on welfare grounds would need re-justification under some other standard.
% FOUNDING_PROBLEM: The categorical licensing conflict — proprietary exclusivity versus software freedom as an absolute — produced stalemates that left both welfare-losing lock-in and welfare-losing fragmentation unadjudicated. Policymakers and courts needed a common metric that could endorse either model where it served aggregate welfare and condemn either where it did not.
% FOUNDING_PROBLEM_CORROBORATION: Competition-authority guidelines and antitrust case law applying consumer-welfare standards, public-procurement frameworks, and the law-and-economics academic literature attest both the founding problem and its live status from outside the vendor and foundation beneficiary sets; open-source foundation position papers corroborate it from inside one beneficiary camp and are therefore not counted as independent attestation.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__utilitarian_hybrid_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__utilitarian_hybrid_reading_tests).
:- end_tests(software_source_status__utilitarian_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.26) because the referent — the standing mixed arrangement — is one this reading largely endorses; the residual extraction it concedes is concentrated in platform-era lock-in and exclusivity regimes that rigorous welfare analysis condemns but practice persists in. Suppression (0.32) is structural rather than coercive: the criterion's institutional embedding determines which arguments are admissible, and non-welfarist claims are excluded by framing rather than by force — suppression here is a raw structural property, unscaled by power or scope, while only extractiveness is scaled in the engine's computation. Theater is low but rising (0.18): as welfare language became the mandatory register of justification, post-hoc rationalization dressed as analysis grew. Accessibility collapse is low (0.30) because the criterion explicitly keeps both models live; resistance is moderate (0.38) from the two camps whose categorical commitments the criterion discounts. The measurement series share one time grid (0/10/20/30/40/50) so every metric is authored at every examined point: extractiveness fell as welfare scrutiny and the open commons spread, then partially re-accumulated with platform concentration; suppression_requirement traces the enforcement story — institutional embedding built up through the consumer-welfare orthodoxy era and partially relaxed under behavioral critique and pluralist pushback — which is why it is tracked rather than left to the scalar. Receipt surface: no named seat captures the constraint's gains standing — legitimacy accrues contextually to whichever model wins the argument in a given domain — hence gain_flow 'diffuse' is an affirmative checked claim, with the resource-asymmetry omega tracking whether that diffusion is real or capture-in-progress. Fixing cost is 'prohibitive' on its own evidence: nothing is broken, and removal would dismantle load-bearing evaluative infrastructure (antitrust doctrine, procurement rules, corporate strategy) for near-zero benefit, so cost is prohibitive relative to benefit.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structural data. From the vendor seat, the criterion is a framework that — more often than not — endorses the arrangements the vendor already prefers, and its justification burden is a manageable cost of doing business. From the user and independent-developer seats, the same framework is a gate whose analytical failures land on them as lock-in and discounted autonomy. From the excluded advocate seat, the framework is a category error that renders their core commitment inaudible. From the agenda-setter seat, it is a workable adjudication standard that converts an unmanageable ideological conflict into a decidable one. Same-level dynamics differentiate the two beneficiary camps: vendors and foundations hold the same declared role but different exit positions — arbitrage for vendors, identity-lock for foundations — so the derivation places them at different distances from the target end despite identical role declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations drive the derivation: proprietary_software_vendors (powerful, arbitrage exit) and open_source_foundations (institutional, identity-locked) derive low directionality — the constraint subsidizes both, in different domains. software_end_users and independent_developers carry overrides (moderate, d=0.45): the derivation from their beneficiary declarations alone would place them near the beneficiary end (~0.2), but both seats bear real payer-side costs — lock-in and price effects for users, compliance complexity and discounted non-market motivations for developers — so the derived d is wrong and the override corrects it. freedom_imperative_advocates carries an override (organized, d=0.60): as an excluded seat with no beneficiary/victim declaration, the canonical fallback would misstate its position; it bears the constraint's soft suppression (its claims must survive welfare translation) while occasionally benefiting when welfare analysis endorses openness. competition_authorities sit near symmetric as administrators who neither collect nor pay substantially; welfare_economists are analytical. Scope is global for most seats, which the engine's scope modifier registers when scaling effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — adjudicating between categorical licensing models — is live: new contexts (SaaS, AI model licensing, platform exclusivity) continuously regenerate it, so no mandatrophy declaration is authored and the R5 mismatch consumer finds status=live with verdict=world_rearranges, no zombie flag. The classification discipline prevents mislabeling in both directions: a tangled_rope or snare reading would fabricate a categorical victim set this reading structurally lacks, while a mountain reading would launder the criterion's constructed, contestable status into natural law. The live risks the corpus should watch are Goodhart drift (welfare language decaying into justification theater — visible in the rising theater_ratio series) and resource-asymmetry capture (welfare analysis systematically favoring resourced parties — carried as an empirical omega whose resolution would force reclassification toward tangled_rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Which reading of the software_source_status kernel should govern licensing evaluation — this utilitarian hybrid, or one of its siblings (freedom imperative, pragmatic development, property rights)?',
    'Not resolvable by data alone: the disagreement is located conceptually in whether software freedom is commensurable with welfare, and in preference over which value governs when they conflict. Each sibling is authored as its own constraint story; cross-reading comparison of computed classifications is the resolution path the corpus supports.',
    'The freedom_imperative_reading would create a categorical victim set (proprietary licensors as rights-violators) and raise epsilon for the standing arrangement sharply; the property_rights_reading would raise epsilon for users and legitimize exclusivity unconditionally; the pragmatic_development_reading would narrow the criterion to development-quality outcomes. This story''s rope classification, contextual victim structure, and low-moderate epsilon hold only within this reading''s own framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this constraint is one reading of a four-reading kernel; sibling readings change the victim set and epsilon.').

omega_variable(
    welfare_analysis_resource_asymmetry,
    'Does the welfare criterion systematically favor parties who can afford economic analysis, converting a pluralist standard into a de facto instrument of well-resourced proprietary vendors?',
    'Audit licensing disputes, procurement challenges, and standards fights: compare win rates and settlement terms by party analytical resources, controlling for underlying merit.',
    'If the asymmetry is real, a categorical victim set emerges (under-resourced communities and users whose interests are systematically underweighted), the coordination function becomes cover for resource-based capture, and the classification drifts toward tangled_rope. If analysis is resource-neutral in outcomes, the pluralist rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_analysis_resource_asymmetry, empirical, 'Whether welfare analysis is resource-neutral or structurally favors resourced parties.').

omega_variable(
    freedom_welfare_incommensurability,
    'Can software-user freedom be fully translated into welfare terms (contingent valuation, revealed preference), or does welfare aggregation structurally discount incommensurable values?',
    'Contingent-valuation and choice-experiment studies of user-freedom valuations, combined with philosophical analysis of commensurability; observe whether freedom-advocate claims survive translation into welfare language without residue.',
    'If freedom is incommensurable, the authored suppression metric understates real suppression — the criterion excludes dissenting value-claims structurally rather than weighing them — and the reading''s legitimacy claim against the freedom reading weakens correspondingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(freedom_welfare_incommensurability, conceptual, 'Whether the welfare criterion can absorb freedom claims without residue.').

omega_variable(
    platform_lockin_diagnosis,
    'Is contemporary platform lock-in and exclusivity — app-store commissions, EULA regimes, refusals to interoperate — a welfare-losing deviation this reading condemns and would correct if rigorously applied, or an outcome the reading''s own framework licenses?',
    'Apply the criterion rigorously to flagship platform cases with published welfare models, and observe whether the reading''s own adherents condemn or defend the arrangements in adjudication and commentary.',
    'If the framework licenses lock-in (innovation-incentive arguments reliably succeeding), epsilon for the standing arrangement rises and the classification drifts toward tangled_rope; if the framework condemns lock-in, the observed extraction is a failure of application rather than an output of the reading, and the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_lockin_diagnosis, empirical, 'Whether platform-era extraction is application failure or framework output.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(utilitarian_hybrid_reading_tr_t0, software_source_status__utilitarian_hybrid_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_tr_t0, observed).
narrative_ontology:measurement(utilitarian_hybrid_reading_tr_t10, software_source_status__utilitarian_hybrid_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_tr_t10, observed).
narrative_ontology:measurement(utilitarian_hybrid_reading_tr_t20, software_source_status__utilitarian_hybrid_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_tr_t20, observed).
narrative_ontology:measurement(utilitarian_hybrid_reading_tr_t30, software_source_status__utilitarian_hybrid_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_tr_t30, observed).
narrative_ontology:measurement(utilitarian_hybrid_reading_tr_t40, software_source_status__utilitarian_hybrid_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_tr_t40, observed).
narrative_ontology:measurement(utilitarian_hybrid_reading_tr_t50, software_source_status__utilitarian_hybrid_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(utilitarian_hybrid_reading_be_t0, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_be_t0, observed).
narrative_ontology:measurement(utilitarian_hybrid_reading_be_t10, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_be_t10, observed).
narrative_ontology:measurement(utilitarian_hybrid_reading_be_t20, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_be_t20, observed).
narrative_ontology:measurement(utilitarian_hybrid_reading_be_t30, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_be_t30, observed).
narrative_ontology:measurement(utilitarian_hybrid_reading_be_t40, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 40, 0.27).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_be_t40, observed).
narrative_ontology:measurement(utilitarian_hybrid_reading_be_t50, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 50, 0.26).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(utilitarian_hybrid_reading_su_t0, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_su_t0, observed).
narrative_ontology:measurement(utilitarian_hybrid_reading_su_t10, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_su_t10, observed).
narrative_ontology:measurement(utilitarian_hybrid_reading_su_t20, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 20, 0.26).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_su_t20, observed).
narrative_ontology:measurement(utilitarian_hybrid_reading_su_t30, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 30, 0.34).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_su_t30, observed).
narrative_ontology:measurement(utilitarian_hybrid_reading_su_t40, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 40, 0.36).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_su_t40, observed).
narrative_ontology:measurement(utilitarian_hybrid_reading_su_t50, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 50, 0.32).
narrative_ontology:measurement_basis(utilitarian_hybrid_reading_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__property_rights_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'software source status' decomposes into four structurally distinct constraint stories per the epsilon-invariance principle: this utilitarian hybrid reading (welfare criterion, no categorical victims, epsilon 0.26 over the standing mixed arrangement), the freedom imperative reading (categorical deontological victims, high epsilon), the pragmatic development reading (instrumental quality criterion), and the property rights reading (creator entitlement, epsilon concentrated on users). They form one constraint family linked by affects_constraints; the freedom imperative reading is upstream in rhetorical influence (its framing supplies the moral stakes the other readings respond to), while this reading is upstream institutionally (its criterion supplies the adjudicative standard courts and agencies actually apply).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_source_status__utilitarian_hybrid_reading, moderate, 0.45).
constraint_indexing:directionality_override(software_source_status__utilitarian_hybrid_reading, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
