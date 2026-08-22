% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Derivative Work Statutory Boundary (Enclosure Reading)
 *   domain: intellectual_property/information_economics
 *
 * SUMMARY:
 *   The enclosure reading of the derivative-work statutory boundary treats
 *   any incorporation of copyrighted expression in a new work as prima facie
 *   preparation of a derivative work, requiring upstream authorization from
 *   the copyright holder. This reading maximizes the gate-control function:
 *   downstream creators cannot proceed with creation that involves
 *   copyrighted expression without clearing rights first. The constraint is
 *   presented as protecting the creative incentive; it operates as enforced
 *   bottlenecking that flows licensing rents to incumbents. This is one of
 *   three readings of the contested kernel
 *   'derivative_work_statutory_boundary'; the others are the coordination
 *   reading (only fixed, substantial recastings are derivative) and the
 *   hybrid carveout reading (commercial uses require authorization,
 *   non-commercial transformative use is permitted). The enclosure reading
 *   stakes the highest extraction and suppression; its persistence depends on
 *   active enforcement of the broad 'any use' scope against competing
 *   doctrines and exemption claims.
 *
 * KEY AGENTS:
 *   - Incumbent rights holders: benefit from the broad derivative-work gate; control licensing terms; extract rents from downstream use
 *   - Downstream creators: pay licensing costs and bear legal risk; creative work is constrained by pre-authorization requirement
 *   - Transformative innovators: powerful commercial actors (remix producers, samplers, AI trainers) whose business models depend on reuse; constrained by licensing obligation despite resource capacity
 *   - Research communities: identity-locked (research identity bound to citation and reuse); face licensing barriers to non-commercial scholarship and analysis
 *   - Copyright enforcement institutions: courts and Copyright Office administer the gate-control mechanism
 *   - Excluded alternative regimes: copyleft licenses, open-access frameworks, fair-use expansions that would permit derivative use without pre-authorization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.82).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.76).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Derivative Work Statutory Boundary (Enclosure Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, '43b9a098-054e-45e2-bb14-80881616554b').
narrative_ontology:cs_kernel_codification('43b9a098-054e-45e2-bb14-80881616554b', fixed_text).
narrative_ontology:cs_authority_grounding('43b9a098-054e-45e2-bb14-80881616554b', extraction).
narrative_ontology:cs_interpretation_layer_present('43b9a098-054e-45e2-bb14-80881616554b').
narrative_ontology:cs_reading_relation('43b9a098-054e-45e2-bb14-80881616554b', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('43b9a098-054e-45e2-bb14-80881616554b', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('43b9a098-054e-45e2-bb14-80881616554b', foundational, any_incorporation_triggers_derivative_status).
narrative_ontology:cs_axiom_status(any_incorporation_triggers_derivative_status, holdable).
narrative_ontology:cs_axiom_grounding('43b9a098-054e-45e2-bb14-80881616554b', any_incorporation_triggers_derivative_status, empirically_contingent).
narrative_ontology:cs_axiom('43b9a098-054e-45e2-bb14-80881616554b', secondary, licensing_gate_incentivizes_original_creation).
narrative_ontology:cs_axiom_status(licensing_gate_incentivizes_original_creation, holdable).
narrative_ontology:cs_axiom_grounding('43b9a098-054e-45e2-bb14-80881616554b', licensing_gate_incentivizes_original_creation, instrumental).
narrative_ontology:cs_reference_frame('43b9a098-054e-45e2-bb14-80881616554b', comprehensive_licensing_gate).
narrative_ontology:cs_drift_state('43b9a098-054e-45e2-bb14-80881616554b', post_digital_technology_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('43b9a098-054e-45e2-bb14-80881616554b', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, downstream_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, transformative_innovators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, research_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, users_and_audiences).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, users_and_audiences).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Copyright owners who hold the original expression and benefit from broad derivative work definitions. They control licensing terms and can deny, delay, or monetize permission for any use of their expression in downstream work. The enclosure reading amplifies their gate-control: even uses that do not substantially replicate original expression still require authorization if any copyrighted expression informed the new work. They directly benefit from the licensing rents and enforcement machinery.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Artists, musicians, authors, and software developers who build on existing work. The enclosure reading requires them to clear rights before creation if any copyrighted expression informs their work, even if the final product is substantially transformative. They bear licensing costs, negotiation delays, and legal risk. Options are licensing (often expensive or withheld), rewriting to avoid the expression (costly and creatively constrained), or proceeding at legal risk. The constrained exit is binding: their creative field sits on a foundation of prior work they cannot fully avoid.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, downstream_creators, payer,
    moderate, biographical, constrained, global).

% Companies and creators whose business models depend on remixing, adapting, or recasting prior expression (video remix producers, music samplers, app developers, game modders, AI training datasets). The enclosure reading creates pre-creation licensing obligations that gate their innovation. Even when transformation is substantial and commercial benefit is genuine, they cannot proceed without permission. Unlike downstream creators they have resources to navigate licensing but still face extraction and delay. Their powerful position is asymmetrically constrained by the rights-holder's control.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, transformative_innovators, payer,
    powerful, biographical, constrained, global).

% Academic and scientific researchers whose work depends on analyzing, mining, and building on existing published expression (computational linguistics, literary analysis, drug discovery from patent databases, machine learning training). The enclosure reading requires permission even for uses that are non-commercial and transformative. Their identity as researchers is bound to citation and reuse; exiting the copyright system entirely is not available. The enforcement machinery increasingly flags research reuse as unlicensed derivative work preparation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, research_communities, payer,
    organized, generational, identity_locked, global).

% End consumers who benefit from access to original licensed works (films, music, books, software). The enclosure reading indirectly affects them by slowing downstream innovation and raising its price — transformative works take longer to produce and cost more when licensing is required. They pay for access to original works and indirectly for the upstream licensing delays that increase the cost of subsequent adaptations.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, users_and_audiences, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, users_and_audiences, payer).

% Courts, Copyright Office, and enforcement agencies that interpret the statutory boundary and police compliance. The enclosure reading expands their enforcement load: more uses fall under the derivative work definition, requiring more clearance decisions and litigation. They administer the gate-control mechanism the constraint creates.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, copyright_enforcement_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Regulatory frameworks (copyleft licenses, open access mandates, fair use expansions, remix rights doctrines) that would permit downstream use without pre-authorization. They are structurally excluded by the enclosure reading's pre-creation licensing requirement. If admitted, they would compete on transaction cost and speed, lowering extraction rents.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, excluded_alternative_regimes, excluded,
    powerful, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a gate-control mechanism for derivative works: a designated authority (copyright owner) reviews and authorizes downstream uses of original expression, protecting the creative financial incentive and ensuring derivative works maintain fidelity to the original.
% TRANSFER_FUNCTION: Moves licensing revenue, control over derivative terms, and strategic time-to-market advantage from downstream creators to incumbent rights holders. The enclosure reading maximizes this transfer by treating any incorporation of copyrighted expression as triggering the licensing requirement, even when the downstream work is substantially transformative.
% ABSENT_VOICES: Downstream creators and transformative innovators are partially excluded in practice: while they have legal standing to negotiate licensing, the enclosure reading structurally silences remix communities, folk-tradition continuations, and research reuse that operates outside commercial licensing. The excluded alternative framework (copyleft, open-access, fair-use expansion) is kept off the table by the reading's statutory scope.
% DISAPPEARANCE_RATIONALE: If the enclosure reading disappeared and were replaced by the coordination reading (narrow scope: only fixed recastings are derivative), downstream creators could proceed with transformative work without pre-authorization; licensing rents would compress to true coordination costs; remix culture, research-driven innovation, and adaptive creation would accelerate. The rights-holder revenue stream would shift from gate-control to actual licensing for high-commercial-value derivations. The creative economy would reorganize around lower transaction costs and faster iteration.
% FOUNDING_PROBLEM: Unauthorized reproduction and commercial exploitation of copyrighted works: creators' incentive to produce original expression was undermined when copies could be made and sold without authorization or compensation to the creator.
% FOUNDING_PROBLEM_CORROBORATION: The copyright system's incentive function is widely attested from economic theory and publishing history outside the rights-holder community. However, the scope question — whether ANY use of copyrighted expression in a new work is a 'derivative work' requiring authorization, or only SUBSTANTIAL recastings — is contested. The foundational problem (incentivizing original creation) is live; the enclosure reading of its solution is not corroborated by technology policy analysts, transformative-use research, or fair-use doctrine advocates, who argue the original problem is solved without such broad scope.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__enclosure_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) and rising because the enclosure reading applies the licensing requirement to an expanding range of downstream uses — as technology enables more remixing and reuse, more activities fall under the derivative-work category and trigger licensing obligations. The measurement series shows steady accumulation from 0.58 to 0.82 over 25 years, reflecting the empirical expansion of what courts treat as derivative-work preparation (computational reuse, AI training, remix flagged as infringing preparation). Suppression is comparably high (0.76 at end) and rising because the enforcement machinery must actively exclude alternative frameworks (copyleft, fair use, research exemptions) to maintain the broad scope; the constraint persists only through ongoing legal and institutional suppression of competing interpretations. Theater ratio (0.42 at end) is moderate: the incentive-protection narrative is real and widely cited, but an increasing fraction of enforcement activity (the rising suppression trajectory) is dedicated to blocking competing doctrines rather than addressing the founding problem (incentivizing original creation). The widening gap between stated coordination function (incentive protection) and actual enforcement (blocking alternatives, expanding scope) is the theater accumulation. Accessibility collapse is moderate (0.68) because downstream creators retain alternative paths (licensing negotiation, legal challenges, rewriting to avoid the expression) but all are costlier than proceeding without the constraint. Resistance is high (0.71) because research communities, remix practitioners, and fair-use advocates actively contest the broad scope through litigation, statutory amendment proposals, and doctrine development.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent rights-holder seat, the enclosure reading is genuine coordination protecting the creative financial incentive; enforcement is necessary to prevent dilution of the rights holder's stake. From the downstream-creator seat, the constraint is enforced extraction: the licensing gate is maintained not to serve the founding problem (incentivizing the incumbent) but to extract rents from subsequent creation. From the research-community seat, the enclosure reading is identity-oppressive: research cannot exit the constraint without ceasing to be research (non-citations would be a betrayal of intellectual integrity). The engine computes per-seat classification from the structural data; the divergence is the measurement the corpus takes — this story's claim (snare) and metrics (high extraction, high suppression, rising theater) align, and the per-seat divergence will be evident in the compiled .pl analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent rights holders occupy the beneficiary seat with maximum directionality toward the constraint (d near 1.0 → full target inverted: they are full beneficiaries, d near 0.0). They set the agenda, control licensing terms, and directly collect licensing revenue; their power is institutional and their exit options approach arbitrage (they can shift between different rights-monetization strategies). Downstream creators and transformative innovators occupy the target seats (d near 1.0): they bear licensing costs, legal risk, and creative constraint; their exit is constrained (leaving creative fields dependent on prior work is not available). Research communities are especially identity-locked targets: their professional identity is bound to citation and reuse; exiting the constraint by reframing as non-researchers is not available. The enforcement institutions are secondary agenda-setters (they administer the gate) but do not directly benefit — they are not listed as beneficiaries; their directionality is intermediate. Excluded alternative regimes (copyleft, fair use) are not agents — they are institutional framings kept off the table; they would reduce extraction if admitted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (incentivizing original creation via copyright) is live, but the enclosure reading's solution (requiring pre-authorization for any use of copyrighted expression) operates as enclosure that outlives coordination necessity. The core coordination problem — ensuring original creators receive compensation — could be satisfied by a narrower gate (authorization required only for competitive recastings, not for transformative uses) or by research/fair-use exemptions. The enclosure reading persists not because the narrower scope would fail to incentivize but because the broader scope generates greater extraction rents. Mandatrophy is not (yet) fully resolved because the founding problem is not demonstrably dead (research communities, downstream creators still invoke incentive logic in their arguments), but the rising theater and suppression trajectory (blocking alternatives, expanding scope) indicates the constraint is increasingly maintained by enforcement of its breadth rather than by participant preference for its coordination function. A mandatrophy event would occur if technology (decentralized licensing, blockchain-based derivative registration) or doctrine (codification of a transformative-use exemption) enabled the founding problem to be solved at lower extraction cost, shifting the constraint from beneficial to purely performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantial_similarity_vs_any_use,
    'Is the enclosure reading''s ''any use'' standard structurally coherent with traditional copyright doctrine''s ''substantial similarity'' or ''fixed expression'' tests, or does it represent a departure?',
    'Historical analysis of statutory intent and case law precedent: does the legislative record or judicial tradition support ''any incorporation'' as the standard, or do precedents cluster around ''substantial recastings''? Textual analysis of ''derivative work'' definition across jurisdictions.',
    'If ''any use'' departs from doctrine tradition, the reading is a contemporary expansion — a shift in what ''derivative work'' means within the same kernel, not a discovery of its historical meaning. If ''any use'' aligns with textual intent, the reading is faithful to the statute but the statute itself is contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantial_similarity_vs_any_use, empirical, 'Whether the enclosure reading aligns with or departs from the historical scope of ''derivative work'' doctrine.').

omega_variable(
    licensing_transaction_cost_vs_incentive_function,
    'Does requiring pre-authorization licensing for any use of copyrighted expression in downstream work materially increase the incentive to create original expression, or does it increase extraction rents without marginal incentive effect?',
    'Empirical comparison: downstream creator productivity, innovation velocity, and licensing cost between jurisdictions or time periods with broad derivative-work scope (enclosure) versus narrow scope (coordination). Survey of rights-holder revenue allocation: what fraction of licensing income reflects the value of authorized derivative works versus extraction rent?',
    'If incentive effect is marginal, the extraction exceeds the coordination necessity — the constraint is a snare using coordination cover. If incentive effect is substantial, part of the extraction is the price of maintaining the creative financial incentive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_transaction_cost_vs_incentive_function, empirical, 'Whether the broad licensing gate produces measurable incentive gains or primarily extracts rents.').

omega_variable(
    committer_kernel_framing_ambiguity,
    'Is the contest between the enclosure reading and the coordination reading a genuine doctrinal debate within the same commitment system (copyright law), or does the enclosure reading represent a fundamentally different legitimacy claim?',
    'Examine the axioms the readings invoke: does the enclosure reading claim exclusive authority over creative derivative decisions on the grounds of incentive protection, originalist statutory interpretation, or market control? Does the coordination reading accept the incentive framework but differ only on scope? Or are they grounded in incompatible legitimacy bases?',
    'If the readings share a legitimacy framework and differ only on scope boundaries, they coexist within copyright doctrine. If they invoke different legitimacy bases (incentive vs. innovation velocity vs. market power), the readings are less about statutory interpretation and more about competing regimes — the kernel framing itself is contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_framing_ambiguity, conceptual, 'Whether the enclosure and coordination readings are doctrinal variants or represent different commitment-system framings.').

omega_variable(
    transformation_sufficiency_boundary,
    'At what threshold of creative transformation does a use cease to be derivative-work preparation under the enclosure reading, or does the reading categorically treat all incorporations as derivative regardless of transformation?',
    'Doctrinal analysis: does the enclosure reading include any exception for transformative use, fair use, or substantial original contribution? Enforcement case studies: where have courts applied the enclosure reading and what transformativeness levels have been deemed non-qualifying?',
    'If the enclosure reading is categorical (any use is derivative), it is internally consistent but maximizes extraction. If it includes a transformation threshold, the threshold itself becomes the site of contention and disputes narrow the reading''s scope in practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformation_sufficiency_boundary, empirical, 'Whether the enclosure reading admits any transformation threshold or is categorical.').

omega_variable(
    research_exemption_vs_commercial_boundary,
    'Does the enclosure reading apply equally to commercial and non-commercial downstream uses, or is research/academic reuse given special treatment?',
    'Statutory text and case law: does the derivative work definition distinguish commercial from non-commercial incorporation? Do research communities have a documented exemption or fair-use harbor?',
    'If the enclosure reading applies uniformly, research communities are fully captured as victims. If research has exemption, the reading''s extractiveness is lower for non-commercial seats. The identity-lock effect (research identity makes exit unavailable) is strongest when no exemption exists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(research_exemption_vs_commercial_boundary, empirical, 'Whether the enclosure reading carves out non-commercial or research use, or applies uniformly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(deri_tr_t25, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 5, 0.64).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement(deri_be_t25, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 25, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(deri_su_t25, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 25, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__enclosure_reading, 0.18).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, fair_use_doctrine_scope).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, copyright_research_exemption_boundary).

% DUAL FORMULATION NOTE:
% The derivative-work statutory boundary decomposes into three structurally distinct readings instantiated as separate constraints. The enclosure reading (this story) maximizes extraction and licensing gate-control; the coordination reading narrows scope to substantial recastings and permits transformative use; the hybrid carveout reading permits non-commercial transformative use while maintaining licensing requirement for commercial derivatives. Each reading has a different ε (enclosure: 0.82, coordination: ~0.35, hybrid: ~0.58), different victim sets, different suppression requirements, and different theater patterns. They are not three perspectives on one constraint — they are three constraints rooted in the same kernel commitment. The sibling constraints link backward to this one via affects_constraints to model the family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__enclosure_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
