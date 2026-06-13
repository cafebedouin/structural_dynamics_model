% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__coordination_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Transformative Use Coordination: Derivative Work Boundary (Coordination Reading)
 *   domain: intellectual_property/information_governance
 *
 * SUMMARY:
 *   The derivative work statutory boundary in copyright law defines which
 *   uses of copyrighted expression require permission from the copyright
 *   holder. This is ONE READING of a contested kernel — the statutory
 *   definition of 'derivative work.' Under the COORDINATION READING (this
 *   constraint), a use counts as derivative (infringing) only if it
 *   substantially preserves and republishes original expression; uses that
 *   meaningfully transform purpose, meaning, or form (criticism, parody,
 *   translation, training, new narrative) fall outside the derivative work
 *   scope and are presumptively non-infringing. This reading interprets
 *   copyright as coordinating between creator freedom and copyright-holder
 *   control, rather than as enclosure. Two sibling readings
 *   (enclosure_reading, hybrid_carveout_reading) interpret the same statutory
 *   text differently, producing different beneficiary/victim structures and
 *   extraction profiles.
 *
 * KEY AGENTS:
 *   - original_copyright_holders: Institutional, powerful, bear the constraint through loss of licensing revenue for transformative uses.
 *   - transformative_creators: Moderate power, mobile exit, benefit from freedom to reinterpret and critique without licensing.
 *   - generative_technology_ecosystem: Organized institutional actors, benefit from training-data intermediate-use immunity.
 *   - research_communities: Organized, benefit from scholarly fair use.
 *   - courts: Institutional agenda-setters, interpret the boundary through case-by-case analysis.
 *   - licensing_ecosystem: Powerful, benefit when boundaries are clear but harmed when broad transformative use reduces licensing revenue.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.28).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.15).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Transformative Use Coordination: Derivative Work Boundary (Coordination Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property/information_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, '4e786c89-1b59-41b8-a6a6-2818af7e3e13').
narrative_ontology:cs_kernel_codification('4e786c89-1b59-41b8-a6a6-2818af7e3e13', fixed_text).
narrative_ontology:cs_authority_grounding('4e786c89-1b59-41b8-a6a6-2818af7e3e13', lineage).
narrative_ontology:cs_interpretation_layer_present('4e786c89-1b59-41b8-a6a6-2818af7e3e13').
narrative_ontology:cs_reading_relation('4e786c89-1b59-41b8-a6a6-2818af7e3e13', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('4e786c89-1b59-41b8-a6a6-2818af7e3e13', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('4e786c89-1b59-41b8-a6a6-2818af7e3e13', foundational, transformative_use_non_infringing).
narrative_ontology:cs_axiom_status(transformative_use_non_infringing, holdable).
narrative_ontology:cs_axiom_grounding('4e786c89-1b59-41b8-a6a6-2818af7e3e13', transformative_use_non_infringing, deontological).
narrative_ontology:cs_axiom('4e786c89-1b59-41b8-a6a6-2818af7e3e13', foundational, copyright_enables_not_restricts_culture).
narrative_ontology:cs_axiom_status(copyright_enables_not_restricts_culture, holdable).
narrative_ontology:cs_axiom_grounding('4e786c89-1b59-41b8-a6a6-2818af7e3e13', copyright_enables_not_restricts_culture, instrumental).
narrative_ontology:cs_reference_frame('4e786c89-1b59-41b8-a6a6-2818af7e3e13', statutory_derivative_work_definition_transformative_interpretation).
narrative_ontology:cs_drift_state('4e786c89-1b59-41b8-a6a6-2818af7e3e13', contemporary_generative_ai_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4e786c89-1b59-41b8-a6a6-2818af7e3e13', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, generative_technology_ecosystem).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, research_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, small_independent_publishers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, licensing_ecosystem).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, original_copyright_holders).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, small_independent_publishers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, licensing_ecosystem).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold statutory copyright in original expression. Under the coordination reading, they retain exclusive licensing rights only for non-transformative uses that preserve and republish substantial original expression. For transformative uses (criticism, parody, new context, remix, training data), they collect no licensing revenue. Their constraint is the court interpretation that narrows which uses require licensing. Their exit is legislative advocacy to shift to the enclosure reading.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, original_copyright_holders, payer,
    institutional, generational, constrained, global).

% Create derivative works (parodies, critical commentary, translations, fan works, remixes, adaptations, new narratives using original materials). Under this reading, they may use copyrighted expression freely if sufficiently transformative. They benefit from broad interpretation of 'transformative' and bear litigation risk if courts narrow it.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, transformative_creators, beneficiary,
    moderate, biographical, mobile, global).

% Machine learning training, generative AI, and deployers ingest large corpora without ex-ante licenses, categorizing training as non-infringing intermediate use under the coordination reading. They benefit from inclusive transformation doctrine and bear risk from court decisions that reclassify training as derivative work preparation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, generative_technology_ecosystem, beneficiary,
    organized, biographical, mobile, global).

% Academic, scientific, and digital humanities researchers analyze and excerpt copyrighted materials for scholarship without licensing. Under this reading, research use is transformative and non-infringing. They benefit from capacious transformation doctrine.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, research_communities, beneficiary,
    organized, generational, mobile, global).

% Create works incorporating copyrighted expression (translations, adaptations, new editions, illustrated versions, audiobook adaptations). They benefit from transformation framework if their work adds substantial new expression. They also face licensing negotiations where copyright holders dispute their transformative claim.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, small_independent_publishers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__coordination_reading, small_independent_publishers, payer).

% Interpret derivative work in statutory language and common law. Under this reading, courts administer the boundary by deciding which uses are sufficiently transformative to fall outside copyright scope. They set the agenda through case-by-case fair-use analysis.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, courts_and_judges, agenda_setter,
    institutional, generational, analytical, national).

% Publishers, rights management organizations, and licensing platforms benefit when boundaries are clear and licensing is required. They are harmed when broad transformative use reduces their licensing revenue. Under this reading, their business is smaller because transformative uses are free.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, licensing_ecosystem, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__coordination_reading, licensing_ecosystem, payer).

% Enacted the Copyright Act and could amend the statutory definition. They observe court interpretation and resulting innovation/licensing/litigation patterns. They face pressure from both copyright holders (narrowing the boundary) and creators/technologists (preserving freedom).
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, legislative_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates between copyright holders' interest in controlling derivative works and creators' interest in freedom to transform, quote, critique, and build on existing expression. By establishing 'transformative use' as the boundary, it enables creative and technological ecosystems to operate without ex-ante licensing while preserving copyright holders' power over direct republication.
% TRANSFER_FUNCTION: Moves licensing revenue only for non-transformative uses; transformative creators and technologists retain freedom to use copyrighted expression without payment. The gain flows diffusely across the creative ecosystem (creators, platforms, researchers) rather than concentrating with copyright holders.
% ABSENT_VOICES: Copyright holders in sectors where transformative use is highest (academic publishing, machine learning) are present in litigation but have less power in legislative processes. Open-source communities and non-commercial creators who benefit most are partially excluded from formal copyright debate. Generative AI companies are newest and have no historical voice in copyright doctrine development.
% DISAPPEARANCE_RATIONALE: If the coordination reading disappeared and all derivative work preparation required licensing, creators would face licensing requirements for parody, criticism, translation, scholarly quotation, machine learning training, remix, and fan works. The creative and technology ecosystems would shrink; licensing would become an upstream bottleneck; research and open-source communities would fragment.
% FOUNDING_PROBLEM: Early copyright doctrine treated derivative works narrowly. As digital technology enabled cheap copying and remixing, courts faced a problem: does copyright cover all uses of expression, or only those that substitute for the original? The transformative-use doctrine emerged to distinguish uses that compete with the original market from uses that create new markets and add social value.
% FOUNDING_PROBLEM_CORROBORATION: Courts (Second Circuit, Supreme Court in Google Books, Campbell v. Acuff-Rose, Harper & Row v. Nation) affirm the problem remains live: digital technologies produce new transformative use cases (AI training, remixing, archival digitization) that courts must categorize. Copyright holders argue the doctrine is too broad; technologists affirm it is under threat. Independent economic analysis (Benkler, Lessig, Posner) from outside copyright-holder constituencies confirms the coordination function is contested and consequential.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__coordination_reading_tests).
:- end_tests(derivative_work_statutory_boundary__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.28) because the coordination reading produces minimal licensing revenue capture: copyright holders retain revenue only for non-transformative republication, while transformative uses (the fastest-growing category in digital media) are free. Suppression is VERY LOW (0.15) because the constraint does not require coercive enforcement — courts apply it retrospectively through fair-use analysis, and the constraint enables many actors to operate without licensing negotiations. Theater ratio is minimal (0.12) because the transformation boundary is functionally real: courts engage substantive analysis of whether new work meaningfully differs from original in purpose/meaning/form. The measurement series shows slight upward drift in extractiveness (0.18→0.28) as digital platforms (social media, YouTube, TikTok) have generated more transformative uses and as AI training has forced courts to clarify whether machine learning is transformative. The suppression stays flat because no institutional machinery actively suppresses transformative creators — the constraint operates through the threat of licensing liability, which is low for clearly transformative uses.
 *
 * PERSPECTIVAL GAP:
 *   From the copyright-holder seat, the constraint is a limitation on their exclusive rights — they see a coordination that is too generous to creators. From the transformative-creator seat, the constraint is essential permission — they see a coordinate that enables cultural production without licensing gatekeeping. From the court seat, the constraint is a working rule: courts must apply transformation analysis and will see the constraint as workable when cases produce consistent verdicts, brittle when courts divide on what counts as transformative. The engine computes these perspectives as different per-seat directionalities (d_copyholder >> d_creator) derived from who benefits and who bears costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders sit at d~0.65-0.75 (targeted): they bear the extraction (loss of licensing revenue) and the constraint is enforced against them (courts rule in favor of transformative uses, limiting their exclusive rights). Transformative creators sit at d~0.15-0.25 (beneficiaries): they benefit from free use without licensing negotiation. The licensing ecosystem sits at d~0.5-0.6 (weakly targeted): they benefit from clear boundaries but are harmed by the breadth of transformative use. Courts sit at d~0.5 (symmetric): they must balance both interests. Beneficiary/victim declarations emerge from structural analysis: copyright holders are victims (lose licensing revenue), creators/technologists are beneficiaries (gain creative freedom).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing derivative works that substitute for the original from transformative uses that add value) remains LIVE. Courts continue to face new transformative-use cases: AI training data (2020s), UGC remixing, derivative audiobooks, AI-generated content trained on copyrighted works. The constraint does not exhibit mandatrophy because the transformation boundary is regularly applied and refined by courts — it is not performing for performance's sake. However, there is a READING CONTEST: the enclosure reading interprets the same statute to include all derivative-work preparation (even transformative uses require licensing), while the hybrid reading splits commercial from non-commercial. The coordination reading persists because (a) it is entrenched in U.S. case law (Campbell v. Acuff-Rose, Google Books), (b) creator/technology constituencies are powerful enough to resist narrowing, and (c) the transformation analysis is functionally administrable by courts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_criterion_ambiguity,
    'What empirical and legal criteria distinguish ''transformative'' from ''substantially incorporating original''? Where is the boundary?',
    'Case-law accumulation and legislative guidance. The fair-use doctrine has produced thousands of cases; consistent patterns in transformation analysis would resolve this. Alternatively, statutory amendment could define transformation more precisely.',
    'If transformation is clearly defined and narrow (only parody, criticism, scholarship), extractiveness rises and the constraint drifts toward the enclosure reading. If transformation is broadly interpreted (includes AI training, remixing, new-form adaptations, commercial parody), extractiveness stays low and the coordination reading holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformation_criterion_ambiguity, empirical, 'Operational definition of transformative use varies across courts and cases.').

omega_variable(
    machine_learning_as_intermediate_use,
    'Does machine learning training on copyrighted works constitute preparation of a derivative work (requiring license), or is it intermediate use that produces transformative outputs (non-infringing)?',
    'Appellate decisions in pending cases (Authors Guild v. Google, Sarah Silverman v. OpenAI, and similar). The Supreme Court or Congress could clarify whether statistical inference from copyrighted data is transformative use or derivative work preparation.',
    'This is the live contested case that will determine whether the coordination reading persists. If courts rule that AI training is transformative because it produces qualitatively new outputs (language models, image generators), the coordination reading strengthens. If courts rule that training is derivative work preparation because it uses substantial quantities of original expression, the constraint drifts toward the enclosure reading and licensing becomes mandatory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(machine_learning_as_intermediate_use, empirical, 'The boundary between training-as-transformation and training-as-derivative-preparation is currently unsettled.').

omega_variable(
    reading_contest_structural,
    'Is the coordination reading (transformative = non-infringing) or the enclosure reading (derivative = any use of original) the correct interpretation of the statute?',
    'The three readings are not resolvable by empirical facts — they are different normative commitments about what copyright law should do. Enclosure reading: maximizes copyright holder control and revenue; Coordination reading: prioritizes creator freedom and transformative culture; Hybrid reading: splits commercial and non-commercial. Courts and legislatures choose among them based on policy goals, not facts.',
    'This is a CONCEPTUAL omega, not empirical. If the enclosure reading prevails (institutionally and legislatively), the statute is reinterpreted and this constraint (coordination reading) becomes a sibling constraint describing an outmoded reading. If the coordination reading prevails, the constraint strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_structural, conceptual, 'The three readings embody different policy commitments about copyright''s proper scope.').

omega_variable(
    creator_vs_copyright_holder_coalitional_stability,
    'Will transformative creators and generative technology companies maintain sufficient political power to resist legislative narrowing of the transformative use doctrine?',
    'Legislative advocacy, industry lobbying, and public debate. If copyright holders successfully lobby Congress to amend the statute to define derivative work more broadly, the coordination reading is legislatively overridden.',
    'If the coalition fragments (technology companies settle with copyright holders and accept licensing; creators are isolated), the constraint drifts toward the enclosure reading. If the coalition holds and expands (public support for creator freedom, legislative resistance to copyright expansion), the coordination reading persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_vs_copyright_holder_coalitional_stability, empirical, 'The constraint''s persistence depends on a coalition of beneficiaries maintaining sufficient power to resist enclosure narrowing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(deri_tr_t0, observed).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(deri_tr_t5, observed).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(deri_tr_t10, observed).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(deri_tr_t15, observed).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 20, 0.115).
narrative_ontology:measurement_basis(deri_tr_t20, observed).
narrative_ontology:measurement(deri_tr_t25, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(deri_tr_t25, observed).
narrative_ontology:measurement(deri_tr_t30, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 30, 0.122).
narrative_ontology:measurement_basis(deri_tr_t30, observed).
narrative_ontology:measurement(deri_tr_t40, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(deri_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(deri_be_t0, observed).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 5, 0.21).
narrative_ontology:measurement_basis(deri_be_t5, observed).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement_basis(deri_be_t10, observed).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 15, 0.26).
narrative_ontology:measurement_basis(deri_be_t15, observed).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement_basis(deri_be_t20, observed).
narrative_ontology:measurement(deri_be_t25, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement_basis(deri_be_t25, observed).
narrative_ontology:measurement(deri_be_t30, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 30, 0.285).
narrative_ontology:measurement_basis(deri_be_t30, observed).
narrative_ontology:measurement(deri_be_t40, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(deri_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(deri_su_t0, observed).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 5, 0.13).
narrative_ontology:measurement_basis(deri_su_t5, observed).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement_basis(deri_su_t10, observed).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 15, 0.145).
narrative_ontology:measurement_basis(deri_su_t15, observed).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement_basis(deri_su_t20, observed).
narrative_ontology:measurement(deri_su_t25, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement_basis(deri_su_t25, observed).
narrative_ontology:measurement(deri_su_t30, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement_basis(deri_su_t30, observed).
narrative_ontology:measurement(deri_su_t40, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement_basis(deri_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, information_standard).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__coordination_reading, 0.05).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, fair_use_doctrine_scope).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, copyright_licensing_ecosystem_stability).

% DUAL FORMULATION NOTE:
% The derivative_work_statutory_boundary kernel instantiates three distinct constraints via three readings: (1) coordination_reading (this file): transformative uses non-infringing, low ε rope; (2) enclosure_reading: all derivative preparation requires licensing, high ε snare; (3) hybrid_carveout_reading: commercial vs. non-commercial split, medium ε tangled_rope. Each reading interprets the same statute (17 U.S.C. § 103) differently based on different normative commitments about copyright's proper scope. The ε values differ substantially because the readings produce different beneficiary/victim structures and licensing regimes. Do NOT attempt to average or merge the readings — they are three separate constraints, each with its own coherent stakeholder logic. Coordination and enclosure readings foreclose each other; both coexist with the hybrid reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__coordination_reading, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
