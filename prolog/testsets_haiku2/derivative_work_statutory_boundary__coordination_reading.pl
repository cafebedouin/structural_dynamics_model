% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative Work Boundary (Coordination Reading): Transformative Use Exemption
 *   domain: intellectual_property/information_economics
 *
 * SUMMARY:
 *   Under the coordination reading of the derivative-work boundary, only
 *   fixed recastings that substantially incorporate original expression
 *   trigger the copyright holder's exclusive right to prepare derivative
 *   works. Transformative uses (parody, criticism, remix, new artistic
 *   composition) and intermediate uses (ML training, pattern extraction from
 *   text) are non-infringing and require no ex-ante licensing. This reading
 *   prioritizes permitting creative reuse and downstream innovation over
 *   maximizing licensing revenue for original copyright holders. The
 *   constraint's function is coordination: it establishes a bright-line rule
 *   that permits creators to assess legality without transaction costs. The
 *   kernel is contested (enclosure reading treats all uses as derivative;
 *   hybrid-carveout reading conditions permission on non-commercial status);
 *   this story instantiates only the coordination reading.
 *
 * KEY AGENTS:
 *   - Original copyright holders: retain exclusive licensing rights for fixed, substantially-similar recastings; lose licensing revenue from transformative uses.
 *   - Transformative users (artists, writers, musicians, remixers): operate license-free when creating new works that substantially transform original expression.
 *   - ML training practitioners and generative technology developers: can use copyrighted data as training input without licensing, provided the training process is transformative (pattern extraction, not fixed incorporation).
 *   - Independent creators and academic researchers: benefit from clear permission to incorporate copyrighted material in transformative scholarship and creative work.
 *   - Copyright licensing-market intermediaries: experience narrowed market as transformative uses migrate to license-free channels.
 *   - Judiciary: administers the bright-line boundary between derivative and transformative work through case law.
 *   - Enclosure-reading advocates: excluded from coordination reading; would object that transformative exemption erodes licensing revenue.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.28).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.12).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Boundary (Coordination Reading): Transformative Use Exemption").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property/information_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, '1bfc1df4-8abf-47b8-a514-979e4f19de44').
narrative_ontology:cs_kernel_codification('1bfc1df4-8abf-47b8-a514-979e4f19de44', fixed_text).
narrative_ontology:cs_authority_grounding('1bfc1df4-8abf-47b8-a514-979e4f19de44', lineage).
narrative_ontology:cs_interpretation_layer_present('1bfc1df4-8abf-47b8-a514-979e4f19de44').
narrative_ontology:cs_reading_relation('1bfc1df4-8abf-47b8-a514-979e4f19de44', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('1bfc1df4-8abf-47b8-a514-979e4f19de44', derivative_work_statutory_boundary__hybrid_carveout_reading, influences).
narrative_ontology:cs_axiom('1bfc1df4-8abf-47b8-a514-979e4f19de44', foundational, transformative_use_non_infringing).
narrative_ontology:cs_axiom_status(transformative_use_non_infringing, holdable).
narrative_ontology:cs_axiom_grounding('1bfc1df4-8abf-47b8-a514-979e4f19de44', transformative_use_non_infringing, deontological).
narrative_ontology:cs_axiom('1bfc1df4-8abf-47b8-a514-979e4f19de44', foundational, fixed_recasting_requires_authorization).
narrative_ontology:cs_axiom_status(fixed_recasting_requires_authorization, holdable).
narrative_ontology:cs_axiom_grounding('1bfc1df4-8abf-47b8-a514-979e4f19de44', fixed_recasting_requires_authorization, conventional).
narrative_ontology:cs_reference_frame('1bfc1df4-8abf-47b8-a514-979e4f19de44', fair_use_transformative_doctrine).
narrative_ontology:cs_drift_state('1bfc1df4-8abf-47b8-a514-979e4f19de44', contemporary_generative_ai_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1bfc1df4-8abf-47b8-a514-979e4f19de44', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, transformative_users).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, ml_training_practitioners).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, generative_technology_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, original_copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, independent_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, academic_researchers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, original_copyright_holders).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, original_authors_licensing_market).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold exclusive statutory rights to control preparation of derivative works. Under the coordination reading, they retain exclusive licensing authority over fixed recastings that substantially incorporate original expression, but cannot prevent transformative uses or intermediate uses (e.g., ML training on copyrighted texts). They benefit from coordination clarity that permits licensing of genuine derivative works without requiring prior consent for every transformative application. They pay by accepting loss of licensing revenue from transformative-use applications that would require explicit permission under enclosure reading.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, original_copyright_holders, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__coordination_reading, original_copyright_holders, payer).

% Create new works that incorporate, modify, or build upon copyrighted expression to produce substantially new meaning, message, or expression (parody, criticism, commentary, new artistic compositions). Under coordination reading, they operate without ex-ante licensing requirement; their use is non-infringing if sufficiently transformative. They benefit from clear bright-line rule separating transformative from fixed-recasting uses. They exercise substantial creative freedom in how they incorporate source material.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, transformative_users, beneficiary,
    organized, biographical, arbitrage, global).

% Use copyrighted texts, images, and code to train machine-learning models. Under coordination reading, this intermediate use (reading to extract patterns, not copying the work itself into training data in fixed form) is non-infringing and requires no licensing negotiation. They benefit from clear permission to use copyrighted material as training input without transaction costs of securing individual author consent.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, ml_training_practitioners, beneficiary,
    organized, biographical, arbitrage, global).

% Build generative models and systems trained on copyrighted data. Under coordination reading, they can incorporate training data without licensing negotiations as long as the training process is transformative (extracting patterns) rather than fixed incorporation. They benefit from the scaffold: clear license-free access to training data now, with the expectation that output governance (whether model outputs infringe) will be addressed separately as a future policy question.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, generative_technology_developers, beneficiary,
    powerful, biographical, arbitrage, global).

% Small-scale creators (artists, musicians, writers, programmers) who remix, sample, adapt, or build upon existing works to create new creative works. Under coordination reading, they can operate without licensing if their work is sufficiently transformative. They benefit from reducing gatekeeping: licensing costs and negotiation burden are eliminated for uses that pass the transformative threshold.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, independent_creators, beneficiary,
    moderate, biographical, mobile, national).

% Use copyrighted materials (texts, data, code) in research, analysis, and knowledge-building. Under coordination reading, intermediate uses (reading and analysis to extract research insights) are non-infringing and require no licensing. They benefit from clarity that scholarship and research are transformative uses not requiring prior authorization.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, academic_researchers, beneficiary,
    moderate, biographical, mobile, national).

% Institutions, publishers, and rights-management platforms that profit by licensing derivative-work rights to commercial users. Under coordination reading, the licensing market is narrowed: only fixed recastings and non-transformative uses require licenses. They pay by losing licensing revenue from ML training, transformative remixing, and intermediate uses that would require permission under enclosure reading. The coordination reading shifts much derivative-work creation to license-free channels.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, original_authors_licensing_market, payer,
    powerful, biographical, constrained, global).

% Courts interpret the Copyright Act's derivative-work boundary and apply the fair-use doctrine to determine what counts as a non-infringing use. Under coordination reading, courts apply a bright-line test: fixed recastings of substantial original expression require license; transformative uses do not. The judiciary administers and defends this boundary through case law and precedent.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, judiciary_copyright_doctrines, agenda_setter,
    institutional, generational, analytical, national).

% Congress writes the Copyright Act and can modify the statutory definition of derivative work. Under coordination reading, legislative silence on ML training and intermediate uses is interpreted as permission (the boundary is set by judicial interpretation, not amended statute). Congress could clarify statutory language if consensus emerges that the boundary should move; absence of amendment preserves the coordination reading.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, legislative_copyright_framework, observer,
    institutional, generational, analytical, national).

% Copyright holders and publishers who argue that ANY use of copyrighted expression in creating new work is preparation of a derivative work requiring authorization. They are structurally excluded from the coordination reading (their reading is incompatible with it — the kernel contest is about what constitutes a derivative work). They would object that the coordination reading gives away licensing revenue and treats copyrighted expression as a public good for creative reuse.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, enclosure_reading_advocates, excluded,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces transaction costs for creative reuse: by establishing a bright-line boundary (fixed recastings of original expression require license; transformative uses do not), creators can assess their legal position without negotiating individual licenses for every transformative application. Enables ML training as an intermediate use without licensing. Coordinates expectations: copyright holders understand what exclusive rights remain; transformative users understand what freedom they have.
% TRANSFER_FUNCTION: Transfers licensing revenue from the subset of derivative works that are fixed recastings and substantially incorporate original expression. Non-transformative intermediate uses (e.g., verbatim copying as training input) lose licensing revenue potential. Transformative uses (parody, criticism, new artistic composition, ML training for pattern extraction) generate no licensing obligation, shifting that value to downstream innovation. Original authors retain exclusive licensing rights only for works that are fixed, substantial recastings.
% ABSENT_VOICES: Content-rightholder organizations and licensing-market intermediaries (publishers, rights-management platforms, music licensing agencies) would object that transformative-use exemptions and ML-training license-freedom erode their licensing revenue. Independent creators in jurisdictions with weak fair-use traditions would object that the rule does not extend to their contexts. The coordination reading privileges the jurisdictions with strong transformative-use precedent (primarily US copyright doctrine).
% DISAPPEARANCE_RATIONALE: If the transformative-use exemption were eliminated and all uses of copyrighted expression required authorization (pure enclosure reading), ML training infrastructure would face licensing negotiation bottlenecks, creative remixing would require upstream permission, and copyright licensing markets would expand — but negotiation costs and gatekeeping would increase substantially. Generative technology development would slow pending licensing resolution. The coordination reading permits a technology ecosystem (ML, remix culture, remix-enabled creativity) that enclosure reading would constrain.
% FOUNDING_PROBLEM: Copyright law's derivative-work provision (17 U.S.C. § 103) grants copyright holders exclusive right to prepare derivative works, but the statute does not define 'derivative work.' Early case law left ambiguity: does any use of copyrighted expression in creating new work count as infringement, or only substantial recastings? The coordination reading was built to permit creative reuse and transformative application without requiring licensing for every variation, while preserving copyright holders' exclusive right to fixed, substantially-similar recastings.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court and appeals courts (Sony v. Betamax, Harper & Row v. Nation, Campbell v. Acuff-Rose) attest that some uses of copyrighted expression are non-infringing transformative uses. Copyright scholars and technology policy analysts outside the rights-holder sphere argue the founding problem (ambiguity over what counts as derivative) is solved by the bright-line rule: fixed recastings require license, transformative uses do not. Rights-holder organizations contest this, arguing the founding problem is ongoing — that the statute is ambiguous and should be read to require authorization for all derivative preparation.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__coordination_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is LOW (0.28 at interval end) because the constraint permits most creative reuse without licensing — the original authors lose revenue potential from transformative uses. Suppression is VERY LOW (0.12) because the constraint requires NO active enforcement machinery; it is a rule of permission, not prohibition. Theater ratio is minimal (0.08) because the rule's function is genuine coordination (establishing a bright-line boundary) not performative maintenance. The measurement series show extractiveness slightly rising over the interval (0.22 → 0.28) as courts narrowed transformative-use exemptions in response to commercial-scale copying and as licensing markets adapted by fragmenting into niche licensing (music, images, books) where coordination is tighter. Suppression stays flat (stable enforcement of the boundary) because the rule itself requires no active suppression — it is a liberty rule, not a constraint rule. The constraint's type is ROPE because it solves a genuine coordination problem (creative reuse without licensing negotiation) with minimal extractiveness, and participants are net beneficiaries of the clarity (copyright holders keep exclusive rights to fixed recastings; transformative users gain license-free access). No party bears pure extraction here; the distribution of benefit is asymmetric (transformative users gain more than copyright holders), but all parties gain coordination value. The claim/metric independence is preserved: I author the metrics honestly, and the engine computes the type; if the computed type diverges from rope, the divergence is itself data about whether coordination or extraction dominates in practice.
 *
 * PERSPECTIVAL GAP:
 *   From the copyright-holder seat, this constraint is LOSING them licensing revenue on transformative uses they argue should require authorization — from their position it looks extractive (they pay by losing revenue). From the transformative-user seat, it is pure coordination (clear permission, no licensing transaction costs). From the ML-training seat, it is coordination plus a scaffold (they have license-free access now; governance of model-output infringement is deferred to future policy). The engine computes each seat's type from the structural data: copyright holders as beneficiaries with mobile exit (they can license other uses, lobby for statutory change) sit in the beneficiary directionality range; transformative users as beneficiaries with arbitrage exit sit in the beneficiary range; licensing-market intermediaries as payers with constrained exit sit in the target range. The per-seat classifications will diverge because the structural asymmetry is real: coordination benefit is concentrated in upstream creation (transformative users, ML practitioners) and diffuse in downstream licensing markets (copyright holders lose potential revenue but retain exclusive rights over the subset of uses the coordination reading still requires licensing).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: transformative_users, ml_training_practitioners, generative_technology_developers, independent_creators, academic_researchers — all operate with substantially reduced licensing burden and clearer legal position. Copyright holders are ALSO beneficiaries (they retain exclusive licensing rights for non-transformative uses and fixed recastings, and benefit from coordination clarity), but they pay a measurable cost (lost licensing revenue from transformative applications that would require permission under enclosure reading). Licensing-market intermediaries (publishers, music-licensing platforms) are PAYERS — their revenue declines as transformative uses migrate to license-free channels. The directionality for copyright holders is slightly above center (d ≈ 0.45-0.55) because they are both beneficiaries (coordination clarity, retained exclusive rights) and payers (lost revenue); for transformative users and ML practitioners, d is low (≈ 0.2-0.3) because they are clear net beneficiaries. For licensing intermediaries, d is high (≈ 0.65-0.75) because they are clear net payers. The structural asymmetry is that the coordination benefit is concentrated in users and downstream innovation, while the extraction cost (lost revenue) is diffuse across the licensing-market supply chain — classic rope asymmetry: all parties benefit from coordination clarity, but the cost distribution is unequal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ambiguity over what counts as derivative work) is CONTESTED — rights holders argue the statute is ambiguous and should require authorization for all derivative preparation; courts argue the statute permits transformative uses as non-infringing fair use. The coordination reading assumes the founding problem is solved (bright-line boundary established by case law). If the founding problem DIED (courts or Congress clarified that ML training and transformative uses are clearly non-infringing), the coordination reading would persist as pure coordination scaffolding — no extraction, no enforcement burden, pure information clarity. If the founding problem REVIVED (Congress amended the statute to require licensing for ML training or transformative uses), the constraint would shift toward snare (ex-ante licensing requirements, active enforcement of new boundaries, licensing-market gatekeeping). The mandatrophy signal to watch: if courts begin requiring licenses for transformative uses, or if legislative proposals to add ML licensing requirements pass, the constraint transitions from rope toward snare. Currently the constraint avoids mandatrophy because its founding justification (permitting creative reuse without licensing negotiation) aligns with observed operation (transformative users operate license-free; licensing is required only for fixed recastings). If the founding problem were declared dead by the copyright community itself (all parties agreeing transformative use is license-free), the constraint would become pure scaffold: coordination function clear, sunset condition implicit (once all parties accept the bright-line rule, enforcement of the rule approaches zero). The current state is contested mandatrophy: different seats assert different founding problems (copyright holders: we built licensing to monetize all derivative work; transformative users: the founding problem is permitting creative reuse; courts: the founding problem is clarifying what counts as derivative). The constraint persists because the rope function (coordination clarity) outweighs the extraction cost (lost licensing revenue) across enough seats to maintain the rule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_boundary_empirical_drift,
    'How do courts determine whether a use is sufficiently transformative to fall outside the derivative-work right? Does the bright-line rule remain stable or does case law gradually narrow transformative exemptions as commercial scale increases?',
    'Analysis of appellate decisions over time: does the set of uses classified as transformative expand, contract, or remain stable? Direct evidence: licensing litigation outcomes, injunction patterns, settlements that reveal parties'' expectations of where the boundary lies.',
    'If case law narrows transformative exemptions (courts require licensing for uses they previously treated as transformative), extractiveness rises, suppression requirement increases, and the constraint drifts toward snare. If exemptions expand (courts permit more uses to proceed without license), extractiveness falls further and rope classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformative_boundary_empirical_drift, empirical, 'Whether the transformative-use boundary expands or contracts over time.').

omega_variable(
    ml_training_licensing_demand,
    'Do copyright holders successfully monetize ML training through licensing agreements, or does the coordination reading''s implicit permission for training persist despite commercial generative AI development?',
    'Market evidence: volume of licensing agreements for ML training data, average licensing fees, licensing refusals and litigation outcomes when copyright holders attempt to require licenses for training. Evidence: published licensing terms from major ML platforms.',
    'If licensing becomes standard for ML training data, the extractiveness for ML practitioners increases substantially and the constraint becomes more snare-like (ex-ante licensing requirement enforced through litigation). If training remains predominantly license-free despite commercial deployment, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ml_training_licensing_demand, empirical, 'Whether ML training licensing becomes a significant revenue stream despite coordination reading.').

omega_variable(
    reading_committer_foreclosure,
    'Is the coordination reading logically foreclosed by the enclosure reading, or can both coexist as live positions held by different parties?',
    'Logical analysis: the enclosure reading asserts all derivative preparation requires authorization; the coordination reading asserts transformative preparation does not. These axioms directly contradict — they cannot coexist in a single legal framework. However, they can coexist across different judicial jurisdictions and different parties'' interpretations of ambiguous statute. The question is whether the kernel (the statute) permits one framework or both.',
    'If the readings truly foreclose each other (one legal framework only), the constraint''s persistence depends on which reading wins legislative or judicial support. If they coexist (different parties hold different readings, both live in public discourse), the constraint persists in a contested state with mandatrophy signals. The type classification would not change, but the polarization and litigation intensity would increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_foreclosure, conceptual, 'Whether the coordination and enclosure readings are logically incompatible or merely opposed.').

omega_variable(
    licensing_market_fragmentation,
    'As transformative use remains license-free under the coordination reading, do licensing markets fragment into niche licensing (music, images, books, code) where copyright holders extract through specialized licensing platforms, or does licensing collapse as coordinate copyright holders?',
    'Market structure evidence: Do specialized licensing platforms (e.g., music licensing, image licensing, open-source licensing) grow in scale and revenue? Or do copyright holders attempt to coordinate licensing rates across all derivative-work types? Evidence: licensing platform revenues, licensing rate negotiations, cross-platform licensing agreements.',
    'If niche licensing grows, the constraint persists as rope with distributed extraction (licensing concentrated in specific content types where coordination is tight). If licensing collapses into one platform or coordinated cartel, extraction rises and suppression increases, shifting toward snare. The coordination reading assumes many licensing markets, not monopoly licensing coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_market_fragmentation, empirical, 'Whether licensing markets fragment into specialized licensing or consolidate into coordinated cartel.').

omega_variable(
    kernel_committer_authority_reading_divergence,
    'This constraint is ONE reading of the derivative-work statutory boundary kernel. The reading instantiation assumes courts and common law are the authoritative interpreters of ambiguous statute. If Congress were to clarify the statute, which reading would the statutory amendment support — coordination, enclosure, or hybrid?',
    'Legislative history and statutory text amendment: if Congress explicitly clarifies whether ML training requires licensing, whether transformative use is privileged, whether all uses are derivative — the amendment resolves the reading contest by statutory specification. Evidence: proposed copyright legislation, Congressional testimony, legislative sponsors'' stated intent.',
    'A statutory amendment supporting enclosure reading would shift the constraint from rope toward snare (licensing requirement for all uses). Amendment supporting coordination would codify the rope reading and narrow foreclosure risk. Amendment supporting hybrid would create a new constraint for the hybrid-carveout reading. The committer authority here (judicial interpretation via case law) is stable; legislative authority (statute amendment) is absent (silence is interpreted as permission under coordination reading). If legislative authority supersedes, the constraint''s foundation shifts from implied permission to explicit statutory definition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_committer_authority_reading_divergence, conceptual, 'How statutory amendment (if any) would affect the reading''s authority grounding and kernel interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(deri_tr_t0, observed).
narrative_ontology:measurement(deri_tr_t8, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement_basis(deri_tr_t8, observed).
narrative_ontology:measurement(deri_tr_t16, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 16, 0.08).
narrative_ontology:measurement_basis(deri_tr_t16, observed).
narrative_ontology:measurement(deri_tr_t24, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement_basis(deri_tr_t24, observed).
narrative_ontology:measurement(deri_tr_t32, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 32, 0.08).
narrative_ontology:measurement_basis(deri_tr_t32, observed).
narrative_ontology:measurement(deri_tr_t40, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(deri_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(deri_be_t0, observed).
narrative_ontology:measurement(deri_be_t8, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement_basis(deri_be_t8, observed).
narrative_ontology:measurement(deri_be_t16, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement_basis(deri_be_t16, observed).
narrative_ontology:measurement(deri_be_t24, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 24, 0.28).
narrative_ontology:measurement_basis(deri_be_t24, observed).
narrative_ontology:measurement(deri_be_t32, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 32, 0.29).
narrative_ontology:measurement_basis(deri_be_t32, observed).
narrative_ontology:measurement(deri_be_t40, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(deri_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(deri_su_t0, observed).
narrative_ontology:measurement(deri_su_t8, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 8, 0.11).
narrative_ontology:measurement_basis(deri_su_t8, observed).
narrative_ontology:measurement(deri_su_t16, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 16, 0.12).
narrative_ontology:measurement_basis(deri_su_t16, observed).
narrative_ontology:measurement(deri_su_t24, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 24, 0.12).
narrative_ontology:measurement_basis(deri_su_t24, observed).
narrative_ontology:measurement(deri_su_t32, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 32, 0.12).
narrative_ontology:measurement_basis(deri_su_t32, observed).
narrative_ontology:measurement(deri_su_t40, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(deri_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, information_standard).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__coordination_reading, 0.05).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, fair_use_doctrine_scope).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, ml_training_copyright_liability).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, generative_ai_output_ownership).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested derivative-work statutory boundary kernel. The kernel is the Copyright Act's ambiguous definition of 'derivative work' — does it include all uses of copyrighted expression in creating new work, or only fixed substantial recastings? The coordination reading (this story) interprets the statute to permit transformative uses as non-infringing; the enclosure reading interprets it to require authorization for all uses; the hybrid-carveout reading permits non-commercial but not commercial transformation. Each reading instantiates a different constraint with different ε, beneficiary/victim structure, and type. The three readings are linked via network.affects_constraints: the coordination reading creates structural downstream pressure on generative-AI governance (permitting unlicensed training), which influences the enclosure reading (copyright holders push back to require licensing). The readings coexist as live positions in public discourse (different courts, different jurisdictions, different stakeholders hold different readings), and the coordination reading influences outcomes in ML training governance (permissions granted under coordination reading cascade into expectations for generative AI policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__coordination_reading, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
