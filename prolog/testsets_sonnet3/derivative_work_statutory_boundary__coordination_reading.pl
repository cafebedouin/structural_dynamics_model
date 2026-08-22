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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative Work Boundary — Narrow/Transformative-Use Reading (Coordination Function)
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint instantiates the coordination reading of the
 *   derivative-work statutory boundary: only a FIXED recasting that
 *   substantially incorporates the original's protected expression counts as
 *   a derivative work requiring authorization; transformative reworkings and
 *   intermediate technical uses (including using a work as a training input
 *   for a generative model) fall outside the exclusive right and are
 *   non-infringing. This is one of three declared readings of the same kernel
 *   statute/doctrine. The enclosure_reading treats any use of copyrighted
 *   expression in creating a new work as derivative-work preparation,
 *   requiring authorization far more broadly. The hybrid_carveout_reading
 *   conditions the boundary on commercial exploitation, permitting
 *   non-commercial transformative use but requiring authorization for
 *   commercial use. This file authors ONLY the coordination reading as a
 *   clean, ε-invariant constraint; the siblings are separate constraint files
 *   linked via network and cs_structure.reading_relations, not alternative
 *   measurements folded into this ε.
 *
 * KEY AGENTS:
 *   - downstream_transformative_creators: primary beneficiary (moderate/mobile) — creates without pre-clearance
 *   - ml_model_developers: primary beneficiary (organized/mobile) — trains without per-work licensing
 *   - original_rightsholders: primary bearer of foregone licensing revenue (powerful/constrained)
 *   - licensing_intermediaries: secondary bearer, loses transaction volume (organized/constrained)
 *   - courts_and_copyright_office: agenda-setter administering the boundary (institutional/analytical)
 *   - general_public_and_future_creators: diffuse beneficiary of a richer commons (powerless/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.18).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.12).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Boundary — Narrow/Transformative-Use Reading (Coordination Function)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property_law/technology_governance/information_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, '08d11dc2-170b-4a65-bf5f-b3f416504daa').
narrative_ontology:cs_kernel_codification('08d11dc2-170b-4a65-bf5f-b3f416504daa', fixed_text).
narrative_ontology:cs_authority_grounding('08d11dc2-170b-4a65-bf5f-b3f416504daa', lineage).
narrative_ontology:cs_interpretation_layer_present('08d11dc2-170b-4a65-bf5f-b3f416504daa').
narrative_ontology:cs_reading_relation('08d11dc2-170b-4a65-bf5f-b3f416504daa', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('08d11dc2-170b-4a65-bf5f-b3f416504daa', derivative_work_statutory_boundary__hybrid_carveout_reading, influences).
narrative_ontology:cs_axiom('08d11dc2-170b-4a65-bf5f-b3f416504daa', foundational, fixation_and_substantial_incorporation_required_for_derivative_status).
narrative_ontology:cs_axiom_status(fixation_and_substantial_incorporation_required_for_derivative_status, holdable).
narrative_ontology:cs_axiom_grounding('08d11dc2-170b-4a65-bf5f-b3f416504daa', fixation_and_substantial_incorporation_required_for_derivative_status, conventional).
narrative_ontology:cs_axiom('08d11dc2-170b-4a65-bf5f-b3f416504daa', foundational, transformative_intermediate_use_categorically_outside_exclusive_right).
narrative_ontology:cs_axiom_status(transformative_intermediate_use_categorically_outside_exclusive_right, holdable).
narrative_ontology:cs_axiom_grounding('08d11dc2-170b-4a65-bf5f-b3f416504daa', transformative_intermediate_use_categorically_outside_exclusive_right, instrumental).
narrative_ontology:cs_reference_frame('08d11dc2-170b-4a65-bf5f-b3f416504daa', narrow_derivative_work_definition_post_campbell_v_acuff_rose).
narrative_ontology:cs_drift_state('08d11dc2-170b-4a65-bf5f-b3f416504daa', generative_ai_training_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('08d11dc2-170b-4a65-bf5f-b3f416504daa', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, downstream_transformative_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, ml_model_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, research_and_commentary_communities).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, the_public_domain_of_ideas).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, general_public_and_future_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, original_rightsholders).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, licensing_intermediaries).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, idea_expression_dichotomy).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, transformative_use_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, fixation_requirement_for_derivative_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Remix artists, critics, parody authors, and scholars who incorporate fragments or stylistic elements of prior works into new fixed expression. Under this reading they need no license because their output is not a 'recasting' substantially incorporating the original's fixed expression — it is a new work that merely draws on ideas, style, or unprotected elements. They can create and publish without pre-clearance.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, downstream_transformative_creators, beneficiary,
    moderate, biographical, mobile, national).

% Firms and labs training generative models on large corpora of copyrighted text, images, and code. Under this reading, ingesting a work to extract statistical patterns does not fix a recasting of that work's original expression into the model's parameters — the trained model is not itself a derivative work of any single ingested source. They can train without per-work licensing, provided outputs don't themselves substantially recast a specific source.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, ml_model_developers, beneficiary,
    organized, generational, mobile, global).

% Authors, publishers, and studios whose works are used as training inputs or creative springboards without a licensing transaction or royalty. They bear the cost of foregone licensing revenue for uses that, under a broader reading, might have required their permission. Their recourse is litigation over whether a specific downstream use crosses into substantial incorporation of fixed expression — a fact-intensive, expensive path, not a categorical veto.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, original_rightsholders, payer,
    powerful, generational, constrained, global).

% Collective licensing bodies and rights-clearance agencies whose business model depends on mandatory ex-ante clearance for adjacent-work creation. A narrow derivative-work boundary shrinks the set of transactions that must clear through them, directly reducing their transaction volume and relevance.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, licensing_intermediaries, payer,
    organized, biographical, constrained, national).

% Administer and interpret the statutory boundary through case law and rulemaking, drawing the line between 'substantial incorporation of fixed original expression' (derivative, infringing without license) and transformative or intermediate use (non-infringing). They set the operative test that determines which of the two readings governs any given dispute.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, courts_and_copyright_office, agenda_setter,
    institutional, civilizational, analytical, national).

% Benefit diffusely from a richer, less encumbered cultural and technological commons — more commentary, more parody, more generative tools available without needing to trace and clear rights in every antecedent influence. They have no direct role in the dispute but inherit the resulting breadth or narrowness of the public domain of expression.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, general_public_and_future_creators, beneficiary,
    powerless, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws a bright(er) statutory line — fixation plus substantial incorporation of original expression — so that creators, technologists, and courts can predict ex ante which downstream uses require a license and which do not, avoiding case-by-case relitigation of every act of creative or computational reuse.
% TRANSFER_FUNCTION: Moves the default allocation of permission away from original rightsholders and toward downstream users: transformative and intermediate uses (including training-data ingestion) proceed without a licensing transaction, shifting potential license revenue from rightsholders to the users who would otherwise have paid for it.
% ABSENT_VOICES: Individual authors whose specific works are heavily represented in training corpora rarely appear as named parties in the doctrinal disputes that set this boundary — the disputes are typically litigated by well-resourced publishers or studios as class representatives, and the diffuse public that benefits from openness is even less present as an articulate party.
% DISAPPEARANCE_RATIONALE: If this narrow reading were replaced overnight by a rule where any incorporation of copyrighted expression in creating a new work counts as derivative-work preparation, transformative use doctrine would collapse, ML training pipelines would require mass licensing or cease, and remix/commentary culture would face a licensing bottleneck it does not currently face — a substantial reorganization of both creative practice and the AI industry's data-sourcing model.
% FOUNDING_PROBLEM: Copyright law needed a workable line between (a) genuine unauthorized copying/adaptation that substitutes for the original in the market, which the exclusive right to prepare derivative works was meant to prevent, and (b) new expression, commentary, or technical processing that draws on prior works without displacing their market — a line that, left undrawn, would let rightsholders veto criticism, parody, scholarship, and now machine learning merely because their work was consulted or ingested.
% FOUNDING_PROBLEM_CORROBORATION: Courts applying the fixation and substantial-incorporation tests (e.g., in transformative-use and fair-use adjacent case law) and copyright scholars outside the AI industry and outside rightsholder trade groups attest that the underlying problem — preventing an exclusive-rights regime from swallowing all downstream creative and technical activity that merely references prior expression — remains active and unresolved as generative AI scales; this is not solely asserted by ML developers who benefit from the narrow reading.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__coordination_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.18 at interval end) because, under this reading's own lights, the standing arrangement imposes minimal coercive overhead: no ex-ante licensing bureaucracy, no gatekeeping toll on transformative creation or model training. Suppression is low (0.12) because alternatives — licensing markets, opt-in clearance, contractual restriction — remain available to rightsholders who want them; the boundary does not foreclose licensing, it simply does not mandate it as a precondition. Theater ratio stays low and flat (0.08→0.10) because the doctrinal machinery (fixation + substantial-incorporation tests) does the actual gatekeeping work courts rely on; it is not performative. Resistance (0.20) and accessibility_collapse (0.15) are both modest, reflecting genuine ongoing litigation contest rather than a settled, defended structure — this is a live doctrinal boundary, not entrenched enclosure.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream creators, ML developers, and the diffuse public sit near the beneficiary end: the constraint subsidizes their activity by not requiring a transaction. Original rightsholders and licensing intermediaries sit nearer the target end: they bear the opportunity cost of licensing revenue that a broader reading would have captured for them, though their exit is only 'constrained' rather than 'trapped' — they retain litigation and contractual paths, and the boundary is itself contested and could move. Courts are the analytical agenda-setters who administer, not benefit from, the line.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing an exclusive right over 'derivative works' from swallowing commentary, parody, scholarship, and now computational analysis merely because they touch prior expression) remains live rather than obsolete — generative AI has raised the stakes on exactly this question rather than resolving it. That keeps this reading from being read as an inertial holdover: it is an active, contested coordination function, not a scaffold whose sunset has passed unnoticed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantial_incorporation_threshold_indeterminacy,
    'Where exactly does ''substantial incorporation of original expression'' end and permissible transformative/intermediate use begin, especially for non-expressive intermediate copying (e.g., an ML model''s internal statistical representation of training data)?',
    'Accumulating appellate case law directly addressing whether model weights/embeddings constitute a ''fixed recasting'' of any specific ingested work, and whether output-level similarity analysis is the correct locus of the substantial-incorporation test rather than input-level ingestion.',
    'If courts converge on treating training ingestion itself (rather than only substantially similar output) as potentially substantial incorporation, this reading''s low-ε coordination-scaffold classification would not survive — the constraint would functionally migrate toward the enclosure_reading''s structure even without a formal doctrinal reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantial_incorporation_threshold_indeterminacy, empirical, 'Unresolved boundary of the substantial-incorporation test as applied to computational/intermediate uses.').

omega_variable(
    reading_selection_is_the_live_dispute,
    'Is the coordination_reading (fixation + substantial incorporation, transformative/intermediate use free) actually the controlling doctrinal reading, or is it one contested position among the coordination_reading, enclosure_reading, and hybrid_carveout_reading, with the eventual controlling reading still undetermined by ongoing litigation over generative AI training?',
    'Track outcomes and reasoning across pending and future generative-AI copyright litigation and any subsequent legislative action; convergence toward one reading across multiple jurisdictions and courts of appeal would resolve which reading is authoritative.',
    'If the enclosure_reading or hybrid_carveout_reading becomes dominant, the real-world constraint governing ML developers and downstream creators shifts to a substantially higher-ε structure than this file authors; this file''s low ε is only accurate under the coordination_reading''s own lights, which is exactly the kernel-reading discipline this story is built to preserve rather than resolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_is_the_live_dispute, conceptual, 'Which of the three declared kernel readings will actually control future enforcement and licensing practice.').

omega_variable(
    rightsholder_market_harm_measurement,
    'Does uncompensated use of copyrighted works as training data or transformative raw material cause measurable market harm to rightsholders (lost licensing revenue, market substitution), or is the foregone revenue illusory because no functioning licensing market existed or would exist for these uses absent the rule?',
    'Empirical market studies of licensing markets that have emerged post-hoc (e.g., data-licensing deals struck after litigation pressure) compared to counterfactual market formation absent any legal exposure.',
    'If a real licensing market would have existed and now doesn''t because of this reading, the ''payer'' status and extractiveness borne by original_rightsholders is understated; if no such market was ever viable, this reading''s low-ε coordination framing is more fully vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rightsholder_market_harm_measurement, empirical, 'Whether foregone licensing revenue to rightsholders under this reading reflects a real or a merely hypothetical market.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(deri_tr_t4, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 4, 0.08).
narrative_ontology:measurement(deri_tr_t8, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(deri_tr_t12, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement(deri_tr_t16, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(deri_be_t4, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 4, 0.13).
narrative_ontology:measurement(deri_be_t8, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 8, 0.15).
narrative_ontology:measurement(deri_be_t12, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 12, 0.16).
narrative_ontology:measurement(deri_be_t16, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 16, 0.17).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 20, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(derivative_work_statutory_boundary__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kernel derivative_work_statutory_boundary, decomposed per the ε-invariance principle: the natural-language question 'what counts as a derivative work' resolves to structurally distinct claims with different ε values depending on how broadly 'derivative work' is read. coordination_reading (this file) authors ε≈0.18 (rope) under a narrow fixation+substantial-incorporation test. enclosure_reading authors a substantially higher ε (tangled_rope/snare candidate) under a test where any use of copyrighted expression in creating new work triggers the exclusive right. hybrid_carveout_reading authors an intermediate ε (tangled_rope candidate) conditioned on commercial exploitation. All three share the same underlying statutory kernel and are linked bidirectionally via affects_constraints; the eventual controlling reading is tracked in omega reading_selection_is_the_live_dispute rather than resolved within any single file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
