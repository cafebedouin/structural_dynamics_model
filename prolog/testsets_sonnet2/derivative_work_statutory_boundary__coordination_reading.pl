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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Derivative Work Boundary — Narrow-Fixation Coordination Reading
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This story instantiates the coordination reading of the derivative-work
 *   statutory boundary kernel: only a fixed recasting that substantially
 *   incorporates original expression qualifies as a derivative work, leaving
 *   transformative and intermediate uses — including model training on
 *   copyrighted corpora, search indexing, parody, and criticism — outside the
 *   derivative-work right and therefore non-infringing without ex-ante
 *   licensing. The reading functions as a low-extraction coordination
 *   scaffold: it lets a large, decentralized population of creators,
 *   platforms, and ML developers act without individualized clearance, at the
 *   cost of narrowing rightsholders' ability to monetize follow-on and
 *   machine-training uses of their expression. Sibling readings of the same
 *   kernel — the enclosure reading (any use in creating new work is
 *   derivative-work preparation) and the hybrid carveout reading (commercial
 *   use requires authorization, non-commercial transformative use does not) —
 *   are separate constraint stories with their own ε and stakeholder
 *   structures, linked here via network.affects_constraints, not folded into
 *   this one.
 *
 * KEY AGENTS:
 *   - transformative_use_creators: primary beneficiary (moderate/mobile) — creates without licensing friction
 *   - machine_learning_developers: primary beneficiary (organized/mobile) — trains models without per-work clearance
 *   - original_rightsholders: primary payer (powerful/constrained) — loses licensing leverage over follow-on and training uses
 *   - search_and_indexing_platforms: secondary beneficiary (institutional/arbitrage) — intermediate copying shielded
 *   - courts_and_copyright_office: agenda-setter (institutional/analytical) — administers and could shift the line
 *   - midlist_and_freelance_authors: excluded (powerless/trapped) — bears diffuse cost, no seat at the table
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.18).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.15).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Boundary — Narrow-Fixation Coordination Reading").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property_law/technology_governance/information_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, 'ba878c07-091a-4781-bc46-7afb177f5d48').
narrative_ontology:cs_kernel_codification('ba878c07-091a-4781-bc46-7afb177f5d48', fixed_text).
narrative_ontology:cs_authority_grounding('ba878c07-091a-4781-bc46-7afb177f5d48', lineage).
narrative_ontology:cs_interpretation_layer_present('ba878c07-091a-4781-bc46-7afb177f5d48').
narrative_ontology:cs_reading_relation('ba878c07-091a-4781-bc46-7afb177f5d48', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('ba878c07-091a-4781-bc46-7afb177f5d48', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('ba878c07-091a-4781-bc46-7afb177f5d48', foundational, fixation_and_substantial_incorporation_required).
narrative_ontology:cs_axiom_status(fixation_and_substantial_incorporation_required, holdable).
narrative_ontology:cs_axiom_grounding('ba878c07-091a-4781-bc46-7afb177f5d48', fixation_and_substantial_incorporation_required, conventional).
narrative_ontology:cs_axiom('ba878c07-091a-4781-bc46-7afb177f5d48', foundational, intermediate_copying_categorically_non_infringing).
narrative_ontology:cs_axiom_status(intermediate_copying_categorically_non_infringing, holdable).
narrative_ontology:cs_axiom_grounding('ba878c07-091a-4781-bc46-7afb177f5d48', intermediate_copying_categorically_non_infringing, instrumental).
narrative_ontology:cs_reference_frame('ba878c07-091a-4781-bc46-7afb177f5d48', narrow_fixation_incorporation_test).
narrative_ontology:cs_drift_state('ba878c07-091a-4781-bc46-7afb177f5d48', generative_ai_training_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ba878c07-091a-4781-bc46-7afb177f5d48', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, transformative_use_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, machine_learning_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, search_and_indexing_platforms).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, downstream_technology_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, original_rightsholders).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, fair_use_transformative_purpose_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, idea_expression_dichotomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Parody artists, critics, remixers, and secondary-market commentators who build new expression referencing existing copyrighted works. Under this reading they need no license and face no ex-ante clearance requirement as long as their use is transformative rather than a substantially-incorporating fixed recasting. They can create, publish, and monetize without seeking permission from the underlying rightsholder.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, transformative_use_creators, beneficiary,
    moderate, biographical, mobile, national).

% Firms and researchers training generative models on large corpora of copyrighted text, images, and code. Under this reading, training-stage ingestion and intermediate model states are non-infringing intermediate uses that do not fix a substantially-incorporating recasting; they can train at scale without per-work licensing, so long as outputs are not themselves fixed recastings of specific protected expression.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, machine_learning_developers, beneficiary,
    organized, generational, mobile, global).

% Authors, publishers, and studios whose expression is ingested, referenced, or transformed without compensation or consent under this reading. They retain rights only over fixed recastings that substantially incorporate their original expression; they bear the cost of a narrower royalty base and reduced leverage over derivative licensing markets, particularly against large-scale ML training uses. Litigation is available but the doctrinal boundary itself is set against them.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, original_rightsholders, payer,
    powerful, biographical, constrained, global).

% Operate indexing, caching, and retrieval-augmented systems that copy and process copyrighted material as an intermediate step to deliver search, summarization, or discovery functions. This reading treats such intermediate copying as non-infringing so long as it does not culminate in a fixed recasting substantially incorporating the original expression, letting them operate without a licensing gate.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, search_and_indexing_platforms, beneficiary,
    institutional, generational, arbitrage, global).

% Adjudicate what counts as a 'fixed recasting substantially incorporating original expression' versus transformative or intermediate use. They administer the doctrinal line case by case, and could shift the boundary toward the enclosure or hybrid readings through future rulings or rulemaking.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, courts_and_copyright_office, agenda_setter,
    institutional, generational, analytical, national).

% Individual writers, illustrators, and photographers with no litigation budget and no seat in the doctrinal debate. They would object that 'transformative' has expanded to swallow commercial substitution effects that displace their own licensing markets, but they are not organized parties before the courts that set the boundary.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, midlist_and_freelance_authors, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws a bright-enough line — fixation plus substantial incorporation of original expression — so that downstream creators, technologists, and platforms can determine ex ante, without individualized licensing negotiation, whether a given transformative or intermediate use is lawful. This lets a large, decentralized ecosystem of remix culture, search infrastructure, and machine learning proceed without a case-by-case permission bottleneck.
% TRANSFER_FUNCTION: Moves the option value of unlicensed transformative and intermediate use from original rightsholders to downstream creators, platform operators, and ML developers: rightsholders lose the ability to charge for uses that fall on the non-infringing side of the line, and that value accrues to whoever can characterize their use as transformative or intermediate rather than a substantially-incorporating fixed recasting.
% ABSENT_VOICES: Midlist and freelance authors whose licensing markets are most directly displaced by expansive transformative-use and ML-training readings are rarely parties to the appellate litigation that sets this boundary; well-resourced studios and large ML firms dominate both sides of the docket, while the diffuse class of individual creators has no comparable voice in shaping where the line falls.
% DISAPPEARANCE_RATIONALE: If this narrow reading of the derivative-work boundary vanished and any use of copyrighted expression in creating new work were treated as derivative-work preparation (the enclosure reading), transformative art, criticism, search indexing, and ML training would all require ex-ante licensing or risk infringement liability — reorganizing entire industries (generative AI, search, fan and remix culture) around clearance regimes that do not currently exist.
% FOUNDING_PROBLEM: Copyright's derivative-work right threatened to give rightsholders veto power over any subsequent creative or technical use that drew on their work, which would strangle criticism, parody, indexing, and follow-on innovation if read literally and broadly; courts needed a limiting principle distinguishing appropriation of expression from use of a work as raw material for something new.
% FOUNDING_PROBLEM_CORROBORATION: Technology industry associations and digital rights organizations attest the founding problem (chilled follow-on innovation) remains live and the narrow reading is necessary. Authors' guilds, photographers' associations, and some copyright scholars — outside the beneficiary set of ML developers and platforms — attest that the problem has been overcorrected: the transformative-use doctrine and intermediate-copying carveout now shield large-scale commercial substitution that the founding limiting principle was never meant to reach.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.18 at interval end) because the coordination reading's own operation does not extract rents through an enforcement apparatus — it removes a licensing requirement rather than imposing one. Suppression is low (0.15): the boundary does not suppress alternatives so much as decline to create a permission gate. Theater ratio is very low (0.10): there is minimal performative compliance machinery because the reading's whole point is to avoid requiring compliance apparatus for transformative and intermediate uses. Accessibility collapse is low (0.20) because the doctrinal line leaves rightsholders litigation and legislative-reform avenues open. Resistance is moderate (0.35), reflecting active, organized pushback from rightsholder associations and ongoing litigation contesting where the transformative-use line should sit — this is a doctrinal boundary genuinely under contest, not a settled fact.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (transformative creators, ML developers, platforms) the reading looks like straightforward, low-cost coordination infrastructure — a bright-enough line that lets productive activity proceed. From the rightsholder payer seat, the same doctrinal boundary looks like an uncompensated transfer of licensing value, particularly as 'transformative' and 'intermediate' are read expansively to cover large-scale commercial ML training. The engine should compute these as structurally different experiences of one boundary, not reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative-use creators, ML developers, and platforms are declared beneficiaries because the coordination reading removes a licensing requirement that would otherwise apply to their activity — this pushes their directionality toward the subsidized end. Original rightsholders are the payer because the same boundary removes their leverage to charge for these uses — directionality pushes toward the target end. Midlist authors are excluded rather than payer-coded as a class stakeholder because their loss is diffuse and unlitigated rather than a direct, organized payer position; they are named to satisfy the absent-voices question, not folded into the beneficiary/victim arrays for a rope reading, since this reading does not carry an active-enforcement or asymmetric-extraction structure that would make it a tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing an over-broad derivative-work right from vetoing all follow-on creative and technical use — remains genuinely live wherever new expressive or technical forms (generative AI being the current instance) depend on using existing works as raw material. Because the problem is still active rather than merely inertially preserved, classifying this reading as a rope rather than a scaffold or piton is defensible: the coordination function it performs is not a relic of a solved problem, it is continuously reapplied to new technological categories the original doctrine never anticipated. The contested status flagged in founding_problem_status registers that some outside observers believe the doctrine has drifted from limiting principle to blanket immunity for commercial-scale extraction — that drift is exactly what the sibling hybrid_carveout_reading is built to correct, and what an enclosure-reading advocate would call capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_derivative_work_boundary,
    'Is the coordination reading (narrow fixation + substantial-incorporation test, transformative/intermediate uses categorically non-infringing) the operative reading of the derivative-work boundary, or does actual enforcement track the enclosure reading (any use in creating new work is derivative-work preparation) or the hybrid carveout reading (commercial/non-commercial line)?',
    'Track circuit-split resolution and Supreme Court guidance on transformative-use scope for generative AI training specifically; a ruling that treats model training as itself a fixed recasting substantially incorporating expression would shift the operative reading toward enclosure or hybrid_carveout for that use case.',
    'If courts converge on the enclosure or hybrid reading for ML training specifically, this coordination-reading story''s low ε for machine_learning_developers becomes descriptively false for that stakeholder going forward, and the network edge to the sibling readings should be read as a live doctrinal transition rather than a stable multi-reading equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_derivative_work_boundary, conceptual, 'Which of the three kernel readings actually governs current enforcement, especially for ML training.').

omega_variable(
    transformative_use_boundary_erosion,
    'Where precisely does ''transformative'' stop and ''substantially incorporating fixed recasting'' begin, especially for generative outputs that can reproduce near-verbatim passages of training data under certain prompts?',
    'Empirical study of generative model output overlap with training corpora (memorization studies) combined with case-by-case judicial application of the transformative-use test to specific outputs.',
    'If outputs are shown to substantially incorporate original expression at scale, some currently-protected ''intermediate use'' training activity may be reclassified as producing derivative works, narrowing this reading''s beneficiary set and raising its effective ε for ML developers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_boundary_erosion, empirical, 'Whether the fixation/incorporation line holds up against generative-model memorization evidence.').

omega_variable(
    diffuse_author_harm_measurement,
    'Does the coordination reading impose a measurable, aggregable harm on midlist and freelance authors (lost licensing revenue, market substitution) large enough to warrant payer-class status rather than excluded-voice status?',
    'Economic studies of licensing-market displacement for authors and illustrators following expansion of transformative-use and ML-training carveouts; class-action or collective bargaining data if it emerges.',
    'If harm is shown to be large and concentrated rather than genuinely diffuse, midlist_and_freelance_authors should be reclassified from excluded to payer, and the base_properties.victims array revisited — potentially reclassifying this reading''s structural type from rope toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diffuse_author_harm_measurement, empirical, 'Whether diffuse-author harm is large enough to warrant reclassification as a named payer class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(deri_tr_t6, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 6, 0.06).
narrative_ontology:measurement(deri_tr_t12, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 12, 0.07).
narrative_ontology:measurement(deri_tr_t18, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 18, 0.08).
narrative_ontology:measurement(deri_tr_t24, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(deri_tr_t30, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(deri_be_t6, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 6, 0.11).
narrative_ontology:measurement(deri_be_t12, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 12, 0.13).
narrative_ontology:measurement(deri_be_t18, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 18, 0.15).
narrative_ontology:measurement(deri_be_t24, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(deri_be_t30, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 30, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(derivative_work_statutory_boundary__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__coordination_reading, 0.1).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the natural-language 'derivative work boundary' kernel: this coordination_reading (narrow fixation + substantial-incorporation test, low ε, rope), enclosure_reading (any use in creating new work is derivative-work preparation, high ε, likely snare or tangled_rope), and hybrid_carveout_reading (commercial/non-commercial split, moderate ε, likely tangled_rope or scaffold). Each reading shares the same underlying statutory text and case law but produces a structurally distinct constraint with its own beneficiary/victim set, its own enforcement posture, and its own ε — per the ε-invariance principle, they are not the same constraint measured differently, they are three constraints linked by a shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
