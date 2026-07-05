% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Derivative Work Boundary — Fixed-Recasting/Substantial-Incorporation Reading (Transformative Use Non-Infringing)
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This story instantiates the coordination reading of the derivative-work
 *   statutory boundary: under this reading, only fixed recastings that
 *   substantially incorporate an original's protected expression count as
 *   derivative works, while transformative uses (parody, commentary,
 *   technical intermediate copying, and — per this reading's extension — ML
 *   training on large corpora) fall outside the definition and require no
 *   ex-ante license. This is deliberately the low-ε member of the kernel
 *   family: no licensing bottleneck, broad downstream latitude, and a genuine
 *   coordination function (predictable clearance line that lets creators and
 *   technologists invest without negotiating every incidental touch of prior
 *   expression). The enclosure reading and hybrid carveout reading are
 *   separate constraints with their own files and their own ε; this story
 *   does not average across them or hedge its ε to accommodate them.
 *
 * KEY AGENTS:
 *   - ml_model_developers: beneficiary (organized/mobile) — trains on corpora without ex-ante clearance
 *   - software_developers: beneficiary (organized/mobile) — intermediate copying treated as non-derivative
 *   - original_rightsholders: payer (organized/constrained) — bears foregone licensing revenue
 *   - courts_and_copyright_office: agenda_setter (institutional/analytical) — draws and administers the line
 *   - licensing_intermediaries: excluded (organized/constrained) — business model shrinks under this reading
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
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Boundary — Fixed-Recasting/Substantial-Incorporation Reading (Transformative Use Non-Infringing)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property_law/technology_governance/information_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, '0a34fcf4-1426-484a-bb0c-b10d50cb8d15').
narrative_ontology:cs_kernel_codification('0a34fcf4-1426-484a-bb0c-b10d50cb8d15', fixed_text).
narrative_ontology:cs_authority_grounding('0a34fcf4-1426-484a-bb0c-b10d50cb8d15', practice).
narrative_ontology:cs_interpretation_layer_present('0a34fcf4-1426-484a-bb0c-b10d50cb8d15').
narrative_ontology:cs_reading_relation('0a34fcf4-1426-484a-bb0c-b10d50cb8d15', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('0a34fcf4-1426-484a-bb0c-b10d50cb8d15', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('0a34fcf4-1426-484a-bb0c-b10d50cb8d15', foundational, fixation_and_substantial_incorporation_required).
narrative_ontology:cs_axiom_status(fixation_and_substantial_incorporation_required, holdable).
narrative_ontology:cs_axiom_grounding('0a34fcf4-1426-484a-bb0c-b10d50cb8d15', fixation_and_substantial_incorporation_required, conventional).
narrative_ontology:cs_axiom('0a34fcf4-1426-484a-bb0c-b10d50cb8d15', secondary, commercial_status_evidentiary_not_dispositive).
narrative_ontology:cs_axiom_status(commercial_status_evidentiary_not_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('0a34fcf4-1426-484a-bb0c-b10d50cb8d15', commercial_status_evidentiary_not_dispositive, instrumental).
narrative_ontology:cs_reference_frame('0a34fcf4-1426-484a-bb0c-b10d50cb8d15', transformative_use_common_law_synthesis).
narrative_ontology:cs_drift_state('0a34fcf4-1426-484a-bb0c-b10d50cb8d15', generative_ai_training_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('0a34fcf4-1426-484a-bb0c-b10d50cb8d15', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, software_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, ml_model_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, documentary_filmmakers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, parody_and_commentary_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, search_and_indexing_platforms).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, follow_on_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, original_rightsholders).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, fair_use_transformative_use_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, idea_expression_dichotomy).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, progress_clause_purpose_of_copyright).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Train models on large corpora of copyrighted text, images, and code. Under this reading, ingestion for statistical pattern-learning that does not fix a substantial recasting of any single original work in the output falls outside the derivative-work boundary, so training proceeds without ex-ante licensing negotiations with every rightsholder whose material appears in the corpus.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, ml_model_developers, beneficiary,
    organized, generational, mobile, global).

% Build interoperable tools, reverse-engineer interfaces, and create intermediate copies (compiling, caching, indexing) as a necessary step toward non-infringing transformative products. This reading treats those intermediate copies as outside the derivative-work definition, letting development proceed without clearance for every transient copy.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, software_developers, beneficiary,
    organized, generational, mobile, global).

% Incorporate brief clips, commentary, and critique of copyrighted footage. This reading's transformative-use carve-out lets them proceed without licensing every underlying clip, provided the recasting doesn't substantially incorporate fixed original expression as its core content.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, documentary_filmmakers, beneficiary,
    moderate, biographical, constrained, national).

% Produce parody, criticism, and remix works that reference originals but transform their meaning. Under this reading their work is presumptively non-infringing absent fixed substantial incorporation, protecting a low-resource creator class that could never afford ex-ante licensing negotiations.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, parody_and_commentary_creators, beneficiary,
    powerless, immediate, mobile, global).

% Crawl, cache, and index copyrighted content to build search and discovery products. This reading's intermediate-use carve-out is central to their business model — indexing copies are not derivative works if the ultimate output does not fix substantial original expression.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, search_and_indexing_platforms, beneficiary,
    institutional, generational, arbitrage, global).

% Authors, photographers, musicians, and publishers whose works are used as training data, source clips, or reference material without a licensing transaction. They bear the cost of foregone licensing revenue and reduced bargaining leverage over derivative markets they might otherwise have monetized, though they retain infringement claims against outputs that do fix substantial recastings.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, original_rightsholders, payer,
    organized, biographical, constrained, global).

% Collective licensing bodies and rights-clearance agencies whose business model depends on ex-ante transaction requirements. This reading shrinks the set of uses that require their services; they would argue for a broader derivative-work definition but are not parties to the litigation and legislative record that shapes the boundary.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, licensing_intermediaries, excluded,
    organized, biographical, constrained, national).

% Adjudicate the fixed-recasting/substantial-incorporation line case by case, drawing on the statutory definition of derivative work and the transformative-use fair use factor. Their interpretive choices set the effective boundary that all other seats operate under.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, courts_and_copyright_office, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a predictable line between uses that require ex-ante licensing (fixed recastings substantially incorporating original expression) and uses that do not (transformative and intermediate uses), so that follow-on creators, technologists, and platforms can invest in new works and tools without negotiating clearance for every incidental or transformative touch of prior expression.
% TRANSFER_FUNCTION: Moves the default allocation of permission away from original rightsholders and toward downstream users for the class of uses that transform or merely intermediate the original — no payment moves from user to rightsholder for these uses; the rightsholder retains only the narrower fixed-substantial-incorporation domain.
% ABSENT_VOICES: Original rightsholders whose works are heavily used as ML training data had limited voice in shaping this doctrinal line, which developed primarily through litigation involving parody, software interoperability, and search indexing before generative AI existed at scale; licensing intermediaries whose revenue model depends on a broader derivative-work definition are not parties to the courts that draw the line.
% DISAPPEARANCE_RATIONALE: If this reading were replaced overnight by the enclosure reading, ML training would require clearance of every corpus work, search indexing and caching would require licenses, documentary and parody creators would face infringement exposure for any incorporation, and entire industries built on the current transformative-use assumption would need to renegotiate their operating basis or halt.
% FOUNDING_PROBLEM: Copyright's derivative-work right, if read to cover any use of expression in creating something new, would let rightsholders control an unbounded universe of downstream transformation, criticism, and technical processes — chilling speech, technical innovation, and scholarship that depends on engaging with existing works.
% FOUNDING_PROBLEM_CORROBORATION: Federal appellate courts (e.g., in software interoperability and search-indexing fair use rulings) and copyright scholars outside the technology industry attest that the transformative-use boundary continues to serve a live First Amendment and innovation-policy function; the Copyright Office's own public studies on AI training have acknowledged the doctrinal problem as unresolved and actively contested, not manufactured by beneficiary advocacy alone.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.18) because the reading's structural effect is to WITHHOLD a licensing requirement rather than to impose a transfer — the coordination function (predictable non-infringement zone) does not by itself move rents to any concentrated party; the modest upward drift over the interval reflects the accumulating commercial scale of ML training under this reading, which increases the aggregate value flowing away from rightsholders without licensing even though no single transaction is extractive. Suppression is low (0.12): rightsholders retain full infringement claims for fixed substantial-incorporation uses and are not coerced into silence — they simply do not have a claim over the transformative/intermediate category by construction. Accessibility collapse is low (0.2) because rightsholders can still license voluntarily, pursue statutory claims where recasting is substantial, or lobby for legislative change (as ongoing AI-training litigation demonstrates) — the boundary is a legal default, not a total foreclosure. Resistance is moderate (0.35), reflecting live litigation and legislative pressure from rightsholder organizations contesting exactly this reading's application to generative AI training.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (ML developers, software developers, search platforms), this reading operates as pure coordination: a predictable rule permitting investment without case-by-case clearance risk. From the original_rightsholders seat, the same rule looks like a structural non-transfer — value is extracted from their potential licensing market without appearing as extraction in any single transaction, because the boundary itself defines the class of use as non-infringing. The engine should register this as the seat divergence characteristic of a rope viewed from a payer position that would prefer the enclosure reading's classification instead.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (ML developers, software developers, documentary filmmakers, parody creators, search platforms, follow-on innovators) are declared because the reading's entire structural function is to exempt their conduct from the licensing requirement — low directionality, near the beneficiary end. original_rightsholders are the payer group: not through an enforced transfer but through the reading's foreclosure of a licensing market they would otherwise have standing to monetize under the sibling enclosure_reading. licensing_intermediaries are excluded rather than victims because their loss is indirect (reduced transaction volume) rather than a direct extraction from a specific work.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing an unbounded derivative-work right from chilling transformation, criticism, and technical innovation — remains live: transformative and intermediate uses continue to require exactly this kind of default-permission scaffold as new technologies (search, software interoperability, now ML training) repeatedly test the boundary. This is not a mandatrophied rope; the coordination function it was built for (predictable non-infringement zone for follow-on creativity) still operates on the same margin that motivated its early articulation in software and parody cases, now extended by courts to generative AI.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_reading_kernel_disagreement_location,
    'Is this constraint one reading of the derivative_work_statutory_boundary kernel, and if so, where exactly does the coordination_reading''s core premise diverge from the enclosure_reading and hybrid_carveout_reading?',
    'Track circuit splits and Supreme Court certiorari grants on whether ''fixed recasting substantially incorporating original expression'' or ''any use in creating a new work'' is the operative statutory test, and whether commercial purpose is a threshold gate (hybrid) or merely a fair-use factor (coordination). The disagreement is located precisely at: (1) whether intermediate/non-fixed copying can ever trigger the derivative-work definition, and (2) whether commercial exploitation status is dispositive or merely evidentiary.',
    'If courts converge on the enclosure_reading''s premise (any use = preparation), this constraint''s coordination function collapses and the low-ε classification here would need re-evaluation as a separate, later-dated story rather than a retroactive edit to this one. If courts converge on hybrid_carveout_reading, ML training''s non-commercial/commercial boundary becomes dispositive, which this reading''s stakeholders (especially ml_model_developers under commercial deployment) would experience very differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_reading_kernel_disagreement_location, conceptual, 'Locates the structural disagreement between kernel readings at the fixed-recasting threshold and the commercial-purpose gate.').

omega_variable(
    ml_training_generality_uncertainty,
    'Does the transformative-use logic developed for search indexing and software interoperability (non-generative technical processes) genuinely extend to generative ML training, where model outputs can sometimes reproduce close paraphrases of training data?',
    'Empirical study of output-similarity rates across major generative models relative to training corpora, combined with pending litigation outcomes (e.g., authors'' suits against AI developers) that will test whether the fixed-recasting/substantial-incorporation line holds when applied to statistical models rather than deterministic indexing.',
    'If generative outputs are found to substantially incorporate original expression at non-trivial rates, this reading''s extension to ML training becomes empirically shakier even though it remains conceptually coherent — the reading''s ε could rise materially, pointing toward eventual decomposition into a training-specific story rather than treating training as a straightforward extension of prior intermediate-use precedent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ml_training_generality_uncertainty, empirical, 'Whether generative AI output behavior empirically fits the intermediate-use precedent this reading extends to it.').

omega_variable(
    rightsholder_market_foreclosure_ambiguity,
    'Does this reading foreclose a licensing market that rightsholders have a legitimate claim to (an emerging AI-training licensing market), or does it correctly preserve a public-domain-adjacent space that was never rightsholders'' to monetize?',
    'Legislative and regulatory inquiry (e.g., Copyright Office AI studies, congressional hearings) into whether a licensing market for training data existed or was reasonably foreseeable at the time the underlying works were created — a market-existence test analogous to the fourth fair-use factor.',
    'If a licensing market is found to have been reasonably foreseeable and is now being foreclosed by this reading, the beneficiary/payer structure here understates the payer side''s legitimate claim, and part of what is authored as low ε may be under-counted transfer. If no such market was foreseeable, the coordination reading''s low-ε classification is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rightsholder_market_foreclosure_ambiguity, empirical, 'Whether the reading forecloses a legitimate emergent licensing market or preserves an area rightsholders never had a claim to.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(deri_tr_t8, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 8, 0.06).
narrative_ontology:measurement(deri_tr_t16, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 16, 0.07).
narrative_ontology:measurement(deri_tr_t24, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement(deri_tr_t32, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 32, 0.09).
narrative_ontology:measurement(deri_tr_t40, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(deri_be_t8, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 8, 0.11).
narrative_ontology:measurement(deri_be_t16, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 16, 0.13).
narrative_ontology:measurement(deri_be_t24, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 24, 0.15).
narrative_ontology:measurement(deri_be_t32, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 32, 0.17).
narrative_ontology:measurement(deri_be_t40, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 40, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(derivative_work_statutory_boundary__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, information_standard).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__coordination_reading, 0.03).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% This is one of three sibling stories decomposing the natural-language claim 'the derivative-work boundary.' coordination_reading (this file) authors a low-ε rope: no ex-ante licensing for transformative/intermediate use, ML training permissible, coordination scaffold for generative technologies. enclosure_reading authors a high-ε story where any use in creating a new work counts as derivative-work preparation, with rightsholders as beneficiaries and downstream creators/technologists as victims — likely a tangled_rope or snare depending on enforcement. hybrid_carveout_reading authors an intermediate-ε story gated on commercial purpose. Each reading is ε-invariant on its own terms; the three are linked via affects_constraints rather than merged, per the BGS decomposition pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
