% ============================================================================
% CONSTRAINT STORY: expressive_attribution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_expressive_attribution_reading, []).

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
 *   constraint_id: expressive_attribution_reading
 *   human_readable: Firm-Speech Attribution Shield for Algorithmic Curation
 *   domain: law_and_technology/first_amendment/products_liability
 *
 * SUMMARY:
 *   A firm whose ranking or recommendation algorithm generates content is
 *   sued or regulated for a harm traceable to that output. The firm asserts
 *   that because the output is attributable to the firm itself — not passed
 *   through neutrally and not authored by a third party — the output is the
 *   firm's own expressive speech, protected against compelled alteration,
 *   liability theories premised on defect, and regulatory mandate under the
 *   First Amendment. This is one of four structurally distinct readings of
 *   the same underlying attribution fact pattern: the
 *   products_liability_reading treats the same algorithmic output as a
 *   defective product; the conduct_regulation_reading treats it as regulable
 *   business conduct incidental to speech; the technician_intent_reading asks
 *   whether any human's specific expressive intent attaches to the particular
 *   output. Each reading routes the same facts through a different doctrinal
 *   gate with a different victim set and a different beneficiary. This story
 *   authors only the expressive_attribution_reading: ε reflects how much this
 *   reading, on its own terms, extracts from the parties it is asserted
 *   against (injured users, plaintiffs, regulators) when courts adopt it —
 *   not a blended or averaged ε across the four readings, and not the ε of
 *   any reading's endorsed alternative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(expressive_attribution_reading, 0.68).
domain_priors:suppression_score(expressive_attribution_reading, 0.6).
domain_priors:theater_ratio(expressive_attribution_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(expressive_attribution_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(expressive_attribution_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(expressive_attribution_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(expressive_attribution_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(expressive_attribution_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(expressive_attribution_reading, tangled_rope).
narrative_ontology:human_readable(expressive_attribution_reading, "Firm-Speech Attribution Shield for Algorithmic Curation").
narrative_ontology:topic_domain(expressive_attribution_reading, "law_and_technology/first_amendment/products_liability").

domain_priors:requires_active_enforcement(expressive_attribution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(expressive_attribution_reading, '8104d24e-6f27-491e-bcf4-b72bd6cc6842').
narrative_ontology:cs_kernel_codification('8104d24e-6f27-491e-bcf4-b72bd6cc6842', distributed).
narrative_ontology:cs_authority_grounding('8104d24e-6f27-491e-bcf4-b72bd6cc6842', lineage).
narrative_ontology:cs_interpretation_layer_present('8104d24e-6f27-491e-bcf4-b72bd6cc6842').
narrative_ontology:cs_reading_relation('8104d24e-6f27-491e-bcf4-b72bd6cc6842', expressive_attribution_reading__products_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('8104d24e-6f27-491e-bcf4-b72bd6cc6842', expressive_attribution_reading__conduct_regulation_reading, influences).
narrative_ontology:cs_reading_relation('8104d24e-6f27-491e-bcf4-b72bd6cc6842', expressive_attribution_reading__technician_intent_reading, coexists_with).
narrative_ontology:cs_axiom('8104d24e-6f27-491e-bcf4-b72bd6cc6842', foundational, algorithmic_curation_is_firm_expression).
narrative_ontology:cs_axiom_status(algorithmic_curation_is_firm_expression, holdable).
narrative_ontology:cs_axiom_grounding('8104d24e-6f27-491e-bcf4-b72bd6cc6842', algorithmic_curation_is_firm_expression, deontological).
narrative_ontology:cs_axiom('8104d24e-6f27-491e-bcf4-b72bd6cc6842', secondary, attribution_alone_establishes_expressive_intent).
narrative_ontology:cs_axiom_status(attribution_alone_establishes_expressive_intent, holdable).
narrative_ontology:cs_axiom_grounding('8104d24e-6f27-491e-bcf4-b72bd6cc6842', attribution_alone_establishes_expressive_intent, conventional).
narrative_ontology:cs_reference_frame('8104d24e-6f27-491e-bcf4-b72bd6cc6842', editorial_judgment_paradigm).
narrative_ontology:cs_drift_state('8104d24e-6f27-491e-bcf4-b72bd6cc6842', algorithmic_curation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8104d24e-6f27-491e-bcf4-b72bd6cc6842', '').
narrative_ontology:cs_kernel_id(expressive_attribution_reading, algorithmic_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(expressive_attribution_reading, platform_operators).
narrative_ontology:constraint_beneficiary(expressive_attribution_reading, platform_shareholders).
narrative_ontology:constraint_victim(expressive_attribution_reading, algorithmically_injured_users).
narrative_ontology:constraint_victim(expressive_attribution_reading, products_liability_plaintiffs).
narrative_ontology:constraint_victim(expressive_attribution_reading, state_regulators).
narrative_ontology:constraint_vindicates(expressive_attribution_reading, editorial_discretion_doctrine).
narrative_ontology:constraint_vindicates(expressive_attribution_reading, compelled_speech_prohibition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and deploy the ranking/recommendation system, litigate its classification as first-party expressive output, and control the doctrinal framing offered to courts. Directly captures the liability shield and the associated reduction in compliance and defense costs; also sets the terms on which the attribution claim is asserted case by case.
narrative_ontology:constraint_stakeholder(expressive_attribution_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(expressive_attribution_reading, platform_operators, beneficiary).

% Benefit from reduced tort exposure and regulatory constraint on the firm's core product; can exit the investment relatively freely if the doctrine fails, but currently capture the valuation upside of insulated algorithmic curation.
narrative_ontology:constraint_stakeholder(expressive_attribution_reading, platform_shareholders, beneficiary,
    organized, biographical, mobile, global).

% Suffer concrete harm traceable to a specific algorithmic output (e.g., harmful recommendation, defective ranking) and seek redress through products liability or negligence theories. The expressive-attribution reading converts the platform's conduct into protected speech, foreclosing the liability theory that would otherwise apply; individual users have no practical way to litigate around the constitutional characterization.
narrative_ontology:constraint_stakeholder(expressive_attribution_reading, algorithmically_injured_users, payer,
    powerless, immediate, trapped, national).

% Plaintiffs' counsel and injured parties who would otherwise frame the algorithm as a product with a design defect. Under this reading, the same conduct is recharacterized as editorial judgment, which routes the claim out of products liability doctrine entirely and into a First Amendment analysis the plaintiff is structurally disadvantaged to win. They can still sue, but the theory that would work is foreclosed.
narrative_ontology:constraint_stakeholder(expressive_attribution_reading, products_liability_plaintiffs, payer,
    moderate, biographical, constrained, national).

% Attempt to impose safety, transparency, or algorithmic-accountability requirements on ranking systems. Under this reading, such requirements risk being struck down as compelled-speech violations or content-based restrictions on protected expression, constraining the regulatory toolkit available even where the underlying harm is analogous to a defective consumer product.
narrative_ontology:constraint_stakeholder(expressive_attribution_reading, state_regulators, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(expressive_attribution_reading, state_regulators, excluded).

% Adjudicate whether a given algorithmic output is first-party expressive speech, third-party conduit content, or product conduct. Their doctrinal choice determines which of the sibling readings governs a given case; they do not benefit from or pay into the arrangement but their rulings allocate who does.
narrative_ontology:constraint_stakeholder(expressive_attribution_reading, courts, observer,
    institutional, generational, analytical, national).

% Support strong speech protections in principle but are largely absent from cases where the expressive-attribution reading is asserted defensively by a firm resisting liability for a discrete injury, rather than in classic government-censorship postures; their doctrinal voice is present in briefing but their interest in protecting individual speakers is not well matched to the firm's institutional posture.
narrative_ontology:constraint_stakeholder(expressive_attribution_reading, civil_liberties_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(expressive_attribution_reading, platform_operators).
narrative_ontology:fixing_cost_class(expressive_attribution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides courts a workable doctrinal test for distinguishing a platform's own expressive judgments (which the First Amendment protects from compelled alteration) from third-party content the platform merely hosts or transmits — a genuine line-drawing problem in an information environment where firms curate at scale.
% TRANSFER_FUNCTION: Moves litigation risk and regulatory exposure away from the firm and onto injured users, products liability plaintiffs, and regulators, by recharacterizing algorithmic conduct as protected first-party expression rather than as a product or service subject to ordinary liability and oversight regimes.
% ABSENT_VOICES: Individual users harmed by specific algorithmic outputs rarely appear as parties in the doctrinal cases that establish this reading; the doctrine is typically built in facial challenges brought by the platforms themselves against regulatory statutes, not in the individual injury suits it later forecloses.
% DISAPPEARANCE_RATIONALE: If courts stopped treating algorithmic curation as the firm's own expressive speech, platforms would face products-liability and regulatory exposure comparable to other complex technical systems; compliance costs would rise, some ranking practices would be redesigned around liability risk, and regulators would regain tools currently foreclosed by the First Amendment characterization.
% FOUNDING_PROBLEM: Courts needed a doctrine to protect genuine editorial judgments (e.g., a newspaper's decision what to print) from government compulsion, at a moment when algorithmic curation began performing functions structurally similar to editorial selection at unprecedented scale.
% FOUNDING_PROBLEM_CORROBORATION: Platforms and allied First Amendment scholars attest the editorial-judgment analogy is sound and the problem (protecting expressive judgment from compelled alteration) remains live. Products liability scholars, disability and consumer advocates, and several dissenting judicial opinions attest that the doctrine has drifted from protecting identifiable expressive choices toward shielding automated systems whose outputs no human reviews before publication — corroboration from outside the benefiting platforms exists but is contested, not unanimous.
narrative_ontology:disappearance_verdict(expressive_attribution_reading, world_rearranges).
narrative_ontology:founding_problem_status(expressive_attribution_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(expressive_attribution_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-13',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(expressive_attribution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(expressive_attribution_reading, 0.68, 'claude-sonnet-5', 'algorithmic_authorless_harm_2026_20260813_215102', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expressive_attribution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(expressive_attribution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(expressive_attribution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects that adoption of this reading systematically forecloses two distinct redress channels (tort liability, regulatory mandate) for the same underlying harm, redirecting cost from the firm to injured parties and the public regulatory apparatus. Suppression (0.6) is substantial but not maximal: the doctrine operates through litigation and precedent rather than direct coercion, and alternative doctrinal readings remain live in other jurisdictions and circuits. Theater ratio (0.42) is elevated because a portion of the doctrine's operation is genuinely functional (protecting authentic editorial judgments, e.g. curated news homepages) while a growing share defends outputs no human reviewed before publication — the editorial framing performs coverage for conduct increasingly distant from the paradigm case (a newspaper editor's deliberate choice) that motivated the doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are the structural beneficiary: they assert the doctrine defensively, capture the liability shield, and retain full control over algorithm design without corresponding tort or regulatory exposure — d sits near the beneficiary end. Injured users and products liability plaintiffs are full targets: the doctrine's operation directly forecloses the theory that would let them recover, and they are trapped or constrained in their capacity to route around the constitutional characterization. State regulators are targets in a different register — d reflects that the doctrine constrains their toolset (a form of institutional extraction of regulatory capacity) even though no direct financial transfer occurs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting genuine editorial judgment from government compulsion) remains partially live — some curation retains the deliberative, reviewable character of classic editorial choice. But the doctrine's application has drifted toward outputs generated by systems no human reviews before publication, where the analogy to a newspaper editor's judgment is strained. Classifying this as tangled_rope rather than a clean rope or snare preserves the genuine coordination function (courts do need SOME way to distinguish protected expression from regulable conduct) while registering that the same structure now systematically transfers liability exposure away from the party best positioned to bear or price it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    editorial_judgment_vs_automated_output_boundary,
    'Does the expressive-attribution reading require an identifiable human expressive judgment behind a specific output, or does firm-level attribution alone suffice even when no human reviewed the particular output before publication?',
    'Track how courts treat cases where discovery establishes no human reviewed the specific challenged output before it was served to a user; a doctrine that survives that fact pattern unmodified reveals attribution-alone is doing the work, not editorial judgment.',
    'If human review is required and largely absent in contested cases, the expressive-attribution reading is being extended well past its founding rationale, supporting the tangled_rope classification''s extraction finding. If courts require and find genuine human judgment, the reading is closer to a clean rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(editorial_judgment_vs_automated_output_boundary, empirical, 'Whether the doctrine''s application tracks genuine editorial deliberation or firm attribution alone.').

omega_variable(
    kernel_reading_selection_mechanism,
    'What determines which of the four sibling readings (expressive_attribution, products_liability, conduct_regulation, technician_intent) a given court adopts for structurally similar facts?',
    'Comparative analysis of circuit splits and case outcomes controlling for the type of harm, type of algorithm, and posture (facial challenge to a statute vs. individual injury suit) to see whether reading selection tracks doctrinal principle or litigation posture and forum.',
    'If reading selection tracks posture (facial challenges favor expressive_attribution; individual injury suits favor products_liability) rather than principled doctrinal distinction, the kernel is unsettled in a way that materially advantages whichever party controls case framing and forum selection — typically the firm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, conceptual, 'What drives court selection among the four sibling readings of algorithmic attribution.').

omega_variable(
    regulatory_capacity_extraction_measurement,
    'How should the extraction of regulatory capacity from state_regulators be measured, given it is not a financial transfer but a foreclosure of policy tools?',
    'Compare the scope of algorithmic accountability legislation enacted vs. struck down or chilled in jurisdictions where this reading is dominant, against jurisdictions or eras where it was not yet established.',
    'A clear before/after gap in enacted-and-surviving regulation would support treating regulatory foreclosure as a substantial component of this reading''s extractiveness, distinct from and additive to the tort-liability foreclosure component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capacity_extraction_measurement, empirical, 'Whether foreclosed regulatory capacity should be weighted comparably to foreclosed tort liability in the extraction measure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(expressive_attribution_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(expr_tr_t0, expressive_attribution_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(expr_tr_t4, expressive_attribution_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(expr_tr_t8, expressive_attribution_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(expr_tr_t12, expressive_attribution_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(expr_tr_t16, expressive_attribution_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(expr_tr_t20, expressive_attribution_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(expr_tr_t24, expressive_attribution_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(expr_be_t0, expressive_attribution_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(expr_be_t4, expressive_attribution_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(expr_be_t8, expressive_attribution_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(expr_be_t12, expressive_attribution_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(expr_be_t16, expressive_attribution_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(expr_be_t20, expressive_attribution_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(expr_be_t24, expressive_attribution_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(expr_su_t0, expressive_attribution_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(expr_su_t4, expressive_attribution_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(expr_su_t8, expressive_attribution_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(expr_su_t12, expressive_attribution_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(expr_su_t16, expressive_attribution_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(expr_su_t20, expressive_attribution_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(expr_su_t24, expressive_attribution_reading, suppression_requirement, 24, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(expressive_attribution_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(expressive_attribution_reading, products_liability_reading).
narrative_ontology:affects_constraint(expressive_attribution_reading, conduct_regulation_reading).
narrative_ontology:affects_constraint(expressive_attribution_reading, technician_intent_reading).

% DUAL FORMULATION NOTE:
% This story is one of four linked readings of the algorithmic_attribution kernel, decomposed per the ε-invariance principle because the same underlying fact pattern (a firm's algorithm produces content attributed to the firm and causes contested harm) yields materially different ε, beneficiary sets, and victim sets depending on which doctrinal frame governs. expressive_attribution_reading (this file) removes the firm from the liability-facing set and installs it as a rights-holder; products_liability_reading keeps the firm inside an ordinary defect-liability frame; conduct_regulation_reading treats the algorithm as regulable business conduct with incidental expressive elements; technician_intent_reading conditions liability on locating a specific human's expressive intent behind the challenged output. Each is authored as its own file with its own claimed_type and metrics; this file's network edges point to all three siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
