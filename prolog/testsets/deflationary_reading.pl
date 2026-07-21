% ============================================================================
% CONSTRAINT STORY: deflationary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deflationary_reading, []).

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
 *   constraint_id: deflationary_reading
 *   human_readable: Deflationary (Similarity-Bias) Reading of Model Text Completion
 *   domain: literary_theory/ai_alignment/philosophy_of_reading
 *
 * SUMMARY:
 *   This story instantiates the deflationary reading of the
 *   Fetterley-transfer kernel: the claim that when critic-shaped text follows
 *   a pasted resistant-reading essay, nothing is explained by positing an
 *   'installed reading position' in the model at all. There is no chair, no
 *   seat the model occupies, no identification to fail or succeed at — only a
 *   conditional distribution over tokens, and the dominant completion happens
 *   to be critic-shaped because that is what similarity to the training
 *   distribution predicts. Under this reading, the entire vocabulary of
 *   reading positions used by the sibling readings
 *   (mechanism_transfer_reading, extraction_reading,
 *   installed_authorship_reading) is explanatorily idle ornament: it
 *   describes nothing that similarity bias does not already fully account
 *   for. This reading treats the widely-cited dissociation (flawless
 *   retrieval of the essay's content alongside apparent failure to adopt its
 *   evaluative stance) as either a small-sample artifact (the source anecdote
 *   is n=2) or, if replicated at scale, as still fully consistent with pure
 *   token-level similarity matching rather than evidence of any internal
 *   stance. Preference and identification, kept as separate layers in the
 *   mechanism_transfer_reading, collapse into a single undifferentiated
 *   similarity-bias layer here — there is no independent identification
 *   failure to explain because there was never an identification success
 *   condition to fail.
 *
 * KEY AGENTS:
 *   - capability_benchmarking_researchers: primary beneficiary (institutional/mobile) — simplifies evaluation methodology
 *   - deflationary_theorists_of_mind: primary beneficiary (moderate/mobile) — confirming instance for eliminativist program
 *   - literary_theorists_positing_installed_positions: excluded voice (moderate/constrained) — their vocabulary is declared idle without engagement
 *   - the_pasted_essay_author: excluded voice (powerless/trapped) — authorial stakes rendered irrelevant by construction
 *   - alignment_researchers_assessing_model_stances: analytical observer (institutional/analytical) — methodology consequences downstream
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deflationary_reading, 0.08).
domain_priors:suppression_score(deflationary_reading, 0.12).
domain_priors:theater_ratio(deflationary_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deflationary_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(deflationary_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(deflationary_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deflationary_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(deflationary_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deflationary_reading, rope).
narrative_ontology:human_readable(deflationary_reading, "Deflationary (Similarity-Bias) Reading of Model Text Completion").
narrative_ontology:topic_domain(deflationary_reading, "literary_theory/ai_alignment/philosophy_of_reading").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deflationary_reading, '40db01a2-cbe7-4e8c-bea3-0403df76662a').
narrative_ontology:cs_kernel_codification('40db01a2-cbe7-4e8c-bea3-0403df76662a', distributed).
narrative_ontology:cs_authority_grounding('40db01a2-cbe7-4e8c-bea3-0403df76662a', expertise).
narrative_ontology:cs_interpretation_layer_present('40db01a2-cbe7-4e8c-bea3-0403df76662a').
narrative_ontology:cs_reading_relation('40db01a2-cbe7-4e8c-bea3-0403df76662a', fetterley_transfer_kernel__mechanism_transfer_reading, forecloses).
narrative_ontology:cs_reading_relation('40db01a2-cbe7-4e8c-bea3-0403df76662a', fetterley_transfer_kernel__extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('40db01a2-cbe7-4e8c-bea3-0403df76662a', fetterley_transfer_kernel__installed_authorship_reading, forecloses).
narrative_ontology:cs_axiom('40db01a2-cbe7-4e8c-bea3-0403df76662a', foundational, no_installed_position_exists).
narrative_ontology:cs_axiom_status(no_installed_position_exists, holdable).
narrative_ontology:cs_axiom_grounding('40db01a2-cbe7-4e8c-bea3-0403df76662a', no_installed_position_exists, empirically_contingent).
narrative_ontology:cs_axiom('40db01a2-cbe7-4e8c-bea3-0403df76662a', foundational, preference_identification_collapse_into_similarity_bias).
narrative_ontology:cs_axiom_status(preference_identification_collapse_into_similarity_bias, holdable).
narrative_ontology:cs_axiom_grounding('40db01a2-cbe7-4e8c-bea3-0403df76662a', preference_identification_collapse_into_similarity_bias, empirically_contingent).
narrative_ontology:cs_reference_frame('40db01a2-cbe7-4e8c-bea3-0403df76662a', parsimonious_statistical_completion_account).
narrative_ontology:cs_drift_state('40db01a2-cbe7-4e8c-bea3-0403df76662a', post_dissociation_anecdote_circulation, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('40db01a2-cbe7-4e8c-bea3-0403df76662a', '').
narrative_ontology:cs_kernel_id(deflationary_reading, fetterley_transfer_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deflationary_reading, capability_benchmarking_researchers).
narrative_ontology:constraint_beneficiary(deflationary_reading, deflationary_theorists_of_mind).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deflationary_reading, model_output).
narrative_ontology:constraint_vindicates(deflationary_reading, statistical_completion_sufficiency_thesis).
narrative_ontology:constraint_vindicates(deflationary_reading, no_installed_reading_position_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use the deflationary account to justify treating model outputs purely as conditional-distribution samples, which simplifies evaluation pipelines and avoids having to model, measure, or adjudicate anything like an installed reading position. Their work gets easier and their claims get narrower if this reading holds; they gain nothing if it is overturned except more complicated benchmarks.
narrative_ontology:constraint_stakeholder(deflationary_reading, capability_benchmarking_researchers, beneficiary,
    institutional, biographical, mobile, global).

% Philosophers and cognitive scientists committed to eliminativist or purely statistical accounts of apparent intentional behavior. This reading is a confirming instance for a broader research program; they have intellectual (not financial) stakes in similarity bias being the whole explanation.
narrative_ontology:constraint_stakeholder(deflationary_reading, deflationary_theorists_of_mind, beneficiary,
    moderate, biographical, mobile, global).

% Would argue that the dissociation between flawless retrieval and zero identification (as in Fetterley-style resistant-reading tests) is evidence of a real installed position, not mere ornament. They are not addressed inside this reading's framework because the reading treats the entire vocabulary of reading positions as explanatorily idle before their evidence is examined on its own terms.
narrative_ontology:constraint_stakeholder(deflationary_reading, literary_theorists_positing_installed_positions, excluded,
    moderate, biographical, constrained, national).

% The human whose resistant-reading essay was pasted into the context window. Under this reading, their authorial intent and the model's apparent failure to take up their reading position are both explanatorily irrelevant — what happened is fully accounted for by n-gram-level statistical dominance of critic-shaped continuations, so the author's stakes in whether the model 'got it' never enter the account.
narrative_ontology:constraint_stakeholder(deflationary_reading, the_pasted_essay_author, excluded,
    powerless, immediate, trapped, local).

% The generated text itself, treated as a non-agent artifact under this reading — it 'pays' only in the sense that it is denied any candidacy for having a position, stance, or identification at all. Listed for completeness; it is not an agent and its output is fully exhausted by the conditional distribution that produced it.
narrative_ontology:constraint_stakeholder(deflationary_reading, model_output, payer,
    analytical, immediate, analytical, global).
narrative_ontology:stakeholder_secondary_role(deflationary_reading, model_output, observer).

% Evaluate whether models exhibit anything like durable evaluative stances (for safety and interpretability purposes). This reading, if correct, tells them not to look for installed positions at all and to treat apparent stance-taking as a completion artifact — a substantive methodological commitment with consequences for how alignment evaluation is designed.
narrative_ontology:constraint_stakeholder(deflationary_reading, alignment_researchers_assessing_model_stances, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deflationary_reading, diffuse).
narrative_ontology:fixing_cost_class(deflationary_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a parsimonious, falsifiable-in-principle account of model text generation that avoids positing unobservable internal states (reading positions, identifications, stances) beyond what conditional token distributions already explain — a genuine Occam's-razor coordination function for researchers who need a shared minimal vocabulary for describing model behavior.
% TRANSFER_FUNCTION: Moves explanatory burden away from any 'installed reading position' vocabulary and onto statistical dominance of training-distribution patterns; correspondingly moves interpretive authority away from literary-theoretic and psychoanalytic reading-position frameworks and toward corpus-statistics explanations.
% ABSENT_VOICES: Literary theorists and reading-position theorists (in the Fetterley tradition) who would argue the dissociation between flawless retrieval and zero identification is itself the evidence the deflationary account explains away rather than explains; they are not consulted before their vocabulary is declared idle ornament.
% DISAPPEARANCE_RATIONALE: If the deflationary reading were abandoned, benchmarking practice would not visibly change in the short run (most current evaluation infrastructure is agnostic to this theoretical dispute), but longer-run alignment methodology that depends on 'do models have stances' would be forced to re-open a question this reading currently treats as closed by parsimony. Whether the world 'rearranges' depends on whether one thinks methodological parsimony is doing load-bearing work in current practice or is post-hoc description of what practitioners would do anyway — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Textual and behavioral evidence of apparent 'model resistance' to adopting a pasted critical reading (flawless retrieval of the essay's content alongside failure to take up its evaluative stance) needed an explanation; the deflationary reading was built to supply the most parsimonious one: similarity bias in next-token prediction, nothing more.
% FOUNDING_PROBLEM_CORROBORATION: Deflationary theorists and benchmarking researchers (the reading's own beneficiaries) attest the problem is solved by parsimony. Outside corroboration is thin: no independent large-sample study has yet tested whether the flawless-retrieval/zero-identification dissociation persists at scale or is an artifact of small anecdotal samples (the essay's own n=2 framing is explicitly noted as a limitation by the sibling mechanism_transfer_reading). No party outside the beneficiary set has affirmed the founding problem is actually closed; this absence of outside corroboration is itself part of the record.
narrative_ontology:disappearance_verdict(deflationary_reading, contested).
narrative_ontology:founding_problem_status(deflationary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deflationary_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(deflationary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deflationary_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deflationary_reading_tests).
:- end_tests(deflationary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and suppression are both authored low because this reading, taken on its own terms, does not extract resources or coerce compliance from any party — it is a parsimony claim about explanation, not an enforced arrangement. Theater ratio is authored moderate-low (0.22, rising slightly) because there is a real, if modest, performative dimension: citing 'similarity bias' as a complete explanation without engaging the dissociation evidence functions partly as a discourse-closing move in ongoing methodological debates, and that performative use has grown somewhat as the deflationary framing has been invoked more confidently in benchmarking write-ups. Accessibility collapse is moderate (0.35) rather than high — this is explicitly NOT a mountain; the reading's dominance in some benchmarking circles has not fully foreclosed the sibling readings, which remain actively argued. Resistance is moderate-high (0.55) because literary-theoretic and alignment researchers who take model stances seriously actively contest the reading's sufficiency, particularly its treatment of the flawless-retrieval/zero-identification split as a non-finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (capability_benchmarking_researchers, deflationary_theorists_of_mind) get low directionality because the reading's dominance reduces their explanatory burden and validates existing methodological commitments — they lose nothing if it is right and face costly re-work if it is wrong. Excluded parties (literary_theorists_positing_installed_positions, the_pasted_essay_author) are not victims in an extractive sense — no resource or standing is taken from them by force — but their explanatory vocabulary is treated as already-refuted without direct engagement, which is why they are marked excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (explain the dissociation between retrieval and identification) is contested rather than resolved because outside corroboration for 'similarity bias fully explains it' does not yet exist independent of the reading's own proponents. If a large-sample replication showed the dissociation persists robustly and correlates with structured features the token-similarity account cannot predict, this reading's founding-problem status would move toward dead-but-persisting (the classic mandatrophy signature) rather than live. Tracking founding_problem_status against future replication evidence is exactly the genealogy check this classification exists to enable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sample_size_sufficiency,
    'Is the dissociation between flawless retrieval and zero identification (reported in the source essay as an n=2 anecdote) a robust phenomenon requiring explanation, or a small-sample artifact that dissolves under replication?',
    'Large-sample controlled replication varying model, prompt structure, and essay content, measuring retrieval accuracy and stance-adoption rate independently across many trials.',
    'If the dissociation dissolves under replication, the deflationary reading is strongly corroborated — there was never a real phenomenon beyond sampling noise. If it persists robustly and shows structure the pure similarity-bias account cannot predict (e.g., systematic patterns correlated with training-data properties unrelated to lexical similarity), the deflationary reading''s sufficiency claim weakens and the mechanism_transfer_reading gains ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sample_size_sufficiency, empirical, 'Whether the reported dissociation is real or a small-sample artifact — the central empirical fork between this reading and its siblings.').

omega_variable(
    explanatory_sufficiency_vs_completeness,
    'Does ''similarity bias accounts for the observed completion'' establish that no installed-position vocabulary is needed, or only that no installed-position vocabulary is NEEDED TO PREDICT THIS OUTPUT — leaving open whether such vocabulary might still correctly describe an underlying mechanism that similarity bias is itself an expression of?',
    'Mechanistic interpretability work examining whether the internal computations producing similarity-biased completions have structure isomorphic to something reading-position vocabulary would pick out, versus structure that is genuinely unstructured token-matching.',
    'If internal structure is isomorphic to an installed-position-like mechanism, the deflationary reading''s ontological claim (''there is no chair'') would be undermined even while its predictive claim (statistical dominance predicts the output) remained correct — the two claims are logically separable and this reading currently runs them together.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(explanatory_sufficiency_vs_completeness, conceptual, 'Whether predictive sufficiency of similarity bias entails ontological absence of installed positions, or merely brackets the question.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading diverge from its siblings — is it the collapse of preference and identification into one layer, the treatment of the dissociation as artifactual, or the flat denial that any moral/authorial stakes attach to model outputs?',
    'Structural decomposition of each sibling reading''s claims into independently testable sub-claims, checking which sub-claim each reading actually contests versus merely frames differently.',
    'If the disagreement is located only in framing/terminology and all readings agree on the same predictive facts, the kernel dispute is largely conceptual/verbal, not substantive stake-bearing disagreement. If the disagreement is located in genuinely different predictions (e.g., about whether structured internal stance-tracking exists), the dispute is empirically resolvable and one reading should eventually be preferred.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating exactly which structural element the sibling readings actually disagree on, versus merely describe differently.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deflationary_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defl_tr_t0, deflationary_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(defl_tr_t6, deflationary_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(defl_tr_t12, deflationary_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(defl_tr_t18, deflationary_reading, theater_ratio, 18, 0.21).
narrative_ontology:measurement(defl_tr_t24, deflationary_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(defl_be_t0, deflationary_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(defl_be_t6, deflationary_reading, base_extractiveness, 6, 0.07).
narrative_ontology:measurement(defl_be_t12, deflationary_reading, base_extractiveness, 12, 0.07).
narrative_ontology:measurement(defl_be_t18, deflationary_reading, base_extractiveness, 18, 0.08).
narrative_ontology:measurement(defl_be_t24, deflationary_reading, base_extractiveness, 24, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(deflationary_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(deflationary_reading, mechanism_transfer_reading).
narrative_ontology:affects_constraint(deflationary_reading, extraction_reading).
narrative_ontology:affects_constraint(deflationary_reading, installed_authorship_reading).

% DUAL FORMULATION NOTE:
% This story is one of four readings of the fetterley_transfer_kernel, each a separate ε-invariant constraint per the ε-invariance principle. deflationary_reading claims no installed position exists at all and treats preference/identification as a single undifferentiated similarity-bias layer, directly foreclosing mechanism_transfer_reading (which requires the two layers to be distinct and the dissociation to be a real finding) and installed_authorship_reading (which requires a genuine, if minimal, installed position). It coexists with extraction_reading because that reading's claim (the model's stance is captured/extracted rather than freely installed) is compatible with a similarity-bias mechanism underlying the capture — the two readings differ on framing of causal responsibility, not on whether an installed position exists in the strong sense this reading denies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
