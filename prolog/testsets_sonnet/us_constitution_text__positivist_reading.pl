% ============================================================================
% CONSTRAINT STORY: us_constitution_text__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__positivist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: us_constitution_text__positivist_reading
 *   human_readable: Constitutional Validity as Formal-Pedigree Test (Legal Positivist Reading)
 *   domain: legal/constitutional/philosophical
 *
 * SUMMARY:
 *   This constraint is the positivist reading of the constitutional-validity
 *   kernel: constitutional norms bind because they passed through a formally
 *   specified enactment procedure (Article V and its predecessors), not
 *   because they encode correct morality or recover a historically fixed
 *   meaning. Under this reading, a judge asking 'is this constitutional' asks
 *   only 'was this properly enacted,' and treats questions of moral content
 *   or original historical understanding as irrelevant to bindingness,
 *   however relevant they may be to policy debate. This is a genuinely
 *   distinct constraint from the originalist reading (which asks about
 *   historically fixed meaning) and the living-constitutionalist reading
 *   (which asks about evolving societal principle) — the three readings would
 *   resolve identical hard cases differently, and each has a stable,
 *   non-overlapping ε. This story generates only the positivist reading; the
 *   other two are separate constraint files linked via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__positivist_reading, 0.42).
domain_priors:suppression_score(us_constitution_text__positivist_reading, 0.55).
domain_priors:theater_ratio(us_constitution_text__positivist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__positivist_reading, "Constitutional Validity as Formal-Pedigree Test (Legal Positivist Reading)").
narrative_ontology:topic_domain(us_constitution_text__positivist_reading, "legal/constitutional/philosophical").

domain_priors:requires_active_enforcement(us_constitution_text__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__positivist_reading, '05f93ccf-4ce5-4920-b444-d917a22f5015').
narrative_ontology:cs_kernel_codification('05f93ccf-4ce5-4920-b444-d917a22f5015', fixed_text).
narrative_ontology:cs_authority_grounding('05f93ccf-4ce5-4920-b444-d917a22f5015', lineage).
narrative_ontology:cs_interpretation_layer_present('05f93ccf-4ce5-4920-b444-d917a22f5015').
narrative_ontology:cs_reading_relation('05f93ccf-4ce5-4920-b444-d917a22f5015', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('05f93ccf-4ce5-4920-b444-d917a22f5015', us_constitution_text__living_constitutionalist_reading, influences).
narrative_ontology:cs_axiom('05f93ccf-4ce5-4920-b444-d917a22f5015', foundational, validity_derives_from_pedigree_not_content).
narrative_ontology:cs_axiom_status(validity_derives_from_pedigree_not_content, holdable).
narrative_ontology:cs_axiom_grounding('05f93ccf-4ce5-4920-b444-d917a22f5015', validity_derives_from_pedigree_not_content, conventional).
narrative_ontology:cs_axiom('05f93ccf-4ce5-4920-b444-d917a22f5015', secondary, moral_and_historical_meaning_are_interpretively_irrelevant_to_bindingness).
narrative_ontology:cs_axiom_status(moral_and_historical_meaning_are_interpretively_irrelevant_to_bindingness, holdable).
narrative_ontology:cs_axiom_grounding('05f93ccf-4ce5-4920-b444-d917a22f5015', moral_and_historical_meaning_are_interpretively_irrelevant_to_bindingness, conventional).
narrative_ontology:cs_reference_frame('05f93ccf-4ce5-4920-b444-d917a22f5015', hartian_rule_of_recognition).
narrative_ontology:cs_drift_state('05f93ccf-4ce5-4920-b444-d917a22f5015', contemporary_originalism_revival_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('05f93ccf-4ce5-4920-b444-d917a22f5015', '').
narrative_ontology:cs_kernel_id(us_constitution_text__positivist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, judicial_institution).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, legislative_drafters).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, settled_expectation_holders).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, unenacted_justice_claimants).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, customary_rights_holders).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, marginalized_groups_without_formal_recourse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts apply a rule of recognition: a norm counts as constitutional law if it was enacted through the pedigreed procedure (proposal, ratification per Article V, or valid amendment/enactment chain), regardless of its moral content. This gives judges a determinate, defensible test that insulates decisions from charges of imposing personal values, and it stabilizes the judiciary's institutional legitimacy by making validity a source-question rather than a merits-question.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, judicial_institution, agenda_setter,
    institutional, generational, arbitrage, national).

% Legislators and amendment proponents know that if they follow the formal procedure, the resulting text acquires binding status independent of substantive contestation. This predictability lets political coalitions bank durable wins through procedure rather than needing to win a permanent moral consensus.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, legislative_drafters, beneficiary,
    powerful, generational, mobile, national).

% Businesses, property owners, and institutions that have organized their affairs around existing enacted constitutional text benefit from a validity test that does not reopen settled law based on shifting moral or historical argument. Predictability of legal source outweighs, for them, any single case's substantive outcome.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, settled_expectation_holders, beneficiary,
    organized, civilizational, constrained, national).

% Groups with a substantively compelling claim to constitutional protection — one grounded in moral argument, historical injury, or contemporary consensus — have no standing under this reading unless that claim was formally enacted. They cannot invoke justice directly; they must first win a procedural fight (litigation establishing pedigree, or a new amendment) before any substantive claim is even cognizable. Exit is effectively unavailable — there is no alternative venue to plead the merits when the pedigree test controls.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, unenacted_justice_claimants, payer,
    powerless, biographical, trapped, national).

% Communities relying on unwritten, customary, or long-practiced norms (rather than codified enactment) find those norms structurally unrecognizable as constitutional law, however deeply settled in practice, because the positivist test asks only about the enactment chain, not about practice or duration.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, customary_rights_holders, payer,
    powerless, generational, trapped, national).

% Groups whose historical exclusion from the enactment process (e.g., from ratification conventions, from prior amendment coalitions) means the formally valid text does not encode their interests bear the cost of a validity rule that treats that historical exclusion as irrelevant to present validity — the door to substantive remedy runs only through winning a new formal enactment, an uphill and resource-intensive path.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, marginalized_groups_without_formal_recourse, payer,
    powerless, biographical, trapped, national).

% Scholars analyze whether the positivist test genuinely separates law from morality or merely defers moral judgment to whoever controlled the enactment procedure at the founding or during subsequent amendment. They document the gap between formal validity and substantive legitimacy without holding power to resolve it.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, legal_academics, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, source-based test for what counts as valid constitutional law, so that judges, legislators, and citizens can identify binding norms without relitigating their moral merits in every case — a genuine solution to the problem of legal indeterminacy and judicial overreach.
% TRANSFER_FUNCTION: Moves interpretive authority away from claimants asserting substantive moral or historical entitlement and toward whoever controlled (or currently controls) the formal enactment and amendment process; moves adjudicative discretion toward courts applying pedigree tests rather than merits tests.
% ABSENT_VOICES: Groups excluded from the historical ratification and amendment processes (enslaved people, women, non-property-holders, and their political descendants) are structurally absent from the pedigree the test defers to; contemporary claimants with compelling substantive arguments but no enacted textual hook are heard only as advocates for future amendments, not as holders of present constitutional claims.
% DISAPPEARANCE_RATIONALE: If courts abandoned source-validity as the test for constitutional bindingness overnight, judicial decision-making would need a new anchor (moral reasoning, contemporary consensus, or raw discretion), settled expectations built on enacted text would become contestable on substantive grounds, and the entire architecture of judicial review, statutory hierarchy, and amendment-based change would require reconstruction around a different validity criterion.
% FOUNDING_PROBLEM: Legal positivism as a jurisprudential stance was built to solve the problem of indeterminate, judge-made 'natural law' or 'higher law' reasoning that let courts smuggle personal moral views into binding law under the guise of discovering pre-existing legal truth; the formal-validity test promised a rule of recognition that any observer could apply without first resolving contested moral questions.
% FOUNDING_PROBLEM_CORROBORATION: Legal academics outside the judiciary (H.L.A. Hart's tradition and its critics, e.g., Dworkin and critical legal studies scholars) attest that the separation-of-law-and-morals problem the test was built to solve remains partially live — courts still exercise substantive judgment when applying supposedly neutral pedigree tests (e.g., contested Article V questions, disputes over what counts as valid ratification). Critical legal scholars and outsider-jurisprudence scholars (e.g., critical race theorists) attest the founding problem has been substantially supplanted by a different function: using formal-validity language to foreclose substantive claims that lack historical access to the enactment process, which the positivist tradition's own internal critics acknowledge but the judiciary rarely does.
narrative_ontology:disappearance_verdict(us_constitution_text__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__positivist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__positivist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__positivist_reading_tests).
:- end_tests(us_constitution_text__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42, rising slowly over the interval) because the positivist test's coordination function — a determinate rule of recognition — is real and widely relied upon, but the cost falls asymmetrically on claimants whose substantive arguments cannot be routed through the pedigree test, and that asymmetry has grown modestly as more contested edge cases (executive power, unenumerated rights, structural inference) have tested the limits of pure source-validity. Suppression (0.55) reflects that courts actively enforce the pedigree criterion against litigants who argue from morality or historical purpose alone — such arguments are treated as inadmissible on the validity question, not merely unpersuasive. Theater ratio is modest (0.28) — the test does real institutional work (constraining judicial discretion, providing predictability) rather than being purely performative, though its neutrality claim is increasingly contested.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial seat, the positivist test is a rope: a genuine coordination solution to the problem of indeterminate, moralized adjudication. From the seat of a claimant with a compelling substantive claim but no enacted textual hook, the same rule operates as an enforced barrier — a tangled rope where the coordination function (determinacy) is real but is achieved by extracting standing from those who lack procedural access. The engine computes this divergence from the structural beneficiary/victim data; the claimed_type here is authored as tangled_rope because both the coordination function and the asymmetric extraction are structurally genuine and simultaneous, not because the metrics were tuned to produce that label.
 *
 * DIRECTIONALITY LOGIC:
 *   The judicial institution and legislative drafters sit near the beneficiary end: they control or apply the enactment procedure and gain predictability and insulation from merits-based challenge. Settled-expectation holders similarly benefit from a validity rule that doesn't reopen enacted law on substantive grounds. Unenacted justice claimants, customary rights holders, and historically excluded groups sit near the target end: their substantive claims are structurally unrecognizable as constitutional law absent a formal enactment win, and they bear the cost of a rule that treats their historical exclusion from the ratification process as irrelevant to present validity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (indeterminate moral adjudication masquerading as legal discovery) remains partially live in some domains but has been substantially resolved in the sense that the coordination benefit (determinacy) is well-established; what has NOT been resolved, and what the mandatrophy question surfaces, is whether the test's neutrality claim continues to do legitimate work or has become a mechanism for foreclosing substantive claims from groups excluded from the original and subsequent enactment coalitions. This is exactly the kind of contested case where classification must not default to either 'purely legitimate coordination' or 'purely extractive gatekeeping' — the tangled_rope label holds both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivist_neutrality_claim,
    'Does the positivist validity test actually achieve moral neutrality, or does it merely relocate moral judgment to the (also contestable) question of what counts as a valid enactment procedure?',
    'Examine hard cases where the enactment-procedure question itself required substantive judgment (e.g., disputed ratification counts, questions about whether a purported amendment followed Article V correctly) — if courts resolve procedural disputes using implicit substantive commitments, the neutrality claim is undermined.',
    'If the pedigree test smuggles in substantive judgment at the procedural-validity stage, the positivist reading''s claimed advantage over the other two readings (determinacy without moralizing) collapses, and its extraction profile should be read as closer to the living-constitutionalist reading''s contested terrain than its own coordination story suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivist_neutrality_claim, conceptual, 'Whether formal-validity tests achieve genuine moral neutrality or relocate the moral question.').

omega_variable(
    sibling_reading_foreclosure,
    'Does adopting the positivist reading as the controlling judicial doctrine foreclose the originalist and living-constitutionalist readings within the same institutional framework, or can all three coexist as competing interpretive methodologies within the same court system?',
    'Survey actual judicial practice: do judges announcing a positivist methodology in fact exclude originalist or living-constitutionalist reasoning from their opinions, or do the three methodologies blend in practice (e.g., ''faint-hearted originalism'' invoking positivist source-tests alongside historical-meaning arguments)?',
    'If the readings blend in practice, the coexists_with relation is empirically confirmed and no single reading has captured the judiciary; if positivist source-validity doctrine is deployed specifically to exclude the other two methodologies as illegitimate, the relationship is closer to foreclosure in individual judicial opinions even though the readings persist as competing schools across the profession.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, empirical, 'Whether the positivist reading''s dominance in a given ruling forecloses or merely coexists with sibling readings.').

omega_variable(
    excluded_group_recourse_pathway,
    'Is the amendment process (Article V) a realistic recourse pathway for historically excluded groups, or is it, in practice, so structurally difficult that the positivist reading''s promise of formal recourse is illusory?',
    'Historical base-rate analysis of successful Article V amendments addressing rights claims from excluded groups, weighted against the number of substantive claims raised that never achieved formal enactment.',
    'If the amendment pathway is realistically available, some of the measured victim-side extraction is better characterized as a genuine (if costly) coordination requirement rather than a foreclosure device; if the pathway is effectively closed for under-resourced groups, the extraction is closer to structural exclusion dressed as procedural opportunity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_group_recourse_pathway, empirical, 'Whether Article V provides genuine recourse or merely a formal illusion of recourse for excluded claimants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__positivist_reading, 1789, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1789, us_constitution_text__positivist_reading, theater_ratio, 1789, 0.15).
narrative_ontology:measurement(us_c_tr_t1868, us_constitution_text__positivist_reading, theater_ratio, 1868, 0.18).
narrative_ontology:measurement(us_c_tr_t1920, us_constitution_text__positivist_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(us_c_tr_t1960, us_constitution_text__positivist_reading, theater_ratio, 1960, 0.23).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_text__positivist_reading, theater_ratio, 1990, 0.26).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_text__positivist_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1789, us_constitution_text__positivist_reading, base_extractiveness, 1789, 0.3).
narrative_ontology:measurement(us_c_be_t1868, us_constitution_text__positivist_reading, base_extractiveness, 1868, 0.35).
narrative_ontology:measurement(us_c_be_t1920, us_constitution_text__positivist_reading, base_extractiveness, 1920, 0.38).
narrative_ontology:measurement(us_c_be_t1960, us_constitution_text__positivist_reading, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_text__positivist_reading, base_extractiveness, 1990, 0.41).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_text__positivist_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1789, us_constitution_text__positivist_reading, suppression_requirement, 1789, 0.4).
narrative_ontology:measurement(us_c_su_t1868, us_constitution_text__positivist_reading, suppression_requirement, 1868, 0.5).
narrative_ontology:measurement(us_c_su_t1920, us_constitution_text__positivist_reading, suppression_requirement, 1920, 0.52).
narrative_ontology:measurement(us_c_su_t1960, us_constitution_text__positivist_reading, suppression_requirement, 1960, 0.53).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_text__positivist_reading, suppression_requirement, 1990, 0.54).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_text__positivist_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the colloquial label 'constitutional interpretation methodology' per the epsilon-invariance principle. Each reading (positivist, originalist, living_constitutionalist) has its own stable epsilon, beneficiary/victim structure, and classification, because each reading would resolve identical hard constitutional cases differently. They are linked here rather than merged into one story with an observable-dependent epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
