% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanbali_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Textualist Jurisprudential Method
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   The Hanbali reading of the jurisprudential method kernel holds that
 *   Islamic law derives solely from the literal text of the Qur'an,
 *   authenticated Hadith, and the opinions of the Prophet's companions;
 *   analogical reasoning (qiyas) and juristic preference (istihsan) are
 *   condemned as bid'ah (innovation) that corrupts divine law, and only
 *   unanimous consensus (ijma) commands authority. This constraint story
 *   models that reading as a standing institutional arrangement that
 *   coordinates textualist scholars around a shared epistemic framework while
 *   asymmetrically extracting legitimacy from rationalist jurists and
 *   customary legal communities. The authored metrics (high extractiveness,
 *   high suppression) describe the arrangement's operation; the claimed type
 *   (tangled_rope) reflects the author's structural assessment that a genuine
 *   coordination function for textualists coexists with asymmetric extraction
 *   from methodological opponents. This constraint is one reading of the
 *   jurisprudential_method_kernel; sibling readings (hanafi_reading,
 *   maliki_reading, shafii_reading) instantiate structurally distinct
 *   methodological claims from the same kernel.
 *
 * KEY AGENTS:
 *   - textualist_scholars (agenda_setter / beneficiary): Institutional power, identity-locked exit â they define the methodological boundary between revelation and innovation and collect interpretive authority.
 *   - rationalist_jurists (payer): Organized power, constrained exit â their core methodology is delegitimized as bid'ah, extracting their judicial standing and fatwa legitimacy.
 *   - customary_practice_communities (payer): Powerless, trapped exit â local norms are invalidated unless rooted in explicit text, forcing reliance on textualist adjudication.
 *   - comparative_legal_historians (observer): Analytical seat â external observers who track the political economy of madhhab formation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.82).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.78).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Textualist Jurisprudential Method").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, 'cfae9100-eab9-4d23-983f-7729ff6ba6e6').
narrative_ontology:cs_kernel_codification('cfae9100-eab9-4d23-983f-7729ff6ba6e6', fixed_text).
narrative_ontology:cs_authority_grounding('cfae9100-eab9-4d23-983f-7729ff6ba6e6', lineage).
narrative_ontology:cs_interpretation_layer_present('cfae9100-eab9-4d23-983f-7729ff6ba6e6').
narrative_ontology:cs_reading_relation('cfae9100-eab9-4d23-983f-7729ff6ba6e6', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('cfae9100-eab9-4d23-983f-7729ff6ba6e6', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('cfae9100-eab9-4d23-983f-7729ff6ba6e6', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('cfae9100-eab9-4d23-983f-7729ff6ba6e6', foundational, qiyas_is_bidah).
narrative_ontology:cs_axiom_status(qiyas_is_bidah, holdable).
narrative_ontology:cs_axiom_grounding('cfae9100-eab9-4d23-983f-7729ff6ba6e6', qiyas_is_bidah, theological).
narrative_ontology:cs_axiom('cfae9100-eab9-4d23-983f-7729ff6ba6e6', foundational, companion_opinions_normatively_binding).
narrative_ontology:cs_axiom_status(companion_opinions_normatively_binding, holdable).
narrative_ontology:cs_axiom_grounding('cfae9100-eab9-4d23-983f-7729ff6ba6e6', companion_opinions_normatively_binding, theological).
narrative_ontology:cs_reference_frame('cfae9100-eab9-4d23-983f-7729ff6ba6e6', classical_textualist_legal_authority).
narrative_ontology:cs_drift_state('cfae9100-eab9-4d23-983f-7729ff6ba6e6', contemporary_global_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cfae9100-eab9-4d23-983f-7729ff6ba6e6', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, textualist_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained in hadith criticism and literalist hermeneutics, they adjudicate legal questions by restricting sources to Qur'an, authenticated Hadith, and companion reports. Their authority depends on maintaining the boundary between legitimate transmission and rationalist innovation; they issue fatwas, teach in seminaries, and staff judicial institutions where Hanbali jurisprudence is state doctrine.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, textualist_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Jurists trained in analogical reasoning and juristic preference whose methodologies are delegitimized as bid'ah under the Hanbali reading. They face exclusion from fatwa councils and judicial appointments in textualist-dominated jurisdictions, and their legal opinions are treated as corrupt rather than merely disagreeing.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    organized, biographical, constrained, global).

% Local Muslim communities whose customary norms govern marriage, trade, and land use outside explicit textual precedent. Under the Hanbali reading, these practices lack legal validity unless grounded in explicit revelation, exposing them to judicial override and scholarly condemnation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities, payer,
    powerless, biographical, trapped, regional).

% Academic observers who map the institutional history of madhhab formation and note that the Hanbali hardline on qiyas crystallized in specific political contexts rather than deriving transparently from scripture alone.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanbali_reading, textualist_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified epistemic framework for textualist scholars by restricting legitimate legal sources to explicit revelation and companion precedent, eliminating the uncertainty and disagreement introduced by subjective reasoning.
% TRANSFER_FUNCTION: Moves interpretive legitimacy and judicial authority from rationalist jurists and local customary practitioners to textualist scholars by categorizing analogical reasoning, juristic preference, and unmoored custom as corrupting innovation (bid'ah).
% ABSENT_VOICES: Hanafi and Shafi'i jurists who would defend the structural necessity of qiyas for novel cases; Maliki jurists who would validate the living tradition of Medina; local customary judges and community elders whose practice-based norms are excluded from the conversation; Muslim reformers seeking adaptive legal responses to modern contexts.
% DISAPPEARANCE_RATIONALE: If the Hanbali methodological constraint disappeared, rationalist jurisprudence would regain equal standing, customary legal norms would be reintegrated into fiqh, and the textualist monopoly on fatwa and judicial appointment would fragment â the institutional landscape of Islamic law would reorganize around methodological pluralism.
% FOUNDING_PROBLEM: The early Muslim community faced a proliferation of conflicting legal opinions and the risk that fallible human reason would substitute divine command with speculative conjecture (zann), especially under rationalist-influenced caliphal courts.
% FOUNDING_PROBLEM_CORROBORATION: Textualist scholars attest the problem remains live, citing the danger of relativism. Rationalist jurists, legal historians, and anthropologists from outside the textualist beneficiary set attest that the founding problem was historically resolved through classical Sunni synthesis and that the Hanbali reading now functions as institutionalized methodological monopoly rather than revelation-protection; no independent corroboration from neutral political authorities exists.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanbali_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.82) because the constraint monopolizes legal legitimacy for textualist scholars by declaring the core methodologies of competing schools bid'ah. Suppression is high (0.78) because the arrangement depends on active enforcement â labeling qiyas and istihsan as innovation, excluding rationalists from judicial posts, and invalidating customary norms. Theater ratio is moderate (0.40): the textualist performance of 'pure' adherence to revelation is genuine in part, but a substantial share of scholarly activity is devoted to maintaining the boundary against innovation rather than resolving novel legal questions. Accessibility collapse is very high (0.85) because, once the textualist framework is accepted, rationalist and customary alternatives collapse not merely into error but into heresy. Resistance is moderate (0.55): rationalist jurists resist but are structurally marginalized in textualist-dominated jurisdictions; their resistance is muted by the asymmetry of institutional power.
 *
 * PERSPECTIVAL GAP:
 *   The textualist scholar seat and the rationalist jurist seat compute radically different classifications from the same structural data. From the textualist position, the constraint is necessary coordination that protects divine law from human corruption; from the rationalist position, the identical structure is enforced extraction that monopolizes interpretive authority. The engine computes this divergence from the structural asymmetry in beneficiary/victim declarations and exit options â the identity-locked textualists experience the constraint as identity-constitutive, while the constrained rationalists experience it as an external barrier to legitimate practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist scholars are the structural beneficiaries and agenda-setters: they define what counts as bid'ah, control access to fatwa legitimacy, and their exit is identity-locked to the textualist community â d near the beneficiary end. Rationalist jurists and customary practice communities are the structural payers: their methodologies and norms are extracted of legitimacy, their exit is constrained or trapped by the delegitimation of their core practices â d near the target end. The comparative legal historian sits at the analytical pole with negligible directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling this constraint as pure coordination (rope) by requiring the victim set and active enforcement flag â the rationalist jurists are not merely disagreed with but structurally harmed by the delegitimation of their methodology. It also prevents mislabeling as pure snare by acknowledging the genuine coordination function the textualist method provides for its adherents: a shared epistemic framework that reduces interpretive disagreement within the textualist community. The founding_problem_status is contested rather than dead, which flags that the coordination story may still be live for some seats even as extraction dominates for others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_relation_ambiguity,
    'Does the Hanbali reading''s rejection of qiyas logically foreclose the Hanafi, Maliki, and Shafi''i readings, or do they structurally coexist as live madhhab options within the broader Sunni tradition?',
    'Historical analysis of madhhab coexistence arrangements (tolerance, judicial appointment patterns) versus logical analysis of methodological premises.',
    'If foreclosed, the Hanbali reading functions as a more aggressive exclusion mechanism; if coexistent, it operates as one faction among plural options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relation_ambiguity, conceptual, 'Whether sibling readings are logically foreclosed or politically coexistent').

omega_variable(
    textualist_method_naturalness,
    'Is the textualist method a natural feature of revealed law, or a constructed constraint that benefits the textualist scholarly class by monopolizing interpretive authority?',
    'Comparative historical analysis of legal development: do textualist communities show higher scholarly reproduction rates and institutional capture compared to rationalist communities?',
    'If constructed benefit, the constraint is a false summit rather than a natural implication of scripture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_method_naturalness, conceptual, 'Natural-law versus constructed-benefit ambiguity of the textualist method').

omega_variable(
    authority_drift_ambiguity,
    'In the contemporary era, is the Hanbali textualist framework experiencing authority erosion through legal modernization, or revival pressure through Salafi movements?',
    'Jurisdiction-specific analysis: map where Hanbali usul is state-backed versus displaced by modern legal codes.',
    'Determines whether the constraint''s effective extraction is rising or falling in the contemporary interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_drift_ambiguity, empirical, 'Whether contemporary drift is erosion or revival for textualist authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(juri_tr_t20, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(juri_tr_t60, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(juri_tr_t80, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(juri_be_t20, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(juri_be_t60, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(juri_be_t80, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 80, 0.78).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 100, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(juri_su_t20, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(juri_su_t60, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(juri_su_t80, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 80, 0.78).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% The jurisprudential method kernel decomposes into four sibling readings (hanafi, hanbali, maliki, shafii) because the natural-language label 'Islamic legal methodology' conflates structurally distinct claims about the sources and limits of legal reasoning. Each reading carries a distinct epsilon and stakeholder configuration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
