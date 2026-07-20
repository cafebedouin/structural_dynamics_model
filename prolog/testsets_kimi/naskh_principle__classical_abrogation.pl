% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Quranic Abrogation (Naskh) Principle
 *   domain: religious/jurisprudential
 *
 * SUMMARY:
 *   The classical abrogation (naskh) reading of the Quranic text holds that
 *   when two verses address the same legal or theological topic and appear to
 *   conflict, the later chronologically revealed verse supersedes the
 *   earlier, which loses legal force though it may retain liturgical and
 *   spiritual value. This principle is institutionalized across the four
 *   Sunni madhhabs and Shi'a jurisprudence, administered by a specialized
 *   class of jurists who control the asbab al-nuzul (occasions of revelation)
 *   literature and the abrogation lists. It is one reading of the
 *   naskh_principle kernel, alongside contextual_harmonization and
 *   progressive_restriction. Key agents include the juridical class that
 *   maintains the framework, the lay community that receives legal certainty
 *   from it, reformist scholars who bear the cost of suppressed interpretive
 *   flexibility, and critical historians structurally excluded from the
 *   classical conversation.
 *
 * KEY AGENTS:
 *   - Classical jurists (agenda_setter/beneficiary): institutional power, constrained exit â maintain the abrogation framework and derive authority from it.
 *   - Lay believers (beneficiary): powerless, constrained exit â receive legal clarity but depend entirely on the juridical class.
 *   - Reformist scholars (payer): moderate power, constrained exit â propose holistic readings and are marginalized via naskh invocation.
 *   - Critical historians (excluded): moderate power, analytical exit â question chronology and abrogation categories from outside the tradition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.62).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.68).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.62).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Quranic Abrogation (Naskh) Principle").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "religious/jurisprudential").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, '6112a400-f88f-462e-9093-72399da7b67a').
narrative_ontology:cs_kernel_codification('6112a400-f88f-462e-9093-72399da7b67a', fixed_text).
narrative_ontology:cs_authority_grounding('6112a400-f88f-462e-9093-72399da7b67a', lineage).
narrative_ontology:cs_interpretation_layer_present('6112a400-f88f-462e-9093-72399da7b67a').
narrative_ontology:cs_reading_relation('6112a400-f88f-462e-9093-72399da7b67a', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_reading_relation('6112a400-f88f-462e-9093-72399da7b67a', naskh_principle__progressive_restriction, coexists_with).
narrative_ontology:cs_axiom('6112a400-f88f-462e-9093-72399da7b67a', foundational, later_revelation_supersedes_earlier_ruling).
narrative_ontology:cs_axiom_status(later_revelation_supersedes_earlier_ruling, holdable).
narrative_ontology:cs_axiom_grounding('6112a400-f88f-462e-9093-72399da7b67a', later_revelation_supersedes_earlier_ruling, theological).
narrative_ontology:cs_axiom('6112a400-f88f-462e-9093-72399da7b67a', secondary, abrogated_verses_preserve_liturgical_status).
narrative_ontology:cs_axiom_status(abrogated_verses_preserve_liturgical_status, holdable).
narrative_ontology:cs_axiom_grounding('6112a400-f88f-462e-9093-72399da7b67a', abrogated_verses_preserve_liturgical_status, conventional).
narrative_ontology:cs_reference_frame('6112a400-f88f-462e-9093-72399da7b67a', classical_juridical_certainty).
narrative_ontology:cs_drift_state('6112a400-f88f-462e-9093-72399da7b67a', contemporary_critical_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6112a400-f88f-462e-9093-72399da7b67a', '').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, lay_believers).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, reformist_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the hermeneutical framework of naskh, author legal manuals and fatwas that classify verses as abrogated or abrogating, and adjudicate disputes by appeal to revelatory chronology. Their institutional authority and livelihood derive from mastery of this framework and its transmission through madhhab lineages.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_jurists, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__classical_abrogation, classical_jurists, beneficiary).

% Receive clear, stable legal rulings from jurists without needing to navigate apparent contradictions in the Quranic text themselves. Their compliance is eased by the reduction of interpretive burden, but they are dependent on the juridical class for authoritative access to divine law and have little recourse outside it.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, lay_believers, beneficiary,
    powerless, biographical, constrained, global).

% Seek to read Quranic verses in light of holistic ethics and modern contexts. Their interpretive proposals are systematically sidelined when classical jurists invoke naskh to close discussion; they bear the cost of narrowed textual conversation and marginalization within mainstream institutions.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, reformist_scholars, payer,
    moderate, biographical, constrained, global).

% Apply historical-critical and philological methods to question the reliability of classical revelatory chronology and the very category of abrogation. Their findings are structurally excluded from classical juridical training, fatwa issuance, and mosque instruction.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, critical_historians, excluded,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__classical_abrogation, classical_jurists).
narrative_ontology:fixing_cost_class(naskh_principle__classical_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves apparent contradictions between Quranic verses by establishing a chronological hierarchy of revelation, yielding determinate legal rulings for a revealed law that addresses diverse situations across time and space.
% TRANSFER_FUNCTION: Moves interpretive authority from the general community of readers to the specialized class of jurists who control chronology and abrogation lists; moves theological tension from the text into the jurist's classification system, stabilizing one reading at the expense of others.
% ABSENT_VOICES: Historical-critical scholars who question the reliability of asbab al-nuzul chronology, and modernist theologians who read the Quran as a holistically coherent document rather than a diachronically layered legal code. They are absent from classical madhhab training, state fatwa councils, and standard mosque curricula.
% DISAPPEARANCE_RATIONALE: If the naskh principle vanished from classical jurisprudence, centuries of fiqh rulings based on abrogated verses would reopen for renegotiation, legal manuals would require systemic revision, and the authority of the juridical classâwhich derives partly from managing this complexityâwould diminish significantly. The community would face renewed interpretive pluralism and legal uncertainty.
% FOUNDING_PROBLEM: The Quran contains verses that appear to command contradictory actions on the same topic (e.g., gradual prohibition of alcohol, rules of warfare, inheritance directives). The early Muslim community needed a systematic method to determine which ruling to follow without rejecting any part of the revealed text as erroneous.
% FOUNDING_PROBLEM_CORROBORATION: Classical jurists attest the problem in usul al-fiqh manuals. Independent corroboration from outside the beneficiary set comes from modern academic historians of Islamic law (e.g., Harald Motzki, John Burton) who document the emergence of naskh as a response to exegetical contradiction, though they frequently dispute whether the classical solution was historically inevitable or theologically necessary.
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high: the framework genuinely coordinates legal certainty, but it also concentrates interpretive authority in the juridical class and suppresses alternative hermeneutics. Suppression (0.68) reflects the institutional gatekeeping of madhhab training and state sharia courts, which marginalize reformist and historical-critical readings. Theater_ratio (0.45) captures the growing performative dimension: much scholarly labor defends abrogation lists whose underlying chronology is increasingly contested, maintaining authority rather than discovering new facts. Accessibility_collapse (0.60) is significant because once inside the classical framework, alternatives such as contextual harmonization appear illegitimate; from outside, they remain visible. Resistance (0.45) reflects sustained modernist and academic challenges. The measurement series share a single time grid tracking the consolidation and modern contestation of the framework.
 *
 * PERSPECTIVAL GAP:
 *   The classical jurist seat experiences the constraint as necessary coordination: without naskh, the revealed law would collapse into contradiction. The reformist scholar seat experiences the same structure as extraction: naskh is deployed to shut down interpretive possibilities and preserve juridical monopoly. The lay believer seat sits near the beneficiary end but carries a diffuse cost of dependence. The engine computes this divergence from the structural data; the authored claim does not adjudicate between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical jurists are structural beneficiaries (low d): they collect institutional authority and have constrained exit because their role is fused with the framework. Lay believers are also beneficiaries (low d): they receive coordination value, though their powerlessness and constrained exit keep them from arbitrage. Reformist scholars are targets (high d): they bear the cost of suppressed flexibility and have constrained exit because leaving the discourse means abandoning institutional voice. Critical historians are analytical (neutral d): they observe from outside the enforcement perimeter.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâapparent contradiction in a revealed textâwas genuine and remains live in the source material. However, the classical solution has outlived its original pluralist context and become an extraction mechanism: the same framework that once solved an urgent coordination problem now serves to suppress reform and centralize authority. It is not a piton because the agenda-setter seat still actively benefits from maintenance, and it is not a snare because the coordination (legal certainty) is structurally real and still valued by the community. Tangled rope is the appropriate classification: coordination and extraction are braided through the same institution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_scope_ambiguity,
    'What is the actual scope of abrogated versesâdoes the classical corpus contain a small handful of clear cases or hundreds of contested ones?',
    'Systematic historical-critical review of classical naskh lists against early tafsir and legal corpora, comparing the major madhhabs'' inventories.',
    'A small scope would reduce measured extractiveness (the framework is a precision tool); a very large scope would indicate that abrogation has become a blanket suppression mechanism and push the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_scope_ambiguity, empirical, 'Uncertainty about how many verses are actually abrogated under classical doctrine.').

omega_variable(
    chronology_reliability,
    'Is the classical asbab al-nuzul chronology historically reliable enough to support determinate legal supersession?',
    'Philological and historical analysis of the isnad chains and textual variants in occasion-of-revelation reports.',
    'If chronology is unreliable, the abrogation framework rests on performative assertion rather than genuine discovery, raising theater_ratio and undermining the coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chronology_reliability, empirical, 'Whether the revelatory chronology that grounds abrogation is historically sound.').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the naskh_principle kernel. Would adopting the contextual_harmonization reading eliminate the victim class entirely, or would it simply shift extraction to a different site such as contextual-expert authority?',
    'Comparative analysis of the sibling reading''s constraint story once generated; examine whether contextual_harmonization carries its own beneficiaries and victims.',
    'If the victim class disappears, the extraction may be reading-specific; if it migrates, the kernel itself may be structurally extractive regardless of reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the extraction is specific to the classical_abrogation reading or inherent to the kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of reformist and critical views structural (institutional gatekeeping by madhhabs and state courts) or internalized (self-censorship by scholars who accept the classical frame as natural)?',
    'Post-exit trajectory analysis: whether scholars who leave classical institutions continue to self-censor on naskh, or freely abandon it.',
    'If internalized, effective suppression exceeds the structural measure because the target carries the constraint after leaving the institution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression mechanism in the classical juridical tradition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nask_tr_t200, naskh_principle__classical_abrogation, theater_ratio, 200, 0.25).
narrative_ontology:measurement(nask_tr_t400, naskh_principle__classical_abrogation, theater_ratio, 400, 0.3).
narrative_ontology:measurement(nask_tr_t600, naskh_principle__classical_abrogation, theater_ratio, 600, 0.35).
narrative_ontology:measurement(nask_tr_t800, naskh_principle__classical_abrogation, theater_ratio, 800, 0.38).
narrative_ontology:measurement(nask_tr_t1000, naskh_principle__classical_abrogation, theater_ratio, 1000, 0.42).
narrative_ontology:measurement(nask_tr_t1200, naskh_principle__classical_abrogation, theater_ratio, 1200, 0.44).
narrative_ontology:measurement(nask_tr_t1400, naskh_principle__classical_abrogation, theater_ratio, 1400, 0.45).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(nask_be_t200, naskh_principle__classical_abrogation, base_extractiveness, 200, 0.48).
narrative_ontology:measurement(nask_be_t400, naskh_principle__classical_abrogation, base_extractiveness, 400, 0.55).
narrative_ontology:measurement(nask_be_t600, naskh_principle__classical_abrogation, base_extractiveness, 600, 0.6).
narrative_ontology:measurement(nask_be_t800, naskh_principle__classical_abrogation, base_extractiveness, 800, 0.63).
narrative_ontology:measurement(nask_be_t1000, naskh_principle__classical_abrogation, base_extractiveness, 1000, 0.64).
narrative_ontology:measurement(nask_be_t1200, naskh_principle__classical_abrogation, base_extractiveness, 1200, 0.63).
narrative_ontology:measurement(nask_be_t1400, naskh_principle__classical_abrogation, base_extractiveness, 1400, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(nask_su_t200, naskh_principle__classical_abrogation, suppression_requirement, 200, 0.56).
narrative_ontology:measurement(nask_su_t400, naskh_principle__classical_abrogation, suppression_requirement, 400, 0.63).
narrative_ontology:measurement(nask_su_t600, naskh_principle__classical_abrogation, suppression_requirement, 600, 0.68).
narrative_ontology:measurement(nask_su_t800, naskh_principle__classical_abrogation, suppression_requirement, 800, 0.71).
narrative_ontology:measurement(nask_su_t1000, naskh_principle__classical_abrogation, suppression_requirement, 1000, 0.73).
narrative_ontology:measurement(nask_su_t1200, naskh_principle__classical_abrogation, suppression_requirement, 1200, 0.71).
narrative_ontology:measurement(nask_su_t1400, naskh_principle__classical_abrogation, suppression_requirement, 1400, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
