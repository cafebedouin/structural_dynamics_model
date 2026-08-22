% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__continuity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: correct_latin_kernel__continuity_reading
 *   human_readable: Medieval Latin as Continuous Classical Latin (Organic Evolution Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This story instantiates the continuity reading of the contested 'correct
 *   Latin' kernel: Medieval Latin is treated as Classical Latin having
 *   undergone natural linguistic evolution, with medieval scribal and
 *   scholastic reconstruction understood as internal correction within a
 *   single continuous tradition rather than reconstitution of a broken one.
 *   Under this reading, medieval institutional practice (monastic,
 *   scholastic) is the legitimate live descendant of classical usage, and the
 *   later humanist insistence on Ciceronian purity is recast as prescriptive
 *   purism imposed on an already-correct evolving system. This is a single
 *   reading among three (continuity, discontinuity, hybrid) — the sibling
 *   readings are separate constraints with their own ε and stakeholder
 *   structures, linked here via network edges, not folded into this file's
 *   classification.
 *
 * KEY AGENTS:
 *   - medieval_scholastic_institutions: agenda_setter/beneficiary (institutional/arbitrage) — sets and administers the standard
 *   - monastic_scriptoria: beneficiary (organized/constrained) — conventions validated retroactively
 *   - continuity_school_philologists: beneficiary/agenda_setter (institutional/mobile) — modern disciplinary stakeholders
 *   - humanist_reform_advocates: payer (moderate/constrained) — delegitimized as purists under this reading
 *   - students_taught_medieval_forms_as_classical: payer (powerless/trapped) — bear cost of flattened history
 *   - discontinuity_and_hybrid_school_philologists: excluded (moderate/constrained) — sidelined rival scholarship
 *   - comparative_linguists: observer (analytical/analytical) — assess divergence data neutrally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, 0.58).
domain_priors:suppression_score(correct_latin_kernel__continuity_reading, 0.62).
domain_priors:theater_ratio(correct_latin_kernel__continuity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__continuity_reading, "Medieval Latin as Continuous Classical Latin (Organic Evolution Reading)").
narrative_ontology:topic_domain(correct_latin_kernel__continuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__continuity_reading, '8c4a4bb6-44a2-4605-aa3b-2134d597948d').
narrative_ontology:cs_kernel_codification('8c4a4bb6-44a2-4605-aa3b-2134d597948d', distributed).
narrative_ontology:cs_authority_grounding('8c4a4bb6-44a2-4605-aa3b-2134d597948d', practice).
narrative_ontology:cs_interpretation_layer_present('8c4a4bb6-44a2-4605-aa3b-2134d597948d').
narrative_ontology:cs_reading_relation('8c4a4bb6-44a2-4605-aa3b-2134d597948d', correct_latin_kernel__discontinuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c4a4bb6-44a2-4605-aa3b-2134d597948d', correct_latin_kernel__hybrid_reading, influences).
narrative_ontology:cs_axiom('8c4a4bb6-44a2-4605-aa3b-2134d597948d', foundational, linguistic_change_is_internally_generated_correction).
narrative_ontology:cs_axiom_status(linguistic_change_is_internally_generated_correction, holdable).
narrative_ontology:cs_axiom_grounding('8c4a4bb6-44a2-4605-aa3b-2134d597948d', linguistic_change_is_internally_generated_correction, empirically_contingent).
narrative_ontology:cs_axiom('8c4a4bb6-44a2-4605-aa3b-2134d597948d', secondary, medieval_usage_is_prima_facie_legitimate_latin).
narrative_ontology:cs_axiom_status(medieval_usage_is_prima_facie_legitimate_latin, holdable).
narrative_ontology:cs_axiom_grounding('8c4a4bb6-44a2-4605-aa3b-2134d597948d', medieval_usage_is_prima_facie_legitimate_latin, conventional).
narrative_ontology:cs_reference_frame('8c4a4bb6-44a2-4605-aa3b-2134d597948d', unbroken_practitioner_continuity).
narrative_ontology:cs_drift_state('8c4a4bb6-44a2-4605-aa3b-2134d597948d', post_codicological_transmission_studies, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8c4a4bb6-44a2-4605-aa3b-2134d597948d', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__continuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, medieval_scholastic_institutions).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, monastic_scriptoria).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, continuity_school_philologists).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, humanist_reform_advocates).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, students_taught_medieval_forms_as_classical).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, unbroken_latinity_thesis).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, internal_correction_model_of_linguistic_change).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Universities and cathedral schools that taught, wrote, and adjudicated correct Latin using medieval forms (altered case usage, expanded vocabulary, shifted syntax) as the living, legitimate continuation of the classical tongue. They set curricular standards and controlled what counted as 'proper' Latin composition across centuries, with no external body positioned to override their judgment.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, medieval_scholastic_institutions, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__continuity_reading, medieval_scholastic_institutions, beneficiary).

% Copyists and compilers whose accumulated manuscript conventions (orthography, abbreviation, syntactic habits) became authoritative simply through institutional continuity and volume of production. Their labor and conventions are retroactively validated as 'natural evolution' rather than treated as departures requiring justification.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, monastic_scriptoria, beneficiary,
    organized, generational, constrained, regional).

% Modern historical linguists who argue for an unbroken diachronic chain from Classical to Medieval Latin. Their scholarly authority, funding, and disciplinary standing depend on the continuity model being accepted as the correct account of Latin's history; they publish, referee, and train students within this framework.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, continuity_school_philologists, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__continuity_reading, continuity_school_philologists, agenda_setter).

% Renaissance humanists and their intellectual descendants who insisted Ciceronian Latin was the only correct standard and that medieval Latin represented corruption requiring deliberate textual restoration, not organic descent. Under the continuity reading, their entire reform project is recast as prescriptive purism imposed on a language that was already correctly evolving — their historical argument is delegitimized by the framework itself.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, humanist_reform_advocates, payer,
    moderate, generational, constrained, continental).

% Learners across centuries instructed that medieval constructions were simply Latin, without disclosure that these forms diverged substantially from Ciceronian usage. They bear the cost of a flattened historical picture: when later confronted with classical texts or humanist critiques, their prior training is revealed as internally inconsistent with the 'purer' register, and they must unlearn or relearn without having chosen the framework that shaped them.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, students_taught_medieval_forms_as_classical, payer,
    powerless, biographical, trapped, regional).

% Scholars who argue Medieval Latin required deliberate symbolic reoccupation from texts (discontinuity) or layered partial recovery (hybrid) are structurally out-argued within continuity-dominated departments and journals; their evidence of textual reconstruction efforts by medieval and humanist scribes is treated as anomalous rather than central.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, discontinuity_and_hybrid_school_philologists, excluded,
    moderate, generational, constrained, global).

% Assess morphological, syntactic, and lexical divergence between corpora using formal historical-linguistic methods without institutional stake in which reading of the kernel prevails; their data can be marshaled by any of the three readings.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__continuity_reading, continuity_school_philologists).
narrative_ontology:fixing_cost_class(correct_latin_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, teachable, internally coherent standard of 'correct Latin' across a millennium of institutions (monasteries, universities, chanceries) that need a shared written register for law, liturgy, and scholarship without constant renegotiation of norms.
% TRANSFER_FUNCTION: Moves legitimacy and curricular authority from classical-purist humanist scholarship toward medieval scholastic and monastic institutional practice; moves the cost of historical inconsistency onto students and onto reform advocates whose corrective project is reframed as illegitimate imposition.
% ABSENT_VOICES: Discontinuity and hybrid-reading philologists are structurally sidelined in continuity-dominated academic departments; humanist reformers, now historical rather than living actors, cannot contest the framework that retroactively delegitimizes their intervention as 'mere purism.'
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished, medieval scholastic Latin's institutional prestige would not evaporate but would require re-justification on different grounds (deliberate normalization rather than organic descent); philology departments organized around unbroken-tradition narratives would need to revise curricula and self-conception, while historical facts of usage would remain unchanged — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Medieval institutions needed a stable, authoritative, non-arbitrary standard for written Latin that did not require constant appeal to a foreign, archaic classical register they could not fully replicate from fragmentary manuscript access.
% FOUNDING_PROBLEM_CORROBORATION: Codicologists studying manuscript transmission gaps (a group outside both the continuity-school beneficiaries and the humanist critics) attest that medieval scribes had genuinely incomplete access to classical exemplars in many regions and periods, which supports treating at least some medieval departures as improvisation rather than pure organic descent — a finding continuity-school philologists tend to downplay and humanist advocates tend to overstate as corruption.
narrative_ontology:disappearance_verdict(correct_latin_kernel__continuity_reading, contested).
narrative_ontology:founding_problem_status(correct_latin_kernel__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__continuity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate-high, rising over the interval) because the continuity reading does real coordination work (a teachable, stable Latin standard across thirteen centuries of institutions) while also functioning to retroactively legitimize whatever medieval institutions happened to produce, at the cost of erasing the humanist critique's evidentiary basis and misinforming generations of students about the register they were learning. Suppression (0.62) reflects that the reading requires active curricular and disciplinary enforcement — continuity-school departments structurally exclude discontinuity/hybrid evidence rather than merely disagreeing with it. Theater ratio (0.4) captures that a meaningful share of continuity-school scholarly activity performs unbroken-tradition narrative maintenance rather than engaging the codicological evidence of genuine transmission gaps. accessibility_collapse (0.45) and resistance (0.55) are moderate: humanist and hybrid alternatives remain visible and actively contested in the literature, unlike a genuine mountain where alternatives would be foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   From the continuity-school agenda-setter seat, this looks like straightforward historical linguistics: language changes, and treating medieval forms as illegitimate is anachronistic prescriptivism. From the humanist-advocate payer seat, the same framework looks like a retroactive erasure of a genuine, historically significant restoration project that responded to real degradation in usage. The engine computes these as structurally different seat classifications from the same authored data; this story does not adjudicate which seat is 'right' — it authors the structural asymmetry that produces the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval institutions and their modern scholarly heirs (continuity-school philologists) are declared beneficiaries: the reading validates their historical practice and current disciplinary framework at low cost to themselves, so directionality sits near the beneficiary end. Humanist reform advocates and students taught medieval forms as classical are declared victims/payers: the former have their entire reconstructive project recast as illegitimate purism (a direct historiographical cost), the latter absorb an undisclosed register conflation with no say in the framework that shaped their education — both sit near the target end of directionality. The discontinuity/hybrid scholars are excluded rather than harmed in the payer sense; their situation is captured by the absent_voices field rather than a victim declaration, since their exclusion is professional marginalization, not extraction of resources.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — medieval institutions needing a stable, non-arbitrary Latin standard without full access to classical exemplars — was genuinely live in the early medieval period (founding_problem_status: contested, since parts of the problem clearly persisted for centuries while access to manuscripts gradually improved). The continuity reading prevents mislabeling this as pure extraction: it does capture a real coordination achievement (a working written standard across a fragmented political landscape). But mandatrophy risk appears in the modern disciplinary persistence — continuity-school philology continues to organize careers, funding, and curricula around the unbroken-tradition thesis long after codicological evidence of real transmission gaps (attested by comparative/codicological observers outside the beneficiary set) complicates the simple internal-correction story. The classification as tangled_rope rather than rope registers this: real coordination function, but also asymmetric extraction (delegitimizing humanist scholarship, misinforming students) sustained by active disciplinary enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_discontinuity_empirical_underdetermination,
    'Does the documentary record (manuscript transmission continuity, absence of a discrete ''break'' generation, gradualness of morphological/syntactic shift) actually support unbroken organic evolution, or is the appearance of continuity an artifact of what texts survived and who controlled their copying?',
    'Systematic codicological survey of transmission gaps by region and period, cross-referenced against independent computational diachronic analysis of morphosyntactic change rate, to determine whether change patterns match natural language drift or show discontinuities consistent with reconstruction from limited exemplars.',
    'If transmission gaps are shown to be substantial and regionally concentrated, this reading''s core empirical premise weakens and the discontinuity or hybrid reading gains support for those regions/periods; if gaps are shown to be minor and evenly distributed, the continuity reading''s account is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_discontinuity_empirical_underdetermination, empirical, 'Whether observed continuity reflects real unbroken transmission or survivorship/control bias in the manuscript record.').

omega_variable(
    kernel_reading_selection_committer_structure,
    'Which of the three readings of the correct_latin_kernel (continuity, discontinuity, hybrid) should be treated as the default account in pedagogical and disciplinary contexts, given that each reading redistributes historiographical legitimacy differently?',
    'This is not resolvable by further linguistic data alone — it is a committer-frame choice about how to weight continuous-institutional-practice evidence against textual-reconstruction evidence. A cross-disciplinary panel making explicit which evidentiary weighting produces which reading would surface the choice rather than resolve it.',
    'Adopting the discontinuity reading would vindicate humanist reform advocates and reframe medieval scholastic Latin as a genuine break requiring textual reoccupation, inverting the beneficiary/victim structure of this story. Adopting the hybrid reading would split the difference, crediting morphological continuity while validating parts of the humanist critique on syntax and lexicon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_committer_structure, conceptual, 'Committer-level choice among sibling kernel readings, each of which structurally redistributes historiographical legitimacy.').

omega_variable(
    humanist_motive_purism_vs_genuine_restoration,
    'Were Renaissance humanist reforms genuinely prescriptive purism imposed on a correctly-evolving language (as this reading holds), or a genuine and partially justified restoration responding to real, documentable degradation in classical competence among medieval writers?',
    'Comparative analysis of humanist-era Latin competence claims against actual medieval-era classical text engagement (library holdings, citation practices, direct classical quotation accuracy) to assess whether the humanists were reacting to a real decline or constructing one rhetorically.',
    'If humanist claims track documentable decline in classical engagement, the ''prescriptive purism'' framing in this reading is itself a rhetorical move that underweights a real problem the humanists were responding to, strengthening grounds for revisiting the victim/payer classification of humanist advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanist_motive_purism_vs_genuine_restoration, empirical, 'Whether characterizing humanist reform as mere purism understates a genuine restorative motivation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__continuity_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(corr_tr_t150, correct_latin_kernel__continuity_reading, theater_ratio, 150, 0.2).
narrative_ontology:measurement(corr_tr_t300, correct_latin_kernel__continuity_reading, theater_ratio, 300, 0.25).
narrative_ontology:measurement(corr_tr_t500, correct_latin_kernel__continuity_reading, theater_ratio, 500, 0.3).
narrative_ontology:measurement(corr_tr_t700, correct_latin_kernel__continuity_reading, theater_ratio, 700, 0.35).
narrative_ontology:measurement(corr_tr_t850, correct_latin_kernel__continuity_reading, theater_ratio, 850, 0.38).
narrative_ontology:measurement(corr_tr_t1000, correct_latin_kernel__continuity_reading, theater_ratio, 1000, 0.4).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__continuity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(corr_be_t150, correct_latin_kernel__continuity_reading, base_extractiveness, 150, 0.38).
narrative_ontology:measurement(corr_be_t300, correct_latin_kernel__continuity_reading, base_extractiveness, 300, 0.45).
narrative_ontology:measurement(corr_be_t500, correct_latin_kernel__continuity_reading, base_extractiveness, 500, 0.5).
narrative_ontology:measurement(corr_be_t700, correct_latin_kernel__continuity_reading, base_extractiveness, 700, 0.55).
narrative_ontology:measurement(corr_be_t850, correct_latin_kernel__continuity_reading, base_extractiveness, 850, 0.57).
narrative_ontology:measurement(corr_be_t1000, correct_latin_kernel__continuity_reading, base_extractiveness, 1000, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(corr_su_t150, correct_latin_kernel__continuity_reading, suppression_requirement, 150, 0.4).
narrative_ontology:measurement(corr_su_t300, correct_latin_kernel__continuity_reading, suppression_requirement, 300, 0.48).
narrative_ontology:measurement(corr_su_t500, correct_latin_kernel__continuity_reading, suppression_requirement, 500, 0.55).
narrative_ontology:measurement(corr_su_t700, correct_latin_kernel__continuity_reading, suppression_requirement, 700, 0.6).
narrative_ontology:measurement(corr_su_t850, correct_latin_kernel__continuity_reading, suppression_requirement, 850, 0.62).
narrative_ontology:measurement(corr_su_t1000, correct_latin_kernel__continuity_reading, suppression_requirement, 1000, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__continuity_reading, 0.08).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language label 'the correct Latin question' per the epsilon-invariance principle. continuity_reading (this file) authors medieval institutional practice as the beneficiary and humanist reformers as payers, with epsilon=0.58 reflecting a real-but-partial coordination function riding alongside disciplinary legitimacy extraction. discontinuity_reading would invert much of this structure, crediting humanist/discontinuity scholars as vindicated and treating medieval scholastic authority as resting on a constructed rather than organic continuity claim. hybrid_reading splits the difference at the morphology/syntax boundary. Each carries an independent epsilon; none is a measurement of the others under a different observable — they are three distinct constraints sharing a kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
