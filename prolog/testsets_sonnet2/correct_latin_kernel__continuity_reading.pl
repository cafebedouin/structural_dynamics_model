% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Medieval Latin as Continuous Classical Latin (Naturalist-Evolution Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This story instantiates the continuity reading of the
 *   correct_latin_kernel: the claim that Medieval Latin is Classical Latin
 *   having undergone ordinary natural linguistic evolution, such that its
 *   distinctive morphology, syntax, and lexicon are internal developments
 *   rather than corruptions requiring external correction. Under this
 *   reading, humanist reconstruction efforts (Renaissance prescriptive
 *   grammar, Ciceronian purism) are recast as unwarranted retroactive
 *   impositions on a living, legitimately-evolved tradition. This is
 *   deliberately ONE of three sibling readings of the same kernel
 *   (discontinuity_reading and hybrid_reading are separate constraint
 *   stories); this story's ε, beneficiaries, and victims are authored solely
 *   from the continuity reading's own internal logic and are not averaged
 *   against or hedged by the sibling readings' claims.
 *
 * KEY AGENTS:
 *   - medieval_scholastic_institutions: agenda_setter/beneficiary (institutional/arbitrage) — certifies medieval usage as correct Latin
 *   - renaissance_humanist_reformers: payer (powerful/constrained) — their corrective project is delegitimized
 *   - students_taught_via_prescriptive_correction: payer (powerless/trapped) — bore historical cost of humanist discipline now reframed as unjustified
 *   - historical_linguistics_discipline: observer (analytical/analytical) — adjudicates the kernel contest with comparative evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, 0.42).
domain_priors:suppression_score(correct_latin_kernel__continuity_reading, 0.55).
domain_priors:theater_ratio(correct_latin_kernel__continuity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__continuity_reading, "Medieval Latin as Continuous Classical Latin (Naturalist-Evolution Reading)").
narrative_ontology:topic_domain(correct_latin_kernel__continuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__continuity_reading, '1b8f2236-958f-488e-a69c-6cf388a8e146').
narrative_ontology:cs_kernel_codification('1b8f2236-958f-488e-a69c-6cf388a8e146', distributed).
narrative_ontology:cs_authority_grounding('1b8f2236-958f-488e-a69c-6cf388a8e146', practice).
narrative_ontology:cs_interpretation_layer_present('1b8f2236-958f-488e-a69c-6cf388a8e146').
narrative_ontology:cs_reading_relation('1b8f2236-958f-488e-a69c-6cf388a8e146', correct_latin_kernel__discontinuity_reading, forecloses).
narrative_ontology:cs_reading_relation('1b8f2236-958f-488e-a69c-6cf388a8e146', correct_latin_kernel__hybrid_reading, influences).
narrative_ontology:cs_axiom('1b8f2236-958f-488e-a69c-6cf388a8e146', foundational, usage_constitutes_correctness).
narrative_ontology:cs_axiom_status(usage_constitutes_correctness, holdable).
narrative_ontology:cs_axiom_grounding('1b8f2236-958f-488e-a69c-6cf388a8e146', usage_constitutes_correctness, conventional).
narrative_ontology:cs_axiom('1b8f2236-958f-488e-a69c-6cf388a8e146', foundational, no_systemic_rupture_occurred).
narrative_ontology:cs_axiom_status(no_systemic_rupture_occurred, holdable).
narrative_ontology:cs_axiom_grounding('1b8f2236-958f-488e-a69c-6cf388a8e146', no_systemic_rupture_occurred, empirically_contingent).
narrative_ontology:cs_axiom('1b8f2236-958f-488e-a69c-6cf388a8e146', secondary, humanist_correction_is_illegitimate_imposition).
narrative_ontology:cs_axiom_status(humanist_correction_is_illegitimate_imposition, holdable).
narrative_ontology:cs_axiom_grounding('1b8f2236-958f-488e-a69c-6cf388a8e146', humanist_correction_is_illegitimate_imposition, conventional).
narrative_ontology:cs_reference_frame('1b8f2236-958f-488e-a69c-6cf388a8e146', classical_ciceronian_norm).
narrative_ontology:cs_drift_state('1b8f2236-958f-488e-a69c-6cf388a8e146', high_medieval_scholastic_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1b8f2236-958f-488e-a69c-6cf388a8e146', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__continuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, medieval_scholastic_institutions).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, vernacular_linguists_of_continuity).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, church_administrative_latinists).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, renaissance_humanist_reformers).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, students_taught_via_prescriptive_correction).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, natural_language_change_hypothesis).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, continuous_transmission_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Universities, monasteries, and chanceries that produced and taught Medieval Latin as a living administrative and scholarly language. They set the norms of correct usage for their era, treating morphological and syntactic shifts (new subordination patterns, expanded vocabulary, altered case usage) as internal developments requiring no external correction. Their authority to certify correct Latin depends on the continuity claim being accepted.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, medieval_scholastic_institutions, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__continuity_reading, medieval_scholastic_institutions, beneficiary).

% Modern historical linguists who read Medieval Latin's changes as ordinary diachronic drift analogous to any living language's evolution. They benefit professionally and theoretically from a framework that treats the medieval corpus as unbroken evidence of natural change, supporting broader claims about how languages evolve without external rupture.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, vernacular_linguists_of_continuity, beneficiary,
    moderate, civilizational, analytical, continental).

% Clerics and administrators who used Medieval Latin daily for governance, liturgy, and law. Their professional competence and institutional authority rest on Medieval Latin being simply 'correct Latin' in its own right, not a degraded or reconstructed form requiring humanist emendation.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, church_administrative_latinists, beneficiary,
    institutional, generational, arbitrage, continental).

% Humanists (Petrarch, Valla, and successors) who treated Ciceronian Latin as the sole legitimate standard and Medieval Latin usage as corruption to be purged. Under the continuity reading, their prescriptive program is recast as illegitimate purism imposed retroactively on a living tradition, delegitimizing their corrective project and the institutional apparatus (humanist academies, printing conventions) built on it.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, renaissance_humanist_reformers, payer,
    powerful, biographical, constrained, continental).

% Generations of students disciplined under humanist pedagogy for 'barbarisms' inherited from medieval usage. If the continuity reading is correct, the corrective apparatus that punished them for using historically legitimate forms was pedagogically unjustified — they bore the cost of a prescriptive standard that this reading treats as a category error.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, students_taught_via_prescriptive_correction, payer,
    powerless, biographical, trapped, regional).

% Philologists committed to Ciceronian Latin as the reference standard would object that treating medieval forms as mere continuation erases the qualitative rupture they perceive in syntax and lexicon after the fall of centralized Roman education. Their objection is largely absent from the continuity reading's own framing, which treats the rupture claim as a category error rather than engaging its evidentiary basis.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, classical_philologists, excluded,
    organized, civilizational, analytical, global).

% The broader discipline adjudicates competing readings of the Latin kernel using comparative evidence from other language continua (e.g., Greek diglossia, Arabic fusha/dialect split) without institutional stake in any single reading's victory.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, historical_linguistics_discipline, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, teachable standard of 'correct Latin' for administrative, liturgical, and scholarly use across a linguistically fragmented medieval Europe, without requiring each institution to independently reconstruct a lost classical norm.
% TRANSFER_FUNCTION: Moves prestige and pedagogical authority from institutions that would certify Latin by external classical reconstruction toward institutions whose living, evolved usage is itself declared the legitimate continuation — and moves retrospective legitimacy away from humanist correctors, recasting their reform project as an unwarranted imposition.
% ABSENT_VOICES: Classical philologists and humanist-tradition pedagogues who hold that a real rupture occurred are structurally outside this reading's own framing, since the reading's core premise (no rupture, only evolution) treats their evidentiary claims as a category confusion rather than engaging them directly.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished as an operative frame, medieval textual corpora currently treated as first-class linguistic evidence would be reclassified as corrupted or transitional material, humanist prescriptive grammar would regain uncontested legitimacy as the corrective standard, and pedagogical and editorial practices built on treating medieval forms as 'valid Latin' would require revision.
% FOUNDING_PROBLEM: Medieval institutions needed a functional, teachable, self-consistent Latin for governance and scholarship without access to (or interest in) reconstructing a lost classical register; the continuity framing solved the problem of legitimizing whatever Latin was actually in productive use.
% FOUNDING_PROBLEM_CORROBORATION: Comparative historical linguists working outside both the medieval scholastic tradition and the humanist tradition (e.g., scholars of Romance language emergence, using evidence from other diglossic traditions) attest that natural-evolution readings of stable-written/spoken-divergent language communities are a documented cross-linguistic pattern, lending outside support to the continuity claim; however, they do not universally corroborate that Medieval Latin specifically followed this pattern rather than experiencing textual reoccupation, so full outside corroboration is only partial.
narrative_ontology:disappearance_verdict(correct_latin_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__continuity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__continuity_reading_tests).
:- end_tests(correct_latin_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 by interval end) because the continuity reading, while it delegitimizes a rival tradition's prescriptive authority, does not itself extract material resources so much as reallocate scholarly and pedagogical legitimacy — the cost to humanist reformers and their pedagogical descendants is reputational and institutional rather than economic. Suppression (0.55) reflects that the continuity reading, once institutionally dominant within its own tradition (scholastic universities, chancery Latin instruction), actively displaced humanist correction as the operative standard within its domain, requiring the ongoing work of treating classicizing 'barbarism' charges as illegitimate. Theater ratio rises over the interval (0.15 to 0.4) as the coordination function (a workable, teachable Latin standard) gradually accumulates performative reinforcement — scholarly citation practices and institutional self-justification defending the continuity claim independent of fresh evidentiary work.
 *
 * PERSPECTIVAL GAP:
 *   From the medieval institutional seat, the continuity reading is simply an accurate description of how Latin actually worked — internal correction, not external judgment. From the humanist reformer seat, the same reading appears as an act of legitimizing corruption and denying the reformers' corrective authority. The engine should compute these as structurally different experiences of the same kernel commitment, not resolve them into a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval scholastic and administrative institutions sit at the beneficiary end: their authority to certify correct Latin depends on the continuity premise, and they set its terms. Vernacular linguists of continuity benefit theoretically and professionally from evidence supporting general natural-language-change models. Humanist reformers and the students disciplined under their prescriptive program sit at the target end: the continuity reading directly delegitimizes the humanist corrective project and retroactively invalidates the pedagogical cost imposed on those students. Classical philologists are excluded rather than coordinated — their rupture-based evidence is treated as a category confusion internal to the continuity reading's own logic, not engaged on its own terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading's founding problem — providing a workable, teachable Latin standard without classical reconstruction — was genuinely live throughout the medieval period (contested status, not dead): institutions actually needed a functional written/administrative Latin and the reading legitimately solved that need. Classifying this as tangled_rope rather than pure snare or pure rope acknowledges that the coordination function (a usable standard) is real and was not manufactured as cover, while the asymmetric cost imposed on the humanist tradition and its pedagogical inheritors is also real and required active institutional enforcement (through university curricula and ecclesiastical Latin instruction) to sustain against the rival humanist standard.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is Medieval Latin''s relationship to Classical Latin better modeled as unbroken natural evolution (this reading), sharp systemic discontinuity requiring textual reoccupation (discontinuity_reading), or a layered mix where morphology continued but syntax/lexicon required recovery (hybrid_reading)?',
    'Comparative corpus analysis tracking specific morphological and syntactic features across the transition period, cross-referenced against known cases of natural diglossic drift in other language traditions (Greek katharevousa/demotic, Arabic fusha/ammiya) to establish whether the observed changes pattern-match natural evolution or textual rupture-and-recovery.',
    'If discontinuity evidence dominates, this reading''s foundational premise (no rupture, only internal correction) is undermined and its delegitimization of humanist reform loses its evidentiary basis, shifting classification pressure toward the discontinuity or hybrid siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, empirical, 'Which of the three kernel readings best fits the documentary and comparative-linguistic evidence.').

omega_variable(
    continuity_as_institutional_convenience,
    'Does the continuity reading persist because it is linguistically well-supported, or because it is institutionally convenient for the scholastic and ecclesiastical institutions whose authority depends on their own Latin being ''correct'' without need of external correction?',
    'Trace whether continuity-reading adoption correlates more strongly with institutional self-interest (universities and chanceries defending their own linguistic practice) than with independent philological argument advanced by parties without institutional stake in the outcome.',
    'If adoption tracks institutional interest more than evidence, the reading functions partly as legitimation cover for existing practice (supporting the tangled_rope classification and the false-summit-adjacent concern that a natural-evolution claim is doing extractive legitimation work); if adoption tracks independent evidence, the coordination function dominates and classification should shift toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continuity_as_institutional_convenience, conceptual, 'Whether the continuity reading''s persistence is evidence-driven or interest-driven.').

omega_variable(
    committer_framing_alternative,
    'An alternative framing treats the kernel not as ''what is correct Latin'' but as ''who has authority to declare correctness'' — under that framing, this story''s declared authority_grounding (practice, the scholastic/administrative community''s own usage) competes with a lineage-grounded framing where authority derives from continuity with classical texts themselves rather than from the practicing community. Which framing better captures the actual locus of contested authority?',
    'Examine primary medieval and humanist sources for their own stated justification for correctness claims — do they appeal to community usage (supporting practice-grounding) or to textual fidelity to ancient authors (supporting lineage-grounding)?',
    'If sources predominantly appeal to textual lineage even within medieval institutions, the authority_grounding value should shift from practice toward lineage, which would also affect whether interpretation_layer_present is coherently assigned.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_alternative, conceptual, 'Alternative framing of where authority is grounded in the continuity reading, and what would change under it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(corr_tr_t15, correct_latin_kernel__continuity_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(corr_tr_t30, correct_latin_kernel__continuity_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(corr_tr_t50, correct_latin_kernel__continuity_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement(corr_tr_t70, correct_latin_kernel__continuity_reading, theater_ratio, 70, 0.37).
narrative_ontology:measurement(corr_tr_t100, correct_latin_kernel__continuity_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__continuity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(corr_be_t15, correct_latin_kernel__continuity_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(corr_be_t30, correct_latin_kernel__continuity_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(corr_be_t50, correct_latin_kernel__continuity_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(corr_be_t70, correct_latin_kernel__continuity_reading, base_extractiveness, 70, 0.4).
narrative_ontology:measurement(corr_be_t100, correct_latin_kernel__continuity_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__continuity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(corr_su_t15, correct_latin_kernel__continuity_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(corr_su_t30, correct_latin_kernel__continuity_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(corr_su_t50, correct_latin_kernel__continuity_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(corr_su_t70, correct_latin_kernel__continuity_reading, suppression_requirement, 70, 0.52).
narrative_ontology:measurement(corr_su_t100, correct_latin_kernel__continuity_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__continuity_reading, 0.1).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the correct_latin_kernel, each authored as a structurally distinct, ε-invariant constraint per the ε-invariance principle: continuity_reading (this story, ε=0.42, tangled_rope), discontinuity_reading (distinct ε, likely lower extraction since it treats humanist reconstruction as legitimate scholarly recovery rather than illegitimate correction), and hybrid_reading (a layered ε reflecting partial continuity/partial reconstruction). The three are linked via affects_constraints rather than merged into one story with a measurement parameter, since averaging or parameterizing across them would violate DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
