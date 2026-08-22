% ============================================================================
% CONSTRAINT STORY: latin_correctness__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__rupture_reading, []).

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
 *   constraint_id: latin_correctness__rupture_reading
 *   human_readable: Rupture Reading of Classical Latin Correctness (Ciceronian Standard Against Medieval Corruption)
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   This story instantiates the rupture reading of the latin_correctness
 *   kernel: the position, consolidated by Renaissance humanist philology,
 *   that classical (Ciceronian/Augustan) Latin constitutes the only
 *   legitimate standard, recoverable only by reconstruction from surviving
 *   ancient sources, and that the roughly one thousand years of medieval
 *   Latin usage — administrative, legal, scholastic, liturgical, and
 *   technical — represents corruption and decline rather than legitimate
 *   linguistic development. Under this reading, ε is authored high because
 *   the standard's operation actively delegitimizes an entire functioning
 *   textual tradition and redirects scholarly prestige, curricular authority,
 *   and publishing markets toward those trained in the reconstructed
 *   classical norm. This is emphatically ONE reading among three live
 *   readings of the same kernel (continuity_reading holds medieval Latin as
 *   organic continuation; hybrid_reading splits legitimacy by domain). Those
 *   sibling readings are separate constraint stories with their own ε and
 *   stakeholder structures — this file does not average across them or hedge
 *   between them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__rupture_reading, 0.72).
domain_priors:suppression_score(latin_correctness__rupture_reading, 0.68).
domain_priors:theater_ratio(latin_correctness__rupture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__rupture_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__rupture_reading, "Rupture Reading of Classical Latin Correctness (Ciceronian Standard Against Medieval Corruption)").
narrative_ontology:topic_domain(latin_correctness__rupture_reading, "historical_linguistics/intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__rupture_reading, 'db17f3dd-2325-4690-a65a-9b154069060d').
narrative_ontology:cs_kernel_codification('db17f3dd-2325-4690-a65a-9b154069060d', fixed_text).
narrative_ontology:cs_authority_grounding('db17f3dd-2325-4690-a65a-9b154069060d', lineage).
narrative_ontology:cs_interpretation_layer_present('db17f3dd-2325-4690-a65a-9b154069060d').
narrative_ontology:cs_reading_relation('db17f3dd-2325-4690-a65a-9b154069060d', latin_correctness__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('db17f3dd-2325-4690-a65a-9b154069060d', latin_correctness__hybrid_reading, influences).
narrative_ontology:cs_axiom('db17f3dd-2325-4690-a65a-9b154069060d', foundational, classical_corpus_is_sole_legitimate_reference).
narrative_ontology:cs_axiom_status(classical_corpus_is_sole_legitimate_reference, holdable).
narrative_ontology:cs_axiom_grounding('db17f3dd-2325-4690-a65a-9b154069060d', classical_corpus_is_sole_legitimate_reference, conventional).
narrative_ontology:cs_axiom('db17f3dd-2325-4690-a65a-9b154069060d', foundational, post_classical_deviation_constitutes_corruption).
narrative_ontology:cs_axiom_status(post_classical_deviation_constitutes_corruption, holdable).
narrative_ontology:cs_axiom_grounding('db17f3dd-2325-4690-a65a-9b154069060d', post_classical_deviation_constitutes_corruption, empirically_contingent).
narrative_ontology:cs_reference_frame('db17f3dd-2325-4690-a65a-9b154069060d', ciceronian_augustan_textual_corpus).
narrative_ontology:cs_drift_state('db17f3dd-2325-4690-a65a-9b154069060d', post_medieval_millennium, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('db17f3dd-2325-4690-a65a-9b154069060d', '').
narrative_ontology:cs_kernel_id(latin_correctness__rupture_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, renaissance_humanist_scholars).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, classical_philology_faculties).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, critical_edition_publishers).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, medieval_latin_scholars).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, scholastic_philosophy_tradition).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, vernacular_adjacent_technical_writers).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, monastic_scriptoria_traditions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish Ciceronian and Augustan-era usage as the sole correct standard, reconstructed philologically from surviving classical manuscripts. They set curricula, adjudicate what counts as 'barbarous' Latin, and gatekeep entry into humanist scholarly networks and patronage. Their own careers and prestige are built on demonstrated mastery of the reconstructed standard, so they have no exit from the standard they impose — but they set its terms rather than bear its costs.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, renaissance_humanist_scholars, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__rupture_reading, renaissance_humanist_scholars, beneficiary).

% Institutionalize the rupture standard in university curricula and textual criticism methodology. Their disciplinary authority and hiring criteria depend on classical philology being treated as the legitimate custodian of 'true' Latin, generating steady demand for their expertise in editing and teaching reconstructed texts.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, classical_philology_faculties, beneficiary,
    institutional, civilizational, arbitrage, continental).

% Produce and sell critical editions purged of medieval scribal 'corruptions,' profiting from the market the rupture standard creates for restored classical texts and the philological apparatus needed to certify authenticity.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, critical_edition_publishers, beneficiary,
    organized, generational, mobile, continental).

% Wrote and taught in a living, functional Latin adapted for scholastic logic, law, and administration for nearly a millennium. Under the rupture standard their entire textual corpus is relabeled corrupt or barbarous, their training devalued, and their scholarly authority erased retroactively. They cannot exit the judgment because it is applied to work already completed and to a tradition they cannot un-inherit.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, medieval_latin_scholars, payer,
    moderate, biographical, trapped, continental).

% Depends on technical scholastic vocabulary (quidditas, haecceitas, and similar coinages) unavailable in classical sources. Under the rupture standard this vocabulary is treated as evidence of degeneration rather than legitimate philosophical development, threatening the tradition's legibility and legitimacy to humanist-trained successors who control academic gatekeeping.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, scholastic_philosophy_tradition, payer,
    moderate, generational, constrained, continental).

% Write practical Latin for medicine, law, and commerce that absorbs vernacular terms for objects and processes classical authors never needed to name. They have no plausible path to classical purity because the concepts they describe did not exist in antiquity, so the standard structurally cannot be met by their domain regardless of effort.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, vernacular_adjacent_technical_writers, payer,
    powerless, biographical, trapped, regional).

% Copied, glossed, and transmitted the manuscript tradition that preserved classical texts in the first place, often adapting orthography and grammar for their own liturgical and administrative Latin. The rupture reading treats their transmission work as contamination of the sources it simultaneously depends on for its reconstruction.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, monastic_scriptoria_traditions, payer,
    moderate, generational, constrained, regional).

% Study the humanist standard itself as a historical artifact, documenting how the rupture framing was constructed for status competition among Renaissance scholars rather than derived from linguistic necessity.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, modern_philological_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__rupture_reading, renaissance_humanist_scholars).
narrative_ontology:fixing_cost_class(latin_correctness__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable reference standard for Latin composition and textual criticism, allowing scholars across Europe to agree on what counts as correct usage and enabling systematic reconstruction of damaged or corrupted manuscript readings against a fixed target.
% TRANSFER_FUNCTION: Moves prestige, institutional position, and interpretive authority from medieval-trained scholars and scholastic institutions to humanist-trained scholars and the philological apparatus they control; moves legitimacy away from a millennium of functional technical, legal, and philosophical Latin toward a narrower classical corpus.
% ABSENT_VOICES: Medieval scholars themselves are mostly dead or professionally sidelined by the time the rupture standard consolidates in the 15th-16th centuries; scholastic philosophers whose technical vocabulary is declared barbarous have no seat in the humanist academies setting the new curricula; practical/technical Latin writers in medicine and law are not consulted because their domain is treated as beneath serious philological attention.
% DISAPPEARANCE_RATIONALE: If the rupture standard vanished, scholastic philosophy, medieval legal Latin, and monastic textual traditions would be readmitted as legitimate objects of study on their own terms rather than as deviations to be corrected; philology curricula would need to justify classical primacy on grounds other than 'medieval usage is corruption'; critical editions would lose their implicit hierarchy between 'authentic' and 'corrupted' readings.
% FOUNDING_PROBLEM: Renaissance humanists confronted a manuscript tradition visibly altered across a millennium of copying, with orthography, vocabulary, and syntax drifting from classical models, alongside a desire to recover and imitate what they saw as a superior literary and rhetorical culture.
% FOUNDING_PROBLEM_CORROBORATION: Humanist scholars and their institutional descendants attest the classical standard is necessary to prevent linguistic degeneration. Outside the beneficiary set, modern historical linguists and medievalists attest that medieval Latin was a fully functional, internally coherent living language undergoing normal diachronic change, not decay — this corroboration comes from disciplines (historical linguistics, medieval studies) that did not exist to defend the humanist project and in some cases were founded partly to correct it.
narrative_ontology:disappearance_verdict(latin_correctness__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__rupture_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply from 0.35 at the kernel's early consolidation (roughly the 14th century, when humanist critique of medieval Latin first gains traction) to a plateau near 0.72-0.74 by the height of institutionalized classical philology, reflecting the standard's transition from a stylistic preference into an enforced disciplinary and curricular gate. Theater ratio rises in parallel (0.2 to 0.45) as humanist rhetoric about 'barbarous' medieval Latin increasingly serves status competition among scholars rather than genuine textual-critical necessity, then eases slightly as philology professionalizes and some performative denunciation gives way to more technical critical-edition work. Suppression requirement tracks the active work of excluding medieval usage from legitimate curricula and publication, peaking as humanist institutions consolidate control over what counts as correct Latin, then settling as the exclusion becomes normalized and requires less active argument.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist agenda-setter seat, the rupture standard looks like coordination: a shared, verifiable reference point that lets scholars across fragmented European institutions agree on correctness and do disciplined textual reconstruction. From the payer seats — medieval scholars, scholastic philosophers, technical writers — the identical standard operates as enforced delegitimization of a functioning, internally coherent tradition whose 'errors' were adaptive developments, not failures. The engine should compute these as structurally different experiences of one constraint: genuine coordination function (shared reconstruction standard, verifiable textual criticism) coexisting with asymmetric extraction (prestige and institutional position moving toward the standard's authors and away from those judged by it) — which is exactly the tangled_rope signature rather than a pure rope or pure snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist scholars and the philological institutions they founded are the structural beneficiaries: they collect prestige, curricular control, and publishing revenue from a standard they authored and administer, so their directionality sits near the full-beneficiary end despite formally being bound by the same classical standard they impose — they set its terms, so the binding costs them little. Medieval scholars, scholastic philosophers, and vernacular-adjacent technical writers are structural targets: the standard was constructed after their work was completed and evaluates it retroactively by criteria their tradition never claimed to satisfy, and they have no exit — the judgment attaches to a corpus and a training that cannot be revised. Vernacular-adjacent technical writers sit closest to the full-target end: their subject matter (medieval commerce, medicine, novel technical processes) has no classical vocabulary at all, so the standard is structurally unmeetable for their domain regardless of skill, effort, or intent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — visible manuscript drift and a desire to recover classical literary quality — was real at the kernel's origin, but by the time the rupture reading fully consolidates, medieval Latin's supposed 'corruption' had already functioned successfully as a living scholarly, legal, and philosophical language for centuries; the founding problem's contemporary force is contested rather than dead or clearly live, which is why founding_problem_status is authored as contested rather than resolved in either direction. The tangled_rope classification (rather than pure snare) matters because it forces the analysis to hold both the standard's genuine coordination value for textual criticism AND its extractive delegitimization of medieval scholarship simultaneously, rather than collapsing into either 'humanism was purely a scholarly triumph' or 'humanism was purely a status grab' — both partial readings are present in the historical record.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_reading_kernel_identity,
    'Is ''the Latin correctness standard'' actually one contested kernel with the rupture reading as its most extreme instantiation, or does the rupture reading effectively BECOME a different kernel once it fully displaces continuity and hybrid framings in institutional practice?',
    'Track whether historical humanist institutions ever formally engaged with continuity or hybrid arguments as live alternatives, versus simply treating rupture as settled fact from the start — the presence or absence of documented internal contestation distinguishes ''one kernel, three readings'' from ''the rupture reading effectively founded its own kernel.''',
    'If rupture effectively founded a new kernel rather than reading an existing one, this story''s framing as one-of-three-siblings would need revision, though the ε value and structural analysis would remain unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_reading_kernel_identity, conceptual, 'Whether rupture_reading is genuinely one reading among live siblings or a kernel-founding move disguised as a reading.').

omega_variable(
    sibling_reading_structural_delta_location,
    'Where exactly does the disagreement between rupture_reading and its siblings (continuity_reading, hybrid_reading) live structurally — is it located in the definition of ''corruption'' itself, in the domain-boundary between literary and technical Latin, or in the historical evidentiary weight given to organic language change versus scribal error?',
    'Comparative analysis of the beneficiary/victim structures across all three sibling constraint files: continuity_reading should show near-zero victims (medieval usage is simply legitimate), hybrid_reading should show a narrower victim set confined to literary/rhetorical domains, and this file''s broader victim set (including scholastic philosophy and technical writing) should be the widest of the three.',
    'Confirms or disconfirms that the three readings are genuinely structurally distinct constraints (per the ε-invariance principle) rather than the same constraint viewed with different rhetorical emphasis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta_location, conceptual, 'Locating the precise structural disagreement between sibling kernel readings.').

omega_variable(
    humanist_beneficiary_naturalness_ambiguity,
    'Is the classical Latin standard, under the rupture reading, closer to a natural-law-like claim (that classical usage genuinely represents linguistic excellence recoverable through philology) or a constructed status hierarchy that benefits identifiable Renaissance humanist institutions?',
    'Examine whether the criteria for ''correct'' Latin under the rupture reading are independently derivable from linguistic principles (regularity, expressive capacity, historical priority) or whether they consistently track whatever usage happens to appear in texts humanists already valued for other (literary, political) reasons.',
    'If the standard is substantially status-tracking rather than linguistically principled, this strengthens the tangled_rope classification and the extraction analysis; if substantially principled, some of the authored extractiveness may reflect genuine coordination cost rather than rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanist_beneficiary_naturalness_ambiguity, conceptual, 'Whether classical correctness criteria are linguistically principled or status-tracking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__rupture_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__rupture_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lati_tr_t50, latin_correctness__rupture_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(lati_tr_t100, latin_correctness__rupture_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement(lati_tr_t200, latin_correctness__rupture_reading, theater_ratio, 200, 0.45).
narrative_ontology:measurement(lati_tr_t350, latin_correctness__rupture_reading, theater_ratio, 350, 0.42).
narrative_ontology:measurement(lati_tr_t500, latin_correctness__rupture_reading, theater_ratio, 500, 0.4).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lati_be_t50, latin_correctness__rupture_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(lati_be_t100, latin_correctness__rupture_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement(lati_be_t200, latin_correctness__rupture_reading, base_extractiveness, 200, 0.74).
narrative_ontology:measurement(lati_be_t350, latin_correctness__rupture_reading, base_extractiveness, 350, 0.71).
narrative_ontology:measurement(lati_be_t500, latin_correctness__rupture_reading, base_extractiveness, 500, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t0, latin_correctness__rupture_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(lati_su_t50, latin_correctness__rupture_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(lati_su_t100, latin_correctness__rupture_reading, suppression_requirement, 100, 0.7).
narrative_ontology:measurement(lati_su_t200, latin_correctness__rupture_reading, suppression_requirement, 200, 0.72).
narrative_ontology:measurement(lati_su_t350, latin_correctness__rupture_reading, suppression_requirement, 350, 0.66).
narrative_ontology:measurement(lati_su_t500, latin_correctness__rupture_reading, suppression_requirement, 500, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(latin_correctness__rupture_reading, 0.08).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint forms one of three sibling readings of the latin_correctness kernel. rupture_reading (this file) authors the highest ε (0.72) and the broadest victim set, encompassing scholastic philosophy and vernacular-adjacent technical Latin alongside medieval Latin scholarship generally. continuity_reading is expected to author near-zero ε with no meaningful victim set, treating medieval Latin as ordinary diachronic development. hybrid_reading is expected to author intermediate ε with a victim set confined to literary/rhetorical medieval Latin, sparing technical and practical domains from the classical standard. Each file's beneficiary/victim declarations and ε value must be evaluated independently by its own reading's lights; none of the three should be treated as more 'true' than the others by the engine — they are structurally distinct constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
