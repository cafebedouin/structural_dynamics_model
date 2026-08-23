% ============================================================================
% CONSTRAINT STORY: latin_correctness__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: latin_correctness__rupture_reading
 *   human_readable: Rupture Reading: Classical Latin as Fixed Reconstructed Standard, Medieval Usage as Corruption
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested latin_correctness
 *   kernel: the rupture reading, on which classical Latin is a fixed textual
 *   standard, recoverable by reconstruction from ancient sources, and
 *   post-classical usage — above all medieval usage — is corruption. The
 *   sibling readings (continuity_reading: medieval Latin as legitimate
 *   organic continuation; hybrid_reading: classical norms binding in literary
 *   domains, medieval forms legitimate in technical domains) are separate
 *   constraint files with their own epsilon values and victim sets; they are
 *   neither described nor averaged here. The epsilon referent is the standing
 *   rupture arrangement itself — the standard as enforced from the humanists
 *   through the twentieth century — assessed by this reading's own lights.
 *   Under that arrangement the standard coordinated a stable supranational
 *   scholarly language for roughly four centuries while simultaneously
 *   transferring authority, standing, and editorial control from
 *   post-classical practitioners to the canon's custodians, and classifying
 *   the entire medieval corpus as defective.
 *
 * KEY AGENTS:
 *   - classical_philologists: agenda setter (institutional / identity_locked) — administers the standard, produces the editions and grammars, certifies competence, collects authority and curricular control
 *   - renaissance_humanists: founding beneficiary (powerful / mobile) — built careers and patronage on the recovery program and the displacement of scholastic rivals
 *   - elite_educational_institutions: beneficiary and pedagogical enforcer (institutional / identity_locked) — admission, promotion, and graduation pass through Latin examination
 *   - technical_latin_practitioners: primary target (moderate / trapped) — theologians, jurists, physicians, clerks whose concepts have no classical vocabulary
 *   - medieval_manuscript_scholars: primary target (organized / constrained) — inherit a corpus pre-classified as defective; resist through counter-institutions
 *   - latin_students_elite_track: diffuse target (powerless / constrained) — pay years of drilling for the credential; secondary beneficiary of the credential itself
 *   - women_barred_from_latin_education: excluded voice (powerless / trapped) — outside the rooms where correctness was defined
 *   - historiographers_of_philology: analytical observer (analytical / analytical) — document the construction, enforcement, and contestation of the standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__rupture_reading, 0.62).
domain_priors:suppression_score(latin_correctness__rupture_reading, 0.32).
domain_priors:theater_ratio(latin_correctness__rupture_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__rupture_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__rupture_reading, "Rupture Reading: Classical Latin as Fixed Reconstructed Standard, Medieval Usage as Corruption").
narrative_ontology:topic_domain(latin_correctness__rupture_reading, "historical_linguistics/intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__rupture_reading, 'a8daf6e1-e2e5-496e-b9f8-ee68313656fe').
narrative_ontology:cs_kernel_codification('a8daf6e1-e2e5-496e-b9f8-ee68313656fe', fixed_text).
narrative_ontology:cs_authority_grounding('a8daf6e1-e2e5-496e-b9f8-ee68313656fe', lineage).
narrative_ontology:cs_interpretation_layer_present('a8daf6e1-e2e5-496e-b9f8-ee68313656fe').
narrative_ontology:cs_reading_relation('a8daf6e1-e2e5-496e-b9f8-ee68313656fe', latin_correctness__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('a8daf6e1-e2e5-496e-b9f8-ee68313656fe', latin_correctness__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('a8daf6e1-e2e5-496e-b9f8-ee68313656fe', foundational, classical_corpus_is_fixed_corrective_standard).
narrative_ontology:cs_axiom_status(classical_corpus_is_fixed_corrective_standard, holdable).
narrative_ontology:cs_axiom_grounding('a8daf6e1-e2e5-496e-b9f8-ee68313656fe', classical_corpus_is_fixed_corrective_standard, conventional).
narrative_ontology:cs_axiom('a8daf6e1-e2e5-496e-b9f8-ee68313656fe', foundational, post_classical_divergence_is_corruption).
narrative_ontology:cs_axiom_status(post_classical_divergence_is_corruption, holdable).
narrative_ontology:cs_axiom_grounding('a8daf6e1-e2e5-496e-b9f8-ee68313656fe', post_classical_divergence_is_corruption, empirically_contingent).
narrative_ontology:cs_reference_frame('a8daf6e1-e2e5-496e-b9f8-ee68313656fe', recoverable_golden_age_norm).
narrative_ontology:cs_drift_state('a8daf6e1-e2e5-496e-b9f8-ee68313656fe', post_descriptive_linguistics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a8daf6e1-e2e5-496e-b9f8-ee68313656fe', '').
narrative_ontology:cs_kernel_id(latin_correctness__rupture_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, renaissance_humanists).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, elite_educational_institutions).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, technical_latin_practitioners).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, medieval_manuscript_scholars).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, latin_students_elite_track).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, latin_students_elite_track).
narrative_ontology:constraint_vindicates(latin_correctness__rupture_reading, humanist_imitatio_doctrine).
narrative_ontology:constraint_vindicates(latin_correctness__rupture_reading, ciceronian_normativity).
narrative_ontology:constraint_vindicates(latin_correctness__rupture_reading, philological_reconstruction_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the canon of correct Latin, produce the critical editions and grammars through which the standard is taught, examine compositions, and staff the journals and appointments that certify competence. Their authority, funding, and curricular centrality depend on the canon remaining the measure of correctness; leaving the discipline would mean abandoning the expertise that constitutes their careers.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, classical_philologists, agenda_setter,
    institutional, generational, identity_locked, global).

% The fifteenth- and sixteenth-century scholars who recovered ancient manuscripts and styled themselves restorers of true eloquence. The verdict that scholastic Latin was barbarous gave them patronage, university chairs, and a printing market for their editions; they chose this program and could and did move between courts and cities selling it.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, renaissance_humanists, beneficiary,
    powerful, biographical, mobile, continental).

% Grammar schools, Jesuit colleges, and universities whose curricula, examinations, and prestige rest on classical training. They administer the standard pedagogically — admission, promotion, and graduation all pass through Latin examination — and collect the enrollment, fees, and reputational capital that the gatekeeping generates.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, elite_educational_institutions, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(latin_correctness__rupture_reading, elite_educational_institutions, agenda_setter).

% Scholastic theologians, canon lawyers, physicians, and chancery clerks who needed Latin for work whose concepts had no classical vocabulary — essence, quiddity, transubstantiation, feudal tenure, apothecary weights. Classicizing their prose meant paraphrase that obscured meaning; keeping their working idiom meant exposure as barbarous. Neither option fit, and their subject matter gave them nowhere else to stand.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, technical_latin_practitioners, payer,
    moderate, generational, trapped, continental).

% Editors and historians of medieval texts. Their sources speak in post-classical Latin, so under the standard every edition became an exercise in apology: silently correcting errors, prefacing volumes with excuses for their authors' latinity, and accepting second-tier disciplinary standing. Their resistance ran through counter-institutions — Du Cange's glossary, the Monumenta Germaniae Historica's editorial practices, and eventually an autonomous medieval-Latin philology.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, medieval_manuscript_scholars, payer,
    organized, generational, constrained, global).

% Boys routed through grammar school toward church, law, and civil service. They paid years of drill in grammar and versification as the price of elite entry, and received the credential that price purchased. Individually they had no say in the curriculum; collectively their families' ambitions kept them enrolled.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, latin_students_elite_track, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(latin_correctness__rupture_reading, latin_students_elite_track, beneficiary).

% Excluded from the grammar schools and universities where the standard was taught, and thus from the clerical, legal, and scholarly offices it gated. They built parallel vernacular learned cultures — salons, translation networks, religious houses — and had no seat in the conversations where correctness was defined.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, women_barred_from_latin_education, excluded,
    powerless, biographical, trapped, continental).

% Historians of linguistics and of scholarship who study how the classical standard was constructed, enforced, and contested. They document the humanist polemic, the schoolroom machinery, and the nineteenth- and twentieth-century rehabilitations of medieval Latin, and collect nothing from which way the dispute goes.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, historiographers_of_philology, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__rupture_reading, classical_philologists).
narrative_ontology:fixing_cost_class(latin_correctness__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single fixed target form of Latin — recoverable from a defined ancient corpus — so that composition, citation, and edition across linguistic and political boundaries aim at one stable standard instead of fragmenting into regional and technical varieties.
% TRANSFER_FUNCTION: Moves linguistic authority and cultural capital from post-classical practitioners to the custodians of the ancient canon; moves years of student labor into credentialing institutions; moves editorial control over every surviving medieval Latin text to editors trained in classical norms.
% ABSENT_VOICES: Medieval authors cannot answer the corruption charge made on their behalf; their modern defenders were heard, when at all, as defendants rather than participants. Women and non-elite learners were outside the room where correctness was defined. Vernacular advocates who wanted scholarship to leave Latin altogether were treated as deserters, not interlocutors.
% DISAPPEARANCE_RATIONALE: Editorial practice, school curricula, appointment and examination criteria, and the prestige ordering of the humanities all presuppose the standard; overnight removal would force every institution that teaches, examines, or edits Latin to rebuild its criteria from scratch, and would redistribute standing between classicists and medievalists immediately.
% FOUNDING_PROBLEM: After the western empire fell, Latin split into regional written varieties and technical dialects; scholars who needed to communicate across Europe lacked a common authoritative form of the language. Renaissance humanists answered by locating that authority in the recovered classics and displacing the scholastic idiom that had filled the gap.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Republic of Letters and of education — outside the benefiting parties — corroborate that the fragmentation problem was real and that a fixed standard stabilized scholarly communication for roughly three centuries. Linguists and medievalists, also outside the beneficiary set, corroborate that the corruption half of the humanist answer was contested from the start and is rejected by descriptive linguistics. No neutral party attests that the problem still requires this arrangement now that Latin composition has largely ceased.
narrative_ontology:disappearance_verdict(latin_correctness__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(latin_correctness__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__rupture_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness ends at 0.62: highest at the enforcement peak (1680-1760, 0.73-0.74) when drilling, examination gating, and editorial emendation were universal, declining as enforcement decayed but persisting in editorial norms and prestige hierarchies. Suppression (0.32 at endpoint) is authored as a raw structural property and is NOT scaled by power or scope — the engine owns any scaling, and only extractiveness is scaled. The suppression_requirement series traces enforcement capacity specifically: a ratchet upward through the print-and-Jesuit era (0.30 to 0.72), a plateau, then decay to 0.32 as vernacular education, historicism, and the medievalist coalition dismantled the machinery. Theater rises monotonically (0.20 to 0.44): the polemic and the philology were both real early; as composition died, a growing share of activity became ceremonial correctness — style prizes, inaugural Latin, ritual condemnation of barbarism — performed around a shrinking functional core. Accessibility_collapse 0.60: within the reading's own framework, once the standard is granted, alternatives (writing post-classical Latin, legitimating medieval forms) collapse almost entirely; but the rival readings stayed live outside it, so collapse sits far below natural-law levels. Resistance 0.62: sustained, organized, and eventually effective — Du Cange's glossary, the Monumenta Germaniae Historica's editorial practices, twentieth-century medieval Latin philology, and the general retreat from Latin requirements. All three series share one nine-point grid (1440-2020) so no metric is sampled against another's gaps.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (classical_philologists, with elite_educational_institutions enforcing) computes a coordination-dominant arrangement: they built the standard, maintain the editions, and experience the corruption verdict as quality control. The trapped seats compute a burden: technical_latin_practitioners faced a standard their subject matter made unattainable, medieval_manuscript_scholars inherited a corpus pre-classified as defective, and students paid years of labor for a credential. The same edifice is a lifeline from one seat and a tax from another; the engine computes this divergence from power, exit, and role data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collectors: classical_philologists (authority, chairs, editorial control), renaissance_humanists (patronage and the displacement of scholastic rivals), elite_educational_institutions (enrollment, fees, reputational capital). Victim declarations map to real bearers: technical_latin_practitioners (trapped — no classical vocabulary exists for their concepts), medieval_manuscript_scholars (constrained — their corpus is their field), latin_students_elite_track (individually powerless, damped slightly by the credential they receive as secondary beneficiaries). The excluded seat (women_barred_from_latin_education) feeds the consensus-provenance picture, not directionality. No directionality overrides are authored: role, power, and exit data already differentiate the seats, and the override mechanism keys on power atoms, which would collide here — institutional-power beneficiaries and institutional-power scholastics share a power atom but sit at opposite ends.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fragmentation of scholarly Latin after the fall of the western empire — was real, is corroborated from outside the beneficiary set, and was substantially solved: a fixed classical standard stabilized European scholarly communication for roughly three centuries. Composition in Latin has since largely ceased, so the coordination need is mostly gone, while the arrangement persists through editorial convention and inherited prestige: founding_problem_status is contested and disappearance_verdict is world_rearranges — the mismatch combination that flags zombie tendency. The classification prevents two opposite mislabels: reading the arrangement as pure coordination ignores the victims the corruption verdict created and keeps creating; reading it as pure extraction erases the genuine coordination the standard delivered while the need was live. The tangled-rope claim holds both facts: one structure, real coordination, asymmetric and enforced transfer. The rising theater series and the decay-phase omega (residual_function_or_theater) monitor the degraded-inertial trajectory without asserting it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the latin_correctness kernel — what would the sibling readings change structurally?',
    'Compare compiled classifications across the three sibling files: continuity_reading and hybrid_reading instantiate different victim sets and different epsilon values over the same historical material.',
    'Under the hybrid reading the victim set shrinks to literary-domain medievalists and epsilon falls; under the continuity reading the corruption verdict disappears and the arrangement approaches pure coordination. This file''s high-extraction profile is conditional on the rupture premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: which reading of the latin_correctness kernel this constraint instantiates and what siblings would change.').

omega_variable(
    corruption_verdict_status,
    'Is ''medieval usage is corruption'' a descriptive linguistic finding or a normative verdict dressed as one?',
    'Descriptive linguistics holds that all living languages change without decaying; test whether the rupture reading can state its criterion of corruption without circular appeal to classical preference.',
    'If the verdict is normative, the victim set''s justification collapses and the reading loses its empirical warrant, accelerating drift toward the continuity reading; if a defensible descriptive sense survives (for example, loss of inflectional transparency), part of the verdict stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corruption_verdict_status, conceptual, 'Whether the corruption premise is empirical or evaluative.').

omega_variable(
    reconstruction_attainability,
    'Can the classical standard actually be reconstructed from ancient sources, given transmission gaps, corrupt manuscripts, and the absence of any native speaker?',
    'Assess convergence across independent textual traditions and survey the admitted uncertainties in standard reference grammars (word order, pronunciation, idiomatic register).',
    'If reconstruction is materially indeterminate, the standard regulates by an ideal nobody can verify, and measured compliance is partly compliance with the examiner''s taste — raising the burden on every target seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_attainability, empirical, 'Whether the fixed standard is attainable enough to function as advertised.').

omega_variable(
    residual_function_or_theater,
    'Is the arrangement''s remaining operation functional (editorial and pedagogical rigor) or mostly theatrical (prestige ritual around a dead compositional practice)?',
    'Track whether classical-norm enforcement still changes outcomes (edition accuracy, scholarly communication) or only status (style prizes, ceremonial Latin, admissions signaling) as Latin composition declines.',
    'If the theatrical share keeps rising, the arrangement drifts toward the degraded-inertial type: maintained by inertia, burdening diffuse payers, profiting nobody enough to fix or remove it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_function_or_theater, empirical, 'Decay-phase trajectory question for the arrangement''s remaining operation.').

omega_variable(
    gatekeeping_constitutivity,
    'Was elite selection through Latin drilling an incidental side effect of the standard, or a constitutive function the standard existed partly to perform?',
    'Comparative history of credentialing: examine whether jurisdictions that loosened Latin requirements replaced them with equivalent filters, and whether contemporaries defended the standard in access terms rather than correctness terms.',
    'If selection was constitutive, removing the standard redistributes access regardless of linguistic merits, and the harm assessment must weight the excluded (women, non-elite learners) rather than only the drilled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_constitutivity, conceptual, 'Whether social sorting was the point of the arrangement or a byproduct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__rupture_reading, 1440, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t1440, latin_correctness__rupture_reading, theater_ratio, 1440, 0.2).
narrative_ontology:measurement_basis(lati_tr_t1440, observed).
narrative_ontology:measurement(lati_tr_t1520, latin_correctness__rupture_reading, theater_ratio, 1520, 0.22).
narrative_ontology:measurement_basis(lati_tr_t1520, observed).
narrative_ontology:measurement(lati_tr_t1600, latin_correctness__rupture_reading, theater_ratio, 1600, 0.25).
narrative_ontology:measurement_basis(lati_tr_t1600, observed).
narrative_ontology:measurement(lati_tr_t1680, latin_correctness__rupture_reading, theater_ratio, 1680, 0.28).
narrative_ontology:measurement_basis(lati_tr_t1680, observed).
narrative_ontology:measurement(lati_tr_t1760, latin_correctness__rupture_reading, theater_ratio, 1760, 0.3).
narrative_ontology:measurement_basis(lati_tr_t1760, observed).
narrative_ontology:measurement(lati_tr_t1840, latin_correctness__rupture_reading, theater_ratio, 1840, 0.33).
narrative_ontology:measurement_basis(lati_tr_t1840, observed).
narrative_ontology:measurement(lati_tr_t1900, latin_correctness__rupture_reading, theater_ratio, 1900, 0.36).
narrative_ontology:measurement_basis(lati_tr_t1900, observed).
narrative_ontology:measurement(lati_tr_t1960, latin_correctness__rupture_reading, theater_ratio, 1960, 0.4).
narrative_ontology:measurement_basis(lati_tr_t1960, observed).
narrative_ontology:measurement(lati_tr_t2020, latin_correctness__rupture_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement_basis(lati_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(lati_be_t1440, latin_correctness__rupture_reading, base_extractiveness, 1440, 0.45).
narrative_ontology:measurement_basis(lati_be_t1440, observed).
narrative_ontology:measurement(lati_be_t1520, latin_correctness__rupture_reading, base_extractiveness, 1520, 0.6).
narrative_ontology:measurement_basis(lati_be_t1520, observed).
narrative_ontology:measurement(lati_be_t1600, latin_correctness__rupture_reading, base_extractiveness, 1600, 0.69).
narrative_ontology:measurement_basis(lati_be_t1600, observed).
narrative_ontology:measurement(lati_be_t1680, latin_correctness__rupture_reading, base_extractiveness, 1680, 0.73).
narrative_ontology:measurement_basis(lati_be_t1680, observed).
narrative_ontology:measurement(lati_be_t1760, latin_correctness__rupture_reading, base_extractiveness, 1760, 0.74).
narrative_ontology:measurement_basis(lati_be_t1760, observed).
narrative_ontology:measurement(lati_be_t1840, latin_correctness__rupture_reading, base_extractiveness, 1840, 0.72).
narrative_ontology:measurement_basis(lati_be_t1840, observed).
narrative_ontology:measurement(lati_be_t1900, latin_correctness__rupture_reading, base_extractiveness, 1900, 0.68).
narrative_ontology:measurement_basis(lati_be_t1900, observed).
narrative_ontology:measurement(lati_be_t1960, latin_correctness__rupture_reading, base_extractiveness, 1960, 0.65).
narrative_ontology:measurement_basis(lati_be_t1960, observed).
narrative_ontology:measurement(lati_be_t2020, latin_correctness__rupture_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement_basis(lati_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t1440, latin_correctness__rupture_reading, suppression_requirement, 1440, 0.3).
narrative_ontology:measurement_basis(lati_su_t1440, observed).
narrative_ontology:measurement(lati_su_t1520, latin_correctness__rupture_reading, suppression_requirement, 1520, 0.48).
narrative_ontology:measurement_basis(lati_su_t1520, observed).
narrative_ontology:measurement(lati_su_t1600, latin_correctness__rupture_reading, suppression_requirement, 1600, 0.66).
narrative_ontology:measurement_basis(lati_su_t1600, observed).
narrative_ontology:measurement(lati_su_t1680, latin_correctness__rupture_reading, suppression_requirement, 1680, 0.72).
narrative_ontology:measurement_basis(lati_su_t1680, observed).
narrative_ontology:measurement(lati_su_t1760, latin_correctness__rupture_reading, suppression_requirement, 1760, 0.7).
narrative_ontology:measurement_basis(lati_su_t1760, observed).
narrative_ontology:measurement(lati_su_t1840, latin_correctness__rupture_reading, suppression_requirement, 1840, 0.63).
narrative_ontology:measurement_basis(lati_su_t1840, observed).
narrative_ontology:measurement(lati_su_t1900, latin_correctness__rupture_reading, suppression_requirement, 1900, 0.54).
narrative_ontology:measurement_basis(lati_su_t1900, observed).
narrative_ontology:measurement(lati_su_t1960, latin_correctness__rupture_reading, suppression_requirement, 1960, 0.42).
narrative_ontology:measurement_basis(lati_su_t1960, observed).
narrative_ontology:measurement(lati_su_t2020, latin_correctness__rupture_reading, suppression_requirement, 2020, 0.32).
narrative_ontology:measurement_basis(lati_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__rupture_reading, information_standard).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the classical Latin standard' decomposes into three readings of one kernel (epsilon-invariance decomposition). This file is the rupture reading; the siblings are separate stories. Epsilon differs across the family because the victim sets differ: the rupture reading universalizes the corruption verdict and thereby maximizes the victim set (all post-classical practice, all technical domains, all drilled students); the hybrid reading confines the verdict to literary domains; the continuity reading eliminates it. Historical edge structure: the rupture reading dominated from the fifteenth through the nineteenth century and its enforcement pressure is what made the hybrid compromise intelligible; the continuity reading gained ground from historicism and descriptive linguistics onward.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
