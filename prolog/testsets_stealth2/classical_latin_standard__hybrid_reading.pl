% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__hybrid_reading, []).

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
 *   constraint_id: classical_latin_standard__hybrid_reading
 *   human_readable: Hybrid Standard of Correct Latin: Classical Fidelity with Licensed Registers
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   A distributed standard governs what counts as correct Latin: the
 *   Classical corpus (Cicero, Virgil, and the school canon) remains the
 *   corrective measure, while designated technical and ecclesiastical
 *   registers receive a licensed exemption that lets theological, legal, and
 *   scientific vocabularies persist without the charge of barbarism. The
 *   standard is administered through lexicographic projects, university
 *   curricula, editorial review, and curial correction; it is contested at
 *   its edges by purists who want the exemption withdrawn and by living-use
 *   advocates who want the anchor dissolved. This file instantiates the
 *   HYBRID READING of the classical_latin_standard kernel only: per the
 *   epsilon-invariance principle, the sibling readings (continuity,
 *   reconstruction) are separate constraint stories with their own epsilon
 *   values over the same standing arrangement, linked through
 *   network.affects_constraints. Time mapping for the interval: t=0
 *   corresponds to approximately 1925 (consolidation of the modern
 *   philological apparatus and peak school enforcement), t=100 to
 *   approximately 2025. KEY AGENTS (by structural relationship): -
 *   international_philological_academies: Agenda setter
 *   (institutional/constrained) — administers the authorized lexicon and
 *   edition norms - university_classics_faculties: Agenda setter and
 *   beneficiary (institutional/constrained) — controls the credentialing
 *   gateway - roman_curia_liturgical_offices: Primary beneficiary with payer
 *   exposure (institutional/constrained) — retains technical vocabulary under
 *   Classical review - scientific_nomenclature_users: Beneficiary
 *   (organized/mobile) — stable naming baseline against vernacular drift -
 *   latin_language_students: Primary target (powerless/constrained) — bears
 *   correction and examination costs - living_latin_practitioners: Target
 *   (moderate/constrained) — fluent usage filtered by Classical review -
 *   late_antique_vulgar_text_scholars: Target (moderate/mobile) — unlicensed
 *   material carries inherited stigma - romance_language_educators: Excluded
 *   voice (organized/mobile) — descendants of the graded drift, no seat in
 *   standard-setting - latin_rite_laity: Excluded voice (powerless/trapped) —
 *   bore liturgical opacity without a channel - historical_linguists:
 *   Analytical observer — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, 0.45).
domain_priors:suppression_score(classical_latin_standard__hybrid_reading, 0.35).
domain_priors:theater_ratio(classical_latin_standard__hybrid_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__hybrid_reading, "Hybrid Standard of Correct Latin: Classical Fidelity with Licensed Registers").
narrative_ontology:topic_domain(classical_latin_standard__hybrid_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__hybrid_reading, '4704af0d-d2e4-4a2e-9b8c-cf2083c5eeca').
narrative_ontology:cs_kernel_codification('4704af0d-d2e4-4a2e-9b8c-cf2083c5eeca', distributed).
narrative_ontology:cs_authority_grounding('4704af0d-d2e4-4a2e-9b8c-cf2083c5eeca', expertise).
narrative_ontology:cs_interpretation_layer_present('4704af0d-d2e4-4a2e-9b8c-cf2083c5eeca').
narrative_ontology:cs_reading_relation('4704af0d-d2e4-4a2e-9b8c-cf2083c5eeca', classical_latin_standard__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('4704af0d-d2e4-4a2e-9b8c-cf2083c5eeca', classical_latin_standard__reconstruction_reading, coexists_with).
narrative_ontology:cs_axiom('4704af0d-d2e4-4a2e-9b8c-cf2083c5eeca', foundational, classical_corpus_remains_corrective_standard).
narrative_ontology:cs_axiom_status(classical_corpus_remains_corrective_standard, holdable).
narrative_ontology:cs_axiom_grounding('4704af0d-d2e4-4a2e-9b8c-cf2083c5eeca', classical_corpus_remains_corrective_standard, conventional).
narrative_ontology:cs_axiom('4704af0d-d2e4-4a2e-9b8c-cf2083c5eeca', foundational, licensed_register_developments_are_legitimate_latin).
narrative_ontology:cs_axiom_status(licensed_register_developments_are_legitimate_latin, holdable).
narrative_ontology:cs_axiom_grounding('4704af0d-d2e4-4a2e-9b8c-cf2083c5eeca', licensed_register_developments_are_legitimate_latin, instrumental).
narrative_ontology:cs_reference_frame('4704af0d-d2e4-4a2e-9b8c-cf2083c5eeca', classical_canon_with_licensed_registers).
narrative_ontology:cs_drift_state('4704af0d-d2e4-4a2e-9b8c-cf2083c5eeca', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4704af0d-d2e4-4a2e-9b8c-cf2083c5eeca', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__hybrid_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, roman_curia_liturgical_offices).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, university_classics_faculties).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, international_philological_academies).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, scientific_nomenclature_users).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, latin_language_students).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, living_latin_practitioners).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, late_antique_vulgar_text_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, roman_curia_liturgical_offices).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, classical_canon_exemplarity).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, selective_register_accommodation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Consortia of national academies and long-running dictionary and edition projects (Thesaurus Linguae Latinae, Mittellateinisches Woerterbuch, Novum Glossarium) through which the standard's judgments are recorded. They decide which post-Classical forms enter the authorized lexicon and which remain outside it. Their funding, staffing, and scholarly standing are bound to the continuation of this adjudicating work; stepping away would mean dissolving mandates built over generations.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, international_philological_academies, agenda_setter,
    institutional, generational, constrained, global).

% Draft encyclicals, liturgical texts, and diplomatic correspondence in Latin, drawing on a theological vocabulary accumulated across centuries. They invoke the authority of the Classical tradition to lend weight to their documents while relying on the settled right to keep technical terms no Classical author knew. Their drafting offices submit to correction by classically trained reviewers and must justify each innovation term by term.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, roman_curia_liturgical_offices, beneficiary,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, roman_curia_liturgical_offices, payer).

% Design curricula, set examinations, and certify competence in Classical authors and composition. Employment, enrollment, and departmental standing depend on the standard remaining the gateway credential for advanced textual work. They train the correctors whom other institutions employ and supply the editorial norms that scholarly presses apply.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, university_classics_faculties, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, university_classics_faculties, beneficiary).

% Taxonomists and nomenclature commissions in zoology, botany, and bacteriology compose species descriptions and names in Latinate form governed by codes that require Classical morphology. A frozen, dead-language baseline protects names from vernacular drift; commissions adjudicate formation questions by appeal to Classical grammar while coining thousands of new combinations no Roman ever formed.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, scientific_nomenclature_users, beneficiary,
    organized, generational, mobile, global).

% School and university learners who must reproduce Classical syntax and morphology under examination to advance. Their vernacular habits are marked as errors to be drilled out; hours of memorization and composition are levied before any independent reading or writing is credited. An individual student can drop the subject, but the credential paths that require it close behind them.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, latin_language_students, payer,
    powerless, biographical, constrained, national).

% Writers and teachers who compose fluent Latin as a working language, often with post-Classical efficiency devices and borrowed idioms. When they publish in academic or ecclesiastical venues their prose is reviewed against Classical norms and returned with corrections; conference papers and textbooks must pass the same filter. They maintain parallel venues where their usage goes unmarked, but institutional recognition still routes through the standard.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, living_latin_practitioners, payer,
    moderate, biographical, constrained, global).

% Philologists working on late-antique and early-medieval secular texts whose registers fall outside the licensed technical and ecclesiastical exemptions. Their sources carry the inherited label of decadence; grant panels and journal reviewers weigh their material against a Classical yardstick their texts never aimed to meet. Many migrate to Romance linguistics, where the same material reads as origins rather than decline.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, late_antique_vulgar_text_scholars, payer,
    moderate, biographical, mobile, continental).

% Teachers and scholars of French, Spanish, Italian, and the sister vernaculars that descend from the very drift the standard grades. They hold no seat in the academies or curricula that define correct Latin, though their disciplines hold the documentary evidence of how the graded forms actually developed. Their objection, that the standard brands their languages' ancestry as corruption, is registered nowhere in the standard-setting process.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, romance_language_educators, excluded,
    organized, generational, mobile, continental).

% Ordinary worshippers in congregations where the liturgy was celebrated in Latin they could not parse. Before the vernacular reforms of the 1960s they had no channel through which their incomprehension could reach the drafting offices; participation was mediated entirely by clergy formed inside the standard, and leaving the rite meant leaving the community.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, latin_rite_laity, excluded,
    powerless, biographical, trapped, global).

% Researchers in comparative and historical linguistics who study the standard itself as an artifact: how its boundary between development and barbarism moved, which interests each placement served, and how the licensed set expanded. They take testimony from every seat and owe allegiance to none of the institutions the standard sustains.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__hybrid_reading, university_classics_faculties).
narrative_ontology:fixing_cost_class(classical_latin_standard__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single shared measure of correct Latin so that texts composed, taught, edited, and certified across centuries and domains remain mutually intelligible and comparable; preserves scholarly access to the Classical corpus while letting theological, legal, and scientific registers keep their working vocabularies.
% TRANSFER_FUNCTION: Moves correction labor, examination compliance, and editorial conformity from learners, living writers, and editors of unlicensed material toward the certifying institutions; moves interpretive authority over which forms count as Latin to the philological profession, together with the employment and funding that follow it.
% ABSENT_VOICES: Romance-language educators hold the documentary record of the graded developments but hold no seat in the academies or curricula; Latin-rite laity bore the liturgy's opacity with no channel to the drafting offices. Both would contest the standard's grading of their linguistic inheritance. The standard-setting conversation runs among Classicists, clergy, and philologists alone.
% DISAPPEARANCE_RATIONALE: Credentialing in classics, editorial norms at scholarly presses, liturgical composition norms, and biological nomenclature codes all presuppose the hybrid measure. Overnight removal would force each institution to improvise its own correctness rule, fragmenting the mutual intelligibility of Latin texts and voiding a century of examination and lexicographic infrastructure.
% FOUNDING_PROBLEM: After the humanist demonstration that medieval usage had drifted from the Classical corpus, institutions that still needed Latin (the Church above all, then law, science, and scholarship) faced a choice between returning to archaeologically recovered Classical forms and keeping the working vocabularies their domains had accumulated. The hybrid standard was built to solve both at once: anchor correctness in Classical norms while exempting designated technical and ecclesiastical registers from the charge of barbarism.
% FOUNDING_PROBLEM_CORROBORATION: Living-Latin practitioners attest the problem from outside the beneficiary set: their own style guides concede the need for a shared correctness measure even while disputing the Classical anchor. Zoological and botanical nomenclature codes attest it functionally, citing stability of names as their stated aim. Purist critics corroborate negatively, agreeing a measure is needed and disputing only the accommodation half. No seat inside the standard's administration is the sole source of the genealogy.
narrative_ontology:disappearance_verdict(classical_latin_standard__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(classical_latin_standard__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__hybrid_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__hybrid_reading_tests).
:- end_tests(classical_latin_standard__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope on the structural facts: a genuine coordination function (one shared correctness measure preserving cross-domain intelligibility and corpus access), asymmetric burden (unlicensed registers bear correction while the philological complex collects authority and employment), and active enforcement (editorial review, examination gatekeeping, curial correction) — all three gates are authored. Metrics are authored independently as descriptive truth: epsilon 0.45 at interval end reflects partial delegitimization with accommodation, matching the reading's own assessment of the arrangement it administers. Suppression (0.35) is authored as a raw structural property and is deliberately NOT scaled by power or scope — only extractiveness is scaled in the engine's computation. Accessibility collapse is moderate (0.45) because alternatives remain visible and partly exercised: living-usage venues, purist reconstruction, and vernacular abandonment all persist. Resistance (0.50) is real and occasionally effective — coalition pressure from students, practitioners, and curriculum reformers abolished compulsory Latin in most school systems, which is the main driver of the falling suppression series. The three measurement series share one time grid (t = 0, 20, 40, 60, 80, 100) so every metric is authored at every examined point. The trajectories are monotonic on this coarse grid; a finer grid would show oscillation around the trends (humanist purist waves, mid-century revival attempts, the recent classical-education movement), but the oscillation is not itself the burden mechanism here — it is external demand variation, so no cyclical reinforcement claim is made. Theater ratio crossing 0.5 late in the interval is the notable drift signal: ceremonial and rhetorical Classicism (papal addresses, diplomas, mottoes, anniversary volumes) now outweighs functional composition, while the lexicographic machinery continues at shrinking readership.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary/agenda seats should compute differently from identical structural inputs. From the academies' and faculties' position the standard is stewardship they built and staff: the same review process that students experience as gatekeeping is, from the examiner's chair, the certification that makes their credential worth holding. The curia occupies a genuinely dual seat — it collects the prestige of Classical pedigree while submitting its own drafts to correction, so its computed directionality should sit well below the pure-payer seats but above the pure-beneficiary seats. Living-latin practitioners and vulgar-text scholars experience the identical norm as exclusion of their fluency and stigmatization of their material. Identity-lock dynamics bind two seats: practitioners fuse ideologically with Latin-as-a-living-language (capitulating to Classical review feels like betraying the revival), and the curia fuses institutionally with Latin as the Church's trans-temporal voice. If either frame broke, exits would widen sharply — practitioners to fully parallel venues (already partially realized), the curia to vernacular composition (partially realized after the 1960s reforms). The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low-directionality seats: the curia, faculties, academies, and nomenclature users are subsidized by the standard (pedigree, mandate, stable naming), with the nomenclature users' mobile exit damping their extraction exposure further. Victim declarations map to high-directionality seats: students (powerless, constrained exit) sit nearest the full-target end; practitioners (moderate, constrained by institutional recognition routes) next; vulgar-text scholars (moderate, mobile exit into Romance linguistics) carry elevated but damped exposure. No directionality overrides are declared: the beneficiary/victim declarations plus differentiated exit options already separate the seats, and the dual-positioned curia is handled through its secondary_role rather than an override keyed to a shared power atom. Receipt is distinguished from benefit: the curia benefits richly but the gains demonstrably accrue to the philological complex, so gain_flow names university_classics_faculties — the seat where credentialing authority, correction labor, and the employment that follows them land. Excluded seats (romance_language_educators, latin_rite_laity) feed the consensus-provenance picture, not classification: unanimity inside the standard-setting rooms is real but was purchased by keeping the descendants of the graded drift and the laity outside them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live: any institution still composing Latin (curial drafting, nomenclature rulings, neo-Latin publication) still faces the reconciliation the standard was built to perform, so founding_problem_status is live and the status-by-verdict consumer finds no mismatch with world_rearranges — no zombie flag. Mandatrophy is therefore not resolved, and no mandatrophy_resolved flag is authored. The rising theater ratio is the early-warning surface: if ceremonial Classicism continues displacing functional composition while enforcement capacity keeps decaying, the arrangement drifts toward inertial maintenance of a norm nobody is left to police — the piton signature — without any change in the founding narrative. The tangled_rope classification is what prevents mislabeling in both directions: a pure-extraction reading would erase the real coordination (without the shared measure, cross-domain Latin intelligibility and corpus access fragment), and a pure-coordination reading would erase the asymmetric burden (the exemption is granted by discretion to favored registers and withheld from disfavored ones, and the discretion belongs to the seats that collect the gains). Fixing cost is prohibitive for the seats that could fix it: replacing the standard would invalidate a century of lexicographic and examination infrastructure and require renegotiating ecclesiastical usage, a cost exceeding any benefit those seats would realize.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the hybrid reading of the classical_latin_standard kernel; how would the classification move under the sibling readings?',
    'Generate the sibling stories (classical_latin_standard__continuity_reading, classical_latin_standard__reconstruction_reading) and compare computed per-seat classifications and epsilon over the same standing arrangement.',
    'Under the continuity reading the victim set contracts toward nil (all transmitted drift is legitimate development) and epsilon falls toward coordination-cost levels, likely computing rope; under the reconstruction reading the victim set expands to all post-Classical usage, epsilon rises sharply, and the arrangement computes snare-flavored. The moderate profile authored here is a property of this reading, not of the underlying practice alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Reading-indexed classification over a shared kernel; sibling readings would recompute the same arrangement with different epsilon and victim sets.').

omega_variable(
    licensure_boundary_instability,
    'Where exactly does the standard''s line between licensed development and barbarism sit, and how much of the measured burden is generated by its current placement relative to neighboring placements?',
    'Code the authorized lexica (Thesaurus Linguae Latinae appendices, Novum Glossarium fascicles, nomenclature commission rulings) for admission dates and rejection records; correlate rejection intensity with register and period.',
    'Each boundary shift redistributes burden among the payer seats: admitting more late-antique secular forms relieves the vulgar-text scholars; tightening Classical review raises the load on students and living practitioners. A generously drawn boundary would push the arrangement toward rope; a boundary policed by gatekeeping interest would push it toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensure_boundary_instability, empirical, 'The licensed/unlicensed boundary is discretionary and movable; burden magnitude depends on its placement.').

omega_variable(
    enforcement_revival_possibility,
    'Is the century-long decline in enforcement capacity a terminal attrition, or a trough in a cycle that classical-education revivals and the living-Latin movement could reverse?',
    'Track examination requirements, curriculum mandates, and new institutional adoptions (classical charter schools, seminary reinstatements) over coming decades; compare enforcement staffing and correction intensity against the 1925 baseline.',
    'A sustained revival would ratchet suppression back up and restore the actively enforced profile; continued decay pushes the arrangement toward theatrical maintenance of a norm few are left to police.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_revival_possibility, empirical, 'Direction of the enforcement trajectory is open; the current series may be a trough rather than a terminus.').

omega_variable(
    accommodation_rent_question,
    'Is the correction burden imposed on payers the irreducible price of maintaining a shared standard, or surplus collected through gatekeeping beyond what the coordination function requires?',
    'Compare correction intensity and rejection rates across licensed and unlicensed registers holding error rates constant; audit whether examination content tests communicative competence or conformity to stylistic ideals no working register meets.',
    'If the burden tracks functional need, the measured figure is largely coordination cost and the rope component dominates; if it systematically exceeds need, the excess is gatekeeping rent and the snare component strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accommodation_rent_question, empirical, 'Separates coordination cost from gatekeeping surplus within the moderate extraction figure.').

omega_variable(
    stigma_internalization_share,
    'How much of the burden students and practitioners experience is structural (examinations, editorial rejection) versus internalized (shame at solecism, fear of marking oneself uneducated)?',
    'Post-exit trajectory studies: survey adults whose careers no longer require Latin for residual avoidance behavior and perceived incompetence; compare with cohorts never exposed to the standard.',
    'If a large share is internalized, effective suppression exceeds the structural measure and persists after institutional enforcement ends; the payer seats would classify harder than enforcement data alone indicates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stigma_internalization_share, empirical, 'Structural versus internalized components of the standard''s hold on its subjects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__hybrid_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cls_hybrid_reading_tr_t0, classical_latin_standard__hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(cls_hybrid_reading_tr_t0, observed).
narrative_ontology:measurement(cls_hybrid_reading_tr_t20, classical_latin_standard__hybrid_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(cls_hybrid_reading_tr_t20, observed).
narrative_ontology:measurement(cls_hybrid_reading_tr_t40, classical_latin_standard__hybrid_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(cls_hybrid_reading_tr_t40, observed).
narrative_ontology:measurement(cls_hybrid_reading_tr_t60, classical_latin_standard__hybrid_reading, theater_ratio, 60, 0.47).
narrative_ontology:measurement_basis(cls_hybrid_reading_tr_t60, observed).
narrative_ontology:measurement(cls_hybrid_reading_tr_t80, classical_latin_standard__hybrid_reading, theater_ratio, 80, 0.53).
narrative_ontology:measurement_basis(cls_hybrid_reading_tr_t80, observed).
narrative_ontology:measurement(cls_hybrid_reading_tr_t100, classical_latin_standard__hybrid_reading, theater_ratio, 100, 0.58).
narrative_ontology:measurement_basis(cls_hybrid_reading_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(cls_hybrid_reading_be_t0, classical_latin_standard__hybrid_reading, base_extractiveness, 0, 0.56).
narrative_ontology:measurement_basis(cls_hybrid_reading_be_t0, observed).
narrative_ontology:measurement(cls_hybrid_reading_be_t20, classical_latin_standard__hybrid_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(cls_hybrid_reading_be_t20, observed).
narrative_ontology:measurement(cls_hybrid_reading_be_t40, classical_latin_standard__hybrid_reading, base_extractiveness, 40, 0.49).
narrative_ontology:measurement_basis(cls_hybrid_reading_be_t40, observed).
narrative_ontology:measurement(cls_hybrid_reading_be_t60, classical_latin_standard__hybrid_reading, base_extractiveness, 60, 0.47).
narrative_ontology:measurement_basis(cls_hybrid_reading_be_t60, observed).
narrative_ontology:measurement(cls_hybrid_reading_be_t80, classical_latin_standard__hybrid_reading, base_extractiveness, 80, 0.46).
narrative_ontology:measurement_basis(cls_hybrid_reading_be_t80, observed).
narrative_ontology:measurement(cls_hybrid_reading_be_t100, classical_latin_standard__hybrid_reading, base_extractiveness, 100, 0.45).
narrative_ontology:measurement_basis(cls_hybrid_reading_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(cls_hybrid_reading_su_t0, classical_latin_standard__hybrid_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(cls_hybrid_reading_su_t0, observed).
narrative_ontology:measurement(cls_hybrid_reading_su_t20, classical_latin_standard__hybrid_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(cls_hybrid_reading_su_t20, observed).
narrative_ontology:measurement(cls_hybrid_reading_su_t40, classical_latin_standard__hybrid_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(cls_hybrid_reading_su_t40, observed).
narrative_ontology:measurement(cls_hybrid_reading_su_t60, classical_latin_standard__hybrid_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(cls_hybrid_reading_su_t60, observed).
narrative_ontology:measurement(cls_hybrid_reading_su_t80, classical_latin_standard__hybrid_reading, suppression_requirement, 80, 0.38).
narrative_ontology:measurement_basis(cls_hybrid_reading_su_t80, observed).
narrative_ontology:measurement(cls_hybrid_reading_su_t100, classical_latin_standard__hybrid_reading, suppression_requirement, 100, 0.35).
narrative_ontology:measurement_basis(cls_hybrid_reading_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__hybrid_reading, information_standard).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__reconstruction_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'correct Latin' conflates three structurally distinct claims about the authority source for correctness, each with its own stable epsilon over the same standing arrangement. The continuity reading authors low epsilon (transmitted drift is legitimate development, victim set near nil); this hybrid reading authors moderate epsilon (partial delegitimization confined to unlicensed registers, with accommodation); the reconstruction reading authors high epsilon (all post-Classical drift condemned, maximal victim set). Upstream/downstream: the reconstruction reading supplies the philological method this reading imports to adjudicate the licensed boundary; the continuity reading supplies the living material this reading selectively absorbs. Sibling stories are separate files linked through this network edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
