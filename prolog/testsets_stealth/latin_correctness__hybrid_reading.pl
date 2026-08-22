% ============================================================================
% CONSTRAINT STORY: latin_correctness__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__hybrid_reading, []).

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
 *   constraint_id: latin_correctness__hybrid_reading
 *   human_readable: Bifurcated Latinity Standard (Hybrid Reading): Classical Norms for Literary Domains, Licensed Post-Classical Forms for Technical Domains
 *   domain: historical_linguistics/intellectual_history
 *
 * SUMMARY:
 *   Across Latin Christendom c. 1150-1440, the learned world operated a
 *   bifurcated correctness standard: composition aspiring to literary or
 *   rhetorical standing was measured against recovered ancient models, while
 *   the technical disciplines — logic, law, medicine, administration — were
 *   permitted the post-classical forms their subject matter required. This
 *   story instantiates the hybrid reading of the latin_correctness kernel as
 *   a clean, epsilon-invariant constraint: one stable referent (the standing
 *   bifurcated arrangement, assessed by this reading's own lights), one
 *   beneficiary/victim structure, one type. The sibling readings (continuity,
 *   rupture) are separate constraints linked through
 *   network.affects_constraints; their differing epsilon values are
 *   documented in their own files. Claim/metric independence is preserved:
 *   claimed_type tangled_rope states the structure I believe true (genuine
 *   register coordination plus asymmetric status transfer, actively
 *   enforced), while the metric values are independent descriptive estimates
 *   of how the arrangement actually operated.
 *
 * KEY AGENTS:
 *   - - grammar_curriculum_masters: Agenda-setting administrator (institutional/constrained) — teaches both halves of the standard and decides which forms pass
 *   - - classically_trained_literati: Primary beneficiary (organized/identity_locked) — collects the prestige differential the literary half reserves
 *   - - scholastic_technical_authors: Primary target (organized/constrained) — bears the status cost of the technical half
 *   - - jurists_and_physicians: Secondary target (organized/constrained) — professional style discount without literary recourse
 *   - - monastic_chroniclers: Peripheral target (powerless/identity_locked) — cited as evidence of decay, unable to adopt the new standard
 *   - - chancery_dictamen_secretaries: Dual-positioned actor (moderate/constrained) — licensed in daily practice, taxed in advancement
 *   - - patrons_and_university_founders: Consuming beneficiary (powerful/mobile) — collects the prestige goods the standard produces
 *   - - humanist_classicizing_reformers: Excluded challenger (moderate/mobile) — builds channels outside the administering consensus
 *   - - modern_philologists: Analytical observer — sees the full diglossic structure retrospectively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__hybrid_reading, 0.62).
domain_priors:suppression_score(latin_correctness__hybrid_reading, 0.66).
domain_priors:theater_ratio(latin_correctness__hybrid_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__hybrid_reading, "Bifurcated Latinity Standard (Hybrid Reading): Classical Norms for Literary Domains, Licensed Post-Classical Forms for Technical Domains").
narrative_ontology:topic_domain(latin_correctness__hybrid_reading, "historical_linguistics/intellectual_history").

domain_priors:requires_active_enforcement(latin_correctness__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__hybrid_reading, 'b612b06c-cf4e-41f0-a1ce-9ef8a0c4769c').
narrative_ontology:cs_kernel_codification('b612b06c-cf4e-41f0-a1ce-9ef8a0c4769c', formalized).
narrative_ontology:cs_authority_grounding('b612b06c-cf4e-41f0-a1ce-9ef8a0c4769c', lineage).
narrative_ontology:cs_interpretation_layer_present('b612b06c-cf4e-41f0-a1ce-9ef8a0c4769c').
narrative_ontology:cs_reading_relation('b612b06c-cf4e-41f0-a1ce-9ef8a0c4769c', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b612b06c-cf4e-41f0-a1ce-9ef8a0c4769c', latin_correctness__rupture_reading, forecloses).
narrative_ontology:cs_axiom('b612b06c-cf4e-41f0-a1ce-9ef8a0c4769c', foundational, domain_indexed_latinity_legitimacy).
narrative_ontology:cs_axiom_status(domain_indexed_latinity_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b612b06c-cf4e-41f0-a1ce-9ef8a0c4769c', domain_indexed_latinity_legitimacy, conventional).
narrative_ontology:cs_axiom('b612b06c-cf4e-41f0-a1ce-9ef8a0c4769c', foundational, subject_matter_licenses_medieval_forms).
narrative_ontology:cs_axiom_status(subject_matter_licenses_medieval_forms, holdable).
narrative_ontology:cs_axiom_grounding('b612b06c-cf4e-41f0-a1ce-9ef8a0c4769c', subject_matter_licenses_medieval_forms, instrumental).
narrative_ontology:cs_reference_frame('b612b06c-cf4e-41f0-a1ce-9ef8a0c4769c', bifurcated_register_legitimacy).
narrative_ontology:cs_drift_state('b612b06c-cf4e-41f0-a1ce-9ef8a0c4769c', early_humanist_ascendancy, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b612b06c-cf4e-41f0-a1ce-9ef8a0c4769c', '').
narrative_ontology:cs_kernel_id(latin_correctness__hybrid_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, classically_trained_literati).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, grammar_curriculum_masters).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, chancery_dictamen_secretaries).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, patrons_and_university_founders).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, scholastic_technical_authors).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, jurists_and_physicians).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, monastic_chroniclers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, chancery_dictamen_secretaries).
narrative_ontology:constraint_vindicates(latin_correctness__hybrid_reading, register_hierarchy_doctrine).
narrative_ontology:constraint_vindicates(latin_correctness__hybrid_reading, imitatio_pedagogy).
narrative_ontology:constraint_vindicates(latin_correctness__hybrid_reading, subject_matter_license_for_neologism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach the arts curriculum in cathedral and university schools: elementary grammar from Donatus and Priscian, composition exercises imitating ancient authors for advanced pupils, and the technical vocabularies the higher faculties require. They administer the standard day to day — correcting compositions, examining candidates, deciding which forms pass — and collect fees and institutional standing from a system that needs both halves of the bifurcation taught. Their own published prose must satisfy the literary side of the rule, so they carry training costs as well as authority.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, grammar_curriculum_masters, agenda_setter,
    institutional, generational, constrained, continental).

% Poets, letter-writers, and court intellectuals whose reputations rest on demonstrated command of ancient style. The bifurcated rule reserves the highest prestige for their register, so appointments, dedications, and patronage flow toward them. Leaving the standard would mean renouncing the cultivated identity their careers and self-conception are built on; they defend the literary half of the rule intensely and police its boundary in both directions.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, classically_trained_literati, beneficiary,
    organized, biographical, identity_locked, continental).

% Write the administrative correspondence of princes, cities, and prelates. The rule licenses their working register — practical documents need not imitate Cicero — which keeps daily output feasible at volume. Advancement nonetheless tracks classical polish, so they invest scarce time and money in stylistic training that the recipients of their documents rarely notice.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, chancery_dictamen_secretaries, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, chancery_dictamen_secretaries, payer).

% University masters in philosophy, logic, and theology who must coin and use terms the ancient canon lacks — quidditas, essentia, the apparatus of the translated Aristotle. Their arguments proceed in a technical register the rule permits, yet their works are marked as stylistically inferior by the literary half of the standard, and they face recurring pressure to classicize prefaces and dedications they cannot sustain through the body of the text. Exit would mean abandoning the international scholarly conversation, which has no vernacular substitute in this period.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, scholastic_technical_authors, payer,
    organized, generational, constrained, continental).

% Practice law and medicine in dense professional Latin inherited from the Justinianic compilers and Galenic translations. Their vocabularies resist classicization, their training includes little ancient literature, and their standing among lettered elites suffers accordingly. They lack the leisure and schooling that the literary half of the rule presupposes, and their professional corporations offer no route around the style discount.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, jurists_and_physicians, payer,
    organized, biographical, constrained, continental).

% Continue house annals and saints' lives in the unclassicized Latin of their communities' own traditions. As classical schooling spreads outward from the schools, their prose is increasingly cited as evidence of general decay. Their vocation binds them to the cloister and its usages; adopting the new standard would mean importing outside teachers and quarreling with their own houses' customs, which obedience discourages.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, monastic_chroniclers, payer,
    powerless, generational, identity_locked, regional).

% Princes, prelates, and civic oligarchs who fund schools, commission libraries, and request dedications. The bifurcated rule supplies the prestige goods they consume — elegant letters, polished histories, ceremonious public disputations — and lets them signal cultivation by demanding classical finish without disturbing the technical legal and medical work their administrations depend on. They can shift patronage between rival centers at will.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, patrons_and_university_founders, beneficiary,
    powerful, generational, mobile, continental).

% Teachers and textual scholars, growing in number toward the interval's end, who hold that ancient texts should govern all Latin and who build private schools and patronage networks outside the university arts faculties. They stand outside the curricular consensus that administers the bifurcated rule, and their objection — that conceding any domain to post-classical usage entrenches corruption — goes largely unaddressed by the standard's administrators until the period's close.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, humanist_classicizing_reformers, excluded,
    moderate, generational, mobile, continental).

% Retrospective analysts of medieval Latinity. Working from surviving corpora, they document the register stratification the rule produced, measure where technical innovation actually required post-classical forms, and assess how much of the pressure on technical writers served communication versus status. They hold no position inside the arrangement.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, modern_philologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__hybrid_reading, classically_trained_literati).
narrative_ontology:fixing_cost_class(latin_correctness__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains two mutually non-interfering registers for a single learned language across a continent: a classical-target register for composition meant to display eloquence, giving geographically scattered elites a shared aesthetic benchmark and keeping high-style Latin mutually intelligible; and a licensed technical register that lets philosophy, law, medicine, and administration coin the terms their subjects require without waiting on ancient precedent.
% TRANSFER_FUNCTION: Moves prestige, appointments, and patronage toward writers with classical training and toward the masters who certify it; moves recognition costs onto technical writers, whose competence is discounted by style regardless of content; moves curriculum time and fees toward grammatical training.
% ABSENT_VOICES: Technical writers sat in the room but not at the controls: the criteria were administered by grammar-rhetoric masters and enforced through patronage preference, so those bearing the heaviest style penalties had little vote on the standard. Vernacular writers, women (largely barred from Latin schooling), and scholars working from Greek and Arabic transmissions were outside the conversation entirely; each would have objected that the bifurcation entrenched a closed clerical-literary monopoly on learned legitimacy.
% DISAPPEARANCE_RATIONALE: If the bifurcated standard vanished overnight, the learned world would snap to one pole or the other: a universal classical regime would stall technical writing (coined terminology ruled out) and strand every working jurist and physician, while a universal post-classical regime would dissolve the literary prestige economy that structures patronage, appointment, and elite self-display. Curricula, chancery practice, and careers reorganize either way.
% FOUNDING_PROBLEM: The eleventh- and twelfth-century expansion of Latin literacy outran the ancient canon: logic, law, and medicine needed vocabulary and syntax the recovered classics did not contain, while literary ambition still measured itself against those same classics. The bifurcation was built so both could proceed — technical work without stylistic paralysis, literary work without losing its ancient benchmark.
% FOUNDING_PROBLEM_CORROBORATION: Scholastic masters — who lose standing under the arrangement — corroborate the technical half: their prefaces repeatedly plead the necessity of new terms for new questions. Chancery archives corroborate the practical half: administrative volume that classical-only composition could not have sustained. The literary half is attested by the persistent market for classical manuscripts and imitatio exercises. At the interval's end, humanist critics dispute the arrangement's necessity while confirming the underlying pressure's reality; no party claims the founding problem had vanished.
narrative_ontology:disappearance_verdict(latin_correctness__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(latin_correctness__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__hybrid_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the status differential is real and career-consequent, but the technical half genuinely licenses most scholarly production, bounding the transfer. Suppression 0.66: enforcement runs through curriculum control, examination, and patronage preference rather than violence, and it intensified as classical schooling expanded — hence the tracked suppression_requirement series, since this story's dynamic is enforcement-capacity growth, not merely shifting extraction. Theater 0.30: classicizing prefaces, dedications, and ceremonial orations perform an adherence the body text often lacks, and that performative share grows with fashion. Accessibility_collapse 0.45: the technical half leaves a large legitimate alternative open, so alternatives never fully collapse. Resistance 0.50: scholastic masters defended their usage, mocked purist excess, and their organized faculties blunted enforcement. All three series share one time grid (seven points, 1150-1440); trajectories are monotonic with no cyclical dynamic. Coordination-type note: identity_coordination carries a known cover-story risk, but the coupling here is genuine boundary maintenance — membership claims (properly trained litteratus vs. untrained technician) adjudicated against evolving criteria (which auctores count, which coinages pass) — not identity framing draped over simple extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the grammar masters' and literati's seats the arrangement is the learned world's working settlement: both registers served, excellence rewarded, the continent's high style mutually legible. From the scholastic and professional seats the same structure operates as a status tax — unlimited license for a register whose rewards they cannot collect, and a prestige ceiling set by training they cannot afford. Organized university power dampens the divergence for scholastics relative to isolated monastic chroniclers, whose identity-locked position computes harshest. The engine computes per-seat types from the structural data; the divergence between seats is the finding, not an inconsistency to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (literati, grammar masters, chancery secretaries, patrons) drive d toward the subsidized end; victim declarations (scholastic authors, jurists/physicians, monastic chroniclers) drive d toward the target end. Exit modulation matters: identity_locked literati and chroniclers sit nearer their respective poles than constrained counterparts, and the patrons' mobility pushes them furthest toward the beneficiary end. Spatial scope is continental — verifying correctness across Latin Christendom is costly, which scales effective extraction modestly upward for targets. Suppression is authored as a raw structural property and is deliberately left unscaled; only extractiveness rides directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two symmetric mislabels. Reading the arrangement as pure coordination would erase the status transfer technical writers pay; reading it as pure extraction would erase the genuine service — the technical license kept law, medicine, and philosophy writable, and the literary target kept a trans-European learned culture legible to itself. No mandatrophy resolution is declared: during the interval the founding problem stays live, enforcement is active rather than inertial, and the function is real rather than performed. The rising suppression series marks where the balance begins tipping toward the rupture reading's successor arrangement after the interval closes — a transition this story bounds but does not model.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the hybrid reading of the latin_correctness kernel; how would the computed classification shift if the same standing arrangement were assessed under the continuity or rupture reading?',
    'Compile the sibling stories (latin_correctness__continuity_reading, latin_correctness__rupture_reading) and compare per-seat classifications across the three files; the divergences locate what each reading changes structurally.',
    'Under the continuity reading the victim set largely dissolves (no unattainable external standard) and epsilon falls toward coordination cost; under the rupture reading the standard extends to all domains, the victim set expands to nearly all working writers, and suppression rises sharply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification variance across the latin_correctness kernel.').

omega_variable(
    bifurcation_boundary_stability,
    'Where does the literary/technical boundary actually fall, and is it stable across genres — sermons, historiography, saints'' lives, and verse paraphrase sit astride it?',
    'Genre-level corpus analysis of formal compliance: classify surviving works by register markers and test whether boundary genres receive inconsistent treatment across scriptoria, faculties, and chanceries.',
    'If the boundary is unstable, the pressured population is larger than the declared victim set — costs concentrate on genres near the line, and the bifurcation operates as discretionary gatekeeping rather than a rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bifurcation_boundary_stability, conceptual, 'Stability of the domain boundary on which the entire bifurcated standard turns.').

omega_variable(
    status_gradient_vs_communication_need,
    'Is the pressure on technical writers to classicize driven by genuine audience expectations for intelligibility and authority, or by pure status competition among the lettered?',
    'Compare reception of classicized versus unclassicized technical works: citation rates, manuscript survival, and explicit reader complaints, controlling for content quality.',
    'If reception gains are negligible, the classicizing pressure is rent-seeking layered onto a functional standard and the extraction component is larger than authored; if real, part of the measured pressure is coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_gradient_vs_communication_need, empirical, 'Whether classicizing pressure serves communication or status.').

omega_variable(
    hybrid_rupture_straddle_coherence,
    'Historical actors (dictatores turned proto-humanists, such as chancery heads who practiced mixed Latin while professing ancient ideals) appear to hold hybrid practice alongside rupture-leaning commitments; does this show the two readings'' premises are combinable after all?',
    'Distinguish operative commitment from professed ideal in the sources: determine whether such actors treat post-classical forms as legitimate in technical domains or merely tolerated pending correction.',
    'If straddling is coherent, the foreclosure edge declared toward the rupture reading is overstated and should soften to influences; if straddlers merely tolerate what they condemn, the foreclosure edge stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_rupture_straddle_coherence, conceptual, 'Whether hybrid and rupture premises can cohere in one framework, testing the declared foreclosure edge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__hybrid_reading, 1150, 1440).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t1150, latin_correctness__hybrid_reading, theater_ratio, 1150, 0.18).
narrative_ontology:measurement_basis(lati_tr_t1150, observed).
narrative_ontology:measurement(lati_tr_t1200, latin_correctness__hybrid_reading, theater_ratio, 1200, 0.2).
narrative_ontology:measurement_basis(lati_tr_t1200, observed).
narrative_ontology:measurement(lati_tr_t1250, latin_correctness__hybrid_reading, theater_ratio, 1250, 0.22).
narrative_ontology:measurement_basis(lati_tr_t1250, observed).
narrative_ontology:measurement(lati_tr_t1300, latin_correctness__hybrid_reading, theater_ratio, 1300, 0.24).
narrative_ontology:measurement_basis(lati_tr_t1300, observed).
narrative_ontology:measurement(lati_tr_t1350, latin_correctness__hybrid_reading, theater_ratio, 1350, 0.26).
narrative_ontology:measurement_basis(lati_tr_t1350, observed).
narrative_ontology:measurement(lati_tr_t1400, latin_correctness__hybrid_reading, theater_ratio, 1400, 0.28).
narrative_ontology:measurement_basis(lati_tr_t1400, observed).
narrative_ontology:measurement(lati_tr_t1440, latin_correctness__hybrid_reading, theater_ratio, 1440, 0.3).
narrative_ontology:measurement_basis(lati_tr_t1440, observed).

% Extraction over time
narrative_ontology:measurement(lati_be_t1150, latin_correctness__hybrid_reading, base_extractiveness, 1150, 0.34).
narrative_ontology:measurement_basis(lati_be_t1150, observed).
narrative_ontology:measurement(lati_be_t1200, latin_correctness__hybrid_reading, base_extractiveness, 1200, 0.39).
narrative_ontology:measurement_basis(lati_be_t1200, observed).
narrative_ontology:measurement(lati_be_t1250, latin_correctness__hybrid_reading, base_extractiveness, 1250, 0.44).
narrative_ontology:measurement_basis(lati_be_t1250, observed).
narrative_ontology:measurement(lati_be_t1300, latin_correctness__hybrid_reading, base_extractiveness, 1300, 0.5).
narrative_ontology:measurement_basis(lati_be_t1300, observed).
narrative_ontology:measurement(lati_be_t1350, latin_correctness__hybrid_reading, base_extractiveness, 1350, 0.54).
narrative_ontology:measurement_basis(lati_be_t1350, observed).
narrative_ontology:measurement(lati_be_t1400, latin_correctness__hybrid_reading, base_extractiveness, 1400, 0.59).
narrative_ontology:measurement_basis(lati_be_t1400, observed).
narrative_ontology:measurement(lati_be_t1440, latin_correctness__hybrid_reading, base_extractiveness, 1440, 0.62).
narrative_ontology:measurement_basis(lati_be_t1440, observed).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t1150, latin_correctness__hybrid_reading, suppression_requirement, 1150, 0.25).
narrative_ontology:measurement_basis(lati_su_t1150, observed).
narrative_ontology:measurement(lati_su_t1200, latin_correctness__hybrid_reading, suppression_requirement, 1200, 0.32).
narrative_ontology:measurement_basis(lati_su_t1200, observed).
narrative_ontology:measurement(lati_su_t1250, latin_correctness__hybrid_reading, suppression_requirement, 1250, 0.38).
narrative_ontology:measurement_basis(lati_su_t1250, observed).
narrative_ontology:measurement(lati_su_t1300, latin_correctness__hybrid_reading, suppression_requirement, 1300, 0.44).
narrative_ontology:measurement_basis(lati_su_t1300, observed).
narrative_ontology:measurement(lati_su_t1350, latin_correctness__hybrid_reading, suppression_requirement, 1350, 0.52).
narrative_ontology:measurement_basis(lati_su_t1350, observed).
narrative_ontology:measurement(lati_su_t1400, latin_correctness__hybrid_reading, suppression_requirement, 1400, 0.6).
narrative_ontology:measurement_basis(lati_su_t1400, observed).
narrative_ontology:measurement(lati_su_t1440, latin_correctness__hybrid_reading, suppression_requirement, 1440, 0.66).
narrative_ontology:measurement_basis(lati_su_t1440, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__rupture_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' decomposes into three structurally distinct constraints, one per declared reading of the kernel. The hybrid reading sits upstream of the rupture reading historically: the bifurcated standard's prestige gradient created the career incentives and the classicizing training pipeline that humanist reformers later radicalized into the uniform-standard position, while its permanent concession of the technical domain to post-classical usage is what the continuity reading generalizes. Each file carries its own epsilon, beneficiaries, and victims; the edges here express family kinship and influence, not identity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
