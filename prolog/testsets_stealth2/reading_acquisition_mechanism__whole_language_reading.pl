% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__whole_language_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: reading_acquisition_mechanism__whole_language_reading
 *   human_readable: Whole-Language Reading Acquisition Doctrine
 *   domain: educational psychology/literacy pedagogy/cognitive science
 *
 * SUMMARY:
 *   The whole-language doctrine holds that reading acquisition occurs through
 *   meaningful engagement with authentic texts and that decoding skill
 *   emerges implicitly from exposure, making systematic grapheme-phoneme
 *   instruction unnecessary and possibly harmful to comprehension
 *   development. Carried by education-school faculties, embedded in adopted
 *   curricula and leveled-text publishing, and enforced through certification
 *   coursework and curriculum-adoption gatekeeping, it governed a large share
 *   of Anglophone early-literacy instruction from the late 1980s onward. Its
 *   genuine coordination function is real: rich authentic-text engagement
 *   builds vocabulary, background knowledge, print motivation, and
 *   comprehension, and the doctrine corrected genuine sterility in
 *   mid-century basal instruction. Its extraction is equally real: the
 *   substantial minority of children who do not infer letter-sound patterns
 *   from exposure receive delayed or denied explicit instruction, accumulate
 *   remediation debt, and disproportionately flow into special education,
 *   while the costs of repair are pushed onto families and late-stage
 *   interventions. This file is ONE reading of the
 *   reading_acquisition_mechanism kernel; the phonics and balanced-literacy
 *   readings are separate constraints with their own epsilon, victim sets,
 *   and classifications, linked through the network block. KEY AGENTS (by
 *   structural relationship): - education_school_faculty: Agenda-setter
 *   (institutional/identity_locked) — trains teachers, certifies methods,
 *   controls the doctrinal pipeline - classroom_teachers: Primary beneficiary
 *   with diffuse payer residue (moderate/constrained) — collects autonomy and
 *   professional identity; bears retraining and blame when students fail -
 *   struggling_readers: Primary target (powerless/trapped) — children who
 *   need explicit decoding instruction and cannot choose their regime -
 *   families_of_struggling_readers: Secondary target (organized/constrained)
 *   — purchase private remediation the instructional system declined to
 *   provide - literacy_curriculum_publishers: Concentrated beneficiary
 *   (powerful/arbitrage) — sells the leveled texts, kits, and workshops the
 *   doctrine requires - reading_cognitive_scientists: Analytical observer
 *   (institutional/analytical) — produced the converging evidence; holds no
 *   seat in adoption or certification - parent_dyslexia_advocates: Excluded
 *   voice (organized/constrained) — organized outside the conversation that
 *   set their children's instruction
 *
 * KEY AGENTS:
 *   - education_school_faculty: agenda-setter; institutional power, identity_locked exit, generational horizon, national scope — the doctrinal pipeline
 *   - classroom_teachers: beneficiary (secondary payer); moderate power, constrained exit, biographical horizon, local scope — collects autonomy, absorbs failure fallout
 *   - struggling_readers: payer; powerless, trapped, biographical horizon, local scope — the disproportionate harm-bearers
 *   - families_of_struggling_readers: payer; organized, constrained exit, biographical horizon, regional scope — buy back the missing instruction
 *   - literacy_curriculum_publishers: beneficiary; powerful, arbitrage exit, generational horizon, national scope — concentrated commercial collector
 *   - reading_cognitive_scientists: observer; institutional, analytical exit, generational horizon, global scope — evidentiary seat outside the adoption process
 *   - parent_dyslexia_advocates: excluded; organized, constrained exit, biographical horizon, national scope — would object, kept outside curriculum decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.63).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.48).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole-Language Reading Acquisition Doctrine").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational psychology/literacy pedagogy/cognitive science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, '88f2ae07-1add-4aa0-a98d-28ba1beaa425').
narrative_ontology:cs_kernel_codification('88f2ae07-1add-4aa0-a98d-28ba1beaa425', distributed).
narrative_ontology:cs_authority_grounding('88f2ae07-1add-4aa0-a98d-28ba1beaa425', lineage).
narrative_ontology:cs_interpretation_layer_present('88f2ae07-1add-4aa0-a98d-28ba1beaa425').
narrative_ontology:cs_reading_relation('88f2ae07-1add-4aa0-a98d-28ba1beaa425', reading_acquisition_mechanism__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('88f2ae07-1add-4aa0-a98d-28ba1beaa425', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('88f2ae07-1add-4aa0-a98d-28ba1beaa425', foundational, decoding_emerges_from_meaningful_text_exposure).
narrative_ontology:cs_axiom_status(decoding_emerges_from_meaningful_text_exposure, holdable).
narrative_ontology:cs_axiom_grounding('88f2ae07-1add-4aa0-a98d-28ba1beaa425', decoding_emerges_from_meaningful_text_exposure, empirically_contingent).
narrative_ontology:cs_axiom('88f2ae07-1add-4aa0-a98d-28ba1beaa425', secondary, explicit_code_instruction_fragments_comprehension).
narrative_ontology:cs_axiom_status(explicit_code_instruction_fragments_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('88f2ae07-1add-4aa0-a98d-28ba1beaa425', explicit_code_instruction_fragments_comprehension, empirically_contingent).
narrative_ontology:cs_reference_frame('88f2ae07-1add-4aa0-a98d-28ba1beaa425', naturalistic_immersion_acquisition).
narrative_ontology:cs_drift_state('88f2ae07-1add-4aa0-a98d-28ba1beaa425', science_of_reading_legislative_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('88f2ae07-1add-4aa0-a98d-28ba1beaa425', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, classroom_teachers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, education_school_faculty).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, literacy_curriculum_publishers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, families_of_struggling_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Train the teachers, certify the methods, write the methods textbooks, and staff the committees that adopt elementary literacy curricula. Graduate programs, consulting income, and scholarly standing are built on constructivist literacy doctrine; faculty who publicly broke with it describe serious career costs. Leaving the doctrine would mean repudiating decades of their own scholarship, teaching, and professional relationships.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, education_school_faculty, agenda_setter,
    institutional, generational, identity_locked, national).

% Run daily literacy blocks under district-adopted programs aligned to the doctrine. They gain freedom from scripted sequences and commercial basals, plus a professional identity as facilitators of authentic reading rather than deliverers of drills. When students fail to decode, they lack systematic tools, absorb parental anger and administrative blame, and some pay for retraining out of pocket when they conclude their preparation failed them. Deviating from the adopted program invites evaluation problems; leaving the profession is the only full exit.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, classroom_teachers, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__whole_language_reading, classroom_teachers, payer).

% Children in K-3 classrooms who do not infer letter-sound patterns from exposure. They sit through meaning-first lessons, watch peers pull ahead visibly, are commonly identified as needing help years late, and are disproportionately referred to special education. They cannot choose, evaluate, or leave their instructional regime; their families speak for them or no one does.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Parents who discover their child cannot decode, then navigate private tutoring markets, educational evaluations, and school meetings, frequently paying hourly rates the household budget barely holds. Many conclude the school will not change its method and organize into advocacy networks instead; exiting via private school or homeschooling is possible only for households with resources to spare.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, families_of_struggling_readers, payer,
    organized, biographical, constrained, regional).

% Sell leveled-text libraries, guided-reading programs, running-record kits, and the professional-development workshops that accompany them. Revenue depends on continued adoption of doctrine-aligned materials. Major players monitor the mandate landscape and have already rebranded and retooled product lines as state requirements shift — their capital and market position survive any single doctrine's fall.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, literacy_curriculum_publishers, beneficiary,
    powerful, generational, arbitrage, national).

% Researchers in eye-tracking, longitudinal cohorts, and controlled intervention trials who produced the converging evidence on explicit phonics instruction. They publish, testify to legislatures, and advise reform commissions, but hold no seat in curriculum adoption, teacher certification, or district material selection — the forums where the arrangement is actually maintained.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, reading_cognitive_scientists, observer,
    institutional, generational, analytical, global).

% Organized networks of parents of dyslexic and struggling readers that lobby statehouses for universal screening and instructional mandates. They operate entirely outside the curriculum-adoption and certification conversations that determined their own children's instruction, and their leverage arrives only after the harm has accumulated — through legislation rather than through the rooms where the arrangement was set.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, parent_dyslexia_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__whole_language_reading, literacy_curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__whole_language_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes early literacy instruction around widely available children's literature and teacher judgment, solving a real provisioning problem: classrooms get engaging, meaningful text work without requiring systematic materials, scripted sequences, or per-child diagnostic instrumentation, and the profession gets a shared identity and low-cost implementation across wildly unequal classroom conditions.
% TRANSFER_FUNCTION: Moves instructional time and attention toward meaning-making activities (read-alouds, guided reading, writing workshop) and away from explicit code instruction; moves the cost of decoding failure out of the instructional budget and onto struggling readers' developmental timelines and their families' wallets, where it surfaces as tutoring fees, evaluations, and special-education placements.
% ABSENT_VOICES: Reading cognitive scientists held the decisive evidence but no seat in curriculum adoption or teacher certification; parents of struggling readers were outside the rooms where methods were chosen until they organized externally; and the struggling readers themselves — small children — cannot represent their interests in any forum. The arrangement's longevity owes much to the fact that its most affected constituency was structurally voiceless in exactly the institutions that administered it.
% DISAPPEARANCE_RATIONALE: If the doctrine and its enforcement vanished overnight, teacher-preparation syllabi, district adoptions, publishing lines, and classroom schedules would reorganize around explicit sequential instruction within a few years — as jurisdictions that mandated it have in fact demonstrated. Certification coursework, workshop industries, and professional identities built on the doctrine would lose their object; the population of late-identified struggling readers would shrink, moving costs out of remediation markets and special-education rolls.
% FOUNDING_PROBLEM: Mid-twentieth-century basal instruction was decontextualized and motivation-killing: repetitive contrived readers, rote skill drills, comprehension treated as question-answering. Whole language was built to restore authentic literature, meaning, and learner motivation to the center of literacy teaching, and secondarily to defend professional teacher judgment against scripted commercial programs.
% FOUNDING_PROBLEM_CORROBORATION: Literacy historians outside the movement corroborate the founding complaint about basal-reader sterility, and independent trend analyses of reading comprehension and motivation confirm those concerns persist regardless of method — the motivational half of the founding problem is live. But the mechanism premise (decoding emerges implicitly) is attested as substantially superseded by sources with no stake in the arrangement: the National Reading Panel findings, converging cognitive-science reviews of word recognition, and the legislative findings accompanying dozens of state mandates. No corroborating source outside the beneficiary set maintains that implicit emergence suffices for most learners in English.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.63: the arrangement transfers instructional time away from the children who most need explicit code instruction, and the resulting remediation debt (private tutoring, late special-education identification, adult literacy failure) is large, durable, and regressive — but it is not total, because a majority of learners acquire serviceable decoding under exposure-rich conditions and all learners receive the genuine comprehension and motivation benefits of authentic-text work. Suppression is 0.48 and falling: at its peak the arrangement suppressed alternatives through certification requirements, adoption committees, and professional ostracism of phonics-oriented teachers; state mandate waves since 2019 have reversed the enforcement gradient, and the residual suppression is concentrated in teacher-preparation institutions. Theater is 0.54 and rising: three-cueing routines are performed as skilled-reading strategy while functioning as taught guessing, and post-2019 relabelings preserve doctrine under new names; the authentic-literacy core remains real activity, so theater stays below the piton threshold. Accessibility collapse is low (0.22) because the alternative — explicit systematic phonics — is fully specified, commercially available, and increasingly legally mandated; nothing about understanding this constraint closes off exits. Resistance is high (0.72): the science-of-reading movement, investigative journalism, dyslexia advocacy networks, and dozens of state statutes constitute organized, sustained, partially successful opposition. The temporal series run on one shared six-point grid (all three metrics authored at every point); suppression_requirement traces a ratchet-and-decay arc — enforcement hardened through the 1995-2005 dominance era, then decayed as counter-enforcement (mandates, curriculum rejections) grew — while extractiveness accumulates monotonically as remediation debt matures even where practice has begun to change.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute opposite types from identical structural data. From the education-faculty and publisher seats the arrangement is a professional liberation and a market: no scripts, professional judgment honored, coherent product lines, a self-reproducing certification pipeline — a coordination structure they built and maintain. From the struggling-reader seat the same structure operates as delayed injury: years of meaning-first instruction without the decoding tools they specifically need, followed by remediation priced to their families. Classroom teachers straddle the gap, which is why the story carries a directionality override for their seat. The engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Struggling_readers (payer, trapped, powerless) derive near the full-target end — they bear the transfer and cannot exit. Families_of_struggling_readers (payer, constrained) derive high-target: they pay twice, in taxes for the ineffective arrangement and out of pocket for its repair. Education_school_faculty (agenda_setter, identity_locked) and literacy_curriculum_publishers (beneficiary, arbitrage) derive near the beneficiary end — the former run the enforcement machinery and are fused with the doctrine professionally, the latter collect concentrated revenue and can pivot product lines at will. One override is declared: classroom_teachers would derive near-beneficiary (roughly d=0.15-0.2) from their beneficiary role alone, but their true position sits nearer symmetric (d=0.35) because the autonomy they collect is partially offset by costs the same arrangement imposes on them — blame for student failure, out-of-pocket retraining when they break with their training, and the moral injury of watching preventable struggle. The override corrects the derivation for this dual-positioned seat; no other seat needs correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Calling this a snare would erase the genuine coordination function — authentic-text engagement demonstrably builds vocabulary, knowledge, and motivation, and the doctrine's founding corrective (against sterile basal drills) addressed a real defect; a pure extraction story cannot explain why capable readers thrived under it. Calling it a rope would erase the identifiable, disproportionate victims and the enforcement machinery needed to keep explicit instruction out of classrooms that wanted it. The tangled-rope classification holds both facts in one structure. Mandatrophy status: the founding problem (decontextualized, motivation-killing literacy instruction) is partly live — comprehension and engagement concerns persist independently — but the specific mechanism claim (implicit decoding emergence) is substantially overridden by converging evidence, hence founding_problem_status 'contested'. The rising theater ratio and the relabeling pattern (whole language becoming balanced literacy becoming 'structured literacy with rich texts') signal drift risk: if mandates strip the arrangement's function while its rhetoric persists in preparation syllabi, the residue becomes piton-flavored performance inside a shrinking territory — the persistence-mechanism omega is designed to detect exactly that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is the whole_language_reading instantiation of the reading_acquisition_mechanism kernel; what would the sibling readings (phonics_reading, balanced_literacy_reading) change structurally if they governed the same classrooms?',
    'Comparative classification across the three sibling story files: locate where the victim set, enforcement surface, and epsilon shift between readings.',
    'Under the phonics reading the victim set contracts (fewer struggling readers) but a new payer seat appears (teachers bound to scripted sequences, publishers of basals); under balanced literacy extraction redistributes rather than disappears. Cross-reading comparison, not within-story metrics, adjudicates which reading minimizes total remediation burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one of three rival readings of the reading-acquisition kernel; sibling readings instantiate different constraints with different victim sets.').

omega_variable(
    implicit_self_teaching_subsets,
    'Does decoding ever genuinely emerge implicitly from print exposure, and for which subsets of children and orthographies?',
    'Longitudinal studies of print-exposed children stratified by home literacy environment and orthographic depth; cross-linguistic comparison with transparent alphabets where Share-style self-teaching is better supported.',
    'If a substantial subset reliably self-teaches, part of the arrangement''s operation is genuine coordination matched to those learners and measured extraction overstates harm for them; if self-teaching is rare in English-classroom conditions, the implicit-emergence premise fails for most learners and extraction approaches the full-target end for nearly all non-advantaged readers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_self_teaching_subsets, empirical, 'Whether the doctrine rests on a real developmental regularity for some population or is a generalization from an unrepresentative minority.').

omega_variable(
    autonomy_benefit_vs_workload_cover,
    'Is the teacher autonomy the arrangement preserves a professional good that improves instruction for typical readers, or primarily a reduction in preparation and evaluation burden that benefits teachers at students'' expense?',
    'Classroom-level comparison of outcomes under teacher-directed authentic-literacy practice versus structured programs holding teacher quality constant; survey of teacher time-allocation under each regime.',
    'If autonomy is functionally productive, the beneficiary declaration reflects real coordination value and the tangled-rope reading holds with lower excess extraction; if it is chiefly workload relief, the teacher seat''s benefit is cover and the arrangement slides toward the snare end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_benefit_vs_workload_cover, preference, 'Whether the principal beneficiary-side gain is a genuine instructional good or rent in kind.').

omega_variable(
    identity_lock_persistence_mechanism,
    'Why does the doctrine persist in teacher preparation after adverse evidence: identity fusion of faculty and trained teachers with constructivist commitments, or ordinary institutional economics of adopted materials and course catalogs?',
    'Track syllabus and program-adoption change in education schools after state phonics mandates; compare jurisdictions where mandates bind certification versus where they bind only district adoption.',
    'If identity lock dominates, decay will be slow even under legal mandate and the arrangement drifts toward theatrical maintenance (piton-flavored persistence inside shrinking territory); if economic, publisher and catalog pivots accelerate decay and the arrangement dissolves as a live constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence_mechanism, empirical, 'Persistence mechanism: internalized professional identity versus institutional economics.').

omega_variable(
    remediation_cost_attribution,
    'Are the long-term remediation costs attributed to this arrangement caused by implicit-only instruction specifically, or confounded by socioeconomic status, home print exposure, and baseline student factors?',
    'Natural experiments from state-level phonics mandates and screening laws (differential timing across states), comparing remediation referrals and special-education identification before and after instructional-regime change within demographic strata.',
    'Clean attribution raises the measured extraction attributable to this constraint and strengthens the case that the victim declaration identifies the arrangement itself as cause; heavy confounding would shift part of the burden to background conditions and reduce the arrangement''s attributable epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_cost_attribution, empirical, 'Causal attribution of the disproportionate harm to struggling readers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1975, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(read_tr_t1985, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(read_tr_t1995, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(read_tr_t2005, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2005, 0.46).
narrative_ontology:measurement(read_tr_t2015, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2015, 0.5).
narrative_ontology:measurement(read_tr_t2025, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2025, 0.54).

% Extraction over time
narrative_ontology:measurement(read_be_t1975, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1975, 0.35).
narrative_ontology:measurement(read_be_t1985, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1985, 0.44).
narrative_ontology:measurement(read_be_t1995, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1995, 0.56).
narrative_ontology:measurement(read_be_t2005, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(read_be_t2015, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement(read_be_t2025, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2025, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1975, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1975, 0.3).
narrative_ontology:measurement(read_su_t1985, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1985, 0.42).
narrative_ontology:measurement(read_su_t1995, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(read_su_t2005, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement(read_su_t2015, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(read_su_t2025, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'how reading is taught' covers three structurally distinct claims that measure differently and must not share one story. The phonics reading (upstream, higher empirical confidence, converging cognitive-science support) carries low epsilon and a coordination-heavy profile; the whole-language reading (this file) carries substantial extraction concentrated on struggling readers; the balanced-literacy reading is intermediate and genealogically downstream of both — it absorbed the whole-language arrangement under accountability pressure while retaining much of its apparatus (leveled texts, cueing routines, workshop structures), which is why this file declares an influences edge toward it. The upstream phonics evidence base is routinely cited as grounds for rejecting this reading's mechanism claim; each member of the family links to the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__whole_language_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
