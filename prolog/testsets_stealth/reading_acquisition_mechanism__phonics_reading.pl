% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__phonics_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Systematic Phonics Foundational Instruction Requirement
 *   domain: educational/cognitive-science/pedagogy
 *
 * SUMMARY:
 *   Across many jurisdictions, statute and curriculum policy now require that
 *   beginning reading instruction include explicit, systematic instruction in
 *   grapheme-phoneme correspondence as a foundational skill — implemented
 *   through defined scope-and-sequence documents, universal screening,
 *   progress-monitoring schedules, approved instructional-materials lists,
 *   and teacher-preparation requirements. The arrangement solves a real
 *   coordination problem (workforce-wide instructional reliability for a
 *   skill most adults assume is automatic) while simultaneously channeling
 *   procurement toward a narrowed vendor set and transferring pedagogical
 *   discretion from experienced teachers to standardized programs. KEY AGENTS
 *   (by structural relationship): - state_education_agencies: agenda setter
 *   (institutional/constrained) — writes and enforces the requirement -
 *   curriculum_publishers: primary commercial beneficiary
 *   (powerful/arbitrage) — receives mandated program spending -
 *   classroom_teachers: primary payer (organized/constrained) — bears
 *   discretion and labor costs, with offsetting gains for novices -
 *   school_district_administrators: payer (organized/constrained) — absorbs
 *   procurement and retraining costs - struggling_readers: principal intended
 *   beneficiary (powerless/trapped) - typical_developing_readers:
 *   near-symmetric seat (powerless/trapped) - dyslexia_advocacy_families:
 *   beneficiary-agenda-setter hybrid (organized/constrained) -
 *   whole_language_advocates: excluded seat (institutional/identity_locked) -
 *   literacy_researchers: analytical observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.42).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.52).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Systematic Phonics Foundational Instruction Requirement").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational/cognitive-science/pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, '3854b3a7-dd90-4084-b676-d57d062d3092').
narrative_ontology:cs_kernel_codification('3854b3a7-dd90-4084-b676-d57d062d3092', distributed).
narrative_ontology:cs_authority_grounding('3854b3a7-dd90-4084-b676-d57d062d3092', expertise).
narrative_ontology:cs_interpretation_layer_present('3854b3a7-dd90-4084-b676-d57d062d3092').
narrative_ontology:cs_reading_relation('3854b3a7-dd90-4084-b676-d57d062d3092', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('3854b3a7-dd90-4084-b676-d57d062d3092', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('3854b3a7-dd90-4084-b676-d57d062d3092', foundational, explicit_systematic_gpc_instruction_necessary).
narrative_ontology:cs_axiom_status(explicit_systematic_gpc_instruction_necessary, holdable).
narrative_ontology:cs_axiom_grounding('3854b3a7-dd90-4084-b676-d57d062d3092', explicit_systematic_gpc_instruction_necessary, empirically_contingent).
narrative_ontology:cs_axiom('3854b3a7-dd90-4084-b676-d57d062d3092', secondary, fidelity_to_scope_and_sequence_over_eclectic_adaptation).
narrative_ontology:cs_axiom_status(fidelity_to_scope_and_sequence_over_eclectic_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('3854b3a7-dd90-4084-b676-d57d062d3092', fidelity_to_scope_and_sequence_over_eclectic_adaptation, instrumental).
narrative_ontology:cs_reference_frame('3854b3a7-dd90-4084-b676-d57d062d3092', systematic_explicit_code_instruction_baseline).
narrative_ontology:cs_drift_state('3854b3a7-dd90-4084-b676-d57d062d3092', contemporary_science_of_reading_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3854b3a7-dd90-4084-b676-d57d062d3092', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, typical_developing_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, curriculum_publishers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, school_district_administrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, classroom_teachers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, dyslexia_advocacy_families).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, typical_developing_readers).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, simple_view_of_reading).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, national_reading_panel_findings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and administer the instructional standards: statutes requiring systematic phonics, approved instructional-materials lists, screening and progress-monitoring schedules, and teacher-preparation accreditation rules. They respond to legislative sessions, advocacy campaigns, and assessment results; they can revise the approved list or the required sequence, but cannot abandon standards-setting altogether without statutory change.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, state_education_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Develop and sell structured literacy programs, decodable text series, assessment platforms, and training packages that districts purchase to demonstrate compliance. Product lines pivot quickly toward whatever the approved lists require; the same firms previously sold meaning-first and integrated materials and will sell whatever the next consensus demands.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, curriculum_publishers, beneficiary,
    powerful, biographical, arbitrage, global).

% Deliver daily lessons from a prescribed scope-and-sequence, complete required training modules, and document progress-monitoring data. Novice teachers report gaining a concrete method their preparation programs never supplied; veterans trained in meaning-first approaches report losing instructional judgment accumulated over careers. Compliance is monitored through walkthroughs and data dashboards; opting out risks evaluation consequences, and leaving the profession is the main exit.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, classroom_teachers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, classroom_teachers, beneficiary).

% Purchase the mandated materials and training within fixed budgets, schedule screening windows, and answer to school boards and local taxpayers for the spending. They choose among approved vendors but not whether to buy, and they absorb the recurring license and retraining costs whenever the approved list changes.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, school_district_administrators, payer,
    organized, biographical, constrained, regional).

% Children who do not pick up decoding incidentally — disproportionately from homes with fewer books or less adult reading time, and including most children with dyslexia. They receive daily small-step code instruction, decodable practice, and frequent screening; before such instruction reached their classrooms, their typical trajectory was falling further behind each year with no route around the teacher they were assigned.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, immediate, trapped, national).

% Children who would likely crack the code under almost any method. They move through the same sequence, often consolidating faster and spelling more accurately, while some instructional minutes go to code work they may not have needed; they cannot choose their curriculum and mostly experience the sequence as ordinary school.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, typical_developing_readers, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, typical_developing_readers, payer).

% Parent networks organized after watching children fail under meaning-first instruction despite intact intelligence and effort. They lobby legislators, testify at hearings, and supply much of the political pressure behind screening mandates and approved-list laws; their children are the population the screening catches earliest.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, dyslexia_advocacy_families, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, dyslexia_advocacy_families, agenda_setter).

% Teacher-education faculty, authors, and consultants whose careers and scholarly identities are built on meaning-first theory. Legislation and approved lists have removed their methods from most public classrooms; they publish critiques, retain influence in preparation programs and professional networks, and experience the new mandates as the erasure of a life's work rather than a correction.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, whole_language_advocates, excluded,
    institutional, generational, identity_locked, national).

% Cognitive psychologists, education scientists, and synthesis authors who run the trials and meta-analyses that every camp cites. They hold no instructional duties and sell no product lines; their leverage is evidentiary — effect sizes, replication, and the credibility of the next synthesis.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, literacy_researchers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__phonics_reading, curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__phonics_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes how a large, unevenly trained teaching workforce delivers beginning decoding instruction: a shared scope-and-sequence, common screening checkpoints, and vetted materials mean a child's literacy no longer depends on which classroom they are assigned to, and novice teachers inherit a working method instead of improvising one.
% TRANSFER_FUNCTION: Moves pedagogical decision authority and daily instructional time from individual teachers to standardized sequences defined by agencies and program vendors; moves public funds from district budgets to commercial publishers through mandated material adoptions and training contracts; moves decoding skill to children, disproportionately to those who would not otherwise acquire it.
% ABSENT_VOICES: Meaning-first theorists and veteran teachers trained in whole-language methods were largely absent from the legislative hearings that produced the mandates; their objections enter mainly through published critiques after the fact. Children, the people the instruction lands on, are present only through proxy testimony from parents and advocacy organizations.
% DISAPPEARANCE_RATIONALE: If the requirement vanished overnight, approved lists would lapse, districts would drift back toward eclectic and meaning-first materials within a few adoption cycles, screening would thin out, and struggling readers would again depend on which teacher they drew; the structured-literacy product market would contract sharply and teacher-preparation syllabi would diverge again.
% FOUNDING_PROBLEM: Mass reading failure: for decades a large minority of children — concentrated among the poor — left elementary school unable to read reliably, while national panels and syntheses documented a widening gap between how reading was taught (immersion in authentic text, guessing from context and pictures) and what laboratory and classroom evidence indicated about how the skill is actually acquired.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: national and international assessments continue to document persistent reading failure; developmental and neuroimaging research on dyslexia attests the decoding deficit the arrangement targets; independent syntheses (the National Reading Panel, the What Works Clearinghouse, the Education Endowment Foundation toolkit) attest the efficacy claim. No serious party outside the arrangement disputes that reading failure exists; the live dispute is over cause and remedy.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__phonics_reading_tests).
:- end_tests(reading_acquisition_mechanism__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end): the dominant flow is genuine coordination value delivered to children, but two real extractions ride the same structure — budget premiums captured because approval lists narrow eligible vendors, and pedagogical discretion transferred from experienced teachers to standardized programs. Suppression (0.52) is authored as a raw structural property, unscaled by power or scope (only extractiveness is scaled downstream by directionality and scope): the enforcement machinery — statutes, approved lists, funding conditionality, monitored compliance — is real and it does displace the rival pedagogy, but exits persist (private, tutoring, homeschool routes; balanced literacy survives in modified form), so suppression sits mid-range rather than high. Theater ratio is low (0.18): the instruction does what it claims where implemented — decoding outcomes improve — though performative compliance (materials purchased, dashboards populated, pacing guides followed nominally) grows with enforcement intensity. Accessibility collapse (0.55): once the evidence base is understood, meaning-first-only positions become hard to sustain publicly, yet hybrid forms persist, so alternatives are partly but not completely collapsed. Resistance (0.48): organized pushback from teacher-education faculties, portions of the teaching force, and professional networks defending meaning-first practice. The temporal series run on one shared grid (t=0,6,12,18,24,30) with all three metrics authored at every point; the trajectories are monotonic ratchets, not cycles — enforcement capacity built up steadily as legislation spread, with extraction and theater following. Claim and metrics are independent authored facts: tangled_rope is my structural claim from this seat; the engine computes per-seat types from the data.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agency seat the arrangement is standards administration responding to documented failure; from the publisher seat it is a market; from the struggling-reader seat it is the difference between acquiring literacy and not; from the typical-reader seat it is near-symmetric (modest benefit, modest deadweight time); from the district-administrator seat it is unfunded-mandate procurement; and the teacher seat splits internally — novices experience inherited method as scaffolding while veterans trained in meaning-first approaches experience the same lesson plans as deskilling, with an identity-locked subset for whom compliance violates professional self-concept. The powerless child seats cannot mount direct resistance; their coalition power runs through the organized parent advocacy seat, which is why the political pressure behind the mandates came from parents rather than from the children affected. The engine computes these per-seat classifications from the structural data; nothing here adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Struggling_readers sit nearest the beneficiary pole (d near 0.05): full recipients of the arrangement's subsidy, with trapped exit meaning they receive everything the classroom delivers. Typical_developing_readers sit near symmetric (d ~0.45): real decoding consolidation gained, real instructional minutes paid. Curriculum_publishers sit near the beneficiary pole (d ~0.12) with arbitrage-grade exit — they collect the procurement flow and can pivot product lines at will. Classroom_teachers sit target-leaning (d ~0.65): discretion and labor flow away from them through the same structure that hands them a usable method, and their constrained exit keeps them in the paying position. School_district_administrators are similar (d ~0.6) with less offsetting gain. Dyslexia_advocacy_families sit low (d ~0.12) as beneficiaries who also helped build the arrangement. Whole_language_advocates sit near the target pole (d ~0.8): the arrangement's operation displaces their practice and market, and their identity_locked exit means displacement lands as identity injury rather than a relocatable cost. State_education_agencies carry no beneficiary declaration; as agenda setters their position is administrative — they neither consume the subsidy nor bear the transfer, and the engine's fallback governs their seat. Literacy_researchers are analytical and feed no directional arithmetic. No directionality overrides are authored: the role-plus-exit derivation captures the asymmetries, and the available override keys by power atom would misapply across the three distinct institutional seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mass reading failure — is live and independently corroborated, so the mismatch consumer reads status=live against verdict=world_rearranges and finds no zombie signature: the arrangement still performs the function it was built for. The classification guards both directions of mislabeling. Against pure-coordination labeling: the victim declarations (teachers, district administrators) and the concentrated receipt surface (publishers) prevent the rent premium and discretion transfer from being absorbed into 'coordination cost.' Against pure-extraction labeling: the dominant flow remains literacy delivered to children who would otherwise not acquire it, so the arrangement cannot be read as a snare wearing a pedagogical costume. The forward risk is Goodhart drift: as enforcement matures, box-ticking compliance can substitute for instructional function — the theater_ratio series is the tripwire, and a sustained rise past 0.5 would signal drift toward a piton profile (mandate maintained performatively while the function atrophies). Fixing is cheap for the agenda setter — the legal lever is a statutory amendment, already exercised by several states — which distinguishes this arrangement from entrenched structures whose removal cost is prohibitive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading (phonics_reading) of the kernel reading_acquisition_mechanism; how would instantiating a sibling reading (whole_language_reading, balanced_literacy_reading) change the structural picture?',
    'Corpus-level comparison across the constraint family: each sibling file carries its own epsilon, beneficiary/victim structure, and enforcement profile; disagreement is located in the necessity claim (whether explicit systematic grapheme-phoneme instruction is foundational or emergent/incidental).',
    'Under whole_language_reading the mandate itself becomes the extractive object (imposed method suppressing the endorsed one) and the beneficiary set inverts; under balanced_literacy_reading the enforcement burden softens and the victim set narrows. This file''s classification holds only under this reading''s premises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    necessity_universality,
    'Is explicit systematic grapheme-phoneme instruction necessary for every child, or necessary for reliable acquisition at population scale while some children acquire decoding through exposure alone?',
    'Longitudinal item-level studies crossing instructional dosage with learner profile (home literacy environment, phonological awareness at entry): if a identifiable subpopulation reaches functional decoding with negligible explicit code instruction, the necessity claim is scale-relative rather than universal.',
    'If necessity is scale-relative, the universal mandate imposes deadweight instructional time on the acquiring subpopulation and effective extraction at the typical-reader seat is higher than modeled; if necessity is universal, the modeled benefit understates the arrangement''s value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_universality, empirical, 'Whether the foundational-instruction requirement binds all learners or only the population reliably.').

omega_variable(
    veteran_teacher_identity_lock,
    'Is the measured teacher-side resistance structural (compliance burden, monitoring load, lost planning autonomy) or internalized (professional identity fused with meaning-first methods acquired during training)?',
    'Post-transition attitude and practice trajectories of teacher cohorts trained before versus after the mandates: if resistance persists undiminished in classrooms with full resource support and visible student gains, the internalized component dominates.',
    'An internalized component predicts informal persistence of meaning-first practice after formal repeal and a longer decay tail for the suppression series; a purely structural component predicts rapid decay once compliance burdens are eased.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veteran_teacher_identity_lock, empirical, 'Structural versus internalized mechanism behind teacher-side resistance.').

omega_variable(
    vendor_rent_share,
    'What share of mandated program spending is coordination value (materials, training, screening tools that would be bought anyway) versus premium captured because approval lists narrow the eligible vendor set?',
    'Procurement audits comparing pricing for approved-list products against comparable unlisted products, and lobbying-expenditure records correlated with list composition changes.',
    'A high rent share would raise effective extraction at the district and taxpayer seats, strengthen the case that the receipt surface concentrates in the publisher seat, and push per-seat computation at payer seats further toward the extractive end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vendor_rent_share, empirical, 'Rent-versus-value split inside mandated program procurement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__phonics_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(read_tr_t0, observed).
narrative_ontology:measurement(read_tr_t6, reading_acquisition_mechanism__phonics_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement_basis(read_tr_t6, observed).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_mechanism__phonics_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement_basis(read_tr_t12, observed).
narrative_ontology:measurement(read_tr_t18, reading_acquisition_mechanism__phonics_reading, theater_ratio, 18, 0.14).
narrative_ontology:measurement_basis(read_tr_t18, observed).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_mechanism__phonics_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement_basis(read_tr_t24, observed).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_mechanism__phonics_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(read_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(read_be_t0, observed).
narrative_ontology:measurement(read_be_t6, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 6, 0.31).
narrative_ontology:measurement_basis(read_be_t6, observed).
narrative_ontology:measurement(read_be_t12, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 12, 0.34).
narrative_ontology:measurement_basis(read_be_t12, observed).
narrative_ontology:measurement(read_be_t18, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 18, 0.37).
narrative_ontology:measurement_basis(read_be_t18, observed).
narrative_ontology:measurement(read_be_t24, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement_basis(read_be_t24, observed).
narrative_ontology:measurement(read_be_t30, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(read_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0, 0.26).
narrative_ontology:measurement_basis(read_su_t0, observed).
narrative_ontology:measurement(read_su_t6, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 6, 0.3).
narrative_ontology:measurement_basis(read_su_t6, observed).
narrative_ontology:measurement(read_su_t12, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement_basis(read_su_t12, observed).
narrative_ontology:measurement(read_su_t18, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 18, 0.42).
narrative_ontology:measurement_basis(read_su_t18, observed).
narrative_ontology:measurement(read_su_t24, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 24, 0.47).
narrative_ontology:measurement_basis(read_su_t24, observed).
narrative_ontology:measurement(read_su_t30, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(read_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the reading wars' covers three structurally distinct arrangements, decomposed per the epsilon-invariance principle into one kernel with three readings: phonics_reading (this file), whole_language_reading, and balanced_literacy_reading. Each carries its own epsilon, beneficiary/victim structure, and enforcement profile; they are linked here as a constraint family. The upstream/downstream structure runs through the shared evidence base: the syntheses this reading cites are the same syntheses the siblings must answer to, so movement in the evidentiary layer propagates to all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
