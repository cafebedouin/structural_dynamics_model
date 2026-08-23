% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__balanced_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__balanced_literacy_reading
 *   human_readable: Balanced-Literacy Settlement Regime of the Reading Wars
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   From the mid-1990s through the early 2020s, American literacy instruction
 *   was governed by a settlement: balanced literacy, a framework holding that
 *   reading acquisition requires both systematic phonics instruction and
 *   meaningful engagement with connected text, that the two are complementary
 *   rather than opposed, and that calibrating the mix is the professional's
 *   key task. The settlement organized preparation curricula, adoption
 *   markets, coaching economies, and classroom routine. This file authors ONE
 *   reading of the literacy_acquisition_kernel - the
 *   balanced_literacy_reading - as a clean, epsilon-invariant constraint; the
 *   sibling readings (phonics_reading, whole_language_reading,
 *   structured_literacy_reading) are separate files, each with its own
 *   epsilon, victim set, and type, linked via network.affects_constraints.
 *   Sibling constraint_ids are authored under the kernel-prefix convention
 *   (literacy_acquisition_kernel__<reading>), matching this file's identifier
 *   pattern; the colloquial label 'how reading should be taught' covers all
 *   four structurally distinct commitments and was decomposed per the
 *   epsilon-invariance principle. Epsilon's referent is the standing
 *   arrangement under contest - the balanced-literacy regime as actually
 *   implemented - valued BY THIS READING'S OWN LIGHTS: the reading concedes
 *   the churn and ambiguity-purchase costs of its own arrangement (moderate)
 *   while locating student harm in implementation infidelity rather than
 *   design, which holds the reading-indexed epsilon below what a rival
 *   reading of the same arrangement would author. The claimed_type and the
 *   metrics are independently authored facts: I claim tangled_rope because
 *   the structure carries a genuine coordination function AND an asymmetric
 *   extraction face behind active enforcement; the metrics describe the
 *   arrangement's actual operation without being tuned to any predicted
 *   engine verdict.
 *
 * KEY AGENTS:
 *   - education_school_faculties: primary agenda-setter (institutional/identity_locked) - defines the framework in preparation coursework; exit means scholarly self-repudiation
 *   - literacy_curriculum_publishers: principal beneficiary (institutional/arbitrage) - collects adoption and renewal revenue; pivots catalogs across method regimes
 *   - literacy_consultants_coaches: secondary beneficiary (organized/mobile) - sells the interpretation the undefined 'balance' requires; portable to any successor framework
 *   - district_curriculum_leaders: enforcing administrator, dual-positioned payer (institutional/constrained) - signs the contracts, monitors fidelity, absorbs budget and political costs
 *   - struggling_decoding_readers: primary target (powerless/trapped) - children who needed systematic code instruction their school's mix ran thin on
 *   - independent_early_readers: incidental beneficiary (powerless/trapped) - home-compensated decoders whose visible success masks the stalled minority
 *   - classroom_teachers: dual-positioned implementer (organized/identity_locked) - absorbs retraining and blame; professional identity fused with the framework's philosophy
 *   - dyslexia_family_advocates: paying outsider-turned-coalition (organized/constrained) - buys privately what schools withhold, then forces the legislative reckoning
 *   - reading_science_researchers: excluded voice for most of the run, late-cycle analyst (institutional/analytical) - produced the evidence the framework's rooms never admitted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.5).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.5).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced-Literacy Settlement Regime of the Reading Wars").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, '0b8c8a2a-4be5-4ae1-ab1a-80b2a5a965b6').
narrative_ontology:cs_kernel_codification('0b8c8a2a-4be5-4ae1-ab1a-80b2a5a965b6', formalized).
narrative_ontology:cs_authority_grounding('0b8c8a2a-4be5-4ae1-ab1a-80b2a5a965b6', lineage).
narrative_ontology:cs_interpretation_layer_present('0b8c8a2a-4be5-4ae1-ab1a-80b2a5a965b6').
narrative_ontology:cs_reading_relation('0b8c8a2a-4be5-4ae1-ab1a-80b2a5a965b6', literacy_acquisition_kernel__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('0b8c8a2a-4be5-4ae1-ab1a-80b2a5a965b6', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('0b8c8a2a-4be5-4ae1-ab1a-80b2a5a965b6', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('0b8c8a2a-4be5-4ae1-ab1a-80b2a5a965b6', foundational, code_meaning_complementarity).
narrative_ontology:cs_axiom_status(code_meaning_complementarity, holdable).
narrative_ontology:cs_axiom_grounding('0b8c8a2a-4be5-4ae1-ab1a-80b2a5a965b6', code_meaning_complementarity, instrumental).
narrative_ontology:cs_axiom('0b8c8a2a-4be5-4ae1-ab1a-80b2a5a965b6', secondary, professional_balance_adjudication).
narrative_ontology:cs_axiom_status(professional_balance_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('0b8c8a2a-4be5-4ae1-ab1a-80b2a5a965b6', professional_balance_adjudication, conventional).
narrative_ontology:cs_reference_frame('0b8c8a2a-4be5-4ae1-ab1a-80b2a5a965b6', integrated_code_and_meaning_instruction).
narrative_ontology:cs_drift_state('0b8c8a2a-4be5-4ae1-ab1a-80b2a5a965b6', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('0b8c8a2a-4be5-4ae1-ab1a-80b2a5a965b6', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, education_school_faculties).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, literacy_curriculum_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, literacy_consultants_coaches).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, independent_early_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, struggling_decoding_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, district_curriculum_leaders).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, dyslexia_family_advocates).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__balanced_literacy_reading, instructional_complementarity_thesis).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__balanced_literacy_reading, professional_judgment_autonomy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Train and certify the large majority of elementary teachers; define balanced literacy in methods coursework, select the textbook and clinical models that reproduce it each cohort, and staff the journals and committees where the framework is elaborated. Their published scholarship and program identities descend from the progressive tradition the framework claims; adopting a rival framing wholesale would mean repudiating decades of their own work. Preparation-course enrollment and graduate credit for later professional development both flow through their programs.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, education_school_faculties, agenda_setter,
    institutional, generational, identity_locked, national).

% Sell what the framework specifies: leveled text libraries that are consumed and replaced on cycles, benchmark assessment kits, lesson scripts, and digital platforms. Because the framework sets no fixed criterion for the instructional mix, every reframing of balance opens a new edition and adoption season; revenue tracks interpretive revision rather than product durability. Conference sponsorship and author partnerships keep their catalogs adjacent to whoever defines the framework next.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, literacy_curriculum_publishers, beneficiary,
    institutional, biographical, arbitrage, global).

% Supply the professional development and classroom coaching the framework requires: since balance has no operational definition, schools must purchase judgment about what the right mix is and whether a given classroom strikes it. A fully scripted method would eliminate their role, so their income depends on continued interpretive openness. Their skills are portable across frameworks, letting them rebrand to whichever approach districts adopt next.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, literacy_consultants_coaches, beneficiary,
    organized, biographical, mobile, national).

% Select programs, sign multi-year adoption contracts, hire coaches, and monitor fidelity to pacing guides; answer to boards for reading scores on annual cycles. They carry the budget cost of each materials refresh and the political cost when scores lag, and they are locked into contract and procurement timelines that outlast any individual superintendent's tenure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, district_curriculum_leaders, agenda_setter,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, district_curriculum_leaders, payer).

% Children who do not infer the alphabetic code from exposure and print-rich surroundings, disproportionately students with dyslexia and students whose homes supply little print. They receive whatever mix their school's version of the framework delivers; where explicit code instruction runs thin, they stall in the earliest grades while peers advance, and the gap compounds each year. They cannot choose their school's method, and private remediation is priced far beyond most families.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, struggling_decoding_readers, payer,
    powerless, biographical, trapped, national).

% Children who crack the code readily with minimal explicit instruction, typically from homes rich in books and shared reading that supply outside school whatever the classroom mix omits. They flourish under the framework's emphasis on authentic literature and show up prominently in aggregate scores, which masks the stalled minority in the same classrooms.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, independent_early_readers, beneficiary,
    powerless, biographical, trapped, national).

% Deliver daily instruction under the framework, absorb retraining each time the recommended mix shifts, and receive the blame when scores disappoint. Many sincerely value the literature-rich classroom culture the framework licenses and have built their professional self-concept on its philosophy, so public criticism of the method lands as criticism of their craft. Union representation protects employment, not instructional doctrine.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, beneficiary).

% Families who discover their child cannot decode, pay out of pocket for private assessment and tutoring costing several thousand dollars a year, and organize through chapter networks and legislative testimony for mandated systematic instruction. Their private spending exists because the school's mix omits what their child needs; over the interval's back half their advocacy becomes the loudest organized opposition the framework faces.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, dyslexia_family_advocates, payer,
    organized, biographical, constrained, national).

% Produce the controlled evidence on decoding instruction, comprehension development, and implementation effects that bears directly on the framework's design choices. For most of the interval they stand outside the rooms where preparation syllabi and adoption decisions are made; findings circulate in journals while practice runs on program guides. They enter policy only when journalism and family advocacy carry the findings to legislatures late in the run.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, reading_science_researchers, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__balanced_literacy_reading, literacy_curriculum_publishers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives a fractured profession one trainable, purchasable answer to what reading instruction includes: both explicit code work and meaning-rich text, sequenced by professional judgment. It lets districts standardize teacher preparation, program adoption, and daily classroom routine at scale, and it hands novice teachers a workable structure that neither pole of the preceding conflict supplied alone.
% TRANSFER_FUNCTION: Moves adoption, licensing, and renewal spending from district budgets (ultimately taxpayers) to program publishers and the consulting layer; moves tuition-bearing enrollment through preparation coursework; moves classroom time and attention away from isolated skill work toward guided reading of leveled texts wherever a district's version of the mix runs code-thin; and, when results disappoint, moves accountability downward onto teachers and family circumstance rather than upward onto the framework's design.
% ABSENT_VOICES: Reading-science researchers and the families of children who failed to decode sat outside the rooms where the framework was written into preparation syllabi and adoption contracts; panel findings circulated in journals while practice ran on program guides. Decoding Dyslexia formed precisely because no seat existed for those families. Both groups enter late, through journalism and legislatures, rather than through the framework's own deliberative bodies.
% DISAPPEARANCE_RATIONALE: Preparation curricula, adoption contracts, leveled-text inventories, coaching rosters, and assessment calendars are all arranged around this framework. Overnight removal would strand districts mid-contract, force publishers to relabel existing stock, leave newly hired teachers without the methods training they were promised, and push every adopting system into an unplanned re-adoption cycle. Instruction itself would reorganize around successor frameworks rather than continue unchanged.
% FOUNDING_PROBLEM: Settle the reading wars: after whole-language dominance left documented cohorts of children unable to decode, and earlier code-heavy eras left fluent decoders with weak comprehension and reluctant readers, the profession needed a single settlement that districts could train, buy, and inspect without re-litigating method allegiance every budget cycle.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the National Reading Panel (2000) and the National Early Literacy Panel (2008) attest that decoding deficits persisted through the settlement years; state science-of-reading legislative findings (Mississippi 2013 onward, the 2019-2024 statute wave) and investigative reporting (APM Reports, 2018-2022) attest the problem remained live under the framework. No source outside the benefiting parties attests that the settlement resolved the founding problem; the beneficiaries' own attestations claim peace and integration, which is a different claim.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.50 - moderate, per this reading's own lights: the reading concedes that its arrangement generates recurring materials spend (consumable leveled libraries, benchmark kits, edition cycles), a purchased-interpretation burden (undefined 'balance' obliges every adopting district to buy judgment), and some design-level responsibility for implementation drift toward code-thin practice, while refusing to book student decoding failure as a cost of the design itself. That refusal is exactly what reading-indexed epsilon encodes; the engine's per-seat computation restores the asymmetry through directionality and scope. Suppression is a raw, unscaled structural property authored at 0.50: rival practice was marginalized through preparation-curricular omission, program-material gatekeeping, and professional-norm pressure rather than legal prohibition, and external alternatives (tutoring markets, homeschool, other publishers) persisted throughout. Theater_ratio 0.42: benchmark rituals, running-record routines, logged PD hours, and 'balanced' lesson templates grew into compliance performance over the run, while genuine integration of code and meaning work also occurred in well-implemented classrooms. Accessibility_collapse 0.55: once a district adopted, switching costs (contracts, trained staff, stocked libraries) collapsed in-system alternatives, but the wider market kept exits available. Resistance 0.60: sustained and escalating - panel reports, meta-analytic critiques, family coalitions, investigative journalism, and finally statute-level displacement. All three temporal series share one grid (points 0, 6, 12, 18, 24, 30); suppression_requirement is traced deliberately because enforcement machinery visibly built up (norms-only, then adoption contracts and fidelity monitoring, peaking mid-run) and then decayed as state law redirected it. Two cyclical notes: within the interval the series rise-then-ease as dismantling begins; beneath it runs the generational pendulum (Chall 1967, whole language 1980s, NRP 2000, science of reading 2010s-20s) whose swings are themselves part of the arrangement's economics - each swing regenerates demand for new materials and retraining, so oscillation functions as revenue mechanism, not noise. Identity-lock dynamics bind two seats: classroom_teachers fuse professional self-concept with the framework's philosophy (criticism of method registers as criticism of craft), and education_school_faculties face exit priced as repudiation of their own scholarship; if either frame broke, those seats would recompute toward mobile/constrained and the arrangement's enforcement base would erode faster than materials turnover alone predicts. Coalition check: the powerless child seat cannot coalition directly; its interests reached policy only through the organized proxy (dyslexia_family_advocates) - the case where a proxy seat, not the trapped victims themselves, converts diffuse harm into binding override.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently and the engine computes that divergence from the structural data. From the education_school_faculties seat the arrangement is professional stewardship of a humane, integrated pedagogy - coordination they built and defend. From the literacy_curriculum_publishers seat it is a durable product line whose interpretive openness guarantees repeat sales; their arbitrage-grade exit means no single method's fall ruins them. From the struggling_decoding_readers seat the same arrangement is delayed access to the code their schooling never systematically taught - experienced from a trapped seat that cannot shop between methods. Classroom_teachers straddle: beneficiaries of the autonomy and culture the framework licenses, payers of its churn and blame, and identity-fused enough that external criticism reads as personal attack. District_curriculum_leaders sit nearest symmetric: they wield enforcement yet bankroll it. The reading's own seat (this file's authorial position) sees stewardship with conceded costs; a structured-literacy-seat file of the same arrangement should author a higher epsilon over the identical referent.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to the low-d end: publishers and consultants collect directly from the arrangement's operation with arbitrage/mobile exits; education_school_faculties collect enrollment, credit, and turf protection despite an agenda_setter role, their identity_locked exit pulling them toward deeper structural commitment rather than away. independent_early_readers benefit incidentally - subsidized by home literacy the classroom mix presupposes. Victims map to the high-d end: struggling_decoding_readers bear the arrangement's sharpest cost from a trapped, powerless seat (near full-target); classroom_teachers bear retraining, blame, and interpretive labor from an identity_locked seat, damped somewhat by their genuine beneficiary side-role. District_curriculum_leaders derive near-symmetric from their dual agenda_setter/payer position. No directionality_overrides are authored: the derivation chain reproduces the qualitative structure from role, exit, and power declarations, and because the institutional and organized power atoms each hold several agents with genuinely different relationships (faculties vs publishers vs districts; consultants vs teachers vs advocates), a power-atom-keyed override would flatten real variation rather than correct a wrong derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what keeps both faces of the settlement visible. Reading it as pure rope (the framework's own framing) dissolves the churn economy and the purchased-interpretation burden into ordinary coordination cost, and books the stranded decoders as implementation error; reading it as pure snare erases the real coordination achievement - a workable, trainable structure that let thousands of districts standardize instruction and that served many children well. The mandatrophy interview sharpens the picture: the founding problem (settling the wars while ending decoding casualties) is contested rather than dead - the war ended, the casualties did not clearly - so the arrangement persisted past its demonstrated adequacy on enforcement momentum rather than on solved function. Forward-looking risk: if statutory structured-literacy displacement completes while 'balanced' branding survives atop mandated phonics blocks, the residue becomes theatrical maintenance of a displaced framework - the signature the engine watches for, with theater_ratio already elevated. Keeping founding_problem_status contested (rather than dead) keeps the zombie-flag mismatch consumer honest: the problem is disputed, not buried, and the beneficiaries' peace-and-integration attestation is corroborated by no outside source.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_synthesis_vs_whole_language_rebrand,
    'Is this reading a genuine third position - a distinct synthesis in which explicit code instruction and meaning-rich text are jointly optimized - or whole-language practice retitled, preserving its assumptions (context-first word-solving, minimal explicit code) under a conciliatory label?',
    'Compare implemented classroom practice under balanced-branded programs against whole-language baselines: weekly minutes of explicit systematic code instruction, decodability profile of independently read text, prevalence of cueing-based word-solving prompts. Practice distributions indistinguishable from whole-language classrooms support the rebrand verdict; materially higher explicit-code dosage supports genuine synthesis.',
    'A rebrand verdict collapses this reading onto the whole_language_reading structure - same victim set (students denied systematic code instruction), higher effective extraction, and the balanced label functioning as cover rather than synthesis. A genuine-synthesis verdict supports treating the arrangement as coordination with modest excess cost and preserves the victim set as narrower than declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_synthesis_vs_whole_language_rebrand, conceptual, 'The kernel contest''s central question: whether the balanced reading is a real third reading or a relabeled continuation of whole language.').

omega_variable(
    victim_structure_under_specified_mix,
    'Who bears the arrangement''s net costs - the students in districts whose version of the mix runs code-thin, the teachers absorbing retraining and blame, or no one beyond ordinary coordination costs?',
    'Disaggregate reading outcomes by implementation fidelity and explicit-code dosage across balanced-literacy implementations; separate outcome gaps attributable to the mix itself from gaps attributable to implementation variance and selection.',
    'If costs concentrate on code-instruction-starved students, the victim declarations stand and the asymmetric face is confirmed; if outcomes are flat across mixes, the constraint recomputes toward low-extraction coordination and the victim arrays should shrink to the teacher seat alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_structure_under_specified_mix, empirical, 'Resolves the manifest-flagged ambiguity (''victim: unclear - either achieves synthesis or satisfies neither camp'') about who actually pays.').

omega_variable(
    balance_operationalizability,
    'Is ''instructional balance'' a specifiable calibration on which competent judges converge, or irreducibly open-ended such that every adopting district must purchase interpretation?',
    'Have independent expert raters assess concrete instructional schedules for balance and measure inter-rater convergence; convergence implies specifiable criteria exist, divergence implies the term has no stable operational content.',
    'Non-specifiability locates a durable interpretive levy in the coaching, PD, and edition-cycle layer and strengthens the extraction face; specifiability converts those flows into ordinary professional-judgment costs and weakens the asymmetric-extraction claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_operationalizability, conceptual, 'Whether the framework''s defining term carries operational content or monetizes its own ambiguity.').

omega_variable(
    authority_grounding_lineage_or_extraction,
    'Does the interpretive authority over the framework (preparation faculties, program authors, the coaching layer) ground its legitimacy in scholarly lineage - continuity with the Clay/Goodman/Dewey tradition - or in the revenue that kernel stability plus periodic reinterpretation generate?',
    'Trace revision timing against evidentiary developments (predicts lineage-grounding: revisions track studies) and against adoption and market cycles (predicts extraction-grounding: revisions track selling seasons); examine citation practice and the revision histories of flagship programs.',
    'Under the extraction framing the authority structure recomputes as drift-denial-funded, strengthening the suppressive-maintenance reading of enforcement; under lineage, revisions are ordinary scholarly updating and the enforcement apparatus reads as professional conservatism. The two framings yield different cs_pattern classifications for the same arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_grounding_lineage_or_extraction, conceptual, 'Alternative defensible framings of what grounds the interpretive authority; documents the framing under-determination rather than resolving it silently.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the marginalization of explicit code instruction structural (absent from preparation curricula, program materials, and adoption lists) or internalized (practitioners'' professional identities carrying an aversion to explicit teaching that outlasts the barriers)?',
    'Track instructional practice in districts after statutory mandates remove the structural barriers: rapid recovery of explicit-code dosage indicates predominantly structural suppression; multi-year lag indicates a substantial internalized share carried in practitioner identity.',
    'A large internalized share predicts slow re-equilibration and continued enforcement need even after formal displacement of the framework; purely structural suppression predicts fast practice change once materials and mandates flip.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split for the arrangement''s suppression of rival practice: structural barriers versus internalized professional identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(lite_tr_t0, observed).
narrative_ontology:measurement(lite_tr_t6, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 6, 0.29).
narrative_ontology:measurement_basis(lite_tr_t6, observed).
narrative_ontology:measurement(lite_tr_t12, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement_basis(lite_tr_t12, observed).
narrative_ontology:measurement(lite_tr_t18, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement_basis(lite_tr_t18, observed).
narrative_ontology:measurement(lite_tr_t24, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement_basis(lite_tr_t24, observed).
narrative_ontology:measurement(lite_tr_t30, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(lite_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(lite_be_t0, observed).
narrative_ontology:measurement(lite_be_t6, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 6, 0.44).
narrative_ontology:measurement_basis(lite_be_t6, observed).
narrative_ontology:measurement(lite_be_t12, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement_basis(lite_be_t12, observed).
narrative_ontology:measurement(lite_be_t18, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 18, 0.5).
narrative_ontology:measurement_basis(lite_be_t18, observed).
narrative_ontology:measurement(lite_be_t24, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement_basis(lite_be_t24, observed).
narrative_ontology:measurement(lite_be_t30, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement_basis(lite_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(lite_su_t0, observed).
narrative_ontology:measurement(lite_su_t6, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement_basis(lite_su_t6, observed).
narrative_ontology:measurement(lite_su_t12, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement_basis(lite_su_t12, observed).
narrative_ontology:measurement(lite_su_t18, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement_basis(lite_su_t18, observed).
narrative_ontology:measurement(lite_su_t24, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement_basis(lite_su_t24, observed).
narrative_ontology:measurement(lite_su_t30, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement_basis(lite_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, resource_allocation).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how reading should be taught' covers four structurally distinct commitments and was decomposed per the epsilon-invariance principle into four files of one constraint family, all linked via affects_constraints. This file authors only the balanced_literacy_reading; each sibling authors its own epsilon, victim set, and type over its own instantiated arrangement. Family topology: whole_language_reading is the practice tradition this reading descended from (and, per the rebrand omega, possibly merely relabeled); phonics_reading and structured_literacy_reading carry the evidentiary weight that pressured this reading late in its run, with structured_literacy_reading acting as the downstream policy vehicle that reshaped this reading's operating environment without logically eliminating it. Epsilon differs across the family because each reading values the same contested field by its own lights: this reading concedes moderate costs of its own arrangement; the structured-literacy seat should author a higher value over the identical referent; the phonics and whole-language seats author over partially different referents (their endorsed arrangements) and their files document that shift.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
