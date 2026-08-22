% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__structured_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__structured_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Structured Literacy Mandate (Orton-Gillingham Tradition)
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   Roughly one child in five does not learn to read from immersion in
 *   meaningful text alone; for a century the remedial answer has been
 *   explicit, sequential, multisensory instruction in the code — the
 *   Orton-Gillingham tradition. Since the mid-2010s that clinical answer has
 *   been scaled into a universal core: dozens of states now mandate early
 *   literacy screening, approved curricula built on the five pillars
 *   (phonological awareness, phonics, fluency, vocabulary, comprehension),
 *   teacher retraining through approved providers, and tiered intervention.
 *   The arrangement delivers real reading skill to students who would
 *   otherwise fail — and simultaneously routes a statutory revenue stream
 *   through certification institutes and curriculum publishers while
 *   transferring instructional authority away from classroom teachers. This
 *   story is ONE READING of the literacy_acquisition_kernel (see
 *   kernel_context); the sibling readings are separate constraints with
 *   separate epsilons, linked through the network block.
 *
 * KEY AGENTS:
 *   - - students_with_dyslexia: primary beneficiary (powerless/trapped) — receives the intervention the arrangement exists to deliver; cannot opt out or shop districts
 *   - - struggling_early_readers: secondary beneficiary (powerless/trapped) — get explicit code instruction earlier than home print exposure would supply
 *   - - general_education_teachers: primary payer (organized/constrained) — bear mandated retraining, certification coursework, and fidelity-monitored script delivery
 *   - - public_school_districts: payer and local administrator (organized/constrained) — absorb procurement and compliance costs, gain litigation cover
 *   - - state_reading_legislators: agenda setters (institutional/mobile) — enact screening, approved-list, and retraining statutes; collect political credit
 *   - - og_training_institutes: concentrated beneficiary (organized/arbitrage) — sell the credential the statutes require; accredit the accreditors
 *   - - structured_literacy_publishers: concentrated beneficiary (organized/arbitrage) — license the approved comprehensive programs
 *   - - private_dyslexia_tutors: beneficiary (moderate/constrained) — fee-for-service remediation priced behind the credential wall
 *   - - parents_of_struggling_readers: beneficiary and payer (moderate/constrained) — advocate and litigate for services; supplement with paid tutoring
 *   - - balanced_literacy_advocates: excluded (organized/analytical) — teacher educators and authors struck from approved lists, absent from legislative rooms
 *   - - reading_science_researchers: analytical observers (analytical/analytical) — produced the convergent evidence; hold no curriculum stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.5).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.6).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy Mandate (Orton-Gillingham Tradition)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, 'b753094f-29a8-4266-90f6-b40fdd7c380f').
narrative_ontology:cs_kernel_codification('b753094f-29a8-4266-90f6-b40fdd7c380f', distributed).
narrative_ontology:cs_authority_grounding('b753094f-29a8-4266-90f6-b40fdd7c380f', expertise).
narrative_ontology:cs_interpretation_layer_present('b753094f-29a8-4266-90f6-b40fdd7c380f').
narrative_ontology:cs_reading_relation('b753094f-29a8-4266-90f6-b40fdd7c380f', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('b753094f-29a8-4266-90f6-b40fdd7c380f', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('b753094f-29a8-4266-90f6-b40fdd7c380f', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('b753094f-29a8-4266-90f6-b40fdd7c380f', foundational, explicit_cumulative_code_instruction_necessary_for_all).
narrative_ontology:cs_axiom_status(explicit_cumulative_code_instruction_necessary_for_all, holdable).
narrative_ontology:cs_axiom_grounding('b753094f-29a8-4266-90f6-b40fdd7c380f', explicit_cumulative_code_instruction_necessary_for_all, empirically_contingent).
narrative_ontology:cs_axiom('b753094f-29a8-4266-90f6-b40fdd7c380f', foundational, dyslexia_derived_methods_generalize_to_all_learners).
narrative_ontology:cs_axiom_status(dyslexia_derived_methods_generalize_to_all_learners, holdable).
narrative_ontology:cs_axiom_grounding('b753094f-29a8-4266-90f6-b40fdd7c380f', dyslexia_derived_methods_generalize_to_all_learners, empirically_contingent).
narrative_ontology:cs_reference_frame('b753094f-29a8-4266-90f6-b40fdd7c380f', science_of_reading_evidence_standard).
narrative_ontology:cs_drift_state('b753094f-29a8-4266-90f6-b40fdd7c380f', post_2018_legislative_wave, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b753094f-29a8-4266-90f6-b40fdd7c380f', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, struggling_early_readers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, og_training_institutes).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, private_dyslexia_tutors).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, parents_of_struggling_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, public_school_districts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, parents_of_struggling_readers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, phonological_deficit_hypothesis).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, national_reading_panel_synthesis).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, convergent_reading_science_consensus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are children whose brains process written language atypically; without explicit, sequential instruction in sound-letter patterns they typically leave school reading far below grade level. Under this arrangement they receive daily structured lessons, small-group intervention, and progress monitoring matched to their profiles. They do not choose their curriculum, cannot practically shop for another district, and experience the arrangement almost entirely through whether someone finally teaches them to read.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia, beneficiary,
    powerless, biographical, trapped, national).

% Enter kindergarten without the phonological foundations that print-rich homes supply. The arrangement guarantees them the same explicit code instruction that used to be reserved for clinical caseloads. Most catch up; a minority are drilled on skills they already command, trading minutes of story time for repetition they did not need.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, struggling_early_readers, beneficiary,
    powerless, biographical, trapped, national).

% Must complete state-mandated retraining hours and, in many states, certification coursework from approved providers to keep teaching; deliver scripts and pacing guides under fidelity checks; surrender curricular judgment they previously exercised. Many report the training improved their instruction; nearly all report the compliance load and the message that their prior practice was deficient. Leaving the profession remains possible but carries pension, salary, and identity costs.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    organized, biographical, constrained, national).

% Administer the mandate locally: adopt from state-approved curriculum lists, purchase materials and coaching contracts, schedule screening windows, and document intervention fidelity for state review. They absorb procurement and personnel costs while gaining a defensible answer to litigation and parental complaint about reading failure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, public_school_districts, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__structured_literacy_reading, public_school_districts, agenda_setter).

% Enact and amend the statutes: screening mandates, approved-list restrictions, promotion provisions, and funded retraining pipelines. Reading legislation polls well across constituencies, generates ribbon-cuttings and press, and requires little ongoing attention once passed; amendment or repeal is procedurally available but rarely politically attractive.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, state_reading_legislators, agenda_setter,
    institutional, immediate, mobile, national).

% Sell the credential: accredited coursework, practicum supervision, and examination fees running to thousands of dollars per trainee, plus trainer-of-trainer licenses sold to districts. Demand is statutory in a growing number of states, which converts their syllabus into a revenue line; they accredit the accreditors and sit on the bodies that decide which providers count.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, og_training_institutes, beneficiary,
    organized, generational, arbitrage, global).

% License comprehensive K-2 and increasingly K-5 instructional systems, decodable text libraries, and assessment platforms to districts facing approved-list deadlines. Adoption decisions made once per cycle lock in multi-year material and subscription spending.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_publishers, beneficiary,
    organized, generational, arbitrage, global).

% Deliver fee-for-service remediation at hourly rates the credential regime underwrites; the certification wall limits competitor entry and sustains pricing. Caseloads shrink where schools implement well and swell where they do not.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, private_dyslexia_tutors, beneficiary,
    moderate, biographical, constrained, local).

% Advocate, litigate, and pay: they push districts for evaluation and services, sometimes supplementing school provision with private tutoring at significant household cost, and gain a concrete remedy where the arrangement functions. Where it functions badly they hold due-process rights but little practical recourse.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, parents_of_struggling_readers, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__structured_literacy_reading, parents_of_struggling_readers, payer).

% Teacher educators, curriculum authors, and professional organizations whose texts, methods, and graduate programs are being struck from approved lists and defunded in mandating states. They publish rebuttals and retain footholds in universities and unregulated markets but are largely absent from the legislative rooms where curricula are now chosen.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, balanced_literacy_advocates, excluded,
    organized, generational, analytical, national).

% Cognitive psychologists, speech-language pathologists, and education scientists who produced the convergent evidence this arrangement cites: eye-tracking, neuroimaging, longitudinal and intervention studies. They advise panels and testify, hold no financial stake in particular curricula, and continue to dispute effect sizes and dosage at the margins.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, reading_science_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__structured_literacy_reading, og_training_institutes).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__structured_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a large decentralized system — schools of education, districts, publishers, screener vendors, legislatures — on a single operational answer to 'what must happen for a child to read': screen early, teach the code explicitly and cumulatively in a fixed sequence across phonological awareness, phonics, fluency, vocabulary, and comprehension, monitor progress, intervene in intensity tiers. Without a shared standard, each classroom improvises and the children least served by improvisation fail invisibly.
% TRANSFER_FUNCTION: Moves public money and teacher labor toward training institutes and curriculum publishers (coursework fees, licensed materials, coaching contracts); moves instructional authority from individual teacher judgment to codified programs and fidelity monitoring; moves reading skill — the intended payload — to students, disproportionately to those who would otherwise have failed.
% ABSENT_VOICES: Whole-language and balanced-literacy scholars and the teacher educators who train most of the workforce were largely absent from the legislative drafting rooms in mandating states; classroom teachers were consulted as implementers rather than as parties; bilingual educators raising concerns about English-centric phonics norms had minimal presence. Their objections survive mainly in journals and conference panels rather than in adopted statute.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, districts would revert to whatever their incumbent staff prefer, screening and intervention tiers would dissolve back into wait-for-failure referral, the certification and curriculum industries built on statutory demand would contract sharply, and the roughly one student in five who does not break the code incidentally would again fail at scale — the pre-2013 equilibrium, with its litigation and attrition, would reassemble.
% FOUNDING_PROBLEM: Widespread reading failure concentrated in students with dyslexia, whom incidental text-exposure methods reliably failed: Samuel Orton's 1920s-30s clinical observations of 'word blindness,' Anna Gillingham and Bessie Stillman's 1936 codification of an explicit multisensory remedial method, and, in the modern wave, two decades of flat national reading scores alongside a widening gap between laboratory findings and classroom practice.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the National Reading Panel (2000) synthesis, Haskins Laboratories and Georgetown neuroimaging work on the reading circuit, pediatric and neuropsychological prevalence studies placing dyslexia near 15-20% of readers, and NAEP trend data documenting persistent bottom-quartile failure. None of these sources sells certification or curriculum; scholarly critiques from Seidenberg and Dehaene — also outside the benefiting parties — additionally attest that parts of the universal-scaling claim outrun the current evidence.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__structured_literacy_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.50: the arrangement's costs are real but ride on a service that works — statutory demand converts training syllabi and curriculum licenses into revenue lines, yet the payload delivered to dyslexic students is genuine, capping epsilon well below pure-extraction territory. Suppression is 0.60: persistence now depends on active machinery — screening mandates, approved-list exclusions, required retraining, fidelity monitoring — that forecloses rival pedagogies inside the public channel; suppression is a raw structural property and is deliberately NOT scaled by power or scope in this authoring. Theater ratio is 0.30: most activity is functional instruction, but a growing share is compliance performance — badge collection, box-checked professional-development hours, documentation for state review. Accessibility collapse is 0.50: within the legislated channel alternatives collapse hard (cueing-based materials banned, balanced-literacy texts delisted), but tutoring markets, private schools, homeschooling, and the academy keep rivals alive. Resistance is 0.55: teacher organizations, teacher-education faculties, and holdout districts mount real, organized opposition, though it weakens as evidence accumulates and statutes spread. Claimed type is tangled_rope, authored independently of these metrics: the structure holds a genuine coordination function (a common, teachable answer to reading failure) together with asymmetric extraction (certification premiums, locked procurement) maintained by active enforcement. The temporal series share one eight-point grid (1995-2025) so every tracked metric is authored at every examined time point; the inflection at 2013 marks the first modern statute wave and the post-2018 period drives the steepest segment. Receipt surface: the statutory fee streams demonstrably accrue to the training institutes (coursework, practicum, accreditation, trainer licenses), so gain_flow names that seat; fixing — decoupling the credential monopoly from statute while preserving the instructional core — is prohibitive for any single mover because approved-provider lists, accreditation chains, and multi-year district contracts interlock.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the dyslexic student's seat the arrangement is the thing that finally teaches reading — extraction is negative, a subsidy. From the veteran teacher's seat the same statutes arrive as compulsory retraining, script fidelity, and an official verdict that her prior craft was deficient — extraction near its ceiling. From the institute's seat the arrangement is demand: statutory, recurring, and defended by the accrediting bodies it staffs. From the researcher's seat it is a rough translation of convergent evidence into bureaucracy, with dosage and effect-size disputes at the margins. The engine computes these per-seat classifications from the structural data above; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: students_with_dyslexia and struggling_early_readers sit near the full-beneficiary end (trapped exit deepens the subsidy — they cannot leave, and the arrangement spends on them). og_training_institutes and structured_literacy_publishers also derive low d, but their arbitrage-grade exit means they collect without bearing compliance risk. general_education_teachers and public_school_districts derive near-full-target d: they pay in time, tuition-like fees, and procurement, with constrained exits (leaving the profession or defying statute). state_reading_legislators sit mid-range: they pay little directly and collect political returns, moderated by constituent accountability. parents_of_struggling_readers split — subsidy received through services, fees paid through tutoring supplements. No directionality overrides are authored: every seat's relationship is already legible from its role, power, and exit declarations, and the override surface (keyed by power atom) is too coarse to separate same-atom seats without contaminating neighboring agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — dyslexic students failing under incidental-exposure instruction — is live, corroborated by prevalence and NAEP data from sources that sell neither credentials nor curricula, so this is not a resolved mandate haunting its own funeral. The mandatrophy risk is prospective and directional: the founding problem justified a REMEDIAL arrangement, and the arrangement has since scaled itself to universal core instruction; if stratified evidence eventually shows the universal scope outruns the need, the mandate will have outlived its founding function while its certification apparatus persists — the classic transition from coordination to inertia. Classifying this as tangled_rope rather than rope keeps the certification-premium extraction visible inside a genuinely beneficial structure; classifying it as snare would erase the dyslexic student's subsidy and misread the strongest counterexample to pure-extraction readings. The six-questions mismatch consumer should find status=live crossed with verdict=world_rearranges — coherent, no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_membership_fourth_reading_or_variant,
    'Is structured literacy a distinct fourth reading of the literacy_acquisition_kernel, or a scope-expanded variant of the phonics_reading sibling?',
    'Comparative axiom analysis across the four readings: if this reading''s foundational claims reduce to phonics-first sequencing plus added pillars and population scope, merge it into phonics_reading; if the dyslexia-derived universality and five-pillar cumulativity generate distinct downstream obligations (screening regimes, tiered intervention, certification walls), retain it as a fourth reading.',
    'If merged, this story''s epsilon and victim set inherit phonics_reading''s profile and the family collapses to three readings; if retained, the certification-and-training extraction axis belongs to this reading alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_membership_fourth_reading_or_variant, conceptual, 'Whether this reading is independent of the phonics_reading sibling.').

omega_variable(
    universal_claim_evidence_boundary,
    'Does the empirical case support universal application of the full five-pillar structured regimen to all students, or only to at-risk and dyslexic populations, with the universal claim expanding the addressable market beyond the demonstrated need?',
    'Meta-analytic evidence stratified by reader risk profile and grade band (tiered-intervention randomized trials and their follow-ups): if effects for typical readers in general-education settings are small or null once dosage is accounted for, the universal claim is scope creep.',
    'If the universal claim outruns the evidence, the coordination function narrows to intervention for at-risk students and the arrangement''s extraction profile hardens on the general-population segment; if supported, the universal scope is coordination, not market expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_claim_evidence_boundary, empirical, 'Whether universal application is evidence-backed or market-expanding.').

omega_variable(
    certification_premium_efficacy_gap,
    'Does Orton-Gillingham-style certification add instructional efficacy beyond well-designed generic explicit-and-systematic-phonics training, or does the certification premium price a badge rather than a technique?',
    'Head-to-head trials comparing outcomes from certified OG-trained teachers versus teachers trained to comparable dosage in non-proprietary explicit-instruction programs, controlling for implementation fidelity.',
    'If outcomes are equivalent, the certification fee stream is positional rent riding on statutory demand and the training axis tilts sharply toward pure extraction; if certified delivery outperforms, the premium prices real skill.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_premium_efficacy_gap, empirical, 'Whether the certification premium tracks efficacy or exclusivity.').

omega_variable(
    sibling_disagreement_location,
    'Where exactly do the four readings of the literacy kernel disagree, and which disagreements are empirical versus valuational?',
    'Locate each sibling''s divergence point: whole_language_reading denies the necessity premise outright (an empirical disagreement, resolvable by convergent evidence); phonics_reading disputes pillar scope and sequencing emphasis (largely empirical); balanced_literacy_reading disputes whether complementarity can be operationalized without diluting explicitness (partly valuational).',
    'If the core disagreement is located in the necessity premise, evidence movements push whole_language_reading toward foreclosure; if located in weighting and dosage, all four readings persist indefinitely as live positions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_disagreement_location, conceptual, 'Location of inter-reading disagreement across the kernel''s sibling set.').

omega_variable(
    teacher_authority_transfer_valuation,
    'Is the transfer of instructional authority from teacher judgment to codified program a cost at all, or a correction of unearned discretion — and does the answer depend on values about professionalism rather than evidence?',
    'Not resolvable by outcome data alone: pairing implementation-fidelity studies with teacher professional-identity surveys can bound the factual components, but the residual valuation (whether fidelity-to-script is deference or deskilling) turns on commitments about what teaching is.',
    'If deskilling is affirmed as a real cost, the teacher seat''s burden includes an irreducible dignitary component no training stipend offsets; if dismissed, the burden reduces to compensable time and the payer asymmetry shrinks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_authority_transfer_valuation, preference, 'Valuational component of the teacher-burden claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(structured_literacy_reading_tr_t1995, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement_basis(structured_literacy_reading_tr_t1995, observed).
narrative_ontology:measurement(structured_literacy_reading_tr_t2000, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement_basis(structured_literacy_reading_tr_t2000, observed).
narrative_ontology:measurement(structured_literacy_reading_tr_t2005, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2005, 0.16).
narrative_ontology:measurement_basis(structured_literacy_reading_tr_t2005, observed).
narrative_ontology:measurement(structured_literacy_reading_tr_t2010, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement_basis(structured_literacy_reading_tr_t2010, observed).
narrative_ontology:measurement(structured_literacy_reading_tr_t2013, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2013, 0.21).
narrative_ontology:measurement_basis(structured_literacy_reading_tr_t2013, observed).
narrative_ontology:measurement(structured_literacy_reading_tr_t2018, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement_basis(structured_literacy_reading_tr_t2018, observed).
narrative_ontology:measurement(structured_literacy_reading_tr_t2022, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2022, 0.28).
narrative_ontology:measurement_basis(structured_literacy_reading_tr_t2022, observed).
narrative_ontology:measurement(structured_literacy_reading_tr_t2025, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2025, 0.3).
narrative_ontology:measurement_basis(structured_literacy_reading_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(structured_literacy_reading_be_t1995, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 1995, 0.22).
narrative_ontology:measurement_basis(structured_literacy_reading_be_t1995, observed).
narrative_ontology:measurement(structured_literacy_reading_be_t2000, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2000, 0.26).
narrative_ontology:measurement_basis(structured_literacy_reading_be_t2000, observed).
narrative_ontology:measurement(structured_literacy_reading_be_t2005, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2005, 0.3).
narrative_ontology:measurement_basis(structured_literacy_reading_be_t2005, observed).
narrative_ontology:measurement(structured_literacy_reading_be_t2010, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2010, 0.33).
narrative_ontology:measurement_basis(structured_literacy_reading_be_t2010, observed).
narrative_ontology:measurement(structured_literacy_reading_be_t2013, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2013, 0.37).
narrative_ontology:measurement_basis(structured_literacy_reading_be_t2013, observed).
narrative_ontology:measurement(structured_literacy_reading_be_t2018, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2018, 0.43).
narrative_ontology:measurement_basis(structured_literacy_reading_be_t2018, observed).
narrative_ontology:measurement(structured_literacy_reading_be_t2022, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2022, 0.47).
narrative_ontology:measurement_basis(structured_literacy_reading_be_t2022, observed).
narrative_ontology:measurement(structured_literacy_reading_be_t2025, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2025, 0.5).
narrative_ontology:measurement_basis(structured_literacy_reading_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(structured_literacy_reading_su_t1995, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 1995, 0.14).
narrative_ontology:measurement_basis(structured_literacy_reading_su_t1995, observed).
narrative_ontology:measurement(structured_literacy_reading_su_t2000, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2000, 0.17).
narrative_ontology:measurement_basis(structured_literacy_reading_su_t2000, observed).
narrative_ontology:measurement(structured_literacy_reading_su_t2005, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2005, 0.21).
narrative_ontology:measurement_basis(structured_literacy_reading_su_t2005, observed).
narrative_ontology:measurement(structured_literacy_reading_su_t2010, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement_basis(structured_literacy_reading_su_t2010, observed).
narrative_ontology:measurement(structured_literacy_reading_su_t2013, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2013, 0.35).
narrative_ontology:measurement_basis(structured_literacy_reading_su_t2013, observed).
narrative_ontology:measurement(structured_literacy_reading_su_t2018, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2018, 0.47).
narrative_ontology:measurement_basis(structured_literacy_reading_su_t2018, observed).
narrative_ontology:measurement(structured_literacy_reading_su_t2022, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2022, 0.55).
narrative_ontology:measurement_basis(structured_literacy_reading_su_t2022, observed).
narrative_ontology:measurement(structured_literacy_reading_su_t2025, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2025, 0.6).
narrative_ontology:measurement_basis(structured_literacy_reading_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the science of reading' covers four structurally distinct claims about reading acquisition; per the epsilon-invariance principle each is authored as its own story with its own epsilon, beneficiary/victim sets, and type. This file instantiates structured_literacy_reading: the five-pillar, dyslexia-derived, universally-applied regimen with its certification-and-training extraction axis. Its epsilon differs from phonics_reading (decoding-first sequencing, narrower pillar scope, thinner certification apparatus) and inverts whole_language_reading (whose harm claim reverses the victim set). Upstream/downstream: the phonics_reading evidence base is cited as support for this reading's broader claims, so this reading sits downstream of phonics_reading while exerting statutory pressure back on it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
