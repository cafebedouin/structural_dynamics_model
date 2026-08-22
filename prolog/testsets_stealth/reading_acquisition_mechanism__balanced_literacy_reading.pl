% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__balanced_literacy_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__balanced_literacy_reading
 *   human_readable: Balanced Literacy Doctrine: Integrated Phonics and Authentic Text Requirement
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   Between roughly 1995 and 2025, English-speaking elementary reading
 *   instruction consolidated around a doctrine promising integration:
 *   explicit phonics AND authentic literature, taught together. The doctrine
 *   settled a genuinely unwinnable institutional conflict (the reading wars)
 *   and gave districts, publishers, and teacher-preparation programs a common
 *   framework. Its commercial instantiations, however, rationed the phonics
 *   component to brief embedded mini-lessons while permitting
 *   meaning-guessing strategies that cognitive science identifies as
 *   counterproductive for the weakest decoders; adoption rents flowed to a
 *   small publisher-consultant complex while costs concentrated in struggling
 *   readers, disproportionately students with dyslexia. Claim and metrics are
 *   independent authored facts: the constraint is CLAIMED here as
 *   tangled_rope because the coordination function is real (war settlement,
 *   practice standardization, genuine integration value for typical readers)
 *   while the metrics describe materially extractive, increasingly theatrical
 *   operation. The engine measures the divergence per seat; nothing here
 *   reconciles claim to metrics.
 *
 * KEY AGENTS:
 *   - curriculum_publishers: primary agenda-setter and receipt seat (institutional/arbitrage) — authors the materials, collects the adoption revenue, can pivot product lines when regulation shifts
 *   - students_with_dyslexia and struggling_readers: primary targets (powerless/trapped) — bear the concentrated cost of rationed explicit instruction
 *   - classroom_teachers: delivery agents bearing diffuse costs (organized/constrained) — implement, absorb blame, lack curriculum-design voice
 *   - district_administrators: enforcing beneficiaries (powerful/constrained) — locked in by their own adoption history
 *   - teacher_preparation_faculties: identity-locked beneficiaries (institutional) — professional selves fused with the framework's premises
 *   - families_of_struggling_readers: excluded payers (moderate/constrained) — buy privately what the arrangement rations
 *   - reading_scientists: analytical observer (institutional/analytical) — produce the evidence base from outside the adoption loop
 *   - state_education_agencies: late-arriving agenda-setter (institutional/constrained) — held authority, deferred for most of the interval, now legislating
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.62).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.62).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Doctrine: Integrated Phonics and Authentic Text Requirement").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, 'ab9a1290-646b-4197-8052-366272e173c0').
narrative_ontology:cs_kernel_codification('ab9a1290-646b-4197-8052-366272e173c0', formalized).
narrative_ontology:cs_authority_grounding('ab9a1290-646b-4197-8052-366272e173c0', lineage).
narrative_ontology:cs_interpretation_layer_present('ab9a1290-646b-4197-8052-366272e173c0').
narrative_ontology:cs_reading_relation('ab9a1290-646b-4197-8052-366272e173c0', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab9a1290-646b-4197-8052-366272e173c0', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_axiom('ab9a1290-646b-4197-8052-366272e173c0', foundational, integrated_code_and_meaning_necessity).
narrative_ontology:cs_axiom_status(integrated_code_and_meaning_necessity, holdable).
narrative_ontology:cs_axiom_grounding('ab9a1290-646b-4197-8052-366272e173c0', integrated_code_and_meaning_necessity, empirically_contingent).
narrative_ontology:cs_axiom('ab9a1290-646b-4197-8052-366272e173c0', foundational, contextualized_phonics_sufficiency).
narrative_ontology:cs_axiom_status(contextualized_phonics_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('ab9a1290-646b-4197-8052-366272e173c0', contextualized_phonics_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('ab9a1290-646b-4197-8052-366272e173c0', constructivist_integration_settlement).
narrative_ontology:cs_drift_state('ab9a1290-646b-4197-8052-366272e173c0', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ab9a1290-646b-4197-8052-366272e173c0', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, literacy_pd_consultants).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, teacher_preparation_faculties).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, district_administrators).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, students_with_dyslexia).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, families_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and sell the flagship curricula (lesson continua, unit calendars, leveled book libraries) that operationalize the integrated approach; fund conference presence, author speaking circuits, and aligned assessments; revenue depends on continued district adoption and consumable replenishment. When regulation shifts, they can repackage product lines for the new market, as several houses began doing once state statutes demanded systematic phonics strands.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_publishers, agenda_setter,
    institutional, generational, arbitrage, global).

% Sell coaching days, summer institutes, and implementation audits to districts running the curricula; income recurs only as long as implementation continues, giving a livelihood stake in the arrangement's continuation. Exit means retraining into a different pedagogical product line.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, literacy_pd_consultants, beneficiary,
    organized, biographical, mobile, national).

% Teach licensure candidates within frameworks descended from constructivist literacy theory; syllabi, dissertations, and scholarly reputations are invested in the approach's premises. Switching traditions means recanting published work and rebuilding courses, so departure is professionally costly even where private doubt exists.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, teacher_preparation_faculties, beneficiary,
    institutional, generational, identity_locked, national).

% Adopted the compromise to end a locally unwinnable curriculum conflict and now enforce its materials through pacing guides and classroom walkthrough instruments. Reversal requires tendering new curricula, retraining every teacher, and publicly conceding a decade of adoption decisions, so staying is administratively cheaper than fixing.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, district_administrators, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, district_administrators, agenda_setter).

% Deliver daily instruction inside the mandated framework: mini-lessons, guided reading rounds, running records. Many privately observe that their weakest decoders stall, but pacing guides, evaluation instruments, and available materials leave little sanctioned room to substitute explicit routines; union representation gives voice on workload, not on curriculum design.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers, payer,
    organized, biographical, constrained, local).

% Children in the weaker part of the distribution for whom meaning-predictive strategies do not bootstrap decoding; they spend primary years guessing from pictures and context, fall further behind each year, and cannot leave compulsory schooling. Remediation arrives late, if at all, and is paid for in self-concept as much as in skill.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Roughly one child in ten needs explicit, systematic grapheme-phoneme instruction delivered with fidelity; the arrangement rations exactly that component. They are disproportionately assessed as inattentive or unmotivated, referred late to special education, and their families carry the cost of private diagnosis and tutoring.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, students_with_dyslexia, payer,
    powerless, biographical, trapped, local).

% Pay for private evaluations, tutors, and advocacy campaigns; historically absent from curriculum adoption committees, they entered the conversation only by organizing externally and litigating. Their exit option is leaving public schooling entirely, which few households can afford.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, families_of_struggling_readers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, families_of_struggling_readers, excluded).

% Cognitive psychologists and education researchers studying word recognition, working outside the adoption loop; they produce the eye-tracking, longitudinal, and intervention evidence that bears on the arrangement's premises but hold no seat in district procurement.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, reading_scientists, observer,
    institutional, generational, analytical, global).

% Hold statutory authority over curriculum standards but deferred to district adoption and the publisher-consultant ecosystem for most of the interval; entered the agenda-setting seat late, as science-of-reading statutes began mandating systematic phonics and barring cueing-based practices.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, state_education_agencies, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settled the reading wars by giving districts an adoptable middle position; standardized instructional practice across classrooms through shared continua, leveling systems, and unit structures; preserved literature engagement alongside skills instruction so teachers had one framework instead of two warring ones.
% TRANSFER_FUNCTION: Moves instructional minutes toward meaning-predictive strategies and away from explicit decoding rehearsal; moves district curriculum and professional-development budgets from taxpayers to publishers and consulting firms; moves the downstream cost of unfinished literacy acquisition onto struggling students, their families, and later employers.
% ABSENT_VOICES: Struggling readers themselves, who cannot testify in adoption hearings; reading scientists outside the publisher-consultant network, who were not seated on curriculum committees; parents of dyslexic children, excluded until they organized externally; special-education staff whose caseloads absorb the failures, consulted late.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, districts would re-tender curricula immediately, publishers would lose flagship revenue lines, teacher-preparation syllabi would rewrite within accreditation cycles, and hundreds of thousands of classrooms would reorganize around whichever framework replaced it; the reorganization currently underway under state science-of-reading statutes previews the shape of that rearrangement.
% FOUNDING_PROBLEM: After California's whole-language experiment produced measurable literacy declines and the phonics counter-movement gained force, districts faced an unwinnable adoption conflict: two camps, incompatible curricula, and no politically survivable middle. Balanced literacy was built to end that war and hand schools a defensible synthesis.
% FOUNDING_PROBLEM_CORROBORATION: The National Reading Panel (2000) and subsequent cognitive-science syntheses attest, from outside the benefiting parties, that the decoding question the compromise papered over had an empirical answer the arrangement declined to implement; state legislative findings in dozens of jurisdictions (2019 onward) attest the arrangement outlived its evidentiary warrant; publisher catalogues and professional-development marketing attest the opposite. No corroborating source inside the arrangement's beneficiary set disputes the shifted-function reading.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62 reflects a transfer that is modest for typical readers but severe and concentrated for the weakest decoders, plus curriculum budgets routed to publishers at margins far above production cost. Suppression 0.62 is structural: multi-year adoption contracts, pacing guides, evaluation instruments, and professional-development pipelines constrain teacher practice; suppression is a raw structural property and is NOT scaled by power or scope — the engine scales only extractiveness. Theater 0.48: running records, leveling rituals, and 'balance' branding consume a growing share of activity as the evidence gap widened; roughly half of maintenance effort defends the compromise rather than teaches. Accessibility_collapse 0.52: alternatives (explicit programs, tutoring, private schooling) existed throughout but sat outside most families' procurement reach. Resistance 0.70: parent coalitions, reading scientists, investigative journalism, and eventually dozens of state statutes mounted sustained opposition. All three series run on one shared grid (t=0 to 30, step 5) so no metric is ever sampled against another's scalar; trajectories rise monotonically — extraction accumulates as rents layer onto the settlement, enforcement hardens as dissent grows — with no oscillation, so no cyclical machinery is invoked. Rising base_extractiveness is the accumulation signature the T17 abductive trigger watches; it fires as hypothesis, not reclassification.
 *
 * PERSPECTIVAL GAP:
 *   From the publisher seat the arrangement is a professional framework it built, defends, and profits from legitimately; from the dyslexic-student seat the same structure operates as a rationing device that withheld the one instruction type they required; from the classroom-teacher seat it is a loyalty bind — trained to believe in the framework, observing daily that their weakest readers stall, and evaluated on implementing it anyway. The engine computes these divergent per-seat classifications from power, exit, and directionality data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers, consultants, preparation faculties, and administrators sit near the beneficiary end: the declaration set marks them as collectors, and their exits (arbitrage for publishers, mobility for consultants) dampen effective extraction further. Classroom teachers are declared victims but remain salaried participants with union voice, placing them high-mid rather than full target. Struggling readers and students with dyslexia are full targets: powerless, trapped in compulsory schooling, identity-forming around failure — the derivation pushes them to the target extreme, where scope amplification bites hardest because the arrangement operates at national scale. Families sit high as paying outsiders. Reading scientists are analytical and collect nothing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ending an unwinnable adoption war — was solved by roughly the mid-2000s: the wars ended and the compromise became orthodoxy. What persisted afterward was maintained by publisher rents, sunk training investments, and administrators' reluctance to concede error, not by the founding problem; hence mandatrophy_resolved is declared true while founding_problem_status stays contested, because the arrangement's defenders assert a continuing integration mission its critics deny. The tangled_rope classification prevents mislabeling in both directions: calling the arrangement a pure snare would erase the real coordination service (war settlement, standardization, genuine literature engagement) that millions of typical readers received; calling it a pure rope would erase the concentrated casualties among the weakest decoders. Identity-lock dynamics concentrate in the preparation faculties, whose fusion is professional and scholarly — recantation costs, not switching costs, hold them; if that frame broke, the beneficiary coalition would fragment quickly. The receipt surface records that gains land on a named seat (publishers) and that fixing is prohibitive for the seats with authority to fix it; a captured cell stays captured under either cost class, so the prohibitive rating sharpens rather than softens the extraction reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates one reading (balanced_literacy_reading) of the kernel reading_acquisition_mechanism; what would change structurally if a sibling reading (phonics_reading or whole_language_reading) were adopted as the operative constraint?',
    'Comparative classification of the sibling stories: phonics_reading carries a low-extraction, high-evidential-support profile; whole_language_reading carries a high-extraction profile; the deltas locate which structural elements (systematicity requirement, literature centrality, cueing permission) drive extraction.',
    'Adopting the phonics_reading constraint would dissolve this reading''s beneficiary structure, since publisher rents depend on the integrated framing; adopting whole_language_reading would remove even the nominal phonics requirement and push suppression higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame placement: one reading of a three-reading kernel.').

omega_variable(
    design_vs_implementation_gap,
    'Is the measured extraction a property of balanced literacy as designed (genuine integration of systematic phonics with rich literature) or of balanced literacy as implemented (brief embedded mini-lessons, cueing-permitted texts)?',
    'Randomized trials of faithfully implemented integrated programs with systematic phonics components at adequate dosage, compared against the commercial implementations districts actually purchased.',
    'If faithful implementation shows low extraction, epsilon attaches to implementation failure and the arrangement could compute nearer rope; if faithful implementations were never commercially offered, the design itself is the extraction vehicle and the current classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(design_vs_implementation_gap, empirical, 'Whether epsilon belongs to the doctrine or to its commercial instantiation.').

omega_variable(
    cueing_component_status,
    'Is the three-cueing strategy (meaning-syntax-visual guessing) a load-bearing component of the arrangement''s coordination function or removable accretion?',
    'Natural experiment from jurisdictions that statutorily barred cueing while retaining integrated curricula: if engagement and comprehension outcomes hold while cueing disappears, it was accretion; if they degrade, it was load-bearing.',
    'If accretion, the coordination function survives reform and extraction was concentrated in one removable practice; if load-bearing, the integration premise weakens and this reading collapses toward whole_language_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cueing_component_status, empirical, 'Load-bearing status of the cueing system inside the integrated practice.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of dissenting practice structural (multi-year contracts, pacing mandates, evaluation instruments) or internalized (teachers trained to believe explicit decoding harms comprehension)?',
    'Post-mandate trajectory: in states where statutes lifted the structural barriers, track whether classroom practice converges on explicit routines or persists in balanced routines; persistent divergence indicates internalized suppression.',
    'If substantially internalized, effective suppression outlasts the mandates that produced it and reform timelines lengthen by a teacher-generation; if structural, repeal produces rapid practice change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression between external barriers and trained belief.').

omega_variable(
    sincerity_of_phonics_requirement,
    'Is this reading''s ''requires explicit phonics'' clause a sincere structural commitment or rhetorical cover allowing whole-language practice to continue under a new name?',
    'Material analysis of adopted curricula: dosage, sequence, and assessment of the phonics strands versus marketing claims; implementation audits of scheduled phonics minutes.',
    'If rhetorical, this reading and whole_language_reading converge in practice, the foreclosure edge softens into convergence, and the arrangement is better read as whole-language-with-cover; if sincere but under-dosed, the reading is a genuine compromise executed badly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincerity_of_phonics_requirement, conceptual, 'Whether the phonics half of the integration claim is operative or ornamental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(read_be_t5, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(read_be_t15, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(read_be_t25, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(read_be_t30, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(read_su_t5, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(read_su_t10, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(read_su_t15, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(read_su_t25, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement(read_su_t30, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, whole_language_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how children learn to read' decomposes into three structurally distinct constraints sharing one kernel: phonics_reading (low epsilon, strong evidential base, negligible extraction within its frame), whole_language_reading (high epsilon, extraction via withheld instruction), and this balanced_literacy_reading (intermediate epsilon, compromise-shaped extraction: coordination for the median reader, rationing for the weakest decoders, rents for the publisher-consultant complex). The phonics evidence base is upstream: the balanced reading cites it for legitimacy while its commercial implementations under-deliver it, so contamination flows from this story into both siblings' operating environments. Each file carries its own stable epsilon; none hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
