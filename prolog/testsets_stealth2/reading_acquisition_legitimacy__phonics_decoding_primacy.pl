% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__phonics_decoding_primacy, []).

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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Systematic Phonics Instruction Legitimacy Mandate (Phonics-Decoding Primacy)
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   Since 2013, a legislative wave commonly labeled the science of reading
 *   has rewritten reading-instruction legitimacy across most U.S. states:
 *   statutes define evidence-based instruction as explicit, systematic
 *   phonics; fund or require teacher retraining; mandate universal decoding
 *   screening; restrict cueing-based strategies; and maintain
 *   approved-curriculum lists that channel procurement toward aligned
 *   vendors. This story instantiates the phonics_decoding_primacy reading of
 *   the reading_acquisition_legitimacy kernel: reading is decoding, and
 *   legitimate instruction makes the alphabetic principle explicit. The
 *   sibling readings (whole_language_meaning_primacy,
 *   balanced_literacy_integration, structured_literacy_remediation) are
 *   separate constraints in separate files, not folded into this one. The
 *   colloquial label 'science of reading' merges two structurally distinct
 *   claims — the empirical core (skilled reading rests on grapheme-phoneme
 *   mapping, authored separately as
 *   alphabetic_principle_orthographic_mapping_necessity) and this policy
 *   mandate — decomposed per the epsilon-invariance principle and linked in
 *   the network. The claim and the metrics are authored independently: I
 *   claim tangled_rope because the arrangement pairs a genuine, empirically
 *   supported coordination function with identifiable commercial capture and
 *   enforced displacement of rivals; the metrics describe its actual
 *   operation without being tuned to that claim.
 *
 * KEY AGENTS:
 *   - state_reading_policy_makers: Agenda-setter (institutional/mobile) — writes and enforces the legitimacy rule, funds compliance
 *   - struggling_dyslexic_students: Primary beneficiary (powerless/trapped) — receives the instruction the rule requires
 *   - structured_literacy_publishers: Commercial beneficiary (powerful/arbitrage) — collects procurement channeled by alignment rules
 *   - teacher_training_providers: Commercial beneficiary (organized/mobile) — collects retraining fees scaled to mandate reach
 *   - dyslexia_advocacy_parents: Mobilized beneficiary (organized/constrained) — supplies the political force behind enforcement
 *   - classroom_teachers: Dual-positioned bearer (organized/constrained) — delivers the mandate; gains method, loses discretion
 *   - education_school_faculty: Displaced authority (institutional/identity_locked) — scholarship delegitimized by statute
 *   - proficient_early_readers: Incidental bearer (powerless/trapped) — bears narrowed early texts for little personal gain
 *   - trade_childrens_publishers: Displaced supplier (moderate/mobile) — loses the early-grade channel to decodables
 *   - balanced_literacy_vendors: Excluded rival (powerful/mobile) — barred from approved lists; their exclusion is what enforcement maintains
 *   - literacy_research_community: Analytical observer (institutional/analytical) — produces the evidence both sides cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.62).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.66).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Systematic Phonics Instruction Legitimacy Mandate (Phonics-Decoding Primacy)").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, '9154b183-f7fb-438d-9773-b513c5d2ac38').
narrative_ontology:cs_kernel_codification('9154b183-f7fb-438d-9773-b513c5d2ac38', formalized).
narrative_ontology:cs_authority_grounding('9154b183-f7fb-438d-9773-b513c5d2ac38', expertise).
narrative_ontology:cs_interpretation_layer_present('9154b183-f7fb-438d-9773-b513c5d2ac38').
narrative_ontology:cs_reading_relation('9154b183-f7fb-438d-9773-b513c5d2ac38', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('9154b183-f7fb-438d-9773-b513c5d2ac38', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('9154b183-f7fb-438d-9773-b513c5d2ac38', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('9154b183-f7fb-438d-9773-b513c5d2ac38', foundational, decoding_is_reading_constitutive).
narrative_ontology:cs_axiom_status(decoding_is_reading_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('9154b183-f7fb-438d-9773-b513c5d2ac38', decoding_is_reading_constitutive, empirically_contingent).
narrative_ontology:cs_axiom('9154b183-f7fb-438d-9773-b513c5d2ac38', foundational, explicit_alphabetic_instruction_required_for_legitimacy).
narrative_ontology:cs_axiom_status(explicit_alphabetic_instruction_required_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9154b183-f7fb-438d-9773-b513c5d2ac38', explicit_alphabetic_instruction_required_for_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('9154b183-f7fb-438d-9773-b513c5d2ac38', explicit_systematic_phonics_standard).
narrative_ontology:cs_drift_state('9154b183-f7fb-438d-9773-b513c5d2ac38', contemporary_science_of_reading_wave, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9154b183-f7fb-438d-9773-b513c5d2ac38', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_dyslexic_students).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, teacher_training_providers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, dyslexia_advocacy_parents).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, education_school_faculty).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, proficient_early_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, trade_childrens_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, proficient_early_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and amend the reading statutes: define evidence-based instruction, fund teacher retraining, require universal screening, restrict cueing-based strategies, and maintain approved-curriculum lists. They respond to parent advocacy and vendor testimony; their political exposure rises and falls with visible reading outcomes. Exit is electoral — a shifted coalition can rewrite the rules next session.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, state_reading_policy_makers, agenda_setter,
    institutional, generational, mobile, national).

% Receive explicit, sequenced decoding instruction and early identification through mandated screening. For many, this is the first instruction matched to how they learn; before the statutes, most waited years for help or never received it. They cannot leave the classroom they are assigned to and have no voice in procurement or statute drafting.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_dyslexic_students, beneficiary,
    powerless, biographical, trapped, local).

% Would learn to decode under almost any method. They sit through phonics lessons they did not need and read decodable texts engineered for letter patterns rather than meaning, trading some richness of early reading experience for a foundation they would likely have built anyway.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, proficient_early_readers, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, proficient_early_readers, beneficiary).

% Deliver mandated lessons under fidelity monitoring and pacing guides, after retraining delivered at district expense. Many report that the training finally explained why some students struggle — a professional gain — while others experience scripted delivery as the loss of the judgment that drew them to teaching. Leaving means leaving the profession, not just the mandate.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers, beneficiary).

% Built careers on constructivist and meaning-first pedagogy; accreditation pressure and district hiring expectations now push their graduates toward explicit instruction. Retooling means conceding that decades of their scholarship misled practitioners, and their professional self-concept is bound to the approach the statutes displace.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, education_school_faculty, payer,
    institutional, generational, identity_locked, national).

% Sell the aligned curricula, decodable text series, and screening tools that approved-list rules effectively require. Alignment certification converts statute into sales; when a state revises its list, product lines are re-cut to match. Multi-state portfolios let them absorb any single state's policy reversal.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_publishers, beneficiary,
    powerful, generational, arbitrage, global).

% Deliver the mandated retraining — licensed courses, certified facilitators, per-seat fees. Revenue scales with the number of teachers the statutes reach, and license renewal cycles create recurring income tied to continued enforcement of the training requirement.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, teacher_training_providers, beneficiary,
    organized, generational, mobile, national).

% Organized the legislative campaign and continue to attend hearings and board meetings. They gained screening, services, and public acknowledgment of their children's difficulty; their leverage depends on staying mobilized, since retreating concedes the field to vendor and agency defaults.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, dyslexia_advocacy_parents, beneficiary,
    organized, biographical, constrained, regional).

% Watch decodable series displace picture books and early novels on approved lists and classroom shelves. Their titles fail alignment criteria not for lack of quality but for lack of controlled letter patterns; they can pivot to supplemental and home markets but lose the core early-grade school channel.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, trade_childrens_publishers, payer,
    moderate, biographical, mobile, global).

% Hold catalogues built on leveled texts and cueing-based strategy instruction. Approved-list rules and cueing bans remove their products from consideration in most mandate states; they retain private-school, homeschool, and non-mandate markets while lobbying for broader definitions of evidence.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_vendors, excluded,
    powerful, generational, mobile, national).

% Produces the effect-size estimates, meta-analyses, and critiques that both sides cite. Some members see their findings enacted into law; others argue the mandates outrun the evidence. They bear no budgetary consequence from any outcome and can study any jurisdiction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, literacy_research_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__phonics_decoding_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns reading instruction across classrooms, districts, and states around the alphabetic principle: common instructional sequencing, shared screening instruments, and early identification of decoding failure — solving the problem that instructional quality previously depended on individual teacher luck and struggling readers went unrecognized until the intermediate grades.
% TRANSFER_FUNCTION: Moves public money (curriculum procurement, per-teacher retraining fees, assessment licensing) from school budgets to aligned vendors; moves instructional authority from classroom teachers and education faculties to state statutes and vendor-specified lesson scripts; moves early-grade classroom time toward explicit decoding practice and away from authentic-text immersion.
% ABSENT_VOICES: Balanced-literacy vendors and educators, constructivist education faculty, researchers who contest the strength of the mandate-grade evidence, and children themselves are outside the conversation: procurement rules and hearing witness lists are dominated by dyslexia advocacy organizations and aligned-vendor testimony, so unanimity in favor of the arrangement arises partly because its sharpest critics were never seated.
% DISAPPEARANCE_RATIONALE: If the statutes and their enforcement vanished overnight, approved lists would reopen, cueing-based materials would return in many districts, vendor revenue channels would collapse, education faculties would regain curricular control, and identification of struggling readers would fragment back to local initiative — the instructional economy would reorganize around whichever coalition recaptured each state's definition of evidence.
% FOUNDING_PROBLEM: Widespread reading failure concentrated among vulnerable learners: teachers entered classrooms without knowledge of the alphabetic principle, struggling readers were identified years too late, and instructional quality depended on which teacher a child happened to draw.
% FOUNDING_PROBLEM_CORROBORATION: National outcome series (long-term NAEP trends) and clinical prevalence studies of dyslexia attest the founding problem from outside the benefiting parties. The same outcome series, however, show stagnation both before and after successive mandate waves, so external data corroborate that the problem is live without confirming that the arrangement as built resolves it; vendor and training-provider attestation is excluded as self-interested.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62 at interval end) reflects the arrangement's actual operation: alignment rules convert statute into vendor revenue, retraining fees scale with mandate reach, and teachers and education faculties bear autonomy and authority costs that serve those transfers rather than instruction. Suppression (0.66) is authored as a raw structural property — cueing bans, approved-list exclusions, fidelity monitoring, certification pressure — and is deliberately left unscaled; only extractiveness is scaled by the engine, through directionality and scope. Theater (0.33) is low-to-moderate: screening, retraining, and curriculum replacement do what they claim, but a growing share of activity is compliance performance — fidelity walkthroughs, alignment paperwork, implementation dashboards — whose share rises as the easy instructional wins are banked. Accessibility collapse (0.50): alternatives survive in private, homeschool, and non-mandate markets and in classroom practice, but are closed off inside governed procurement. Resistance (0.58): teacher organizations, displaced faculty, contesting researchers, and excluded vendors mount real, ongoing opposition. All three tracked series run on one shared seven-point grid (2013-2025) so no metric borrows another's end-state value. The longer history is pendular — phonics mid-century, whole language in the 1980s-90s, balanced compromise in the 2000s, this revival since 2013 — and the interval captures one ascending phase of that cycle; the pendulum_cycle_driver omega asks whether the cycle itself is a market mechanism rather than an evidentiary one.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From struggling_dyslexic_students and dyslexia_advocacy_parents, the arrangement is the thing that finally taught them to read — near-pure coordination. From structured_literacy_publishers and teacher_training_providers, the same statutes are a protected revenue channel: they bear none of the costs and experience every expansion as gain. From classroom_teachers, the arrangement arrives as both method and script — a genuine professional gain wrapped in fidelity monitoring. From education_school_faculty, it is the delegitimation of a life's scholarly work — experienced as pure imposition. Inter-institutionally, the legislature spends money it raises elsewhere, vendors collect it, districts pass it through, and faculties absorb the reputational cost: four institutions, one flow, four different experiences. Same-level laterals: classroom_teachers and education_school_faculty occupy the same professional stratum but differ sharply in exit — teachers are constrained (certification binds them to employment, not to doctrine) while faculty are identity_locked (their self-concept is the displaced doctrine), so identical statutory pressure lands as inconvenience on one seat and existential threat on the other. Likewise structured_literacy_publishers and trade_childrens_publishers share an industry and differ mainly in product alignment: the rule sorts winners and losers inside a single market by fit to the mandated method, not by size or quality.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map toward the low-directionality end: struggling_dyslexic_students receive the subsidy side outright, dyslexia_advocacy_parents collect recognition and services, and the two vendor seats collect revenue without bearing mandate costs. Victims map high: education_school_faculty sit nearest the full-target end (identity_locked exit amplifies their exposure), classroom_teachers derive mid-high (their secondary beneficiary role weights the method gain against the autonomy loss), proficient_early_readers bear costs with little personal offset, and trade_childrens_publishers lose the channel. state_reading_policy_makers sit near symmetric: they spend funds and political capital and collect electoral alignment with a mobilized constituency. No directionality overrides are authored: the beneficiary/victim declarations plus exit options distinguish every seat the derivation needs to distinguish, and the two dual-positioned seats carry secondary_role so both sides of their position enter the derivation. The vindicated propositions (alphabetic_principle_centrality, orthographic_mapping_theory, national_reading_panel_synthesis) are listed separately — they collect no rents and are not beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — vulnerable readers failed by implicit instruction and identified too late — remains live, corroborated by outcome series and clinical prevalence data from outside the benefiting parties. The mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no zombie signature: the arrangement still does what it was built for. The warning sits in the theater trajectory (0.15 rising to 0.33): as screening and retraining bank their early wins, compliance artifacts grow faster than outcome movement, which is the early shape of a mandate drifting toward performance. If outcome series flatten while fidelity metrics keep climbing, the arrangement will be maintaining itself on its own paperwork, and mandatrophy resolution becomes live. Classification discipline cuts both ways: naming the vendor capture keeps this from being mislabeled pure coordination, and naming the genuine decoding gains for struggling readers keeps it from being mislabeled pure extraction. The tangled-rope claim is what remains when both mislabels are refused.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'This constraint is one reading of the reading_acquisition_legitimacy kernel; would instantiating a sibling reading (whole_language_meaning_primacy, balanced_literacy_integration, structured_literacy_remediation) relocate the victim and beneficiary sets rather than merely relabeling them?',
    'Author the sibling stories and compare computed seat classifications across the kernel; divergence in victim sets (not just epsilon values) confirms the readings instantiate different constraints.',
    'Under whole_language_meaning_primacy, decodable-narrowed students and script-bound teachers swap positions with immersion-deprived struggling readers; the same classrooms classify oppositely depending on which reading governs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Committer structure: which reading of the legitimacy kernel governs, and what each relocation of victims implies.').

omega_variable(
    science_core_vs_mandate_wrapper,
    'Is the measured burden attributable to the cognitive-science core (explicit phonics instruction works) or to the policy wrapper (exclusive procurement lists, retraining contracts, cueing bans)?',
    'Compare jurisdictions that adopted explicit phonics instruction without exclusive-alignment procurement; if burdens track the wrapper rather than the instruction, the core decomposes into a separate near-fixed constraint.',
    'If separable, the science core certifies with negligible burden and only the mandate carries the measured asymmetry; this story''s epsilon drops accordingly and the family splits cleanly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(science_core_vs_mandate_wrapper, empirical, 'Whether the arrangement''s burden lives in the instruction or in the enforcement-and-procurement machinery around it.').

omega_variable(
    struggling_reader_benefit_attribution,
    'How much of the documented gain for struggling readers comes from explicit decoding instruction specifically, versus universal screening, early identification, or increased instructional attention generally?',
    'Meta-analyses and trials with active controls that isolate instructional explicitness from screening and dosage effects.',
    'Determines the size of the genuine coordination function; a small attributed share would shift the balance from coordination-with-burdens toward burden-with-coordination-cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(struggling_reader_benefit_attribution, empirical, 'Attribution of the arrangement''s headline benefit to its specific mechanism.').

omega_variable(
    decodable_text_diet_effects,
    'Does an early reading diet dominated by decodable texts measurably reduce vocabulary growth, background-knowledge building, or reading motivation relative to richer text exposure?',
    'Longitudinal cohorts comparing text-exposure profiles with decoding outcomes held constant.',
    'If harms are real, proficient_early_readers'' burden deepens and the arrangement''s overall burden rises; if not, their payer position is largely nominal and the victim set narrows.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decodable_text_diet_effects, empirical, 'Whether the opportunity cost borne by already-proficient readers is substantively real.').

omega_variable(
    teacher_suppression_mechanism_split,
    'Is the pressure on classroom teachers predominantly structural (statute, procurement rules, certification consequences) or internalized (professional norms reframed so deviation from the script feels like malpractice)?',
    'Post-mandate teacher surveys and attrition trajectories: if reported pressure persists where enforcement lapses, the internalized share is substantial.',
    'Internalized pressure raises the effective burden teachers carry beyond the statutory measure and slows any future relaxation of the mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_suppression_mechanism_split, empirical, 'Structural versus internalized split of the pressure bearing on the teacher seat.').

omega_variable(
    pendulum_cycle_driver,
    'Is the historical phonics-to-whole-language-to-balanced-to-phonics oscillation driven by accumulating evidence, or by market and career cycles that periodically invalidate incumbent curricula?',
    'Compare swing timing against publication of decisive evidence versus curriculum adoption cycles and publisher turnover data.',
    'If the cycle is market-driven, each swing functions as planned obsolescence for pedagogy — a recurring transfer mechanism that no single-interval measurement captures and that would raise the arrangement''s lifetime burden substantially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pendulum_cycle_driver, empirical, 'Whether the multi-decade instructional pendulum is evidentiary or commercial in origin.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 2013, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t2013, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2013, 0.15).
narrative_ontology:measurement(read_tr_t2015, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(read_tr_t2017, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2017, 0.2).
narrative_ontology:measurement(read_tr_t2019, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2019, 0.24).
narrative_ontology:measurement(read_tr_t2021, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2021, 0.27).
narrative_ontology:measurement(read_tr_t2023, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2023, 0.3).
narrative_ontology:measurement(read_tr_t2025, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2025, 0.33).

% Extraction over time
narrative_ontology:measurement(read_be_t2013, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2013, 0.38).
narrative_ontology:measurement(read_be_t2015, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(read_be_t2017, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2017, 0.46).
narrative_ontology:measurement(read_be_t2019, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement(read_be_t2021, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2021, 0.54).
narrative_ontology:measurement(read_be_t2023, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2023, 0.58).
narrative_ontology:measurement(read_be_t2025, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t2013, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2013, 0.4).
narrative_ontology:measurement(read_su_t2015, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2015, 0.46).
narrative_ontology:measurement(read_su_t2017, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2017, 0.52).
narrative_ontology:measurement(read_su_t2019, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2019, 0.57).
narrative_ontology:measurement(read_su_t2021, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2021, 0.61).
narrative_ontology:measurement(read_su_t2023, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2023, 0.64).
narrative_ontology:measurement(read_su_t2025, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2025, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, alphabetic_principle_orthographic_mapping_necessity).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle. The colloquial label 'science of reading' conflates two structurally distinct claims: (1) the empirical core — skilled reading depends on grapheme-phoneme mapping and orthographic mapping — which is highly replicated, uncontested in its essentials, and carries negligible burden (authored separately as alphabetic_principle_orthographic_mapping_necessity, the upstream member of this family); and (2) the policy mandate that legitimate instruction must be explicit systematic phonics, which is contested in degree, actively enforced, and burden-bearing (this story). The upstream claim is cited as warrant for the downstream mandate, so contamination propagates downstream: challenges to mandate overshoot are routinely misread as challenges to the science. Kernel-family links to the three sibling readings are also declared, since this reading's ascendance structurally reshapes the environments in which the siblings operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
