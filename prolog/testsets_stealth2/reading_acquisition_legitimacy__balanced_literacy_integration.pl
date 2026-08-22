% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__balanced_literacy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__balanced_literacy_integration, []).

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
 *   constraint_id: reading_acquisition_legitimacy__balanced_literacy_integration
 *   human_readable: Balanced Literacy Integration Norm
 *   domain: education_policy/cognitive_science
 *
 * SUMMARY:
 *   Since the late 1990s, the governing norm for early reading instruction in
 *   most Anglophone school systems has been the balanced settlement: reading
 *   is held to require both accurate decoding and meaning-making, and
 *   instruction is deemed legitimate when it allocates substantially to each
 *   — explicit phonics lessons alongside authentic literature, direct
 *   teaching toggled with facilitation, and dual-track support (phonics
 *   intervention plus guided reading) for struggling readers. The norm solved
 *   a real factional war, and it also created an indeterminate allocation
 *   question ('how much of each?') on which no adjudicating procedure exists.
 *   Around that indeterminacy a commercial and professional complex has
 *   accumulated: bundled curricula, perpetual professional development, and
 *   an enforcement apparatus of adoption committees, walkthrough rubrics, and
 *   PD mandates. The costs concentrate on the learners who needed the most
 *   explicit instruction and got the median mix. KEY AGENTS (by structural
 *   relationship): - balanced_curriculum_publishers: Primary beneficiary
 *   (institutional/arbitrage) — collects adoption revenue from the
 *   bundled-program market the norm sustains - literacy_pd_providers:
 *   Beneficiary (organized/mobile) — sells guidance on an allocation the norm
 *   leaves permanently undefined - teacher_preparation_faculties: Beneficiary
 *   (institutional/identity_locked) — pipeline and scholarship invested in
 *   the framework - district_administrators: Agenda setter with secondary
 *   benefit (institutional/constrained) — administers adoption, rubrics, and
 *   PD; buys factional peace - struggling_readers: Primary target
 *   (powerless/trapped) — bear diluted instruction during a finite
 *   developmental window - classroom_teachers: Target with secondary benefit
 *   (moderate/constrained) — deliver the prescribed mix, absorb two-front
 *   criticism - dyslexia_parent_advocates: Excluded voice
 *   (organized/constrained) — historically outside adoption decisions -
 *   reading_scientists: Analytical observer (institutional/analytical) —
 *   effect-size syntheses pressing from outside Claim and metrics are
 *   independent authored facts: claimed_type reflects the structural reading
 *   (a genuine coordination settlement carrying asymmetric cost-bearing under
 *   active enforcement); the metrics describe observed operation. The engine
 *   computes per-seat types from the structural data; divergence between
 *   claim and computation is signal, not error. This file instantiates ONE
 *   reading of the reading_acquisition_legitimacy kernel — see
 *   commentary.kernel_context and the committer omega; epsilon is authored
 *   for the standing balanced arrangement as this reading's own lights see
 *   it, never for any sibling's endorsed alternative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.58).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.58).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration Norm").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education_policy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, 'ce6519fc-44db-4346-ab35-85d3787335b4').
narrative_ontology:cs_kernel_codification('ce6519fc-44db-4346-ab35-85d3787335b4', distributed).
narrative_ontology:cs_authority_grounding('ce6519fc-44db-4346-ab35-85d3787335b4', expertise).
narrative_ontology:cs_interpretation_layer_present('ce6519fc-44db-4346-ab35-85d3787335b4').
narrative_ontology:cs_reading_relation('ce6519fc-44db-4346-ab35-85d3787335b4', reading_acquisition_legitimacy__phonics_decoding_primacy, forecloses).
narrative_ontology:cs_reading_relation('ce6519fc-44db-4346-ab35-85d3787335b4', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_axiom('ce6519fc-44db-4346-ab35-85d3787335b4', foundational, reading_requires_dual_components).
narrative_ontology:cs_axiom_status(reading_requires_dual_components, holdable).
narrative_ontology:cs_axiom_grounding('ce6519fc-44db-4346-ab35-85d3787335b4', reading_requires_dual_components, empirically_contingent).
narrative_ontology:cs_axiom('ce6519fc-44db-4346-ab35-85d3787335b4', foundational, instructional_balance_necessity).
narrative_ontology:cs_axiom_status(instructional_balance_necessity, holdable).
narrative_ontology:cs_axiom_grounding('ce6519fc-44db-4346-ab35-85d3787335b4', instructional_balance_necessity, instrumental).
narrative_ontology:cs_reference_frame('ce6519fc-44db-4346-ab35-85d3787335b4', integrated_dual_component_instruction).
narrative_ontology:cs_drift_state('ce6519fc-44db-4346-ab35-85d3787335b4', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ce6519fc-44db-4346-ab35-85d3787335b4', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_pd_providers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_preparation_faculties).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, district_administrators).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, dual_component_reading_model).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, instructional_pragmatism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish K-2 literacy programs that pair decodable text sets with leveled literature collections, teacher guides, and assessment kits, sold as bundles through district adoption. Adoption happens at district scale on multi-year renewal cycles, making them the largest single recipients of classroom literacy spending. When the surrounding debate shifts vocabulary, they repackage the same architecture under new labels — the whole-language series of the 1990s returned as balanced programs, and the same houses now market structured strands alongside existing catalogs — without giving up the bundle model.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_curriculum_publishers, beneficiary,
    institutional, generational, arbitrage, continental).

% Sell workshops, coaching contracts, and micro-credentials on putting the two components together in one classroom. Because the governing norm names no fixed ratio between explicit code lessons and literature work, demand for guidance on getting the mix right regenerates with every adoption cycle and every new cohort of teachers. Offerings can be re-themed quickly to whichever terminology is ascendant in a given state.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_pd_providers, beneficiary,
    organized, biographical, mobile, national).

% Prepare and certify the teacher pipeline. Methods courses, practicum supervision, dissertations, and faculty careers are built around constructivist and meaning-first pedagogy with skills instruction folded in. A wholesale move to explicit-instruction-first course architecture would strand decades of scholarly output and unsettle the field's self-understanding, so program revisions tend to add modules rather than replace foundations.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_preparation_faculties, beneficiary,
    institutional, generational, identity_locked, national).

% Run the adoption committees, sign the multi-year material contracts, set the walkthrough rubrics and the professional-development calendar. Choosing the middle position avoids open conflict with either parent faction and gives board presentations a defensible story. They carry the political cost when reading scores disappoint, and signed contracts plus retraining timelines make mid-course correction slow and expensive.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, district_administrators, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, district_administrators, beneficiary).

% Children whose word recognition develops slowly need the most explicit, intensive, and cumulative code instruction available, delivered early while the developmental window is open. Under an allocation set by tradition and adopted materials rather than by diagnosis, they receive short embedded skills lessons alongside activities pitched to the median learner. They cannot leave the classroom, and whether they get outside tutoring depends entirely on family resources.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers, payer,
    powerless, biographical, trapped, national).

% Deliver both components daily under time pressure, using materials they did not select, and absorb criticism from skills-first advocates and literature-first loyalists at the same time. The framework gives them collegial consensus, ready-made lessons, and a shared professional vocabulary; what they cannot do is change the allocation the adopted program prescribes or opt out of either component.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers, beneficiary).

% Organize after a child fails to learn to read under the prevailing mix, pressing legislatures for screening mandates and explicit-code requirements. Historically absent from adoption committees and standards panels, which were staffed by educators trained inside the framework and advised by its commercial partners; increasingly heard in statehouses, still rarely seated where materials are chosen.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, dyslexia_parent_advocates, excluded,
    organized, biographical, constrained, national).

% Publish syntheses of effect sizes on phonemic awareness, phonics, fluency, and comprehension instruction; testify in hearings; advise legislators. Their findings press on the arrangement from outside it; they run no classrooms and control no adoptions.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, reading_scientists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__balanced_literacy_integration, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles a two-faction conflict over instructional legitimacy with a framework both camps can inhabit; keeps explicit code instruction and rich literature work simultaneously present in classrooms; gives districts a procurement category and teachers a shared professional vocabulary; dampens pendulum disruption across administrative turnovers.
% TRANSFER_FUNCTION: Moves instructional minutes — the scarcest classroom resource — between explicit code lessons and literature work according to adopted-program defaults rather than individual diagnosis; moves adoption dollars from districts to publishers and professional-development firms; moves struggling readers' limited instructional time away from the intensity their profiles call for toward the median allocation.
% ABSENT_VOICES: Dyslexia parent advocates and reading scientists were absent from the tables where materials and rubrics were chosen; struggling readers are present only as outcome statistics. Adoption committees drew members from educators trained inside the framework and advisors employed by its suppliers, so unanimity about the mix reflected who was in the room as much as what the evidence said.
% DISAPPEARANCE_RATIONALE: Districts would immediately have to pick a successor rule — explicit-code-first, immersion-first, or vulnerability-first design — publishers would re-bundle around the winner, preparation programs would revise course architecture, and the open factional war the settlement froze would resume in public. Instructional minutes would be reallocated, and the population of struggling readers would face a different regime within one adoption cycle.
% FOUNDING_PROBLEM: By the mid-1990s the phonics-versus-whole-language fight had made reading instruction a proxy war: districts lurched between mandates with each administrative turnover, teachers were whipsawed between incompatible directives, and both camps claimed the moral high ground while children in the middle received whatever the latest pendulum swing left behind. The settlement was built to end the war without requiring either side to concede it had been wrong.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the National Reading Panel (2000) and subsequent meta-analytic syntheses attested that the underlying instructional questions remained unanswered through the settlement's first decade; legislative findings accompanying the recent science-of-reading statute wave attest that the settlement failed vulnerable learners at scale; historians of education document the pre-settlement pendulum independently of any faction. No corroborating source attests the war is fully over; several attest it resumed in legislative form.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.58: the settlement genuinely delivers both components (real coordination value), but a rent layer has accumulated on the undefined ratio — bundle pricing, perpetual PD demand, rebranding cycles — and the cost of the median allocation concentrates on learners needing maximum intensity. Suppression is 0.58 and is authored as a raw structural property (adoption mandates, evaluation rubrics, PD requirements, contractual lock-in); it is deliberately NOT scaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater_ratio ends at 0.48: a growing share of activity defends the appearance of balance (mission-statement language, minute-counting walkthroughs, 'we already teach phonics' compliance displays) rather than performing calibration; it approaches but does not cross the Goodhart line. Accessibility_collapse is 0.40: the poles remain reachable — schools and states have moved to full explicit-code architectures — but doing so carries stigma, contractual friction, and professional isolation, so alternatives are degraded rather than eliminated. Resistance is 0.60: the arrangement fights a two-front war against science-of-reading advocates and whole-language loyalists simultaneously. Boltzmann: coordination_type is identity_coordination — the norm's dominant function is maintaining the profession's consensus on what counts as legitimate instruction (boundary maintenance and membership claims), and its failure mode is the return of open factional war, not the loss of a physical infrastructure. The FNL gaming risk is acknowledged: 'balanced' functions partly as professional identity cover, and identity_coordination's modest complexity offset must not excuse the Power x Scope coupling that concentrates costs on powerless learners at national scale. Temporal data run on ONE shared grid (t = 0,5,10,15,20,25,30, mapping 1996-2026) with all three metrics authored at every point: base_extractiveness accumulates monotonically (rent layering on top of unchanged coordination), suppression_requirement hardens slowly (enforcement maturing into compliance culture, slight easing at the end as statutes force accommodation), and theater_ratio climbs steadily toward the substitution threshold. No cyclical dynamics are claimed; the drift is monotonic.
 *
 * PERSPECTIVAL GAP:
 *   From the publisher seat the arrangement is a market structure that rewards bundling; from the faculty seat it is a professional identity two generations deep; from the administrator seat a peace treaty with contractual teeth; from the teacher seat a workload with two bosses and no allocation authority; from the struggling reader's seat it is the difference between learning to read and not. Same norm, same classroom hour — the engine should compute materially different types per seat from this structural data, and that divergence is the finding the corpus exists to take, not noise to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low d: publishers sit nearest the beneficiary end (arbitrage-grade exit — they can rebrand into whichever vocabulary wins), PD firms close behind (mobile), faculties low-d but sticky (identity_locked raises their cost of acknowledging the arrangement's costs), administrators somewhat higher than pure beneficiaries because they also bear enforcement labor, political risk, and contract lock-in. Declared victims derive high d: teachers moderately high, moderated by their secondary benefit and moderate power; struggling_readers highest — powerless, trapped, biographical horizon, and nationally dispersed, which makes their outcomes the hardest to verify and their extraction the most amplified by scope. The excluded advocates and the analytical observer feed no extraction. No directionality_overrides are authored: the role-plus-exit declarations already separate the seats correctly, and overrides key on power_atom alone, which would smear any correction across unrelated institutional seats sharing that atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ending a destructive pendulum war — was real, and the settlement performed it: the war went underground for two decades. Its status is contested because the settlement froze the underlying question rather than answering it, and the war has resumed in legislative form. Mandatrophy symptoms are present and honestly measured (theater_ratio climbing toward 0.5, rebranding in place of revision), but mandatrophy_resolved is deliberately NOT declared: the coordination function still operates, and the dual-component question the arrangement embodies remains live. Classification discipline cuts both ways: calling this a snare would erase the real goods the settlement delivers (both components genuinely present, factional peace, literature access, a usable teacher framework); calling it a rope would launder the indeterminacy rents and the concentrated cost on struggling readers. Tangled_rope holds both truths. The omegas route the migration paths: a discoverable optimal ratio would let the norm calibrate and drift toward rope; failure of the absorption strategy would leave scaffold-or-piton remnants depending on whether the transition to a successor architecture completes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the reading_acquisition_legitimacy kernel — the balanced_literacy_integration reading. Which reading governs a given jurisdiction, and what structural change follows when the governing reading switches?',
    'Statutory text and adoption-committee composition reveal the operative reading: mandated systematic phonics with decodable-text requirements and cueing bans signals phonics_decoding_primacy or structured_literacy_remediation governance; retention of leveled-text guided reading as the core block signals continued balanced governance. Track statute language and adopted-material composition ratios per state.',
    'Under phonics_decoding_primacy the victim set collapses to students denied explicit code instruction outright and the balance norm itself becomes the extraction target; under whole_language_meaning_primacy decodable-text mandates become the imposed cost; under structured_literacy_remediation the design referent shifts from the median classroom to the most vulnerable learner, changing whose outcomes define legitimacy and hence the entire directionality surface.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which reading of the reading-acquisition legitimacy kernel is operative, and what each sibling switch would restructure.').

omega_variable(
    balance_ratio_indeterminacy,
    'Does ''balance'' admit a determinate, evidence-discoverable allocation ratio between explicit code instruction and literature work, or is the ratio irreducibly context-dependent?',
    'Dose-response studies varying explicit-code minutes within constant total instructional time, stratified by learner profile; stable optimum ranges emerging per profile would establish determinacy.',
    'A determinate ratio would let the norm calibrate itself, shrink the rent surface, and drift the classification toward rope; irreducible indeterminacy makes the vagueness a permanent arbitrage surface for publishers and PD firms and drifts the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_ratio_indeterminacy, empirical, 'Whether the balance norm has a discoverable optimum or a permanent, exploitable indeterminacy.').

omega_variable(
    principle_vs_implementation_extraction,
    'Is the measured cost borne by struggling readers a property of the balanced principle itself, or of permitted-but-not-required implementations (three-cueing, minimal decodable exposure, allocation by tradition rather than diagnosis)?',
    'Compare outcome distributions across balanced implementations that differ in diagnostic allocation while holding materials constant, isolating the principle''s contribution from implementation variance.',
    'If implementation-specific, recalibration inside the reading suffices and the tangled_rope verdict is contingent on enforcement quality; if principled, the reading itself imposes the cost and no internal reform closes the gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(principle_vs_implementation_extraction, conceptual, 'Whether the cost concentration lives in the balance principle or in its common implementations.').

omega_variable(
    absorption_capacity_of_balance,
    'Can the balanced frame absorb the science-of-reading critique indefinitely by rebranding structured literacy as one more component, or will sustained statutory pressure displace the frame?',
    'Track adopted-program composition, decodable-text share, explicit-minutes requirements, and bundle pricing across the next adoption cycles: rising component compliance with persistent bundle economics indicates absorption; state-mandated program architectures incompatible with the bundle model indicate displacement.',
    'Absorption extends the current structure indefinitely with rising theater; displacement converts the remnant into transitional-support or inertial-remnant territory depending on whether the successor transition completes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absorption_capacity_of_balance, empirical, 'Whether the frame absorbs or is displaced by the remediation wave now moving through legislatures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(read_tr_t0, observed).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(read_tr_t5, observed).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(read_tr_t10, observed).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 15, 0.34).
narrative_ontology:measurement_basis(read_tr_t15, observed).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(read_tr_t20, observed).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 25, 0.44).
narrative_ontology:measurement_basis(read_tr_t25, observed).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(read_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(read_be_t0, observed).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 5, 0.41).
narrative_ontology:measurement_basis(read_be_t5, observed).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(read_be_t10, observed).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 15, 0.5).
narrative_ontology:measurement_basis(read_be_t15, observed).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 20, 0.53).
narrative_ontology:measurement_basis(read_be_t20, observed).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 25, 0.56).
narrative_ontology:measurement_basis(read_be_t25, observed).
narrative_ontology:measurement(read_be_t30, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(read_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(read_su_t0, observed).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(read_su_t5, observed).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 10, 0.54).
narrative_ontology:measurement_basis(read_su_t10, observed).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(read_su_t15, observed).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(read_su_t20, observed).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 25, 0.57).
narrative_ontology:measurement_basis(read_su_t25, observed).
narrative_ontology:measurement(read_su_t30, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(read_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% The colloquial label 'balanced literacy' conflates a legitimacy norm (this file) with the commercial package sold under it and with the remediation regimes succeeding it. Per the epsilon-invariance principle the family decomposes: phonics_decoding_primacy, whole_language_meaning_primacy, and structured_literacy_remediation instantiate rival readings, each with its own epsilon, victim set, and classification. This reading authors epsilon for the standing balanced arrangement as seen by its own lights (moderate: genuine dual-component delivery plus indeterminacy rents); the decode-primacy sibling authors epsilon for a decode-first arrangement; the remediation sibling authors epsilon for vulnerability-first design. Lineage: the whole-language and phonics traditions are upstream sources this settlement absorbed; the remediation reading is downstream pressure currently reshaping this arrangement's operating environment. Each family member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
