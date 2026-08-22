% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Balanced Literacy Integrated-Practice Regime
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   Since the mid-1990s, the dominant institutional answer to the
 *   reading-wars standoff in English-speaking schooling has been an
 *   integrated-practice regime: purchased curricula, leveled-book libraries,
 *   running-record assessment, and coach-delivered training promising
 *   explicit phonics woven into authentic literature work. The arrangement
 *   solves a real coordination problem — aligning thousands of schools on one
 *   teachable, procurable pedagogy — while channeling recurring revenue to a
 *   compact publisher-and-training complex and imposing its largest costs on
 *   the children least equipped to absorb weak decoding instruction. KEY
 *   AGENTS (by structural relationship): - curriculum_publishers: Primary
 *   agenda setter (institutional/arbitrage) — sets materials and lesson
 *   architecture, collects the receipts - literacy_pd_industry: Beneficiary
 *   (organized/identity_locked) — collects training fees, cannot exit without
 *   forfeiting credentials - education_school_faculties: Beneficiary
 *   (institutional/identity_locked) — reproduces the framework through
 *   teacher preparation - struggling_readers: Primary target
 *   (powerless/trapped) — bears the largest costs - typical_readers:
 *   Secondary beneficiary-payer (powerless/trapped) — genuine benefit,
 *   diffuse cost - classroom_teachers: Payer-beneficiary
 *   (organized/constrained) — delivers the regime, absorbs retraining and
 *   blame - school_districts: Administering agenda-setter and payer
 *   (institutional/constrained) — signs the contracts, bears replacement
 *   costs - dyslexia_advocacy_families: Excluded voice
 *   (organized/constrained) — forced entry via litigation and legislation -
 *   reading_research_community: Analytical observer
 *   (institutional/analytical) — produces the adjudicating evidence
 *   Constraint-family note: this story is one of three readings of the
 *   reading-acquisition kernel. Each sibling authors its own epsilon over its
 *   own arrangement; the edges run through the shared evidence base and the
 *   shared procurement market, not through logical entailment. The
 *   claim/metric gap is deliberate: the arrangement is CLAIMED as a genuine
 *   integration (its own framing) while the authored metrics describe
 *   increasingly extractive, actively enforced operation — the engine
 *   measures that divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.7).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.62).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Integrated-Practice Regime").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, '0ca32eb8-1a79-4036-968a-997687a75ef8').
narrative_ontology:cs_kernel_codification('0ca32eb8-1a79-4036-968a-997687a75ef8', distributed).
narrative_ontology:cs_authority_grounding('0ca32eb8-1a79-4036-968a-997687a75ef8', lineage).
narrative_ontology:cs_interpretation_layer_present('0ca32eb8-1a79-4036-968a-997687a75ef8').
narrative_ontology:cs_reading_relation('0ca32eb8-1a79-4036-968a-997687a75ef8', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('0ca32eb8-1a79-4036-968a-997687a75ef8', reading_acquisition_mechanism__phonics_reading, influences).
narrative_ontology:cs_axiom('0ca32eb8-1a79-4036-968a-997687a75ef8', foundational, dual_strand_instructional_necessity).
narrative_ontology:cs_axiom_status(dual_strand_instructional_necessity, holdable).
narrative_ontology:cs_axiom_grounding('0ca32eb8-1a79-4036-968a-997687a75ef8', dual_strand_instructional_necessity, empirically_contingent).
narrative_ontology:cs_axiom('0ca32eb8-1a79-4036-968a-997687a75ef8', foundational, integration_over_discrete_sequencing).
narrative_ontology:cs_axiom_status(integration_over_discrete_sequencing, holdable).
narrative_ontology:cs_axiom_grounding('0ca32eb8-1a79-4036-968a-997687a75ef8', integration_over_discrete_sequencing, instrumental).
narrative_ontology:cs_reference_frame('0ca32eb8-1a79-4036-968a-997687a75ef8', integrated_authentic_literacy_with_embedded_code_instruction).
narrative_ontology:cs_drift_state('0ca32eb8-1a79-4036-968a-997687a75ef8', post_science_of_reading_legislation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('0ca32eb8-1a79-4036-968a-997687a75ef8', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, literacy_pd_industry).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, education_school_faculties).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, school_districts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, typical_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, typical_readers).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__balanced_literacy_reading, cueing_msv_word_recognition_theory).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__balanced_literacy_reading, authentic_text_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish the flagship integrated-practice curricula, leveled-book libraries, and assessment kits that districts purchase; operate the institutes and conferences where teachers are certified in the method; set the pacing guides and lesson structures classrooms follow. Revenue rides on multi-year adoptions and consumable refresh cycles. When evidence pressure mounts, the product line is revised to add more explicit code instruction while retaining the installed customer base — a product pivot, not a market exit.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_publishers, agenda_setter,
    institutional, generational, arbitrage, continental).

% Staff developers, consultants, and district coaches deliver the training days, model lessons, and coaching cycles the method prescribes. Their income, credentials, and professional networks are built inside this framework; moving to a rival paradigm would mean forfeiting certifications, published materials, and community standing accumulated over a career.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, literacy_pd_industry, beneficiary,
    organized, biographical, identity_locked, national).

% Teacher-preparation programs teach the integrated constructivist framework as mainstream literacy method; course sequences, dissertations, and faculty hiring reproduce it. Course architecture and scholarly reputations are invested in the framework's premises, so revising them means reworking curricula and conceding error in print.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, education_school_faculties, beneficiary,
    institutional, generational, identity_locked, national).

% Children who do not readily infer the code — disproportionately those with dyslexia and those from homes without abundant print — sit in classrooms where meaning-guessing strategies substitute for systematic decoding practice. They fall behind in the earliest grades, cannot choose another instructional track, and carry the gap into adolescence unless outside tutoring intervenes.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, trapped, national).

% Children from print-rich homes who crack the code with little help. They receive genuine benefits — daily engagement with real books, discussion, writing — while absorbing a slower and less precise start than explicit instruction provides, and their adequate outcomes lend the arrangement its public credibility.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, typical_readers, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, typical_readers, payer).

% Deliver the lessons day to day. Many hold coaching roles or side income inside the framework's training economy and belong to its professional community; at the same time they absorb fidelity mandates, retraining when paradigms shift, and public blame when reading scores disappoint. Unions give them collective voice but little control over which curriculum the district buys.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers, beneficiary).

% Adopt the curriculum, sign the multi-year material and training contracts, and employ the coaches who implement it. Administration of the arrangement sits here, and so do its budget consequences: replacement purchases, retraining days, and remediation staffing when early grades underperform. State legislation and board elections now constrain which adoption choices remain open.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, school_districts, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, school_districts, payer).

% Parents of failing readers who paid for private evaluation and tutoring while asking the school for systematic instruction. They were not seated on curriculum-adoption committees; they gained leverage through right-to-read lawsuits, legislative testimony, and screening mandates rather than through the ordinary adoption process.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, dyslexia_advocacy_families, excluded,
    organized, biographical, constrained, national).

% Cognitive psychologists, learning scientists, and education researchers who produce the longitudinal, eye-tracking, and intervention evidence on how printed words are actually learned. They hold no procurement authority; their influence arrives through legislative findings, court records, and the slow turnover of teacher-preparation syllabi.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, reading_research_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__balanced_literacy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives districts, preparation programs, and publishers a single purchasable, trainable answer to 'how should reading be taught': one framework bundling philosophy, lesson structures, leveled materials, assessments, and certification, so thousands of schools can align practice, hiring, and procurement without adjudicating the underlying mechanism dispute.
% TRANSFER_FUNCTION: Moves district and state purchasing power to publishers and training providers through material contracts and professional-development fees; moves instructional minutes from explicit decoding practice toward meaning-oriented activities; and moves the cost of unresolved decoding gaps onto students' later schooling and onto district remediation budgets.
% ABSENT_VOICES: Cognitive-science researchers and parents of failing readers were outside the rooms where curricula were adopted; publishers, university project leaders, and district administrators decided. Dissenting teachers risked fidelity reviews. The consensus that the compromise worked arose in forums its sharpest critics could not enter until litigation and legislation forced seating.
% DISAPPEARANCE_RATIONALE: Districts would re-open procurement, publishers would rebrand product lines around whichever framework follows, preparation programs would rewrite syllabi, and classrooms would reorganize around successor materials and coaching — the school economy around early literacy visibly rearranges, as state-level replacements have already demonstrated.
% FOUNDING_PROBLEM: After decades of open conflict between code-first and meaning-first camps, districts needed a defensible middle position: one framework that could unify faculties, satisfy procurement, and plausibly claim both rigor and authenticity, ending a war the profession could not win outright.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: state legislative findings accompanying science-of-reading statutes cite stagnant national reading scores and dyslexia screening data; the National Reading Panel report and subsequent cognitive-science syntheses document the evidence impasse the compromise papered over; investigative reporting and right-to-read settlement records document its costs. Publisher and training-provider attestations that the founding problem remains live stand without external corroboration.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.70 at interval end) because pricing is decoupled from instructional value at the margin: classroom leveled-library packages and multi-year training contracts price far above comparable materials, and the largest cost — delayed or absent decoding competence for vulnerable children — is transferred forward to remediation budgets and lifetimes. Suppression (0.62) is structural rather than legal: multi-year contracts, fidelity mandates, credentialing gated through the framework's own institutes, and marginalization of dissent inside adoption processes closed practical exits while rival curricula technically remained purchasable. Theater (0.50) reflects Goodhart drift: running records, leveling rituals, and cueing mini-lessons perform balance while the phonics strand thins — the measurable signature of the collapse-toward-whole-language implementation mode. Accessibility_collapse is moderate-low (0.40) because alternatives never vanished; resistance is high (0.70) and rising, carried by the science-of-reading movement, right-to-read litigation, state statutes, and investigative journalism. All three tracked series share one seven-point grid (t=0..30 step 5) so no metric borrows another's endpoints. Receipt surface: the gains demonstrably accrue to curriculum_publishers, which anchors the catalog that training providers resell; fixing is 'cheap' relative to benefit because multiple states have executed full replacement within ordinary budget and retraining cycles, demonstrating the cost class empirically. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by the engine, from directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats should compute differently. From the publisher and education-school positions the arrangement is a professional ecosystem they built, maintain, and sincerely defend — coordination they experience as service. From the struggling-reader seat the same structure operates as enforced substitution of guessing for instruction, with no exit available to a seven-year-old. Typical readers sit near symmetric: real literary benefit, modest opportunity cost. Teachers are split internally — identity-locked into the framework's community while bearing its fidelity demands and public blame. The engine computes this divergence from the structural data; the authored claim does not adjudicate it. Coalition potential: the powerless payer seat gained leverage only by coalition through parents, litigators, and legislators — the route by which resistance reached 0.70.
 *
 * DIRECTIONALITY LOGIC:
 *   Curriculum_publishers sit at the beneficiary pole (d near 0.0): they collect the transfer and control the rules, with arbitrage-grade exit that lets them pivot product lines without bearing the transition. Literacy_pd_industry and education_school_faculties derive low d from beneficiary declarations, and their identity_locked exits keep them structurally committed to the arrangement's persistence. Struggling_readers derive d near 1.0: full targets, trapped, bearing the concentrated harm. Typical_readers sit mid-low — genuine beneficiaries carrying diffuse costs. Classroom_teachers derive a middling d from their dual declaration. School_districts are the interesting case: agenda-setting power paired with payer costs (budgets, remediation, replacement), pulling their effective d above what their administrative role alone would suggest. Dyslexia_advocacy_families hold target-position interests with no seat — their exclusion is precisely what the adoption process maintained until external force opened it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ending an unwinnable professional war with a defensible middle position — is contested rather than dead: proponents attest integration remains necessary; the legislative and evidentiary record attests the compromise masked non-teaching of decoding. Because status is contested and the disappearance verdict is world_rearranges, the dead-mandate mismatch flag does not fire; this is not a zombie mandate but a live, disputed one. The tangled_rope classification prevents mislabeling in both directions: a pure-snare reading would erase the real coordination delivered (one procurable pedagogy, genuine literature engagement for typical readers) and the sincere professional investment of most practitioners; a pure-rope reading would erase the asymmetric extraction — rent-decoupled pricing, concentrated harm to the least resilient learners, and an enforcement apparatus whose growth tracks dissent rather than instruction. The omega battery locates the swing variables: if design_vs_implementation_fidelity resolves toward design soundness, the classification drifts rope-ward; if integration proves separable branding over a market truce, it drifts snare-ward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (balanced_literacy_reading) of the reading_acquisition_mechanism kernel; what would the sibling readings (phonics_reading, whole_language_reading) change structurally if instantiated?',
    'Compile the sibling stories and compare victim sets, beneficiary sets, and epsilon across readings; the disagreement is located in the necessity/systematicity/status of explicit code instruction, which changes who counts as harmed and how large epsilon is.',
    'Under phonics_reading the victim set shifts toward all students denied systematic instruction and epsilon falls for compliant arrangements; under whole_language_reading the victim set shifts toward students denied meaning-rich exposure. This story''s classifications hold only for the integrated-practice instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position of this story within the reading-acquisition kernel.').

omega_variable(
    design_vs_implementation_fidelity,
    'Are the measured harms caused by the integrated-practice design itself, or by implementations collapsing toward whole-language practice with insufficiently systematic phonics?',
    'Dose-response studies comparing high-fidelity integrated programs that deliver genuinely systematic code instruction against typical field implementations, controlling for student demographics.',
    'If faithful integration performs comparably to explicit instruction, the damage attributes to implementation drift and the reading''s core axiom survives with reduced institutional guilt; if even faithful integration underperforms, the reading''s foundational premise is empirically refuted and the arrangement''s coordination claim collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(design_vs_implementation_fidelity, empirical, 'Whether observed failure belongs to the design or to fidelity decay.').

omega_variable(
    integration_separability,
    'Is simultaneous integration of code instruction and literature work causally necessary, or can the two be scheduled separately with equal effect?',
    'Randomized trials of integrated versus blocked scheduling with matched content and dosage, measuring decoding growth and comprehension.',
    'If separable, the ''integrated practice'' clause functions as branding rather than mechanism, the coordination claim narrows to ordinary curriculum provision, and part of the measured extraction loses its coordination-side justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_separability, empirical, 'Whether integration itself carries pedagogical causal weight.').

omega_variable(
    compromise_market_truce,
    'Was the balanced position adopted as pedagogy, or as a procurement truce that served incumbent publishers and education-school lineages regardless of evidence?',
    'Archival comparison of adoption-decision records, publisher marketing timelines, and university-project funding ties against what the contemporaneous evidence already showed.',
    'If truce-driven, the arrangement''s persistence is capture wearing a pedagogical costume and the computed classification should drift toward the extractive pole; if evidence-driven, coordination credit rises and the extraction reading softens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compromise_market_truce, conceptual, 'Pedagogical design versus market-truce origin of the compromise.').

omega_variable(
    harm_distribution_concentration,
    'Does the arrangement''s cost concentrate on identifiable subgroups — dyslexic students and children from low-print homes — while aggregate averages conceal the concentration?',
    'Disaggregated longitudinal outcome data by initial decoding profile and home print exposure, across adopting districts.',
    'Concentrated harm raises effective extraction for those seats and hardens the victim declarations; genuinely diffuse harm would weaken the asymmetry that makes this a hybrid rather than a pure coordination arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_distribution_concentration, empirical, 'Distributional concentration of the arrangement''s costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 25, 0.46).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 30, 0.5).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(read_be_t5, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(read_be_t15, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(read_be_t25, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(read_be_t30, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(read_su_t5, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(read_su_t10, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(read_su_t15, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(read_su_t25, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 25, 0.59).
narrative_ontology:measurement(read_su_t30, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, resource_allocation).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, whole_language_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'balanced literacy' conflates a pedagogical claim (integration necessity) with an institutional arrangement (a procurement-and-training regime). Per the epsilon-invariance principle, this story authors the institutionalized integrated-practice arrangement under the balanced_literacy_reading's own lights; phonics_reading and whole_language_reading author the rival arrangements with their own epsilon, beneficiaries, and victims. Family edges run through the shared evidence base and shared procurement markets: this reading's partial absorption of the phonics claim altered the rivals' legitimacy conditions and market share without resolving the mechanism dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
