% ============================================================================
% CONSTRAINT STORY: tenure_contract__demographic_reproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__demographic_reproduction_reading, []).

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
 *   constraint_id: tenure_contract__demographic_reproduction_reading
 *   human_readable: Tenure Peer Review as Demographic Reproduction Mechanism (Demographic-Reproduction Reading)
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This story instantiates the demographic_reproduction_reading of the
 *   tenure_contract kernel: the claim that tenure peer review reproduces the
 *   senior ranks' existing composition through 'fit' and 'collegiality'
 *   criteria that float free of research productivity. The arrangement under
 *   contest is the standing tenure-review apparatus — committees of
 *   incumbents, confidential external letters, closed deliberation, an
 *   up-or-out clock — assessed by this reading's own lights, which locate the
 *   operative selection in discretionary similarity judgment layered over a
 *   genuine productivity screen. CONSTRAINT FAMILY: the same kernel supports
 *   two sibling stories — tenure_contract__academic_freedom_reading (which
 *   authors low epsilon over the identical arrangement, reading it as
 *   protection that decouples inquiry survival from institutional or
 *   political displeasure) and
 *   tenure_contract__institutional_extraction_reading (which authors high
 *   epsilon with a different victim surface: contingent labor and blocked
 *   resource reallocation). All three are linked through
 *   network.affects_constraints; the epsilon values differ because epsilon is
 *   a property of the reading, not the topic — the referent, the standing
 *   arrangement, is common to all three. CLAIM/METRIC INDEPENDENCE:
 *   claimed_type is authored as tangled_rope because the same committee act
 *   that screens scholarship also filters demographically — coordination and
 *   extraction run through one structure; the metrics are authored
 *   descriptively (extractiveness 0.72, suppression 0.70, theater_ratio 0.50)
 *   without reference to what type they would certify.
 *
 * KEY AGENTS:
 *   - tenure_review_committees: agenda setter (institutional/constrained) — administers review, draws its members from the beneficiary pool
 *   - dominant_demographic_tenured_faculty: primary beneficiary (powerful/mobile) — collects reproduced composition and protected discretion
 *   - elite_advisor_lineage_networks: secondary beneficiary (organized/arbitrage) — reproduces placement and letter circuits across institutions
 *   - underrepresented_junior_scholars: primary target (powerless/constrained) — bears the filter during the tenure-clock window
 *   - women_scholars_male_dominated_fields: primary target (powerless/constrained) — collegiality judged by a demographically homogeneous bench
 *   - first_generation_academics: target (powerless/constrained) — class-inflected 'polish' discounted as fit
 *   - contingent_instructional_faculty: excluded (powerless/trapped) — absorbs the filtered-out workload, holds no seat in governance
 *   - faculty_diversity_officers: monitor (moderate/constrained) — compiles the gap data, holds no vote
 *   - academic_labor_researchers: analytical observer — publishes the attrition and letter-language audits outside parties cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.72).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.7).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Reproduction Mechanism (Demographic-Reproduction Reading)").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, 'd50841ca-afc0-4c20-8c74-9c75cb04f1f6').
narrative_ontology:cs_kernel_codification('d50841ca-afc0-4c20-8c74-9c75cb04f1f6', formalized).
narrative_ontology:cs_authority_grounding('d50841ca-afc0-4c20-8c74-9c75cb04f1f6', practice).
narrative_ontology:cs_interpretation_layer_present('d50841ca-afc0-4c20-8c74-9c75cb04f1f6').
narrative_ontology:cs_reading_relation('d50841ca-afc0-4c20-8c74-9c75cb04f1f6', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('d50841ca-afc0-4c20-8c74-9c75cb04f1f6', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('d50841ca-afc0-4c20-8c74-9c75cb04f1f6', foundational, homophily_is_not_merit).
narrative_ontology:cs_axiom_status(homophily_is_not_merit, holdable).
narrative_ontology:cs_axiom_grounding('d50841ca-afc0-4c20-8c74-9c75cb04f1f6', homophily_is_not_merit, empirically_contingent).
narrative_ontology:cs_axiom('d50841ca-afc0-4c20-8c74-9c75cb04f1f6', foundational, evaluation_criteria_must_track_productivity).
narrative_ontology:cs_axiom_status(evaluation_criteria_must_track_productivity, holdable).
narrative_ontology:cs_axiom_grounding('d50841ca-afc0-4c20-8c74-9c75cb04f1f6', evaluation_criteria_must_track_productivity, instrumental).
narrative_ontology:cs_reference_frame('d50841ca-afc0-4c20-8c74-9c75cb04f1f6', productivity_indexed_merit_screening).
narrative_ontology:cs_drift_state('d50841ca-afc0-4c20-8c74-9c75cb04f1f6', contemporary_accountability_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d50841ca-afc0-4c20-8c74-9c75cb04f1f6', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, dominant_demographic_tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, elite_advisor_lineage_networks).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_junior_scholars).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, women_scholars_male_dominated_fields).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, first_generation_academics).
narrative_ontology:constraint_vindicates(tenure_contract__demographic_reproduction_reading, collegiality_evaluation_doctrine).
narrative_ontology:constraint_vindicates(tenure_contract__demographic_reproduction_reading, discretionary_peer_judgment_authority).
narrative_ontology:constraint_vindicates(tenure_contract__demographic_reproduction_reading, local_fit_standard_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compose review panels from incumbent faculty, solicit confidential external letters, deliberate behind closed doors, and cast the up-or-down vote on six-year cases. Members are drawn from the senior ranks they are evaluating into; deans and provosts ratify or return their recommendations. Their discretion over what counts as 'fit' is the operative standard, exercised without a published rubric. Leaving the role means returning to ordinary senior-faculty duties; the committee system itself predates every current member.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, tenure_review_committees, agenda_setter,
    institutional, generational, constrained, national).

% Hold permanent appointments secured under the current criteria and staff the committees that apply them. The criteria reward familiarity: shared intellectual lineage, comparable career shapes, social ease in departmental settings. Each cycle that applies these standards leaves the senior ranks resembling the people who applied them. Individual members can move between institutions freely; the standards travel with them.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, dominant_demographic_tenured_faculty, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__demographic_reproduction_reading, dominant_demographic_tenured_faculty, agenda_setter).

% Clusters of senior scholars at prestige departments whose students, letter-writers, and co-authors form self-referencing circuits. Placement, recommendation, and citation flow along these lines, and 'fit' assessments reliably favor candidates already inside them. The networks span institutions and countries, so no single department's reform touches them.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, elite_advisor_lineage_networks, beneficiary,
    organized, generational, arbitrage, global).

% Enter the tenure track with six years to build a case, knowing the decisive criteria are unwritten. Service burdens fall disproportionately on them — diversity work, mentoring students of color — which panels discount as non-scholarly. A negative vote ends the appointment and, typically, the academic career built around narrow specialization. Relocation to another tenure track restarts the clock elsewhere under the same unwritten standards.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_junior_scholars, payer,
    powerless, biographical, constrained, national).

% Work in departments where collegiality judgments are rendered almost entirely by men. Parenthood penalties land inside the probationary window; service requests arrive earlier and count less. External letters frequently describe the same record differently depending on the candidate. Exit options mirror other junior scholars': abandon the specialization or accept a lower-prestige track.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, women_scholars_male_dominated_fields, payer,
    powerless, biographical, constrained, national).

% Reached the doctorate without the class-inflected fluency — elite undergraduate pedigree, unpaid internships, conference socialization — that 'fit' talk reads as polish. Their records are legible on paper and discounted in rooms where ease signals competence. Debt loads make repeating the probationary gamble at another institution harder to sustain.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, first_generation_academics, payer,
    powerless, biographical, constrained, national).

% Teach the courses the tenure lines no longer cover, on year-to-year contracts, without voting rights in the bodies that decide how many tenure lines exist or who fills them. Many passed through the review machinery this story concerns; none sit in the room when it is defended or reformed. Exit means leaving higher-education instruction altogether.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, contingent_instructional_faculty, excluded,
    powerless, immediate, trapped, national).

% Administer cluster-hire programs, audit search slates, and report compositional gaps to provosts. They can see which stages lose which candidates but hold no vote in tenure cases and no authority over the criteria. Their continued employment depends on the institutions whose records they compile.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, faculty_diversity_officers, observer,
    moderate, biographical, constrained, national).

% Study tenure outcomes, evaluation language, and attrition across institutions and decades. They publish the stage-by-stage attrition decompositions and the letter-language audits that outside parties cite. No case outcome depends on their findings.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, academic_labor_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__demographic_reproduction_reading, dominant_demographic_tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__demographic_reproduction_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools the dispersed judgment of senior scholars into a single decision about which of many applicants gains one of a department's few permanent positions, and sets a common evidentiary bar (record, letters, teaching) so that hiring across departments remains mutually legible.
% TRANSFER_FUNCTION: Moves permanent employment security, laboratory space, course relief, and professional standing from the applicant pool to candidates whom incumbent panels read as familiar; moves unpaid evaluative and diversity service onto junior and underrepresented faculty; moves decision rights over the future composition of the professoriate to current incumbents.
% ABSENT_VOICES: Candidates denied at the gate are absent from every later review of the criteria — many have left academe entirely. Contingent instructors who teach the displaced workload have no seat in governance. Graduate students contemplating the track see the filter only from inside it. The counterfactual productivity of rejected candidates is structurally unobservable, so no one speaks for the careers the criteria ended.
% DISAPPEARANCE_RATIONALE: If the fit-and-collegiality layer vanished overnight and panels weighed audited records alone, offer patterns and tenure rates would shift within a few cycles, the senior ranks would recompose over a decade or two, and the lineage networks would lose their reproductive advantage. Departments would need to rebuild evaluation around transparent criteria, and the incumbents' discretion — the asset the arrangement protects — would evaporate.
% FOUNDING_PROBLEM: Permanent appointments cannot be safely undone, so departments needed an ex ante screen trustworthy enough to grant lifetime security: a way to judge, before the fact, whether a stranger would produce valuable work and function as a durable colleague for decades.
% FOUNDING_PROBLEM_CORROBORATION: Disciplinary-society climate studies and academic-labor economists attest both the live screening problem and the compositional findings from outside the benefiting parties. The AAUP's own warnings against admitting collegiality as an independent criterion (1999) constitute corroboration from within faculty governance but against the beneficiary practice. No attesting source denies that some ex ante judgment is necessary; the dispute is over what the judgment may legitimately weigh.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__demographic_reproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__demographic_reproduction_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__demographic_reproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__demographic_reproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores are authored descriptively. Extractiveness 0.72: the discretionary layer (fit, collegiality, letter tone) is decoupled from audited productivity and decides close cases, which is precisely where compositional selection happens; the productivity screen beneath it is real, which is why epsilon is high but not extreme. Suppression 0.70 is authored as a raw structural property — unscaled by power or scope in the engine's arithmetic: closed deliberation, confidential letters, a binary up-or-out gate timed to peak family-formation years, and heavy stigma on non-track exits. Theater_ratio 0.50 marks the point where compliance performance (diversity statements, slate audits, cluster-hire announcements) rivals substantive evaluation in the apparatus's activity — Goodhart-drift territory, tracked temporally below. Accessibility_collapse 0.60: understanding the criteria creates no alternative channel — alt-ac and industry exits exist but do not lead back to the permanent ranks, so the understood game remains the only game. Resistance 0.55: unionization drives, transparency litigation, public tenure-rate dashboards, and disciplinary-society climate reports are real and growing but have not yet altered the criteria. The measurement series runs on ONE shared grid (seven points across t=0..48, all three metrics authored at every point) so no metric row is silently substituted with an end-state scalar. Trajectories are monotonic ratchets, not cycles: removal of formal bars in the 1970s pushed selection into discretionary criteria, and each subsequent transparency-pressure wave was answered with more sophisticated confidentiality — hence the warranted suppression_requirement series, since the story specifically tracks enforcement-capacity maturation rather than mere extraction shift. Rising base_extractiveness across the interval is the accumulation signature the temporal detector looks for. Coalition note: the three payer groups are individually powerless but share informational interests; cross-institutional data collectives and sectoral unionization are the unrealized coalition path, currently fragmented by discipline and institution.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the committee/beneficiary seats compute differently from the same facts. From inside a panel, exercising judgment about 'fit' feels like quality control and community protection; the same act, from a candidate whose difference is being priced, operates as a filter with no appeal and no written standard. Same-level lateral dynamics: two assistant professors with statistically identical records but different demographic and network positions face different effective constraints despite equal formal rank — what differs is exit-relevant network embedding and the demographic composition of the judging bench, not global power. Identity-lock: for targets, professional identity is fused with the specialization the probationary clock financed; walking away reads as forfeiting the self, so candidates stay on the clock at visibly worsening odds — if a credible, dignified exit existed, effective extraction would fall without any procedural change. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low d: dominant_demographic_tenured_faculty and elite_advisor_lineage_networks are subsidized by the arrangement — each cycle reproduces their composition and extends their networks at others' expense. Victim declarations map to high d: the three junior-scholar groups bear the filter during the only window in which it can end their careers, with constrained exit; trapped or network-dependent targets sit nearer the full-target end than mobile ones. The administrative seat is the one place the structural derivation needs help: a canonical fallback would read an institutional administrator as a neutral arbiter near symmetric, but tenure_review_committees are staffed from, answerable to, and socially continuous with the beneficiary pool — the directionality_overrides entry sets the institutional atom to 0.35, beneficiary-side of symmetric, reflecting shared position minus the workload and legal exposure the seat uniquely bears. Contingent_instructional_faculty sit outside the beneficiary/victim derivation entirely (authored as excluded, not payer): their costs are downstream of the filter, and forcing them into d would misattribute this constraint's extraction surface; their adjacency to the target position is recorded in their situation text. Faculty_diversity_officers and academic_labor_researchers are observational seats with no material flow either way.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — screening irreversible lifetime hires — is live, so no mandatrophy resolution is declared and none should be inferred from the theater trajectory. The classification's protective work is bidirectional: reading the whole apparatus as pure extraction would erase the genuine pooled-judgment screen (coordination mislabeled as extraction); accepting the academic_freedom_reading's low-epsilon story wholesale would erase the filter (extraction mislabeled as coordination). The tangled_rope claim holds both surfaces in one structure. Watch item: theater_ratio reaches 0.5 at interval end — if compliance performance continues displacing substantive evaluation while compositional outcomes stay flat, the apparatus drifts toward performance-maintained inertia layered on the filter, the piton-adjacent signature; the temporal series is the instrument that would catch it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    criterion_validity_ambiguity,
    'Do ''fit'' and ''collegiality'' judgments predict post-tenure scholarly contribution and departmental functioning once demographic similarity is controlled, or do they proxy homophily?',
    'Longitudinal validation linking panel ratings to blinded post-tenure outcomes (publication trajectory, grant success, teaching evaluations, retention) with demographic controls; natural experiments from departments that adopted structured rubrics and published letter templates.',
    'If the criteria are predictive, part of the measured extraction is legitimate screening cost and the constraint sits nearer the coordination end; if they are not, the discretionary layer is a pure similarity filter and the classification moves decisively toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criterion_validity_ambiguity, empirical, 'Whether the discretionary criteria carry evaluative validity or proxy demographic similarity.').

omega_variable(
    pipeline_vs_gate_attribution,
    'How much of the compositional persistence this reading attributes to tenure review actually originates upstream (doctoral production, application, shortlist stages) rather than in the tenure decision itself?',
    'Stage-by-stage attrition decomposition comparing identically productive cohorts across the academic pipeline; audit studies separating shortlisting effects from tenure-vote effects.',
    'Determines how much of the authored epsilon belongs to this constraint versus upstream ones; if most attrition is upstream, repairing tenure review alone changes little and the constraint''s effective extraction is lower than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pipeline_vs_gate_attribution, empirical, 'Attribution of compositional outcomes between the tenure gate and upstream pipeline stages.').

omega_variable(
    reading_indexical_status,
    'This constraint is the demographic_reproduction_reading of the tenure_contract kernel; would instantiating the academic_freedom_reading or the institutional_extraction_reading instead relocate the victim set and the epsilon value, and where exactly is the disagreement located?',
    'Comparative classification across the three linked stories: the freedom reading authors low epsilon over the identical arrangement (protection that decouples inquiry survival from institutional displeasure); the extraction reading authors high epsilon with contingent labor and blocked resource reallocation as the victim surface. The disagreement is located in what the peer-review criteria fundamentally select for: inquiry protection, demographic similarity, or early-winner rents.',
    'Per-seat classifications and any family-level verdict depend on which reading a seat endorses; merging the readings into one constraint would average incompatible epsilon values over one referent and destroy the signal the family exists to take.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexical_status, conceptual, 'Committer structure: one kernel, three readings, disagreement located in the selection criterion.').

omega_variable(
    suppression_structural_internalized_split,
    'Is the suppression junior scholars experience structural (closed deliberation, confidential letters, up-or-out timing, binary credential gate) or internalized (anticipatory conformity, self-censorship of risky research agendas, self-selection out before review)?',
    'Post-exit trajectory interviews and behavioral comparison under structured-transparency reforms: if conformity and self-limitation persist after procedures open, the internalized share is large.',
    'Internalized suppression travels with the target after exit and survives procedural reform, raising effective suppression above the structural measure and slowing compositional response to criteria fixes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_internalized_split, empirical, 'Split between structural and internalized suppression mechanisms acting on candidates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_demo_repro_tr_t0, tenure_contract__demographic_reproduction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(tenure_demo_repro_tr_t0, observed).
narrative_ontology:measurement(tenure_demo_repro_tr_t8, tenure_contract__demographic_reproduction_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(tenure_demo_repro_tr_t8, observed).
narrative_ontology:measurement(tenure_demo_repro_tr_t16, tenure_contract__demographic_reproduction_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement_basis(tenure_demo_repro_tr_t16, observed).
narrative_ontology:measurement(tenure_demo_repro_tr_t24, tenure_contract__demographic_reproduction_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement_basis(tenure_demo_repro_tr_t24, observed).
narrative_ontology:measurement(tenure_demo_repro_tr_t32, tenure_contract__demographic_reproduction_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement_basis(tenure_demo_repro_tr_t32, observed).
narrative_ontology:measurement(tenure_demo_repro_tr_t40, tenure_contract__demographic_reproduction_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(tenure_demo_repro_tr_t40, observed).
narrative_ontology:measurement(tenure_demo_repro_tr_t48, tenure_contract__demographic_reproduction_reading, theater_ratio, 48, 0.5).
narrative_ontology:measurement_basis(tenure_demo_repro_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(tenure_demo_repro_be_t0, tenure_contract__demographic_reproduction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(tenure_demo_repro_be_t0, observed).
narrative_ontology:measurement(tenure_demo_repro_be_t8, tenure_contract__demographic_reproduction_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(tenure_demo_repro_be_t8, observed).
narrative_ontology:measurement(tenure_demo_repro_be_t16, tenure_contract__demographic_reproduction_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement_basis(tenure_demo_repro_be_t16, observed).
narrative_ontology:measurement(tenure_demo_repro_be_t24, tenure_contract__demographic_reproduction_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(tenure_demo_repro_be_t24, observed).
narrative_ontology:measurement(tenure_demo_repro_be_t32, tenure_contract__demographic_reproduction_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(tenure_demo_repro_be_t32, observed).
narrative_ontology:measurement(tenure_demo_repro_be_t40, tenure_contract__demographic_reproduction_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement_basis(tenure_demo_repro_be_t40, observed).
narrative_ontology:measurement(tenure_demo_repro_be_t48, tenure_contract__demographic_reproduction_reading, base_extractiveness, 48, 0.72).
narrative_ontology:measurement_basis(tenure_demo_repro_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(tenure_demo_repro_su_t0, tenure_contract__demographic_reproduction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(tenure_demo_repro_su_t0, observed).
narrative_ontology:measurement(tenure_demo_repro_su_t8, tenure_contract__demographic_reproduction_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement_basis(tenure_demo_repro_su_t8, observed).
narrative_ontology:measurement(tenure_demo_repro_su_t16, tenure_contract__demographic_reproduction_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement_basis(tenure_demo_repro_su_t16, observed).
narrative_ontology:measurement(tenure_demo_repro_su_t24, tenure_contract__demographic_reproduction_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(tenure_demo_repro_su_t24, observed).
narrative_ontology:measurement(tenure_demo_repro_su_t32, tenure_contract__demographic_reproduction_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement_basis(tenure_demo_repro_su_t32, observed).
narrative_ontology:measurement(tenure_demo_repro_su_t40, tenure_contract__demographic_reproduction_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement_basis(tenure_demo_repro_su_t40, observed).
narrative_ontology:measurement(tenure_demo_repro_su_t48, tenure_contract__demographic_reproduction_reading, suppression_requirement, 48, 0.7).
narrative_ontology:measurement_basis(tenure_demo_repro_su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, resource_allocation).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__institutional_extraction_reading).

% DUAL FORMULATION NOTE:
% One colloquial label — 'tenure' — covers three structurally distinct claims about the same arrangement, per the epsilon-invariance principle: protection of inquiry (low epsilon), demographic reproduction (high epsilon, demographic victim surface), and early-winner rent extraction (high epsilon, contingent-labor and rigidity victim surface). Authored as three linked stories sharing one referent; epsilon differs because it is a property of the reading, not the topic. This story links to both siblings. The freedom reading is treated as upstream in public legitimacy — its claim is the one cited to defend the arrangement this reading indicts — so this reading's evidence exerts downstream legitimacy pressure on it (relation: influences, not forecloses: critics standardly hold both claims simultaneously). The extraction reading is an independent parallel indictment with a disjoint victim surface; neither reading logically eliminates the other (relation: coexists_with).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__demographic_reproduction_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
