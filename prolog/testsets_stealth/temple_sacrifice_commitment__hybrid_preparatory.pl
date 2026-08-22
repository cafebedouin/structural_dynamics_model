% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__hybrid_preparatory, []).

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
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Sacrifice-Law Study as Suspended Commitment Maintenance (Hybrid-Preparatory Reading)
 *   domain: religious_law/commitment_system_theory
 *
 * SUMMARY:
 *   In the hybrid_preparatory reading of the sacrificial-commitment kernel,
 *   the standing arrangement under contest is the institutionalized regime of
 *   ongoing study of sacrificial law in the absence of the Temple: daily
 *   liturgical recitation of the sacrificial passages, mandatory curriculum
 *   exposure from childhood, and advanced full-time study of the sacrificial
 *   tractates, all justified as maintaining a live but suspended covenantal
 *   obligation and preparing for its messianic restoration. Per the
 *   epsilon-referent rule, epsilon is authored for THIS standing arrangement
 *   as this reading sees it — study as genuine preparatory maintenance
 *   extracting real present resources for deferred benefit — not for the
 *   arrangements the sibling readings would endorse. The claim and the
 *   metrics are independent authored facts: the claimed type states what I
 *   believe is structurally true; the metrics state what I believe is
 *   descriptively true of the arrangement's operation; the engine computes
 *   per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - rabbinic_leadership: Agenda setter (institutional/identity_locked) — administers curriculum, doctrine, and enforcement of the study requirement
 *   - torah_scholarly_class: Primary beneficiary with payer residue (organized/identity_locked) — collects stipends, honor, and vocation; bears the same deferred timeline
 *   - yeshiva_institutions: Institutional beneficiary (institutional/constrained) — receives tuition, donations, and endowments flowing to the study economy
 *   - observant_laity: Dual payer-beneficiary (organized/identity_locked) — funds and staffs the arrangement; receives continuity and membership
 *   - kollel_student_households: Payer (moderate/constrained) — defers material welfare against an undated benefit
 *   - communal_donor_base: Payer with beneficiary residue (powerful/constrained) — directs philanthropy under community-standing pressure
 *   - schoolchildren_in_curriculum: Excluded bearer (powerless/trapped) — spends compulsory years on non-performable procedure without voice
 *   - symbolic_transformation_advocates: Excluded rival (organized/mobile) — holds an alternative instantiation and is answered rather than seated
 *   - religious_studies_analysts: Analytical observer (analytical/analytical) — documents the arrangement without participating
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.48).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.38).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.48).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Sacrifice-Law Study as Suspended Commitment Maintenance (Hybrid-Preparatory Reading)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious_law/commitment_system_theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, '5263f91b-f209-468c-9bd6-95397a59d65c').
narrative_ontology:cs_kernel_codification('5263f91b-f209-468c-9bd6-95397a59d65c', fixed_text).
narrative_ontology:cs_authority_grounding('5263f91b-f209-468c-9bd6-95397a59d65c', lineage).
narrative_ontology:cs_interpretation_layer_present('5263f91b-f209-468c-9bd6-95397a59d65c').
narrative_ontology:cs_reading_relation('5263f91b-f209-468c-9bd6-95397a59d65c', temple_sacrifice_commitment__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('5263f91b-f209-468c-9bd6-95397a59d65c', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_axiom('5263f91b-f209-468c-9bd6-95397a59d65c', foundational, sacrificial_obligation_persists_suspended).
narrative_ontology:cs_axiom_status(sacrificial_obligation_persists_suspended, holdable).
narrative_ontology:cs_axiom_grounding('5263f91b-f209-468c-9bd6-95397a59d65c', sacrificial_obligation_persists_suspended, theological).
narrative_ontology:cs_axiom('5263f91b-f209-468c-9bd6-95397a59d65c', foundational, study_maintains_commitment_pending_restoration).
narrative_ontology:cs_axiom_status(study_maintains_commitment_pending_restoration, holdable).
narrative_ontology:cs_axiom_grounding('5263f91b-f209-468c-9bd6-95397a59d65c', study_maintains_commitment_pending_restoration, conventional).
narrative_ontology:cs_axiom('5263f91b-f209-468c-9bd6-95397a59d65c', secondary, messianic_service_restoration_anticipated).
narrative_ontology:cs_axiom_status(messianic_service_restoration_anticipated, holdable).
narrative_ontology:cs_axiom_grounding('5263f91b-f209-468c-9bd6-95397a59d65c', messianic_service_restoration_anticipated, theological).
narrative_ontology:cs_reference_frame('5263f91b-f209-468c-9bd6-95397a59d65c', study_sustained_suspension).
narrative_ontology:cs_drift_state('5263f91b-f209-468c-9bd6-95397a59d65c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5263f91b-f209-468c-9bd6-95397a59d65c', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, torah_scholarly_class).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, observant_laity).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, kollel_student_households).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, communal_donor_base).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, observant_laity).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, schoolchildren_in_curriculum).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, communal_donor_base).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, torah_scholarly_class).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, suspended_covenantal_obligation_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, mesorah_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the curriculum determining how much sacrificial law each generation masters, rules on which texts are obligatory study, and teaches the doctrine that the obligation to offer sacrifices remains binding though suspended. Draws authority and livelihood from the transmission chain they administer; abandoning the role would dissolve the basis of their standing.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Devotes working life to mastering and teaching the sacrificial corpus; receives stipends, honor, and vocational identity from the study economy. Bears the same deferred timeline as everyone else: their reward is partly the study itself and partly standing in a system whose payoff depends on events none of them control.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, torah_scholarly_class, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, torah_scholarly_class, payer).

% Operate academies whose advanced curricula center on the sacrificial tractates; collect tuition, donations, and endowments earmarked for this study, and compete for students and funders on the strength of their sacrificial-law programs.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions, beneficiary,
    institutional, generational, constrained, global).

% Recite the sacrificial passages in daily liturgy, fund the academies, and send their children through a curriculum weighted toward these laws; in return they receive communal continuity, textual fluency, and membership in a community that treats the commitment as live. Leaving would cost them family, community, and self-understanding.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, observant_laity, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, observant_laity, beneficiary).

% Subsist on modest stipends while a breadwinner spends years on sacrificial-law study that yields no current practice; the household defers housing quality, savings, and career development against a benefit tied to a restoration whose date is undetermined.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, kollel_student_households, payer,
    moderate, biographical, constrained, regional).

% Direct philanthropy toward institutions and stipends on the understanding that the study sustains the covenantal commitment; could redirect giving to poverty relief or general education, but social standing within the donor community tracks support for full-time sacred study.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, communal_donor_base, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, communal_donor_base, beneficiary).

% Spend school years memorizing sacrifice procedures they will never perform; they did not choose the curriculum, cannot exit it, and their preferences reach the system only through adults who endorse the arrangement.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, schoolchildren_in_curriculum, excluded,
    powerless, biographical, trapped, national).

% Hold that prayer and liturgy already constitute the authorized present-day form of the sacrificial service and would reallocate study time accordingly; they sit outside this arrangement's decision-making and are answered rather than seated.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, symbolic_transformation_advocates, excluded,
    organized, biographical, mobile, global).

% Document the post-destruction adaptation of sacrificial religion into textual practice; take no part in the obligation structure and bear none of its burdens.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, religious_studies_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__hybrid_preparatory, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves working knowledge of an elaborate legal system (sacrificial procedure) across generations without a functioning referent institution; coordinates communal identity, liturgical rhythm, and educational priority around a shared textual practice; keeps a covenantal commitment in recognizable, teachable form pending changed conditions.
% TRANSFER_FUNCTION: Moves cognitive labor, years of schooling, and money from schoolchildren, student households, donors, and congregations toward scholarly institutions and the scholarly class, in exchange for a deferred and conditional benefit: readiness for a restoration whose timing no participant controls.
% ABSENT_VOICES: Schoolchildren subject to the curriculum have no seat; households bearing the opportunity cost are represented only by institutions that administer the requirement; advocates of the rival readings (symbolic transformation, reduced emphasis) stand outside the conversation and are rebutted rather than consulted.
% DISAPPEARANCE_RATIONALE: If the study-maintenance arrangement vanished overnight, the sacrificial passages would lose their structural place in daily liturgy and advanced curricula, institutional funding streams earmarked for this study would reallocate, and the doctrinal claim of a live suspended obligation would lose its operational vehicle — drifting toward either archival preservation or full symbolic replacement. The community's relationship to the sacrificial corpus would reorganize within a generation.
% FOUNDING_PROBLEM: The destruction of the Second Temple removed the performative site of a commandment-centered legal system. The founding problem: how a covenantally obligated community keeps its commitment, knowledge, and identity alive when the commanded practice cannot be performed — across an interregnum of undetermined length.
% FOUNDING_PROBLEM_CORROBORATION: Academic historiography of post-70 CE Judaism attests the founding crisis from outside the benefiting parties, and advocates of the rival readings concede the crisis while disputing the solution — performance_only advocates affirm the practice is defunct, which presupposes the same founding rupture. No party denies the founding problem; the dispute concerns whether study resolves it.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__hybrid_preparatory, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48): the arrangement genuinely preserves a body of practical-legal knowledge and communal identity, but it draws years of cognitive labor and material support toward an outcome no participant can cause or schedule; the burden is real, present, and concentrated, the benefit deferred and probabilistic. Suppression (0.38) is enforced communally rather than by state machinery — educational gating, stipend dependency, and marriage-market alignment — and its trajectory is non-monotonic: early enforcement was dense communal coercion, emancipation-era mobility eroded external enforcement capacity (falling series through the middle of the interval), and the modern rebound reflects identity-carried compliance replacing institutional coercion. Theater (0.28) rises across the interval as rote liturgical recitation without comprehension grows as a share of total engagement, dipping slightly at the endpoint as contemporary advanced study deepens for the engaged minority. Accessibility_collapse (0.40) is well below natural-law levels: the tradition itself contains live alternatives (symbolic transformation, reduced emphasis, private versus institutional study), and understanding the arrangement does not eliminate them. Resistance (0.35) is chronic and low-intensity: rationalist critique, denominational departure, and periodic reallocation proposals, but no mass refusal. All three tracked series run on one shared seven-point grid so every metric is authored at every examined time point. Coalition note: the payer seats (households, donors) jointly control the resource flows that sustain the arrangement, but identity lock and community-standing pressure fragment coalition formation — the constraint's stability rests partly on preventing exactly that coordination.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as fidelity: continuing an unbroken transmission and honoring a live obligation. The payer seats experience the same structure as deferred-benefit burden: present cost, undated return. The excluded seats experience it as imposition without voice — children bound to a curriculum they did not choose, rivals whose alternative is rebutted rather than adopted. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic leadership and yeshiva institutions sit near the beneficiary end: they administer the arrangement and collect authority, revenue, and continuity from its operation. The scholarly class sits low-mid: it collects livelihood and standing yet bears the identical deferred timeline and the opportunity cost of its own specialization. Observant laity sits near symmetric: genuine continuity and membership goods received, real money and curriculum burden paid. Kollel households and the donor base sit toward the target end: concentrated present cost, diffuse deferred return, constrained exit. Schoolchildren sit nearest the full-target end: total burden-bearing with trapped exit and no voice. The beneficiary/victim declarations map onto these positions directly; no directionality overrides are authored because the derivation from declared roles plus exit options already differentiates the seats, and overrides keyed by power atom would collide across distinct agents sharing atoms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — sustaining a commandment-bound community's commitment without its performative site — remains live: the interregnum has not ended, so no mandatrophy declaration is authored, and the R5 mismatch consumer finds status=live paired with verdict=world_rearranges, producing no zombie flag. The tangled_rope claim prevents two symmetrical mislabels: reading the arrangement as pure coordination ignores the asymmetric deferral structure (present concentrated cost, undated conditional benefit, identifiable bearer households); reading it as pure extraction ignores the genuine coordination function — knowledge preservation and identity continuity — that would survive any reform of the benefit distribution. Theater is treated as a symptom, not the test: the arrangement's persistence tracks a live doctrine held by its administrators, not inertial habit, and its administrators could change it but the doctrinal cost of change exceeds what they bear.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_occupation_underdetermination,
    'This constraint is one reading (hybrid_preparatory) of the kernel temple_sacrifice_commitment; the sibling readings assign study structurally different functions — full performance, archival preservation, or authorized symbolic replacement. Which function does study actually discharge within the living tradition?',
    'Compare the readings'' operational predictions against communal behavior: if communities treat mastery as discharging the obligation, study_as_exercise tracks; if practical knowledge atrophies without ritual rehearsal structures, performance_only tracks; if liturgical recitation fully displaces dedicated study, symbolic_transformation tracks.',
    'Under study_as_exercise the arrangement''s burden reclassifies as fulfillment cost and target-side directionality drops; under performance_only the maintaining function vanishes and the arrangement drifts toward inertial maintenance; under symbolic_transformation the dedicated study requirement itself dissolves into liturgy. Each resolution changes the victim set and epsilon materially.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(degree_of_occupation_underdetermination, conceptual, 'Kernel-level ambiguity over what study does to the sacrificial commitment: maintain, perform, archive, or replace.').

omega_variable(
    indefinite_preparation_coherence,
    'Can study remain genuinely preparatory when the anticipated restoration is indefinitely deferred, or does the preparatory justification decay into identity practice indistinguishable from the rival readings?',
    'Longitudinal comparison of communities differing in restoration-imminence belief: measure whether curriculum choices track restoration-relevance (procedural mastery versus abstract theory) and whether preparatory framing persists in homiletics and policy as the wait lengthens.',
    'If preparation decays into identity practice, the arrangement''s coordination function weakens and the measured burden shifts from investment-for-benefit toward unmotivated cost, moving the classification toward extraction-dominant forms; if the preparatory frame is stable under indefinite extension, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefinite_preparation_coherence, conceptual, 'Whether a preparatory justification survives unlimited deferral of the prepared-for event.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the compliance force keeping participants inside the study regime structural (educational gating, stipend dependency, communal economic alignment) or internalized (identity fusion in which exit equals self-betrayal)?',
    'Post-exit trajectory study of leavers: if study-norm pressure, guilt, and curriculum-shaped self-assessment persist after leaving the enforcing community, the internalized component is confirmed; if they attenuate with distance, the structural measure dominates.',
    'If substantially internalized, effective suppression exceeds the structural measure and exit costs exceed observable barriers; the payer seats'' constrained-exit ratings understate their true entrapment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in the study-compliance regime.').

omega_variable(
    opportunity_cost_quantification,
    'What is the realized opportunity cost borne by the study subjects — years, foregone earnings, alternative expertise — relative to the benefit the arrangement actually delivers?',
    'Economic tracing of full-time-study career paths against matched counterfactual cohorts, plus curriculum time-allocation audits quantifying the share of schooling devoted to non-performable procedure.',
    'A high cost-to-benefit ratio confirms the weight of the victim declarations in the directionality derivation; a low ratio supports treating the burden as ordinary coordination cost and would pull the classification toward purer coordination forms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_quantification, empirical, 'Magnitude of the payer-seat burden relative to delivered benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsc_hybrid_prep_tr_t0, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(tsc_hybrid_prep_tr_t0, observed).
narrative_ontology:measurement(tsc_hybrid_prep_tr_t10, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(tsc_hybrid_prep_tr_t10, observed).
narrative_ontology:measurement(tsc_hybrid_prep_tr_t20, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(tsc_hybrid_prep_tr_t20, observed).
narrative_ontology:measurement(tsc_hybrid_prep_tr_t30, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(tsc_hybrid_prep_tr_t30, observed).
narrative_ontology:measurement(tsc_hybrid_prep_tr_t40, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(tsc_hybrid_prep_tr_t40, observed).
narrative_ontology:measurement(tsc_hybrid_prep_tr_t50, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 50, 0.29).
narrative_ontology:measurement_basis(tsc_hybrid_prep_tr_t50, observed).
narrative_ontology:measurement(tsc_hybrid_prep_tr_t60, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(tsc_hybrid_prep_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(tsc_hybrid_prep_be_t0, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(tsc_hybrid_prep_be_t0, observed).
narrative_ontology:measurement(tsc_hybrid_prep_be_t10, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(tsc_hybrid_prep_be_t10, observed).
narrative_ontology:measurement(tsc_hybrid_prep_be_t20, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(tsc_hybrid_prep_be_t20, observed).
narrative_ontology:measurement(tsc_hybrid_prep_be_t30, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 30, 0.46).
narrative_ontology:measurement_basis(tsc_hybrid_prep_be_t30, observed).
narrative_ontology:measurement(tsc_hybrid_prep_be_t40, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 40, 0.47).
narrative_ontology:measurement_basis(tsc_hybrid_prep_be_t40, observed).
narrative_ontology:measurement(tsc_hybrid_prep_be_t50, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 50, 0.48).
narrative_ontology:measurement_basis(tsc_hybrid_prep_be_t50, observed).
narrative_ontology:measurement(tsc_hybrid_prep_be_t60, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 60, 0.48).
narrative_ontology:measurement_basis(tsc_hybrid_prep_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(tsc_hybrid_prep_su_t0, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(tsc_hybrid_prep_su_t0, observed).
narrative_ontology:measurement(tsc_hybrid_prep_su_t10, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(tsc_hybrid_prep_su_t10, observed).
narrative_ontology:measurement(tsc_hybrid_prep_su_t20, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 20, 0.43).
narrative_ontology:measurement_basis(tsc_hybrid_prep_su_t20, observed).
narrative_ontology:measurement(tsc_hybrid_prep_su_t30, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 30, 0.4).
narrative_ontology:measurement_basis(tsc_hybrid_prep_su_t30, observed).
narrative_ontology:measurement(tsc_hybrid_prep_su_t40, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 40, 0.36).
narrative_ontology:measurement_basis(tsc_hybrid_prep_su_t40, observed).
narrative_ontology:measurement(tsc_hybrid_prep_su_t50, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 50, 0.34).
narrative_ontology:measurement_basis(tsc_hybrid_prep_su_t50, observed).
narrative_ontology:measurement(tsc_hybrid_prep_su_t60, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 60, 0.38).
narrative_ontology:measurement_basis(tsc_hybrid_prep_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__performance_only).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'study of sacrifices in place of sacrifice' decomposes, per the epsilon-invariance principle, into structurally distinct claims about the degree to which intellectual engagement occupies the sacrificial commitment. This file instantiates the hybrid_preparatory reading (moderate epsilon: genuine maintenance function, real deferred-benefit burden). study_as_exercise assigns study full performative force (epsilon drops toward coordination cost); performance_only denies study any maintaining force (the arrangement loses its function and drifts toward inertial maintenance). The upstream/downstream linkage runs through shared doctrinal infrastructure: whichever reading prevails determines whether the study economy's resource flows count as fulfillment, maintenance, or misallocation. A fourth contest reading, symbolic_transformation, is documented in the kernel-context commentary and omegas but is not a declared sibling edge in this file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
