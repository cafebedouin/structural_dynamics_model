% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__exogenous_override_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__exogenous_override_reading
 *   human_readable: Climb-Only Mechanism Taxonomy (M-set) as Assessed from the Exogenous Override Reading
 *   domain: historical sociology / state formation / commitment systems
 *
 * SUMMARY:
 *   The standing arrangement under contest is the discipline's operative
 *   mechanism taxonomy for commitment displacement — the M-set whose single
 *   climb pathway (fringe adoption, then gradual ascent) codes every episode.
 *   This file instantiates the exogenous_override_reading of the
 *   imposition_pathway_kernel: the claim that state capacity enables
 *   displacement WITHOUT a fringe-adoption pathway (Meiji calendar and dress
 *   decrees show compliance arriving with enforcement, ahead of any
 *   detectable popular adoption), and that the taxonomy is therefore
 *   incomplete without a distinct override cell. Epsilon's referent is the
 *   climb-only regime as this reading assesses it — NOT the
 *   override-inclusive taxonomy the reading endorses. The colloquial label
 *   decomposes into three structurally distinct claims (this reading,
 *   endogenous_climb, hybrid_cascade); per the epsilon-invariance principle
 *   they are separate stories linked by network.affects_constraints. Claim
 *   and metrics are authored independently: claimed_type reflects the
 *   structure I believe true (genuine coordination plus asymmetric
 *   extraction); the metrics describe the regime's operation as the
 *   documentary record shows it.
 *
 * KEY AGENTS:
 *   - climb_paradigm_incumbents: Primary beneficiary (institutional / identity_locked) — collects theoretical scope, citation centrality, and canonical status from the template's monopoly
 *   - peer_review_gatekeepers: Agenda setter (institutional / constrained) — enforces the climb template at the venue gate; administers machinery it did not build
 *   - decree_first_case_specialists: Primary target (moderate / constrained) — bears the distortion costs; area expertise ports poorly
 *   - policy_lesson_consumers: Secondary target (organized / constrained) — inherits skewed mechanism lessons through the dominant synthesis channel
 *   - graduate_students_in_historical_sociology: Reproduction conduit (powerless / identity_locked) — trained into the template before encountering contrary evidence
 *   - rival_framework_proposers: Organized resistance (moderate / mobile) — advances override and cascade amendments; holds arbitrage-grade publication exits
 *   - local_chroniclers_of_reform_episodes: Excluded voice (powerless / trapped) — holds the adjudicating records outside the citation network
 *   - comparative_mechanism_analysts: Analytical observer (analytical / analytical) — sees the full structure; collects nothing from the template's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, 0.58).
domain_priors:suppression_score(imposition_pathway_kernel__exogenous_override_reading, 0.55).
domain_priors:theater_ratio(imposition_pathway_kernel__exogenous_override_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__exogenous_override_reading, "Climb-Only Mechanism Taxonomy (M-set) as Assessed from the Exogenous Override Reading").
narrative_ontology:topic_domain(imposition_pathway_kernel__exogenous_override_reading, "historical sociology / state formation / commitment systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__exogenous_override_reading, 'e53e95c0-5785-411b-aa8b-03809712dfda').
narrative_ontology:cs_kernel_codification('e53e95c0-5785-411b-aa8b-03809712dfda', formalized).
narrative_ontology:cs_authority_grounding('e53e95c0-5785-411b-aa8b-03809712dfda', expertise).
narrative_ontology:cs_interpretation_layer_present('e53e95c0-5785-411b-aa8b-03809712dfda').
narrative_ontology:cs_reading_relation('e53e95c0-5785-411b-aa8b-03809712dfda', imposition_pathway_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('e53e95c0-5785-411b-aa8b-03809712dfda', imposition_pathway_kernel__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('e53e95c0-5785-411b-aa8b-03809712dfda', foundational, decreed_compliance_constitutes_displacement).
narrative_ontology:cs_axiom_status(decreed_compliance_constitutes_displacement, holdable).
narrative_ontology:cs_axiom_grounding('e53e95c0-5785-411b-aa8b-03809712dfda', decreed_compliance_constitutes_displacement, empirically_contingent).
narrative_ontology:cs_axiom('e53e95c0-5785-411b-aa8b-03809712dfda', secondary, taxonomy_requires_exhaustive_mechanism_cells).
narrative_ontology:cs_axiom_status(taxonomy_requires_exhaustive_mechanism_cells, holdable).
narrative_ontology:cs_axiom_grounding('e53e95c0-5785-411b-aa8b-03809712dfda', taxonomy_requires_exhaustive_mechanism_cells, instrumental).
narrative_ontology:cs_reference_frame('e53e95c0-5785-411b-aa8b-03809712dfda', override_inclusive_mechanism_map).
narrative_ontology:cs_drift_state('e53e95c0-5785-411b-aa8b-03809712dfda', contemporary_discipline, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e53e95c0-5785-411b-aa8b-03809712dfda', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, climb_paradigm_incumbents).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, peer_review_gatekeepers).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, decree_first_case_specialists).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, policy_lesson_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, graduate_students_in_historical_sociology).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, graduate_students_in_historical_sociology).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, rival_framework_proposers).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__exogenous_override_reading, universal_fringe_adoption_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior comparative-historical scholars whose theories, textbooks, and accumulated career capital rest on the universal fringe-adoption template. Citation centrality, invited lectures, and canonical status flow to the framework's architects. Leaving would mean recasting a lifetime of work; professional identity and the framework have fused.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, climb_paradigm_incumbents, beneficiary,
    institutional, generational, identity_locked, global).

% Editors and referees at the field's flagship journals. They decide which mechanism claims pass review, routinely returning override-cell proposals as 'taxonomy inflation,' and they administer enforcement machinery they did not build. Their standing is tied to their venues' prestige, which rests on the established framework; moving to lesser venues would cost them the gatekeeping role itself.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, peer_review_gatekeepers, agenda_setter,
    institutional, biographical, constrained, global).

% Area-studies historians of Meiji Japan, Republican Turkey, and similar episodes whose dense archives show compliance arriving with the decree, ahead of any detectable popular adoption. To publish in the main venues they must narrate their evidence as a 'compressed climb with invisible fringe' or accept peripheral outlets; their area expertise ports poorly to adjacent fields, so exit is costly.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, decree_first_case_specialists, payer,
    moderate, biographical, constrained, national).

% Development agencies, governance-reform teams, and institutional designers who import mechanism lessons from historical sociology. The dominant synthesis tells them imposed change fails without grassroots groundwork — or conceals the episodes where decree sufficed — shaping program design and country strategies. Alternative syntheses exist, but the climb literature crowds syllabi, summaries, and evidence reviews.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, policy_lesson_consumers, payer,
    organized, generational, constrained, global).

% Doctoral trainees who learn the climb template as the field's grammar before meeting contrary evidence. The shared vocabulary makes early work publishable and seminars legible; the cost arrives later, as careers spent unlearning a coding scheme. Professional identity forms around the template during training, before any stance is chosen.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, graduate_students_in_historical_sociology, payer,
    powerless, immediate, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__exogenous_override_reading, graduate_students_in_historical_sociology, beneficiary).

% Scholars advancing override-cell and cascade amendments to the taxonomy. They absorb above-average rejection rates and 'unnecessary complexity' objections at flagship venues, but keep arbitrage routes — interdisciplinary journals, computational social science venues, preprint servers — that let them publish around the gate at real career cost.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, rival_framework_proposers, payer,
    moderate, biographical, mobile, global).

% Village record-keepers, temple and parish registrars, and local newspaper archives of the reform eras. Their dense, dated records could settle whether popular adoption preceded or followed the decrees, but they were never consulted when the taxonomy was built; their accounts circulate as period color, not as mechanism evidence, and they hold no seat in the debate.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, local_chroniclers_of_reform_episodes, excluded,
    powerless, generational, trapped, regional).

% Methodologists and meta-analysts who study how mechanism taxonomies shape cumulative knowledge across the social sciences. They watch the three-reading contest from outside it, collect nothing from the template's operation, and can name the structure — coordination value, distortion costs, gatekeeping — without a stake in which reading wins.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, comparative_mechanism_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__exogenous_override_reading, climb_paradigm_incumbents).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared mechanism vocabulary that lets displacement episodes across periods, regions, and domains be coded, compared, and aggregated; without a common template, case studies cannot accumulate into comparative findings.
% TRANSFER_FUNCTION: Moves analytic authority and publication success from scholars holding decree-first evidence toward scholars whose cases fit the climb template; moves explanatory credit for imposed changes away from state capacity and toward diffuse grassroots dynamics; moves policy-relevant counterfactuals about what change requires from decree-feasible to groundwork-required.
% ABSENT_VOICES: Local chroniclers and non-Anglophone historiographies of the reform episodes — village record-keepers, temple registrars, local presses whose timelines could adjudicate fringe presence — sit outside the citation network that built the taxonomy. The historical actors' own testimony about why they complied (fear, incentive, conviction) enters the debate only indirectly, through administrative records compiled by the imposing state itself.
% DISAPPEARANCE_RATIONALE: Comparative projects, graduate curricula, coding manuals, and a large literature of cross-case findings depend on the shared grid; overnight removal would strand in-progress comparisons, force ad hoc reclassification, and stall cumulative work until a successor taxonomy — likely one containing the override cell this reading demands — was rebuilt.
% FOUNDING_PROBLEM: Mid-century comparative history faced an unmanageable diversity of change stories — revolutions, reforms, conversions, imposed modernizations — with no common mechanism language; the fringe-adoption climb template was built to give displacement one tractable pathway so heterogeneous cases could be coded and compared at all.
% FOUNDING_PROBLEM_CORROBORATION: Climb incumbents attest the problem is live from inside the benefiting set, so the load-bearing corroboration is external: the hybrid_cascade literature explicitly documents artificial-fringe dynamics that presuppose an override initiator, attesting that pure-climb coding fails on a real case class; dense-archive microhistory publishes decree-first compliance timelines against the template's grain; and decree_first_case_specialists' dissent persists in print despite gatekeeping. Stated plainly: no attestation is fully independent of the disputing camps — the strongest external signal is that rival readings survive in flagship-adjacent venues at all.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_pathway_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__exogenous_override_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the distortion is real and systematic for the decree-first subclass but bounded — most episodes genuinely fit the climb template, so the misclassification taxes a minority of cases and their specialists rather than the whole field. Suppression 0.55: enforcement is reputational and procedural (desk rejections, 'taxonomy inflation' objections, syllabus canon), not prohibitive; alternatives remain publishable at cost. Theater 0.30: most coding work is functional; the performative share is the pseudo-fitting exercise in which decree-first evidence is narrated as 'invisible fringe' to satisfy the template. Accessibility_collapse 0.40: seeing the override possibility does not collapse alternatives — the three-reading contest is live and visible. Resistance 0.60: sustained specialist dissent, rival proposals, and the hybrid compromise all press on the regime. All three tracked series run on ONE shared seven-point grid (t=0..60; interval units are years since the template consolidated in mid-century modernization and comparative politics, roughly 1965-2025). Base_extractiveness rises monotonically — rent-layering, as gatekeeping converts a heuristic into a career filter. Theater_ratio creeps up as fitting exercises multiply. Suppression_requirement is tracked because this story's narrative specifically traces enforcement-capacity change: review norms hardened through the middle decades, then plateaued as resistance matured. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine (via directionality and scope).
 *
 * PERSPECTIVAL GAP:
 *   Payer seats compute a different type than the incumbent seat. From decree_first_case_specialists, the regime operates as a distortion machine their evidence cannot survive honestly. From climb_paradigm_incumbents, the same structure is hard-won cumulative science — the parsimony that made comparison possible at all. From peer_review_gatekeepers, it is quality control against unconstrained taxonomy proliferation. Identity-lock dynamics: the incumbents' exit is identity_locked in the institutional sense — the field's leading figures have 'become' the framework, and recanting means dissolving the theoretical identity a generation of work built. Graduate students are identity_locked through training-era fusion: professional identity forms around the template before any stance is chosen. If the incumbent identity frame broke — say, a flagship journal publishing a decisive dense-archive null result — enforcement would soften faster than codification.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: climb_paradigm_incumbents sit near the beneficiary end (d low) — the template subsidizes their theoretical scope, and identity lock cements rather than threatens their position; peer_review_gatekeepers collect editorial authority at slightly higher d, since they also bear administration costs. Victims: decree_first_case_specialists sit near the target end (d high) — they pay distortion costs with constrained exit, since area expertise is portability-poor; policy_lesson_consumers carry high d damped somewhat by organized power and the option to discount historical lessons entirely, though the dominant-synthesis effect follows them regardless. Graduate students straddle: subsidized vocabulary, inherited distortion. Rival_framework_proposers pay gatekeeping costs but hold arbitrage-grade exit (interdisciplinary and preprint channels), pulling their effective burden below the trapped specialists'. Local chroniclers are excluded rather than targeted — their absence is structural, not extracted-from. Coalition note: specialists and rival proposers could coalition (shared grievance, complementary assets — archives plus methodological standing); the regime's enforcement is calibrated precisely to keep that coalition expensive, since venue gatekeeping separates the two groups' publication channels.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — displacement episodes were incommensurable without a common mechanism language — remains partly live: the majority of cases still code cleanly as climbs, so the template's coordination function has not died. Founding-problem status is therefore contested, not dead, and the mismatch consumer reads contested x world_rearranges: no zombie flag fires. The classification prevents symmetric mislabeling: calling the regime pure extraction erases the genuine coordination most of the field still receives; calling it pure coordination erases the systematic distortion borne by the decree-first subclass and the transfer of analytic authority to the paradigm's center. The tangled_rope claim holds both truths in one structure. Mandatrophy is NOT resolved — the mandate (common mechanism language) still functions; what is contested is its completeness, which is an amendment question, not an obsolescence question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This story instantiates the exogenous_override_reading of imposition_pathway_kernel; how would instantiating the endogenous_climb or hybrid_cascade readings instead change the constraint''s victim set, epsilon, and classification?',
    'Author the two sibling stories over their own referents and compare victim sets and epsilon: the endogenous reading finds no misclassified decree-first subclass (its referent dissolves this story''s victims); the hybrid reading finds partial victims (initiation-stage distortion only).',
    'If the endogenous reading is structurally right, this story''s victims dissolve and epsilon collapses toward coordination-only (rope); if the hybrid reading is right, the distortion burden halves and the arrangement migrates toward rope-with-residual-cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one reading of a contested mechanism-map kernel; sibling readings instantiate different constraints with different victim sets.').

omega_variable(
    archival_silence_vs_absence,
    'Does the absence of pre-decree fringe adoption in Meiji-type episodes reflect genuine absence, or the archival invisibility of peasant-level adoption?',
    'Microhistorical studies exploiting unusually dense local records (village diaries, temple and parish registration, household inventories, local gazetteers) to test for undated pre-decree adoption; density-bounded negative results distinguish absence from silence.',
    'Dense-record absence stabilizes the override cell and raises this story''s epsilon; detected fringe stages recode the episodes as compressed climbs and dissolve the payer seat''s core grievance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archival_silence_vs_absence, empirical, 'Whether ''no fringe stage'' is a finding or an archival artifact.').

omega_variable(
    compliance_commitment_conflation,
    'Did decree-enforced compliance displace commitment, or merely install performed compliance that persisted only while enforcement held?',
    'Track practice retention across enforcement-relaxation episodes (occupation endings, regime transitions, inspection-budget collapses): retention surviving enforcement decay indicates displaced commitment; rapid reversion indicates enforced performance.',
    'If only performance, the override mechanism shrinks to a compliance cell — the M-set amendment this reading demands is smaller than claimed, and the transfer-function account changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_commitment_conflation, conceptual, 'Whether coerced compliance constitutes commitment displacement depends on the working definition of commitment.').

omega_variable(
    override_cell_policy_gaming,
    'Once the override cell is admitted, will it be gamed — invoked to legitimize imposed reforms (''it worked by decree'') and to excuse consent-building that the climb evidence shows matters?',
    'Monitor policy rhetoric and program design after taxonomic adoption; compare invocation patterns against case-level mechanism evidence.',
    'Gaming would convert a corrective taxonomy cell into a rhetorical license — a values-level consequence the classification engine cannot price; it argues for pairing the cell with evidentiary admission criteria rather than mere availability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_cell_policy_gaming, preference, 'Risk that admitting the override cell licenses imposed-change advocacy irrespective of mechanism evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__exogenous_override_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(impo_tr_t0, observed).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(impo_tr_t10, observed).
narrative_ontology:measurement(impo_tr_t20, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(impo_tr_t20, observed).
narrative_ontology:measurement(impo_tr_t30, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(impo_tr_t30, observed).
narrative_ontology:measurement(impo_tr_t40, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement_basis(impo_tr_t40, observed).
narrative_ontology:measurement(impo_tr_t50, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement_basis(impo_tr_t50, observed).
narrative_ontology:measurement(impo_tr_t60, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement_basis(impo_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(impo_be_t0, observed).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(impo_be_t10, observed).
narrative_ontology:measurement(impo_be_t20, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(impo_be_t20, observed).
narrative_ontology:measurement(impo_be_t30, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(impo_be_t30, observed).
narrative_ontology:measurement(impo_be_t40, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(impo_be_t40, observed).
narrative_ontology:measurement(impo_be_t50, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement_basis(impo_be_t50, observed).
narrative_ontology:measurement(impo_be_t60, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(impo_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(impo_su_t0, observed).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(impo_su_t10, observed).
narrative_ontology:measurement(impo_su_t20, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(impo_su_t20, observed).
narrative_ontology:measurement(impo_su_t30, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement_basis(impo_su_t30, observed).
narrative_ontology:measurement(impo_su_t40, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 40, 0.53).
narrative_ontology:measurement_basis(impo_su_t40, observed).
narrative_ontology:measurement(impo_su_t50, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement_basis(impo_su_t50, observed).
narrative_ontology:measurement(impo_su_t60, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement_basis(impo_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__exogenous_override_reading, information_standard).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'how commitment displacement happens' covers three structurally distinct claims, authored as three linked stories. This file authors the exogenous_override_reading; its epsilon referent is the standing climb-only M-set regime as this reading assesses it. The endogenous_climb_reading authors epsilon over the same regime but denies the victim class exists (invisible-fringe rescue); the hybrid_cascade_reading authors epsilon over a regime it reads as initiation-plus-climb, halving the distortion burden. The upstream/downstream structure runs from this reading to the hybrid (admitting the override cell is a precondition for articulating an override-initiated cascade) and in logical opposition to the endogenous reading (existential vs. universal claims about the fringe stage). Each sibling file links back here via its own network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
