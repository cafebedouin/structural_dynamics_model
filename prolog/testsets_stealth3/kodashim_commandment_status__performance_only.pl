% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Continued Mass Study of the Sacrificial Order Without Altar (Performance-Only Reading)
 *   domain: religious_studies/halakhic_theory/commitment_systems
 *
 * SUMMARY:
 *   No altar has existed since 70 CE on which the sacrificial order could
 *   operate, and this reading takes the commandment's validity to be
 *   contingent on exactly that substrate: no altar, no operative commandment
 *   — what the curriculum sustains is a husk. The standing arrangement under
 *   assessment is the continued mass allocation of academy hours,
 *   examinations, commentaries, review cycles, and celebratory completions to
 *   the sacrificial tractates, nineteen centuries after their practice
 *   context ended. Assessed by this reading's own lights, the arrangement's
 *   operative justification is void, and its persistence is carried by
 *   institutional reproduction: each cohort is socialized into valuing
 *   material it can never apply, and the diverted capacity accrues to the
 *   academies' product and the scholarly establishment's standing. The story
 *   authors epsilon for THAT arrangement — the standing study investment —
 *   not for any alternative allocation, and not for the arrangement as any
 *   other reading would price it. KEY AGENTS (by structural relationship): -
 *   yeshiva_students: Primary target (powerless/identity_locked) — bear the
 *   diverted formative years - rabbinic_scholarly_establishment: Primary
 *   beneficiary (institutional/identity_locked) — collects standing and
 *   credential rents - yeshiva_institutions: Agenda-setter and secondary
 *   beneficiary (institutional/arbitrage) — administers the allocation,
 *   receives the flows - communal_educational_funders: Secondary target with
 *   incidental benefit (organized/constrained) — finances the fidelity they
 *   purchase - applied_halakhic_practitioners: Excluded voice
 *   (moderate/trapped) — would absorb redirected effort, has no seat -
 *   halakha_historians: Analytical observer (analytical/analytical) — sees
 *   the full structure from outside
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.72).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.56).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.72).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Continued Mass Study of the Sacrificial Order Without Altar (Performance-Only Reading)").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious_studies/halakhic_theory/commitment_systems").

domain_priors:requires_active_enforcement(kodashim_commandment_status__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, 'af0cbf29-603b-4cb7-8c1a-27a3e2644f2f').
narrative_ontology:cs_kernel_codification('af0cbf29-603b-4cb7-8c1a-27a3e2644f2f', fixed_text).
narrative_ontology:cs_authority_grounding('af0cbf29-603b-4cb7-8c1a-27a3e2644f2f', lineage).
narrative_ontology:cs_interpretation_layer_present('af0cbf29-603b-4cb7-8c1a-27a3e2644f2f').
narrative_ontology:cs_reading_relation('af0cbf29-603b-4cb7-8c1a-27a3e2644f2f', kodashim_commandment_status__messianic_deferral, forecloses).
narrative_ontology:cs_reading_relation('af0cbf29-603b-4cb7-8c1a-27a3e2644f2f', kodashim_commandment_status__study_as_performance, forecloses).
narrative_ontology:cs_axiom('af0cbf29-603b-4cb7-8c1a-27a3e2644f2f', foundational, commandment_validity_requires_altar_existence).
narrative_ontology:cs_axiom_status(commandment_validity_requires_altar_existence, holdable).
narrative_ontology:cs_axiom_grounding('af0cbf29-603b-4cb7-8c1a-27a3e2644f2f', commandment_validity_requires_altar_existence, conventional).
narrative_ontology:cs_axiom('af0cbf29-603b-4cb7-8c1a-27a3e2644f2f', foundational, suspended_commandment_imposes_no_present_duty).
narrative_ontology:cs_axiom_status(suspended_commandment_imposes_no_present_duty, holdable).
narrative_ontology:cs_axiom_grounding('af0cbf29-603b-4cb7-8c1a-27a3e2644f2f', suspended_commandment_imposes_no_present_duty, deontological).
narrative_ontology:cs_axiom('af0cbf29-603b-4cb7-8c1a-27a3e2644f2f', secondary, textual_continuity_is_not_performance).
narrative_ontology:cs_axiom_status(textual_continuity_is_not_performance, holdable).
narrative_ontology:cs_axiom_grounding('af0cbf29-603b-4cb7-8c1a-27a3e2644f2f', textual_continuity_is_not_performance, conventional).
narrative_ontology:cs_reference_frame('af0cbf29-603b-4cb7-8c1a-27a3e2644f2f', altar_conditioned_performative_validity).
narrative_ontology:cs_drift_state('af0cbf29-603b-4cb7-8c1a-27a3e2644f2f', post_temple_textual_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('af0cbf29-603b-4cb7-8c1a-27a3e2644f2f', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, rabbinic_scholarly_establishment).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, yeshiva_institutions).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, yeshiva_students).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, communal_educational_funders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, communal_educational_funders).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, unbroken_oral_transmission_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, total_corpus_mastery_ideal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior scholars and heads of the great academies whose standing rests on demonstrated command of the entire transmitted corpus, the sacrificial order included. Mastery of the hardest and least applicable material functions as the field's highest credential, and their authority claims invoke an unbroken chain of teaching reaching back to the era when the altar operated. Departing from the arrangement would mean repudiating the basis of their own standing, so none do; they defend the full curriculum in rulings, endorsements, and fundraising appeals.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, rabbinic_scholarly_establishment, beneficiary,
    institutional, generational, identity_locked, global).

% Academies and their boards assign the tractate cycle, set examinations, and certify mastery; they decide each generation's curricular weight for the sacrificial order and could re-weight it at will. They collect tuition and earmarked donations justified in part by curricular fidelity to the whole received tradition, and their graduates' credentials are the institution's product. The cost of changing course is reputational — charges of truncating the tradition — weighed against the freed capacity a redirection would release.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__performance_only, yeshiva_institutions, beneficiary).

% Adolescents and young men spend their formative years, roughly fourteen through twenty-five and beyond, working through the sacrificial tractates alongside the rest of the cycle. The material cannot be practiced anywhere in the world as they study it; the years carry an opportunity cost against applied legal training, livelihood preparation, or other scholarship. Opting out of the sacrificial order within their institutions marks a learner as selective, and the surrounding marriage market and communal esteem price breadth of mastery, so the realistic exit is leaving the track altogether rather than trimming it.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_students, payer,
    powerless, biographical, identity_locked, regional).

% Lay donors, community funds, and parent households finance the academies. Part of what their money purchases is assurance that nothing of the received tradition has been dropped — a fidelity they cite approvingly when giving. The same fidelity directs their funds toward sustaining non-performable material at scale rather than toward applied training, welfare, or vocational ends they also care about; their exit is limited because the fidelity norm defines respectable giving.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, communal_educational_funders, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__performance_only, communal_educational_funders, beneficiary).

% Decisors, kosher-certification professionals, teachers, and court registrars work in the fields where halakhic knowledge meets daily use. They report chronic shortages of thoroughly trained colleagues while curriculum hours concentrate on material with no practice context; they have no seat in the academies' curricular deliberations, and their staffing needs surface only as private complaints, never as agenda items.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, applied_halakhic_practitioners, excluded,
    moderate, biographical, trapped, national).

% Academic historians of rabbinic literature trace how a functioning altar-centered legal system became, after 70 CE, a purely textual one, and they document the machinery — review cycles, examinations, celebratory completions — that reproduces the textual form generation after generation. They hold no stake in the curriculum and can see the whole structure from outside.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, halakha_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__performance_only, yeshiva_institutions).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps the sacrificial corpus in living interpretive circulation across generations and provides the community's most demanding shared analytic discipline; guarantees that no tractate of the received corpus goes dark.
% TRANSFER_FUNCTION: Moves the formative years and attention of students, and a share of communal educational funds, away from live-application domains (applied legal training, livelihood preparation, welfare) and into the maintenance of a corpus with no practice context; converts that investment into credentialed graduates, institutional product, and scholarly standing.
% ABSENT_VOICES: Applied-halakhic practitioners and students who would prefer redirected curricula are not seated in curriculum deliberation; their preferences surface only as private attrition and staffing complaints, never as agenda items.
% DISAPPEARANCE_RATIONALE: If the mass allocation vanished overnight, curricula would re-weight toward applied and livelihood-relevant material, specialist cadres would shrink to preservation scale, the status market for sacrificial-order mastery would reprice sharply downward, and the academies' fidelity-based fundraising claims would lose their object — the communal economy of learning would visibly reorganize.
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE, the sacrificial order dropped out of communal practice, and the study apparatus was built to keep its laws alive in memory and interpretation so the corpus would not be lost.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of rabbinic literature — outside the benefiting establishment — attest that sacrificial performance ceased in 70 CE and never resumed, and that the invention of printing removed the preservation rationale mass memorization once served; the surviving allocation tracks institutional momentum rather than any ongoing practice need. The establishment itself attests the opposite (that readiness or fulfillment justifies continued investment), so corroboration for the dead-function reading rests on the historical and technological record, not on any beneficiary seat.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72) because the diverted quantity is large — multi-year blocks of the strongest learners' formative capacity — and the performative return is nil under this reading: no altar exists, so nothing studied can be done. Suppression (0.56) is soft-structural rather than coercive: curriculum assignment, examination gates, funder expectations, and a marriage market that prices breadth do the enforcing; force plays no role. Theater_ratio (0.62) is high because a large share of observable activity — review cycles, completion celebrations, simulated-procedure disputation — maintains continuity rather than preparing any possible act; the residual functional share is real but bounded (corpus custody and analytic formation). Accessibility_collapse (0.45): alternatives have not collapsed — applied-legal tracks, divergent curricula, and vocational paths exist — but choosing them carries marking costs inside the community, so exit is narrowed, not closed. Resistance (0.38): critique surfaces episodically from pragmatist strands and outside observers and is absorbed without curricular change.
 *   
 *   Claim and metrics are independent: claimed_type tangled_rope states my structural judgment that a real residual coordination function (custody of the corpus, the community's most demanding shared analytic discipline) coexists with asymmetric extraction routed through prestige gates; the metrics describe operation, not the claim. If the establishment honestly restated its warrant as custody-plus-discipline rather than commandment-status, the allocation would shrink but not vanish — the goods are real, which is why this is not a snare; but the warrant actually offered is void under this reading, which is why it is not a rope.
 *   
 *   All three tracked series run on one shared nine-point grid (1950–2026). The interval narrative: postwar rebuilding expanded enrollments; the massified daily-page cycle reached the sacrificial order in the 1970s–80s, exposing every learner to it; consolidation from the 1980s hardened the breadth norm; print and digital access made the preservation rationale redundant while the allocation kept growing — hence the parallel rises in extractiveness, theater, and enforcement intensity. suppression_requirement is tracked because the enforcement picture genuinely changed over the interval (hardening, not decay).
 *   
 *   Coalition note: students are individually powerless but numerous; coordinated curricular preference is conceivable, yet identity-locking fragments any would-be coalition, since each member's standing depends on the same breadth norm the coalition would relax. Suppression here is a raw structural property; only extractiveness is scaled downstream by directionality and scope. Coordination type is declared identity_coordination because the mass-participation function is boundary and breadth signaling — specialists would suffice for custody — and the known gaming risk for this type is accepted knowingly: the coupling here does concentrate costs on powerless agents at global scope, which the framework treats as diagnostically meaningful rather than excused by the complexity offset.
 *
 * PERSPECTIVAL GAP:
 *   From the establishment seat the arrangement is the tradition's crown: proof that nothing received was dropped, and the source of its own credentialing — the same structure computes as subsidy-side from that position. From the student seat it is foreclosed youth: years priced against alternatives that choosing would mark them for declining. The academies sit administratively above a tradeoff they themselves set, experiencing neither pole directly. Outside observers see the mechanism whole — a practice context that ended in antiquity, reproduced as text by institutional momentum. The engine computes these per-seat classifications from the structural data; the divergence between the establishment and student seats is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (rabbinic_scholarly_establishment, yeshiva_institutions) derive directionality near the beneficiary end; the establishment's identity_locked exit pins it deepest, since its standing is fused to the arrangement it benefits from. Declared victims (yeshiva_students, communal_educational_funders) derive directionality toward the target end; students' identity_locked exit places them nearest full-target, because their foreclosed alternatives are constitutive of self-concept rather than merely costly. Funders carry both roles and should compute mid-range. Global spatial scope raises verification difficulty, scaling effective extraction modestly upward for the target seats. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms already separate the seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping a performable sacrificial system alive in communal competence — died with the Temple, and print completed the job by making preservation a shelf problem rather than a population problem. The arrangement persists on successor functions. The classification guards both mislabelings: reading the arrangement as pure rope (transmission doctrine as sufficient warrant) launders the diverted-capacity cost; reading it as pure snare (all of it waste) erases the genuine custody and analytic-training goods that survive independently of the void warrant. Tangled_rope holds the residue and the diversion together, which is what a husk is: shell and former kernel, one dead, one still feeding. The R5 mismatch (founding_problem_status dead x disappearance_verdict world_rearranges) is the formal zombie flag this reading's husk diagnosis predicts, and it cross-checks against the elevated theater_ratio.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story is one reading of the kodashim_commandment_status kernel; would the sibling readings (messianic_deferral, study_as_performance) restructure the beneficiary/victim set and epsilon over the same standing arrangement?',
    'Compare the sibling stories'' authored epsilon and victim sets over the identical referent (the standing study arrangement); divergence localizes the disagreement to the kernel''s present normative status.',
    'Under messianic_deferral the study is readiness-investment and the victim set contracts sharply; under study_as_performance the study IS the commandment''s performance and measured extraction collapses toward coordination cost. Only this reading prices the identical investment as extraction with no performative return.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; epsilon is reading-indexed over a fixed referent.').

omega_variable(
    restoration_contingency,
    'If the Temple were restored, would the suspended commandment re-kernel and retroactively justify the accumulated study investment as option-value?',
    'Not resolvable by data — the event is eschatological; resolvable only as a framing choice about how to price contingent-future obligations against present costs.',
    'A credible restoration pathway would reclassify diverted capacity as investment and cut effective extraction; its absence leaves the husk assessment standing and the accumulated centuries priced as sunk diversion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_contingency, preference, 'Contingent-future pricing of the suspended commandment.').

omega_variable(
    preservation_cadre_sufficiency,
    'What cadre size actually suffices to keep the sacrificial corpus in living interpretive circulation, versus the mass allocation the arrangement sustains?',
    'Comparative analysis of small-specialist traditions (academic Talmud programs, boutique kollelim) for corpus vitality against mass-curriculum communities of comparable textual output.',
    'If a small cadre suffices, the mass-allocation excess is unbuffered diversion and the arrangement trends toward pure extraction; if mass participation is constitutive of interpretive vitality, the coordination floor rises and the tangled structure firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_cadre_sufficiency, empirical, 'Scale requirement of corpus preservation versus mass allocation.').

omega_variable(
    opportunity_cost_attribution,
    'How much of a student''s diverted formative capacity is attributable to the kodashim allocation specifically, rather than to the general depth-first pedagogy it rides inside?',
    'Curriculum-hour accounting across yeshiva tracks with differential kodashim weighting, controlling for total learning load and outcomes on applied-legal measures.',
    'Low attribution shrinks the victim magnitude and softens the extraction reading; high attribution confirms the diverted-resource diagnosis and sharpens the payer seat''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_attribution, empirical, 'Attribution of diverted scholarly capacity to the kodashim component.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression structural (curriculum control, funder expectations, marriage-market valuation of breadth) or internalized (students and scholars hold total-corpus mastery as the highest good independent of external pressure)?',
    'Post-exit valuation trajectory: graduates who leave the community and shed the structural pressures — does the total-corpus ideal persist in their private study priorities and their advice to juniors?',
    'If internalized, effective suppression exceeds the structural measure and persists after exit — the constraint travels inside its targets; if structural, removal of the gates would release the diverted capacity quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    cs_framing_underdetermination,
    'Is the yeshiva curriculum the right frame for this commitment-system constraint, or is the load-bearing structure the unbroken-transmission legitimacy claim layered above the curriculum?',
    'Test whether removing the curriculum while retaining the transmission doctrine (or vice versa) preserves the arrangement''s authority function; whichever survives alone is the true kernel-carrier.',
    'Framing on the doctrine rather than the curriculum shifts the kernel''s codification profile and could move the classification toward the doctrine''s own authority-extraction structure rather than the curriculum''s resource structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative CS framings: curriculum versus transmission-doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 1950, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t1950, kodashim_commandment_status__performance_only, theater_ratio, 1950, 0.4).
narrative_ontology:measurement_basis(koda_tr_t1950, observed).
narrative_ontology:measurement(koda_tr_t1960, kodashim_commandment_status__performance_only, theater_ratio, 1960, 0.43).
narrative_ontology:measurement_basis(koda_tr_t1960, observed).
narrative_ontology:measurement(koda_tr_t1970, kodashim_commandment_status__performance_only, theater_ratio, 1970, 0.46).
narrative_ontology:measurement_basis(koda_tr_t1970, observed).
narrative_ontology:measurement(koda_tr_t1980, kodashim_commandment_status__performance_only, theater_ratio, 1980, 0.49).
narrative_ontology:measurement_basis(koda_tr_t1980, observed).
narrative_ontology:measurement(koda_tr_t1990, kodashim_commandment_status__performance_only, theater_ratio, 1990, 0.52).
narrative_ontology:measurement_basis(koda_tr_t1990, observed).
narrative_ontology:measurement(koda_tr_t2000, kodashim_commandment_status__performance_only, theater_ratio, 2000, 0.55).
narrative_ontology:measurement_basis(koda_tr_t2000, observed).
narrative_ontology:measurement(koda_tr_t2010, kodashim_commandment_status__performance_only, theater_ratio, 2010, 0.58).
narrative_ontology:measurement_basis(koda_tr_t2010, observed).
narrative_ontology:measurement(koda_tr_t2020, kodashim_commandment_status__performance_only, theater_ratio, 2020, 0.61).
narrative_ontology:measurement_basis(koda_tr_t2020, observed).
narrative_ontology:measurement(koda_tr_t2026, kodashim_commandment_status__performance_only, theater_ratio, 2026, 0.62).
narrative_ontology:measurement_basis(koda_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t1950, kodashim_commandment_status__performance_only, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement_basis(koda_be_t1950, observed).
narrative_ontology:measurement(koda_be_t1960, kodashim_commandment_status__performance_only, base_extractiveness, 1960, 0.49).
narrative_ontology:measurement_basis(koda_be_t1960, observed).
narrative_ontology:measurement(koda_be_t1970, kodashim_commandment_status__performance_only, base_extractiveness, 1970, 0.53).
narrative_ontology:measurement_basis(koda_be_t1970, observed).
narrative_ontology:measurement(koda_be_t1980, kodashim_commandment_status__performance_only, base_extractiveness, 1980, 0.57).
narrative_ontology:measurement_basis(koda_be_t1980, observed).
narrative_ontology:measurement(koda_be_t1990, kodashim_commandment_status__performance_only, base_extractiveness, 1990, 0.61).
narrative_ontology:measurement_basis(koda_be_t1990, observed).
narrative_ontology:measurement(koda_be_t2000, kodashim_commandment_status__performance_only, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement_basis(koda_be_t2000, observed).
narrative_ontology:measurement(koda_be_t2010, kodashim_commandment_status__performance_only, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement_basis(koda_be_t2010, observed).
narrative_ontology:measurement(koda_be_t2020, kodashim_commandment_status__performance_only, base_extractiveness, 2020, 0.71).
narrative_ontology:measurement_basis(koda_be_t2020, observed).
narrative_ontology:measurement(koda_be_t2026, kodashim_commandment_status__performance_only, base_extractiveness, 2026, 0.72).
narrative_ontology:measurement_basis(koda_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t1950, kodashim_commandment_status__performance_only, suppression_requirement, 1950, 0.42).
narrative_ontology:measurement_basis(koda_su_t1950, observed).
narrative_ontology:measurement(koda_su_t1960, kodashim_commandment_status__performance_only, suppression_requirement, 1960, 0.44).
narrative_ontology:measurement_basis(koda_su_t1960, observed).
narrative_ontology:measurement(koda_su_t1970, kodashim_commandment_status__performance_only, suppression_requirement, 1970, 0.46).
narrative_ontology:measurement_basis(koda_su_t1970, observed).
narrative_ontology:measurement(koda_su_t1980, kodashim_commandment_status__performance_only, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement_basis(koda_su_t1980, observed).
narrative_ontology:measurement(koda_su_t1990, kodashim_commandment_status__performance_only, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement_basis(koda_su_t1990, observed).
narrative_ontology:measurement(koda_su_t2000, kodashim_commandment_status__performance_only, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement_basis(koda_su_t2000, observed).
narrative_ontology:measurement(koda_su_t2010, kodashim_commandment_status__performance_only, suppression_requirement, 2010, 0.54).
narrative_ontology:measurement_basis(koda_su_t2010, observed).
narrative_ontology:measurement(koda_su_t2020, kodashim_commandment_status__performance_only, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement_basis(koda_su_t2020, observed).
narrative_ontology:measurement(koda_su_t2026, kodashim_commandment_status__performance_only, suppression_requirement, 2026, 0.56).
narrative_ontology:measurement_basis(koda_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__messianic_deferral).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__study_as_performance).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'status of the sacrificial commandments' decomposes into three structurally distinct claims — performance_only (this file), messianic_deferral, and study_as_performance — each with its own epsilon, beneficiary/victim structure, and classification over the same standing arrangement (continued mass study of the sacrificial order). Upstream/downstream: messianic_deferral is the historically dominant reading; its readiness-framing supplies the justificatory language this reading finds void, so the deferral reading's cultural dominance shapes this reading's operating environment, while this reading's critique erodes the deferral framing's cover in turn. study_as_performance cites the study-as-sacrifice equivalence to occupy the kernel intellectually. All three files link one another via affects_constraints; epsilon values differ by reading while the referent is held fixed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
