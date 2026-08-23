% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__symbolic_confessional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__symbolic_confessional_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: nicene_creed_authority__symbolic_confessional_reading
 *   human_readable: Nicene Creed as Historically Contingent Witness (Symbolic-Confessional Reading)
 *   domain: religious/ecclesial
 *
 * SUMMARY:
 *   In the symbolic-confessional reading, the Nicene Creed is a historically
 *   contingent witness — a fourth-century community's testimony arising from
 *   a particular controversy — whose authority is not intrinsic but conferred
 *   anew wherever communities discern it as faithful and believers assent
 *   through personal faith. This story instantiates that reading as a
 *   standing arrangement: congregations confess voluntarily, discern locally,
 *   permit theological pluralism, and carry the creed into interfaith
 *   conversation as a witness offered rather than a test imposed. The
 *   arrangement's costs fall not on dissenters but on would-be central
 *   authorities, whose prerogative to fix the creed's meaning and sanction
 *   departure is structurally withheld. This is ONE reading of a contested
 *   kernel; the strict-orthodox and liturgical-habituation readings are
 *   separate constraints with their own epsilon, beneficiary/victim
 *   structure, and classification. KEY AGENTS (by structural relationship): -
 *   local_congregations: primary beneficiary (organized/mobile) — receive the
 *   shared confessional vocabulary, discern locally - individual_believers:
 *   primary beneficiary (moderate/mobile) — their personal assent confers the
 *   creed's authority in their case - interfaith_dialogue_partners: secondary
 *   beneficiary (organized/mobile) — meet the creed as witness, not test -
 *   centralized_magisterial_authorities: primary payer
 *   (institutional/identity_locked) — binding and sanctioning prerogative
 *   withheld - creedal_traditionalist_minorities: excluded voice
 *   (organized/identity_locked) — premise ruled out of the discernment they
 *   sit inside - historians_of_doctrine: analytical observer
 *   (analytical/analytical) — compare the reading's claims against the
 *   documentary record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__symbolic_confessional_reading, 0.18).
domain_priors:suppression_score(nicene_creed_authority__symbolic_confessional_reading, 0.18).
domain_priors:theater_ratio(nicene_creed_authority__symbolic_confessional_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__symbolic_confessional_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__symbolic_confessional_reading, "Nicene Creed as Historically Contingent Witness (Symbolic-Confessional Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__symbolic_confessional_reading, "religious/ecclesial").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__symbolic_confessional_reading, 'c45eee18-05d4-4603-9ff0-60665bff3509').
narrative_ontology:cs_kernel_codification('c45eee18-05d4-4603-9ff0-60665bff3509', fixed_text).
narrative_ontology:cs_authority_grounding('c45eee18-05d4-4603-9ff0-60665bff3509', distributed).
narrative_ontology:cs_reading_relation('c45eee18-05d4-4603-9ff0-60665bff3509', nicene_creed_authority__strict_orthodox_reading, forecloses).
narrative_ontology:cs_reading_relation('c45eee18-05d4-4603-9ff0-60665bff3509', nicene_creed_authority__liturgical_habituation_reading, coexists_with).
narrative_ontology:cs_axiom('c45eee18-05d4-4603-9ff0-60665bff3509', foundational, creed_authority_is_derivative_of_discernment).
narrative_ontology:cs_axiom_status(creed_authority_is_derivative_of_discernment, holdable).
narrative_ontology:cs_axiom_grounding('c45eee18-05d4-4603-9ff0-60665bff3509', creed_authority_is_derivative_of_discernment, theological).
narrative_ontology:cs_axiom('c45eee18-05d4-4603-9ff0-60665bff3509', foundational, creed_is_historically_contingent_witness).
narrative_ontology:cs_axiom_status(creed_is_historically_contingent_witness, holdable).
narrative_ontology:cs_axiom_grounding('c45eee18-05d4-4603-9ff0-60665bff3509', creed_is_historically_contingent_witness, empirically_contingent).
narrative_ontology:cs_reference_frame('c45eee18-05d4-4603-9ff0-60665bff3509', contingent_witness_under_communal_discernment).
narrative_ontology:cs_drift_state('c45eee18-05d4-4603-9ff0-60665bff3509', contemporary_pluralist_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('c45eee18-05d4-4603-9ff0-60665bff3509', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, local_congregations).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, individual_believers).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, interfaith_dialogue_partners).
narrative_ontology:constraint_victim(nicene_creed_authority__symbolic_confessional_reading, centralized_magisterial_authorities).
narrative_ontology:constraint_vindicates(nicene_creed_authority__symbolic_confessional_reading, liberty_of_conscience).
narrative_ontology:constraint_vindicates(nicene_creed_authority__symbolic_confessional_reading, doctrinal_revisability).
narrative_ontology:constraint_vindicates(nicene_creed_authority__symbolic_confessional_reading, faith_as_uncoerced_assent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gathered communities that confess the creed together as a received witness from the fourth-century church. They discern its meaning in their own context, may restate or supplement it in their own confessions, and are not answerable to any external office for their reading. What flows to them is a shared vocabulary of faith linking them to other communities and to their own past; what flows from them is voluntary assent. A congregation that discerned differently could revise its confession without sanction.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, local_congregations, beneficiary,
    organized, generational, mobile, local).

% Members who make the creed their own through personal faith; their assent is what gives the words authority in their case. They may question clauses, reinterpret imagery, or hold the creed loosely while remaining full members. Dissent or reinterpretation carries no doctrinal penalty, though the ordinary social weight of congregational expectation remains.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, individual_believers, beneficiary,
    moderate, biographical, mobile, local).

% Communities and traditions outside the credal churches — other Christian bodies, other religions, secular interlocutors — who engage credal communities in conversation. Because the creed is held as a contingent witness rather than a non-negotiable test, they meet it as an object of comparison rather than a demand for assent; they bear no cost from the arrangement and are not bound by it.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, interfaith_dialogue_partners, beneficiary,
    organized, generational, mobile, global).

% Hierarchical teaching offices — papal congregations, synodal hierarchies claiming binding interpretive authority, doctrinal commissions — whose office consists in defining what the creed obligates and sanctioning departure from it. Under this arrangement their binding judgments receive no acknowledgment from communities that discern for themselves; the office's claim to fix the creed's meaning is structurally set aside. An office of this kind cannot adopt the reading without dissolving the claim that constitutes the office.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, centralized_magisterial_authorities, payer,
    institutional, civilizational, identity_locked, global).

% Members and factions within symbolic-reading communities who hold that the creed states binding truth and that deviation matters eternally. In congregations where discernment presupposes the contingent-witness frame, their premise is ruled out before discussion begins; they find themselves without standing in the very discernment processes the arrangement empowers, and their objections are heard as temperament rather than doctrine.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, creedal_traditionalist_minorities, excluded,
    organized, generational, identity_locked, regional).

% Scholars of the creeds' composition and reception who study how authority has been exercised over the text across centuries. They take no side in the contest, hold no stake in the arrangement, and can compare the reading's claims about the creed's historical character against the documentary record.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, historians_of_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__symbolic_confessional_reading, diffuse).
narrative_ontology:fixing_cost_class(nicene_creed_authority__symbolic_confessional_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of common confession across diverse, self-governing communities: a scattered, generation-spanning fellowship shares one historic vocabulary of faith without requiring a central interpreter to fix its meaning, because each community and each believer supplies the assent that gives the words authority.
% TRANSFER_FUNCTION: Moves interpretive authority and status: from would-be central teaching offices to local congregational discernment and individual conscience. Material transfer is minimal — the arrangement moves assent, attention, and the standing to define the faith, not wealth.
% ABSENT_VOICES: Strict-orthodox members inside symbolic-reading communities (creedal_traditionalist_minorities) would object that the creed binds and that eternal stakes are being dissolved, but the discernment frame rules their premise out before they speak. The fourth-century bishops who framed the creed against Arianism also cannot speak: whether their anti-Arian witness is faithfully received or domesticated into mere witness is exactly what they would dispute.
% DISAPPEARANCE_RATIONALE: Communities holding this reading would lose their shared confessional vocabulary overnight: common confession, the ecumenical posture it enables, and the distribution of interpretive authority (congregation and conscience over office) all depend on the arrangement and would reorganize — toward a competing reading of the kernel or fragmentation into unconnected local testimonies.
% FOUNDING_PROBLEM: The arrangement was built against coercive creedalism: the use of the creed as an imposed metaphysical test enforced by sanction — consciences bound, dissenters anathematized, doctrinal police empowered — in the aftermath of the confessional conflicts and the Enlightenment critique of imposed orthodoxy.
% FOUNDING_PROBLEM_CORROBORATION: Historians of doctrine outside the beneficiary set document the coercive-creedalism problem the reading answered — records of anathemas, synodal and inquisitorial sanctions, and confessional-era persecution of dissenters. The strict-orthodox parties corroborate it from the opposite seat: their defense of binding creedal authority confirms the reading arose against exactly that practice, even as they judge the response infidelity rather than liberation.
narrative_ontology:disappearance_verdict(nicene_creed_authority__symbolic_confessional_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__symbolic_confessional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__symbolic_confessional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_creed_authority__symbolic_confessional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__symbolic_confessional_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__symbolic_confessional_reading_tests).
:- end_tests(nicene_creed_authority__symbolic_confessional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18): the arrangement transfers no material goods and binds no conscience; the residual value reflects the ordinary weight of communal confession — voluntary assent still costs something, and congregational expectation is real. Suppression is low (0.18) and authored as a raw structural property, unscaled: no sanction machinery exists, and the soft pressure that remains is the ordinary social gravity of any shared practice (an omega tracks whether that gravity hardens into majoritarian extraction). Theater is low (0.15) but drifting up: as personal faith thins in some mainline congregations, creed recitation persists as habit for a minority of reciters — the drift the measurement series records, and the seam where the liturgical-habituation sibling reading lives. Alternatives remain substantially available (accessibility_collapse 0.35): congregations may restate, supplement, or decline the creed; believers may reinterpret without penalty; rival confessional witnesses circulate freely. Resistance is moderate (0.45): the reading meets sustained resistance from creedal traditionalists inside its own communities and from centralized authorities outside them, but not enough to threaten the arrangement where it is institutionally established. The measurement series run on ONE shared time grid — interval points are roughly five-year spans from the reading's late-nineteenth-century institutional consolidation (point 0) to the present (point 30) — so every tracked metric is authored at every point. No suppression_requirement series is authored: the reading builds no enforcement machinery, so the enforcement picture is static and carried by the scalar. Identity-lock note: the magisterial payer's lock is institutional identity (the office has become its function — its claim to fix creedal meaning constitutes the office); the traditionalist minority's lock is ideological (a worldview in which eternal stakes make loose holding unthinkable). If either frame broke, the payer seat's effective cost would drop sharply and the excluded voice would re-enter the conversation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the congregation's seat the arrangement is the condition of honest confession: the creed means what the community discerns, and no external office can overrule that. From the magisterial seat the same arrangement is dispossession — an office constituted to fix the creed's meaning watches its claim go unacknowledged, and its identity-locked position means it cannot join the reading without dissolving itself. The excluded traditionalist seat experiences a third thing: living inside a discernment process whose premise forecloses theirs, heard as temperament rather than doctrine. The authored claim (rope) is the structural truth from the reading's own lights; the payer and excluded seats should compute a different experience, and that divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real structure: local_congregations and individual_believers sit at the beneficiary end (the arrangement subsidizes their discernment and assent), and interfaith_dialogue_partners benefit incidentally without participating or bearing cost. The single victim declaration — centralized_magisterial_authorities — maps to the arrangement's one real cost: the withholding of binding recognition, borne by offices whose identity_locked exit pushes them toward the full-target end of the derivation. The residual costs are diffuse: no seat captures another's contribution — individuals' assent accrues to the congregational common life the individuals themselves share. That is why gain_flow is authored 'diffuse' as an affirmative checked claim (each named seat was examined; none captures) rather than naming a capturer. Fixing is cheap for whoever could fix it: a congregation can drop or revise the practice at will; nothing locks it in, which is also why neglect of the practice would be transient rather than structurally protected.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline runs in both directions here. Mislabeling the arrangement a snare (creed as instrument of control) would read the strict-orthodox history onto a reading that exists precisely to refuse that history — the founding problem is live, not dead, and the arrangement still does what it was built to do. Mislabeling it a mountain is foreclosed by the reading's own contingency claim: the creed's authority is explicitly NOT a natural or inevitable fact but a conferred one. The live mandatrophy risk is subtler: if communal discernment hardens into majoritarian expectation (the discernment_majoritarian_risk omega tracks this), the arrangement would persist in name while its non-coercive function atrophied — a drift the rising theater series already faintly records. The rope claim is the structural truth under the reading's own lights; the engine's per-seat computation is what detects the drift if it comes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading — the symbolic_confessional_reading — of the nicene_creed_authority kernel; what structurally changes if a sibling reading is instantiated instead?',
    'Compare the sibling constraint stories (nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__liturgical_habituation_reading): the strict-orthodox sibling inverts the topology — dissenting believers and pluralist communities become the targets, sanction machinery activates, and extractiveness rises sharply; the liturgical sibling detaches the creed''s function from personal assent entirely.',
    'The disagreement is located in the SOURCE of the creed''s authority: intrinsic binding ontology versus community discernment versus liturgical performance. Which reading holds determines the victim set, the enforcement surface, and the classification of every seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this story instantiates one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    discernment_majoritarian_risk,
    'Is community discernment under this reading genuinely non-coercive, or does congregational consensus develop its own soft majoritarian pressure — dissenters socially managed into the discerned ''sense of the community''?',
    'Longitudinal study of how minority discernment is treated in symbolic-reading congregations: whether dissenting readings receive standing in the discernment process, or are absorbed, deferred indefinitely, or socially sanctioned.',
    'If majoritarian pressure is systematic, the arrangement''s extractiveness rises above the authored range and its classification drifts toward a hybrid coordination/extraction shape; individual_believers would need a payer secondary role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discernment_majoritarian_risk, empirical, 'Whether distributed discernment stays non-coercive or regenerates local extraction.').

omega_variable(
    magisterial_victim_status,
    'Are centralized magisterial authorities genuinely victims of this arrangement — bearing an imposed cost — or are they merely denied a prerogative this reading holds was never legitimately theirs?',
    'Depends on a prior account of legitimate doctrinal authority, which is the kernel contest itself: within this reading''s own frame the ''cost'' is withdrawal of recognition from an illegitimate claim, not extraction; on a strict-orthodox frame the arrangement strips a legitimate office of its authority.',
    'If the cost is illegitimate-power-denied, the victim declaration is a framing artifact and effective extraction is lower still; if binding creedal authority is legitimate, the arrangement imposes real loss and the payer declaration hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_victim_status, conceptual, 'Whether the payer seat bears extraction or only forfeits an illegitimate claim — reading-indexed victim status.').

omega_variable(
    revisability_boundary,
    'Where does historical contingency end — may the community''s discernment revise the creed''s core confessional content and still confess ''the same faith'' the witness bears?',
    'Observe how symbolic-reading communities handle proposed revisions (creedal restatement, non-theistic reinterpretation of credal clauses): accepted as discernment, or refused as breach of the witness?',
    'Too much revisability dissolves the coordination function (the shared vocabulary ceases to be shared); too little re-imports the binding authority the reading exists to withhold. The boundary determines whether the arrangement holds as steady-state coordination or drifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revisability_boundary, conceptual, 'The reading''s internal boundary between discernment and dissolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__symbolic_confessional_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nicene_creed_symbolic_tr_t0, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(nicene_creed_symbolic_tr_t0, observed).
narrative_ontology:measurement(nicene_creed_symbolic_tr_t6, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement_basis(nicene_creed_symbolic_tr_t6, observed).
narrative_ontology:measurement(nicene_creed_symbolic_tr_t12, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement_basis(nicene_creed_symbolic_tr_t12, observed).
narrative_ontology:measurement(nicene_creed_symbolic_tr_t18, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 18, 0.13).
narrative_ontology:measurement_basis(nicene_creed_symbolic_tr_t18, observed).
narrative_ontology:measurement(nicene_creed_symbolic_tr_t24, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement_basis(nicene_creed_symbolic_tr_t24, observed).
narrative_ontology:measurement(nicene_creed_symbolic_tr_t30, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(nicene_creed_symbolic_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(nicene_creed_symbolic_be_t0, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(nicene_creed_symbolic_be_t0, observed).
narrative_ontology:measurement(nicene_creed_symbolic_be_t6, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 6, 0.14).
narrative_ontology:measurement_basis(nicene_creed_symbolic_be_t6, observed).
narrative_ontology:measurement(nicene_creed_symbolic_be_t12, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 12, 0.16).
narrative_ontology:measurement_basis(nicene_creed_symbolic_be_t12, observed).
narrative_ontology:measurement(nicene_creed_symbolic_be_t18, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 18, 0.17).
narrative_ontology:measurement_basis(nicene_creed_symbolic_be_t18, observed).
narrative_ontology:measurement(nicene_creed_symbolic_be_t24, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 24, 0.18).
narrative_ontology:measurement_basis(nicene_creed_symbolic_be_t24, observed).
narrative_ontology:measurement(nicene_creed_symbolic_be_t30, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement_basis(nicene_creed_symbolic_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nicene_creed_authority__symbolic_confessional_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__symbolic_confessional_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority__liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'the authority of the Nicene Creed' covers at least three structurally distinct arrangements — binding ontological standard (strict_orthodox_reading), liturgical identity marker (liturgical_habituation_reading), and contingent witness under communal discernment (this story). Their epsilon values differ widely: the strict-orthodox arrangement is substantially extractive (sanction-backed binding of conscience), the liturgical arrangement extracts attention and identity rather than assent, and this arrangement is minimally extractive. The strict-orthodox sibling is historically prior, and its enforcement history is the founding problem this reading was built against; the stories are linked so contamination analysis can track, for example, how revival_pressure from the strict-orthodox frame alters this arrangement's operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
