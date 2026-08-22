% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__reformist_ijtihad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__reformist_ijtihad, []).

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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Reformist Ijtihad Mandate: Contextual Override on Ethical Conflict
 *   domain: religious/legal-theoretical
 *
 * SUMMARY:
 *   A dominant pattern in modern Islamic legal thought splits over one shared
 *   substrate — the Quran and the transmitted hadith corpus — into competing
 *   readings of who may bind a believer's conscience. This story instantiates
 *   the reformist_ijtihad reading: contextual interpretation is not merely
 *   permitted but mandated wherever a classical ruling collides with
 *   contemporary ethical commitments, human-rights norms, or maslaha, and the
 *   Quran's discernible ethical arc takes hermeneutical priority over
 *   literalist application of transmitted reports. The mandate solves a real
 *   retention problem — millions of believers caught between fidelity and
 *   conscience — while simultaneously transferring interpretive authority
 *   away from the madhhab-trained ulema class whose standing depends on
 *   treating classical consensus as settled. Constraint-family note: the
 *   colloquial label 'Islamic legal authority' decomposes into three
 *   structurally distinct readings with separate epsilon values —
 *   traditionalist_taqlid (categorical madhhab binding; high suppression,
 *   extraction borne by dissenters), state_hybrid (selective adoption steered
 *   by political sovereignty), and this reformist reading (moderate
 *   extraction concentrated on traditional authority structures). Each is
 *   authored as its own file; they are linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   reformist_fatwa_councils: Agenda setter (institutional/mobile) —
 *   administers the contextual-override mandate, credentials preachers,
 *   collects the transferred interpretive authority -
 *   traditional_ulema_class: Primary target (institutional/identity_locked) —
 *   bears dissolution of interpretive monopoly; excluded from the councils
 *   that govern them - progressive_muslim_laity: Primary beneficiary
 *   (organized/identity_locked) — receives a sanctioned path to remain inside
 *   the tradition - muslim_women_family_law_litigants: Beneficiary
 *   (moderate/constrained) — improved personal-status outcomes where the
 *   mandate holds - lgbtq_muslims: Beneficiary (powerless/identity_locked) —
 *   first doctrinal space short of exit-or-concealment -
 *   non_muslim_citizens_under_personal_status: Beneficiary
 *   (powerless/constrained) — equality norms enter the override calculus -
 *   madhhab_trainee_jurists: Secondary target (powerless/constrained) — sunk
 *   training costs as certification value declines -
 *   human_rights_monitoring_bodies: Analytical observer
 *   (analytical/analytical) — documents outcomes that feed the override
 *   calculus
 *
 * KEY AGENTS:
 *   - reformist_fatwa_councils: agenda setter administering the mandate and collecting transferred authority
 *   - traditional_ulema_class: primary target bearing monopoly dissolution, identity-locked to the taqlid system
 *   - progressive_muslim_laity: primary beneficiary, identity-locked to the faith community the mandate keeps open
 *   - muslim_women_family_law_litigants: beneficiary whose relief depends on which faction holds the bench
 *   - lgbtq_muslims: beneficiary of thin, reversible doctrinal space
 *   - non_muslim_citizens_under_personal_status: beneficiary dependent on majoritarian maintenance of the reading
 *   - madhhab_trainee_jurists: secondary target bearing sunk-cost losses
 *   - human_rights_monitoring_bodies: analytical observer feeding the override calculus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.42).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.38).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.42).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Ijtihad Mandate: Contextual Override on Ethical Conflict").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "religious/legal-theoretical").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, 'f63bfe9b-c1c5-4f72-9b23-7dcd94233388').
narrative_ontology:cs_kernel_codification('f63bfe9b-c1c5-4f72-9b23-7dcd94233388', fixed_text).
narrative_ontology:cs_authority_grounding('f63bfe9b-c1c5-4f72-9b23-7dcd94233388', expertise).
narrative_ontology:cs_interpretation_layer_present('f63bfe9b-c1c5-4f72-9b23-7dcd94233388').
narrative_ontology:cs_reading_relation('f63bfe9b-c1c5-4f72-9b23-7dcd94233388', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('f63bfe9b-c1c5-4f72-9b23-7dcd94233388', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('f63bfe9b-c1c5-4f72-9b23-7dcd94233388', foundational, contextual_override_obligatory_on_ethical_conflict).
narrative_ontology:cs_axiom_status(contextual_override_obligatory_on_ethical_conflict, holdable).
narrative_ontology:cs_axiom_grounding('f63bfe9b-c1c5-4f72-9b23-7dcd94233388', contextual_override_obligatory_on_ethical_conflict, instrumental).
narrative_ontology:cs_axiom('f63bfe9b-c1c5-4f72-9b23-7dcd94233388', foundational, quranic_ethical_arc_supersedes_literal_hadith_application).
narrative_ontology:cs_axiom_status(quranic_ethical_arc_supersedes_literal_hadith_application, holdable).
narrative_ontology:cs_axiom_grounding('f63bfe9b-c1c5-4f72-9b23-7dcd94233388', quranic_ethical_arc_supersedes_literal_hadith_application, theological).
narrative_ontology:cs_reference_frame('f63bfe9b-c1c5-4f72-9b23-7dcd94233388', quranic_ethical_trajectory_supremacy).
narrative_ontology:cs_drift_state('f63bfe9b-c1c5-4f72-9b23-7dcd94233388', contemporary_human_rights_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f63bfe9b-c1c5-4f72-9b23-7dcd94233388', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_laity).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, muslim_women_family_law_litigants).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, non_muslim_citizens_under_personal_status).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditional_ulema_class).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, madhhab_trainee_jurists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State-affiliated and independent councils of jurists and academics that decide when a classical ruling yields to contextual reasoning. They issue binding opinions where they hold official standing, appoint and credential preachers, and keep literalist opinions out of official channels. Their members' professional standing rests on the contextual method they administer, and several councils sit inside ministries of religious affairs answering to governments with their own stakes in how Islam is governed.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, reformist_fatwa_councils, agenda_setter,
    institutional, generational, mobile, national).

% Educated urban believers who hold contemporary ethical commitments alongside deep attachment to the tradition. The contextual mandate gives them a sanctioned way to remain practicing Muslims without treating classical rulings on gender, punishment, or religious difference as binding on conscience. Leaving the community entirely carries family and identity costs most are unwilling to pay; their standing inside it depends on interpretations like this one remaining available.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_laity, beneficiary,
    organized, biographical, identity_locked, global).

% Women contesting classical personal-status outcomes — unilateral divorce, unequal inheritance, polygyny, male guardianship — before courts or councils where reformist reasoning has been adopted. Outcomes improve where contextual override is accepted; where traditionalist benches prevail, the same arguments fail and can attract social sanction. Their access to relief runs through whichever interpretive faction controls the bench.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, muslim_women_family_law_litigants, beneficiary,
    moderate, biographical, constrained, national).

% Believers whose existence classical rulings condemn outright. Contextual readings open the first doctrinal space in which they are not required to choose between identity and membership. The space is thin, unevenly recognized, and reversible wherever traditionalist authority returns; most navigate it without institutional protection.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims, beneficiary,
    powerless, biographical, identity_locked, continental).

% Religious minorities whose legal position under classical categories — tribute, differential testimony weight, marriage restrictions — improves when equality norms enter the override calculus. Their standing depends on majoritarian willingness to keep the reformist reading in force; they hold little leverage to defend it themselves.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, non_muslim_citizens_under_personal_status, beneficiary,
    powerless, generational, constrained, national).

% Madhhab-trained jurists whose authority rests on unbroken chains of transmission and on consensus treated as settled. Where reformist councils control appointments and official opinion channels, they lose students, endowments, and standing, and are shut out of the deliberations that govern them. Their training has no marketable equivalent outside the system being displaced; abandoning the taqlid framework would dissolve the authority that constitutes them.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditional_ulema_class, payer,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, traditional_ulema_class, excluded).

% Students investing a decade or more mastering classical legal methodology and substantive doctrine whose certification value declines wherever contextual method controls hiring and credentialing. Some pivot to reformist curricula; those who cannot face sunk-cost traps with narrow exits.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, madhhab_trainee_jurists, payer,
    powerless, biographical, constrained, national).

% International and domestic rights organizations tracking how personal-status and criminal law treat women and minorities. They document outcomes, publish shadow reports, and press governments. They neither issue rulings nor bear the doctrine's costs, and their assessments feed the very override calculus the councils invoke.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, human_rights_monitoring_bodies, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__reformist_ijtihad, reformist_fatwa_councils).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__reformist_ijtihad, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the fidelity crisis: gives modern believers a single sanctioned method for remaining inside the tradition while holding contemporary ethical commitments, so that each believer need not improvise a private departure from classical rulings. Standardizes when contextual override is legitimate, keeping the community's boundary-maintenance coherent under shifting moral conditions.
% TRANSFER_FUNCTION: Moves interpretive authority — the right to bind conscience — from the madhhab-trained ulema class to context-sensitive reasoning administered by reformist councils and academically credentialed scholars; moves legitimacy from consensus-as-settled to alignment with maslaha and human-rights norms; and moves women and religious minorities from subordinate personal-status positions toward formal equality.
% ABSENT_VOICES: Literalist hadith specialists and madhhab traditionalists are largely absent from the councils' deliberations where reformists hold the gates — they would argue that extra-textual ethical standards cannot trigger binding override and that the transmitted corpus constrains the Quran's interpretation, not the reverse. Ordinary believers without academic credentials are also absent: both factions adjudicate their practice over their heads.
% DISAPPEARANCE_RATIONALE: If the mandate vanished overnight, the fidelity crisis it manages would return in force: believers facing conscience conflicts would either exit the tradition or revert to unregulated private departure, traditional authority structures would reclaim interpretive monopoly, and family-law reform projects in several jurisdictions would lose the doctrinal cover that makes them defensible internally.
% FOUNDING_PROBLEM: The collision between classical fiqh rulings on women, minorities, punishment, and apostasy and the post-Enlightenment ethical commitments of educated believers produced mass cognitive dissonance — exit, concealment, or wholesale importation of foreign frameworks. Reformist ijtihad was built to solve the retention-and-integrity problem: keep the community coherent while reconciling doctrine with the moral world its members actually inhabit.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of Islamic law document the nineteenth-century reform lineage (Abduh onward) responding to codification and colonial legal pressure — attesting the problem from outside any benefiting party. Traditionalist ulema polemics concede the fidelity crisis exists even while disputing the remedy, providing adversarial corroboration of the problem itself. Demographic work on religious disaffiliation and concealed nonbelief in Muslim-majority societies independently attests that the underlying tension remains unresolved.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__reformist_ijtihad, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__reformist_ijtihad, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__reformist_ijtihad_tests).
:- end_tests(quran_hadith_substrate__reformist_ijtihad_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end): the mandate genuinely coordinates, but its operation transfers interpretive authority and institutional standing from the ulema class to the councils that administer it — a real, asymmetric transfer running through the same structure that subsidizes progressive constituencies. Suppression (0.38) is materially lower than the traditionalist sibling's profile yet nonzero: where reformists hold official gates, literalist opinions are barred from channels, appointments, and curricula. Theater (0.30) reflects a documented pattern — state-sponsored councils performing modernization rhetorically while core practice lags — partially corrected in recent decades as grassroots reformism turned substantive again. Accessibility collapse is low (0.30): taqlid remains fully available everywhere; the mandate forecloses nothing at the level of private belief. Resistance is high (0.62): traditionalist counter-mobilization is organized, transnational, and recurrently successful. The measurement series run on ONE shared time grid (points 0–130, roughly 1895–2025) with all three metrics authored at every point. The suppression series OSCILLATES rather than drifting monotonically: enforcement intensity tracks which faction controls the institutional gates, alternating with political fortunes — this cycle is itself partly an extraction mechanism, since each swing lets the gate-holding faction recredential its own personnel and defund the other's. Base_properties values describe the interval END state; earlier values appear in the series.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the ulema seat, the mandate is dispossession enforced by rivals — an imposed structure dissolving the presumption that their rulings bind, with identity-lock making exit equivalent to self-annihilation. From the progressive-laity seat, the same structure is subsidy — a sanctioned path that keeps them inside a community they cannot cheaply leave. From the council seat, it is restoration — recovery of an authentic method (ijtihad, maqasid) allegedly suppressed by centuries of taqlid inertia. Coalition dynamics matter on the target side: the ulema are institutionally powerful and, despite historic inter-madhhab rivalry, have repeatedly coordinated transnationally against reformist gatekeeping — visible in the suppression series' swings. Their coalition capacity is the main reason this reading's persistence remains an open question rather than a settled trajectory.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality overrides are needed: the beneficiary/victim declarations plus exit profiles derive the correct d for every seat. The councils sit near the beneficiary end (they collect the transferred authority; mobile exit lets them reposition doctrine). The ulema sit near the full-target end — victim-declared, identity-locked, which amplifies their effective extraction since they cannot abandon the system without dissolving themselves. Trainee jurists are victim-declared with constrained exit (high d, dampened slightly by the pivot option). The four beneficiary groups sit at low d: the mandate subsidizes them with doctrinal space, and their identity-lock or constrained exit makes the subsidy hard to replace. Monitoring bodies are analytical seats outside the chi arithmetic. Scope effects apply modestly: the mandate operates mostly at national scale where verification is feasible, damping the scope amplification a global-scope constraint would carry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so no mandatrophy resolution is declared — the mandate has not outlived its function. The classification discipline matters in both directions here. Labeling the mandate a pure snare would erase the verifiable retention function: believers demonstrably remain inside the tradition under it who would otherwise exit or conceal, and women and minorities obtain outcomes unavailable under the sibling readings. Labeling it a pure rope would erase the authority transfer: the same structure that subsidizes progressive constituencies strips the ulema class of monopoly, students, and endowments, and the gains demonstrably accrue to the council seat (hence gain_flow names that seat, not diffuse). Tangled rope holds both facts. The decay vector to watch is maslaha capture: if public-interest definitions collapse into sponsor preference, the coordination half atrophies while the transfer half persists — the theater-ratio series is the early-warning instrument for exactly that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the quran_hadith_substrate kernel — the reformist_ijtihad reading, instantiated here alone. Is the mandate to override classical rulings on contemporary-ethics conflict a faithful continuation of the substrate''s own trajectory, or a rupture that forfeits the substrate''s authority? The disagreement with the traditionalist_taqlid sibling is located precisely here: whether extra-textual ethical standards can trigger a binding override, and whether the Quran''s ethical arc outranks transmitted hadith.',
    'Comparative reception analysis: track whether fence-sitting believers and neutral scholars accept the reformist genealogy (maqasid reasoning, early-ijtihad precedent) as continuous with the tradition, using survey and seminary-curriculum evidence across jurisdictions.',
    'If rupture-framing prevails, the reading loses the audience it was built to retain and its extraction profile collapses toward irrelevance; if continuity-framing holds, the mandate consolidates as the substrate''s dominant reading and the sibling readings retreat to minority enclaves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the reformist reading is continuous with or a rupture from the shared textual substrate, and where the sibling disagreement is located.').

omega_variable(
    traditionalist_counter_mobilization_durability,
    'How durable is the reformist mandate against traditionalist counter-mobilization, and what structural delta would a traditionalist_taqlid reversal produce?',
    'Longitudinal tracking of appointment patterns, fatwa-channel control, and seminary enrollment across jurisdictions where the two factions alternate in power.',
    'A traditionalist reversal flips the victim and beneficiary sets — the ulema recover monopoly, progressive constituencies lose doctrinal cover — and re-derives this story''s epsilon at the sibling''s higher-suppression, lower-accessibility profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_counter_mobilization_durability, empirical, 'Persistence of the reformist mandate under organized traditionalist opposition.').

omega_variable(
    maslaha_definer_capture,
    'Who defines maslaha (public interest) when the override triggers — and does elite capture of that definition convert the coordination function into a channel for state or clerical preference?',
    'Trace override rulings'' alignment with sponsoring governments'' policy preferences versus independent welfare evidence; compare councils under differing degrees of state dependence.',
    'If maslaha tracks sponsor preferences, effective extraction rises well above the authored 0.42 and the mandate decays toward performance; if it tracks independent welfare evidence, the coordination reading strengthens and the moderate band holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_definer_capture, empirical, 'Whether public-interest definitions are captured by the councils'' sponsors.').

omega_variable(
    institutional_backing_epsilon_variance,
    'Does the mandate''s extractiveness vary primarily with institutional backing — near-zero where reformists hold no gates, substantially higher where they control official channels?',
    'Cross-jurisdiction comparison of enforcement capacity against measured displacement of traditionalist personnel from official religious institutions.',
    'Confirms epsilon sits in the moderate band only under institutional backing; unbacked contexts compute near pure persuasion, backed contexts push toward the upper edge of the band.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_backing_epsilon_variance, empirical, 'Dependence of the mandate''s extraction profile on institutional gatekeeping power.').

omega_variable(
    cs_framing_under_determination,
    'Is the operative kernel the texts themselves (Quran and hadith corpus) or the legitimacy claim layered above them (who may bind a believer''s conscience)? This story is authored under the first framing; under the second, the constraint is a jurisdictional contest over interpretive monopoly and the classification shifts.',
    'Run both framings against the same structural data: if per-seat classifications diverge between framings, the framing choice is doing classificatory work and must be surfaced per story rather than absorbed silently.',
    'Under the jurisdictional framing, the constraint reads as an authority-monopoly contest with higher effective extraction, and the axioms route differently through foreclosure analysis; under the textual framing, the current moderate-band reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Alternative framings of the kernel (texts vs. legitimacy claim) yield different classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 20, 0.14).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 40, 0.22).
narrative_ontology:measurement(qura_tr_t60, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 60, 0.32).
narrative_ontology:measurement(qura_tr_t80, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 80, 0.36).
narrative_ontology:measurement(qura_tr_t100, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 100, 0.34).
narrative_ontology:measurement(qura_tr_t115, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 115, 0.31).
narrative_ontology:measurement(qura_tr_t130, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 130, 0.3).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(qura_be_t60, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(qura_be_t80, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 80, 0.45).
narrative_ontology:measurement(qura_be_t100, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 100, 0.47).
narrative_ontology:measurement(qura_be_t115, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 115, 0.44).
narrative_ontology:measurement(qura_be_t130, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 130, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 20, 0.12).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 40, 0.28).
narrative_ontology:measurement(qura_su_t60, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(qura_su_t80, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 80, 0.36).
narrative_ontology:measurement(qura_su_t100, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 100, 0.3).
narrative_ontology:measurement(qura_su_t115, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 115, 0.41).
narrative_ontology:measurement(qura_su_t130, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 130, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, state_hybrid).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the quran_hadith_substrate kernel. The natural-language label 'Islamic legal authority' conflates three structurally distinct claims with different epsilon values, victim sets, and enforcement profiles: traditionalist_taqlid (upstream, established, extraction borne by dissenters under high suppression), state_hybrid (selective adoption steered by sovereignty), and reformist_ijtihad (this file; moderate extraction concentrated on traditional authority structures). The upstream traditionalist claim is cited as evidence AGAINST this reading, and this reading's doctrinal outputs supply the content state_hybrid selectively adopts — hence the influence edge to state_hybrid and the coexistence edge to traditionalist_taqlid. Each member links to the others via network.affects_constraints; no member averages its epsilon across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
