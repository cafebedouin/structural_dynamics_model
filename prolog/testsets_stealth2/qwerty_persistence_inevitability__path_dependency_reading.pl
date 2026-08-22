% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__path_dependency_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: qwerty_persistence_inevitability__path_dependency_reading
 *   human_readable: QWERTY Persistence as Accident-Seeded Path Dependence (Path-Dependency Reading)
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   By the 1890s the QWERTY letter arrangement — seeded onto Remington
 *   typewriters in the 1870s for reasons historians still dispute — had
 *   become the de facto standard for English typing. This story authors the
 *   path_dependency_reading of that persistence: the arrangement holds
 *   because each new cohort learns what employers already expect,
 *   manufacturers build what buyers already demand, and schools teach what
 *   employers require — a self-reinforcing loop ignited by contingent initial
 *   conditions, with no seat administering it, collecting from it, or bearing
 *   a concentrated cost. The efficiency question (whether rival layouts such
 *   as Dvorak are materially better) is treated as a diffuse, unclaimed
 *   externality: if the loss exists, it falls on everyone in trivial
 *   per-capita slices and accrues to no one. Epsilon's referent is the
 *   standing arrangement — QWERTY's de facto monopoly — assessed by this
 *   reading's own lights (diffuse deadweight, no capture), never the
 *   rival-layout regime this reading's critics would prefer. The sibling
 *   reading (strategic_lock_in_reading) is a separate constraint in a
 *   separate file with its own beneficiary/victim geometry; it is not folded
 *   into this one. KEY AGENTS (by structural relationship): -
 *   trained_typists: diffuse cost-bearers (powerless/constrained) — inherit
 *   the layout through training; per-capita burden trivial and unorganized -
 *   keyboard_hardware_manufacturers: demand-responsive producers
 *   (institutional/arbitrage) — collect stable-demand scale economies without
 *   engineering persistence - typing_education_providers: transmission
 *   intermediaries (moderate/constrained) — sell instruction in the incumbent
 *   layout - keyboard_standards_bodies: reactive codifiers
 *   (institutional/constrained) — ratify incumbency in published standards
 *   without enforcing adoption - alternative_layout_advocates: outside the
 *   conversation (organized/trapped) — maintain rival layouts, hold no seat
 *   in standard-setting - path_dependence_scholars: analytical observers
 *   (analytical/analytical) — document and dispute the persistence mechanism
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.13).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.08).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.13).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Persistence as Accident-Seeded Path Dependence (Path-Dependency Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, '0fbb7ffe-7ba8-4d09-bbcd-d86d5d4d7509').
narrative_ontology:cs_kernel_codification('0fbb7ffe-7ba8-4d09-bbcd-d86d5d4d7509', distributed).
narrative_ontology:cs_authority_grounding('0fbb7ffe-7ba8-4d09-bbcd-d86d5d4d7509', practice).
narrative_ontology:cs_reading_relation('0fbb7ffe-7ba8-4d09-bbcd-d86d5d4d7509', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('0fbb7ffe-7ba8-4d09-bbcd-d86d5d4d7509', foundational, persistence_without_strategic_design).
narrative_ontology:cs_axiom_status(persistence_without_strategic_design, holdable).
narrative_ontology:cs_axiom_grounding('0fbb7ffe-7ba8-4d09-bbcd-d86d5d4d7509', persistence_without_strategic_design, empirically_contingent).
narrative_ontology:cs_axiom('0fbb7ffe-7ba8-4d09-bbcd-d86d5d4d7509', foundational, initial_conditions_fix_the_outcome).
narrative_ontology:cs_axiom_status(initial_conditions_fix_the_outcome, holdable).
narrative_ontology:cs_axiom_grounding('0fbb7ffe-7ba8-4d09-bbcd-d86d5d4d7509', initial_conditions_fix_the_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('0fbb7ffe-7ba8-4d09-bbcd-d86d5d4d7509', accident_seeded_spontaneous_standardization).
narrative_ontology:cs_drift_state('0fbb7ffe-7ba8-4d09-bbcd-d86d5d4d7509', post_fable_of_the_keys_debate, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('0fbb7ffe-7ba8-4d09-bbcd-d86d5d4d7509', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__path_dependency_reading, keyboard_hardware_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__path_dependency_reading, typing_education_providers).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__path_dependency_reading, trained_typists).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__path_dependency_reading, path_dependence_theory).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__path_dependency_reading, increasing_returns_economics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Learn the QWERTY layout because every employer, school, and device assumes it; carry the learning cost once and a lifetime of small frictions thereafter. Individual escape exists — operating systems ship alternative layouts — but switching means losing fluency on every shared machine and marking oneself as unusual in hiring. Costs are spread across hundreds of millions of users in slices too small to organize around; no body represents typists on layout choice.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, trained_typists, payer,
    powerless, biographical, constrained, global).

% Produce keyboards with QWERTY legends because that is what buyers expect; the legend itself is cosmetic and retoolable at negligible cost. Collect the stable-demand and scale-economy benefits of a universal standard without having invested in creating or defending it. If demand shifted to another layout, product lines would follow within a production cycle.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, keyboard_hardware_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Sell instruction in the layout employers recognize; curricula, certifications, and course materials are built around the incumbent arrangement. Revenue follows the standard's dominance rather than causing it; teaching a rival layout would render the product unemployable for students.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, typing_education_providers, beneficiary,
    moderate, generational, constrained, global).

% Publish the layout in interface and interoperability standards, ratifying what practice has already settled. Adoption is not enforced — compliance follows market expectation — and no procedural venue exists for evaluating whether a different layout would serve users better. Institutional relevance flows from documenting the incumbent.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, keyboard_standards_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Maintain and promote rival layouts such as Dvorak and Colemak; won inclusion in major operating systems decades ago but never adoption. Must themselves use QWERTY to work and communicate, bearing the arrangement they argue against. No seat in the standards process; objections surface in academic journals and enthusiast communities without decisional effect.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, alternative_layout_advocates, excluded,
    organized, biographical, trapped, global).

% Document and dispute the persistence mechanism: one lineage (David and successors) reads the record as contingency plus increasing returns; another (Liebowitz and Margolis) disputes both the efficiency loss and the severity of the lock-in. Hold no stake in the arrangement's operation; their product is the interpretive contest itself.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, path_dependence_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__path_dependency_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__path_dependency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single shared letter layout makes typing skill portable across machines, employers, and generations: any trained typist operates any keyboard, hardware interoperability needs no negotiation, and instructional materials amortize across the whole user population.
% TRANSFER_FUNCTION: Under this reading, essentially nothing transfers to any seat: each new cohort of learners pays adaptation effort into maintaining the shared standard, and that effort accrues to no recipient — the arrangement's cost side is diffuse, unowned deadweight rather than a flow from payers to beneficiaries.
% ABSENT_VOICES: Alternative-layout advocates and ergonomic researchers object to the incumbency but hold no seat in the venues where keyboard standards are published; standards bodies ratify existing practice without a standing process for weighing layout efficiency. Future cohorts of typists — who will pay the learning cost — are likewise absent from every decision that extends the arrangement to them.
% DISAPPEARANCE_RATIONALE: Remove the persistence dynamic overnight and layout choice becomes contestable: keyboard products, typing curricula, and hiring expectations would reorganize over a transition decade as layouts competed on measured efficiency; the installed base of trained muscle memory — the largest single asset the arrangement maintains — would begin depreciating immediately.
% FOUNDING_PROBLEM: Separating frequently struck typebars on 1870s lever-operated typewriters, whose mechanical jams penalized adjacent-key sequences; the arrangement attributed to solving that problem (and to convenient sales demonstrations) then propagated through manufacturing, schooling, and hiring.
% FOUNDING_PROBLEM_CORROBORATION: No benefiting-party attestation exists to discount under this reading, since the reading denies benefiting parties. External corroboration of the dead status: technology-history archival scholarship (the Yasuokas' patent and trade-press reconstruction) and empirical economics (Liebowitz and Margolis) both attest that the original design rationales — jam avoidance, sales-demo convenience — are dead or mythical; neither school attests any live problem the arrangement currently solves beyond its own reproduction.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__path_dependency_reading, 0.13, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, ExtMetricName, E),
    domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored low (0.13 at interval end) because this reading denies any recipient: the arrangement's cost side is unclaimed deadweight — inherited switching costs and any unrealized efficiency — not a transfer to a capturing seat. Suppression is minimal (0.08): nothing forbids rival layouts; every major operating system ships alternatives; the arrangement holds through preference structure, not enforcement, so no suppression_requirement series is authored (static enforcement picture; the scalar carries it). Theater_ratio rises across the interval (0.05 to 0.28) as the arrangement's functional rationale decayed — the mechanical-jam problem died with direct-print mechanisms — while confirmatory ratification and mythologized origin stories grew as a share of maintenance activity. Accessibility_collapse (0.75) is high but not total: individual escape exists (operating-system-level remapping), yet collective alternatives collapse socially, since an individual switcher forfeits compatibility with everyone else. Resistance is low (0.10): advocacy persists at the margin but no organized movement contests the standard. The claimed_type is mountain per this reading's own thesis — technological inevitability given the initial seed — authored independently of the metrics; where the engine's computed type diverges (note that the receipt surface's prohibitive-fix/diffuse-gain combination is piton-shaped), that divergence is the measurement the corpus exists to take. All temporal series share one grid (t = 0, 25, 50, 75, 100, 125, 150).
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the manufacturer seat (arbitrage-grade exit) the arrangement is barely visible as a restraint: keycap legends are cosmetic, switching cost near zero, so the seat computes near the beneficiary end with negligible experienced burden. From the trained-typist seat (constrained exit) the same arrangement is an inherited tax paid in learning time and forfeited efficiency — real but diffuse, too small per capita to organize against. From the advocate seat (trapped) the arrangement is an exclusion: advocates must use the layout they oppose to remain economically legible. The scholar seat sees the full loop — seed, reinforcement, ratification — and is the only seat positioned to compare readings. No seat experiences concentrated extraction, which is this reading's core structural assertion.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared: this reading's structural delta is precisely the absence of an extraction geometry, so the derivation chain runs on power and exit atoms alone. Manufacturers (institutional, arbitrage) derive near the beneficiary end — the standard subsidizes them with stable demand they did not engineer. Typists (powerless, constrained) derive toward the target end, but their per-capita burden is trivial, so effective extraction stays low even at elevated d. Advocates (organized, trapped) derive the highest d among non-analytical seats — they bear the arrangement while opposing it — but hold no lever. Standards bodies sit near symmetric: they expend ratification effort and collect institutional relevance. No directionality overrides are authored: the power/exit fallbacks already place each seat correctly, and overrides keyed only to power atoms could not distinguish the multiple institutional seats without misrepresenting them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — separating frequently struck typebars on 1870s lever machines — died with the technology that posed it; the arrangement outlived its function by roughly a century. Authored honestly, that yields the mismatch signature (founding_problem_status=dead crossed with disappearance_verdict=world_rearranges), which the apparatus reads as a capture/zombie flag cross-checked against the computed piton/theater path. This reading contributes a third possibility the binary misses: persistence without either a captor (snare-shaped) or a negligent administrator (piton-shaped) — millions of decentralized renewals, each individually rational, aggregating into an arrangement nobody chose and nobody can be charged with maintaining. The mandatrophy question here therefore tests whether 'inevitability' is a description or an alibi: if a feasible revision point existed and was declined, the mountain claim is an alibi for neglect; if none existed, the mismatch flag fires on a constraint that was never anyone's to fix.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_strategic_vs_accidental,
    'This story instantiates the path_dependency_reading of kernel qwerty_persistence_inevitability; does the historical record support accident-driven persistence without strategic beneficiaries, or the sibling strategic_lock_in_reading''s manufacturer-engineered reinforcement?',
    'Archival study of manufacturer conduct across the standardization era: training-partnership contracts, typewriter-industry standardization agreements, deliberate compatibility withholding. Absence of such evidence sustains this reading; presence shifts the referent to the sibling constraint.',
    'If strategic engineering is documented, this reading''s no-beneficiary/no-victim structure is wrong-headed: the sibling''s arrangement (manufacturer capture, enforced lock-in, high epsilon) becomes the correct referent and this file''s classification voids.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_strategic_vs_accidental, empirical, 'Reading-choice omega: accidental path dependence vs engineered lock-in.').

omega_variable(
    dvorak_superiority_empirical_status,
    'Does a material efficiency gap between QWERTY and rival layouts actually exist, or is the presumed loss illusory (the Liebowitz-Margolis challenge)?',
    'Controlled longitudinal retraining studies with proper experimental controls (the historical Dvorak experiments lacked them); modern keystroke-dynamics and text-entry research.',
    'If no material gap exists, the arrangement''s cost term vanishes: epsilon drops toward the pure coordination floor and the mountain reading strengthens; a confirmed large gap raises the deadweight burden this reading must attribute to persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_superiority_empirical_status, empirical, 'Whether the diffuse efficiency loss attributed to the standard is real.').

omega_variable(
    spontaneous_order_vs_maintainable_convention,
    'Is QWERTY persistence a spontaneous-order regularity (natural given the seed, with no maintainable choice point) or a constructed convention that some coalition could revise at bounded cost?',
    'Counterfactual decision-point analysis: identify historical moments when coordinated migration was feasible (wartime retraining programs, early personal-computer keyboard standardization) and assess why revision failed — structural impossibility versus unattempted coordination.',
    'Spontaneous-order resolution certifies the mountain claim; a demonstrated feasible-but-untaken revision path reclassifies toward rope (failed coordination) or piton (neglect), collapsing the inevitability premise this reading rests on.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spontaneous_order_vs_maintainable_convention, conceptual, 'Naturality ambiguity: emergent regularity vs revisable convention.').

omega_variable(
    unclaimed_deadweight_or_hidden_capture,
    'Is the efficiency burden genuinely unclaimed deadweight (no seat receives it), or does some seat quietly capture value from the incumbent standard''s persistence?',
    'Incidence tracing: quantify whether any actor''s returns exceed competitive benchmarks attributable specifically to incumbency of the layout (training-industry margins, standard-essential positioning advantages).',
    'Hidden capture would falsify gain_flow=''diffuse'', raise effective extraction, and pull the classification away from this reading''s mountain claim toward tangled_rope or snare structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unclaimed_deadweight_or_hidden_capture, empirical, 'Audit of the diffuse-externality claim: deadweight vs concealed capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_pathdep_tr_t0, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(qwerty_pathdep_tr_t0, observed).
narrative_ontology:measurement(qwerty_pathdep_tr_t25, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 25, 0.07).
narrative_ontology:measurement_basis(qwerty_pathdep_tr_t25, observed).
narrative_ontology:measurement(qwerty_pathdep_tr_t50, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 50, 0.09).
narrative_ontology:measurement_basis(qwerty_pathdep_tr_t50, observed).
narrative_ontology:measurement(qwerty_pathdep_tr_t75, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 75, 0.12).
narrative_ontology:measurement_basis(qwerty_pathdep_tr_t75, observed).
narrative_ontology:measurement(qwerty_pathdep_tr_t100, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 100, 0.16).
narrative_ontology:measurement_basis(qwerty_pathdep_tr_t100, observed).
narrative_ontology:measurement(qwerty_pathdep_tr_t125, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 125, 0.21).
narrative_ontology:measurement_basis(qwerty_pathdep_tr_t125, observed).
narrative_ontology:measurement(qwerty_pathdep_tr_t150, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 150, 0.28).
narrative_ontology:measurement_basis(qwerty_pathdep_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(qwerty_pathdep_be_t0, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement_basis(qwerty_pathdep_be_t0, observed).
narrative_ontology:measurement(qwerty_pathdep_be_t25, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 25, 0.03).
narrative_ontology:measurement_basis(qwerty_pathdep_be_t25, observed).
narrative_ontology:measurement(qwerty_pathdep_be_t50, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement_basis(qwerty_pathdep_be_t50, observed).
narrative_ontology:measurement(qwerty_pathdep_be_t75, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 75, 0.07).
narrative_ontology:measurement_basis(qwerty_pathdep_be_t75, observed).
narrative_ontology:measurement(qwerty_pathdep_be_t100, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 100, 0.09).
narrative_ontology:measurement_basis(qwerty_pathdep_be_t100, observed).
narrative_ontology:measurement(qwerty_pathdep_be_t125, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 125, 0.11).
narrative_ontology:measurement_basis(qwerty_pathdep_be_t125, observed).
narrative_ontology:measurement(qwerty_pathdep_be_t150, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 150, 0.13).
narrative_ontology:measurement_basis(qwerty_pathdep_be_t150, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_inevitability__path_dependency_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__path_dependency_reading, strategic_lock_in_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'QWERTY persistence': the path_dependency_reading (this file) authors the arrangement as accident-seeded spontaneous standardization — diffuse unclaimed deadweight, no beneficiary extraction, no victim set, epsilon near the coordination floor. The strategic_lock_in_reading authors the same observable label as manufacturer-engineered lock-in — concentrated capture, enforced exclusion of alternatives, high epsilon. Same label, different constraints: epsilon diverges because the readings disagree about who, if anyone, receives the arrangement's costs as gain. The accident narrative is the historically prior account that the strategic account cites as the thing to be corrected, hence the upstream-to-downstream edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
