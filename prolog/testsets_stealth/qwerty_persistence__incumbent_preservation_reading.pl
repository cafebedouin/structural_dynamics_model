% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Persistence via Incumbent Defense (Incumbent-Preservation Reading)
 *   domain: technological_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   Since the 1870s a single key arrangement has dominated text-entry
 *   hardware across typewriters, terminals, and computers. This story authors
 *   the incumbent-preservation account of that persistence: the arrangement
 *   holds because parties with capital committed to it — manufacturers
 *   amortizing layout-specific tooling, schools selling layout-specific
 *   curricula, millions carrying layout-specific motor skills — actively
 *   maintain it through standards-committee gatekeeping, procurement
 *   screening, factory defaults, and training pipelines, while the costs of
 *   any change land on those who propose or attempt to switch. KEY AGENTS (by
 *   structural relationship): - keyboard_equipment_manufacturers:
 *   agenda-setting incumbent (institutional/constrained) — controls tooling,
 *   defaults, and committee votes - typing_instruction_industry: beneficiary
 *   (organized/constrained) — sells layout-specific training; curriculum
 *   replacement is its worst case - trained_typist_workforce: beneficiary
 *   (moderate/constrained) — motor-skill capital rides on continuity -
 *   office_procurement_departments: dual-positioned beneficiary/payer
 *   (organized/constrained) — buys continuity, pays retraining when
 *   experiments occur - alternative_layout_adopters: primary target
 *   (powerless/trapped) — bears hardware scarcity and shared-machine
 *   exclusion - ergonomic_efficiency_seekers: target (moderate/constrained) —
 *   projected gains never clear the migration hurdle -
 *   alternative_layout_inventors: excluded challenger (moderate/trapped) — no
 *   committee seat, no hardware partner - standards_bodies: co-agenda-setter
 *   (institutional/constrained) — ratifies the written standard; consensus
 *   withholdable by incumbents - technology_historians: analytical observer
 *   (analytical/analytical) — holds the record the argument runs on
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.6).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.62).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Persistence via Incumbent Defense (Incumbent-Preservation Reading)").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technological_history/industrial_standards/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, '738004b8-d7fc-4a6f-8b3a-3438a835dfdb').
narrative_ontology:cs_kernel_codification('738004b8-d7fc-4a6f-8b3a-3438a835dfdb', formalized).
narrative_ontology:cs_authority_grounding('738004b8-d7fc-4a6f-8b3a-3438a835dfdb', extraction).
narrative_ontology:cs_interpretation_layer_present('738004b8-d7fc-4a6f-8b3a-3438a835dfdb').
narrative_ontology:cs_reading_relation('738004b8-d7fc-4a6f-8b3a-3438a835dfdb', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('738004b8-d7fc-4a6f-8b3a-3438a835dfdb', foundational, persistence_requires_active_defense).
narrative_ontology:cs_axiom_status(persistence_requires_active_defense, holdable).
narrative_ontology:cs_axiom_grounding('738004b8-d7fc-4a6f-8b3a-3438a835dfdb', persistence_requires_active_defense, empirically_contingent).
narrative_ontology:cs_axiom('738004b8-d7fc-4a6f-8b3a-3438a835dfdb', secondary, sunk_capital_becomes_standard_guardianship).
narrative_ontology:cs_axiom_status(sunk_capital_becomes_standard_guardianship, holdable).
narrative_ontology:cs_axiom_grounding('738004b8-d7fc-4a6f-8b3a-3438a835dfdb', sunk_capital_becomes_standard_guardianship, empirically_contingent).
narrative_ontology:cs_reference_frame('738004b8-d7fc-4a6f-8b3a-3438a835dfdb', incumbent_guarded_installed_base).
narrative_ontology:cs_drift_state('738004b8-d7fc-4a6f-8b3a-3438a835dfdb', software_remapping_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('738004b8-d7fc-4a6f-8b3a-3438a835dfdb', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, keyboard_equipment_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_typist_workforce).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_instruction_industry).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, office_procurement_departments).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, ergonomic_efficiency_seekers).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_layout_inventors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, office_procurement_departments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, tool, and ship text-entry hardware; hold seats on standards committees and choose factory defaults. Dies, molds, and firmware embody one key arrangement, so a layout change would strand factories of capital. They favor continuity, gate which rival layouts reach mass production, and cite compatibility and quality when the status quo is questioned.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, keyboard_equipment_manufacturers, agenda_setter,
    institutional, generational, constrained, global).

% Sell courses, textbooks, and certificates keyed to one arrangement; curricula and instructors' skills do not transfer to a rival layout, and a wholesale switch would obsolete their product quickly. They press education boards and vocational certifiers to keep the established arrangement in job requirements.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_instruction_industry, beneficiary,
    organized, biographical, constrained, national).

% Carry years of motor-skill investment in one arrangement; pay and employability ride on employer demand for exactly that skill. Retraining means weeks of lost speed and income, so their hands vote for continuity even when they concede a rival arrangement might serve them better.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_typist_workforce, beneficiary,
    moderate, biographical, constrained, global).

% Buy equipment and screen applicants for proficiency in the established arrangement because the surrounding labor market supplies it. A department-level switch breaks compatibility with temporary staff, shared machines, and shared documents; when a unit experiments with a rival layout, this seat pays the retraining bill and usually ends the experiment.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, office_procurement_departments, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, office_procurement_departments, payer).

% People who learned a rival arrangement for comfort, injury avoidance, or speed. Hardware for it is scarce or absent at retail, so they remap software themselves, carry their own boards, and accept being slow on every shared machine, kiosk, or borrowed desk. Leaving the mainstream arrangement is easy; living inside a rival one full-time is not.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters, payer,
    powerless, biographical, trapped, global).

% Employers and team leads who weigh retraining costs against projected productivity and injury-reduction gains from rival arrangements. They find no turnkey migration vendor, no certified trainer pool, and no applicant pool fluent in the alternative, so the projected gains never clear the hurdle and the idea gets shelved.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, ergonomic_efficiency_seekers, payer,
    moderate, biographical, constrained, global).

% Designers of rival arrangements, patented or public-domain, who need manufacturers, schools, and employers to move together and have no seat in the committees that set the written standard. Proposals die in committee, in unanswered licensing letters, or in the absence of a hardware partner willing to tool up.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_layout_inventors, excluded,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, alternative_layout_inventors, payer).

% Ratify and maintain the written specification of the key arrangement that procurement documents reference. Committee membership skews toward incumbent equipment makers, and revisions proceed by consensus that those members can withhold; national variants are absorbed as interpretive notes rather than admitted as rival standards.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, standards_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Trace how the arrangement spread, what early mechanisms required, and which parties funded or resisted challenges to it. Hold no stake in outcomes; supply the archival record that later arguments are built from.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, technology_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__incumbent_preservation_reading, keyboard_equipment_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence__incumbent_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single shared key arrangement lets any trained operator sit at any machine, lets employers hire from a common trained pool, lets manufacturers amortize one set of tooling, and lets schools sell skills that transfer between workplaces. It solves the interoperability problem among keyboards, operators, training, and hiring.
% TRANSFER_FUNCTION: Moves equipment demand and training enrollment toward the established arrangement and away from rivals; places the full cost of any layout change — retraining hours, hardware replacement, curriculum rewrite — on whoever proposes to switch, while incumbent equipment sellers, schools, and skilled operators collect the returns of guaranteed continuity.
% ABSENT_VOICES: Rival-layout designers and the people who would benefit from them — injured workers, speed-focused employers — have no seat in standards committees or procurement processes; their objections surface in niche journals and hobbyist communities after defaults are already set.
% DISAPPEARANCE_RATIONALE: If the maintenance machinery — committee gatekeeping, procurement screening, factory defaults, training pipelines — vanished overnight, rival arrangements would reach the market on equal terms; hardware lines, curricula, and job postings would re-sort around whichever arrangement won on merit, and the installed base would migrate over roughly a generation.
% FOUNDING_PROBLEM: Early typewriters jammed when adjacent typebars struck in quick succession; the arrangement separated frequently paired letters to keep striking sequences slow enough for the mechanism, and gave sales demonstrations a memorable top-row word.
% FOUNDING_PROBLEM_CORROBORATION: Engineering histories of the typewriter and machine-reconstruction studies corroborate both the jam limitation of early mechanisms and its elimination in later designs; no modern keyboard exhibits the problem. Corroboration comes from technology historians and mechanical engineers outside the benefiting parties — though the precise details of the origin story are themselves disputed in the literature, the disappearance of the underlying mechanical problem is not.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__incumbent_preservation_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.60: substantial, because the arrangement imposes real costs on would-be switchers (scarce hardware, absent trainer pools, hiring screens) and this reading prices defensive activity into the base rate, but bounded, because every participant including victims shares the genuine interoperability value. Suppression is authored at 0.62 as a raw structural property — codified standards, procurement gating, default settings, hardware economics — and is deliberately NOT scaled here; only extractiveness is scaled by the engine through directionality and scope. Theater ratio 0.30: efficiency and ergonomic justifications have become progressively post-hoc, layered over a continuity commitment whose original mechanical rationale expired generations ago, but most activity (manufacturing, training, hiring) remains functionally real. Accessibility collapse 0.55: alternatives remain technically reachable (software remapping costs nearly nothing) yet collapse practically once hardware, training, and hiring ecosystems are priced in. Resistance 0.50: a century of advocacy, commissioned studies, and niche adoptions that repeatedly surfaced and never reached decisive scale. The measurement series run on one shared eight-point grid (1878–2024) so every tracked metric is authored at every examined time point; extractiveness climbs with accumulated sunk capital and codification, peaks at the PC-standardization era, and eases slightly as software remapping arrives; suppression_requirement traces enforcement build-up (ratification, defaults) then mild relaxation; theater rises as justifications drift post-hoc. Trajectories are monotone rather than cyclical — no intermittent-reinforcement mechanism is alleged. Series endpoints match the base_properties scalars by construction.
 *
 * PERSPECTIVAL GAP:
 *   From the manufacturer seat the arrangement is stewardship: a working ecosystem whose continuity protects legitimately sunk capital and keeps machines interchangeable. From the adopter seat the same structure is a closed door: hardware that does not exist at retail, terminals that fight their hands, job postings that screen them out. From the procurement seat it is prudent risk management; from the inventor seat it is a wall with no handle. These seats hold comparable nominal standing in some cases (organized buyers versus organized sellers) yet compute differently because their roles, exit options, and relationships to the arrangement differ — the engine derives per-seat classifications from that structural data, and the divergence between the stewardship experience and the closed-door experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for keyboard_equipment_manufacturers, typing_instruction_industry, trained_typist_workforce, and office_procurement_departments: the arrangement subsidizes their existing capital and skill positions, and their constrained exits mean they would not abandon it even if free to. Victim declarations drive high directionality for alternative_layout_adopters (trapped exit pushes them toward the full-target end — every shared machine taxes them), ergonomic_efficiency_seekers (constrained exit moderates but does not erase their target position), and alternative_layout_inventors (trapped: their life's work has no route to market except through the parties the arrangement binds). Standards bodies derive a mid-to-low directionality from their administrative role and incumbent-weighted composition: they collect no direct rents but their institutional position depends on the arrangement they administer. No directionality overrides were needed — the beneficiary/victim declarations plus exit options reproduce the intended structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping early typebars from jamming — died with the mechanism that created it, yet the arrangement persists and the world would rearrange if the maintenance machinery stopped; that combination (dead founding problem, world rearranges) is the signature of a mandate outlived by its arrangement, and it routes through the R5 fields here. The classification prevents two mislabels. Reading the arrangement as pure coordination ignores who pays: adopters, efficiency seekers, and inventors bear real, recurring costs that a coordination-only account renders invisible. Reading it as pure predation ignores what every participant including victims receives: a shared interface that makes skills, machines, and hiring mutually intelligible. The tangled-rope structure holds both facts — genuine coordination function, asymmetric incidence, active enforcement — and the mandatrophy lens explains the hybrid's stability: the coordination value recruits defenders, the defense protects positions, and the original justification survives only as decoration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This file instantiates the incumbent_preservation_reading of the qwerty_persistence kernel; what structurally changes if the lapsed_alternatives_reading is instantiated instead?',
    'Cross-reading comparison in the corpus: compile both readings and diff beneficiary/victim sets, base-rate components, and computed types. The disagreement resolves at the framework-selection level, not inside either file.',
    'Under the sibling reading, non-adopters stop counting as victims, the defensive component drops out of the base rate, and the computed type moves from tangled_rope toward rope; the two files must never be merged into one base rate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of two readings of the QWERTY-persistence kernel; records the reading boundary and the sibling''s structural delta.').

omega_variable(
    defense_mechanism_load_bearing,
    'Is active beneficiary defense load-bearing in the arrangement''s persistence, or does coordination value alone sustain it once critical mass is reached?',
    'Natural experiments where defense relaxed — operating systems shipping rival layouts as built-in toggles, niche hardware runs, remote hiring that ignores layout pedigree — tracked against whether rival-layout share rose in proportion to their measured merits.',
    'If defense is load-bearing, this reading stands with its authored base rate near 0.60; if not, the sibling reading absorbs the phenomenon and effective extraction collapses toward the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defense_mechanism_load_bearing, empirical, 'The locus of the kernel contest: whether persistence runs through defense or through coordination value.').

omega_variable(
    origin_story_accuracy,
    'Did the arrangement actually originate as a jam-avoidance design, as the founding-problem narrative assumes, or is that origin story itself retrospective?',
    'Archival work on Sholes''s prototypes and patent correspondence; replication of jam behavior on reconstructed early mechanisms under alternative arrangements.',
    'If the jam story fails, the arrangement looks coordination-first from birth, the founding problem was never the operative driver, and the genealogy strengthens the sibling''s case; if it holds, the dead-mandate trajectory stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(origin_story_accuracy, empirical, 'Whether the founding problem is real history or convenient myth — the genealogy the R5 answer rests on.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression carried by external machinery (codified standards, procurement screening, hardware economics) or by internalized factors (motor memory, exaggerated perceived switching cost)?',
    'Post-exit trajectory: track people who fully retrained to a rival layout — if their reported barriers stay high after external barriers fall (remappable software, cheap hardware), the internalized share is large.',
    'If internalized, effective suppression exceeds the structural measure and persists even if every institution relaxes; if structural, dismantling the machinery would release the alternatives quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression between external enforcement machinery and carried cognitive habit.').

omega_variable(
    hardware_defense_erosion,
    'Has software-era remapping permanently weakened the hardware pillar of the maintenance machinery, and does the agenda-setting seat now sit with operating-system default setters rather than manufacturers?',
    'Track vendor default policies, enterprise image deployments, and whether any manufacturer has shipped a rival layout as the default on mass-market hardware since software remapping became universal.',
    'If the seat has moved and hardware enforcement is spent, the arrangement''s persistence increasingly rides on habit and defaults alone, pushing the long-run trajectory toward inertial maintenance rather than active upkeep.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hardware_defense_erosion, empirical, 'Whether the maintenance machinery''s center of gravity migrated from factories to software defaults.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 1878, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_incumbent_preservation_tr_t1878, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1878, 0.1).
narrative_ontology:measurement(qwerty_incumbent_preservation_tr_t1900, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(qwerty_incumbent_preservation_tr_t1920, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1920, 0.22).
narrative_ontology:measurement(qwerty_incumbent_preservation_tr_t1940, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1940, 0.28).
narrative_ontology:measurement(qwerty_incumbent_preservation_tr_t1960, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1960, 0.3).
narrative_ontology:measurement(qwerty_incumbent_preservation_tr_t1980, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1980, 0.32).
narrative_ontology:measurement(qwerty_incumbent_preservation_tr_t2000, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 2000, 0.31).
narrative_ontology:measurement(qwerty_incumbent_preservation_tr_t2024, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(qwerty_incumbent_preservation_be_t1878, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1878, 0.25).
narrative_ontology:measurement(qwerty_incumbent_preservation_be_t1900, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(qwerty_incumbent_preservation_be_t1920, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1920, 0.52).
narrative_ontology:measurement(qwerty_incumbent_preservation_be_t1940, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1940, 0.6).
narrative_ontology:measurement(qwerty_incumbent_preservation_be_t1960, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1960, 0.63).
narrative_ontology:measurement(qwerty_incumbent_preservation_be_t1980, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1980, 0.66).
narrative_ontology:measurement(qwerty_incumbent_preservation_be_t2000, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement(qwerty_incumbent_preservation_be_t2024, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_incumbent_preservation_su_t1878, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1878, 0.15).
narrative_ontology:measurement(qwerty_incumbent_preservation_su_t1900, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement(qwerty_incumbent_preservation_su_t1920, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1920, 0.48).
narrative_ontology:measurement(qwerty_incumbent_preservation_su_t1940, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1940, 0.58).
narrative_ontology:measurement(qwerty_incumbent_preservation_su_t1960, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1960, 0.62).
narrative_ontology:measurement(qwerty_incumbent_preservation_su_t1980, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(qwerty_incumbent_preservation_su_t2000, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 2000, 0.66).
narrative_ontology:measurement(qwerty_incumbent_preservation_su_t2024, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence__lapsed_alternatives_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'QWERTY persistence' covers two structurally distinct claims about why the arrangement holds, decomposed per the epsilon-invariance principle. This file authors the incumbent-preservation claim: persistence runs through active upkeep by parties with sunk capital, so the base rate includes defensive costs and the victim set includes blocked adopters and efficiency seekers. The sibling file authors the lapsed-alternatives claim: persistence runs through coordination value alone, alternatives simply failed to reach critical mass, and the base rate sits near the coordination floor. Both stories share the same referent — the standing arrangement — and must keep separate base rates; they are linked here so comparison and contamination analysis can read the pair.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
