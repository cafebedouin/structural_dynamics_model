% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__lapsed_alternatives_reading, []).

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
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: Keyboard Layout Coordination Equilibrium (Lapsed-Alternatives Reading)
 *   domain: technology_history/economic
 *
 * SUMMARY:
 *   One character map — fixed in outline by the late 1870s and carried to
 *   dominance by Remington's commercial success — coordinates text input for
 *   the literate world. This story authors that arrangement as the
 *   lapsed_alternatives_reading presents it: the map persists because one
 *   shared layout solves a genuine collective-action problem (portable typing
 *   skill, interchangeable hardware, standard training), and rival layouts
 *   lapse because adoption stalls below the critical mass at which switching
 *   becomes worthwhile for any individual before the group. Nothing enforces
 *   the arrangement: no body administers it, no flow is collected under it,
 *   and no alternative is barred — software remapping is free, alternative
 *   boards are purchasable, advocacy is lawful and continuous. Its costs are
 *   a one-time learning charge on every new typist and the forgone promise of
 *   unrealized rivals; its benefit is interoperability delivered to every
 *   participant. This file is one of two linked stories decomposing the
 *   colloquial label 'QWERTY persistence'; the sibling file authors the
 *   active-defense causal story with a different epsilon and beneficiary
 *   structure. See network.dual_formulation_note and
 *   commentary.kernel_context.
 *
 * KEY AGENTS:
 *   - - touch_typists: diffuse beneficiary pool ([moderate]/[constrained]) — hold portable typing skill, paid a one-time learning charge, exit to minority layouts forfeits shared-machine fluency
 *   - - office_employers: coordination consumer ([institutional]/[mobile]) — hire pre-trained typists and buy commodity hardware; consume the standard without maintaining it
 *   - - keyboard_manufacturers: scale-economy collector ([powerful]/[mobile]) — tool for one layout; follow demand rather than set or defend it
 *   - - alternative_layout_communities: excluded advocates ([moderate]/[identity_locked]) — promote Dvorak and Colemak; outside every decision surface; switcher members absorb compatibility friction
 *   - - occupational_health_researchers: excluded evidentiary voice ([organized]/[mobile]) — publish ergonomic evaluations that procurement never reads
 *   - - national_standards_institutes: ratifying observer ([institutional]/[analytical]) — codified the de facto map decades late; administer nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.2).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.1).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "Keyboard Layout Coordination Equilibrium (Lapsed-Alternatives Reading)").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology_history/economic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, 'd4a2dcc4-1d07-4367-ad2b-293b1f820dea').
narrative_ontology:cs_kernel_codification('d4a2dcc4-1d07-4367-ad2b-293b1f820dea', implicit).
narrative_ontology:cs_authority_grounding('d4a2dcc4-1d07-4367-ad2b-293b1f820dea', practice).
narrative_ontology:cs_reading_relation('d4a2dcc4-1d07-4367-ad2b-293b1f820dea', qwerty_persistence__incumbent_preservation_reading, influences).
narrative_ontology:cs_axiom('d4a2dcc4-1d07-4367-ad2b-293b1f820dea', foundational, coordination_value_suffices_for_persistence).
narrative_ontology:cs_axiom_status(coordination_value_suffices_for_persistence, holdable).
narrative_ontology:cs_axiom_grounding('d4a2dcc4-1d07-4367-ad2b-293b1f820dea', coordination_value_suffices_for_persistence, empirically_contingent).
narrative_ontology:cs_axiom('d4a2dcc4-1d07-4367-ad2b-293b1f820dea', secondary, switching_costs_bound_extraction).
narrative_ontology:cs_axiom_status(switching_costs_bound_extraction, holdable).
narrative_ontology:cs_axiom_grounding('d4a2dcc4-1d07-4367-ad2b-293b1f820dea', switching_costs_bound_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('d4a2dcc4-1d07-4367-ad2b-293b1f820dea', benign_coordination_equilibrium).
narrative_ontology:cs_drift_state('d4a2dcc4-1d07-4367-ad2b-293b1f820dea', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('d4a2dcc4-1d07-4367-ad2b-293b1f820dea', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, touch_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, office_employers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers).
narrative_ontology:constraint_vindicates(qwerty_persistence__lapsed_alternatives_reading, coordination_equilibrium_sufficiency).
narrative_ontology:constraint_vindicates(qwerty_persistence__lapsed_alternatives_reading, network_effect_standard_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Learn one character map deeply enough to type without looking; that skill is then portable across every employer, machine, and decade. They were never asked which map would prevail and were never consulted; their benefit is interoperability they did not have to organize to obtain. Adopting a minority layout is possible on their own devices but forfeits fluency on shared machines, colleagues' desks, and public terminals, and demands weeks of retraining.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, touch_typists, beneficiary,
    moderate, biographical, constrained, global).

% Hire typists already fluent in the prevailing map, order commodity hardware, and run training curricula built around it. They consume the standard's coordination output without maintaining it; migrating a workforce to a different layout would mean retraining staff and replacing device fleets for efficiency gains they do not believe are material.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, office_employers, beneficiary,
    institutional, biographical, mobile, global).

% Tool production lines and source components for a single layout, harvesting unit-cost scale and inventory simplicity. They follow demand rather than set it; a manufacturer shipping a different layout at volume would face indifferent or hostile retail channels, which is why none attempts it at scale. They defend nothing — the arrangement defends itself.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, beneficiary,
    powerful, biographical, mobile, global).

% Promote rival layouts — Dvorak since 1936, Colemak since 2006 — through tutorials, forums, and free conversion tools. Members who switch report comfort gains but absorb day-to-day compatibility friction: borrowed keyboards, restrictive IT policies, scarce hardware legends. The communities stand outside every surface where input conventions get decided, because there is no decision surface — the arrangement assembles itself from billions of default purchases. Long-standing members frequently fuse the layout choice with technical identity, which keeps membership stable even as adoption stalls.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_communities, excluded,
    moderate, biographical, identity_locked, global).

% Publish on repetitive-strain injury and input-device ergonomics; successive generations have evaluated alternative layouts and generally found effects too small or too confounded by allegiance to drive procurement. Their recommendations rarely reach purchasing decisions, and the arrangement's persistence owes nothing to their assent or objection.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, occupational_health_researchers, excluded,
    organized, biographical, mobile, global).

% Ratified the de facto map decades after it hardened (ANSI X4.3-1968 and later ISO input-standard work), converting a market fact into a citable standard. They administer nothing about day-to-day layout choice and collect no flow from it; their catalogs gain authority by recording settled practice.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, national_standards_institutes, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__lapsed_alternatives_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence__lapsed_alternatives_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of uniform text input: one agreed character map makes typing skills transferable across machines, employers, and decades; lets hardware and software assume a fixed key-to-code assignment; and lets training curricula standardize. Any single layout solves the problem equally; the value lies in its being solved once, centrally, by convergence — not in the particular map chosen.
% TRANSFER_FUNCTION: Moves little. No ongoing payment flows through the arrangement. What moves is a one-time learning cost onto every new typist — borne by whoever learns, whichever layout prevails — and an opportunity cost onto rival layouts that never reach adoption critical mass. Marginal switching costs fall only on voluntary defectors who try a minority layout.
% ABSENT_VOICES: Historical typists — overwhelmingly women office workers from roughly 1890 to 1950 — absorbed the learning cost with no seat anywhere in the arrangement; nobody consulted them because there was no forum to consult. Ergonomic researchers and alternative-layout advocates sit outside the decision process today: their objections surface in journals and hobbyist forums that procurement never reads, and the arrangement's unanimity is real precisely because dissenting voices were never in the room where defaults get set.
% DISAPPEARANCE_RATIONALE: If the shared layout vanished overnight — every manufacturer shipping arbitrary key assignments — interoperability would collapse immediately: typing skill would stop transferring, typing education would lose its object, and hardware and software assumptions about key positions would break. The coordination problem would re-solve itself around some new common map within years, but only after a massive, wasteful transition; current arrangements demonstrably depend on the existing equilibrium.
% FOUNDING_PROBLEM: Late-nineteenth-century typewriter manufacturing needed buyers to trust an unfamiliar machine: a uniform, learnable key arrangement made typing teachable, operator skills portable between employers, and Remington's product line coherent. The founding problem was market-bootstrapping for mechanical writing.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set by continuing standards-body maintenance activity on input specifications (ISO/IEC 9995 revisions), by the persistent requirement for typing skill across labor markets documented in occupational statistics, and by the ergonomic literature's ongoing engagement with input-device design. None of these corroborating seats collects anything from the particular layout choice.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__lapsed_alternatives_reading_tests).
:- end_tests(qwerty_persistence__lapsed_alternatives_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.20) because no rent flows through the arrangement: its entire cost side is the one-time learning charge every new typist pays for whichever map prevails, plus switching friction borne only by voluntary defectors — epsilon here is switching costs and little else, as the reading specifies. Suppression is minimal (0.10): alternatives are legal, purchasable, and free to configure in software; what remains is coordination gravity, not coercion, and suppression is authored as the raw structural property it is — the engine scales only extractiveness. Theater sits near the floor (0.08): maintenance is behavioral — billions of default purchases — with a thin ceremonial layer of post-hoc ratification. Accessibility_collapse (0.58) is middling: understanding the equilibrium reveals that alternatives are individually accessible but socially expensive, so practical alternatives partially collapse without vanishing. Resistance (0.25) reflects durable but thin advocacy and periodic ergonomic critique; there is no enforcement to resist. The measurement series run on one shared grid (1880–2024, seven points, both tracked metrics at every point). A suppression_requirement series is deliberately omitted: enforcement capacity never varied across the interval because there was none — the static fact lives in base_properties.suppression. Extractiveness peaks in the personal-computing era (1985, 0.26), when retrofitting costs and the path-dependence critique made stickiness feel costly, then declines as software remapping collapsed switching costs. Claim and metrics are authored independently: the claim is rope; the engine certifies from the data.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. Manufacturers experience the map as a scale windfall and would call any alternative a nuisance; employers experience a frictionless market for typing labor; typists experience an invisible good — never asked, yet receiving portability; advocates experience a wall of indifference that, from inside, feels like suppression — the seed of the sibling reading; ergonomic researchers experience repeatedly ignored evidence. One structure, five experiences; the engine derives per-seat classifications from power, exit, and declared position, not from any seat's self-report.
 *
 * DIRECTIONALITY LOGIC:
 *   All three declared beneficiary groups are diffuse participants, so derivation places each near the beneficiary end (low d); none approaches the target end because no victim class is declared — this reading asserts none exists. The excluded and observer seats carry commentary-grade weight only and do not feed directionality. Global spatial scope modestly amplifies effective extraction through verification difficulty, but with base epsilon at 0.20 the scaled result remains near the coordination floor. The only structurally elevated-cost pocket is the identity_locked advocate seat, whose compatibility friction is the price of voluntary defection from a coordination pool rather than extraction aimed at them; if the sibling reading is right that channel behavior constitutes passive defense, that pocket converts to targeted extraction — the boundary is carried by the distribution_gatekeeping_blur omega. Note the structural novelty this reading asserts: there is no agenda_setter seat because no one sets the agenda — the arrangement administers itself through cumulative default choices.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope classification blocks two symmetrical errors. Reading the arrangement as a snare — the sibling reading's strongest form — would fabricate victims and enforcement machinery the historical record lacks, mistaking voluntary switching friction for extraction. Reading it as a mountain would deny constructedness: the map's triumph was contingent on Sholes-era mechanics, Remington's orders, and typing-school economics, and a different equilibrium was reachable. Rope fits: a genuine coordination function, minimal coercive overhead, alternatives unsuppressed, participants net beneficiaries. The founding function — uniform text input — remains live, so no mandatrophy is declared: nothing here is a mandate outliving its purpose, no scaffolding awaits a sunset, and no atrophied shell is being performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persistence_mechanism_attribution,
    'Is this reading''s attribution correct — does coordination value alone explain the map''s persistence, or does part of the causal load belong to the incumbent_preservation_reading''s administered defense?',
    'Historical process-tracing: search the record for coordinated defense activity attributable to incumbent beneficiaries — lobbying, exclusive distribution contracts, procurement foreclosure aimed at rival layouts. Documented defense machinery supports the sibling reading; a verified absence across the interval supports this one.',
    'If defense machinery is found, part of the persistence is administered, the sibling file gains structural ground, and this file''s beneficiary-free, enforcement-free profile understates both extraction and suppression. If absent, the rope profile stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_mechanism_attribution, conceptual, 'Committer-frame uncertainty: which reading of the qwerty_persistence kernel carries the causal load.').

omega_variable(
    true_efficiency_differential,
    'How large is the real-world efficiency and ergonomic differential between the entrenched map and the best alternative layouts?',
    'Pre-registered longitudinal trials with matched trained cohorts, experimenters blinded to allegiance — the historical trials were run by interested parties and cannot settle the question.',
    'A material differential would convert the arrangement''s diffuse cost into a real deadweight burden borne by every typist, raising epsilon and potentially creating a symmetric victim class; a negligible differential confirms the rope profile with epsilon bounded by switching costs alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_efficiency_differential, empirical, 'Size of the true performance gap underlying the arrangement''s residual cost.').

omega_variable(
    distribution_gatekeeping_blur,
    'Does passive channel structure — OEMs shipping one legend only, enterprise procurement inertia — constitute neutral coordination gravity, or a soft form of the incumbent defense the sibling reading posits?',
    'Quasi-experiments from the mechanical-keyboard hobbyist scene and niche OEM offerings: track adoption curves where a credible vendor supplies an alternative at parity pricing. Sustained stall despite parity supply indicates channel gravity doing preservation work; stall even with parity supply confirms threshold arithmetic.',
    'If channel behavior amounts to preservation-by-default, the boundary between the two readings blurs, this file''s suppression score is understated, and part of the advocate seat''s friction becomes targeted exclusion rather than defection cost; if parity-supplied alternatives still stall, coordination-threshold dynamics suffice and the rope profile holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distribution_gatekeeping_blur, conceptual, 'Where neutral network gravity ends and soft incumbent defense begins.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1880, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1880, 0.04).
narrative_ontology:measurement_basis(qwer_tr_t1880, observed).
narrative_ontology:measurement(qwer_tr_t1910, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1910, 0.04).
narrative_ontology:measurement_basis(qwer_tr_t1910, observed).
narrative_ontology:measurement(qwer_tr_t1936, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement_basis(qwer_tr_t1936, observed).
narrative_ontology:measurement(qwer_tr_t1985, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1985, 0.06).
narrative_ontology:measurement_basis(qwer_tr_t1985, observed).
narrative_ontology:measurement(qwer_tr_t1990, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1990, 0.07).
narrative_ontology:measurement_basis(qwer_tr_t1990, observed).
narrative_ontology:measurement(qwer_tr_t2010, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2010, 0.07).
narrative_ontology:measurement_basis(qwer_tr_t2010, observed).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2024, 0.08).
narrative_ontology:measurement_basis(qwer_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1880, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1880, 0.17).
narrative_ontology:measurement_basis(qwer_be_t1880, observed).
narrative_ontology:measurement(qwer_be_t1910, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1910, 0.19).
narrative_ontology:measurement_basis(qwer_be_t1910, observed).
narrative_ontology:measurement(qwer_be_t1936, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1936, 0.23).
narrative_ontology:measurement_basis(qwer_be_t1936, observed).
narrative_ontology:measurement(qwer_be_t1985, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1985, 0.26).
narrative_ontology:measurement_basis(qwer_be_t1985, observed).
narrative_ontology:measurement(qwer_be_t1990, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement_basis(qwer_be_t1990, observed).
narrative_ontology:measurement(qwer_be_t2010, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2010, 0.21).
narrative_ontology:measurement_basis(qwer_be_t2010, observed).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2024, 0.2).
narrative_ontology:measurement_basis(qwer_be_t2024, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence__lapsed_alternatives_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% 'QWERTY persistence' as commonly discussed conflates two causal stories with different epsilon values, beneficiary structures, and failure modes; per the epsilon-invariance principle the label decomposes into two files. This file instantiates the lapsed_alternatives_reading: coordination value explains persistence, alternatives lapse below critical mass, no rent is collected, epsilon is bounded by switching costs (authored 0.20), claimed type rope. The sibling file incumbent_preservation_reading instantiates the active-defense story: capital-protecting incumbents, enforcement, concentrated beneficiaries, higher epsilon. Lineage runs from this reading upstream: the coordination-sufficiency baseline is the null hypothesis any administered-defense claim must beat, so this reading constrains the sibling's legitimacy conditions; where the sibling holds, it raises this file's measured suppression. Both files carry the family link in affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
