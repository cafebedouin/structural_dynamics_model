% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Printing Press as Physical Constraint Enabling Continental Theological Dissemination
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'technological_mediation_reading'
 *   of the reformation_composite kernel. It treats the printing press as a
 *   physical Mountain constraint — an information-technology phase shift with
 *   negligible extractiveness (ε ≈ 0.02), near-zero suppression, and maximal
 *   accessibility collapse (once the press exists, hand-copying cannot
 *   compete for scale). The press does not extract; it enables. Its operation
 *   vindicates the proposition that material substrate changes are historical
 *   phase shifts. The other two readings (theological_fragmentation_reading,
 *   political_realignment_reading) are separate constraint stories with their
 *   own ε, stakeholders, and types. This reading's ε refers to the standing
 *   arrangement (the press as physical fact) assessed by this reading's
 *   lights.
 *
 * KEY AGENTS:
 *   - printing_press_technology: Mountain (universal/analytical) — the physical constraint itself
 *   - vernacular_readers: Beneficiary (organized/constrained) — gain access to theological texts in their own language
 *   - latin_literate_elites: Payer (powerful/constrained) — lose gatekeeping monopoly over textual interpretation
 *   - print_shop_operators: Agenda_setter (moderate/mobile) — operate the presses, set production priorities
 *   - imperial_censors: Excluded (institutional/trapped) — their suppression capacity is structurally outpaced by print velocity
 *   - historical_analyst: Observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.02).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.01).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, mountain).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Printing Press as Physical Constraint Enabling Continental Theological Dissemination").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, '705eb2ba-f858-41dd-8da6-bbe6b1df4f5b').
narrative_ontology:cs_kernel_codification('705eb2ba-f858-41dd-8da6-bbe6b1df4f5b', implicit).
narrative_ontology:cs_authority_grounding('705eb2ba-f858-41dd-8da6-bbe6b1df4f5b', distributed).
narrative_ontology:cs_reading_relation('705eb2ba-f858-41dd-8da6-bbe6b1df4f5b', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('705eb2ba-f858-41dd-8da6-bbe6b1df4f5b', reformation_composite__political_realignment_reading, coexists_with).
narrative_ontology:cs_axiom('705eb2ba-f858-41dd-8da6-bbe6b1df4f5b', foundational, material_substrate_primacy_in_historical_phase_shifts).
narrative_ontology:cs_axiom_status(material_substrate_primacy_in_historical_phase_shifts, holdable).
narrative_ontology:cs_axiom_grounding('705eb2ba-f858-41dd-8da6-bbe6b1df4f5b', material_substrate_primacy_in_historical_phase_shifts, empirically_contingent).
narrative_ontology:cs_axiom('705eb2ba-f858-41dd-8da6-bbe6b1df4f5b', secondary, information_technology_determines_diffusion_topology).
narrative_ontology:cs_axiom_status(information_technology_determines_diffusion_topology, holdable).
narrative_ontology:cs_axiom_grounding('705eb2ba-f858-41dd-8da6-bbe6b1df4f5b', information_technology_determines_diffusion_topology, empirically_contingent).
narrative_ontology:cs_reference_frame('705eb2ba-f858-41dd-8da6-bbe6b1df4f5b', pre_print_manuscript_culture).
narrative_ontology:cs_drift_state('705eb2ba-f858-41dd-8da6-bbe6b1df4f5b', post_print_revolution_1500_1550, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('705eb2ba-f858-41dd-8da6-bbe6b1df4f5b', '').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, vernacular_readers).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, latin_literate_elites).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, information_technology_as_historical_phase_shift).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, material_substrate_of_ideational_diffusion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The physical technology of movable-type printing. It sets the agenda by determining what reproduction is physically possible — speed, volume, cost, error rate. It has no agency, no exit, no horizon; it is the Mountain itself, described as an agent for structural completeness.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, printing_press_technology, agenda_setter,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(reformation_composite__technological_mediation_reading, printing_press_technology).

% Literate and semi-literate populations in German, French, English, Dutch, Czech, and other vernaculars. They gain direct access to theological texts (Luther's Bible, Calvin's Institutes, pamphlets) without Latin mediation. Their exit is constrained — they cannot un-read what they have read; the press has already collapsed the alternative of priestly mediation.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, vernacular_readers, beneficiary,
    organized, biographical, constrained, continental).

% Clergy, scholars, and officials whose authority rested on exclusive access to and interpretation of Latin texts. The press extracts their gatekeeping rent by making vernacular equivalents widely available. They cannot exit the constraint — the technology exists regardless of their consent — but they can resist the content (censorship, Index, Counter-Reformation).
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, latin_literate_elites, payer,
    powerful, biographical, constrained, continental).

% Master printers and their workshops in Basel, Strasbourg, Wittenberg, Antwerp, Geneva, Venice, Paris, Lyon. They decide what to print, in what quantities, and for whom. They capture normal commercial profits (coordination benefit) but do not extract rents from the press itself — the technology is available to competitors. Their exit is mobile: a printer can relocate to a more favorable jurisdiction.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, print_shop_operators, agenda_setter,
    moderate, biographical, mobile, regional).

% The Habsburg imperial apparatus and papal Index congregations tasked with suppressing heretical texts. Their suppression capacity is structurally outpaced by print velocity and decentralized production. They are excluded from the coordination function of the press — they cannot use it to suppress more effectively than it disseminates. Their exit is trapped: the institution cannot abandon its censorship mandate without losing legitimacy, but the mandate is physically unenforceable at scale.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, imperial_censors, excluded,
    institutional, generational, trapped, continental).

% The analytical seat that sees the full structure: the press as Mountain, the readers as beneficiaries, the elites as payers, the printers as coordinators, the censors as excluded. This seat does not participate in the constraint; it classifies it.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, historical_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__technological_mediation_reading, diffuse).
narrative_ontology:fixing_cost_class(reformation_composite__technological_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press solves the coordination problem of scalable textual reproduction: one master text → thousands of identical copies → distributed across linguistic and political boundaries → enabling synchronized mass reading of the same content. Before the press, this coordination was physically impossible at continental scale.
% TRANSFER_FUNCTION: The press transfers interpretive authority from Latin-literate gatekeepers (clergy, scholars) to vernacular readers, by making the same text accessible to both without mediation. It transfers production cost from per-copy scribal labor to fixed-cost typesetting + marginal-cost printing. It transfers dissemination speed from months/years (manuscript travel) to weeks (printed book trade routes).
% ABSENT_VOICES: Illiterate populations (peasant majority, urban poor, women in most regions) — they are structurally excluded from the press's direct coordination function because they cannot read. They would experience the Reformation through oral transmission, preaching, and images — a different constraint layer. Also absent: non-European printing traditions (Chinese, Korean movable type) — the press's Mountain status is Eurocentric in this reading.
% DISAPPEARANCE_RATIONALE: If the printing press vanished overnight in 1520, the Reformation as a continental mass movement would collapse. Luther's theses would remain a local academic dispute; vernacular Bibles would not reach thousands of households; Calvin's Institutes would not circulate to Geneva, France, the Netherlands, Scotland, England simultaneously. The theological and political readings would still have local force, but their continental scale — their mass character — would not exist.
% FOUNDING_PROBLEM: The founding problem is not a 'problem' in the mandate sense — the press was not built to solve the Reformation. The press was invented c. 1440 for commercial book production (indulgences, calendars, classical texts, legal forms). The Reformation (1517+) is an emergent consequence of the press's pre-existing operation. The 'founding problem' of the press itself was the economics of manuscript production: high cost, low volume, high error, slow speed.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of the book (Febvre & Martin, Eisenstein, Pettegree, Johns) attest that the press's commercial founding problem (manuscript economics) was solved by 1500 — the press was a mature, profitable industry before Luther. The Reformation is a downstream consequence, not the press's purpose. No beneficiary of the press (printers, readers, authors) claims the press was 'for' the Reformation; the claim is made retrospectively by historical analysts.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_composite__technological_mediation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The printing press is a Mountain because its physical operation (movable type, ink, paper, mechanical reproduction) follows from material laws that no party can alter. Its ε ≈ 0.02 reflects only the marginal cost of production/distribution — not extraction from users. Suppression ≈ 0.01 because the press does not coerce; it outpaces suppression. Theater ≈ 0.01 because there is no performative maintenance — the press works or it doesn't. Accessibility collapse ≈ 0.95 because once vernacular printing exists, the alternative (Latin manuscripts, oral transmission) cannot scale to continental dissemination. Resistance ≈ 0.02 because the press faces no organized resistance to its physical operation; resistance targets the *content* it carries, not the press itself. The claimed_type 'mountain' is independent of these metrics — the metrics describe the press's physical operation; the claim states its structural classification.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes per-seat classifications from the structural data. For vernacular_readers (organized/constrained, d ≈ 0.2), the press is a Rope — genuine coordination (access to texts) with minimal extraction. For latin_literate_elites (powerful/constrained, d ≈ 0.7), the press is a Snare — it extracts their gatekeeping rents without their consent, and they cannot suppress it. For print_shop_operators (moderate/mobile, d ≈ 0.5), it is a Rope — they coordinate production and capture normal profits. For imperial_censors (institutional/trapped, d ≈ 0.9), it is a Mountain they cannot influence. The analytical observer sees the press as Mountain from all seats simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. The press has no declared beneficiaries or victims in base_properties (it is a Mountain with no parties). The stakeholders surface assigns roles that the engine uses to derive d: vernacular_readers are beneficiaries (low d), latin_literate_elites are payers (high d), print_shop_operators are agenda_setters (mid d), imperial_censors are excluded (very high d), historical_analyst is observer (d = 0.5). The press itself is the Mountain — it has no directionality; it is the reference frame.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable — the printing press is not a mandate that has outlived its function. It is a physical technology whose operation continues unchanged. The Reformation's theological and political consequences are downstream constraints (the other readings), not mandatrophy of the press.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the printing press a Mountain-type physical constraint that all readings of the Reformation share, or is ''technological mediation'' itself a reading that frames the press as primary?',
    'Compare the structural position of the press across all three readings: if theological_fragmentation_reading and political_realignment_reading treat the press as a contingent accelerant rather than a necessary substrate, then this reading''s claim that the press is the enabling Mountain is a framing choice, not a shared structural fact.',
    'If the press is a shared Mountain, all three readings inherit its ε ≈ 0. If it is a framing choice, this reading''s ε describes the press-as-primary-cause, and the other readings author their own press-constraints with potentially different ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the printing press is a kernel-shared Mountain or a reading-specific framing of the same technology').

omega_variable(
    literacy_causality_direction,
    'Does the printing press cause rising literacy (enabling mass dissemination), or does rising literacy create demand for the press (making literacy the Mountain and the press a response)?',
    'Longitudinal economic history of pre-Reformation literacy rates and book production; cliometric studies of the endogenous vs. exogenous drivers of the print revolution.',
    'If literacy drives the press, the press is not the Mountain — literacy is, and the press is a Rope or Scaffold coordinating literate demand. If the press drives literacy, the press remains the Mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literacy_causality_direction, empirical, 'Causal direction between printing technology and literacy expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 1440, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1440, reformation_composite__technological_mediation_reading, theater_ratio, 1440, 0.01).
narrative_ontology:measurement(refo_tr_t1480, reformation_composite__technological_mediation_reading, theater_ratio, 1480, 0.01).
narrative_ontology:measurement(refo_tr_t1520, reformation_composite__technological_mediation_reading, theater_ratio, 1520, 0.01).
narrative_ontology:measurement(refo_tr_t1560, reformation_composite__technological_mediation_reading, theater_ratio, 1560, 0.01).
narrative_ontology:measurement(refo_tr_t1600, reformation_composite__technological_mediation_reading, theater_ratio, 1600, 0.01).

% Extraction over time
narrative_ontology:measurement(refo_be_t1440, reformation_composite__technological_mediation_reading, base_extractiveness, 1440, 0.01).
narrative_ontology:measurement(refo_be_t1480, reformation_composite__technological_mediation_reading, base_extractiveness, 1480, 0.01).
narrative_ontology:measurement(refo_be_t1520, reformation_composite__technological_mediation_reading, base_extractiveness, 1520, 0.02).
narrative_ontology:measurement(refo_be_t1560, reformation_composite__technological_mediation_reading, base_extractiveness, 1560, 0.02).
narrative_ontology:measurement(refo_be_t1600, reformation_composite__technological_mediation_reading, base_extractiveness, 1600, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1440, reformation_composite__technological_mediation_reading, suppression_requirement, 1440, 0.01).
narrative_ontology:measurement(refo_su_t1480, reformation_composite__technological_mediation_reading, suppression_requirement, 1480, 0.01).
narrative_ontology:measurement(refo_su_t1520, reformation_composite__technological_mediation_reading, suppression_requirement, 1520, 0.01).
narrative_ontology:measurement(refo_su_t1560, reformation_composite__technological_mediation_reading, suppression_requirement, 1560, 0.01).
narrative_ontology:measurement(refo_su_t1600, reformation_composite__technological_mediation_reading, suppression_requirement, 1600, 0.01).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, information_standard).
narrative_ontology:boltzmann_floor_override(reformation_composite__technological_mediation_reading, 0.02).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).

% DUAL FORMULATION NOTE:
% The reformation_composite kernel decomposes into three constraint stories: this reading (technological_mediation) authors the press as Mountain (ε ≈ 0.02); theological_fragmentation_reading authors competing soteriologies as Tangled Ropes (coordination of belief communities + extraction of conformity); political_realignment_reading authors state-church settlements as Tangled Ropes or Snares (coordination of political legitimacy + extraction of dissent). The press Mountain structurally enables both downstream constraints by collapsing the accessibility of mass dissemination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
