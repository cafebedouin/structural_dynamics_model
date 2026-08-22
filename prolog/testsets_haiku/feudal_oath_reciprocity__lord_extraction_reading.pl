% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__lord_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__lord_extraction_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: feudal_oath_reciprocity__lord_extraction_reading
 *   human_readable: Feudal Oath as Maximal Extraction Vehicle (Lord's Reading)
 *   domain: political/legal/economic
 *
 * SUMMARY:
 *   The feudal oath is a contested kernel in medieval political economy. This
 *   constraint instantiates the lord's reading: the oath as a legal framework
 *   authorizing maximal extraction bounded only by the vassal's capacity to
 *   deliver service and the rebellion threshold. Vassals swear personal
 *   loyalty; lords interpret that loyalty as a license to demand whatever
 *   surplus extraction the feudal hierarchy will bear. Ecclesiastical
 *   authorities and vassal solidarity movements contest this reading,
 *   claiming the oath carries reciprocal obligation and moral restraint. This
 *   story models extraction as increasing over the medieval period (0.55 →
 *   0.81 across 500 years) as lords systematize demands and ecclesiastical
 *   restraint weakens. The claim/metric independence rule applies: the
 *   constraint is CLAIMED as snare (pure extraction) and the authored metrics
 *   describe high, rising extraction with active enforcement — they align
 *   here because this reading's own structural logic produces that outcome.
 *   The sibling readings (vassal_coordination_reading,
 *   ecclesiastical_mediation_reading) are separate constraint files with
 *   their own ε values and claim/metric pairs; the network links them.
 *
 * KEY AGENTS:
 *   - nobility_with_vassals: institutional, extraction beneficiary — sets oath terms and enforces maximal demand
 *   - bound_vassals: moderate power, biographical horizon, identity-locked exit — core victims of extraction, constrained by oath oath itself and dependence on land tenure
 *   - serf_populations: powerless, trapped exit — secondary victims, bear labor and harvest obligations with no reciprocal claim
 *   - ecclesiastical_authority: institutional observer, excluded from feudal court enforcement — claims moral standing to restrain extraction but lacks coercive power
 *   - vassal_coalitions: organized, constrained exit — set the rebellion threshold by threatening collective withdrawal, but lack formal seat in oath negotiation
 *   - royal_overlord: institutional observer — theoretically above feudal extraction but often complicit or indifferent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.81).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.79).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Maximal Extraction Vehicle (Lord's Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "political/legal/economic").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, '424a505d-8098-4531-b0a8-e97e8b24770a').
narrative_ontology:cs_kernel_codification('424a505d-8098-4531-b0a8-e97e8b24770a', fixed_text).
narrative_ontology:cs_authority_grounding('424a505d-8098-4531-b0a8-e97e8b24770a', extraction).
narrative_ontology:cs_interpretation_layer_present('424a505d-8098-4531-b0a8-e97e8b24770a').
narrative_ontology:cs_reading_relation('424a505d-8098-4531-b0a8-e97e8b24770a', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('424a505d-8098-4531-b0a8-e97e8b24770a', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('424a505d-8098-4531-b0a8-e97e8b24770a', foundational, lord_extraction_authority_maximal).
narrative_ontology:cs_axiom_status(lord_extraction_authority_maximal, holdable).
narrative_ontology:cs_axiom_grounding('424a505d-8098-4531-b0a8-e97e8b24770a', lord_extraction_authority_maximal, instrumental).
narrative_ontology:cs_axiom('424a505d-8098-4531-b0a8-e97e8b24770a', foundational, vassal_obligation_bounded_by_capacity_and_rebellion_only).
narrative_ontology:cs_axiom_status(vassal_obligation_bounded_by_capacity_and_rebellion_only, holdable).
narrative_ontology:cs_axiom_grounding('424a505d-8098-4531-b0a8-e97e8b24770a', vassal_obligation_bounded_by_capacity_and_rebellion_only, empirically_contingent).
narrative_ontology:cs_reference_frame('424a505d-8098-4531-b0a8-e97e8b24770a', oath_as_extraction_license).
narrative_ontology:cs_drift_state('424a505d-8098-4531-b0a8-e97e8b24770a', late_medieval_systematization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('424a505d-8098-4531-b0a8-e97e8b24770a', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, nobility_with_vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, bound_vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, serf_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms of oath, interprets obligations, enforces compliance through military power and land seizure. The oath gives them a framework to extract whatever surplus the vassal can produce without triggering open rebellion. They justify extraction as the price of protection and governance, though enforcement capacity — not service delivery — determines actual obligation boundaries.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, nobility_with_vassals, agenda_setter,
    institutional, generational, arbitrage, regional).

% Swear personal oath to the lord in exchange for land tenure and theoretical protection. In practice, they bear the core extraction burden: military service on demand, annual payments in cash and kind, hospitality obligations for the lord's retinue, labor duties on lord's demesne. Exit means breach of oath (damnation + forfeiture of land + death); their identity as knights, gentry, or established landholders IS the oath. Extraction rises to whatever the lord demands short of triggering vassal coalition rebellion.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, bound_vassals, payer,
    moderate, biographical, identity_locked, regional).

% Legally bound to the land under the oath framework; they bear labor obligations, harvest tithes, and mill fees without contractual reciprocity claim. The lord's oath binds vassals; the lord's oath does NOT bind the lord to the serfs. Serfs experience the constraint as pure domination with no countervailing obligation.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, serf_populations, payer,
    powerless, biographical, trapped, local).

% Church claims oaths are binding before God and carry moral obligations of restraint and charity. They are excluded from enforcing these claims in secular law (kings and lords control feudal courts); they can excommunicate or refuse sacraments but lack coercive power to reduce extraction. Their moral framing is not seated at the table where actual obligation is adjudicated.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_authority, excluded,
    institutional, generational, constrained, regional).

% Multiple vassals coordinating to withhold service or threaten rebellion. They are excluded from the formal oath structure (oaths are individual, lord-to-vassal); their only leverage is collective withdrawal of service and threat of open revolt. They set the rebellion threshold beyond which extraction becomes unsustainable, but they have no seat in setting the rules that define extraction within the threshold.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, vassal_coalitions, excluded,
    organized, biographical, constrained, regional).

% Crown stands above feudal vassals; can strip lands and titles for violations of royal oath. Crown's authority limits lord extraction only when the crown itself chooses to reduce it (rare, and usually self-interested — the crown wants tax revenue, not vassal welfare). Most extraction passes unchecked as a matter of delegated authority.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, royal_overlord, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__lord_extraction_reading, nobility_with_vassals).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__lord_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Feudal oath structures land tenure, military obligation, and governance hierarchy: a decentralized security system where local military power is bundled with land control and reciprocal loyalty. The oath coordinates who owes what to whom and provides a framework for dispute resolution within the vassalic hierarchy.
% TRANSFER_FUNCTION: Moves wealth (labor, harvests, cash, military service, hospitality) from vassals and serfs to lords; the flow is asymmetric and contingent only on the lord's demand remaining below the rebellion threshold. Unlike the coordination reading (where reciprocal obligations bound extraction), this reading treats the oath as a license to extract whatever the market for rebellion tolerance allows.
% ABSENT_VOICES: Serfs have no voice in the oath structure at all — they are subjects of the land, not parties to it. Ecclesiastical authorities claim moral standing to restrain extraction but are excluded from feudal courts and cannot enforce their claims. Vassal coalitions are structurally prevented from negotiating collectively; the oath is individual and hierarchical, forcing each vassal to face the lord alone.
% DISAPPEARANCE_RATIONALE: If the feudal oath and its enforcement vanished overnight, land tenure would lack a legitimating framework; vassals would renegotiate from roughly equal footing; extraction would collapse to what military coercion alone could sustain without consent. The entire medieval security and allocation system depends on the oath's continued operation.
% FOUNDING_PROBLEM: Post-Roman Europe faced a collapse of centralized military and administrative capacity. Local strongmen offered protection in exchange for land and service; the oath formalized this into a hereditary, transferable system where protection and obligation became recursive up a hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Historians specializing in collapse and early medieval recovery (outside the benefiting nobility) attest the founding protection-function was real and necessary in the 5th–7th centuries. By the 12th–13th centuries, independent economic historians and ecclesiastical critics attest the protection justification has eroded: extraction persists because the oath now privileges lord power, not because protection remains scarce or necessary. The founding problem is dead; the arrangement persists as institutional inertia and enforced hierarchy.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__lord_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__lord_extraction_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__lord_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__lord_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.55 (early medieval, extractive baseline but protection genuinely scarce) to 0.81 (late medieval, extraction optimized but protection no longer justifies the cost). Suppression is high (0.79) and stable because enforcement depends on military hierarchy, oath formality, and identity-lock — the constraint is maintained by active suppression, not by participant preference. Theater_ratio (0.42) reflects that a meaningful share of enforcement activity goes to maintaining the oath's legitimacy (lord's claim to justice, vassal's honor claim) rather than to the actual extraction mechanics; once the oath is sworn, extraction largely follows automatically. The measurement series show accumulating extraction over centuries — lords discovered they could demand more as the system became formalized and ecclesiastical restraint weakened. Accessibility_collapse (0.72) reflects that vassal identity-lock and oath binding make alternatives hard to access, but not impossible (rebellion, flight to cities, religious orders, or waiting for crown intervention). Resistance (0.68) reflects consistent pushback from vassal coalitions, ecclesiastical critics, and (later) peasant revolts — the constraint is actively resisted but survives because the lord's military capacity exceeds any coalition resistance.
 *
 * PERSPECTIVAL GAP:
 *   The lord and the vassal compute different types from the same constraint. From the lord's seat, the oath is genuine coordination: they set clear rules, enforce them consistently, and deliver security in return for service. From the vassal's seat, it is snare: the rules are unilaterally interpreted, enforcement extracts whatever the market for rebellion tolerance allows, and security justification erodes over time. The engine computes per-seat types from power, exit_options, and beneficiary/victim declarations; the wide divergence reflects real structural asymmetry, not analytical error.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality increases monotonically for vassals and serfs as the system matures (early medieval: oath is genuinely reciprocal, d ≈ 0.5; late medieval: extraction dominates, d ≈ 0.8+). For the lord, d stays near zero throughout (extraction beneficiary). For ecclesiastical and coalition seats, d stays high (they bear the cost of restraint they cannot enforce; they are targets of the lord's suppression if they try to reduce extraction, making them structurally vulnerable despite their nominal power).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-Roman military collapse) was genuinely alive in the 5th–7th centuries. By the 12th century, the founding problem was substantially solved — professional knights, fortified settlements, and written law provided security beyond what the feudal oath offered. Yet the oath persisted and extraction rose. This is mandatrophy: the constraint's original function has atrophied, but the extraction machinery remains and amplifies. Ecclesiastical critics and vassal charters attest the shifted function. The constraint persists because lords profit from it and enforcement capacity remains high; it is a zombie function, extraction masked as reciprocal obligation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    obligation_ceiling_mechanism,
    'What structural property sets the upper bound on extraction under the oath framework? Is it the vassal''s actual service capacity, the rebellion threshold, or something else?',
    'Comparative study of oath-breaking events and subsequent lord behavior: if lords who pushed extraction past some threshold faced consistent coalition rebellion and were forced to back down, that threshold is the mechanism; if extraction simply continued without regard to vassal capacity, the ceiling is institutional memory and cultural lag, not structural.',
    'If rebellion threshold is the mechanism, extraction is bounded and predictable — the constraint is more stable snare with a known ceiling. If extraction can rise above service capacity (via serf intensification or debt-shift), the snare lacks a structural bound and approaches absolute predation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_ceiling_mechanism, empirical, 'Whether extraction has a structural upper bound beyond lord will.').

omega_variable(
    kernel_contest__coordination_vs_extraction,
    'Is the feudal oath fundamentally a coordinated reciprocal arrangement (the vassal_coordination_reading) that lords can exceed and abuse, or is it fundamentally an extraction device dressed in reciprocal language (this reading)?',
    'Framing via the alternative readings: coordination_reading claims fixed, bounded obligations sustained by charter text and custom enforcement; lord_extraction_reading claims the oath is a legal pretext for maximal predation bounded only by rebellion cost. The contest is not empirical — both readings match the same medieval records — it is interpretive. Resolution comes from: (1) which reading best explains structural drift in extraction over centuries, (2) which reading best explains why ecclesiastical criticism focuses on excess (implying a norm to exceed), and (3) which reading survives hypothetical renegotiation (if vassals could renegotiate from equal standing, would they accept obligations as written, or would they narrow them).',
    'If coordination_reading is the true structure, the constraint is Tangled Rope (genuine coordination + extractive excess) and can be reformed via charter enforcement. If lord_extraction_reading is true, the constraint is Snare and can only be escaped via collective rebellion or external rule change (crown or peasant revolt). Classification divergence is fundamental — measurement cannot resolve it alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest__coordination_vs_extraction, conceptual, 'Whether the oath is fundamentally reciprocal coordination or fundamentally extraction cover.').

omega_variable(
    identity_lock_mechanism,
    'Is vassal identity-lock (exit_options: identity_locked) structural — constituted by oath-taking itself, birth into vassal status, and hereditary tie to the office — or internalized through education and cultural narrative?',
    'Vassal behavior in crisis (peasant revolts, jurisdictional collapses, dynastic succession disputes): do vassals exit when external barriers fall (structural lock), or do they remain bound even when the institutional framework dissolves (internalized identity)? Post-feudalism, do descendants of vassals actively resist the identity or actively reconstruct it?',
    'If structural, the constraint''s suppression is institutional (external barriers enforce it); removal of barriers (crown authority collapse) permits exit. If internalized, suppression persists after barriers fall — the target carries the constraint with them. Internalized identity-lock increases effective suppression beyond the authored 0.79 figure, because the target remains constrained even if the external mechanism fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether vassal identity-lock is structural or internalized.').

omega_variable(
    sibling_reading_framing_choice,
    'Why does this reading (lord_extraction_reading) center the lord''s authority to demand maximal extraction, while the vassal_coordination_reading centers the vassal''s reciprocal right to fixed obligation, and the ecclesiastical_mediation_reading centers the oath''s moral boundaries? Are these three readings incommensurable, or do they identify different layers of the same medieval system?',
    'Historical source analysis: if the three readings emerge from different textual sources (lord''s manuals, vassal charters, ecclesiastical homilies), they represent different authored framings of a contested kernel, incommensurable only because the parties wrote from different seats. If all three readings can be drawn from the same source (e.g., a single oath formulary), the contest is interpretive framing of an ambiguous text, not different foundational claims.',
    'If incommensurable (different sources), each reading is a valid instantiation of distinct constraints and the sibling relationships are coexists_with (different parties hold different readings). If drawn from the same source, the relationships might include forecloses (one reading''s logic rules out another''s) or influences (one reading''s adoption changes the authority structure available to the others).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_framing_choice, conceptual, 'Whether sibling readings are different sources or different framings of the same source.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(feud_tr_t0, projected).
narrative_ontology:measurement(feud_tr_t50, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(feud_tr_t50, observed).
narrative_ontology:measurement(feud_tr_t150, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 150, 0.35).
narrative_ontology:measurement_basis(feud_tr_t150, observed).
narrative_ontology:measurement(feud_tr_t250, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 250, 0.4).
narrative_ontology:measurement_basis(feud_tr_t250, observed).
narrative_ontology:measurement(feud_tr_t400, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 400, 0.42).
narrative_ontology:measurement_basis(feud_tr_t400, observed).
narrative_ontology:measurement(feud_tr_t500, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 500, 0.42).
narrative_ontology:measurement_basis(feud_tr_t500, observed).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(feud_be_t0, projected).
narrative_ontology:measurement(feud_be_t50, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(feud_be_t50, observed).
narrative_ontology:measurement(feud_be_t150, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 150, 0.71).
narrative_ontology:measurement_basis(feud_be_t150, observed).
narrative_ontology:measurement(feud_be_t250, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 250, 0.78).
narrative_ontology:measurement_basis(feud_be_t250, observed).
narrative_ontology:measurement(feud_be_t400, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 400, 0.81).
narrative_ontology:measurement_basis(feud_be_t400, observed).
narrative_ontology:measurement(feud_be_t500, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 500, 0.81).
narrative_ontology:measurement_basis(feud_be_t500, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(feud_su_t0, projected).
narrative_ontology:measurement(feud_su_t50, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement_basis(feud_su_t50, observed).
narrative_ontology:measurement(feud_su_t150, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 150, 0.72).
narrative_ontology:measurement_basis(feud_su_t150, observed).
narrative_ontology:measurement(feud_su_t250, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 250, 0.76).
narrative_ontology:measurement_basis(feud_su_t250, observed).
narrative_ontology:measurement(feud_su_t400, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 400, 0.79).
narrative_ontology:measurement_basis(feud_su_t400, observed).
narrative_ontology:measurement(feud_su_t500, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 500, 0.79).
narrative_ontology:measurement_basis(feud_su_t500, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__lord_extraction_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% The feudal_oath_reciprocity kernel decomposes into three structurally distinct constraints instantiating three readings of the oath's meaning and binding force. This file models the lord's extraction reading (high ε snare). The vassal_coordination_reading models reciprocal obligation as constraining extraction (lower ε, tangled rope or rope). The ecclesiastical_mediation_reading models sacramental and moral restraint (lower ε, tangled rope). All three share the same kernel (the feudal oath text and practice) but differ fundamentally in who is authorized to set obligations and what bounds apply to extraction. The three constraints are linked bidirectionally in network.affects_constraints to show their sibling relationship — adoption of one reading affects the legitimacy conditions of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
