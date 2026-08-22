% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__stateless_coordination_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligation as Stateless Coordination
 *   domain: legal_anthropology/medieval_history
 *
 * SUMMARY:
 *   This constraint story instantiates the STATELESS COORDINATION READING of
 *   the blood-feud obligation kernel. It frames feuding as a self-enforcing,
 *   norm-governed system that solves genuine coordination problems (justice,
 *   deterrence, boundary maintenance) in the absence of centralized
 *   enforcement capacity. The beneficiary set includes feud participants (who
 *   receive justice and deterrence services) and kinship networks (who
 *   maintain territorial stability through the obligation's deterrent
 *   function). The claimed type is ROPE — pure coordination with minimal
 *   asymmetric extraction. The metrics are authored independently:
 *   extractiveness is moderate (0.38 at interval end) because the obligation
 *   does extract a cost from defectors and maintains honor inequality between
 *   aggressor and retaliator lineages, but this is framed as the price of
 *   deterrence, not as parasitic rent. Suppression is low (0.22) because
 *   alternative mechanisms (wergild settlement, private negotiation) coexist
 *   and are not actively suppressed by the feud obligation itself. Theater is
 *   very low (0.18) because the functional claim (deterrence through credible
 *   threat) matches the operational reality closely — most retaliation is
 *   genuine response to injury rather than performative display. The
 *   claim/metric gap is deliberate and intentional: this reading claims ROPE
 *   while acknowledging measurable extractive and suppressive features,
 *   because those features are, in this framework, the necessary cost of
 *   coordination in a stateless setting, not evidence of hidden extraction.
 *
 * KEY AGENTS:
 *   - Feud participants: members of lineages seeking justice; benefit from the obligation as a mechanism to restore honor and deter future injury; locked into the identity by kinship and honor norms.
 *   - Kinship networks: the organizational seats that enforce the feud obligation internally; set the bounds of legitimate retaliation and police defectors; maintain territorial control through the obligation's deterrent effect.
 *   - Territorial lineages: the same networks functioning at the political level; their reputation for retaliation is their security guarantee.
 *   - Wergild practitioners: mediators and brokers who offer compensation-based settlement; indicate the feud obligation does not monopolize justice claims.
 *   - Feud defectors: bear the cost of defection (honor loss, ostracism); their cost is the glue holding the coordination together.
 *   - External observers: anthropologists and historians assessing whether the reading fits the data.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.38).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.22).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligation as Stateless Coordination").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/medieval_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, '3afd6cf9-6ff4-4d3a-88cb-6b186bc96c8f').
narrative_ontology:cs_kernel_codification('3afd6cf9-6ff4-4d3a-88cb-6b186bc96c8f', distributed).
narrative_ontology:cs_authority_grounding('3afd6cf9-6ff4-4d3a-88cb-6b186bc96c8f', practice).
narrative_ontology:cs_interpretation_layer_present('3afd6cf9-6ff4-4d3a-88cb-6b186bc96c8f').
narrative_ontology:cs_reading_relation('3afd6cf9-6ff4-4d3a-88cb-6b186bc96c8f', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('3afd6cf9-6ff4-4d3a-88cb-6b186bc96c8f', feud_obligation_kernel__christianized_pacification_reading, influences).
narrative_ontology:cs_axiom('3afd6cf9-6ff4-4d3a-88cb-6b186bc96c8f', foundational, retaliatory_justice_legitimate_without_state).
narrative_ontology:cs_axiom_status(retaliatory_justice_legitimate_without_state, holdable).
narrative_ontology:cs_axiom_grounding('3afd6cf9-6ff4-4d3a-88cb-6b186bc96c8f', retaliatory_justice_legitimate_without_state, instrumental).
narrative_ontology:cs_axiom('3afd6cf9-6ff4-4d3a-88cb-6b186bc96c8f', foundational, kinship_honor_sustains_deterrence).
narrative_ontology:cs_axiom_status(kinship_honor_sustains_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('3afd6cf9-6ff4-4d3a-88cb-6b186bc96c8f', kinship_honor_sustains_deterrence, empirically_contingent).
narrative_ontology:cs_reference_frame('3afd6cf9-6ff4-4d3a-88cb-6b186bc96c8f', kinship_retaliatory_justice_legitimate).
narrative_ontology:cs_drift_state('3afd6cf9-6ff4-4d3a-88cb-6b186bc96c8f', christian_royal_authority_emergence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3afd6cf9-6ff4-4d3a-88cb-6b186bc96c8f', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, feud_participants_seeking_justice).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, kinship_networks).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, territorial_lineages).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, wergild_practitioners).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, feud_defectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of a kinship network that has suffered injury (death, maiming, property loss, honor violation) from another lineage. The feud obligation provides them a legitimate mechanism to pursue retaliation without requiring appeal to a centralized authority that does not exist. They restore honor and deter future injuries by making the cost of transgression known to be retaliation. Their stake in the obligation is their social position within the lineage: withdrawal from the feud obligation brings honor loss and potential expulsion from the kinship network.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_participants_seeking_justice, beneficiary,
    moderate, biographical, identity_locked, regional).

% Enforce the feud obligation through internal norms and sanctions: they decide when retaliation is justified, who carries it out, when reciprocation is sufficient, and what happens to members who refuse the obligation or who escalate without collective approval. The obligation stabilizes the network's territorial position and reputation by making both internal loyalty and external deterrence visible and costly to violate.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kinship_networks, agenda_setter,
    organized, generational, constrained, regional).

% Priests, merchants, or experienced mediators who broker wergild (compensation) settlements as an alternative to feud escalation. Under the stateless coordination reading, they are not suppressed by the feud obligation — wergild coexists as a settlement mechanism for parties who prefer compensation to retaliation. They benefit from access to dispute resolution fees and reputation for prudence. Their existence and regular use indicate the feud obligation does not monopolize justice claims.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, wergild_practitioners, beneficiary,
    powerful, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__stateless_coordination_reading, wergild_practitioners, observer).

% Maintain territorial control and security through the feud obligation's deterrent function. Each lineage's reputation for retaliation — the credible threat that injury will be answered — is what prevents raids and encroachment. They set the obligation's limits through customary law (how many retaliations are sufficient, when escalation is forbidden) and police their own members' compliance.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, territorial_lineages, agenda_setter,
    organized, generational, trapped, regional).

% Members of a kinship network who refuse the feud obligation when called upon to retaliate or who privately settle with the opposing lineage without collective approval. They face honor loss, social ostracism, and potential expulsion from the network. In the stateless coordination reading, their cost is the price of the coordination function itself — defection undermines the deterrent if it appears costless.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_defectors, payer,
    moderate, biographical, identity_locked, regional).

% Anthropologists, historians, and legal scholars analyzing the feud obligation from outside the system. They assess whether the reading is accurate to the data and whether the coordination claim holds up under comparative and historical scrutiny.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, external_observers, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__stateless_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__stateless_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In the absence of centralized courts or enforcement capacity, the feud obligation solves the problem of credible retaliation and deterrence: it allows a kinship network to respond to injury in a way that is (1) proportionate within customary limits, (2) collectively authorized so that one hot head cannot drag the whole lineage into escalation, (3) visible to potential aggressors so the deterrent works, and (4) self-enforcing because honor and lineage membership ride on participation. The mechanism thus stabilizes territorial boundaries and property rights without requiring a state.
% TRANSFER_FUNCTION: Moves honor (social status within the lineage and in the broader regional network) from the aggressor lineage to the retaliating lineage; moves risk of death or injury to those who commit or execute the retaliation; distributes the enforcement burden across the kinship network rather than concentrating it in a state apparatus.
% ABSENT_VOICES: Persons from the opposing lineage who might wish to negotiate before retaliation occurs (they are sometimes present through wergild mediators, but not always); victims of mistaken identity or collateral damage (the feud obligation does not include a mechanism for exonerating the wrong target, though wergild mediators sometimes intervene); individuals who would benefit from a centralized court but cannot establish one themselves (merchants seeking uniform commercial law, travelers seeking safe passage across lineage boundaries, women and children caught in escalation); Christian clergy and royal administrators who argue that feuding violates divine law and human dignity.
% DISAPPEARANCE_RATIONALE: If the feud obligation disappeared, deterrence would collapse: aggressors would have no reason to fear retaliation, territorial boundaries would become unstable, and kinship networks would lose the coordination mechanism that keeps internal loyalty credible. Without the feud obligation AND without a replacement (a state court system), the region would reorganize around different mechanisms — predatory raid-and-consolidate dynamics, tribute systems, or new forms of alliance. The obligation is what prevents that reorganization in the first place.
% FOUNDING_PROBLEM: In a stateless political system, how does a kinship network pursue justice and deter injury without a court to adjudicate disputes or a police force to execute sentences? How does a territory maintain stable boundaries and property rights when no central authority exists to enforce them?
% FOUNDING_PROBLEM_CORROBORATION: Anthropological research on blood-feud systems in contemporary stateless societies (Somali pastoral lineages, Afghan tribal systems, Papua New Guinea highland societies documented by researchers like Mahmood Mamdani and Mark Mosko) confirms that where centralized enforcement capacity is absent or weak, retaliatory justice systems with customary limits and lineage sanctions DO solve deterrence and boundary-maintenance problems in practice. The founding problem persists wherever centralized enforcement capacity remains absent. Medieval historians including Charles-Edmond Dufourc and René Girard note that feuding systems maintained relatively stable territorial and commercial networks for centuries before centralized states emerged — suggesting functional success over that time horizon.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__stateless_coordination_reading_tests).
:- end_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS is authored at 0.38 rather than near-zero for a genuine rope because: (1) defectors face honor loss and expulsion, which is a real cost extracted by the obligation; (2) the obligation maintains a status hierarchy between retaliator (elevated) and aggressor (shamed), which asymmetrically benefits those aligned with the custom and harms those who deviate or are on the losing side of a feud; (3) the obligation's benefits are concentrated in participants while its risks are borne by those who execute retaliation. However, these extractions are framed as COORDINATION COSTS, not parasitic rent, because: they reinforce the very mechanism that solves the deterrence problem; they do not concentrate gains in a third party; and they are proportionate to the service provided (deterrence from a credible retaliation threat). SUPPRESSION is low (0.22) because wergild coexists as a live settlement alternative; kinship networks do not actively suppress it; participants retain choice to pursue compensation rather than retaliation. THEATER is low (0.18) because the obligation's deterrent function depends on credibility — if retaliation is theatrical (performed without real commitment), the deterrent fails and the obligation collapses. The measurement series traces a shallow rise over 500 time-units (spanning, roughly, the period from early medieval Germanic societies to the 12th century when centralized states began to monopolize justice in Europe), indicating slight accumulation of extractiveness and suppression as Christian doctrine and royal authority begin to delegitimize feud and as wergild settlements increasingly displace pure retaliation.
 *
 * PERSPECTIVAL GAP:
 *   The kinship-network seats and the external-observer seats compute different types from the same structural data. From the kinship perspective, the obligation is essential coordination that could not be replaced without either accepting predatory vulnerability or building a state apparatus (unavailable). From an external analytical perspective, the obligation includes asymmetric extraction (honor gains for retaliators, honor loss for defectors) and suppression of alternatives (wergild is permitted to coexist but is never the default). The engine should compute this divergence: the organized-power agenda-setter seat might classify as ROPE while the analytical observer seat classifies as TANGLED ROPE. The authored claim and metrics should remain unchanged; the per-seat divergence is the point.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seats (feud participants, kinship networks, territorial lineages) sit near the beneficiary end of directionality (d ≈ 0.1–0.3): they receive justice, deterrence, and territorial stability from the obligation. Defectors sit near the target end (d ≈ 0.7–0.8): they bear honor loss and expulsion if they refuse participation. Wergild practitioners sit near symmetric (d ≈ 0.45–0.55): they benefit from fees and reputation but also bear the cost of mediating dangerous disputes. The analytical observer is exempt from directionality (d = undefined, analytical power atom). No overrides are necessary: the beneficiary/victim declarations and exit options produce the correct d through derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The stateless-coordination reading argues that the feud obligation's founding problem (how to achieve justice and deterrence without state courts) is LIVE — wherever state institutions are absent or weak, the founding problem persists and the obligation solves it. This classification resists mandatrophy: the reading defends the obligation as an ongoing solution, not as an atrophied remnant. However, the measurement series shows slight rise in extractiveness and suppression over the 500-year interval, driven by the emergence of Christian doctrine and royal authority that delegitimize feuding and gradually establish wergild + state courts as preferred alternatives. The reading DOES NOT claim the obligation is eternal; rather, it claims the obligation is rational and functional WHERE AND WHEN state enforcement capacity is absent. Mandatrophy would apply if the obligation persisted in a fully centralized state — a dead founding problem — but the reading's internal logic is that the founding problem dies precisely when centralization arrives, and so does the obligation's primary justification. The slight rise in metrics over time suggests pressure from this emergent delegitimation, not mandate atrophy; the reading remains vivid and defended by genealogy practitioners and some customary communities well into the early modern period.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_genuine_or_rhetorical,
    'Is the founding problem (how to achieve justice without centralized courts) genuinely solved by the feud obligation, or is the obligation a mechanism for warrior elites to capture power and resources, with the ''justice'' framing being rhetorical cover?',
    'Comparative institutional analysis: examine settlement patterns in jurisdictions where feuding is tolerated vs. suppressed; measure dispute resolution outcomes (frequency of escalation, proportion of settlements, time to closure) under feud-based vs. wergild-based vs. state-court systems; assess whether feud norms are egalitarian or systematically favor elites.',
    'If the founding problem is solved, the obligation remains ROPE and the stateless-coordination reading stands. If elite capture is found, the obligation is TANGLED ROPE at minimum (coordination function + asymmetric extraction) or SNARE (pure extraction under a coordination cover). The rise in extractiveness over time (measured at t=500) could be evidence that elite capture increased or that state pressure forced the obligation to become more extractive to survive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_genuine_or_rhetorical, empirical, 'Whether feud obligation solves the founding problem or is elite rent-capture.').

omega_variable(
    internalized_vs_coercive_suppression,
    'Is the low measured suppression of alternatives (wergild) structural (legal barriers do not exist) or internalized (norm pressure makes deviation costly even when alternatives are formally available)?',
    'Post-suppression trajectory: if conversion to Christianity or adoption of state courts reduces honor sanctions around feuding but does not change formal legal barriers, and if wergild adoption then accelerates, suppression was primarily internalized. If formal legal barriers to wergild existed and their removal enables wergild adoption even without norm change, suppression was primarily structural.',
    'If internalized, the obligation''s persistence relies on norm transmission; disruption of honor norms (through religious conversion, urbanization, etc.) would weaken the obligation even if formal alternatives remain suppressed. If structural, the obligation persists regardless of norm changes until legal alternatives are formally opened. The distinction informs whether the obligation''s collapse is norm-driven (education and cultural change) or state-driven (legal monopolization).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internalized_vs_coercive_suppression, empirical, 'Whether suppression is internalized norm pressure or structural legal barriers.').

omega_variable(
    alternative_reading_feasibility,
    'Is the extraction-cycle reading (feuding depletes productive capacity and prevents state-building) an alternative description of the same facts or a mutually incompatible structural claim?',
    'Historical data on productive capacity (agricultural output, population, capital accumulation, military capability) in regions with entrenched blood-feud systems vs. regions where feuding was successfully suppressed (e.g., Islamic legal systems, early state monopolies). If productive capacity consistently rises after feuding suppression and falls during its prevalence, the extraction-cycle reading describes the same system more accurately. If capacity is stable or rises under stable feuding, the coordination reading prevails.',
    'If the extraction-cycle reading is empirically more accurate, the kernel contest resolves in its favor and the stateless-coordination reading becomes an inaccurate description. If both readings describe real but temporally distinct phases (feuding sustains deterrence initially but becomes extractive/stagnating as lineages consolidate), both readings can coexist as describing different intervals — the kernel would split into temporal substories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_feasibility, empirical, 'Whether extraction-cycle reading is alternative or incompatible description.').

omega_variable(
    honor_as_transferable_value,
    'Is honor a genuine transferable good that flows from aggressor to retaliator, or is it a status label applied post-hoc to justify retaliation that is motivated by material interests (land, livestock, slaves)?',
    'Ethnographic data on actual honor flows and their material consequences: does a lineage that kills an aggressor in feud improve its marriage prospects, trading partnerships, alliance formation, or military recruitment? Or do honor claims persist while material outcomes are driven by force and land control? If honor transfers material access, it is a real transferable good. If honor is symbolic only and material outcomes flow from military power, honor is the framing, not the function.',
    'If honor is a real transferable good solving the settlement problem (prestige replaces land as the object of competition), the coordination reading is strengthened. If honor is symbolic and material outcomes are decoupled from honor transfers, the obligation may be more extractive (extracting compliance through honor symbolism while material benefits accrue separately) or less functional (honor doesn''t actually solve the deterrence problem, deterrence comes from military readiness regardless of honor claims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_as_transferable_value, empirical, 'Whether honor is a real transferable settlement value or post-hoc justification.').

omega_variable(
    reading_foreclosure_under_state_authority,
    'Does the Christianized Pacification reading (feuding violates divine law; legitimate authority resides in God and delegated institutions) logically foreclose the Stateless Coordination reading, or do they coexist as competing values held by different factions?',
    'Structural-logical analysis: does the pacification reading''s claim (that feuding is categorically impermissible by divine law) prevent a single party from holding BOTH positions? If a party could theoretically accept the pacification reading AND defend feuding as necessary where state capacity fails, the readings coexist; if not, one forecloses the other. Examine actual historical positions: did feuding defenders ever concede that feuding violates divine law but argue for it anyway as a lesser evil?',
    'If foreclosure exists, the kernel contest is zero-sum: one reading prevails and the others become untenable within any framework. If coexistence is possible, the readings reflect a genuine value conflict (justice via retaliation vs. justice via authority) that plays out across different parties and historical periods. The measurement trajectory (slight rise in extractiveness and suppression over time) may reflect the emergence of the pacification reading''s authority-grounding pressure, which would correspond to influence rather than foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_under_state_authority, conceptual, 'Whether pacification reading forecloses or coexists with coordination reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(feud_tr_t50, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 50, 0.13).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 100, 0.14).
narrative_ontology:measurement(feud_tr_t200, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 200, 0.16).
narrative_ontology:measurement(feud_tr_t350, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 350, 0.17).
narrative_ontology:measurement(feud_tr_t500, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 500, 0.18).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(feud_be_t50, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 100, 0.31).
narrative_ontology:measurement(feud_be_t200, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 200, 0.35).
narrative_ontology:measurement(feud_be_t350, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 350, 0.37).
narrative_ontology:measurement(feud_be_t500, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 500, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(feud_su_t50, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 50, 0.19).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 100, 0.2).
narrative_ontology:measurement(feud_su_t200, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 200, 0.21).
narrative_ontology:measurement(feud_su_t350, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 350, 0.215).
narrative_ontology:measurement(feud_su_t500, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 500, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__stateless_coordination_reading, 0.12).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% The blood-feud obligation is a contested kernel with three structurally distinct readings: stateless_coordination_reading (this file) frames feuding as genuine coordination solving the justice problem in stateless settings; extraction_cycle_reading frames feuding as destructive rent-capture that prevents state-building; christianized_pacification_reading frames feuding as illegitimate violence violating divine authority and mandating institutional pacification. Each reading produces a different constraint type: rope vs. tangled_rope vs. mountain (prohibition). The readings are linked via network.affects_constraints and their ε-invariance is preserved by deriving each reading's ε from its own framing, not by averaging across readings or selecting a measurement basis that produces convergence. The stateless_coordination_reading's ε=0.38 describes the obligation's extractiveness assessed by the coordination frame's own lights; the extraction_cycle_reading's ε would be substantially higher (0.65+) because the cycle frame counts productive losses and stagnation effects; the pacification reading's ε would be near-maximum (~0.9) because the frame sees feuding as pure violation of divine law with no coordination function to offset. The readings are not different measurements of the same ε; they are different constraints with different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
