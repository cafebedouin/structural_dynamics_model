% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Article 51 Self-Defense: Expansive Preventive Reading
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   Article 51 of the UN Charter permits self-defense if an armed attack
 *   occurs. This constraint instantiates the EXPANSIVE PREVENTIVE READING:
 *   one party to the contested kernel argues that Article 51 extends to
 *   preemptive and preventive force against non-state actors and emerging
 *   threats when the acting state judges necessity internally. This reading
 *   benefits militarily capable states (who can conduct preventive operations
 *   with low institutional constraint) and defense sectors (sustained demand
 *   for weapons and intelligence); it extracts from target populations (who
 *   bear military force without consent), from multilateral institutions
 *   (whose veto authority is sidelined by unilateral interpretation), and
 *   from constrained states (who face preventive force asymmetrically). The
 *   reading is instantiated through doctrine, military planning, and
 *   occasional operational justification; its persistence depends on
 *   major-power acceptance and the absence of coordinated multilateral
 *   challenge. The three readings of the Article 51 kernel occupy different
 *   institutional positions and produce substantially different constraint
 *   types: the narrow reading makes Article 51 nearly a rope (genuine
 *   coordination problem: states need a rule permitting response to actual
 *   attacks); the expansive preventive reading makes it a tangled rope or
 *   snare (coordination framing masks unilateral authority); the
 *   unable/unwilling doctrine reading sits in between (hybrid constraint
 *   responding to real sovereignty gaps).
 *
 * KEY AGENTS:
 *   - militarily_capable_states: institutional power, set the doctrine and determine necessity unilaterally, benefit from legal cover for preventive force
 *   - target_region_populations: powerless, trapped, bear direct costs of preventive operations
 *   - defense_industrial_sector: organized power, sustain demand for weapons and security systems through sustained threat perception
 *   - multilateral_veto_authority (UNSC, ICJ): institutional power, sidelined by unilateral interpretation, constrained from acting without veto-holder agreement
 *   - constrained_states: moderate power, vulnerable to preventive force, cannot claim same interpretive latitude
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.82).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.71).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Article 51 Self-Defense: Expansive Preventive Reading").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, '23f95117-c4cd-4d5f-bc70-e4dd10d8270c').
narrative_ontology:cs_kernel_codification('23f95117-c4cd-4d5f-bc70-e4dd10d8270c', fixed_text).
narrative_ontology:cs_authority_grounding('23f95117-c4cd-4d5f-bc70-e4dd10d8270c', extraction).
narrative_ontology:cs_interpretation_layer_present('23f95117-c4cd-4d5f-bc70-e4dd10d8270c').
narrative_ontology:cs_reading_relation('23f95117-c4cd-4d5f-bc70-e4dd10d8270c', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('23f95117-c4cd-4d5f-bc70-e4dd10d8270c', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('23f95117-c4cd-4d5f-bc70-e4dd10d8270c', foundational, unilateral_necessity_determination).
narrative_ontology:cs_axiom_status(unilateral_necessity_determination, holdable).
narrative_ontology:cs_axiom_grounding('23f95117-c4cd-4d5f-bc70-e4dd10d8270c', unilateral_necessity_determination, conventional).
narrative_ontology:cs_axiom('23f95117-c4cd-4d5f-bc70-e4dd10d8270c', foundational, preventive_force_legitimacy).
narrative_ontology:cs_axiom_status(preventive_force_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('23f95117-c4cd-4d5f-bc70-e4dd10d8270c', preventive_force_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('23f95117-c4cd-4d5f-bc70-e4dd10d8270c', state_security_autonomy_post_2001).
narrative_ontology:cs_drift_state('23f95117-c4cd-4d5f-bc70-e4dd10d8270c', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23f95117-c4cd-4d5f-bc70-e4dd10d8270c', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_industrial_sector).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, constrained_states).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, unilateral_security_determination).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, state_necessity_doctrine).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, preemptive_force_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply Article 51 unilaterally to authorize force against perceived emerging threats, non-state actors, and preventive targets. Determine 'necessity' through domestic security assessment without external verification. Benefit from a legal framework that permits autonomous security decision-making and constrains peer challenge. Set strategic doctrine and deploy military force based on this reading.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the direct costs of military operations justified under this reading: civilian casualties, infrastructure destruction, displacement, and destabilization. Have no seat in the necessity determination process. Cannot exit the target region to escape the constraint's application. Subject to force based on another state's unilateral threat assessment.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_populations, payer,
    powerless, immediate, trapped, local).

% Sustains demand for military systems, intelligence platforms, and security technology through sustained threat perception and military operations justified under this reading. Expands market access into new regions and conflict zones. Benefits from contractual relationships with states invoking Article 51 preventively.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, defense_industrial_sector, beneficiary,
    organized, biographical, mobile, global).

% Is structurally sidelined in necessity determinations by unilateral interpretation of Article 51. Cannot enforce review or restraint on actions claiming Article 51 justification without amending the Charter itself. The Security Council veto holder can override but only through affirmative action, making inaction = tacit approval. Constrained by the same reading that subordinates multilateral authority.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority, excluded).

% Cannot claim Article 51 self-defense with the same interpretive latitude as militarily capable states, due to power asymmetry and institutional capacity constraints. May be TARGET of Article 51 preventive action by more capable states. Constrained by the reading in two directions: their own preventive actions are contested; they are vulnerable to others' preventive actions.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, constrained_states, payer,
    moderate, generational, constrained, global).

% Adjudicate disputes over Article 51 interpretation after the fact, with limited enforcement power over acting states. Cannot prevent preemptive action. May issue advisory opinions or judgments that are not binding on Security Council permanent members. Analyze whether necessity was demonstrated, but do so retrospectively and without direct authority over the constraint's application.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_courts, observer,
    institutional, generational, analytical, global).

% Argue for narrower readings of Article 51 that would constrain preventive force and require higher burden of proof for necessity. Are not parties to the security determinations that invoke Article 51. Can generate counterarguments, NGO pressure, and legal alternative framings, but cannot veto state action.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, humanitarian_advocates, excluded,
    organized, biographical, mobile, global).

% Are often the nominal trigger or pretext for Article 51 preventive action. Have no recognized standing to defend themselves in the necessity determination. Subject to force justified as preemptive self-defense under this reading, even if they have not mounted an actual attack.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, non_state_armed_actors, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:fixing_cost_class(article_51_self_defense__expansive_preventive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a multilateral legal framework that permits unilateral state security action against emerging threats and non-state actors, coordinating the international system's response to transnational security challenges through autonomous rather than collective decision-making. Reduces coordination friction by permitting self-help.
% TRANSFER_FUNCTION: Transfers the authority to determine necessity from the multilateral system (Security Council veto) to individual capable states; transfers the costs of military operations (casualties, infrastructure loss, destabilization) from decision-maker to target populations; transfers security contracts and sustained demand from peace-dependent markets to conflict-dependent ones.
% ABSENT_VOICES: Target populations cannot appear at the determination table; non-state actors being targeted have no international legal standing; smaller states are excluded from symmetric interpretation of the same Article; humanitarian advocates and development agencies are structural outsiders to security determinations.
% DISAPPEARANCE_RATIONALE: If this reading of Article 51 vanished and reverted to a narrow 'actual armed attack' constraint, military doctrine would shift dramatically: preventive operations would lose legal cover, defense spending would face fiercer domestic challenge, and regional conflicts currently justified under Article 51 preventive authority would require either narrower justification or multilateral approval. Geopolitical alignments would reorganize around constraint on unilateral force.
% FOUNDING_PROBLEM: Post-2001 security environment presented asymmetric threats from non-state actors and failed states hosting them; 9/11 and subsequent terrorism created demand for a legal doctrine permitting anticipatory action against emerging threats before they materialized into conventional armed attacks.
% FOUNDING_PROBLEM_CORROBORATION: Security establishments and defense strategists attest the founding problem remains live and justify Article 51 preventive reading as necessary response. International courts (ICJ Nicaragua, ICJ Congo v. Uganda opinions) argue the founding problem is overstated and the threat has been addressed by alternative legal doctrines; humanitarian organizations and non-aligned states attest that the founding problem has been displaced by post-2008 geopolitical shifts and the reading now persists as institutional convenience rather than necessity.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the reading permits force based on self-judged necessity, decoupled from any external constraint or burden of proof, and permits operations against targets with no demonstrated imminent threat. Suppression is substantial (0.71) because the constraint's operation depends on maintaining states' monopoly on necessity determination and excluding multilateral review mechanisms from real-time constraint. Theater is moderate (0.48): the reading is operationalized through military doctrine and intelligence assessment (not pure performance), but a significant share of the Article 51 preventive claim-making is justified retroactively for operations that had other strategic motives. The measurement series tracks increasing extractiveness over the 25-year interval as the reading became institutionalized in doctrine post-2001 and faced declining multilateral challenge, and increasing theater as the foundational security problem (asymmetric threats from non-state actors) partially addressed itself through technology and counter-terrorism, while the preventive doctrine persisted.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of militarily capable states, this reading solves a genuine security coordination problem and is justified by the inability of multilateral mechanisms to respond quickly to emerging threats. From the seat of target populations and constrained states, the same reading is an institutional mechanism for unilateral force without constraint. The engine computes these divergent per-seat classifications from the structural data: militarily capable states see coordination benefit (low d) and may compute the constraint as rope or tangled rope with substantial coordination function; target populations see pure extraction (high d, no coordination benefit) and compute it as snare. The perspectival gap is structural, not observational.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states are beneficiaries: they unilaterally determine necessity, face no external veto, and can project force globally under Article 51 cover (d near 0.0 — full beneficiary). Defense sectors benefit indirectly through sustained demand (d near 0.1). Target populations are targets: they cannot exit, have no voice in determination, bear all costs (d near 1.0 — full target). Multilateral veto authority is trapped: it is nominally in charge of peace and security but is sidelined by unilateral interpretation; it cannot veto unless the acting state permits (institutional lock-in, constrained exit, d near 0.75 — high target). Constrained states are in a hybrid position: they face vulnerability to preventive force by more capable states but retain nominal Article 51 rights they cannot meaningfully exercise due to power asymmetry (d near 0.65 — weighted toward target).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading faces a classic mandatrophy signal: the founding problem (asymmetric post-2001 terrorism threats) has substantially degraded (counter-terrorism effectiveness improved, threats evolved, no major attack on scale of 9/11 for two decades), but the reading persists because it serves interests independent of the founding problem — military doctrine, defense contracting, institutional power. The theater_ratio rising from 0.38 to 0.48 reflects this: an increasing proportion of Article 51 preventive justifications are offered for operations where the stated necessity (emerging threat from non-state actor) is secondary to strategic objectives (regional power projection, resource competition). The reading is not abandoned because abandonment would require peer-state coordination to change doctrine simultaneously — a collective-action problem that favors the status quo.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_determination_location,
    'Where is the authority to determine ''necessity'' located: in the acting state''s unilateral judgment, in the ICJ, in the Security Council, or in some hybrid?',
    'Formal amendment to the Charter (rare) or consensus among major powers to adopt a narrower reading and enforce it through veto coordination (more realistic, but unlikely absent a major triggering incident).',
    'If necessity determination moves to external bodies (ICJ, UNSC), extractiveness drops sharply and suppression requirement increases as external veto is re-established. If it remains unilateral, the constraint persists as highly extractive with moderate suppression (states tolerate it through informal power balance, not through coordinated enforcement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_determination_location, conceptual, 'Locus of authority over necessity determination is the central structural ambiguity; different institutional locations produce fundamentally different constraint types.').

omega_variable(
    threat_versus_strategic_operation,
    'In cases where Article 51 preventive force is invoked, what proportion of the operation''s real motivation is response to genuine emerging threat versus pursuit of strategic objectives (regional power, resource access, institutional positioning)?',
    'Declassified post-operation reviews, leaked strategic documents, or ex-post-facto academic analysis cross-referencing operational outcomes with stated threat assessment.',
    'If threat-response motivations dominate, the theater_ratio should be lower (~0.25-0.35) and the constraint reads more as tangled rope with real coordination component. If strategic motivations dominate, theater_ratio should be higher (~0.55+) and the constraint reads as snare with coordination cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_versus_strategic_operation, empirical, 'Genuineness of threat assessment versus rationalization of strategic operations.').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem (asymmetric transnational terrorism) substantially decreased in severity, frequency, or global impact such that the reading''s persistence can no longer be justified by its original rationale?',
    'Longitudinal analysis of terrorism fatality rates, attack frequency, and threat reports; comparison of 2001-2010 period (peak asymmetric threat) to 2015-2025 (baseline comparison); expert consensus among security analysts outside the benefiting institutions.',
    'If founding problem is substantially solved, the reading becomes a clear mandatrophy case (theater ratio rises, extractiveness remains high, founding_problem_status shifts from ''live'' to ''dead''), and alternative readings or charter amendments become more compelling. If the problem remains live (ongoing transnational terrorism remains credible threat), the reading retains mandatrophy resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Temporal decay of the founding justification for the expansive preventive reading.').

omega_variable(
    peer_state_veto_equilibrium,
    'Is the expansive preventive reading sustained primarily by actual major-power agreement on its desirability, or by a veto equilibrium where no peer state challenges it because all benefit from symmetrical right to invoke it?',
    'Pattern analysis of Article 51 preventive invocations by power level: if only peer states invoke it and never challenge each other''s invocations, veto equilibrium is operative; if constrained states invoke it against peers and are challenged, or if peer states invoke it against each other, the structure is different.',
    'If veto equilibrium: the suppression requirement is sustained by power balance, not by coordination or enforcement; if shared agreement: the constraint is more robust and less dependent on power parity. The mechanism affects which institutional change could destabilize the reading (peer-power shift → veto equilibrium destabilizes; ideological shift → agreement-based reading destabilizes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_state_veto_equilibrium, empirical, 'Mechanism sustaining the reading: genuine consensus versus power-balanced veto equilibrium.').

omega_variable(
    reading_foreclosure_boundary,
    'Could a state simultaneously hold BOTH the expansive preventive reading AND the narrow armed attack reading, or are they logically incompatible in a single framework?',
    'Careful logical analysis of the axiom sets: if a state says ''we can invoke preventive force AND we will not allow others to do so against us unless there is actual armed attack,'' the two readings coexist (opportunistic coexistence, not genuine logical compatibility). If a state must choose one reading and apply it consistently to all actors, they foreclose each other.',
    'If they coexist (likely), the readings are related by ''coexists_with'' and power asymmetry, not by logical foreclosure. If they foreclose, the engine''s axiom_contradiction path activates and one reading is displaced over time. Affects prediction of which reading dominates in equilibrium.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Logical compatibility or incompatibility of the expansive and narrow readings in a single state''s strategic framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__expansive_preventive_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t5, article_51_self_defense__expansive_preventive_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement_basis(arti_tr_t5, observed).
narrative_ontology:measurement(arti_tr_t10, article_51_self_defense__expansive_preventive_reading, theater_ratio, 10, 0.43).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t15, article_51_self_defense__expansive_preventive_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement_basis(arti_tr_t15, observed).
narrative_ontology:measurement(arti_tr_t20, article_51_self_defense__expansive_preventive_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t25, article_51_self_defense__expansive_preventive_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(arti_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t5, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 5, 0.74).
narrative_ontology:measurement_basis(arti_be_t5, observed).
narrative_ontology:measurement(arti_be_t10, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 10, 0.77).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t15, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 15, 0.8).
narrative_ontology:measurement_basis(arti_be_t15, observed).
narrative_ontology:measurement(arti_be_t20, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t25, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(arti_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t5, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(arti_su_t5, observed).
narrative_ontology:measurement(arti_su_t10, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t15, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(arti_su_t15, observed).
narrative_ontology:measurement(arti_su_t20, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t25, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(arti_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__expansive_preventive_reading, 0.18).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__unable_unwilling_doctrine_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, transnational_terrorism_threat_consensus).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority_constraint).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, non_state_actor_legal_personhood).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Article 51 self-defense kernel. The narrow_armed_attack_reading interprets Article 51 to permit self-defense only in response to actual or imminent state-attributed armed attack, producing a constraint that is nearly a rope (genuine coordination problem with minimal extraction). The expansive_preventive_reading (this constraint) interprets Article 51 to permit preemptive and preventive force based on unilateral necessity determination, producing a substantially extractive tangled_rope or snare. The unable_unwilling_doctrine_reading permits force against non-state actors when the host state is unable or unwilling to suppress them, producing a hybrid constraint between the two. All three readings instantiate from the same Charter text but produce different ε values, different beneficiary structures, and different constraint types. The readings coexist as live positions held by different state coalitions; none logically forecloses the others in a single framework, though they create institutional pressure on each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__expansive_preventive_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
