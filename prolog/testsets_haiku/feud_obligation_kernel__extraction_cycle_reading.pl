% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Obligation Extraction Cycle (Destructive Resource Depletion Reading)
 *   domain: legal/anthropological/political
 *
 * SUMMARY:
 *   Blood-feud obligation in early medieval societies functioned initially as
 *   a decentralized deterrent mechanism: if harming a kinship group member
 *   triggered an obligation to pursue retaliation with social death as the
 *   cost of non-compliance, it constrained violence. This extraction-cycle
 *   reading argues that over time the mechanism degraded into a destructive
 *   cycle: the obligation became an identity-lock mechanism (exit means
 *   renouncing kin and honor), retaliation cycles depleted productive
 *   capacity faster than deterrence gains accumulated, and the system's
 *   persistence came to depend on suppression of exit alternatives rather
 *   than on functional necessity. Royal and ecclesiastical authorities
 *   benefited from the cycle's continuation because it justified their
 *   monopoly claims on legitimate violence. The constraint is claimed as a
 *   snare (pure extraction with suppression of alternatives and identified
 *   victims) rather than the rope or stateless-coordination readings would
 *   suggest. The measurement trajectory shows increasing extractiveness and
 *   theater-ratio over the interval, consistent with a constraint whose
 *   primary function has atrophied but which persists through institutional
 *   interest and ideological suppression of exit frames.
 *
 * KEY AGENTS:
 *   - kinship_groups_trapped_in_feuds: Primary victims bearing the resource-depletion and mortality costs; identity-locked exit; generational time horizon means the obligation reproduces across lifespans.
 *   - emerging_royal_authority: Agenda-setter benefiting from the cycle's persistence; monopoly claim on legitimate violence is strengthened by blood-feud obligation's destructiveness.
 *   - ecclesiastical_institutions: Agenda-setters claiming divine prohibition on private vengeance; position themselves as exclusive peace-makers and conflict mediators; extract tithes and spiritual authority.
 *   - subordinate_lineages_bearing_retaliation_cost: Powerless payers absorbing disproportionate violence; trapped exit; no voice in compensation calculations.
 *   - productive_territories_destabilized_by_cycles: Non-agent victim representing aggregate economic damage; trade disruption, fortification costs, labor scarcity.
 *   - neighboring_non_feuding_groups: Excluded from negotiation but dragged into cycles through alliances; represent the constrained alternatives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.81).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.79).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Obligation Extraction Cycle (Destructive Resource Depletion Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal/anthropological/political").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, '722eb7f1-36fa-41a5-b246-6580911673ce').
narrative_ontology:cs_kernel_codification('722eb7f1-36fa-41a5-b246-6580911673ce', distributed).
narrative_ontology:cs_authority_grounding('722eb7f1-36fa-41a5-b246-6580911673ce', extraction).
narrative_ontology:cs_interpretation_layer_present('722eb7f1-36fa-41a5-b246-6580911673ce').
narrative_ontology:cs_reading_relation('722eb7f1-36fa-41a5-b246-6580911673ce', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('722eb7f1-36fa-41a5-b246-6580911673ce', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('722eb7f1-36fa-41a5-b246-6580911673ce', foundational, feud_obligation_functionally_degraded).
narrative_ontology:cs_axiom_status(feud_obligation_functionally_degraded, holdable).
narrative_ontology:cs_axiom_grounding('722eb7f1-36fa-41a5-b246-6580911673ce', feud_obligation_functionally_degraded, empirically_contingent).
narrative_ontology:cs_axiom('722eb7f1-36fa-41a5-b246-6580911673ce', secondary, kinship_exit_identity_locked_irreversible_absent_intervention).
narrative_ontology:cs_axiom_status(kinship_exit_identity_locked_irreversible_absent_intervention, holdable).
narrative_ontology:cs_axiom_grounding('722eb7f1-36fa-41a5-b246-6580911673ce', kinship_exit_identity_locked_irreversible_absent_intervention, empirically_contingent).
narrative_ontology:cs_reference_frame('722eb7f1-36fa-41a5-b246-6580911673ce', kinship_based_deterrence_authority).
narrative_ontology:cs_drift_state('722eb7f1-36fa-41a5-b246-6580911673ce', high_medieval_law_emergence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('722eb7f1-36fa-41a5-b246-6580911673ce', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, emerging_royal_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_institutions_claiming_monopoly).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, kinship_groups_trapped_in_feuds).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, productive_territories_destabilized_by_cycles).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, subordinate_lineages_bearing_retaliation_cost).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, subordinate_lineages_bearing_retaliation_cost).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__extraction_cycle_reading, monopoly_on_legitimate_violence_doctrine).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__extraction_cycle_reading, divine_prohibition_on_private_vengeance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by kinship obligation to avenge slain members. Each retaliation triggers counter-retaliation, creating a repeating cycle of violence that depletes labor, disrupts agricultural cycles, and forces constant military readiness. Exit from the cycle means renouncing kinship identity and the group's honor code—a social death equivalent to physical death. The obligation is enforced internally through shame, exclusion, and the derision of kin as oath-breakers.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, kinship_groups_trapped_in_feuds, payer,
    moderate, generational, identity_locked, regional).

% Justifies concentration of violence authority by claiming that private blood-feud obligation is destructive and that only centralized, law-based justice can restore peace. Collects legitimacy (and tax revenue) from pacification promises. The longer feuds persist, the more urgently intervention is framed as necessary, reinforcing the claim that violence monopoly is the only solution. Beneficiaries from the constraint's persistence because it validates the extraction of tribute and allegiance in the name of peace.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, emerging_royal_authority, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, emerging_royal_authority, beneficiary).

% Lower-status lineages within feuding groups absorb a disproportionate share of retaliation: they carry out raids, take casualties, and lose reproductive members. They gain minimal share of any compensation paid by the offended kin, yet bear the full cost of escalation. Their only exit is death or incorporation into a dominant lineage—both effectively foreclose their original kinship identity.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, subordinate_lineages_bearing_retaliation_cost, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, subordinate_lineages_bearing_retaliation_cost, beneficiary).

% Claims divine authority to condemn blood-feud obligation as violation of divine law (prohibitions on private vengeance, the sanctity of peace-oaths sworn before God). Positions itself as the mediator and enforcer of peace-bonds, collecting tithes and spiritual authority from pacification efforts. Benefits structurally from the feud cycle's persistence because ongoing violence validates the church's monopoly claim on legitimate absolution and peace-making.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_institutions_claiming_monopoly, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_institutions_claiming_monopoly, beneficiary).

% Would benefit from regional peace and trade stability but are not seated in the feud obligation structure itself. They are dragged into cycles through alliance claims, marriage ties, or raiding for plunder. Their preferences for non-participation are structurally ignored—the obligation framework does not recognize opt-out; it only recognizes kinship and enemy.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, neighboring_non_feuding_groups, excluded,
    moderate, generational, constrained, regional).

% Examines the feud system from outside the medieval legal framework, measuring extractiveness, mortality, and territorial consolidation impact. Produces comparative analysis showing that feuding societies exhibit lower capital accumulation, shorter state-formation timelines, and higher per-capita violence compared to centralized-enforcement baselines.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, historical_observer_modern_jurisprudence, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__extraction_cycle_reading, emerging_royal_authority).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__extraction_cycle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: On this reading, no coordination function exists or is being served. The extraction reading denies that blood-feud obligation coordinates justice or deterrence—it instead models the cycle as pure waste: participants maintain military readiness and honor-code enforcement not because it solves a problem, but because renouncing the obligation means social death. The 'coordination' claimed by the stateless_coordination_reading is rejected as a rationalization after the fact; the cycle persists through identity-lock and suppression of exit, not through functional necessity.
% TRANSFER_FUNCTION: Transfers violence capacity (blood-debt repayment obligations) from productive kinship groups into cycles of retaliation that exhaust labor, capital, and reproductive cohorts. The constraint routes political legitimacy toward emerging royal and ecclesiastical authorities—their claim to monopolize justice is strengthened by the cycle's persistence. Transfers wealth from occupied territories toward military expenditure and fortification, then toward tribute and tithes paid in exchange for peace-making promises.
% ABSENT_VOICES: The voices excluded are: (1) non-feuding neighboring groups who would advocate for open trade and regional peace but lack kinship standing to negotiate feud terms; (2) subordinate lineages whose disproportionate casualty burden is never weighted in compensation calculations; (3) the productive capacity of territories themselves, which cannot represent the economic cost of instability; (4) future generations whose inheritance is depleted by each cycle's violence.
% DISAPPEARANCE_RATIONALE: If blood-feud obligation vanished overnight, kinship groups would retain internal coherence but lose the obligation to pursue blood debt, dramatically reducing inter-group violence, freeing labor for agricultural production, and allowing territorial consolidation around defensible boundaries under centralized authority. Royal and ecclesiastical institutions would lose their primary legitimacy claim (that they alone can restore peace) and would face pressure to justify their authority through law and consent rather than through pacification monopoly. Trade routes would stabilize, capital accumulation would accelerate, state formation would follow within 1–2 generations.
% FOUNDING_PROBLEM: Early medieval societies lacked centralized enforcement machinery. Blood-feud obligation functioned as a decentralized deterrent: if harming a member meant that the entire kinship group would pursue retaliation with social-death consequences for failure, it constrained opportunistic violence. Over time, this mechanism created a destructive stable equilibrium: groups could not exit without losing honor-based identity; each generation renewed the obligation; the cycle consumed productive capacity.
% FOUNDING_PROBLEM_CORROBORATION: Royal historians and ecclesiastical authorities claim the founding problem persists (justifying monopoly on violence). Comparative historical data and economic reconstructions from outside the benefiting parties show that by the high medieval period, the constraint's original deterrent function had degraded into pure extraction: retaliation cycles were increasingly stylized, honor-codes were increasingly theatrical, and the constraint's persistence depended on suppressing kinship-group exit and denying the legitimacy of territorial peace-bonds. The founding problem (lack of deterrence) was solved by the rise of royal law; the constraint itself became an obstacle to consolidation, not its prerequisite.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.81 reflects: (1) the constraint routes violence capacity and productive labor into cycles that generate minimal net deterrence value (the founding problem is solved once royal law emerges); (2) the ongoing retaliation cycles extract wealth from territories in the form of fortification costs, military readiness, and labor diversion; (3) royal and ecclesiastical authorities extract legitimacy and tribute by positioning themselves as the sole pacifiers. Suppression at 0.79 reflects: (1) the kinship-group exit is blocked by identity-fusion (renouncing the obligation means social death); (2) alternative dispute-resolution mechanisms (peace-bonds, oath-swearing to neutral authorities, compensation schemes) are denied legitimacy by those benefiting from continued cycles; (3) ecclesiastical condemnation of private vengeance reframes feud obligation as sin, adding moral suppression to structural suppression. Theater-ratio rises from 0.15 to 0.41 because as royal law solidifies, retaliation cycles increasingly become performative: they are carried out under formulaic codes (the ritualized raid, the ceremonial compensation), but the actual deterrent function has been displaced onto law and royal justice. The constraint persists not because it works but because kinship identity is fused with the obligation and because benefiting authorities have suppressed alternative frames. Measurement grid is aligned: every metric is authored at every time point within the shared interval [0, 40].
 *
 * PERSPECTIVAL GAP:
 *   The kinship-group seats and the royal/ecclesiastical seats compute different types from the same structural data. Kinship-group payers experience the constraint as identity-locked extraction (high d, high χ) from which exit means death-equivalent social exclusion. Royal and ecclesiastical agenda-setters experience it as strategic opportunity for authority consolidation (low d, negative χ as subsidy to their monopoly claim). The subordinate lineages experience higher effective extraction than high-status lineages because they bear retaliation costs disproportionately but capture negligible compensation. The neighboring non-feuding groups, excluded from the framework, experience the constraint as external threat with no voice in its terms. The engine computes each seat's type from the structural data—power, exit, beneficiary/victim positioning—and the divergence across seats IS the measurement the framework exists to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Kinship groups: trapped (identity_locked exit), moderate power (can field military but cannot negotiate with royal authority as equals), generational time horizon (obligation reproduces across lifespans) → d near 0.85 (near-full target). Subordinate lineages: powerless, trapped, biographical horizon, absorb disproportionate cost → d near 0.92 (extreme target). Royal authority: institutional, arbitrage exit (can reframe or abandon pacification claims if challenged by larger rivals), generational horizon → d near 0.15 (beneficiary-aligned). Ecclesiastical institutions: institutional, arbitrage (can shift from feud condemnation to other salvation narratives if political winds shift), civilizational horizon → d near 0.12 (beneficiary-aligned). Neighboring non-feuding groups: moderate power, constrained exit (can only ally or be raided, no neutral option), generational horizon, excluded from negotiation → d near 0.71 (partial target, pulled into cycles without consent). The measurement captures structural relationships: those identity-locked to the kinship framework and bearing mortality costs sit at the extractive end; those administering the framework sit at the beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a clear candidate for mandatrophy resolution. The founding problem (lack of deterrence in stateless societies) is dead once royal law emerges in the 10th–12th centuries. The constraint persists after its problem is solved, maintained by: (1) identity-lock (renouncing the obligation means social death), (2) suppression of alternative frames (ecclesiastical condemnation of private vengeance, royal monopoly claims), and (3) structural benefit to authorities (royal and ecclesiastical legitimacy is strengthened by the cycle's destruction). The measurement trajectory shows theater-ratio rising (retaliation cycles become increasingly performative while law provides actual deterrence) while extractiveness plateaus—classic piton dynamics. However, the constraint is claimed as snare, not piton, because the extraction is concentrated (beneficiaries are identifiable—royal and ecclesiastical authorities) rather than diffuse. Piton would require that the constraint persists by inertia with no concentrated beneficiary; snare is appropriate when identified parties benefit from suppression of exit and from the cycle's perpetuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_resurrection_vs_artifact,
    'Does blood-feud obligation persist because the founding problem (lack of deterrence) resurrected in regions where royal law failed, or does it persist only where suppression of alternatives and identity-lock remain intact?',
    'Comparative case analysis of feud persistence in regions with functioning royal law (where resurgence should be absent or minimal) versus regions with state collapse (where functional deterrence should drive persistence). If feuds persist in functioning-law regions only where kinship identity remains fused to the obligation, the persistence is suppression-driven (supporting the snare reading). If feuds resurrect whenever royal law fails even without kinship identity reinforcement, the persistence is functional (supporting the stateless-coordination reading).',
    'If suppression-driven, the constraint is definitively a snare with identifiable victims and beneficiaries. If functional, the snare reading collapses and the stateless-coordination reading gains purchase as the primary explanation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_resurrection_vs_artifact, empirical, 'Whether feud persistence reflects suppression or functional necessity.').

omega_variable(
    beneficiary_intentionality_in_persistence,
    'Do royal and ecclesiastical authorities actively suppress alternative dispute mechanisms (peace-bonds, neutral arbitration, compensation schedules) to maintain feud obligation as a problem requiring their monopoly solution, or is suppression a side effect of law-enforcement without intentional strategic benefit-capture?',
    'Historical evidence of: (1) explicit royal decrees prohibiting peace-bonds or neutral arbitration; (2) ecclesiastical councils condemning private dispute resolution; (3) patterns of enforcement targeting peace-oath violators more harshly than feud participants (evidence of strategic suppression). Corroborate with administrative records showing tithes and tribute collection increasing when feuds intensify (beneficiary profit motive evident).',
    'If intentional strategic suppression is evident, the authorities are conscious agenda-setters (snare reading fully supported). If suppression is incidental to law-enforcement, the reading weakens—the constraint might be a tangled rope (forced pacification coordination with asymmetric extraction) rather than pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_intentionality_in_persistence, empirical, 'Whether beneficiary persistence is strategic or incidental.').

omega_variable(
    identity_lock_malleability_across_generations,
    'Is kinship identity fused with blood-feud obligation irreversibly (rendering exit equivalent to death across generations), or is the identity-lock maintained by institutional suppression of alternative identity frames (rendering exit theoretically reversible if suppression were lifted)?',
    'Historical evidence of: (1) kinship groups that successfully adopted new identity frames (Christian martyr status, royal vassal honor, territorial settlement status) and abandoned the obligation without losing group coherence; (2) patterns of younger generations resisting the obligation more strongly when alternative legitimacy sources emerge; (3) rapid collapse of feuding when suppression is lifted (short-run evidence) versus slow persistence when suppression remains (long-run evidence).',
    'If malleably institutionalized (not intrinsically irreversible), the identity-lock is a form of suppression that policy interventions could theoretically dismantle, and the constraint is a pure snare (exits are blocked but theoretically unblockable). If irreversibly fused at the kinship level, the exit is harder to dislodge and the constraint approaches a piton (the obligation persists as internalized practice regardless of suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_malleability_across_generations, empirical, 'Whether identity-lock is structural or institutional.').

omega_variable(
    alternative_readings_kernel_contest,
    'This constraint is one reading of the feud_obligation_kernel; how should the other readings (stateless_coordination_reading, christianized_pacification_reading) be evaluated and classified?',
    'Separate constraint story files will be generated for each sibling reading, modeling feud obligation as pure coordination or as theological obligation, respectively. Each reading will carry its own ε, beneficiary/victim set, and structural metrics. The three constraints together form a constraint family linked by network.affects_constraints; the family enables cross-reading comparative analysis.',
    'The kernel contest is irreducible at this reading''s level. This story instantiates the extraction-cycle reading and argues feud obligation is a snare; sibling readings will argue it is a rope (stateless coordination) or a tangled rope (theological pacification with institutional coordination). The corpus''s job is to preserve all three readings with their structural data intact, enabling empirical testing of which reading captures the actual mechanism in any given medieval society or time period.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_kernel_contest, conceptual, 'Kernel contest: three readings of blood-feud obligation coexist.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of feud-obligation exit structural (external barriers: law enforcement, ecclesiastical sanction, social exclusion machinery) or internalized (kinship members have absorbed the obligation as their identity and cannot imagine rejecting it even if barriers were removed)?',
    'Post-suppression empirical test: in regions where royal law is enforced and ecclesiastical pressure is lifted (external barriers removed), do kinship groups continue practicing blood-feud obligation, or does it collapse rapidly? If it persists, suppression is partially internalized. If it collapses, suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests—exit would remain difficult even if enforcement were lifted. If structural, policy intervention (law enforcement, reframing legitimacy) could dislodge the constraint more readily.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(feud_tr_t0, observed).
narrative_ontology:measurement(feud_tr_t5, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement_basis(feud_tr_t5, observed).
narrative_ontology:measurement(feud_tr_t10, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(feud_tr_t10, observed).
narrative_ontology:measurement(feud_tr_t15, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(feud_tr_t15, observed).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(feud_tr_t20, observed).
narrative_ontology:measurement(feud_tr_t25, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 25, 0.36).
narrative_ontology:measurement_basis(feud_tr_t25, observed).
narrative_ontology:measurement(feud_tr_t30, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement_basis(feud_tr_t30, observed).
narrative_ontology:measurement(feud_tr_t35, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(feud_tr_t35, observed).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(feud_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 0, 0.61).
narrative_ontology:measurement_basis(feud_be_t0, observed).
narrative_ontology:measurement(feud_be_t5, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 5, 0.66).
narrative_ontology:measurement_basis(feud_be_t5, observed).
narrative_ontology:measurement(feud_be_t10, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(feud_be_t10, observed).
narrative_ontology:measurement(feud_be_t15, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement_basis(feud_be_t15, observed).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement_basis(feud_be_t20, observed).
narrative_ontology:measurement(feud_be_t25, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement_basis(feud_be_t25, observed).
narrative_ontology:measurement(feud_be_t30, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement_basis(feud_be_t30, observed).
narrative_ontology:measurement(feud_be_t35, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 35, 0.81).
narrative_ontology:measurement_basis(feud_be_t35, observed).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(feud_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(feud_su_t0, observed).
narrative_ontology:measurement(feud_su_t5, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(feud_su_t5, observed).
narrative_ontology:measurement(feud_su_t10, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(feud_su_t10, observed).
narrative_ontology:measurement(feud_su_t15, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(feud_su_t15, observed).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(feud_su_t20, observed).
narrative_ontology:measurement(feud_su_t25, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 25, 0.74).
narrative_ontology:measurement_basis(feud_su_t25, observed).
narrative_ontology:measurement(feud_su_t30, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(feud_su_t30, observed).
narrative_ontology:measurement(feud_su_t35, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 35, 0.78).
narrative_ontology:measurement_basis(feud_su_t35, observed).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement_basis(feud_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__extraction_cycle_reading, 0.12).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the feud_obligation_kernel. The extraction-cycle reading models blood-feud obligation as destructive extraction maintained through identity-lock and suppression, with royal/ecclesiastical authorities as beneficiaries. Sibling readings frame the same kernel as: (1) stateless-coordination reading: genuine decentralized deterrence mechanism solving the problem of violence in absence of centralized enforcement; (2) christianized-pacification reading: theological obligation violating divine law, requiring ecclesiastical/royal monopoly on legitimate violence. The three readings share the same kernel (the meaning and function of blood-feud obligation) but generate three distinct constraints with different ε values, beneficiary/victim sets, and structural metrics. The extraction-cycle reading positions feud participants as victims and authorities as beneficiaries; the stateless-coordination reading positions participants as coordinating beneficiaries; the christianized-pacification reading positions the kinship obligation as the victim (object of theological violation) and divine/institutional authority as the beneficiary. The readings are linked via network.affects_constraints (each reading influences how the others are interpreted) and are analyzed together as a constraint family to enable empirical testing of which reading captures the actual mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feud_obligation_kernel__extraction_cycle_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
