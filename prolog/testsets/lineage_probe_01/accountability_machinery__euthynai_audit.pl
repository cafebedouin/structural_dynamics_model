% ============================================================================
% CONSTRAINT STORY: accountability_machinery__euthynai_audit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_accountability_machinery__euthynai_audit, []).

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
 *   constraint_id: accountability_machinery__euthynai_audit
 *   human_readable: Euthynai: Universal Audit of Officials at Office Exit
 *   domain: legal/doctrinal/athenian_democracy
 *
 * SUMMARY:
 *   The euthynai (auditing magistrates) mechanism represents one reading of
 *   how Athenian democracy structured accountability: every official leaving
 *   office faced mandatory audit, with any citizen able to bring accusation.
 *   No official could exit without reckoning — the constraint suppressed the
 *   option of leaving office without facing public examination and potential
 *   prosecution. This reading instantiates accountability as a *coordination
 *   problem solved by transparency*: the demos benefits from knowing every
 *   magistrate's conduct has been publicly examined, without the demos
 *   needing to monitor each official continuously. The beneficiary is the
 *   demos (collective accountability achieved); the victims are officials
 *   facing prosecution, particularly those targeted by organized factions or
 *   lacking resources to defend themselves. The constraint exhibits genuine
 *   coordination function (Rope from institutional/analytical perspectives)
 *   alongside extracted value from politically-motivated prosecutions (Snare
 *   for trapped officials, Tangled Rope for organized factional targets). The
 *   extractiveness value (0.18) reflects that the primary function is
 *   accountability coordination, not extraction — though extractive overlay
 *   exists. The suppression value (0.65) reflects high barriers to exit
 *   without facing consequences, a feature of the mechanism's anti-corruption
 *   design rather than a sign of pure coercion.
 *
 * KEY AGENTS:
 *   - Demos (Athenian Citizenry): Primary beneficiary (institutional/arbitrage) — benefits from guaranteed accountability of all magistrates without needing continuous monitoring; designed exit mechanism for magistrial abuses
 *   - Honest Officials: Secondary beneficiary (powerful/constrained) — experience euthynai as formalized exit ritual that establishes legitimacy and protects from later challenge when accounts are clean
 *   - Peculating Officials: Primary victim (powerless/trapped) — clawed-back extractiveness in office; unable to exit without facing prosecution; no exit option
 *   - Wealthy Rivals Facing Factional Prosecution: Secondary victim (organized/constrained) — experience euthynai as weaponized by organized factions; formal audit structure enables factional prosecution rather than preventing it
 *   - Reform Coalition: Temporary institutional actor (organized/constrained) — implemented euthynai as scaffold; expected sunset as democratic norms internalized (sunset never occurred)
 *   - Assembly of Jurors: Institutional enforcer (institutional/arbitrage) — applies euthynai verdicts; can be influenced by factional rhetoric and wealth of defendants
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks naturalizing euthynai as immutable law of governance rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(accountability_machinery__euthynai_audit, 0.18).
domain_priors:suppression_score(accountability_machinery__euthynai_audit, 0.65).
domain_priors:theater_ratio(accountability_machinery__euthynai_audit, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(accountability_machinery__euthynai_audit, extractiveness, 0.18).
narrative_ontology:constraint_metric(accountability_machinery__euthynai_audit, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(accountability_machinery__euthynai_audit, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(accountability_machinery__euthynai_audit, rope).
narrative_ontology:human_readable(accountability_machinery__euthynai_audit, "Euthynai: Universal Audit of Officials at Office Exit").
narrative_ontology:topic_domain(accountability_machinery__euthynai_audit, "legal/doctrinal/athenian_democracy").

domain_priors:requires_active_enforcement(accountability_machinery__euthynai_audit).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(accountability_machinery__euthynai_audit, '0b2ce142-5335-412a-a959-0afdd1c7cecb').
narrative_ontology:cs_kernel_codification('0b2ce142-5335-412a-a959-0afdd1c7cecb', formalized).
narrative_ontology:cs_authority_grounding('0b2ce142-5335-412a-a959-0afdd1c7cecb', lineage).
narrative_ontology:cs_interpretation_layer_present('0b2ce142-5335-412a-a959-0afdd1c7cecb').
narrative_ontology:cs_reading_relation('0b2ce142-5335-412a-a959-0afdd1c7cecb', accountability_machinery__graphe_paranomon, coexists_with).
narrative_ontology:cs_reading_relation('0b2ce142-5335-412a-a959-0afdd1c7cecb', accountability_machinery__ostracism_institution, coexists_with).
narrative_ontology:cs_axiom('0b2ce142-5335-412a-a959-0afdd1c7cecb', foundational, exit_reckoning_mandatory).
narrative_ontology:cs_axiom_status(exit_reckoning_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('0b2ce142-5335-412a-a959-0afdd1c7cecb', exit_reckoning_mandatory, deontological).
narrative_ontology:cs_axiom('0b2ce142-5335-412a-a959-0afdd1c7cecb', foundational, citizen_accusation_standing).
narrative_ontology:cs_axiom_status(citizen_accusation_standing, holdable).
narrative_ontology:cs_axiom_grounding('0b2ce142-5335-412a-a959-0afdd1c7cecb', citizen_accusation_standing, conventional).
narrative_ontology:cs_reference_frame('0b2ce142-5335-412a-a959-0afdd1c7cecb', universal_audit_athenian_democracy).
narrative_ontology:cs_drift_state('0b2ce142-5335-412a-a959-0afdd1c7cecb', late_fourth_century_bce, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0b2ce142-5335-412a-a959-0afdd1c7cecb', '').
narrative_ontology:cs_kernel_id(accountability_machinery__euthynai_audit, accountability_machinery).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(accountability_machinery__euthynai_audit, demos_principal).
narrative_ontology:constraint_beneficiary(accountability_machinery__euthynai_audit, athenian_citizenry).
narrative_ontology:constraint_victim(accountability_machinery__euthynai_audit, peculating_officials).
narrative_ontology:constraint_victim(accountability_machinery__euthynai_audit, abusive_magistrates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% An official with clean accounts and no accusations experiences euthynai as pure coordination: a formalized exit ritual that establishes legitimacy and protects the official from later challenge. Low extraction, high transparency. The constraint solves the collective action problem of verifying official conduct without requiring constant monitoring during office.
constraint_indexing:constraint_classification(accountability_machinery__euthynai_audit, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% An official facing accusation from wealthy rivals, with no resources to defend themselves in court, experiences euthynai as pure extraction. They cannot exit office without facing charges they cannot afford to contest. Suppression is absolute — exit is blocked; the only way out is to pay their accusers or lose their property.
constraint_indexing:constraint_classification(accountability_machinery__euthynai_audit, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The demos benefits from universal audit as a coordination mechanism: every magistrate's conduct is publicly examined, creating ongoing accountability without the demos needing to monitor each official continuously. The demos has structured exit from magisterial abuse — the euthynai process. Extractiveness is minimal; the constraint solves a genuine collective action problem.
constraint_indexing:constraint_classification(accountability_machinery__euthynai_audit, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% A wealthy official facing prosecution from organized political factions (not as victim of genuine malfeasance, but as target of factional competition) experiences euthynai as mixed coordination and extraction. The formal audit structure is designed to prevent factional immunity, but factions use it as a weapon — they coordinate their accusations, dominate the courts, and extract political advantage. The official cannot exit without facing the organized faction's combined power.
constraint_indexing:constraint_classification(accountability_machinery__euthynai_audit, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% Reformers who implemented euthynai saw it as a temporary solution with an implicit sunset: universal audit was necessary *until* democratic norms became internalized and officials developed intrinsic accountability. The constraint was framed as a bridge to a future state where formal audits could relax. Over generations, the audit became institutionalized rather than dissolved — transforming from scaffold to rope.
constraint_indexing:constraint_classification(accountability_machinery__euthynai_audit, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% By the fourth century, euthynai had become largely performative: the formal ritual persisted through institutional inertia even as real accountability mechanisms had shifted (wealthy magistrates could afford better legal defense, juries could be influenced by rhetorical skill). The audit remained on the books as a symbol of democratic scrutiny, but its power to actually constrain abuse had degraded. Theater ratio reflects persistent ritual with declining functional force.
constraint_indexing:constraint_classification(accountability_machinery__euthynai_audit, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(local))).

% From a universal/civilizational view, euthynai instantiates an allegedly immutable principle: any official given discretionary power *must* be audited upon exit, or power becomes unaccountable. This perspective treats the euthynai mechanism as a natural law of governance — a structural necessity, not a constructed institutional choice. However, the beneficiary declarations and extractiveness metrics contradict this naturalization.
constraint_indexing:constraint_classification(accountability_machinery__euthynai_audit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(accountability_machinery__euthynai_audit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(accountability_machinery__euthynai_audit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(accountability_machinery__euthynai_audit, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(accountability_machinery__euthynai_audit, TR),
    TR >= 0.70.

:- end_tests(accountability_machinery__euthynai_audit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The euthynai mechanism is primarily a coordination solution to the genuine problem of magistrial accountability — how does a democracy ensure officials don't abuse power without monitoring each one constantly? The mechanism distributes monitoring to the entire demos (any citizen can accuse) and creates a formal exit ritual (audit before leaving office). The primary beneficiary is the collective demos; extractiveness from individuals is secondary. The value rises slightly over the interval (0.08 → 0.18) as factional factions increasingly use the audit mechanism as a political weapon. Suppression (0.65): High. The constraint suppresses exit-without-reckoning absolutely — every official must face audit, and the demos (through citizen accusers) has standing to bring charges. This suppression is *by design*, not incidental; it is the mechanism's core anti-corruption feature. Officials cannot avoid the constraint without leaving the system entirely. Theater ratio (0.35): Low-moderate. The euthynai process involves genuine examination of accounts and evidence of conduct; it is not primarily performative. The ratio rises slightly as rhetorical skill and jury influence become more important relative to documentary evidence (0.25 → 0.35), but remains below 0.50 throughout the interval shown.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits dramatically different classifications across perspectives. For the demos and honest officials, euthynai is pure coordination (Rope) — it solves a genuine collective action problem (how to ensure official accountability) without extracting from those it governs. For peculating or abusive officials, it is pure extraction (Snare) — they cannot exit office without facing consequences, suppression is absolute. For wealthy officials facing factional prosecution, it is mixed coordination and extraction (Tangled Rope) — the formal mechanism is designed to prevent factional immunity, but organized factions capture the process and use it as a weapon. For late-period observers, the mechanism becomes partially degraded (Piton) — the ritual persists but its force has eroded. For the early reformers, it was always intended as temporary (Scaffold) — a bridge to internalized democratic norms. The analytical observer risks seeing immutable law (Mountain) but the structural data contradicts this naturalization: beneficiaries exist (the demos), victims are identifiable (peculating officials), and extractiveness is measurable (0.18, rising). This is a reading of a contested kernel about accountability, not an eternal law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the agent's structural position relative to the extraction flow. Honest officials benefit from euthynai (low d); peculating officials bear costs (high d); the demos is the primary beneficiary (low d). For wealthy officials facing organized factional prosecution, the derivation requires care: they have resources (high power) and some exit options (constrained, not trapped), but they are structurally victimized by coordinated factions using the formal audit as cover for political warfare. Their d sits intermediate between pure beneficiary and pure target. The demos experiences the constraint as coordination (low extraction experienced), because their collective exit option is activated (if magistrates abuse power, the demos can prosecute at audit). Peculating officials experience high extraction because their exit is blocked (cannot leave without facing charges) and their defense costs are borne entirely by them. The early reformers experience it as low extraction (scaffold) because they see the mechanism as temporary and designed to transition to internalized norms. The late-period assembly experiences it as institutional arbitrage (low d) — the formal ritual persists and the assembly administers it, but the assembly's power to actually constrain abuse has eroded.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing the coordination problem (how to ensure magistrial accountability at scale without continuous monitoring) from the extraction problem (how factional factions use the audit mechanism as a political weapon). Early period euthynai is primarily coordination (Rope), with extractive overlay only for genuinely corrupt officials. Late period euthynai remains Rope for honest officials and the demos, but becomes Tangled Rope for politically-targeted officials as factions learn to weaponize the mechanism. The piton classification (late period, degraded) reflects that the theater of formal audit has increased (juries influenced by rhetoric rather than documentary evidence) while its function (actually preventing abuse) has declined. The constraint does not slide from one type to another — different perspectives see different types simultaneously. The mandatrophy is resolved by recognizing that all six types are present in the perspectival presheaf: Rope (coordination-dominant), Snare (extraction-dominant for trapped), Tangled Rope (mixed for organized factions), Piton (degraded ritual), Scaffold (early reform vision), Mountain (naturalization to be rejected).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    factional_capture_threshold,
    'At what point does the audit mechanism shift from preventing corruption to enabling factional prosecution?',
    'Historical analysis of euthynai prosecutions: ratio of convictions for actual malfeasance vs. convictions driven by factional alignment of jurors; comparison of penalty severity across political groups',
    'If capture threshold is crossed early (Fourth Century BCE): euthynai becomes primarily an extraction weapon for organized factions rather than accountability mechanism. Classification shifts toward Tangled Rope or Snare for politically targeted officials.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(factional_capture_threshold, empirical, 'Threshold at which euthynai transitions from accountability to factional weapon').

omega_variable(
    wealth_defense_asymmetry,
    'Does wealth provide asymmetric defense capacity in euthynai prosecution, undermining the mechanism''s anti-corruption function?',
    'Comparative analysis of outcomes: prosecution/conviction rates by wealth class; ability to hire logographers (speech-writers) and hire jurors'' advocates; property restoration rates by wealth',
    'If substantial asymmetry confirmed: euthynai functions as class-differentiator rather than universal accountability. Honest poor officials face snare; honest rich officials face rope. Extractiveness increases for powerless magistrates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wealth_defense_asymmetry, empirical, 'Wealth gradient in euthynai prosecution outcomes').

omega_variable(
    accusation_filtering_mechanism,
    'What prevents frivolous accusations from flooding the euthynai process and making it a harassment tool?',
    'Documentary evidence of filters: were accusers bonded or required to post security? What were penalties for failed prosecutions? How many accusations were brought vs. proceeded to trial?',
    'If filtering is weak: euthynai enables mass accusation harassment, increasing suppression for all officials. If filtering is strong: euthynai maintains coordinating function. Difference between rope and snare depends on this mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accusation_filtering_mechanism, empirical, 'Robustness of accusation filters in euthynai process').

omega_variable(
    reading_kernel_ambiguity,
    'Is the euthynai a reading of a contested kernel about accountability machinery, or is it a distinct constraint from the graphe paranomon and ostracism readings?',
    'Structural analysis: Do all three mechanisms (euthynai, graphe paranomon, ostracism) ground their legitimacy in the same foundational commitment about how accountability operates in a democracy? Or do they instantiate fundamentally different accountability theories? If the former, they are readings of a single kernel; if the latter, they are independent constraints.',
    'If single kernel: the three readings coexist and may foreclose or influence each other. If independent constraints: they are sister mechanisms with separate extractiveness profiles and network dependencies. Classification architecture changes accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether euthynai is a reading of accountability_machinery kernel or independent constraint').

omega_variable(
    internalized_democratic_norms,
    'Did the euthynai mechanism ever successfully create intrinsic democratic accountability (officials self-policing), or did it remain dependent on external coercion throughout its existence?',
    'Historical counterfactual: comparison of official conduct quality in early (high formalism, high demos enforcement) vs. late period (lower formalism, lower demos enforcement); analysis of rhetoric about official duties in speeches across centuries',
    'If intrinsic norms developed: the scaffold reading is vindicated — euthynai functioned as a bridge toward internalized accountability. If external coercion remained necessary: euthynai is a rope mechanism that never achieved its scaffold goal. Affects periodization and theater_ratio trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_democratic_norms, conceptual, 'Degree to which euthynai created intrinsic vs. extrinsic accountability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(accountability_machinery__euthynai_audit, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(euthynai_tr_t0, accountability_machinery__euthynai_audit, theater_ratio, 0, 0.25).
narrative_ontology:measurement(euthynai_tr_t25, accountability_machinery__euthynai_audit, theater_ratio, 25, 0.3).
narrative_ontology:measurement(euthynai_tr_t50, accountability_machinery__euthynai_audit, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(euthynai_be_t0, accountability_machinery__euthynai_audit, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(euthynai_be_t25, accountability_machinery__euthynai_audit, base_extractiveness, 25, 0.15).
narrative_ontology:measurement(euthynai_be_t50, accountability_machinery__euthynai_audit, base_extractiveness, 50, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(accountability_machinery__euthynai_audit, enforcement_mechanism).
narrative_ontology:affects_constraint(accountability_machinery__euthynai_audit, accountability_machinery__graphe_paranomon).
narrative_ontology:affects_constraint(accountability_machinery__euthynai_audit, accountability_machinery__ostracism_institution).

% DUAL FORMULATION NOTE:
% The euthynai is structurally downstream from the demos' foundational commitment to accountability (kernel: accountability_machinery). It is a sibling reading to graphe paranomon (proposer liability) and ostracism (direct exile), all three grounding their legitimacy in the same kernel. Graphe paranomon addresses deception in assembly decisions; ostracism addresses prominence-risk; euthynai addresses corruption-in-office. The three readings influence each other: euthynai provides the formal audit structure; graphe paranomon provides the self-binding mechanism; ostracism provides the blunt remove option. None forecloses the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
