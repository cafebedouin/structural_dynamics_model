% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Legislative Supremacy: Override-Conclusive Constitutional Meaning
 *   domain: constitutional/political/comparative-law
 *
 * SUMMARY:
 *   A constitutional settlement in which the elected legislature holds final
 *   authority over constitutional meaning: courts interpret, issue rulings
 *   and declarations of inconsistency, and supply authoritative advice, but
 *   the sitting majority can re-enact or override any judicial determination
 *   through a notwithstanding clause or simple legislative act. This story
 *   instantiates the legislative sovereignty reading of the constitutional
 *   text kernel (see commentary.kernel_context); the standing arrangement
 *   under contest — the override-conclusive settlement itself — is the
 *   epsilon referent, assessed by this reading's own lights, never the
 *   judicial-supremacy arrangement this reading rejects. Claim/metric
 *   independence: the constraint is CLAIMED as tangled_rope because it
 *   possesses both a genuine coordination function (electorally accountable
 *   resolution of constitutional disputes) and asymmetric extraction
 *   (minorities and the subordinated judiciary bear costs the majority does
 *   not); the authored metrics describe its actual operation independently of
 *   that claim. Sibling readings of the same text are separate constraint
 *   stories, linked through network.affects_constraints. KEY AGENTS (by
 *   structural relationship): - parliamentary_majorities: Agenda-setter and
 *   primary beneficiary (institutional/mobile) — holds and exercises the
 *   override; collects final interpretive authority -
 *   majoritarian_electorate: Beneficiary with secondary payer exposure
 *   (organized/mobile) — gains majoritarian self-governance, carries diffuse
 *   override risk - minority_groups: Primary target (powerless/constrained) —
 *   bears the cost of overridable rights protection - apex_judiciary:
 *   Subordinated interpreter (institutional/identity_locked) — bears the cost
 *   of revocable authority - minority_rights_advocates: Excluded from the
 *   formal determination (organized/constrained) - constitutional_scholars:
 *   Analytical observer — sees the full comparative structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.62).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.58).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Legislative Supremacy: Override-Conclusive Constitutional Meaning").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional/political/comparative-law").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, '393e0682-1898-457e-aa2d-914a08038eec').
narrative_ontology:cs_kernel_codification('393e0682-1898-457e-aa2d-914a08038eec', fixed_text).
narrative_ontology:cs_authority_grounding('393e0682-1898-457e-aa2d-914a08038eec', practice).
narrative_ontology:cs_interpretation_layer_present('393e0682-1898-457e-aa2d-914a08038eec').
narrative_ontology:cs_reading_relation('393e0682-1898-457e-aa2d-914a08038eec', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('393e0682-1898-457e-aa2d-914a08038eec', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('393e0682-1898-457e-aa2d-914a08038eec', foundational, elected_legislature_conclusive_interpretive_authority).
narrative_ontology:cs_axiom_status(elected_legislature_conclusive_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('393e0682-1898-457e-aa2d-914a08038eec', elected_legislature_conclusive_interpretive_authority, conventional).
narrative_ontology:cs_axiom('393e0682-1898-457e-aa2d-914a08038eec', secondary, judicial_review_advisory_not_binding).
narrative_ontology:cs_axiom_status(judicial_review_advisory_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('393e0682-1898-457e-aa2d-914a08038eec', judicial_review_advisory_not_binding, instrumental).
narrative_ontology:cs_reference_frame('393e0682-1898-457e-aa2d-914a08038eec', electoral_majoritarian_settlement).
narrative_ontology:cs_drift_state('393e0682-1898-457e-aa2d-914a08038eec', contemporary_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('393e0682-1898-457e-aa2d-914a08038eec', '2026-08-04T12:00:00Z').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, parliamentary_majorities).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, majoritarian_electorate).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, minority_groups).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, apex_judiciary).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, apex_judiciary).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, majoritarian_electorate).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, political_constitutionalism_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, counter_majoritarian_objection_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the power to re-enact or override any judicial determination of constitutional meaning, through a notwithstanding clause or simple legislative act. Sets the terms on which the courts' constitutional advice is accepted or rejected, and controls the legislative agenda on which override decisions are taken. Gains the ability to enact its program without a judicial veto; its members answer to voters at the next election, which is the main check on how the override is used. Because nothing entrenches the arrangement against it, it can also restructure the settlement itself.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, parliamentary_majorities, agenda_setter,
    institutional, biographical, mobile, national).

% Gets the laws it voted for upheld without judicial invalidation, and can change constitutional direction through ordinary elections rather than amendment supermajorities or doctrinal evolution. Individual voters who belong to some minority — religious, linguistic, ideological — also carry the risk that a future majority overrides protections they personally rely on; their protection is only as secure as their current electoral weight.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, majoritarian_electorate, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, majoritarian_electorate, payer).

% Rely on constitutional rights protections that the sitting majority can override at will. They litigate, petition, and mobilize, but when a court's ruling in their favor is overridden, no further institutional recourse exists inside the arrangement. Leaving the jurisdiction is costly and rarely feasible; their day-to-day protection depends on majority restraint and on the political cost of invoking the override.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, minority_groups, payer,
    powerless, generational, constrained, national).

% Interprets the constitution, issues rulings and declarations of inconsistency, and supplies the authoritative advice the legislature may accept or reject. Its constitutional rulings carry persuasive force, shape public debate, and are usually complied with — but its final determinations are revocable by ordinary legislative act. The court cannot resign from the constitutional order it interprets: its role, prestige, and docket are constituted by the very arrangement whose limits it chafes against, and its members are appointed through processes the governing majority influences.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, apex_judiciary, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, apex_judiciary, beneficiary).

% Litigate test cases, publish constitutional argument, and campaign to entrench judicial review or raise the political cost of invoking the override. They have no formal seat in the override decision: when the legislature invokes the override, their arguments are heard only in the advisory proceedings that the override then erases. Their influence runs entirely through public opinion and the courts' advisory voice.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, minority_rights_advocates, excluded,
    organized, generational, constrained, national).

% Compare override jurisdictions across countries, track invocation rates and their political consequences, and adjudicate the theoretical dispute over where final interpretive authority over a constitution should sit. They collect no direct gain from the arrangement and bear none of its costs; their analyses feed the public debate the other seats conduct.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__legislative_sovereignty_reading, parliamentary_majorities).
narrative_ontology:fixing_cost_class(constitutional_text__legislative_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, electorally accountable decision point for constitutional disputes: when courts and the elected legislature disagree about what the constitution permits, the arrangement routes final resolution to the legislature, avoiding interpretive deadlock between branches and keeping fundamental law responsive to ordinary electoral majorities rather than amendment supermajorities or judicial doctrine.
% TRANSFER_FUNCTION: Moves final interpretive authority over constitutional meaning from the courts to the sitting legislative majority, and moves the security of rights protections from minorities — who can no longer rely on a judicial determination sticking — to the discretion of that majority.
% ABSENT_VOICES: Permanent minorities — groups that cannot plausibly command a legislative majority — have no seat in the override decision; their interests appear only through the advisory proceedings the override can erase. Future generations bound by overrides are likewise unrepresented. Rights-advocacy organizations argue from outside the formal process, with no vote in the outcome.
% DISAPPEARANCE_RATIONALE: If the override power and the subordination of courts vanished overnight, judicial determinations of constitutional meaning would become conclusive, legislative behavior would reorganize around the new limitation (bills pre-cleared for constitutional compliance, litigation strategy transformed), minority protections would harden, and the electoral bargain — full policy control in exchange for rights exposure — would have to be renegotiated. The constitutional order as constituted depends on the arrangement.
% FOUNDING_PROBLEM: The counter-majoritarian difficulty: in a democracy, final authority over the meaning of the fundamental law had been claimed by unelected judges, whose determinations could not be corrected by the elected representatives of the people. The arrangement was built to return that final say to the electoral process, and to prevent interpretive deadlock between the branches of government.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: political-constitutionalist scholarship — most prominently Jeremy Waldron's case against judicial review — argues the counter-majoritarian objection on democratic-equality grounds; apex-court judges in override jurisdictions publicly acknowledge the legitimacy question in extra-judicial writing; and the persistence of the competing readings as live institutional positions across comparative practice attests that the founding problem remains contested rather than settled. No attestation comes only from the legislative beneficiaries.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) is authored high-moderate: the override power is a standing exposure rather than a constant levy — minorities' protections and the apex court's determinations hold only at the pleasure of the sitting majority, and each invocation re-prices future invocations downward. Suppression (0.58) is authored as a raw structural property, unscaled by power or scope (the engine scales only extractiveness): it reflects the machinery that keeps courts subordinated — appointment leverage, majority control of the agenda, the public framing of judicial rulings as advisory. Theater (0.30): the advisory layer is functional — judicial rulings shape debate and are usually complied with — but a rising share of constitutional activity is pronouncement known in advance to be overridable. Accessibility_collapse (0.45): alternatives remain partly available (amendment, electoral turnover, international bodies, convention), so understanding the arrangement does not close the option set. Resistance (0.55): override invocations reliably trigger backlash; rights coalitions, bar associations, and scholarly opinion impose a standing political cost, which is why measured extraction oscillates rather than climbing smoothly. Measurements run on one shared time grid (every 5 years, 9 points) with all three tracked metrics authored at every point. The series show a ratchet with episodic relief: high-visibility invocations near T10 and T25 produce backlash dips at T15 and T30, and each cycle re-baselines higher — the oscillation itself partly functions as legitimation (restrained phases re-certify the standing override power), not merely as noise. End-state values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the parliamentary_majorities seat and the majoritarian_electorate seat, the arrangement is experienced as democratic self-government: final say rests where electoral accountability lives, and the override is a rarely needed guarantee of that accountability. From the minority_groups seat, the same structure is experienced as unprotected exposure: a judicial win can be erased by the very majority it binds, with no further institutional recourse. The apex_judiciary seat sits between: institutional power and a protected docket, but locked into a role whose final determinations are revocable — it experiences the arrangement as subordination it cannot exit without dissolving the function itself. Note the same-level differentiation: legislature and courts hold the same nominal constitutional rank (both institutional), yet their exits differ structurally — the majority can restructure the settlement by ordinary act; the court cannot resign from interpreting the constitution. Minority coalition potential is real but limited: the groups exposed to override are dispersed across issue domains (religious, linguistic, ideological), so their shared structural position rarely converts into combined electoral weight.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: parliamentary_majorities (agenda-setter; collects final interpretive authority; mobile exit — it can restructure or relinquish the settlement) derives d near the beneficiary end; majoritarian_electorate (beneficiary, mobile exit) derives low d with mild upward pressure from its secondary exposure as indirect bearer of override risk. Victim declarations: minority_groups (payer, powerless, constrained exit) derives d near the full-target end — trapped targets sit at the extreme; apex_judiciary (payer, institutional, identity_locked) derives high d — the identity lock keeps it near the target end despite institutional power, because its professional and institutional identity is constituted through the very function the settlement caps. minority_rights_advocates (excluded, organized, constrained) derive moderate-high d: they absorb the cost of overridden victories without holding a formal seat. constitutional_scholars (observer, analytical) are directionally neutral. No directionality overrides are authored: the derivation from declared roles, exit options, and power atoms reproduces these positions, and the one candidate correction (the court's retained advisory benefit) is carried by its secondary beneficiary role rather than by an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the counter-majoritarian difficulty — is still live, so no mandatrophy is declared. The tangled_rope classification does double work here: it prevents a judicial-supremacist observer from mislabeling the arrangement as pure extraction (the coordination function — electorally accountable final say — is genuine and its founding problem unresolved), and it prevents a sovereigntist from mislabeling it as pure coordination (the victim declarations — minorities and the subordinated court — are structural, not rhetorical). The lifecycle risk this story watches is piton drift: if the convention against invocation hardens to the point of non-use while the override power remains on the books, the arrangement persists as theatrical maintenance of a safety valve nobody may pull — the theater_ratio series is the early indicator, and the founding_problem_status x disappearance_verdict pair (live x world_rearranges) currently shows no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading does the constitutional text itself mandate — does the text establish parliamentary supremacy with conclusive override (this reading), conclusive judicial invalidation, or retained popular interpretive authority above both branches?',
    'Constitutional amendment, an entrenched interpretive convention, or a sustained shift in institutional practice that settles the locus of final authority; until then the readings remain competing live positions.',
    'This story''s epsilon (0.62), beneficiary/victim structure, and classification are authored for the legislative sovereignty arrangement only. If a sibling reading prevails institutionally, the structure inverts — minorities gain a conclusive judicial backstop and the legislature loses final say — and this constraint''s profile is superseded by the sibling''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level contest: this constraint is one reading of constitutional_text; sibling readings are separate constraints with their own epsilon.').

omega_variable(
    override_taboo_durability,
    'Is the political cost of invoking the override (the convention against use) a durable structural restraint or a contingent norm that erodes with each invocation?',
    'Comparative tracking of override invocation rates and post-invocation electoral consequences across override jurisdictions over successive governments.',
    'If the taboo erodes, invocation normalizes, extraction climbs toward snare-range, and minority protection becomes formally contingent; if the taboo holds or strengthens, the arrangement stabilizes as a coordination mechanism with a rarely pulled backstop.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_taboo_durability, empirical, 'Durability of the political restraint on override usage.').

omega_variable(
    minority_protection_substitutability,
    'Do non-judicial mechanisms — electoral representation, legislative rights committees, human rights commissions, international treaty bodies — adequately substitute for conclusive judicial protection once override is available?',
    'Comparative outcomes for minority rights claimants under override regimes versus entrenched judicial review: incidence of rights violations, remedy rates, and durability of protections.',
    'If substitutes fail, minority_groups are effectively trapped targets and effective extraction exceeds the base measure; if substitutes hold, part of the measured extraction is the price of majoritarian coordination rather than pure loss.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_protection_substitutability, empirical, 'Whether minority protection survives judicial subordination through non-judicial channels.').

omega_variable(
    sovereignty_necessity_claim,
    'Is legislative final say a logical requirement of democratic constitutional order (the claim that entrenchment against the elected legislature is incoherent), or a contestable design choice among available alternatives?',
    'Comparative constitutional performance under entrenched judicial review versus legislative supremacy, plus analysis of whether entrenchment genuinely contradicts democratic theory.',
    'If the necessity claim holds, the arrangement approaches a structural feature of democracy rather than a constructed choice, and resistance to it is misplaced; if it is contestable, the arrangement is a constructed settlement whose beneficiaries owe an account of the costs imposed on minorities and courts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_necessity_claim, conceptual, 'Whether the arrangement is a democratic necessity or a design choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t5, constitutional_text__legislative_sovereignty_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__legislative_sovereignty_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(cons_tr_t15, constitutional_text__legislative_sovereignty_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__legislative_sovereignty_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(cons_tr_t25, constitutional_text__legislative_sovereignty_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__legislative_sovereignty_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement(cons_tr_t35, constitutional_text__legislative_sovereignty_reading, theater_ratio, 35, 0.29).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__legislative_sovereignty_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(cons_be_t5, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(cons_be_t10, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(cons_be_t15, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement(cons_be_t20, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(cons_be_t25, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(cons_be_t30, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(cons_be_t35, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 35, 0.61).
narrative_ontology:measurement(cons_be_t40, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cons_su_t5, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement(cons_su_t10, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement(cons_su_t15, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(cons_su_t20, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(cons_su_t25, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(cons_su_t30, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(cons_su_t35, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 35, 0.57).
narrative_ontology:measurement(cons_su_t40, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'constitutional supremacy' covers structurally distinct claims that decompose into readings of the constitutional_text kernel (epsilon-invariance decomposition). This story authors the legislative sovereignty reading only: epsilon 0.62 is authored for the override-conclusive settlement, not for the judicial-supremacy arrangement (whose epsilon would be authored from its own beneficiary/victim structure) nor the popular-sovereignty arrangement. The readings form a constraint family linked through affects_constraints; pressure runs through institutional practice — each reading's adoption changes the operating environment of the others, since the same courts, legislatures, and electorates populate all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
