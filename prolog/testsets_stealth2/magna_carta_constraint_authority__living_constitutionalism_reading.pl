% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__living_constitutionalism_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta as Inherited Due Process Binding Successive Rulers (Living Constitutionalism Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the
 *   magna_carta_constraint_authority kernel: the living-constitutionalism
 *   claim that the charter establishes inherited due process and lawful
 *   restraint binding all subsequent rulers through juridical precedent and
 *   evolutionary interpretation. Per the epsilon-invariance discipline, the
 *   sibling readings (feudal_obsolescence_reading,
 *   parliamentary_sovereignty_reading) are separate constraints with their
 *   own stories; nothing about them is averaged into this file. Epsilon's
 *   referent is the standing arrangement under contest as this reading sees
 *   it — the inherited-restraint arrangement that binds rulers — assessed by
 *   this reading's own lights, not by the rivals'. KEY AGENTS (by structural
 *   relationship): - common_law_judiciary: administrator/enforcer
 *   (institutional/identity_locked) — maintains the arrangement through
 *   precedent and gains interpretive authority from doing so; -
 *   royal_prerogative_officeholders: primary target historically
 *   (powerful/trapped) — bore the restraint under duress of arms and civil
 *   war; - modern_executive_authorities: primary target today
 *   (institutional/constrained) — operates inside judicially policed
 *   procedural limits; - charter_protected_subjects: primary beneficiary
 *   (moderate/mobile) — received the guarantee and carried it abroad; -
 *   modern_due_process_claimants: beneficiary (moderate/mobile) — invoke the
 *   inherited guarantee across common-law jurisdictions; -
 *   westminster_parliament: dual-positioned (institutional/constrained) —
 *   legitimated by the continuity narrative yet qualified by it; -
 *   unfree_villein_class: excluded (powerless/trapped) — outside 'no free
 *   man' at the founding and for generations after; -
 *   constitutional_historians: analytical observer — documents which
 *   invocations did legal work and which were ceremony.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.28).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.26).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta as Inherited Due Process Binding Successive Rulers (Living Constitutionalism Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__living_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, '521b0d76-3865-40c1-8e3c-47dfd42c7224').
narrative_ontology:cs_kernel_codification('521b0d76-3865-40c1-8e3c-47dfd42c7224', fixed_text).
narrative_ontology:cs_authority_grounding('521b0d76-3865-40c1-8e3c-47dfd42c7224', lineage).
narrative_ontology:cs_interpretation_layer_present('521b0d76-3865-40c1-8e3c-47dfd42c7224').
narrative_ontology:cs_reading_relation('521b0d76-3865-40c1-8e3c-47dfd42c7224', magna_carta_constraint_authority__feudal_obsolescence_reading, forecloses).
narrative_ontology:cs_reading_relation('521b0d76-3865-40c1-8e3c-47dfd42c7224', magna_carta_constraint_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('521b0d76-3865-40c1-8e3c-47dfd42c7224', foundational, charter_restraint_binds_through_juridical_lineage).
narrative_ontology:cs_axiom_status(charter_restraint_binds_through_juridical_lineage, holdable).
narrative_ontology:cs_axiom_grounding('521b0d76-3865-40c1-8e3c-47dfd42c7224', charter_restraint_binds_through_juridical_lineage, conventional).
narrative_ontology:cs_axiom('521b0d76-3865-40c1-8e3c-47dfd42c7224', foundational, executive_power_subject_to_inherited_due_process).
narrative_ontology:cs_axiom_status(executive_power_subject_to_inherited_due_process, holdable).
narrative_ontology:cs_axiom_grounding('521b0d76-3865-40c1-8e3c-47dfd42c7224', executive_power_subject_to_inherited_due_process, deontological).
narrative_ontology:cs_axiom('521b0d76-3865-40c1-8e3c-47dfd42c7224', secondary, evolutionary_interpretation_renews_charter_meaning).
narrative_ontology:cs_axiom_status(evolutionary_interpretation_renews_charter_meaning, holdable).
narrative_ontology:cs_axiom_grounding('521b0d76-3865-40c1-8e3c-47dfd42c7224', evolutionary_interpretation_renews_charter_meaning, conventional).
narrative_ontology:cs_reference_frame('521b0d76-3865-40c1-8e3c-47dfd42c7224', charter_as_living_restraint_on_rulers).
narrative_ontology:cs_drift_state('521b0d76-3865-40c1-8e3c-47dfd42c7224', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('521b0d76-3865-40c1-8e3c-47dfd42c7224', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, charter_protected_subjects).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, modern_due_process_claimants).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative_officeholders).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, modern_executive_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, westminster_parliament).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, westminster_parliament).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, rule_of_law_supremacy_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, due_process_inheritance_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Monarchs and their councils from John onward held powers of arrest, taxation, and disposition that the charter's chapters place under legal procedure. Each reign begins with an oath to uphold the laws the charter anchors; rulers who tested the limits — John, Henry III, Charles I — met baronial arms, parliamentary opposition, or civil war. The office cannot leave the jurisdiction it governs; its options are compliance, negotiation, or overthrow.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative_officeholders, payer,
    powerful, biographical, trapped, national).

% Government departments and ministers today exercise prerogative and statutory powers inside procedural limits that courts police: reasoned decisions, hearing rights, freedom from arbitrariness. They can contest limits case by case, draft legislation to clarify powers, and reshape the boundaries politically, but they cannot step outside the legal order that reviews them.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, modern_executive_authorities, payer,
    institutional, biographical, constrained, national).

% Free subjects from the barons onward acquired enforceable claims to lawful judgment, fixed rents, and freedom from arbitrary seizure. Over centuries the protected circle widened from tenants-in-chief to commoners to citizens. Protection traveled with them: emigrants carried the due-process inheritance to colonies and new constitutions, so leaving never meant abandoning the guarantee.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, charter_protected_subjects, beneficiary,
    moderate, generational, mobile, national).

% Litigants and citizens across common-law jurisdictions invoke procedural fairness against public power — habeas corpus, notice, hearing, reasoned decision. Their claims are heard in courts whose standing rests on the inherited tradition; they can and do move between jurisdictions offering stronger or weaker versions of the guarantee.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, modern_due_process_claimants, beneficiary,
    moderate, biographical, mobile, continental).

% Judges administer the inherited restraint: they hear claims against the crown, develop precedent, and decide what the old chapters mean now. Their professional formation and standing are inseparable from the continuity narrative — senior judges are, in a real sense, made by the tradition they maintain. Leaving would mean repudiating the craft that constitutes them.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Parliament confirmed the charter repeatedly (1225, 1297) and wove its guarantees into statute; its own authority borrows legitimacy from the ancient-continuity story. At the same time, judicial policing of executive action qualifies what Parliament's statutes can authorize, and from time to time Parliament chafes at limits courts derive from principles older than any sitting legislature.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, westminster_parliament, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, westminster_parliament, payer).

% The majority of the 1215 population — villeins and the unfree — stood outside 'no free man': no access to the courts developing the guarantees, no standing at Runnymede, no path into the protected circle for generations. Their descendants entered the protection only as the category 'free' expanded, long after the founding bargain.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, unfree_villein_class, excluded,
    powerless, generational, trapped, local).

% Scholars trace the charter's text, its reissues, its reception, and its rhetorical afterlives across eight centuries and multiple jurisdictions. They take no side in the contest over binding force but document which invocations did legal work and which were ceremony.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_judiciary).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__living_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes restraint on sovereign power durable across reigns: by anchoring limits in inherited law that courts enforce, rulers and ruled coordinate expectations without renegotiating the basic terms at each succession — a change of monarch stops being an occasion for repricing obedience.
% TRANSFER_FUNCTION: Moves discretion from rulers to subjects: security of person and property passes from executive caprice to legal process. It also moves interpretive authority to the courts, which gain the standing to say what the old promises mean in each new age.
% ABSENT_VOICES: The unfree majority of the thirteenth century had no seat: 'no free man' excluded villeins from both the founding bargain and the courts that elaborated it. In the modern frame, populations governed by exported common law without consent — colonized peoples subject to the tradition's guarantees in rhetoric and its exceptions in practice — would object to a continuity story told on their behalf.
% DISAPPEARANCE_RATIONALE: If the inherited-restraint arrangement vanished overnight, every due-process guarantee tracing to the charter would need independent re-foundation; executive detention, seizure, and taxation powers would expand pending reconstruction; courts would lose the warrant for reviewing prerogative; and the legitimacy settlement linking crown, Parliament, and subject would have to be renegotiated from zero.
% FOUNDING_PROBLEM: King John's arbitrary rule: punitive reliefs and scutage, disseisin of lands without judgment, abuse of wardship and marriage rights, foreign mercenaries levied against his own subjects. The barons demanded that exaction and dispossession proceed only by lawful judgment or settled custom.
% FOUNDING_PROBLEM_CORROBORATION: By this reading's lights the narrow feudal grievances are settled history, but the general problem the charter first answered — unaccountable executive power over person and property — recurs in every age. Constitutional historians outside the beneficiary set document the recurrence of executive-overreach crises (1620s ship money, 1790s Alien and Sedition controversies, twentieth-century emergency powers) as the continuing problem; executive-branch law officers themselves concede the necessity of procedural limits in litigation and advice. No serious voice attests that the narrow feudal grievances of 1215 remain live; the live-problem claim rests on the generalized reading and is corroborated from observer and payer seats, not only from beneficiaries.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).
:- end_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low-to-moderate (0.28 at interval end) because the arrangement's principal cost falls on rulers' discretion, and that cost purchases the legitimacy that keeps compliant regimes durable — even the paying seat nets out ahead over a reign, which is the signature of coordination rather than plunder. Suppression (0.26) is now institutional rather than physical: enforcement runs through courts, not baronial levies; the suppression_requirement series shows the enforcement burden falling steeply from armed compulsion at the sealing, rising briefly through the Stuart resistance era, then declining as compliance internalized after the seventeenth-century settlements. Theater (0.52) is the honest hard number: a majority of contemporary charter activity is ceremonial — anniversaries, rhetorical citation, decorative judicial references — while the operative work is done by descendant instruments the reading absorbs as 'evolution.' Accessibility collapse is low (0.40): understanding this reading does not close off the rival readings; the kernel contest itself proves the alternatives stay open. Resistance (0.60) reflects eight centuries of executive pushback, from John's annulment politics to modern battles over judicial review. The measurement series run on one shared time grid (eight anchor points from the 1215 sealing to the present) so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the judiciary's seat the arrangement is the source of its own authority — administering the inherited restraint is what makes the bench matter, so that seat computes near-pure coordination. From the executive's seat the same arrangement is a binding limit policed by adversaries, experienced as imposed cost. From the subjects' seat it is a protective guarantee with no visible price. Same-level divergence: Westminster Parliament and the modern executive hold comparable institutional power yet sit differently — Parliament's authority is legitimated by the continuity narrative (pulling it toward beneficiary) while the executive bears the daily policing of its discretion (holding it at target). The engine computes these per-seat classifications from the structural data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (charter_protected_subjects, modern_due_process_claimants) sit near the full-beneficiary end: they receive the guarantee and bear no enforcement burden, with mobile exit that historically exported the guarantee rather than escaping it. Targets (royal_prerogative_officeholders, modern_executive_authorities) sit near the full-target end: they surrender discretion and cannot exit the legal order that binds them — trapped for the crown, constrained for modern government. The judiciary sits nearest the beneficiary pole among administrators: it collects interpretive authority from running the arrangement, which is why gain_flow names that seat. Parliament is genuinely dual: legitimated by the narrative (downward pull) yet qualified by it (upward pull), landing mid-scale. No directionality overrides are used: the beneficiary/victim declarations plus exit differentiation already separate the seats, and a power-atom-level override would flatten the judiciary-versus-executive contrast that is the story's analytical point.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem's original form — baronial feudal grievances against a specific king — is dead; feudal tenure is abolished. The living reading survives precisely by refusing to let the mandate die with its form: evolutionary interpretation migrates the function (restraint of executive power) out of the obsolete vehicle (feudal clauses, nearly all repealed) into descendant doctrine. Mandatrophy is therefore NOT resolved on this reading: the mandate still performs, and the rising theater ratio measures the growing ceremonial share around a functioning core, not replacement of function by performance. The classification guards both directions: it prevents mislabeling the arrangement as pure extraction (the paying seat nets positive over a reign — restraint bought dynastic durability), and it prevents mistaking the theatrical surplus for the whole (the operative due-process lineage is real and load-bearing). Identity-lock dynamics concentrate in the judiciary: the fusion is institutional-professional — judges are formed by, promoted within, and authoritative because of the continuity narrative, so exit would mean repudiating the craft that constitutes them. If that identity frame broke, the arrangement would decay toward pure ceremony within generations, and the theater ratio would complete its climb.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the magna_carta_constraint_authority kernel (reading: living_constitutionalism_reading). Which reading of the kernel governs the charter''s binding force — this one, feudal_obsolescence_reading, or parliamentary_sovereignty_reading?',
    'Adjudication by constitutional practice and legal-historical scholarship: which account courts and legislatures actually act on when charter-derived limits collide with executive or parliamentary will.',
    'Under feudal_obsolescence_reading this constraint dissolves — no binding force, no parties, no extraction. Under parliamentary_sovereignty_reading the victim set shifts: Parliament inherits agenda-setting authority and every charter provision becomes revisable, converting inherited restraint into ordinary statute-backed policy. This reading''s rope classification and its beneficiary/victim structure hold only within the living-constitutionalism frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the Magna Carta kernel fixes the constraint''s binding force and victim set.').

omega_variable(
    transmission_channel_location,
    'Where is the kernel disagreement located: in whether the charter binds at all, or in the channel through which binding force travels (an evolving juridical tradition versus revisable statute)?',
    'Separate the two disputes empirically: parties who accept binding force but dispute the channel (this reading versus the parliamentary reading) versus parties who deny binding force outright (feudal obsolescence).',
    'If the live dispute is channel-only, this reading and the parliamentary reading are rivals within a shared acceptance of restraint, and hybrid channels (statutory entrenchment of charter clauses plus common-law development) become available; if the dispute is existence-level, no hybrid is available and the foreclosure structure is total.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_channel_location, conceptual, 'Locating the structural element the sibling readings actually differ on.').

omega_variable(
    operational_vs_symbolic_binding,
    'What share of the charter''s contemporary binding force is operative law applied in justiciable disputes versus ceremonial genealogy invoked for legitimation?',
    'Systematic survey of charter citations in appellate judgments and constitutional arguments, coding each invocation for operative work (dispositive reasoning) versus decorative work (preamble, analogy, anniversary rhetoric).',
    'A high decorative share confirms the theater trajectory and signals drift toward inertial maintenance; a robust operative share supports the coordination reading and the low-extraction profile. The theater_ratio series (0.10 rising to 0.52) presumes the decorative share is growing; this omega tests that presumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_vs_symbolic_binding, empirical, 'Operative versus ceremonial share of the charter''s present-day binding force.').

omega_variable(
    generalization_legitimacy,
    'Does extending the charter''s ''free man'' protections to all subjects and citizens track the document''s own logic, or retrofit modern egalitarian commitments onto a narrow baronial compact?',
    'Textual-historical analysis of chapter 39''s scope at 1215 and 1297, plus reception history: when and through what instruments the protected circle widened, and whether the widening was contested as betrayal or claimed as fulfillment.',
    'If the widening is retrofit, part of the beneficiary set is manufactured by the reading rather than inherited from the arrangement, and the extraction accounting shifts — the arrangement coordinated a narrower community than this reading claims, with correspondingly different directionality for modern claimants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generalization_legitimacy, conceptual, 'Whether the reading''s universalized beneficiary set is inherited or retrofitted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 0, 810).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(magn_tr_t82, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 82, 0.15).
narrative_ontology:measurement(magn_tr_t400, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 400, 0.22).
narrative_ontology:measurement(magn_tr_t413, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 413, 0.2).
narrative_ontology:measurement(magn_tr_t576, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 576, 0.32).
narrative_ontology:measurement(magn_tr_t700, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 700, 0.44).
narrative_ontology:measurement(magn_tr_t783, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 783, 0.48).
narrative_ontology:measurement(magn_tr_t810, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 810, 0.52).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(magn_be_t82, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 82, 0.4).
narrative_ontology:measurement(magn_be_t400, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 400, 0.46).
narrative_ontology:measurement(magn_be_t413, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 413, 0.43).
narrative_ontology:measurement(magn_be_t576, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 576, 0.38).
narrative_ontology:measurement(magn_be_t700, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 700, 0.33).
narrative_ontology:measurement(magn_be_t783, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 783, 0.3).
narrative_ontology:measurement(magn_be_t810, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 810, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(magn_su_t82, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 82, 0.55).
narrative_ontology:measurement(magn_su_t400, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 400, 0.66).
narrative_ontology:measurement(magn_su_t413, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 413, 0.62).
narrative_ontology:measurement(magn_su_t576, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 576, 0.4).
narrative_ontology:measurement(magn_su_t700, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 700, 0.32).
narrative_ontology:measurement(magn_su_t783, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 783, 0.29).
narrative_ontology:measurement(magn_su_t810, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 810, 0.26).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Magna Carta's authority' decomposes into three structurally distinct constraints corresponding to the kernel's three readings. This file authors the living_constitutionalism_reading (inherited restraint binding rulers through evolving precedent; low-to-moderate extraction; victim set = prerogative holders and executive discretion). The feudal_obsolescence_reading authors a constraint with no binding force and effectively no parties; the parliamentary_sovereignty_reading authors a constraint whose authority seat is Parliament and whose provisions are revisable. The epsilon values differ because the arrangements described differ, not because one observable varies: each reading is a separate constraint, linked here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
