% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__maximal_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC Resolution 242 Withdrawal Clause — Maximal Withdrawal Reading (French Definite Article Controls)
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   Security Council Resolution 242 (November 1967), adopted after the June
 *   war, addresses the disposition of territories whose armed forces entered
 *   during the conflict. Its equally authentic English and French texts
 *   diverge grammatically at the decisive point: the English reads
 *   'withdrawal ... from territories occupied in the recent conflict,' while
 *   the French 'des territoires occupés' carries a definite article admitting
 *   no qualifier. This story instantiates the maximal reading, which holds
 *   that the French text controls and that behind it stands the Charter's
 *   Article 2(4) default that territorial conquest confers no title — so the
 *   withdrawal owed is from all the territories. On this reading the clause
 *   binds the occupying state to comprehensive retrocession and hands the
 *   displaced populations and pre-war sovereigns an enforceable legal
 *   position. Base extractiveness is authored over the standing arrangement
 *   the clause contests — the occupation itself — assessed by this reading's
 *   own lights.
 *
 * KEY AGENTS:
 *   - occupying_state — primary payer seat (powerful / constrained): bears the comprehensive retrocession demand; patron-shielded from coercive enforcement
 *   - displaced_territorial_claimants — primary beneficiary seat (organized / trapped): holds the enforceable legal position this reading creates
 *   - front_line_arab_states — secondary beneficiaries (organized / mobile): converted the baseline into bilateral recovery treaties
 *   - security_council_permanent_members — agenda setters (institutional / arbitrage): gate every enforcement moment by veto
 *   - israeli_settler_communities — secondary payers (organized / identity_locked): the physical form of non-withdrawal
 *   - broker_patron_state — dual beneficiary/agenda-setter (institutional / mobile): collects mediation leverage, leans toward qualified scope
 *   - international_court_of_justice — analytical observer (institutional / analytical): supplies the authoritative grammar of the dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.78).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.6).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC Resolution 242 Withdrawal Clause — Maximal Withdrawal Reading (French Definite Article Controls)").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'c1adb604-dc38-4a34-9289-7980fa572e00').
narrative_ontology:cs_kernel_codification('c1adb604-dc38-4a34-9289-7980fa572e00', fixed_text).
narrative_ontology:cs_authority_grounding('c1adb604-dc38-4a34-9289-7980fa572e00', lineage).
narrative_ontology:cs_interpretation_layer_present('c1adb604-dc38-4a34-9289-7980fa572e00').
narrative_ontology:cs_reading_relation('c1adb604-dc38-4a34-9289-7980fa572e00', unsc_242_withdrawal_clause__partial_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('c1adb604-dc38-4a34-9289-7980fa572e00', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('c1adb604-dc38-4a34-9289-7980fa572e00', foundational, french_definite_article_controls_scope).
narrative_ontology:cs_axiom_status(french_definite_article_controls_scope, holdable).
narrative_ontology:cs_axiom_grounding('c1adb604-dc38-4a34-9289-7980fa572e00', french_definite_article_controls_scope, conventional).
narrative_ontology:cs_axiom('c1adb604-dc38-4a34-9289-7980fa572e00', foundational, territorial_conquest_confers_no_title).
narrative_ontology:cs_axiom_status(territorial_conquest_confers_no_title, holdable).
narrative_ontology:cs_axiom_grounding('c1adb604-dc38-4a34-9289-7980fa572e00', territorial_conquest_confers_no_title, deontological).
narrative_ontology:cs_reference_frame('c1adb604-dc38-4a34-9289-7980fa572e00', charter_territorial_integrity_default).
narrative_ontology:cs_drift_state('c1adb604-dc38-4a34-9289-7980fa572e00', contemporary_post_advisory_opinions, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c1adb604-dc38-4a34-9289-7980fa572e00', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, displaced_territorial_claimants).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, front_line_arab_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, israeli_settler_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, broker_patron_state).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, charter_article_2_4_territorial_integrity).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, vienna_convention_equal_authenticity_rule).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the territories its forces entered in June 1967 and has annexed parts of them by domestic legislation. Maintains a civilian settlement enterprise inside the held area under state sponsorship. Faces a standing international demand to retire its forces to the pre-war lines. Its principal patron shields it from coercive council action, but full compliance would fracture its governing coalition, and open repudiation of the Charter framework would cost it the legal vocabulary it relies on elsewhere.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state, payer,
    powerful, generational, constrained, regional).

% The resident and displaced populations of the West Bank, East Jerusalem, and Gaza, together with those displaced in the 1967 fighting. They command no army and held no seat among the drafters, but under this reading they hold the enforceable legal position: each year of continued presence strengthens their claim to retrocession. They cannot leave the dispute — their claim travels with them — and they depend on the council, the courts, and third states to convert the legal position into actual return.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, displaced_territorial_claimants, beneficiary,
    organized, generational, trapped, regional).

% Egypt, Syria, and Jordan as the pre-war sovereigns of portions of the held territory. Each has demonstrated the baseline's negotiability: Egypt recovered the Sinai through the 1979 treaty and Jordan normalized through the 1994 treaty, converting the legal position into bilateral agreement once they accepted the framework. Syria's portion remains unrecovered, keeping it inside the dispute.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, front_line_arab_states, beneficiary,
    organized, biographical, mobile, regional).

% Adopted the resolution and have re-affirmed it repeatedly since. Each holds a veto, so each can select which enforcement moments occur; collectively they keep the norm alive while individually modulating its application. Several have sponsored negotiation frameworks that lean toward qualified withdrawal, placing their agenda-setting thumb on the scope question itself.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, security_council_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Has been asked to construe the clause and the legality of practices inside the held territory, most prominently in the 2004 wall advisory opinion and the 2024 advisory opinion on occupation policies. Its constructions feed the interpretive contest over the clause's scope. It can compel no one, but it supplies the authoritative grammar in which the dispute is argued.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_court_of_justice, observer,
    institutional, generational, analytical, global).

% Several hundred thousand civilians relocated into the held territory under state sponsorship; their presence is the physical form of non-withdrawal. Removal has happened before — Sinai in 1982, Gaza in 2005 — and proved socially traumatic but executable. For the ideological core, departure is not expensive but unthinkable, which distinguishes them from ordinarily relocatable populations.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, israeli_settler_communities, payer,
    organized, civilizational, identity_locked, regional).

% The occupier's principal outside patron and the broker of essentially every negotiation round since 1967. Collects diplomatic influence from its position as indispensable mediator, and its 'land for peace' formulations lean toward a qualified reading of the clause. It can step out of the broker role only at real cost to its regional position, so it stays seated and keeps collecting.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, broker_patron_state, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__maximal_withdrawal_reading, broker_patron_state, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__maximal_withdrawal_reading, displaced_territorial_claimants).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__maximal_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts territorial-conquest disputes from bilateral power contests into rule-governed retrocession claims under a shared Charter baseline: one agreed reference point that occupiers, claimants, mediators, and courts can all cite, replacing case-by-case bargaining over accomplished facts.
% TRANSFER_FUNCTION: Moves territorial control — and the water, land, jurisdiction, and movement freedom attached to it — from the occupying state back toward the pre-war sovereigns and resident populations; secondarily moves diplomatic leverage toward whichever seat holds the enforceable reading.
% ABSENT_VOICES: The occupied population itself had no seat at the drafting table in 1967 — the territories were then held by Egypt and Jordan, and the displaced residents were represented only indirectly; settler communities, whose removal the maximal reading entails, were likewise absent and enter only as objects of implementation. Both would speak to scope and to transition mechanics if seated.
% DISAPPEARANCE_RATIONALE: If the maximal withdrawal norm vanished overnight, the dispute loses its legal baseline: conquest-fait-accompli logic returns, the claimants' enforceable position evaporates, the settlement enterprise acquires retrospective legitimacy, and the Charter's broader rule that conquest confers no title takes a visible wound that every revisionist power would price immediately.
% FOUNDING_PROBLEM: The June 1967 war produced a fait accompli — armed forces deep inside neighboring territory — and the council needed a common basis tying any peace to undoing the conquest, stating the inadmissibility of acquiring territory by war alongside withdrawal and secure boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the International Court of Justice's 2004 and 2024 advisory opinions treat the withdrawal obligation as continuing law; successive Secretaries-General report the occupation as ongoing; neutral member states re-affirm the clause annually. The occupier itself accepts Resolution 242 as the negotiation's basis while disputing this reading's scope — attesting the founding problem's liveness even from the paying seat.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.78) is authored over the standing arrangement this reading contests — the occupation — as the reading itself assesses it: a comprehensive Article 2(4) violation with settlement transfer, resource appropriation, and administrative control accumulating for five decades; the series rises monotonically as the settlement enterprise entrenches. Suppression (0.6) encodes the reading's categorical structure: it grants the occupier no partial-compliance credit and forecloses the retention alternative within its own premises, while resting on Chapter VI machinery that compels no one physically. Theater (0.52) crosses the Goodhart line late in the interval — annual re-affirmation increasingly substitutes for implementation — though the clause still does real work as treaty baseline and judicial grammar. Accessibility collapse (0.62) is high-within-premises but incomplete in discourse: grant the Vienna Convention's equal-authenticity rule and the retention alternative collapses, yet the interpretive-authority contest keeps the rival reading institutionally alive. Resistance (0.82) is the interval's dominant fact: annexation legislation, manifold settlement growth, and a patron veto shield. All three series run on one shared grid (t = 0, 10, 20, 30, 40, 50, 58, mapping 1967-2025). Suppression_requirement oscillates rather than trends: enforcement mobilization peaked in the 1970s-80s consensus era, relaxed during the 1990s negotiation round (the t=30 dip), then hardened as veto protection matured — the oscillation tracks negotiation rounds and functions partly as intermittent reinforcement, keeping claimants engaged in processes that defer the constraint's payoff. Base properties are measured at interval end (t=58), the post-revival phase following the 2016 council resolution and the 2024 advisory opinion.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the occupying state's position the clause is a categorical demand with no partial exit and a fifty-eight-year accumulation of sunk settlement costs — from that seat the structure reads as enforced stripping of holdings. From the claimants' seat the same text is an entitlement machine that appreciates annually. The permanent members sit at arbitrage distance, experiencing the clause as a manageable instrument they both maintain and modulate. The broker seat monetizes the gap between the other seats. The engine derives these divergences from the structural data; the rope claim is authored from the analytical seat and does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Displaced territorial claimants and the front-line states are declared beneficiaries: the clause subsidizes their position at near-zero cost to themselves, so their derived directionality sits near the beneficiary pole — the front-line states somewhat less so, since treaty recovery already moved them partway out of the constraint's reach. The occupying state and the settler communities are declared payers: the entire content of the constraint is a flow away from them, and the settlers' identity-locked exit pins them at the full-target end regardless of mobility elsewhere. The permanent members' veto arbitrage blurs their derivation — they administer the constraint while selectively shielding a party from it — which is why their seat is best read through the agenda-setter role rather than any single d value. Scope is regional for the dispute's seats and global for the administering ones, so verification difficulty amplifies effective extraction modestly at the administering tier.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — the occupation persists and the withdrawal remains unexecuted in its core territories — so no mandatrophy declaration is authored, and the status-by-verdict pair (live x world_rearranges) raises no zombie flag. The classification discipline matters in both directions: the constraint is not a snare despite meeting a concentrated payer with maximal resistance, because nothing about the payment is concealed — the occupier knows precisely what the norm demands and why, and the demanded flow is public restitution under a peremptory norm rather than rent collected behind a coordination cover story. Nor is it a piton: its function is actively consumed as the negotiation baseline, the judicial grammar, and the treaty anchor (Sinai, Jordan), so theatrical re-affirmation coexists with real use. The rope claim rests on that active consumption plus the fact that exits were never suppressed — Egypt and Jordan walked out through the front door, by treaty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This file instantiates only the maximal_withdrawal_reading of kernel unsc_242_withdrawal_clause; which reading should supply the operative constraint — this one (French definite article plus the Charter''s territorial-integrity default), the partial_withdrawal_reading (drafters'' intent plus the secure-boundaries qualification), or whatever outcome the interpretive_authority_structure contest selects?',
    'Authoritative determination by the International Court of Justice in a binding proceeding, or consolidated Security Council practice adopting one textual construction; until then the three files stand as rival constraints over one clause.',
    'Under this reading the occupier owes full retrocession and the dispossessed hold an enforceable position; under the partial reading a negotiated-adjustment space opens and the paying set shrinks to whatever retention exceeds the secure-boundaries allowance; under an authority-structure resolution the substantive scope follows whichever interpreter prevails.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame omega: one clause, three rival constraint instantiations; this story is the maximal-scope member.').

omega_variable(
    french_text_control_question,
    'Does the French authentic text (''des territoires occupés'', definite article) control the withdrawal scope under the Vienna Convention''s equal-authenticity rule, or can drafting history and the English indefinite phrasing sustain a qualified scope?',
    'Doctrinal analysis applying VCLT Articles 31-33 to the trilingual drafting record, supplemented by subsequent practice of the parties and of the Council.',
    'If the definite article controls, the constraint is comprehensive and categorical as authored here; if drafting intent prevails, scope becomes discretionary and this story''s suppression and accessibility_collapse values drop sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(french_text_control_question, empirical, 'Whether treaty-law methodology compels the definite-article reading.').

omega_variable(
    chapter_vi_binding_force,
    'Is a Chapter VI resolution''s withdrawal provision legally mandatory on the occupier, or recommendatory pending the Council''s own determination?',
    'Judicial treatment of Chapter VI instruments, Council practice characterizing its own resolutions, and consolidation of subsequent-agreement doctrine.',
    'If recommendatory-only prevails, the constraint''s coercive backing falls away and its classification drifts toward hortatory scaffolding; if mandatory force is accepted via the Article 2(4) jus cogens grounding, the authored suppression profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chapter_vi_binding_force, empirical, 'Whether the clause binds as law or operates as recommendation.').

omega_variable(
    settler_identity_lock_depth,
    'How much of the settler communities'' refusal weight is internalized identity fusion (ideological and religious commitment to remaining) versus structural subsidy and state protection?',
    'Post-evacuation trajectories of comparable removals (Sinai 1982, Gaza 2005): if opposition collapsed quickly once state support withdrew, the lock is structural; if a committed core persisted, the lock is internalized.',
    'If internalized, the occupier''s compliance cost is higher than material accounting suggests and the identity_locked exit attribution hardens; if structural, compensation-plus-withdrawal packages could dissolve the paying bloc comparatively cheaply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_identity_lock_depth, empirical, 'Structural versus internalized component of settler-community resistance.').

omega_variable(
    enforcement_capacity_trajectory,
    'Is the recent hardening of great-power veto protection around the occupier a durable ratchet or an episodic phase that future council compositions will relax?',
    'Track veto frequency and draft-resolution attrition across successive council terms; compare enforcement throughput in the 1970s-80s consensus era against the current period.',
    'A durable ratchet pushes suppression_requirement upward and dates a possible drift toward a hybrid coordination-extraction profile; an episodic phase keeps the oscillation centered and the rope claim stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_capacity_trajectory, empirical, 'Durability of the veto-shield enforcement environment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0, 58).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(unsc_tr_t10, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(unsc_tr_t20, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(unsc_tr_t30, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(unsc_tr_t40, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(unsc_tr_t50, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 50, 0.49).
narrative_ontology:measurement(unsc_tr_t58, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 58, 0.52).

% Extraction over time
narrative_ontology:measurement(unsc_be_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(unsc_be_t10, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(unsc_be_t20, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(unsc_be_t30, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(unsc_be_t40, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 40, 0.73).
narrative_ontology:measurement(unsc_be_t50, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 50, 0.76).
narrative_ontology:measurement(unsc_be_t58, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 58, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(unsc_su_t10, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(unsc_su_t20, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(unsc_su_t30, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(unsc_su_t40, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(unsc_su_t50, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 50, 0.57).
narrative_ontology:measurement(unsc_su_t58, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 58, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% Constraint family: one clause, three structurally distinct stories. The two scope readings (this file and the partial reading) carry different epsilon over the same referent — the occupation — because they assess its legality differently; the authority-structure story carries the contest over who decides. Upstream/downstream: the authority-structure story conditions which scope reading prevails, and each scope reading's fortunes feed back into the authority contest. Linked via affects_constraints across all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
