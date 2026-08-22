% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__magistrate_reading, []).

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
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Remonstrance Authority — Magistrate Reading (Parlementary Registration as Constitutional Check)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   In the French ancien régime, a royal edict — above all a fiscal edict —
 *   became law only when registered by a sovereign court, and the courts held
 *   the right of remonstrance: a formal refusal with written objections that
 *   forced negotiation, amendment, delay, or an override by lit de justice.
 *   This story instantiates the magistrate reading of that arrangement: the
 *   remonstrance right as a fundamental constitutional mechanism preserving
 *   the kingdom's ancient liberties against arbitrary royal innovation. The
 *   sibling reading (remonstrance_authority__crown_reading) reads the same
 *   arrangement as an illegitimate minoritarian veto protecting particularist
 *   privilege and is authored as a separate constraint with its own epsilon,
 *   linked through network.affects_constraints. Under this reading, the
 *   epsilon referent is the standing registration-and-remonstrance
 *   arrangement itself, assessed by this reading's own lights: the checking
 *   function is genuine, and the same gate systematically carried asymmetric
 *   extraction — fiscal reform edicts were blocked or emasculated, protecting
 *   the tax-exempt magistracy, the privileged orders, and venal office
 *   property, while the crown's finances and the commoner taxpayers absorbed
 *   the cost. The interval 1750–1788 runs from the post-1748 fiscal
 *   settlement through the Jansenist and vingtième conflicts, the Maupeou
 *   suppression and restoration, to the final block of 1787–88 that forced
 *   the convocation of the Estates-General. KEY AGENTS (by structural
 *   relationship): - parlement_magistracy: agenda-setter and primary
 *   beneficiary (institutional / identity_locked) — administers the
 *   registration gate, collects exemption protection and office rents; enters
 *   the victim position episodically when overridden (lit de justice, Maupeou
 *   abolition) - french_crown: primary payer (institutional / constrained) —
 *   fiscally starved by blocked reform; bears override legitimacy costs;
 *   holds the secondary agenda-setting seat as author of the edicts -
 *   privileged_tax_orders: secondary beneficiary (powerful / constrained) —
 *   exemption shield maintained by fiscal remonstrances they pay nothing to
 *   defend - venal_officeholders: beneficiary (organized / constrained) —
 *   office-property values underwritten by the arrangement's defense of
 *   judicial office - commoner_taxpayers: payer (powerless / trapped) — bear
 *   the regressive tax structure the arrangement preserves and the debt its
 *   rigidity forces - royal_finance_administration: operational payer
 *   (powerful / constrained) — drafts the edicts the arrangement blocks;
 *   careers absorb the failure - third_estate_unrepresented: excluded
 *   (moderate / trapped) — outside the contest entirely until 1789; objects
 *   to both parties - constitutional_historians: analytical observer — sees
 *   the full dual structure from outside every party's interest
 *
 * KEY AGENTS:
 *   - parlement_magistracy: agenda-setter and primary beneficiary (institutional / identity_locked) — administers the registration gate, collects exemption protection and office rents; enters the victim position episodically when overridden
 *   - french_crown: primary payer (institutional / constrained) — fiscally starved by blocked reform; bears override legitimacy costs; secondary agenda-setter as author of the edicts
 *   - privileged_tax_orders: secondary beneficiary (powerful / constrained) — exemption shield maintained by fiscal remonstrances they pay nothing to defend
 *   - venal_officeholders: beneficiary (organized / constrained) — office-property values underwritten by the arrangement's defense of judicial office
 *   - commoner_taxpayers: payer (powerless / trapped) — bear the regressive tax structure the arrangement preserves and the debt its rigidity forces
 *   - royal_finance_administration: operational payer (powerful / constrained) — drafts the edicts the arrangement blocks; careers absorb the failure
 *   - third_estate_unrepresented: excluded (moderate / trapped) — outside the contest entirely until 1789; would object to both parties
 *   - constitutional_historians: analytical observer (analytical / analytical) — sees the full dual structure from outside every party's interest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.74).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.78).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Remonstrance Authority — Magistrate Reading (Parlementary Registration as Constitutional Check)").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '3d7fb656-c55a-439e-a144-faffc551996b').
narrative_ontology:cs_kernel_codification('3d7fb656-c55a-439e-a144-faffc551996b', distributed).
narrative_ontology:cs_authority_grounding('3d7fb656-c55a-439e-a144-faffc551996b', lineage).
narrative_ontology:cs_interpretation_layer_present('3d7fb656-c55a-439e-a144-faffc551996b').
narrative_ontology:cs_reading_relation('3d7fb656-c55a-439e-a144-faffc551996b', remonstrance_authority__crown_reading, forecloses).
narrative_ontology:cs_axiom('3d7fb656-c55a-439e-a144-faffc551996b', foundational, registration_constitutionally_required).
narrative_ontology:cs_axiom_status(registration_constitutionally_required, holdable).
narrative_ontology:cs_axiom_grounding('3d7fb656-c55a-439e-a144-faffc551996b', registration_constitutionally_required, conventional).
narrative_ontology:cs_axiom('3d7fb656-c55a-439e-a144-faffc551996b', foundational, fundamental_laws_bind_crown).
narrative_ontology:cs_axiom_status(fundamental_laws_bind_crown, holdable).
narrative_ontology:cs_axiom_grounding('3d7fb656-c55a-439e-a144-faffc551996b', fundamental_laws_bind_crown, deontological).
narrative_ontology:cs_axiom('3d7fb656-c55a-439e-a144-faffc551996b', secondary, hereditary_judicial_office_guarantees_independence).
narrative_ontology:cs_axiom_status(hereditary_judicial_office_guarantees_independence, holdable).
narrative_ontology:cs_axiom_grounding('3d7fb656-c55a-439e-a144-faffc551996b', hereditary_judicial_office_guarantees_independence, instrumental).
narrative_ontology:cs_reference_frame('3d7fb656-c55a-439e-a144-faffc551996b', monarchy_tempered_by_fundamental_laws).
narrative_ontology:cs_drift_state('3d7fb656-c55a-439e-a144-faffc551996b', pre_revolutionary_crisis, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3d7fb656-c55a-439e-a144-faffc551996b', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, parlement_magistracy).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, privileged_tax_orders).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, venal_officeholders).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, french_crown).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, royal_finance_administration).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, commoner_taxpayers).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, parlement_magistracy).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, fundamental_laws_doctrine).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, ancient_constitution_theory).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, judicial_independence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sovereign-court corps of the realm — the Parlement of Paris and the provincial sovereign courts — hereditary venal officeholders drawn largely from the robe nobility. They run the registration process: each royal edict is deliberated in their chambers, and they may accept it, amend it in conference with royal commissioners, delay it, or refuse it with a written remonstrance. What flows to them: their offices are exempt from direct taxation, their office values rest on the jurisdiction and prestige that the remonstrance power sustains, and their fiscal remonstrances shield their own order's exemptions. What flows from them: the enforcement risk — when the crown overrides, magistrates are exiled, their deliberations annulled, and in 1771 their offices abolished outright. Exit from their side would mean accepting royal registration as a formality and taking whatever compensation was offered, as Maupeou proposed in 1771; the corps refused almost to a man, because conceding that the courts were abolible dissolved the premise of their own authority.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlement_magistracy, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, parlement_magistracy, beneficiary).

% The monarchy proposes fiscal and religious edicts and must route them through sovereign-court registration to give them legal force. What flows from it: the edicts, the concessions won in conference, and the legitimacy cost of every override — a lit de justice compels registration but at the price of advertising that the king's will, not the court's judgment, made the law. What flows to it: war debt accumulates while universal taxation is refused, and each capitulation (the 1774 recall of the courts after Maupeou abolished them) teaches the corps that refusal pays. The crown cannot leave the frame: it cannot govern without registered edicts, and it cannot abolish the courts without paralyzing the judiciary and the sale of justice. It draws one offsetting gain it cannot disclaim: registration by sovereign courts stamps royal edicts with judicial legitimacy and absorbs unpopularity that direct royal taxation would draw onto the king personally.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, french_crown, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, french_crown, agenda_setter).

% The hereditary nobility and the clergy, exempt from the taille and from most direct taxation. They collect whenever the courts refuse an edict that would universalize the tax base: their exemption is the concrete stake the fiscal remonstrances defend, and they pay nothing for the defense — the magistracy absorbs the crown's retaliation. Their exposure runs in the other direction: if the arrangement falls and universal taxation passes, their privileged position ends, which is why the orders' assemblies conceded the loss of exemptions in 1789 rather than face the alternative.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, privileged_tax_orders, beneficiary,
    powerful, generational, constrained, national).

% The wider class of officeholders — judicial, financial, municipal — whose offices are heritable property secured by the Paulette's annual payments. The registration-and-remonstrance arrangement underwrites the principle that office is inviolable property: every successful defense of judicial office against royal abolition raises the floor under every venal office's market value. They collect capital security and tax exemption without operating the registration process, and their exit — selling the office — is profitable precisely so long as the arrangement holds.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, venal_officeholders, beneficiary,
    organized, generational, constrained, national).

% Peasants and town commoners who pay the taille, the gabelle, and the vingtième. They pay twice for the arrangement's output: the tax structure the fiscal remonstrances preserve falls on them because it exempts everyone above them, and the fiscal rigidity that blocks reform forces the crown to borrow at worsening rates whose servicing the same taxpayers fund. They hold no seat in the contest — no assembly, no court, no remonstrance of their own — and no exit from the kingdom's tax system; their numbers are overwhelming but their organization, before 1789, is nonexistent.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, commoner_taxpayers, payer,
    powerless, biographical, trapped, national).

% The controllers-general and their bureaus — Machault, Terray, Turgot, Calonne, Loménie de Brienne across this interval — who draft the fiscal reform edicts and watch them refused, delayed, or hollowed out in conference. The arrangement's operational cost lands on their careers: a blocked reform reads as ministerial failure, and several were dismissed or driven out for edicts the courts would not register. Their exit is resignation or dismissal; they cannot route around the registration requirement, and their memoranda are the clearest outside record of what the block cost the treasury.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, royal_finance_administration, payer,
    powerful, biographical, constrained, national).

% The educated Third Estate — lawyers, procureurs, merchants, officials — who would staff and argue for a reformed polity. They stand outside the entire contest: the Estates-General has not met since 1614, and crown and courts each claim to speak for the nation in its absence. They would object to both parties — to arbitrary royal taxation and to the exemptions the remonstrances defend — and their pamphlet literature, growing through the interval, becomes the political force that in 1789 abolishes the arrangement and the privilege it protected in the same stroke.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, third_estate_unrepresented, excluded,
    moderate, biographical, trapped, national).

% Later analysts of the ancien-régime constitution, from Tocqueville's generation through modern fiscal-constitutional history. They reconstruct the arrangement's dual character from remonstrance texts, crown finance records, and the cahiers de doléances, and they hold no stake in any party's vindication — their seat is the only one from which both the checking function and the privilege protection are visible at once.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__magistrate_reading, parlement_magistracy).
narrative_ontology:fixing_cost_class(remonstrance_authority__magistrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the kingdom's single recognized procedure for turning royal will into law: every fiscal and religious edict must be registered by a sovereign court, which reviews it against the fundamental laws, delays it, proposes amendments in conference, or refuses it. This solves a real coordination problem — it gives crown and courts a shared, rule-governed channel for constitutional conflict, gives enacted edicts a legitimacy stamp, and creates a negotiation venue where edicts are amended rather than simply imposed or blocked.
% TRANSFER_FUNCTION: Moves fiscal burden protection and office rents to the privileged orders and the magistracy: by refusing universal taxation edicts, the arrangement preserves noble, clerical, and officeholder exemptions, maintains venal office values, and shifts the cost of fiscal rigidity onto the crown's creditors and the commoner taxpayers' regressive burden. It also moves constitutional authority itself — each successful remonstrance transfers interpretive authority over the kingdom's laws from the crown to the courts.
% ABSENT_VOICES: The Third Estate — unrepresented since 1614 — is the arrangement's structurally absent voice: it would object both to arbitrary royal taxation and to the exemptions the remonstrances defend, but it had no seat, no court, and no remonstrance of its own. Provincial populations spoken for by their parlements without consultation are similarly absent. The contest's apparent unanimity (crown and courts each claiming the constitution's mantle) held only because the taxed nation was never in the room.
% DISAPPEARANCE_RATIONALE: If the registration-and-remonstrance arrangement vanished overnight, the fiscal-political settlement of the kingdom rearranges: the crown registers edicts by its own authority, universal taxation proceeds or fails on royal responsibility alone, the magistracy loses its political leverage and its offices' premium value, and the privileged orders lose the shield that kept the tax base narrow. Both historical suspensions confirm the rearrangement — the 1771 abolition was followed immediately by fiscal edicts the old courts would never have registered, and the 1790 abolition coincided with the collapse of privilege itself.
% FOUNDING_PROBLEM: How a hereditary monarchy that claimed absolute sovereignty could nevertheless be bound — the problem of arbitrary royal innovation against the kingdom's ancient liberties. Registration by sovereign courts was built as the mechanism: royal will becomes law only through the kingdom's courts, giving the fundamental laws a procedural guardian.
% FOUNDING_PROBLEM_CORROBORATION: The crown's own finance records and controllers-general memoranda — outside the beneficiary set — attest that the fiscal emergencies were real and that blocked reform carried costs; the 1789 cahiers de doléances, authored by assemblies outside both crown and courts, attest that arbitrary taxation and privileged exemption were simultaneously live grievances, corroborating that the founding problem was genuine while the arrangement had fused with privilege. No party's self-attestation is relied on: the crown says the problem is the courts, the courts say the problem is the crown, and the outside record says both.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__magistrate_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__magistrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__magistrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74 at interval end) because the arrangement's fiscal operation is decoupled from any public service it renders: remonstrances against universal taxation protected exemptions that carried no service burden, and the block held across four decades of escalating fiscal emergency — through Machault's vingtième, Terray's partial reforms, Turgot's Six Edicts, Calonne's program, and Loménie de Brienne's last attempt. Suppression is high (0.78) because persistence required active coercive machinery on both sides: the crown's lit de justice, exiles of magistrates, and the 1771 abolition of the courts; the corps' registration refusal, collective discipline, and threats of simultaneous resignation. Theater ratio is moderate (0.46): the ancient-constitution rhetoric was substantially performative — remonstrances were drafted for the pamphlet-reading public as much as for the king — but the delay, amendment, and negotiation functions were real work the arrangement performed every year. Accessibility collapse is moderate (0.45): alternatives existed at every node — registration by lit de justice, new courts built from scratch (Maupeou's tribunaux supérieurs), edicts withdrawn and re-framed, provincial assemblies convened — but every alternative carried prohibitive legitimacy or administrative costs, so no alternative cleanly substituted. Resistance is high (0.70): the arrangement met sustained, escalating resistance — edicts forced through in lit de justice, magistrates exiled en masse, the courts abolished in 1771, and the whole structure abolished in 1790. The claimed type, tangled_rope, is stated from the structure and not reconciled to the metrics: the arrangement possesses BOTH a genuine coordination function — the kingdom's only rule-governed channel for constitutional conflict, with review, amendment conferences, and a legitimacy stamp for enacted law — AND asymmetric extraction through the same gate, protecting the tax-exempt magistracy, the privileged orders, and venal office property while the crown's finances and the commoner taxpayers absorb the cost. The magistracy's own self-presentation was rope — a pure constitutional mechanism — and that divergence between their claim and the structure is part of the data this story preserves. The measurement series runs on one shared grid of eight points (1750, 1756, 1763, 1771, 1774, 1776, 1787, 1788) with all three tracked metrics authored at every point; base_properties values are the interval-end state, measured at the accumulation-phase peak of 1787–88. The series oscillates rather than drifting monotonically: accumulation (1750–63) as war debts built and the courts dug in; suppression (1771) when Maupeou abolished the courts and the arrangement's machinery was dismantled outright; restoration and rebound (1774–76) when Louis XVI recalled the parlements and blocking resumed at a higher level; accumulation to peak (1787–88) when the revocation of Calonne's program and the demand for the Estates-General marked the block's maximum extent. The cycle is driven by war-fiscal crisis forcing reform attempts, court refusal, crown escalation, and capitulation; the oscillation is partly an extraction mechanism in its own right — intermittent enforcement taught the corps that refusal pays, and the 1774 restoration was read as proof that the crown always capitulates. Suppression_requirement is tracked because enforcement capacity is the dynamic this story traces: it spiked with the coup, relaxed with restoration, and ratcheted up again as the crown's last overrides failed.
 *
 * PERSPECTIVAL GAP:
 *   From the magistracy's seat the arrangement is the constitution itself: without registration and remonstrance, royal innovation is bounded only by the king's restraint, and the kingdom's liberties are a revocable grace. From the crown's seat the same structure is a minoritarian veto — a few thousand venal officeholders holding the fiscal survival of a war-financing state hostage, with the aggravating fact that the veto's beneficiaries are the veto's operators. From the commoner taxpayers' seat both parties are the problem: the crown-court contest was fought over who would bear privilege's cost, not whether privilege should exist, and the taxpayers financed the fight through regressive taxes and royal borrowing. The engine computes these per-seat classifications from the structural data; this story authors the structure and the claim independently of any seat's self-understanding. Identity-lock dynamics: the magistracy's exit is identity_locked on institutional-ideological fusion — the corps had become its constitutional role as guardian of the fundamental laws, reinforced by office-as-family-patrimony and by collective discipline inside the chambers. The Maupeou episode is the natural experiment: compensation and reappointment in the new courts were offered — material exit existed — and was refused almost universally, because accepting conceded that the old courts were abolible and dissolved the premise of the corps' own authority. Had the identity frame broken earlier, the arrangement would have collapsed rather than migrated; in the event it died with the identity in 1790, when the offices were abolished and no corps remained to defend it.
 *
 * DIRECTIONALITY LOGIC:
 *   The magistracy holds the agenda-setter seat and is declared beneficiary, with an episodic victim position (override episodes: lit de justice, exile, the 1771 abolition) recorded in the victim set — its directionality derives near the beneficiary end but lifted off it by override exposure and identity-locked exit. The privileged orders and the venal officeholders derive low d as pure beneficiaries: they collect exemption protection and office-value security without operating the gate and without absorbing the crown's retaliation. The crown and the finance administration derive high d as declared victims: blocked reform, debt-service costs, override legitimacy costs, and ministerial careers destroyed by edicts they could not register. The crown's d is damped from the full-target end by one offsetting gain the structural data records — sovereign-court registration stamped royal edicts with judicial legitimacy and absorbed unpopularity that direct royal taxation would have drawn onto the king, a blame-screen the crown used deliberately and could not disclaim. Commoner taxpayers derive high d: declared victims, trapped exit, no enforcement leverage, and a coalition (their numbers) that was organizationally unreachable before 1789 — the coalition question is live for this seat and is answered only by the 1789 mobilization. The third estate derives no directionality from the arrangement directly — it is excluded rather than coordinated or borne upon; its stake is that the contest's output, preserved privilege, is carried by it indirectly. Suppression is authored as a raw structural property and is not scaled by power or scope; the engine scales extractiveness from these directionalities and the national spatial scope of every seated actor.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the arrangement as the magistrates claimed it — a rope, a pure constitutional mechanism — would erase the extraction: the same gate that checked arbitrary innovation systematically preserved privilege across four decades of fiscal emergency. Reading it as the crown claimed it — a snare, pure minoritarian veto — would erase the coordination: Jansenist and procedural remonstrances with no fiscal stake at issue, amendment conferences that materially improved edicts, and the rule-governed channel that kept constitutional conflict inside a shared procedure instead of open force. Tangled_rope holds both: one structure, coordination function and asymmetric extraction fused. On the R5 interview, the founding problem — binding a sovereign who claimed to be unbound — was live throughout the interval: the crown's fiscal and religious innovations were real, continuous, and resisted. This is not a zombie arrangement outliving its function; the mandate and the extraction matured together, which is exactly the tangled_rope signature and why mandatrophy_resolved is not declared. The founding problem's status is authored contested rather than dead: the parties disputed whether the operative problem was still royal arbitrariness (the courts' claim) or had become the courts' own veto (the crown's claim), and the outside record — the crown's finance memoranda and the 1789 cahiers de doléances — corroborates both grievances as live. The mismatch consumer reads contested × world_rearranges: no zombie flag fires, correctly — the arrangement was alive, load-bearing, and rearranging the world until the day it was abolished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'Is the remonstrance arrangement a fundamental constitutional mechanism preserving ancient liberties against arbitrary innovation (this magistrate reading), or an illegitimate minoritarian veto protecting particularist privilege (the crown_reading sibling)?',
    'The readings are held by opposed parties with no shared adjudicator; historical resolution came only through a framework both were forced into (the 1789 Estates-General, which abolished privilege and arbitrariness together). Analytically, the disagreement is located in the kernel''s grounding: whether the invoked ''fundamental laws'' are a real binding constitutional order or rhetorical cover for fiscal privilege — testable against remonstrance content in domains with no fiscal stake.',
    'Under the crown reading, the arrangement''s coordination function collapses into extraction and the crown becomes the primary victim; under this reading the arrangement retains genuine checking function and the victim set is fiscal reform, the crown''s finances, and the commoner taxpayers. Epsilon and per-seat classifications shift accordingly; the two readings are separate constraints and must not be averaged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Which reading of the remonstrance_authority kernel the structural evidence supports; the contest''s location is the grounding status of the fundamental laws.').

omega_variable(
    checking_vs_privilege_contingency,
    'When the Parlements remonstrated in the name of the nation''s ancient liberties, was the checking function exercised independently of the magistracy''s fiscal self-interest, or did remonstrance intensity track privilege stakes?',
    'Compare remonstrance behavior across domains: Jansenist and procedural conflicts (no direct fiscal stake) against fiscal edicts (direct exemption stake). If the courts checked with equal intensity where no privilege was at stake, the coordination function is genuine and separable; if checking collapses without a fiscal stake, the coordination story is cover.',
    'If checking is privilege-contingent, this story drifts toward pure extraction with the coordination function as cover; if separable, the tangled_rope structure stands with a genuinely dual gate and the magistracy''s directionality stays near the beneficiary end rather than collapsing toward target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(checking_vs_privilege_contingency, empirical, 'Whether the arrangement''s coordination function is separable from the magistracy''s privilege protection.').

omega_variable(
    magistracy_identity_lock_basis,
    'Was the magistracy''s persistence through suppression (Maupeou exile, abolition) driven by material stake (venal office capital, exemption income) or by identity fusion with the guardian-of-the-laws role?',
    'The Maupeou episode is a near-natural experiment: compensation and reappointment in the new courts were offered — material exit existed. Track acceptance rates and the rhetoric of refusal; if officeholders with depreciated offices refused at the same rate as protected ones, identity dominates; if acceptance tracked compensation generosity, material interest dominates.',
    'If material, the arrangement is closer to a property-rights cartel and suppression is the binding constraint on its persistence; if ideological, the arrangement''s suppression is partly self-enforcing and it can survive enforcement collapse (as it did in the 1774 restoration) — raising the coercive load the structure can absorb before failing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magistracy_identity_lock_basis, empirical, 'Material versus ideological basis of the magistracy''s identity-locked persistence.').

omega_variable(
    fiscal_collapse_counterfactual,
    'Did the remonstrance block on fiscal reform causally produce the crown''s fiscal collapse and the revolutionary fiscal crisis, or would royal finances have failed under any arrangement?',
    'Comparative fiscal-constitutional history: Britain converted the same wars'' debt into fundable debt through a parliamentary tax bargain while France''s equivalent bargain was structurally blocked. Test whether French borrowing spreads and default proximity track remonstrance-block episodes more closely than war expenditure itself.',
    'If the block was decisive, the arrangement''s extraction measured in fiscal outcome is causal and high, and the commoner taxpayers'' victim status is direct; if royal finances would have failed regardless, epsilon is overstated and the arrangement''s extraction is better read as distributional (who pays) rather than aggregate (how much is lost).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_collapse_counterfactual, empirical, 'Causal weight of the remonstrance block in the crown''s fiscal collapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 1750, 1788).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rem_mag_tr_t1750, remonstrance_authority__magistrate_reading, theater_ratio, 1750, 0.3).
narrative_ontology:measurement_basis(rem_mag_tr_t1750, observed).
narrative_ontology:measurement(rem_mag_tr_t1756, remonstrance_authority__magistrate_reading, theater_ratio, 1756, 0.33).
narrative_ontology:measurement_basis(rem_mag_tr_t1756, observed).
narrative_ontology:measurement(rem_mag_tr_t1763, remonstrance_authority__magistrate_reading, theater_ratio, 1763, 0.36).
narrative_ontology:measurement_basis(rem_mag_tr_t1763, observed).
narrative_ontology:measurement(rem_mag_tr_t1771, remonstrance_authority__magistrate_reading, theater_ratio, 1771, 0.55).
narrative_ontology:measurement_basis(rem_mag_tr_t1771, observed).
narrative_ontology:measurement(rem_mag_tr_t1774, remonstrance_authority__magistrate_reading, theater_ratio, 1774, 0.34).
narrative_ontology:measurement_basis(rem_mag_tr_t1774, observed).
narrative_ontology:measurement(rem_mag_tr_t1776, remonstrance_authority__magistrate_reading, theater_ratio, 1776, 0.36).
narrative_ontology:measurement_basis(rem_mag_tr_t1776, observed).
narrative_ontology:measurement(rem_mag_tr_t1787, remonstrance_authority__magistrate_reading, theater_ratio, 1787, 0.44).
narrative_ontology:measurement_basis(rem_mag_tr_t1787, observed).
narrative_ontology:measurement(rem_mag_tr_t1788, remonstrance_authority__magistrate_reading, theater_ratio, 1788, 0.46).
narrative_ontology:measurement_basis(rem_mag_tr_t1788, observed).

% Extraction over time
narrative_ontology:measurement(rem_mag_be_t1750, remonstrance_authority__magistrate_reading, base_extractiveness, 1750, 0.54).
narrative_ontology:measurement_basis(rem_mag_be_t1750, observed).
narrative_ontology:measurement(rem_mag_be_t1756, remonstrance_authority__magistrate_reading, base_extractiveness, 1756, 0.57).
narrative_ontology:measurement_basis(rem_mag_be_t1756, observed).
narrative_ontology:measurement(rem_mag_be_t1763, remonstrance_authority__magistrate_reading, base_extractiveness, 1763, 0.62).
narrative_ontology:measurement_basis(rem_mag_be_t1763, observed).
narrative_ontology:measurement(rem_mag_be_t1771, remonstrance_authority__magistrate_reading, base_extractiveness, 1771, 0.38).
narrative_ontology:measurement_basis(rem_mag_be_t1771, observed).
narrative_ontology:measurement(rem_mag_be_t1774, remonstrance_authority__magistrate_reading, base_extractiveness, 1774, 0.65).
narrative_ontology:measurement_basis(rem_mag_be_t1774, observed).
narrative_ontology:measurement(rem_mag_be_t1776, remonstrance_authority__magistrate_reading, base_extractiveness, 1776, 0.67).
narrative_ontology:measurement_basis(rem_mag_be_t1776, observed).
narrative_ontology:measurement(rem_mag_be_t1787, remonstrance_authority__magistrate_reading, base_extractiveness, 1787, 0.73).
narrative_ontology:measurement_basis(rem_mag_be_t1787, observed).
narrative_ontology:measurement(rem_mag_be_t1788, remonstrance_authority__magistrate_reading, base_extractiveness, 1788, 0.74).
narrative_ontology:measurement_basis(rem_mag_be_t1788, observed).

% Suppression requirement over time
narrative_ontology:measurement(rem_mag_su_t1750, remonstrance_authority__magistrate_reading, suppression_requirement, 1750, 0.55).
narrative_ontology:measurement_basis(rem_mag_su_t1750, observed).
narrative_ontology:measurement(rem_mag_su_t1756, remonstrance_authority__magistrate_reading, suppression_requirement, 1756, 0.58).
narrative_ontology:measurement_basis(rem_mag_su_t1756, observed).
narrative_ontology:measurement(rem_mag_su_t1763, remonstrance_authority__magistrate_reading, suppression_requirement, 1763, 0.62).
narrative_ontology:measurement_basis(rem_mag_su_t1763, observed).
narrative_ontology:measurement(rem_mag_su_t1771, remonstrance_authority__magistrate_reading, suppression_requirement, 1771, 0.85).
narrative_ontology:measurement_basis(rem_mag_su_t1771, observed).
narrative_ontology:measurement(rem_mag_su_t1774, remonstrance_authority__magistrate_reading, suppression_requirement, 1774, 0.6).
narrative_ontology:measurement_basis(rem_mag_su_t1774, observed).
narrative_ontology:measurement(rem_mag_su_t1776, remonstrance_authority__magistrate_reading, suppression_requirement, 1776, 0.63).
narrative_ontology:measurement_basis(rem_mag_su_t1776, observed).
narrative_ontology:measurement(rem_mag_su_t1787, remonstrance_authority__magistrate_reading, suppression_requirement, 1787, 0.72).
narrative_ontology:measurement_basis(rem_mag_su_t1787, observed).
narrative_ontology:measurement(rem_mag_su_t1788, remonstrance_authority__magistrate_reading, suppression_requirement, 1788, 0.78).
narrative_ontology:measurement_basis(rem_mag_su_t1788, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, remonstrance_authority__crown_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the remonstrance right' decomposes into two structurally distinct claims per the epsilon-invariance principle. This file authors the magistrate_reading — remonstrance as fundamental constitutional check (tangled_rope: genuine coordination function plus asymmetric extraction through the same gate; epsilon 0.74 with the referent being the standing registration-and-remonstrance arrangement as the magistrate reading assesses it). The sibling, remonstrance_authority__crown_reading, authors the crown_reading — remonstrance as illegitimate minoritarian veto (coordination story as cover; the crown as primary victim; epsilon authored from the crown's seat). The readings are not one constraint viewed from two angles: they disagree about the kernel's grounding (real constitutional order versus rhetorical cover), which yields different victim sets and different epsilon. The enforcement history runs through both: the courts cite the lit de justice and Maupeou as proof of royal arbitrariness; the crown cites the fiscal block as proof of the veto's illegitimacy. Both files should carry the reciprocal affects_constraints edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
