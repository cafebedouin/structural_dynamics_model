% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute Sovereignty: Categorical Non-Interference Norm (Westphalian Kernel, Absolute Reading)
 *   domain: international law / political philosophy / global governance
 *
 * SUMMARY:
 *   The categorical non-interference norm: every state holds unconditional
 *   authority over its domestic affairs, and no external act of coercion or
 *   judgment is legitimate against it, whatever its domestic conduct. The
 *   rule is codified in UN Charter Article 2(7) and the customary law of
 *   non-intervention, administered by the community of states through
 *   recognition, veto, and diplomatic practice. Its coordination dividend is
 *   real — it lets deeply divergent polities coexist and gives weak states
 *   their main legal shield against predation — but its cost incidence has
 *   migrated across the Charter era: the populations it most binds are those
 *   whose own governments repress them, since the rule removes every external
 *   recourse channel and leaves the repressor's veto-backed shield intact.
 *   This file is ONE READING of the westphalian_sovereignty kernel (the
 *   absolute reading); the conditional and graduated readings are separate
 *   constraints with their own victim sets and epsilon values, linked through
 *   network.affects_constraints. The epsilon referent is the standing
 *   absolute-sovereignty arrangement itself, assessed by this reading's own
 *   lights: even the reading's strongest instrumental defense concedes the
 *   shield's cost to trapped populations and argues it is the price of
 *   coexistence, so the honest reading-indexed value is not negligible. Claim
 *   and metrics are authored independently: the claimed type is what I
 *   believe structurally true; the metrics are what I believe descriptively
 *   true of the rule's actual operation. KEY AGENTS (by structural
 *   relationship): - authoritarian_regimes: Primary beneficiary
 *   (institutional/identity_locked) — the shield's marginal value
 *   concentrates here as regime insurance - small_vulnerable_states:
 *   Secondary beneficiary (moderate/constrained) — collects the rule's
 *   genuine coordination dividend - great_power_governments: Agenda setter
 *   and beneficiary (institutional/arbitrage) — administers the rule and
 *   self-exempts from it - democratic_state_governments: Ambivalent
 *   beneficiary (institutional/constrained) — shielded by the rule, chafing
 *   at its exceptions bar - populations_under_repressive_rule: Primary target
 *   (powerless/trapped) — bears the shield's cost as lost external recourse -
 *   persecuted_minorities: Primary target (powerless/trapped) — protection
 *   conditional on the persecutor's consent - domestic_dissident_movements:
 *   Target (powerless/trapped) — struggle legally confined to the ruler's
 *   arena - human_rights_organizations: Excluded voice
 *   (organized/constrained) — documents and protests, holds no decision seat
 *   - stateless_nations: Excluded voice (powerless/trapped) — outside the
 *   statehood allocation entirely - international_legal_institutions:
 *   Analytical observer (institutional/analytical) — narrows the reading case
 *   by case - international_law_scholars: Analytical observer
 *   (analytical/analytical) — documents the doctrine-practice gap
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.6).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.5).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.6).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute Sovereignty: Categorical Non-Interference Norm (Westphalian Kernel, Absolute Reading)").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international law / political philosophy / global governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, '2612bf23-2859-47db-a747-1b13d912c707').
narrative_ontology:cs_kernel_codification('2612bf23-2859-47db-a747-1b13d912c707', formalized).
narrative_ontology:cs_authority_grounding('2612bf23-2859-47db-a747-1b13d912c707', practice).
narrative_ontology:cs_interpretation_layer_present('2612bf23-2859-47db-a747-1b13d912c707').
narrative_ontology:cs_reading_relation('2612bf23-2859-47db-a747-1b13d912c707', westphalian_sovereignty__conditional_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('2612bf23-2859-47db-a747-1b13d912c707', westphalian_sovereignty__graduated_sovereignty, forecloses).
narrative_ontology:cs_axiom('2612bf23-2859-47db-a747-1b13d912c707', foundational, external_interference_categorically_illegitimate).
narrative_ontology:cs_axiom_status(external_interference_categorically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('2612bf23-2859-47db-a747-1b13d912c707', external_interference_categorically_illegitimate, conventional).
narrative_ontology:cs_axiom('2612bf23-2859-47db-a747-1b13d912c707', foundational, state_consent_bounds_international_obligation).
narrative_ontology:cs_axiom_status(state_consent_bounds_international_obligation, holdable).
narrative_ontology:cs_axiom_grounding('2612bf23-2859-47db-a747-1b13d912c707', state_consent_bounds_international_obligation, conventional).
narrative_ontology:cs_reference_frame('2612bf23-2859-47db-a747-1b13d912c707', unconditional_domestic_jurisdiction).
narrative_ontology:cs_drift_state('2612bf23-2859-47db-a747-1b13d912c707', contemporary_human_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2612bf23-2859-47db-a747-1b13d912c707', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, small_vulnerable_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, great_power_governments).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, democratic_state_governments).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, populations_under_repressive_rule).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, persecuted_minorities).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_dissident_movements).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, nonintervention_norm_charter_article_2_7).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, formal_equality_of_states).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, state_pluralism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governments that systematically repress domestic opposition, imprison dissidents, and persecute minorities. The non-interference rule is their primary international asset: it converts every external criticism of their domestic conduct into a breach by the critic, blocks rescue and accountability channels for their victims, and lets them trade and conduct diplomacy while insulating the internal machinery of control. Leaving the arrangement would mean accepting external oversight of exactly the conduct the rule shields — for these governments, exit is indistinguishable from surrendering power.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes, beneficiary,
    institutional, generational, identity_locked, national).

% States with small economies, populations, or militaries that could not resist predation by stronger neighbors in an intervention-permissive world. The rule is their main protection: it prices invasion and coercion as breaches of a shared standard and gives them standing in collective bodies disproportionate to their power. They pay for it by accepting the same shield for every other government, including repressive ones whose conduct they may deplore; seeking patron protection instead would trade one dependency for another.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, small_vulnerable_states, beneficiary,
    moderate, generational, constrained, national).

% The permanent members of the Security Council and other major military powers. They administer the rule through veto power over any enforcement that would bind them or their clients, through recognition decisions, and through the diplomatic machinery that polices breaches. They invoke it defensively to shield their own conduct and their clients', and they breach it when interests warrant, accepting condemnation but not constraint. They benefit twice: from the shield and from the discretion to override it.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, great_power_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, great_power_governments, beneficiary).

% Governments of constitutionally constrained states that invoke non-interference defensively while periodically seeking exceptions — humanitarian intervention, sanctions, conditionality — for cases they judge intolerable. They benefit from the shield, since no external power may remake their institutions, but they bear an opportunity cost: the same rule blocks the interventions they sometimes want to make. Their position shifts with each crisis between defending the rule and eroding it.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, democratic_state_governments, beneficiary,
    institutional, generational, constrained, global).

% The civilian populations of states that torture, disappear, and starve them. The rule removes their last external recourse: no outside force may be sent, no outside court may reach their rulers, no outside coalition may arm their defense, whatever their rulers do. Their options are flight, endurance, or internal revolt — each priced by the regime the rule insulates. In every international forum they are spoken for by the very government that represses them.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, populations_under_repressive_rule, payer,
    powerless, generational, trapped, national).

% Ethnic, religious, and linguistic groups targeted for elimination or expulsion within their own states. The rule guarantees that the response to their persecution is limited to whatever their persecutor's government consents to: peacekeeping by invitation, humanitarian access negotiated with the besiegers, atrocity prevention conditional on Security Council unanimity. Rwanda and Syria are the recurring demonstrations of what this protection is worth when the persecutor holds or is shielded by a veto.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, persecuted_minorities, payer,
    powerless, generational, trapped, national).

% Opposition movements, independent journalists, and civil society organizers inside repressive states. The rule assures them that no external actor will come to their aid: their struggle is legally confined to the arena their rulers control. External support they do receive — sanctions, naming-and-shaming, asylum — is calibrated to avoid breaching the rule, which caps it well below the level that would threaten the regimes they face.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, domestic_dissident_movements, payer,
    powerless, biographical, trapped, national).

% Amnesty International, Human Rights Watch, and the wider advocacy ecosystem. They document the conduct the rule insulates and press for override doctrines — responsibility to protect, universal jurisdiction, conditional aid — but hold no decision seat in the interstate bodies where the rule is made and applied. Their access is consultative; their objections are recorded and then outvoted or vetoed by the governments the objections concern.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, human_rights_organizations, excluded,
    organized, biographical, constrained, global).

% Peoples without a state of their own — Kurds, Rohingya, Uyghurs, and others. The rule allocates protection by statehood: they are represented internationally by the states that rule, displace, or deny them, and their own claims to collective self-determination have no forum the rule recognizes. They bear the arrangement's costs without even the formal voice that statehood confers.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, stateless_nations, excluded,
    powerless, generational, trapped, global).

% The International Court of Justice, UN treaty bodies, and the International Law Commission. They adjudicate the rule's boundary — what counts as impermissible intervention, where domestic jurisdiction ends — and their jurisprudence has narrowed the absolute reading case by case through human rights obligations and peremptory norms, without ever formally displacing it. They hold analytical standing but no enforcement power independent of the states they would constrain.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_legal_institutions, observer,
    institutional, generational, analytical, global).

% Academic specialists in international law and relations who track the gap between the rule as written and the rule as operated. Their analyses — of the Westphalian settlement's actual content, of the Charter's drafting history, of intervention outcomes — supply the evidentiary base on which the sibling readings argue, and they document the selective enforcement that the formal doctrine conceals.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__absolute_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coexistence problem for political communities with incompatible domestic orders: one shared rule (do not reach into each other's internal jurisdiction) lets monarchies and republics, theocracies and secular states, market and command economies trade, conclude treaties, and conduct diplomacy without adjudicating each other's legitimacy, and gives small states a legal standard that prices predation by the strong.
% TRANSFER_FUNCTION: Moves impunity and decision authority from the international level to state executives: the standing of outside actors to judge, condition, or intervene in domestic conduct is removed and accrues to the governments being judged; the corresponding cost — lost external protection — is borne by the populations those governments rule. Secondarily it moves status: formal equality among states regardless of domestic conduct.
% ABSENT_VOICES: The governed — especially populations under repressive rule, persecuted minorities, and stateless peoples — have no seat in the interstate fora where the norm is made and applied; states speak for them, and where the state is the repressor the victim's only formal representative is the party harming them. Human rights organizations hold consultative but non-decisional positions, and their objections are regularly outvoted or vetoed by the governments the objections concern.
% DISAPPEARANCE_RATIONALE: If the categorical non-interference rule vanished overnight, every border and regime would become contestable: intervention coalitions would form around each crisis, weak states would scramble for patrons or deterrents, treaty diplomacy would lose its baseline presumption, and the Charter system's core bargain would collapse into explicit spheres-of-influence bargaining. Whether the rearrangement would net-benefit oppressed populations is precisely what the sibling readings dispute — but rearrange it would.
% FOUNDING_PROBLEM: The confessional wars of the sixteenth and seventeenth centuries: mutually universalist claims (each power obligated to impose the true religion) made coexistence impossible, and the Westphalian settlement — later the UN Charter — needed a rule that let differently-ordered polities coexist without permanent war. Decolonization later added a second formulation: fragile new states needed a shield against recolonization and great-power predation.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the 1648 settlement and the UN Charter drafting record corroborates the founding problem as stated (coexistence of deeply divided polities after confessional and world war). On current status the corroboration splits by seat, and no source outside the beneficiary set attests the original formulation as still operative: human rights organizations, international legal scholars, and victim testimony from Rwanda and Syria attest that the operative threat to civilian populations now originates inside states, which the shield then insulates; small-state diplomats and most foreign ministries — beneficiaries — attest the coexistence problem remains live. The status is therefore contested, with the strongest corroboration from outside the benefiting parties on the transformed-problem side.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.60: the rule decouples domestic conduct from external consequence for every government while the cost lands almost entirely on populations with no exit — the transfer runs from the governed to their rulers, and it has grown as the human rights regime raised the very standard the rule now shields governments from. Suppression 0.50 is a raw structural property, unscaled by power or scope (only extractiveness is scaled in the engine's computation): the enforcement machinery — Security Council veto, non-recognition, diplomatic protest, standing rejection of override doctrines — actively suppresses external recourse channels, but it does not suppress every exit; flight remains open and some naming-and-shaming proceeds. Theater 0.48: formal equality of states and ritual Charter invocation do real diplomatic work, but close to half the rule's observable activity is performative defense of great-power discretion — invoked against the weak, breached by the strong — rather than functional coordination. Accessibility collapse 0.45: the sibling readings are not collapsed — the conditional reading is institutionalized (2005 World Summit responsibility-to-protection language) and the graduated reading drives failed-state and sanctions practice — so alternatives remain live once the rule is understood. Resistance 0.55: sustained doctrinal resistance from the human rights regime, the ICC, and intervention coalitions has narrowed the rule case by case without displacing its core, which the veto powers protect. The measurement series runs on one shared grid (1945-2025, decadal) with all three metrics authored at every point; the suppression series is authored because the story genuinely tracks enforcement-capacity change — the non-intervention machinery hardened through decolonization and the Cold War, then eroded under post-1990 intervention practice before partially stabilizing. Receipt surface: the shield's protective value demonstrably accrues to the authoritarian-executive seat as regime insurance (great powers accrue a second, discretionary gain — freedom to breach — but the insurance value lands on the repressor seat); fixing the arrangement — replacing the categorical rule with a conditional one — is prohibitive for every seat with capacity to fix it, since the capable fixers are its principal beneficiaries and bypassing them carries great-power war risk.
 *
 * PERSPECTIVAL GAP:
 *   Three seats inhabit the same rule as three different arrangements. From the agenda-setter seat (the permanent five), the rule is a discretionary instrument: invoked to shield clients and self, breached when interests warrant, experienced as freedom. From the small-state beneficiary seat, the rule is survival: it is the difference between a legal order that prices invasion and one that does not. From the trapped payer seats (populations under repressive rule, persecuted minorities, dissidents), the same rule is a sealed ceiling: it is the difference between a world where rescue is possible and one where the persecutor holds a veto over it. The engine computes these divergent per-seat classifications from the power and exit data; the authored claim does not adjudicate between them. Note on coalition potential: the powerless victim seats cannot readily coalition — their natural coalition vehicle is the interstate system itself, where they hold no seats, and their domestic coalition vehicle is the state that represses them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality. authoritarian_regimes (identity-locked — for a repressive government, exiting the rule means accepting external oversight of exactly the conduct the rule shields, so exit is indistinguishable from surrender) sit nearest the beneficiary end. small_vulnerable_states (constrained — patron alliances trade one dependency for another) collect the rule's genuine coordination dividend. great_power_governments (arbitrage) sit lowest of all: the rule subsidizes them twice, once as shield and once as discretion to breach it. democratic_state_governments (constrained) benefit with an ambivalent discount, since the same rule blocks the interventions they periodically seek. Victim declarations drive high directionality: populations_under_repressive_rule, persecuted_minorities, and domestic_dissident_movements are trapped — no recourse channel, no external forum, no coalition vehicle — and sit near the full-target end. The excluded seats (human_rights_organizations, stateless_nations) do not feed the directionality computation, but they document the consensus provenance: the interstate unanimity behind the rule arises partly because the affected seats are not in the room.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question is whether the founding problem — confessional-era coexistence, later decolonization-era protection of fragile new states — has died while the rule persists on inertia. The honest verdict is contested, and the tangled_rope classification preserves both halves of that answer. A pure extraction reading fails because the coordination function is live: weak states genuinely depend on the shield, and dismantling the rule naively would harm the seat least able to defend itself. A pure coordination reading fails because the cost incidence has migrated: the rule now insulates repressors against their own populations more than it insulates weak states against predators, and the measurement series shows extraction rising monotonically across the Charter era while enforcement contestation rose and then partially receded. The founding problem status is therefore contested rather than dead, mandatrophy is not resolved, and the constraint sits exactly where the sibling readings live: squeezed between a real coordination dividend and a real extraction cost, with the override condition as the fault line.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the absolute_sovereignty reading of the westphalian_sovereignty kernel: how would the sibling readings (conditional_sovereignty, graduated_sovereignty) restructure the beneficiary and victim sets, and where exactly is the disagreement located?',
    'The sibling stories are authored as separate files; compare their victim sets and epsilon values against this file''s. The conditional reading converts part of this reading''s victim set (trapped populations) into protected parties with an override channel; the graduated reading re-sorts the beneficiary/victim split by state capacity and governance legitimacy rather than regime type. The disagreement is located at the override condition: this reading asserts there is none.',
    'If the conditional reading became the operative norm, this constraint''s epsilon falls as trapped populations gain external recourse; if the graduated reading prevailed, the directionality structure re-sorts by capacity and the repressive-regime beneficiary seat loses its categorical shield.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Kernel-reading indexicality: one kernel, three readings, three distinct constraints with distinct victim sets.').

omega_variable(
    coordination_vs_ruler_cartel,
    'Is the non-interference norm primarily a coordination good (protecting weak states from great-power predation and enabling coexistence of divergent polities) or primarily a ruler''s cartel (mutual insurance among state executives against accountability for domestic conduct)?',
    'Incidence analysis: compare how often the shield is invoked to block intervention against repressive regimes versus to protect small states from predation, using event data on interventions, Security Council vetoes, and recognition decisions across the Charter era.',
    'If cartel-dominant, the authored extractiveness understates the transfer and the computed classification drifts toward pure extraction at the payer seats; if coordination-dominant, the tangled_rope reading holds and dismantling the rule would harm the weak-state beneficiary seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_ruler_cartel, empirical, 'Whether the rule''s primary function is coordination among states or insurance for executives.').

omega_variable(
    selective_enforcement_asymmetry,
    'Does the norm operate symmetrically (all states equally shielded and bound) or asymmetrically (enforced by the powerful against the weak, self-exempted by the powerful)?',
    'Compare formal doctrine (Charter text, ICJ jurisprudence) against the operational record: consequences of great-power breaches (Vietnam, Afghanistan, Iraq, Ukraine) versus weak-state breaches; Security Council veto patterns on sovereignty-implicating resolutions.',
    'Asymmetric operation raises effective extraction on weak-state populations, who bear the rule''s costs without its protection, and identifies the agenda-setter seat as self-exempting — a capture signature that pushes computed per-seat classifications toward pure extraction at the payer seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_enforcement_asymmetry, empirical, 'Symmetry of the norm''s application across power levels.').

omega_variable(
    intervention_counterfactual_dispute,
    'For populations under repressive rule, is the shield''s cost (no external recourse) greater or smaller than the expected cost of the alternative (intervention under a conditional regime)?',
    'Comparative case analysis: Rwanda (non-intervention, catastrophic), Kosovo and Libya (intervention, mixed — protection delivered but destabilization and precedent abuse), Syria (shielded, catastrophic). No clean natural experiment exists; triangulate across cases and intervention types.',
    'If intervention''s expected cost to victims exceeds the shield''s cost, part of the measured extraction is actually protection and epsilon falls toward the coordination pole; if the shield''s cost dominates, epsilon rises toward the extraction pole at the victim seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_counterfactual_dispute, empirical, 'The counterfactual baseline dispute that underlies the kernel contest.').

omega_variable(
    governed_consent_counterfactual,
    'Would the constraint''s victims — populations under repressive rule — consent to the shield if given an authentic voice in interstate fora, or does the arrangement''s stability depend on their exclusion?',
    'Survey and representative-body evidence where it exists: opposition platforms, exile governments, diaspora appeals for intervention — noting these are themselves selected samples and no clean resolution is available.',
    'If victims would reject the shield, the arrangement''s legitimacy claim rests entirely on the excluded seats'' silence, strengthening the extraction reading; if victims would accept it as the price of order, part of the extraction is consented coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(governed_consent_counterfactual, preference, 'Whether victim consent to the shield exists or the arrangement depends on victim voicelessness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ws_absolute_sovereignty_tr_t1945, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1945, 0.2).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_tr_t1945, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_tr_t1955, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1955, 0.22).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_tr_t1955, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_tr_t1965, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1965, 0.28).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_tr_t1965, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_tr_t1975, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1975, 0.33).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_tr_t1975, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_tr_t1985, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1985, 0.35).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_tr_t1985, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_tr_t1995, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1995, 0.38).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_tr_t1995, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_tr_t2005, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2005, 0.42).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_tr_t2005, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_tr_t2015, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2015, 0.45).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_tr_t2015, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_tr_t2025, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(ws_absolute_sovereignty_be_t1945, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_be_t1945, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_be_t1955, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1955, 0.42).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_be_t1955, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_be_t1965, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1965, 0.45).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_be_t1965, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_be_t1975, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_be_t1975, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_be_t1985, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_be_t1985, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_be_t1995, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_be_t1995, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_be_t2005, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2005, 0.57).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_be_t2005, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_be_t2015, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2015, 0.59).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_be_t2015, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_be_t2025, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2025, 0.6).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(ws_absolute_sovereignty_su_t1945, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_su_t1945, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_su_t1955, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1955, 0.55).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_su_t1955, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_su_t1965, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_su_t1965, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_su_t1975, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_su_t1975, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_su_t1985, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_su_t1985, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_su_t1995, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_su_t1995, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_su_t2005, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_su_t2005, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_su_t2015, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_su_t2015, observed).
narrative_ontology:measurement(ws_absolute_sovereignty_su_t2025, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2025, 0.5).
narrative_ontology:measurement_basis(ws_absolute_sovereignty_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, graduated_sovereignty).

% DUAL FORMULATION NOTE:
% The colloquial label Westphalian sovereignty covers three structurally distinct claims sharing one kernel but differing on the override condition. This file instantiates the absolute reading (no override; epsilon 0.60; victims are populations under repressive rule). conditional_sovereignty instantiates the R2P-lineage reading (override on systematic atrocity; part of this reading's victim set converts to protected parties). graduated_sovereignty instantiates the capacity/legitimacy-spectrum reading (override varies by state; the beneficiary/victim split re-sorts by capacity rather than regime type). The three stories form one constraint family; the upstream claim (formal codification in the Charter) is cited as authority by the downstream contested claims. Epsilon differs across the family because the override condition changes who can reach whom — per the epsilon-invariance principle each reading is a separate constraint with its own stable epsilon, and this file's referent is the standing absolute-sovereignty arrangement, never the siblings' endorsed alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
