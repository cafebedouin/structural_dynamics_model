% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__defensive_spiritual_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Defensive-Spiritual Jihad Discipline (State-Gated Reading)
 *   domain: religious law/political theology/comparative religious law
 *
 * SUMMARY:
 *   This story instantiates the defensive_spiritual_reading of the
 *   jihad_quranic_corpus kernel: armed force is legitimate only in response
 *   to aggression, requires public authority, observes proportionality and
 *   non-combatant immunity, and the believer's primary struggle is interior.
 *   The reading rose to institutional dominance across the twentieth century
 *   — through anti-colonial reframing, the post-caliphate settlement,
 *   official Azharite and Diyanet doctrine, and post-2001 counter-extremism
 *   partnership — and it now functions as the operative discipline on
 *   sanctified violence in most Muslim-majority polities. Its structure has a
 *   genuine coordination face (it solves the private-war problem and protects
 *   non-combatants) and an extraction face (the public-authority gate hands
 *   the administering state a violence monopoly it does not apply to itself,
 *   plus a delegitimation instrument against rival movements). The
 *   claim/metric gap is deliberate: the reading is CLAIMED here as
 *   tangled_rope because both faces are structurally load-bearing, while the
 *   metrics describe the arrangement's actual operation — moderately
 *   extractive, actively enforced, heavily resisted, with alternatives (the
 *   sibling readings) far from collapsed. The kernel decomposes into three
 *   linked stories per the epsilon-invariance principle; this file covers
 *   only this reading.
 *
 * KEY AGENTS:
 *   - muslim_majority_states: dual-positioned administrator-beneficiary (institutional/arbitrage) — enforces the gate it sits above and stretches the threshold for its own campaigns
 *   - official_religious_institutions: agenda-setting interpreter (institutional/identity_locked) — certifies the reading as orthodoxy; their authority is constituted by the reading
 *   - ordinary_believers: coordinated payer-beneficiary (moderate/identity_locked) — surrender unilateral judgment over sanctified force; receive communal peace and a spiritual frame
 *   - unauthorized_militant_movements: primary enforcement target (organized/trapped) — bear proscription and lethal targeting for rejecting the public-authority gate
 *   - noncombatant_civilians: protected beneficiary and residual casualty (powerless/trapped) — covered by immunity where the discipline holds, exposed where states stretch it
 *   - coexisting_religious_minorities: framework-dependent beneficiary (moderate/constrained) — their civic standing tracks this reading's custody
 *   - classical_legalist_scholars: excluded custodian of the rival expansionist framework (moderate/identity_locked) — sidelined from threshold definition
 *   - academic_islamicists: analytical observer (analytical/analytical) — sees the kernel, the readings, and the custody struggle from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.54).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.62).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Defensive-Spiritual Jihad Discipline (State-Gated Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "religious law/political theology/comparative religious law").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__defensive_spiritual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '09503852-8f40-4ba5-96d3-3bf74d66c1b5').
narrative_ontology:cs_kernel_codification('09503852-8f40-4ba5-96d3-3bf74d66c1b5', fixed_text).
narrative_ontology:cs_authority_grounding('09503852-8f40-4ba5-96d3-3bf74d66c1b5', lineage).
narrative_ontology:cs_interpretation_layer_present('09503852-8f40-4ba5-96d3-3bf74d66c1b5').
narrative_ontology:cs_reading_relation('09503852-8f40-4ba5-96d3-3bf74d66c1b5', jihad_quranic_corpus__expansionist_legalist_reading, forecloses).
narrative_ontology:cs_reading_relation('09503852-8f40-4ba5-96d3-3bf74d66c1b5', jihad_quranic_corpus__revolutionary_vanguard_reading, coexists_with).
narrative_ontology:cs_axiom('09503852-8f40-4ba5-96d3-3bf74d66c1b5', foundational, armed_jihad_exclusively_defensive).
narrative_ontology:cs_axiom_status(armed_jihad_exclusively_defensive, holdable).
narrative_ontology:cs_axiom_grounding('09503852-8f40-4ba5-96d3-3bf74d66c1b5', armed_jihad_exclusively_defensive, deontological).
narrative_ontology:cs_axiom('09503852-8f40-4ba5-96d3-3bf74d66c1b5', foundational, interior_struggle_primacy).
narrative_ontology:cs_axiom_status(interior_struggle_primacy, holdable).
narrative_ontology:cs_axiom_grounding('09503852-8f40-4ba5-96d3-3bf74d66c1b5', interior_struggle_primacy, deontological).
narrative_ontology:cs_axiom('09503852-8f40-4ba5-96d3-3bf74d66c1b5', secondary, public_authority_gate_for_armed_force).
narrative_ontology:cs_axiom_status(public_authority_gate_for_armed_force, holdable).
narrative_ontology:cs_axiom_grounding('09503852-8f40-4ba5-96d3-3bf74d66c1b5', public_authority_gate_for_armed_force, conventional).
narrative_ontology:cs_reference_frame('09503852-8f40-4ba5-96d3-3bf74d66c1b5', medinan_covenant_coexistence_order).
narrative_ontology:cs_drift_state('09503852-8f40-4ba5-96d3-3bf74d66c1b5', contemporary_nation_state_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('09503852-8f40-4ba5-96d3-3bf74d66c1b5', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, noncombatant_civilians).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, coexisting_religious_minorities).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, official_religious_institutions).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_majority_states).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, unauthorized_militant_movements).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, noncombatant_civilians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, ordinary_believers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, ordinary_believers).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, defensive_war_doctrine).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, noncombatant_immunity_norm).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, proportionality_requirement).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, public_authority_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Al-Azhar, the Diyanet, national fatwa councils, and seminary networks produce the authoritative commentaries that fix this reading as orthodoxy: they certify which conflicts meet the defensive threshold, train clergy who preach the primacy of the interior struggle, and issue condemnations of unauthorized militancy. Their endowments, state salaries, and standing depend on remaining the certified interpreters; stepping away from that seat would dissolve the very authority the reading confers on them.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, official_religious_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Criminalize unauthorized armed activity, run counter-extremism programs, and invoke the defensive-spiritual reading in diplomacy and domestic legitimation. Because they occupy the gate the reading builds — they are the authority whose sanction makes force legitimate — they can stretch the defensive threshold for their own campaigns (preemption, cross-border operations framed as defense) while binding citizens strictly. Relinquishing the arrangement would cost them the violence monopoly and the legitimation frame, so they maneuver between the reading's demands and sovereign prerogative rather than exiting.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_majority_states, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__defensive_spiritual_reading, muslim_majority_states, beneficiary).

% Are taught that the greater jihad is the struggle against the self, that armed force is exceptional and requires public sanction, and that private militant claims corrupt the faith. They surrender unilateral judgment over sanctified violence to authorities they do not control, bear taxation and conscription for wars declared in the reading's name, and cannot readily hold the state to the same threshold they themselves are bound by. Leaving the arrangement would mean leaving the faith community, which few can afford socially or existentially.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, ordinary_believers, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__defensive_spiritual_reading, ordinary_believers, beneficiary).

% Receive the protection of non-combatant immunity and proportionality limits wherever the discipline actually holds, and are the first casualties wherever a state stretches 'defense' to cover initiative. They hold no seat in the councils that define the threshold and no remedy when it is stretched; their protection depends on enforcement they do not control.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, noncombatant_civilians, beneficiary,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__defensive_spiritual_reading, noncombatant_civilians, payer).

% Live inside the coexistence framework this reading privileges — covenant, citizenship, shared civic space. Their security tracks the reading's dominance: each shift of institutional custody toward the expansionist or vanguard readings narrows their standing. They advocate for the reading but cannot enforce it; emigration is open to some and community-dissolving for all.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, coexisting_religious_minorities, beneficiary,
    moderate, generational, constrained, regional).

% Reject the public-authority gate through emergency jurisprudence and takfir, claiming individual obligation against rulers and occupiers. They bear the arrangement's enforcement directly: proscription, imprisonment, lethal targeting, deradicalization pipelines. Exit is closed from both sides — states hunt them, and their own commitment frameworks brand defection as betrayal. They are the visible evidence that this reading requires active enforcement to hold.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, unauthorized_militant_movements, payer,
    organized, biographical, trapped, global).

% Custodians of the classical fiqh al-siyar corpus, which licenses offensive campaigns under conditions (invitation first, ruler's authority, proportionality) that this reading recasts as historical artifact or embarrassment. Official institutions have sidelined their framework; their objection — that the defensive-spiritual reading flattens fourteen centuries of jurisprudence — carries diminishing weight in the councils that matter. They are not invited to help define the threshold; they watch their tradition repurposed.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, classical_legalist_scholars, excluded,
    moderate, civilizational, identity_locked, global).

% Document the exegetical history, the juridification of jihad, and this reading's rise alongside the nation-state system. They take no seat in the contest, but their philological and historical work is cited by every party, and they can see the whole structure — kernel, competing readings, custody struggles — from outside it.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, academic_islamicists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__defensive_spiritual_reading, muslim_majority_states).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__defensive_spiritual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels and disciplines sanctified violence: concentrates the authority to declare armed jihad in legitimate public institutions, sets a high defensive threshold, binds fighters to proportionality and non-combatant immunity, and redirects the believer's primary obligation of struggle inward (jihad al-nafs) — solving the collective-action problems of private warfare, feud escalation, and uncontrolled militancy.
% TRANSFER_FUNCTION: Moves interpretive authority and the license for sanctified force from individual believers and rival movements to state institutions and official religious bodies; moves obedience and surrendered unilateral judgment from believers to the public-authority gate; moves legitimacy, diplomatic capital, and a domestic-suppression instrument to the states and establishments that certify the reading.
% ABSENT_VOICES: Classical legalist scholars holding the expansionist fiqh corpus are institutionally sidelined — their framework is treated as legacy rather than live position. Vanguard theorists appear only as subjects of counter-extremism proceedings, never as interlocutors. Non-Muslim communities affected by wars declared defensive have no seat in the councils that define the defensive threshold.
% DISAPPEARANCE_RATIONALE: If the defensive-spiritual discipline vanished overnight, private claims to sanctified violence would lose their principal theological counterweight, states would lose a legitimating frame and a delegation tool against rivals, minority communities would lose the coexistence framework their standing rides on, and the contest among the three readings would reorganize around raw coercion rather than juristic certification.
% FOUNDING_PROBLEM: Regulating divinely-sanctioned violence in a community that reads its scripture as commanding striving: preventing private war and unbounded militancy while preserving the community's right of defense, and giving the believer's duty of struggle a form compatible with ordered social life.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic Islamicists (Wael Hallaq on the juridification of jihad, Asma Afsaruddin on the exegetical history, Michael Bonner on the early raiding economy) document the classical construction of declaration conditions as a restraint problem; international humanitarian lawyers note the functional parallel between fiqh al-siyar's immunity rules and jus in bello. The state and establishment seats attest the problem too, but the scholarly record outside the beneficiary set carries the corroboration.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.54, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.54: the arrangement transfers real goods (unilateral judgment, the license to answer perceived aggression, rival movements' existence) upward to the gate-holder, but it also delivers large protective value, so base extraction sits mid-range rather than high. Suppression 0.62 is authored as a raw structural property — the enforcement machinery (proscription regimes, counter-extremism courts, curriculum control, official condemnation cycles) needed to keep unauthorized readings from operating — and is NOT scaled by power or scope; only extractiveness is scaled, by the engine, from directionality and scope. Theater_ratio 0.40: the disciplinary core is real, but a growing share of activity is performative — interfaith declarations, diplomatic condemnations, and orthodoxy statements that decouple from state war practice. Accessibility_collapse 0.30: the sibling readings have not collapsed at all; the expansionist corpus persists in the tradition and the vanguard reading recurs under occupation and crisis, so alternatives remain visibly available. Resistance 0.65: the arrangement meets sustained resistance from movements that reject the gate, from sidelined legalists, and from states that resist the reading's application to themselves. Measurements run on one shared grid (interval 0-100 approximates 1924-2024, points every 20 years): base extraction climbs as states adopt and instrumentalize the reading; theater climbs with diplomatic performance; suppression_requirement climbs as counter-extremism infrastructure matured — an enforcement-intensification trajectory, which is why suppression_requirement is tracked rather than left static. Receipt surface: the gains demonstrably accrue to the state seat (violence monopoly, legitimation, suppression instrument), so gain_flow names muslim_majority_states rather than 'diffuse'. Fixing cost: the seat that could repair the asymmetry — by binding itself to the threshold it administers — would have to concede sovereign war prerogative, a cost prohibitive relative to what that seat gains from the status quo, hence fixing_cost 'prohibitive'.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the state and establishment seats, the arrangement is a legitimate order they administer: they built the gate, staff the councils, and experience the discipline as their own achievement — coordination-beneficiary classifications with low effective extraction. From the unauthorized movements' seat, the same structure is enforced closure: proscription, targeting, and the criminalization of their reading — a high-directionality target experience. Ordinary believers sit mid-range: they receive protection and a workable spiritual frame while surrendering judgment to authorities they cannot hold accountable. The sidelined legalist scholars experience discursive displacement without physical enforcement. The engine computes these divergences from the power, exit, and role data; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality: official_religious_institutions (their authority is constituted by the reading), muslim_majority_states (monopoly and legitimation flows to them), coexisting_religious_minorities (framework-dependent protection), noncombatant_civilians (immunity coverage). Declared victims derive high directionality: unauthorized_militant_movements (trapped, organized, bearing the full enforcement burden) and noncombatant_civilians, who are deliberately dual-listed — their protection and their exposure to threshold-stretching are both structural facts, landing them near the middle rather than at either pole. Ordinary_believers are likewise dual-positioned (payer-beneficiary), deriving a mid-range d. No directionality overrides are used: the beneficiary/victim declarations plus exit options already produce the right relationships, and the state seat's self-exemption is captured structurally by its arbitrage exit and agenda-setter role rather than by distorting its d away from its genuine beneficiary position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — disciplining sanctified violence while preserving defense — is live, not dead: every occupation, insurgency, and inter-state war in the Muslim world re-litigates the threshold. Mandatrophy is therefore NOT resolved, and the classification guards against both mislabelings. Reading the arrangement as pure coordination (rope) would erase the state-gate capture: the monopoly, the self-exemption, and the suppression instrument are not overhead but features of how the gate operates. Reading it as pure extraction (snare) would erase the genuine protective function: non-combatant immunity, proportionality, and the interior-struggle redirect solve a real collective-action problem that would not solve itself. Tangled_rope holds both faces simultaneously, and the temporal series shows the extraction face thickening over the interval without the coordination face disappearing — accumulation, not atrophy. The status=live founding problem combined with the world_rearranges disappearance verdict produces no zombie flag; the arrangement persists because the problem persists, not because anyone is performing a dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (defensive_spiritual_reading) of the jihad_quranic_corpus kernel; the expansionist_legalist and revolutionary_vanguard readings instantiate different constraints over the same verses — which reading holds institutional custody, and what does each custody transfer change?',
    'Track custody events: leadership appointments at al-Azhar/Diyanet-type institutions, state adoption of counter-extremism curricula, shifts in juristic consensus documents; custody is observable in who certifies legitimate force.',
    'Under expansionist custody the victim set expands to non-aggressing non-Muslim polities and extraction rises sharply; under vanguard custody the public-authority gate dissolves and enforcement targets flip from movements to states; under this reading''s custody the current structure holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the jihad kernel holds institutional custody determines the operative constraint.').

omega_variable(
    faithful_recovery_vs_state_accommodation,
    'Is the defensive-spiritual reading a faithful recovery of the kernel''s center of gravity, or a modern accommodation shaped by state sovereignty and international law?',
    'Philological and exegetical analysis independent of state patronage: verse chronology (Meccan/Medinan layers), hadith authentication strata, and pre-modern interior-struggle traditions (Sufi jihad al-nafs lineages) that predate the nation-state.',
    'If accommodation, the reading''s authority partly reduces to alignment with state interest and its extraction profile worsens; if recovery, the discipline is the kernel''s authentic constraint and its coordination claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(faithful_recovery_vs_state_accommodation, conceptual, 'Whether the reading''s content is driven by the text or by the modern state''s needs.').

omega_variable(
    state_gate_discipline_or_monopoly,
    'Does the public-authority gate discipline state violence, or does it function as a monopoly instrument that exempts the administering state from the discipline it imposes on everyone else?',
    'Systematic audit of state-declared defensive campaigns against the reading''s own criteria (imminence, last resort, proportionality, non-combatant immunity) across a defined case set.',
    'If monopoly instrument, effective extraction concentrates at the state seat and the arrangement trends toward captured-snare dynamics; if genuine discipline, the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_gate_discipline_or_monopoly, empirical, 'Whether the gate binds its gatekeeper or only its subjects.').

omega_variable(
    threshold_operationalizability,
    'Can the defensive threshold be specified precisely enough to distinguish defense from pretext, or is its indeterminacy structural?',
    'Comparative application of the criteria to contested cases by independent juristic panels; convergence rates across panels indicate operability.',
    'Structural indeterminacy would mean the gate''s output tracks the gatekeeper''s interest by design, raising effective extraction regardless of intent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_operationalizability, empirical, 'Whether the defensive threshold is determinate enough to constrain.').

omega_variable(
    coexistence_equality_status,
    'Does the coexistence framework this reading privileges extend full civic equality to non-Muslims, or conditioned toleration whose terms the Muslim polity sets?',
    'Compare the reading''s juristic outputs (citizenship charters, minority-fiqh documents such as the Amman Message and Mardin Declaration) against the lived legal status of minority communities under them.',
    'Conditioned toleration would place minorities inside a graded protection structure rather than equal protection, shifting their directionality and weakening the arrangement''s coordination purity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_equality_status, preference, 'Whether the privileged coexistence framework means equality or licensed toleration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(jiha_tr_t0, observed).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(jiha_tr_t20, observed).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(jiha_tr_t40, observed).
narrative_ontology:measurement(jiha_tr_t60, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement_basis(jiha_tr_t60, observed).
narrative_ontology:measurement(jiha_tr_t80, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 80, 0.36).
narrative_ontology:measurement_basis(jiha_tr_t80, observed).
narrative_ontology:measurement(jiha_tr_t100, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 100, 0.4).
narrative_ontology:measurement_basis(jiha_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(jiha_be_t0, observed).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement_basis(jiha_be_t20, observed).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement_basis(jiha_be_t40, observed).
narrative_ontology:measurement(jiha_be_t60, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 60, 0.46).
narrative_ontology:measurement_basis(jiha_be_t60, observed).
narrative_ontology:measurement(jiha_be_t80, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 80, 0.5).
narrative_ontology:measurement_basis(jiha_be_t80, observed).
narrative_ontology:measurement(jiha_be_t100, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 100, 0.54).
narrative_ontology:measurement_basis(jiha_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(jiha_su_t0, observed).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(jiha_su_t20, observed).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(jiha_su_t40, observed).
narrative_ontology:measurement(jiha_su_t60, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 60, 0.53).
narrative_ontology:measurement_basis(jiha_su_t60, observed).
narrative_ontology:measurement(jiha_su_t80, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement_basis(jiha_su_t80, observed).
narrative_ontology:measurement(jiha_su_t100, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 100, 0.62).
narrative_ontology:measurement_basis(jiha_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the jihad_quranic_corpus kernel per the epsilon-invariance principle: the colloquial label 'jihad' conflates three structurally distinct constraints. This story (defensive_spiritual_reading) authors epsilon for the state-gated defensive discipline as this reading sees it — mid-range, with a genuine coordination core and a state-capture face. The expansionist_legalist_reading authors epsilon for the classical offensive-campaign framework (higher victim-set breadth, different enforcement geometry); the revolutionary_vanguard_reading authors epsilon for the takfir-emergency framework (state gate dissolved, enforcement inverted). The upstream classical corpus feeds all three as shared textual substrate; each story links the others via network.affects_constraints. Divergence in epsilon across the family is the signal that the colloquial label was doing conflation work, not that any single story is mis-measured.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
