% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: Post-1945 Winnability Rhetorical Taboo with Persistent Classified Planning
 *   domain: strategic studies/international relations
 *
 * SUMMARY:
 *   Since the late 1940s the public language of nuclear strategy has
 *   contracted around a single renunciation — nuclear war cannot be won and
 *   must never be fought — while inside classification the planning of
 *   nuclear war has continued without interruption, from the early SIOPs
 *   through PD-59's countervailing refinement to successive posture reviews
 *   and contemporary modernization programs. This story instantiates the
 *   rhetorical_contraction reading of the war_winnability_post_1945 kernel:
 *   the discursive space for winnability closed while the operational space
 *   merely narrowed. The arrangement's signature is the gap between the two
 *   layers — public renunciation stabilizes signaling and shields leaders
 *   electorally, while the planning channel operates free of public
 *   contestation. Democratic oversight bears the loss: legislatures fund what
 *   they may not openly debate, and publics carry risks they may not
 *   deliberate. KEY AGENTS (by structural relationship): -
 *   nuclear_war_planners: agenda-setting establishment
 *   (institutional/identity_locked) — runs the classified planning channel
 *   and drafts the declaratory line - elected_political_leadership:
 *   beneficiary with speech burden (powerful/constrained) — collects
 *   deterrent credibility without defending war-fighting logic -
 *   democratic_oversight_institutions: primary target (organized/trapped) —
 *   funds and ratifies what it may not publicly contest - citizen_publics:
 *   diffuse risk-bearers (powerless/trapped) — bear plan risk without
 *   deliberative voice - adversary_general_staffs: reciprocal beneficiary
 *   (institutional/identity_locked) — mirrored establishments sheltered by
 *   the same taboo - anti_nuclear_movements: excluded voice
 *   (organized/constrained) — objects from outside the classified rooms -
 *   strategic_studies_scholars: analytical observer (analytical/analytical) —
 *   documents the two-layer structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.66).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.58).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.66).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "Post-1945 Winnability Rhetorical Taboo with Persistent Classified Planning").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic studies/international relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, '00a19983-cca5-4906-8e13-25a3f9d99491').
narrative_ontology:cs_kernel_codification('00a19983-cca5-4906-8e13-25a3f9d99491', distributed).
narrative_ontology:cs_authority_grounding('00a19983-cca5-4906-8e13-25a3f9d99491', distributed).
narrative_ontology:cs_reading_relation('00a19983-cca5-4906-8e13-25a3f9d99491', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('00a19983-cca5-4906-8e13-25a3f9d99491', war_winnability_post_1945__countervailing_thinkable, influences).
narrative_ontology:cs_axiom('00a19983-cca5-4906-8e13-25a3f9d99491', foundational, rhetorical_taboo_masks_operational_continuity).
narrative_ontology:cs_axiom_status(rhetorical_taboo_masks_operational_continuity, holdable).
narrative_ontology:cs_axiom_grounding('00a19983-cca5-4906-8e13-25a3f9d99491', rhetorical_taboo_masks_operational_continuity, empirically_contingent).
narrative_ontology:cs_axiom('00a19983-cca5-4906-8e13-25a3f9d99491', secondary, civilian_control_requires_doctrinal_visibility).
narrative_ontology:cs_axiom_status(civilian_control_requires_doctrinal_visibility, holdable).
narrative_ontology:cs_axiom_grounding('00a19983-cca5-4906-8e13-25a3f9d99491', civilian_control_requires_doctrinal_visibility, deontological).
narrative_ontology:cs_reference_frame('00a19983-cca5-4906-8e13-25a3f9d99491', public_renunciation_private_preparedness).
narrative_ontology:cs_drift_state('00a19983-cca5-4906-8e13-25a3f9d99491', contemporary_great_power_competition, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('00a19983-cca5-4906-8e13-25a3f9d99491', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, nuclear_war_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, elected_political_leadership).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, adversary_general_staffs).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_oversight_institutions).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, citizen_publics).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, existential_deterrence_doctrine).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, nuclear_taboo_normative_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and continuously revise target packages, force-employment plans, and damage-limitation options inside classified channels, from the early single integrated operational plans through later posture reviews and modernization programs. Simultaneously help draft the declaratory language asserting nuclear war cannot be won and must never be fought. Career paths, budgets, and institutional purpose depend on the planning mission continuing; the organization's identity is fused with preparedness, so leaving the mission would mean dissolving the institution itself.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, nuclear_war_planners, agenda_setter,
    institutional, generational, identity_locked, national).

% Set declaratory policy and approve targeting guidance, collecting the political benefit of a credible deterrent without ever defending war-fighting logic in public. Any leader who publicly argues nuclear war could be won risks electoral ruin and alliance alarm, so the taboo disciplines their speech even as it shields their decisions from scrutiny. Leaving office is the only exit from the muzzle, and it arrives on a fixed clock.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, elected_political_leadership, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, elected_political_leadership, agenda_setter).

% Legislatures appropriate the funds and ratify the frameworks but conduct oversight almost entirely inside classification; the rhetorical closure removes the public half of deliberation, so committee members can question numbers but not contest doctrine before the public. They cannot exit the constitutional duty to fund or reject postures they may not openly debate.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, democratic_oversight_institutions, payer,
    organized, generational, trapped, national).

% Bear the ultimate risk of the plans and the opportunity cost of foreclosed debate. Participation is limited to episodic mobilization — bans-the-bomb campaigns, freeze movements, humanitarian-consequence initiatives — that rises and falls without access to the planning channel. There is no exit from the state's protective umbrella or from the targeting decisions made under it.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, citizen_publics, payer,
    powerless, generational, trapped, national).

% Mirror-image planning establishments in rival nuclear states operate under the reciprocal taboo: each side's public renunciation gives the other cover for its own hidden planning and stabilizes mutual signaling. Their institutional identities are likewise fused with preparedness, and the same speech discipline binds their political masters.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, adversary_general_staffs, beneficiary,
    institutional, generational, identity_locked, national).

% Transnational campaigns argue that planning for use entrenches the danger and demand open doctrinal debate. They shape public mood episodically but hold no seat in the classified rooms where plans are written, and the rhetorical consensus frames them as naive rather than as a missing oversight function. Their participation is bounded to protest, litigation, and treaty advocacy outside the planning channel.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, anti_nuclear_movements, excluded,
    organized, generational, constrained, global).

% Analysts across universities and think tanks document the gap between declaratory renunciation and operational planning, from the era of Kahn and Schelling through archival revelations about countervailing strategy and successive posture reviews. They see the full two-layer structure and publish on it, but publication does not enter the planning channel and carries no vote over targeting guidance.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__rhetorical_contraction, nuclear_war_planners).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__rhetorical_contraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates great-power signaling among nuclear-armed states: by mutually forswearing public claims that nuclear war is winnable, rivals reduce crisis pressure toward usable postures, reassure allies without promissory war-fighting doctrines, and stabilize reciprocal expectations of restraint. Stated without evaluation of whether the price paid for this coordination is justified.
% TRANSFER_FUNCTION: Moves deliberative authority and doctrinal visibility from public and legislative spheres into classified planning channels; moves reputational safety to political leaders, who avoid owning war-fighting logic; moves risk-bearing onto publics without a corresponding voice in the plans that generate it.
% ABSENT_VOICES: Anti-nuclear movements and open-government advocates sit outside the classified rooms; rank-and-file legislators without clearances cannot contest doctrine they may not name; publics in targeted and host nations — including non-nuclear alliance members living under others' war plans — have no seat at all. Their absence is what lets the rhetorical consensus appear unanimous.
% DISAPPEARANCE_RATIONALE: If the taboo lifted overnight and planning surfaced, declaratory postures would be rewritten in public, alliance bargains would reopen as extended-deterrent hosts demanded a voice over targeting, crisis bargaining would lose the stabilizing ambiguity both sides currently exploit, and nuclear budgets would face open contestation — the entire two-layer settlement would reorganize within a few political cycles.
% FOUNDING_PROBLEM: How to reconcile continuous preparation for nuclear war with liberal-democratic consent and alliance cohesion: early Cold War governments needed mass arsenals and employment plans but could neither publicly defend war-fighting logic nor abandon it. The dual-layer arrangement was built to hold both — renounce victory publicly, refine it privately.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting set: declassified NSC, Joint Chiefs, and strategic air command records interpreted by independent diplomatic historians; congressional oversight hearings and audit reports; retired officials writing after office with no ongoing stake in shielding current plans. Attestation from within the planning establishment alone would not suffice, and none is relied upon here.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.66 at interval end) because the arrangement transfers deliberative authority and doctrinal visibility from public and legislative spheres into a classified channel that never faces open contestation; the transfer is continuous, not episodic. Suppression (0.58) is real but incomplete: the taboo is enforced by political penalty and the classification regime, yet alternatives are not fully closed — scholars publish, movements mobilize, and occasional politicians breach the line at real cost. Qualitatively the suppression splits between structural mechanisms (classification boundaries, career penalty, alliance discipline — the larger share) and internalized norms (professional discretion, civic self-censorship — the smaller share); omega taboo_penalty_internalization tracks the split. Theater ratio (0.44) reflects the arrangement's dual face: ritual renunciation is partly functional signaling and partly cover, and the share that is cover grew as the threat receded after 1991 while planning persisted. Accessibility collapse (0.55): open doctrinal debate remains possible but carries heavy political cost, so alternatives narrow without vanishing. Resistance (0.5): bans-the-bomb campaigns, the freeze movement, humanitarian-consequence initiatives, and heterodox strategists mount recurring pressure in waves rather than continuously. The measurement series run on one shared time grid (1945–2025, eight points) so every tracked metric is authored at every examined point; the trajectories show accumulation through the Cold War, a post-1991 relaxation, and partial re-intensification under renewed great-power competition. The claimed type (tangled_rope) is authored from structure — a genuine coordination function plus asymmetric extraction — independently of these descriptive metrics; the engine computes per-seat classifications from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the planner seat the arrangement is professional prudence: you deter with what you can credibly employ, and preparing for what you prevent is duty, not deception; institutional identity is fused with preparedness, so exit would mean dissolving the organization's purpose. From the oversight seat the same structure is a democratic deficit: appropriations flow to plans that cannot be named in public debate, reducing accountability to ritual. From the leadership seat it is political survival: renunciation buys electoral safety and alliance reassurance while approval of targeting guidance stays private. From the observer seat the gap itself is the datum — the fact that the two layers diverge is what distinguishes this reading from its siblings. Identity-lock binds the two planning establishments hardest: their organizations have become their function, and the classification boundary protects that fusion from public challenge.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear_war_planners anchor the beneficiary pole: they receive the arrangement's principal yield — operational flexibility without public accountability — and derive low directionality. Elected_political_leadership is declared a beneficiary but also directly bears the taboo's speech penalty: a derivation reading the beneficiary declaration alone would place them near the beneficiary pole (~0.1), so an explicit override sets d to 0.28, reflecting that they collect the shield while carrying the muzzle. Adversary_general_staffs benefit reciprocally — each side's renunciation shelters the other's planning and stabilizes mutual signaling — placing them modestly on the beneficiary side. Democratic_oversight_institutions and citizen_publics anchor the target pole: they bear the transferred costs (foreclosed deliberation, unconsulted risk) with trapped exit, deriving high directionality. Anti_nuclear_movements are excluded rather than coordinated — their exclusion is part of what the rhetorical consensus maintains. Strategic_studies_scholars hold the analytical seat and feed no directionality. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the taboo as pure coordination would erase the accountability extraction: it would predict that opening doctrinal debate is costless and miss why planners and leaders defend the closure. Reading it as pure extraction would erase the genuine signaling function: it would predict that abolishing the taboo leaves deterrence untouched and miss the crisis-stability work the renunciation performs. The tangled_rope classification preserves both halves and directs analysis at their coupling — which functions ride together and which could be separated (omega signaling_accountability_separability). On the genealogy interview, the founding problem (reconciling continuous nuclear preparation with democratic consent and alliance cohesion) remains live as long as arsenals persist, so no resolved-mandatrophy flag is warranted; the mismatch consumer should nonetheless watch the status-by-verdict pair, since a future in which arsenals lapse while the rhetorical apparatus persists would flip the founding-problem status to dead and expose the residual renunciation as ceremony.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'This constraint instantiates the rhetorical_contraction reading of the war_winnability_post_1945 kernel; which reading should govern classification when the readings diverge?',
    'Compile the sibling stories (war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable) and compare epsilon referents, victim sets, and empirical support; adjudication follows whichever reading''s structural premises the archival record sustains.',
    'Under deterrence_unthinkable the hidden planning layer becomes incoherent residue (piton-flavored); under countervailing_thinkable the rhetorical layer becomes dishonesty rather than stabilization; under this reading the gap between the layers is itself the operative structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which reading of the post-1945 winnability kernel governs classification.').

omega_variable(
    taboo_penalty_internalization,
    'Is the taboo''s suppressive force carried mainly by external political and institutional penalty, or by internalized professional and civic norms that persist without enforcement?',
    'Trace violation episodes (Goldwater''s 1964 campaign, Reagan-era defensive rhetoric, post-2022 Russian threshold-lowering signals) and measure career, alliance, and market consequences of violations versus unpunished ones.',
    'If largely internalized, suppression outlives formal enforcement and the arrangement resists removal even after penalties lapse; effective suppression then exceeds the structural measure and exit remains closed after reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_penalty_internalization, empirical, 'Structural versus internalized enforcement of the rhetorical taboo.').

omega_variable(
    planning_persistence_driver,
    'Does operational planning persist beneath the taboo as rational hedging (states prepare for what they deter) or as bureaucratic inertia maintained theatrically?',
    'Compare planning scale and doctrinal revision rates against the threat environment across the interval; examine the post-1991 contraction lag between threat reduction and planning reduction.',
    'If inertia dominates, the arrangement drifts toward vestigial planning under ceremonial renunciation; if hedging dominates, the dual layer is a stable equilibrium and the measured extraction is the deliberate price of preparedness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(planning_persistence_driver, empirical, 'Functional versus vestigial character of the hidden planning layer.').

omega_variable(
    signaling_accountability_separability,
    'Is the taboo''s crisis-stabilizing signaling function separable from its accountability-shielding function?',
    'Natural experiments where the rhetorical layer weakened (SDI-era winnability talk, post-2022 signaling) — assess whether crisis stability degraded through signaling channels independently of oversight effects.',
    'If separable, reforms could restore public deliberation while preserving restraint signaling; if inseparable, part of the measured extraction is the irreducible price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(signaling_accountability_separability, conceptual, 'Whether the taboo''s coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1945, 0.06).
narrative_ontology:measurement_basis(war__tr_t1945, observed).
narrative_ontology:measurement(war__tr_t1957, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1957, 0.16).
narrative_ontology:measurement_basis(war__tr_t1957, observed).
narrative_ontology:measurement(war__tr_t1962, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1962, 0.26).
narrative_ontology:measurement_basis(war__tr_t1962, observed).
narrative_ontology:measurement(war__tr_t1969, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1969, 0.36).
narrative_ontology:measurement_basis(war__tr_t1969, observed).
narrative_ontology:measurement(war__tr_t1980, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1980, 0.43).
narrative_ontology:measurement_basis(war__tr_t1980, observed).
narrative_ontology:measurement(war__tr_t1991, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1991, 0.51).
narrative_ontology:measurement_basis(war__tr_t1991, observed).
narrative_ontology:measurement(war__tr_t2003, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2003, 0.47).
narrative_ontology:measurement_basis(war__tr_t2003, observed).
narrative_ontology:measurement(war__tr_t2025, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2025, 0.44).
narrative_ontology:measurement_basis(war__tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1945, 0.14).
narrative_ontology:measurement_basis(war__be_t1945, observed).
narrative_ontology:measurement(war__be_t1957, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1957, 0.34).
narrative_ontology:measurement_basis(war__be_t1957, observed).
narrative_ontology:measurement(war__be_t1962, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1962, 0.5).
narrative_ontology:measurement_basis(war__be_t1962, observed).
narrative_ontology:measurement(war__be_t1969, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1969, 0.58).
narrative_ontology:measurement_basis(war__be_t1969, observed).
narrative_ontology:measurement(war__be_t1980, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement_basis(war__be_t1980, observed).
narrative_ontology:measurement(war__be_t1991, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1991, 0.6).
narrative_ontology:measurement_basis(war__be_t1991, observed).
narrative_ontology:measurement(war__be_t2003, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2003, 0.63).
narrative_ontology:measurement_basis(war__be_t2003, observed).
narrative_ontology:measurement(war__be_t2025, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2025, 0.66).
narrative_ontology:measurement_basis(war__be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1945, 0.08).
narrative_ontology:measurement_basis(war__su_t1945, observed).
narrative_ontology:measurement(war__su_t1957, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1957, 0.3).
narrative_ontology:measurement_basis(war__su_t1957, observed).
narrative_ontology:measurement(war__su_t1962, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1962, 0.46).
narrative_ontology:measurement_basis(war__su_t1962, observed).
narrative_ontology:measurement(war__su_t1969, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1969, 0.56).
narrative_ontology:measurement_basis(war__su_t1969, observed).
narrative_ontology:measurement(war__su_t1980, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1980, 0.62).
narrative_ontology:measurement_basis(war__su_t1980, observed).
narrative_ontology:measurement(war__su_t1991, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1991, 0.5).
narrative_ontology:measurement_basis(war__su_t1991, observed).
narrative_ontology:measurement(war__su_t2003, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2003, 0.53).
narrative_ontology:measurement_basis(war__su_t2003, observed).
narrative_ontology:measurement(war__su_t2025, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(war__su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, identity_coordination).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__countervailing_thinkable).

% DUAL FORMULATION NOTE:
% The colloquial label 'post-1945 winnability' decomposes into three structurally distinct claims per the epsilon-invariance principle: categorical unwinnability (war_winnability_post_1945__deterrence_unthinkable), constrained winnability through counterforce (war_winnability_post_1945__countervailing_thinkable), and the speech/planning gap (this story). Each carries its own epsilon, beneficiary structure, and classification. deterrence_unthinkable is upstream: its public currency supplies the rhetorical layer this reading treats as the constraint's visible face. countervailing_thinkable is downstream: documented gaps between renunciation and planning legitimize its advocacy by proving the planning is real. All three files cross-link via network.affects_constraints and cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__rhetorical_contraction, powerful, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
