% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Succession Rule as Revocable Positive Law (Sovereign Override Reading)
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint models the succession dispute from the perspective of
 *   sovereign legislative supremacy: Salic Law is not a fixed dynastic
 *   constitution but ordinary positive law, made by prior sovereign acts and
 *   therefore alterable by a subsequent sovereign act (a Pragmatic Sanction
 *   or equivalent instrument) that designates a female or cognatic heir. On
 *   this reading, the reigning monarch's power to legislate succession is
 *   itself the higher-order constitutional fact, and armed resistance from
 *   displaced agnatic claimants is rebellion against a lawfully altered
 *   order, not defense of an unbreakable rule. The story authors ε for the
 *   standing arrangement — sovereign-override successions as they actually
 *   operate, including the wars fought to enforce them — not for an idealized
 *   frictionless legislative process.
 *
 * KEY AGENTS:
 *   - reigning_monarch: sets the override, holds the enforcement apparatus
 *   - pragmatic_sanction_designated_heir: primary beneficiary of the reinterpretation
 *   - displaced_agnatic_claimants: bear the direct cost of the legal change, contest via war
 *   - war_burdened_subjects: powerless payers who fund and fight the enforcement of the sovereign's chosen heir
 *   - constitutional_historians: analytical observers assessing whether the doctrine is genuine constitutional innovation or retrospective legal cover
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.58).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.62).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Succession Rule as Revocable Positive Law (Sovereign Override Reading)").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, '0847b1ab-2c0b-47b1-9e3a-644672342fb1').
narrative_ontology:cs_kernel_codification('0847b1ab-2c0b-47b1-9e3a-644672342fb1', formalized).
narrative_ontology:cs_authority_grounding('0847b1ab-2c0b-47b1-9e3a-644672342fb1', lineage).
narrative_ontology:cs_interpretation_layer_present('0847b1ab-2c0b-47b1-9e3a-644672342fb1').
narrative_ontology:cs_reading_relation('0847b1ab-2c0b-47b1-9e3a-644672342fb1', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('0847b1ab-2c0b-47b1-9e3a-644672342fb1', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('0847b1ab-2c0b-47b1-9e3a-644672342fb1', foundational, sovereign_legislative_will_binds_succession_law).
narrative_ontology:cs_axiom_status(sovereign_legislative_will_binds_succession_law, holdable).
narrative_ontology:cs_axiom_grounding('0847b1ab-2c0b-47b1-9e3a-644672342fb1', sovereign_legislative_will_binds_succession_law, conventional).
narrative_ontology:cs_axiom('0847b1ab-2c0b-47b1-9e3a-644672342fb1', secondary, prior_sovereign_acts_do_not_bind_later_sovereign_acts).
narrative_ontology:cs_axiom_status(prior_sovereign_acts_do_not_bind_later_sovereign_acts, holdable).
narrative_ontology:cs_axiom_grounding('0847b1ab-2c0b-47b1-9e3a-644672342fb1', prior_sovereign_acts_do_not_bind_later_sovereign_acts, conventional).
narrative_ontology:cs_reference_frame('0847b1ab-2c0b-47b1-9e3a-644672342fb1', sovereign_legislative_supremacy_over_customary_succession).
narrative_ontology:cs_drift_state('0847b1ab-2c0b-47b1-9e3a-644672342fb1', post_pragmatic_sanction_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0847b1ab-2c0b-47b1-9e3a-644672342fb1', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, reigning_monarch).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, pragmatic_sanction_designated_heir).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, central_dynastic_administration).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, displaced_agnatic_claimants).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, provincial_estates_excluded_from_ratification).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, war_burdened_subjects).
narrative_ontology:constraint_vindicates(salic_prohibition__sovereign_override_reading, sovereign_legislative_supremacy_over_customary_law).
narrative_ontology:constraint_vindicates(salic_prohibition__sovereign_override_reading, dynastic_continuity_as_state_interest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues a Pragmatic Sanction or equivalent legislative act overriding the customary male-only succession rule to secure the throne for a daughter or other non-agnatic heir. Frames Salic Law as a rule of positive convenience the sovereign made and can therefore unmake, not a fixed constitutional bedrock. Commands the treasury, the army, and the diplomatic apparatus needed to enforce the new order.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, reigning_monarch, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains a claim to succeed that customary Salic practice would have denied. Depends entirely on the sovereign act's continued recognition by foreign courts, domestic estates, and the army; without ongoing military and diplomatic defense of the sanction, the claim collapses back to the excluded agnatic line.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, pragmatic_sanction_designated_heir, beneficiary,
    powerful, generational, constrained, national).

% Ministers, jurists, and court factions who draft and administer the sovereign act. They benefit from continuity of the ruling house's favor and from the precedent that sovereign legislative will can reshape succession law, which enlarges their own institutional authority to interpret and enforce dynastic rules.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, central_dynastic_administration, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, central_dynastic_administration, agenda_setter).

% Male relatives who held the stronger customary claim under strict Salic practice are cut out by the sovereign act. They bear the cost of the reinterpretation directly — lost inheritance, lost title — and their only recourse is to contest the sanction's validity, often through war of succession, since domestic courts recognize the sovereign's authority to have made the change.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, displaced_agnatic_claimants, payer,
    powerful, biographical, constrained, continental).

% Regional diets, parlements, or noble assemblies whose formal ratification is sought as legitimating theater but whose substantive objections are overridden once the sovereign's will is declared. They would prefer either strict adherence to the old rule or a genuine deliberative process, but the sovereign act is presented to them largely as a fait accompli requiring registration, not debate.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, provincial_estates_excluded_from_ratification, excluded,
    organized, biographical, constrained, regional).

% Peasants, townspeople, and soldiers who pay in taxation, conscription, and casualties when the displaced agnatic line contests the sanction through war (as in wars of succession). They have no voice in the legislative act and no ability to exit the territory whose throne is disputed, yet they fund and fight the defense of the sovereign's chosen heir.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, war_burdened_subjects, payer,
    powerless, biographical, trapped, national).

% Foreign monarchies and their jurists who either recognize or refuse to recognize the sovereign act, often instrumentally, depending on their own strategic interest in the succession outcome. They are not bound by the domestic legal act and can choose to treat it as valid or as usurpation, which is itself a lever in continental power politics.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, rival_dynastic_courts, excluded,
    institutional, generational, arbitrage, continental).

% Study whether the sovereign-override framing is a genuine legal innovation consistent with the dynasty's own constitutional tradition or a post-hoc rationalization for a power transfer decided by force and diplomacy first, law second.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__sovereign_override_reading, pragmatic_sanction_designated_heir).
narrative_ontology:fixing_cost_class(salic_prohibition__sovereign_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal mechanism for preserving dynastic continuity when the male line fails, avoiding the interregnum, partition, or foreign absorption that a strict, unamendable succession rule would otherwise produce.
% TRANSFER_FUNCTION: Moves the throne and its attached revenues, patronage networks, and territorial authority from the closest agnatic male claimant to the sovereign's preferred heir; moves the costs of defending that transfer (war, taxation, conscription) onto subjects and provincial estates who did not choose the reallocation.
% ABSENT_VOICES: Displaced agnatic claimants are heard only through the courts of war, not through the legislative process that overrode their claim. Provincial estates are nominally consulted but their ratification is largely ceremonial once the sovereign's will is announced; genuine objection is treated as disloyalty rather than legitimate constitutional dissent.
% DISAPPEARANCE_RATIONALE: If the sovereign-override doctrine were repudiated, every succession secured through a Pragmatic Sanction or equivalent act would revert to contestable status, unwinding decades of territorial settlements, alliances contracted on the strength of the designated heir's legitimacy, and the administrative apparatus built around that heir's court.
% FOUNDING_PROBLEM: A ruling house facing extinction of the male line needed a legal instrument to prevent the realm from fragmenting, being absorbed by a rival power, or falling into open civil war over an undefined succession.
% FOUNDING_PROBLEM_CORROBORATION: The sovereign's own jurists and the designated heir's court attest the problem is live and the sanction is the necessary solution. Displaced agnatic claimants and several rival dynastic courts attest, from outside the beneficiary circle, that the 'problem' is a pretext for a power transfer already decided by marriage alliances and military calculation, with the legal act supplying retrospective cover; contemporary and later constitutional historians largely corroborate the outside reading.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__sovereign_override_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises over the interval (0.35 to 0.58) as the initial legislative act (moderate extraction — mostly a redistribution of expectancy) is followed by the wars and enforcement measures required to make the new succession stick, which layer real costs onto subjects and provincial estates. Theater ratio climbs (0.20 to 0.40) because a growing share of the legitimating apparatus — provincial ratifications, foreign recognition ceremonies, published legal opinions — is performative validation of a settlement already decided by dynastic marriage strategy and military capacity, not genuine deliberation. Suppression rises sharply in the early-middle period (0.30 to 0.62) reflecting the shift from legislative announcement to active military and diplomatic enforcement against the displaced line, then plateaus once the settlement is consolidated.
 *
 * PERSPECTIVAL GAP:
 *   From the monarch's and designated heir's seats, the sovereign act is a lawful exercise of legislative supremacy resolving a genuine succession crisis. From the displaced claimants' and war-burdened subjects' seats, the same act is an extractive reallocation enforced by war and asymmetric suppression. The engine computes these divergent seat classifications from the structural power/exit data; this story does not adjudicate which seat is correct — that adjudication is exactly what the sibling readings (immutable_mandate_reading, cognatic_reversion_reading) exist to contest.
 *
 * DIRECTIONALITY LOGIC:
 *   The monarch and the designated heir sit near the beneficiary end: the sovereign act was made in their interest and they command the apparatus that defends it. Displaced agnatic claimants sit near the target end: they lose a stronger customary claim to a legislative act they did not consent to and can contest only through war. War-burdened subjects are powerless payers with trapped exit — they fund and are conscripted into a succession dispute they have no legislative voice in, which is why their directionality sits at the full-target end despite not being named parties to the dispute itself. Provincial estates are excluded rather than coordinated: their ratification role is largely ceremonial.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview captures the mandatrophy risk directly: the doctrine's own beneficiaries insist the extinction-of-the-male-line problem is still live and the sanction remains necessary, while displaced claimants and rival courts read the legal instrument as a wrapper around a decision already made by marriage and force. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (avoiding fragmentation/absorption of the realm) while still registering the asymmetric extraction imposed on displaced claimants and war-burdened subjects — collapsing it to pure extraction would erase the real continuity problem the sovereign act was invoked to solve; collapsing it to pure coordination would erase the war costs and excluded voices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereign_override_kernel_reading,
    'Is Salic Law''s binding force best understood as ordinary positive law the sovereign can revise (this reading), as an immutable dynastic constitution beyond sovereign alteration (immutable_mandate_reading), or as a custom that was never validly binding outside Frankish lands in the first place (cognatic_reversion_reading)?',
    'No empirical resolution is available; the question is adjudicated by which constitutional tradition and legal theory the observer accepts as authoritative for the dynasty in question, and by which side prevails militarily and thereby writes the accepted history.',
    'If the sovereign-override reading is accepted, the designated heir''s succession is fully legitimate and displaced claimants are rebels; if the immutable_mandate_reading prevails, the sovereign act is a nullity and the sanction-based succession is itself the usurpation; if the cognatic_reversion_reading prevails, no override was ever legally necessary because the exclusionary rule did not validly apply to begin with.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereign_override_kernel_reading, conceptual, 'Which of the three kernel readings of Salic Law''s bindingness is structurally correct is irreducibly contested; this story fixes one reading and does not resolve the kernel-level dispute.').

omega_variable(
    legislative_act_vs_military_fact,
    'Did the sovereign legislative act (the Pragmatic Sanction or equivalent) actually determine the succession outcome, or did it merely ratify an outcome already fixed by dynastic marriage alliances and military capacity, with the legal act supplying retrospective legitimation?',
    'Comparative study of cases where a sovereign act was issued but the designated heir lacked military/diplomatic backing (and the sanction failed) versus cases where backing was present (and it succeeded) would clarify whether the legal instrument is doing independent causal work.',
    'If the legal act is causally inert relative to military/diplomatic power, the sovereign-override framing functions primarily as theater legitimating a force-determined outcome, which would push the theater_ratio and suppression readings higher than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_act_vs_military_fact, empirical, 'Whether the legislative override is a genuine determinant of succession or a legitimating gloss on a force-determined outcome.').

omega_variable(
    provincial_ratification_genuineness,
    'Was provincial estate ratification of the sovereign act a genuine check capable of blocking the succession change, or purely ceremonial registration?',
    'Examine recorded instances of provincial or estate bodies withholding or conditioning ratification and whether such withholding had any effect on the succession''s ultimate validity or enforcement.',
    'If ratification was never a real check, the provincial_estates_excluded_from_ratification stakeholder''s exclusion is total rather than partial, which would raise the authored suppression and accessibility_collapse values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provincial_ratification_genuineness, empirical, 'Whether provincial ratification functioned as a real veto point or as legitimating theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__sovereign_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sali_tr_t8, salic_prohibition__sovereign_override_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(sali_tr_t16, salic_prohibition__sovereign_override_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(sali_tr_t24, salic_prohibition__sovereign_override_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(sali_tr_t32, salic_prohibition__sovereign_override_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__sovereign_override_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__sovereign_override_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sali_be_t8, salic_prohibition__sovereign_override_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(sali_be_t16, salic_prohibition__sovereign_override_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(sali_be_t24, salic_prohibition__sovereign_override_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(sali_be_t32, salic_prohibition__sovereign_override_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__sovereign_override_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__sovereign_override_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(sali_su_t8, salic_prohibition__sovereign_override_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(sali_su_t16, salic_prohibition__sovereign_override_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(sali_su_t24, salic_prohibition__sovereign_override_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(sali_su_t32, salic_prohibition__sovereign_override_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__sovereign_override_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(salic_prohibition__sovereign_override_reading, 0.12).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial label 'Salic Law' into structurally distinct kernel readings, per the ε-invariance principle: the immutable_mandate_reading treats the rule as unchangeable natural/divine dynastic law (near-mountain, minimal beneficiary structure, near-total accessibility collapse from the standpoint of that tradition); the cognatic_reversion_reading treats it as a Frankish custom improperly extended to non-Frankish realms (framing it closer to an illegitimate imposition with its own distinct victim set — non-Frankish claimants excluded by an inapplicable rule); this sovereign_override_reading treats it as ordinary revisable positive law, yielding a tangled_rope structure with a real coordination function (dynastic continuity) layered with real asymmetric extraction (displaced claimants, war-burdened subjects). Each reading carries its own ε and its own classification; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
