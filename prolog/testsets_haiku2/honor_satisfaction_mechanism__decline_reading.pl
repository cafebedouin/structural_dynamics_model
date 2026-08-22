% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Honor Satisfaction Mechanism (Decline Reading)
 *   domain: social/legal/normative
 *
 * SUMMARY:
 *   Dueling was a practice that provided honor-class gentlemen a mechanism to
 *   settle disputes involving personal reputation and standing. The
 *   constraint was initially high-value (securing honor's restorative
 *   function) and socially centered (the honor class controlled
 *   adjudication). Over three centuries, the constraint weakened through
 *   rising state enforcement (making participation illegal), declining
 *   participation (as the bourgeoisie reframed honor around non-violent
 *   status markers), and cultural reframing (dueling became readable as
 *   anachronistic rather than necessary). This reading emphasizes PERSISTENCE
 *   AT DECLINING FREQUENCY: the practice never became cognitive impossibility
 *   (that is the contraction_reading), but rather persisted as a
 *   diminishing-frequency option, increasingly theatrical, until it reached
 *   fringe status. The constraint remained structurally available—a gentleman
 *   could still duel, the rules remained known—but participation dropped and
 *   enforcement cost rose. Epsilon rises over the interval (0.42→0.68) as
 *   participation declines and the ratio of enforcement to actual practice
 *   grows, and suppression requirement rises steeply (0.15→0.71) as the state
 *   had to work harder to keep something that declined naturally.
 *
 * KEY AGENTS:
 *   - honor_class_gentlemen: The social class for whom honor is identity-constitutive; beneficiary of the mechanism, identity-locked to participation
 *   - dueling_participants: Individuals who actually fought; bore physical risk and legal jeopardy; identity-locked exit cost made formal withdrawal impossible
 *   - state_legal_authority: Criminal enforcement, increasingly aggressive; raised the cost of participation through legal suppression
 *   - bourgeois_non_combatant_class: Rising class that benefited from cultural reframing of honor away from combat; shaped narrative of anachronism
 *   - families_of_casualties: Bore grief and economic loss; powerless to prevent participation by kinsmen without status damage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.68).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.71).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Honor Satisfaction Mechanism (Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "social/legal/normative").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, 'b10794d0-5f84-484a-aac2-bf0e54f6a643').
narrative_ontology:cs_kernel_codification('b10794d0-5f84-484a-aac2-bf0e54f6a643', distributed).
narrative_ontology:cs_authority_grounding('b10794d0-5f84-484a-aac2-bf0e54f6a643', practice).
narrative_ontology:cs_interpretation_layer_present('b10794d0-5f84-484a-aac2-bf0e54f6a643').
narrative_ontology:cs_reading_relation('b10794d0-5f84-484a-aac2-bf0e54f6a643', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b10794d0-5f84-484a-aac2-bf0e54f6a643', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('b10794d0-5f84-484a-aac2-bf0e54f6a643', foundational, honor_persistence_through_fringe_participation).
narrative_ontology:cs_axiom_status(honor_persistence_through_fringe_participation, holdable).
narrative_ontology:cs_axiom_grounding('b10794d0-5f84-484a-aac2-bf0e54f6a643', honor_persistence_through_fringe_participation, empirically_contingent).
narrative_ontology:cs_axiom('b10794d0-5f84-484a-aac2-bf0e54f6a643', foundational, identity_lock_sustains_mechanism_despite_decline).
narrative_ontology:cs_axiom_status(identity_lock_sustains_mechanism_despite_decline, holdable).
narrative_ontology:cs_axiom_grounding('b10794d0-5f84-484a-aac2-bf0e54f6a643', identity_lock_sustains_mechanism_despite_decline, empirically_contingent).
narrative_ontology:cs_reference_frame('b10794d0-5f84-484a-aac2-bf0e54f6a643', honor_as_restorable_through_combat).
narrative_ontology:cs_drift_state('b10794d0-5f84-484a-aac2-bf0e54f6a643', industrial_bourgeois_era_1880_1900, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b10794d0-5f84-484a-aac2-bf0e54f6a643', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, honor_class_gentlemen).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, dueling_participants).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, families_of_casualties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, bourgeois_non_combatant_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A social class for whom honor is constitutive of identity and status. Dueling provided the mechanism by which honor-violations could be settled, reputation defended, and standing restored. The constraint benefits them by encoding their honor-status as socially recognizable and enforceable through combat. As the practice declines, they preserve the cognitive availability of dueling as a restoration mechanism even as participation drops.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, honor_class_gentlemen, beneficiary,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, honor_class_gentlemen, agenda_setter).

% Individuals who actually engaged in duels to settle honor disputes. They bore the direct physical and legal risk. Exit from dueling meant social status loss and being read as cowardly—an identity-constitutive cost that made formal withdrawal from the practice impossible even as the practice declined around them. They paid in blood and legal jeopardy.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, dueling_participants, payer,
    powerful, immediate, identity_locked, national).

% Families who lost household members to duels. They bore grief, loss of economic support, and social exposure. As the practice declined, they could not prevent duel participation by their kinsmen without inflicting status damage on the family honor itself.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, families_of_casualties, payer,
    moderate, generational, constrained, national).

% Criminal legal systems that prohibited dueling and prosecuted participants but whose enforcement effort was selective and episodic. The state gradually increased enforcement intensity, making duel participation carry mounting legal risk. The state could—and eventually did—enforce dueling out of viability, but could not eliminate the cognitive availability of the honor frame itself.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, state_legal_authority, agenda_setter,
    institutional, generational, mobile, national).

% A rising class for whom the honor-satisfaction mechanism was culturally alien or economically irrational. They benefited from dueling's decline because it reduced the perceived legitimacy of combat-based honor restoration and opened space for alternative status-signaling mechanisms (professional achievement, property, education). They did not participate in duels but shaped the cultural narrative that reframed dueling as anachronistic.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, bourgeois_non_combatant_class, beneficiary,
    organized, generational, mobile, national).

% People without access to the honor code entirely—they could not duel because honor-status was restricted to the gentleman class. They would have used violence to settle grievances if afforded the cognitive frame; they were systematically barred from the mechanism by class boundary.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, excluded_lower_classes, excluded,
    powerless, biographical, trapped, national).

% The analytical seat observing how the constraint weakened through declining participation, rising state enforcement, and cultural reframing—while remaining conceptually available and invocable until the final institutional delegitimization.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, historical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__decline_reading, honor_class_gentlemen).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__decline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a recognized, rule-bound mechanism for honor-class gentlemen to settle disputes involving reputation, status, and personal vindication outside the civil legal system. Dueling offered a swift, conclusive restoration of honor when formal law could not address reputational harm.
% TRANSFER_FUNCTION: Transfers physical risk and mortality from the honor-class to dueling participants and their families. Social recognition and status restoration flow from duelists to the honor class. Cultural legitimacy flows from the bourgeoisie toward alternative status mechanisms as dueling declines.
% ABSENT_VOICES: The excluded lower classes—who had no access to the honor frame and thus could not voice a position within it—would have demanded equal access to honor-satisfaction mechanisms if the class boundary had been permeable. They remain structurally barred from the conversation.
% DISAPPEARANCE_RATIONALE: If the honor-satisfaction mechanism had never existed, honor disputes among gentlemen would have been routed differently: either absorbed into civil law, handled through less-formalized social pressure, or managed through alternative status-signaling. The absence of dueling as a practiced mechanism would have restructured how status conflicts were adjudicated, even if the cognitive frame of honor remained available.
% FOUNDING_PROBLEM: A social class for whom honor—personal reputation, standing, and vindication of insult—was identity-constitutive and essential to claim-making, but formal legal systems offered no remedy for purely reputational harm. Dueling provided a mechanism by which honor could be settled and restored through bounded, rule-governed combat.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars outside the honor-class attest that the founding problem (honor without remedy in law) was real in early modern Europe. They also attest that by the 19th century, alternative mechanisms (professional reputation, property, education, legal remedies for slander) had substantially addressed the underlying problem, yet dueling persisted. The benefiting parties (honor-class gentlemen and their apologists) maintained that honor required dueling long after external observers judged the problem functionally solved.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The decline reading models dueling as a Tangled Rope: it coordinatesYeah honor-satisfaction (real function) AND extracts from participants (physical risk, legal jeopardy). Extractiveness rises over time not because the extraction mechanism intensifies, but because participation declines faster than the underlying honor-logic. The ratio of enforcement to actual practice grows—more legal effort per duel—because the practice is increasingly theater: gentlemen maintain the cognitive availability of dueling as an honor restoration mechanism even though fewer actually use it. By 1900, the constraint is performing honor-logic for those still identity-locked to it, while the state enforces heavily against a shrinking fringe. Theater ratio rises (0.08→0.42) because maintenance becomes increasingly performative: the honor class preserves the mechanism's legitimacy through rhetoric and rare high-profile duels rather than routine participation. Suppression requirement rises steeply (0.15→0.71) because the state had to enforce a dying practice; natural decline required less suppression to achieve than artificial elimination would have.
 *
 * PERSPECTIVAL GAP:
 *   The honor-class gentleman perceives dueling as necessary restoration of identity and standing; legal authorities perceive it as criminal violence to be eliminated; the bourgeoisie perceive it as anachronistic theater; participants perceive it as an identity-constitutive risk they cannot exit. The decline reading privileges the empirical fact of PERSISTENCE despite declining frequency, which supports the tangled-rope classification: the mechanism persists because the honor frame persists, even as participation drops.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor-class gentlemen benefit from the cognitive availability of dueling—it encodes their status as restorable and their honor disputes as settleable by themselves rather than external authorities. They are identity-locked to the practice and cannot exit without status loss. Dueling participants bear direct physical and legal risk; they are also identity-locked (exit = cowardice). The state is the enforcer, gradually raising the cost of participation. The bourgeoisie benefit indirectly by the cultural reframing that delegitimates the honor mechanism. Families of casualties pay in grief and economic loss. The lower classes are excluded from the mechanism entirely, so they are not parties to the constraint in the decline reading—they never had access to the frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (honor without legal remedy) was substantially solved by 1800 (civil law expanded, slander remedies available, professional reputation mechanisms emerged). Yet the constraint persisted until ~1920, and indeed the cognitive frame persists today in historical residue and rare high-profile cases. This is a textbook mandatrophy: the mechanism that solved the founding problem was replaced, but the constraint's justification lingered. The decline reading captures this as PERSISTENCE WITHOUT FUNCTION: extractiveness rises, suppression intensifies, theater increases, because the constraint persists in fringe status even after the problem it solved is solved. This distinguishes the decline reading from the contraction reading (where dueling becomes unthinkable) and the composite reading (where multiple mechanisms operate simultaneously).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persistence_vs_functionality,
    'Does the constraint persist because the honor frame remains live (genuine coordination value), or because it persists through institutional inertia after the founding problem is solved?',
    'Detailed analysis of WHY participants continued dueling after legal alternatives existed: did participants believe honor still required it (honor frame live) or did they continue from identity-lock and social pressure (institutional inertia)? Textual evidence from contemporary accounts of late duels.',
    'If honor frame is live, epsilon measures real extraction from a genuine coordination mechanism. If institutional inertia, epsilon measures mandatrophy: a mechanism that lost its function but persists in theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_vs_functionality, conceptual, 'Whether declining dueling represents honor-frame vitality or institutional zombification').

omega_variable(
    identity_lock_vs_legal_suppression,
    'Did participation decline primarily because state enforcement raised the cost, or because the bourgeois reframing of honor delegitimated the practice culturally, leaving identity-lock insufficient to sustain participation?',
    'Comparison of duel frequency in jurisdictions with stricter enforcement vs. looser enforcement; analysis of who stopped dueling and why (legal fear vs. cultural shame vs. class transition).',
    'If enforcement-driven, suppression is the primary mechanism of decline. If culturally-driven, the honor frame is weakening independent of enforcement, and the decline reading''s depiction of persistent availability may overstate the constraint''s vitality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_legal_suppression, empirical, 'Whether decline is enforcement-driven or culturally-driven').

omega_variable(
    reading_vs_contraction_boundary,
    'At what point does a practice at declining frequency become impossible to distinguish from cognitive impossibility? If dueling survives as a ceremonial or honorific gesture with no actual fatal intent, is that persistence of the constraint or category-shift to something else?',
    'Genealogy of rare late-19th and early-20th century duels (the Bismarck/Arnim duel, etc.): are these examples of the honor mechanism still functioning, or are they historical echoes divorced from real honor-satisfaction logic?',
    'If the rare late duels are echoes without function, the contraction reading is empirically correct and this decline reading mischaracterizes the terminal state. If they retain honor-settlement logic, the decline reading is correct and the mechanism genuinely persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_contraction_boundary, conceptual, 'Whether terminal state is persistence-in-fringe-status or cognitive category-dissolution').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71 by 1900) structural (external legal and social barriers preventing dueling) or internalized (participants have internalized the cultural reading that dueling is shameful/anachronistic)?',
    'Post-suppression trajectory: if suppression were lifted entirely (no legal penalties, no social stigma), would late-19th-century honor-class members still duel, or has the practice been internalized as wrong even by those identity-locked to the honor frame?',
    'If structural, removing enforcement would restore participation. If internalized, the suppression operates through identity-reframing and enforcement removal would not restore the practice—the decline is deeper than measured suppression suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is externally enforced or internalized through cultural reframing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1600, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1600, 0.08).
narrative_ontology:measurement_basis(hono_tr_t1600, projected).
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1700, 0.12).
narrative_ontology:measurement_basis(hono_tr_t1700, projected).
narrative_ontology:measurement(hono_tr_t1780, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1780, 0.18).
narrative_ontology:measurement_basis(hono_tr_t1780, observed).
narrative_ontology:measurement(hono_tr_t1840, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1840, 0.28).
narrative_ontology:measurement_basis(hono_tr_t1840, observed).
narrative_ontology:measurement(hono_tr_t1880, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1880, 0.38).
narrative_ontology:measurement_basis(hono_tr_t1880, observed).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1900, 0.42).
narrative_ontology:measurement_basis(hono_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1600, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1600, 0.42).
narrative_ontology:measurement_basis(hono_be_t1600, projected).
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1700, 0.55).
narrative_ontology:measurement_basis(hono_be_t1700, projected).
narrative_ontology:measurement(hono_be_t1780, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1780, 0.61).
narrative_ontology:measurement_basis(hono_be_t1780, observed).
narrative_ontology:measurement(hono_be_t1840, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1840, 0.66).
narrative_ontology:measurement_basis(hono_be_t1840, observed).
narrative_ontology:measurement(hono_be_t1880, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1880, 0.68).
narrative_ontology:measurement_basis(hono_be_t1880, observed).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1900, 0.68).
narrative_ontology:measurement_basis(hono_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1600, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1600, 0.15).
narrative_ontology:measurement_basis(hono_su_t1600, projected).
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1700, 0.28).
narrative_ontology:measurement_basis(hono_su_t1700, projected).
narrative_ontology:measurement(hono_su_t1780, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1780, 0.42).
narrative_ontology:measurement_basis(hono_su_t1780, observed).
narrative_ontology:measurement(hono_su_t1840, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1840, 0.58).
narrative_ontology:measurement_basis(hono_su_t1840, observed).
narrative_ontology:measurement(hono_su_t1880, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1880, 0.68).
narrative_ontology:measurement_basis(hono_su_t1880, observed).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1900, 0.71).
narrative_ontology:measurement_basis(hono_su_t1900, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1600, tn=1900
narrative_ontology:measurement(hono_grid_01, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(class), 1600, 0.88).
narrative_ontology:measurement(hono_grid_02, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(class), 1900, 0.52).
narrative_ontology:measurement(hono_grid_03, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(individual), 1600, 0.76).
narrative_ontology:measurement(hono_grid_04, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(individual), 1900, 0.68).
narrative_ontology:measurement(hono_grid_05, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(organizational), 1600, 0.81).
narrative_ontology:measurement(hono_grid_06, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(organizational), 1900, 0.38).
narrative_ontology:measurement(hono_grid_07, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(structural), 1600, 0.72).
narrative_ontology:measurement(hono_grid_08, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(structural), 1900, 0.41).
narrative_ontology:measurement(hono_grid_09, honor_satisfaction_mechanism__decline_reading, resistance(class), 1600, 0.22).
narrative_ontology:measurement(hono_grid_10, honor_satisfaction_mechanism__decline_reading, resistance(class), 1900, 0.75).
narrative_ontology:measurement(hono_grid_11, honor_satisfaction_mechanism__decline_reading, resistance(individual), 1600, 0.31).
narrative_ontology:measurement(hono_grid_12, honor_satisfaction_mechanism__decline_reading, resistance(individual), 1900, 0.71).
narrative_ontology:measurement(hono_grid_13, honor_satisfaction_mechanism__decline_reading, resistance(organizational), 1600, 0.35).
narrative_ontology:measurement(hono_grid_14, honor_satisfaction_mechanism__decline_reading, resistance(organizational), 1900, 0.68).
narrative_ontology:measurement(hono_grid_15, honor_satisfaction_mechanism__decline_reading, resistance(structural), 1600, 0.28).
narrative_ontology:measurement(hono_grid_16, honor_satisfaction_mechanism__decline_reading, resistance(structural), 1900, 0.72).
narrative_ontology:measurement(hono_grid_17, honor_satisfaction_mechanism__decline_reading, stakes_inflation(class), 1600, 0.42).
narrative_ontology:measurement(hono_grid_18, honor_satisfaction_mechanism__decline_reading, stakes_inflation(class), 1900, 0.71).
narrative_ontology:measurement(hono_grid_19, honor_satisfaction_mechanism__decline_reading, stakes_inflation(individual), 1600, 0.51).
narrative_ontology:measurement(hono_grid_20, honor_satisfaction_mechanism__decline_reading, stakes_inflation(individual), 1900, 0.74).
narrative_ontology:measurement(hono_grid_21, honor_satisfaction_mechanism__decline_reading, stakes_inflation(organizational), 1600, 0.35).
narrative_ontology:measurement(hono_grid_22, honor_satisfaction_mechanism__decline_reading, stakes_inflation(organizational), 1900, 0.68).
narrative_ontology:measurement(hono_grid_23, honor_satisfaction_mechanism__decline_reading, stakes_inflation(structural), 1600, 0.28).
narrative_ontology:measurement(hono_grid_24, honor_satisfaction_mechanism__decline_reading, stakes_inflation(structural), 1900, 0.62).
narrative_ontology:measurement(hono_grid_25, honor_satisfaction_mechanism__decline_reading, suppression(class), 1600, 0.15).
narrative_ontology:measurement(hono_grid_26, honor_satisfaction_mechanism__decline_reading, suppression(class), 1900, 0.72).
narrative_ontology:measurement(hono_grid_27, honor_satisfaction_mechanism__decline_reading, suppression(individual), 1600, 0.18).
narrative_ontology:measurement(hono_grid_28, honor_satisfaction_mechanism__decline_reading, suppression(individual), 1900, 0.74).
narrative_ontology:measurement(hono_grid_29, honor_satisfaction_mechanism__decline_reading, suppression(organizational), 1600, 0.08).
narrative_ontology:measurement(hono_grid_30, honor_satisfaction_mechanism__decline_reading, suppression(organizational), 1900, 0.68).
narrative_ontology:measurement(hono_grid_31, honor_satisfaction_mechanism__decline_reading, suppression(structural), 1600, 0.12).
narrative_ontology:measurement(hono_grid_32, honor_satisfaction_mechanism__decline_reading, suppression(structural), 1900, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__decline_reading, 0.12).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% The honor_satisfaction_mechanism kernel admits three structurally distinct constraint readings: (1) DECLINE_READING (this file)—dueling persists at declining frequency until fringe status; epsilon rises as participation drops, suppression intensifies, theater increases. (2) CONTRACTION_READING—dueling becomes cogically unthinkable, a categorical impossibility; epsilon asymptotically approaches zero as the frame dissolves. (3) COMPOSITE_READING—multiple mechanisms operate simultaneously (state monopoly, bourgeois norm-shift, insurance, category-shift); epsilon measures their compound effect. Each reading instantiates a different constraint with different mechanisms, different epsilon trajectories, and different terminal states. The decline reading emphasizes PERSISTENCE despite declining frequency; it distinguishes persistence-in-fringe-status from cognitive-impossibility (contraction) and from mechanical aggregation of multiple mechanisms (composite). All three readings share the same historical period (1600–1900+) and the same kernel (the standing arrangement for honor-satisfaction), but diverge on the mechanism of cessation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_mechanism__decline_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
