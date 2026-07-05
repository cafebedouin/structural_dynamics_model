% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__national_liberation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: AP I Article 1(4) National Liberation Combatant Status Extension
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This story instantiates ONLY the national_liberation_reading of the
 *   contested combatant-status kernel: Article 1(4) of Additional Protocol I
 *   extends combatant status (and hence conditional POW protection) to
 *   organized, command-controlled non-state armed groups fighting against
 *   colonial domination, alien occupation, or racist regimes in exercise of
 *   self-determination, provided they meet Article 43-style organizational
 *   criteria and the distinction requirement of Article 44. This is
 *   deliberately narrower than the general combatant-status kernel: it does
 *   not describe the state-centric Article 4 baseline (a separate
 *   constraint), nor the status-independent Common Article 3 floor (also a
 *   separate constraint) — those are sibling readings, generated as their own
 *   stories, linked here by network edges. The ε for this reading is moderate
 *   for qualifying liberation movements (conditional, criteria-gated benefit)
 *   and structurally high for occupying/colonial powers, who bear an
 *   obligation many never consented to and some categorically reject as a
 *   matter of treaty law.
 *
 * KEY AGENTS:
 *   - national_liberation_movement_fighters: Primary beneficiary (organized/constrained) — gains conditional combatant immunity and POW status
 *   - liberation_movement_political_leadership: Beneficiary and agenda-setter (organized/constrained) — shapes military organization to meet the treaty's command-and-control criteria and leverages recognition diplomatically
 *   - occupying_power_militaries: Primary target (institutional/trapped) — bears the extraction as an imposed legal obligation constraining counterinsurgency doctrine
 *   - colonial_regime_security_forces: Secondary target (institutional/trapped) — bears reputational and operational costs of international characterization
 *   - third_states_and_un_bodies: Analytical/administering observer (institutional/analytical) — applies and enforces the characterization
 *   - civilian_populations_in_contested_territory: Excluded (powerless/trapped) — bears downstream consequences of contested distinction requirement without a voice in drawing the line
 *   - non_ratifying_major_military_powers: Excluded by choice (institutional/arbitrage) — rejects the reading's legitimacy and its ratification obligations entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.52).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.68).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "AP I Article 1(4) National Liberation Combatant Status Extension").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, 'baa7af9a-2185-49e8-b277-89bef8db3928').
narrative_ontology:cs_kernel_codification('baa7af9a-2185-49e8-b277-89bef8db3928', formalized).
narrative_ontology:cs_authority_grounding('baa7af9a-2185-49e8-b277-89bef8db3928', distributed).
narrative_ontology:cs_reading_relation('baa7af9a-2185-49e8-b277-89bef8db3928', combatant_status_definition__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('baa7af9a-2185-49e8-b277-89bef8db3928', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('baa7af9a-2185-49e8-b277-89bef8db3928', foundational, self_determination_struggle_confers_belligerent_status).
narrative_ontology:cs_axiom_status(self_determination_struggle_confers_belligerent_status, holdable).
narrative_ontology:cs_axiom_grounding('baa7af9a-2185-49e8-b277-89bef8db3928', self_determination_struggle_confers_belligerent_status, deontological).
narrative_ontology:cs_axiom('baa7af9a-2185-49e8-b277-89bef8db3928', secondary, cause_of_conflict_is_relevant_to_combatant_classification).
narrative_ontology:cs_axiom_status(cause_of_conflict_is_relevant_to_combatant_classification, holdable).
narrative_ontology:cs_axiom_grounding('baa7af9a-2185-49e8-b277-89bef8db3928', cause_of_conflict_is_relevant_to_combatant_classification, conventional).
narrative_ontology:cs_reference_frame('baa7af9a-2185-49e8-b277-89bef8db3928', self_determination_belligerency_framework).
narrative_ontology:cs_drift_state('baa7af9a-2185-49e8-b277-89bef8db3928', post_cold_war_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('baa7af9a-2185-49e8-b277-89bef8db3928', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, national_liberation_movement_fighters).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, liberation_movement_political_leadership).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_power_militaries).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, colonial_regime_security_forces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, non_ratifying_major_military_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fighters organized under a command structure within a movement resisting colonial domination, alien occupation, or racist regimes gain conditional access to POW status and combatant immunity if they meet the organizational and command-and-control criteria and (per Protocol requirements) distinguish themselves from civilians during military engagement. This shields them from prosecution as ordinary criminals for lawful acts of war and from ill-treatment reserved for unlawful combatants, but only if they can demonstrate the qualifying command structure — many fighters in decentralized or clandestine movements cannot meet this bar and remain exposed.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, national_liberation_movement_fighters, beneficiary,
    organized, generational, constrained, regional).

% Leadership bodies (e.g., the political wings of liberation movements) gain international legal standing and legitimacy by having their armed wing recognized as a lawful belligerent under Article 1(4), which they use diplomatically to press claims for statehood or self-determination. They administer and structure their forces specifically to meet the command-and-control criteria, effectively shaping their military organization around the treaty's requirements.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, liberation_movement_political_leadership, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, liberation_movement_political_leadership, agenda_setter).

% Armed forces of the occupying, colonial, or racist regime bear the direct cost of the reading: they must extend POW treatment and combatant immunity to captured insurgents who meet the criteria, rather than treating them as unlawful combatants or criminals subject to domestic prosecution. This constrains counterinsurgency doctrine, complicates detention and interrogation practices, and is experienced as a legal obligation imposed without their consent to the treaty's underlying political premise (many occupying/colonial powers never ratified AP I or explicitly rejected Article 1(4)).
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_power_militaries, payer,
    institutional, biographical, trapped, regional).

% Domestic security and military forces of the incumbent regime bear reputational and operational costs when international bodies and third states apply the Article 1(4) framework to characterize their conflict as one against a lawful belligerent rather than mere internal unrest, constraining their freedom to treat captured fighters purely under domestic criminal law.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, colonial_regime_security_forces, payer,
    institutional, biographical, trapped, national).

% States that ratified AP I and UN bodies invoke Article 1(4) to characterize particular conflicts, extend diplomatic recognition, or condition aid and legitimacy on the liberation movement's compliance with IHL. Their characterization decisions shape which movements can successfully claim the protection in practice.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, third_states_and_un_bodies, observer,
    institutional, generational, analytical, global).

% Civilians living under occupation or colonial rule are not party to the combatant-status determination but bear consequences of it: the distinction requirement (that liberation fighters carry arms openly during engagements) is meant to protect them from being conflated with combatants, but its practical looseness in guerrilla warfare contexts is frequently cited by occupying powers as justification for expanded targeting or collective punishment. Their voice on how the standard should be drawn is absent from the treaty negotiation record and from most tribunal proceedings.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, civilian_populations_in_contested_territory, excluded,
    powerless, biographical, trapped, local).

% Several major military powers (including the United States and Israel) never ratified AP I in substantial part because of Article 1(4), objecting that it politicizes IHL by extending combatant status based on the cause fought for rather than purely on conduct and organization. They are excluded from the treaty regime by their own choice but remain affected when customary international law arguments are made to bind them regardless of ratification, and their non-ratification is a standing structural fact this reading's proponents must contend with.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, non_ratifying_major_military_powers, excluded,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, non_ratifying_major_military_powers, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates recognition: it gives armed groups fighting colonial domination, alien occupation, or racist regimes a legible path to lawful-combatant status if they organize themselves under a responsible command and comply with the laws of war, aligning incentive to professionalize and discipline irregular forces with the international community's willingness to protect their fighters.
% TRANSFER_FUNCTION: Moves legal protection (POW status, combatant immunity from ordinary prosecution) from occupying/colonial/racist-regime militaries to organized non-state fighters; moves legitimacy and diplomatic standing from incumbent regimes to liberation movements able to meet the criteria; imposes compliance costs and doctrinal constraints on occupying forces.
% ABSENT_VOICES: Civilian populations in contested territories have no direct voice in how the distinction and command-and-control criteria are drawn or applied, despite bearing consequences when the line is contested; non-ratifying major military powers are structurally excluded from the treaty's negotiated compromise even though their forces are often the ones administering occupations the provision targets.
% DISAPPEARANCE_RATIONALE: Liberation movements and their advocates would say the world rearranges sharply: fighters would revert to being treated categorically as unlawful combatants or criminals, losing the international legal leverage the provision provides. Occupying and colonial powers who reject the reading (or never ratified it) would say the world is largely unchanged for them, since they already treat the provision as inapplicable or nonexistent in practice — the dispute over whether it currently changes anything is itself part of the underlying contest.
% FOUNDING_PROBLEM: Decolonization-era diplomatic consensus (largely built by newly independent and non-aligned states at the 1974-77 Geneva diplomatic conference) sought to address the widespread practice of colonial and occupying powers treating anti-colonial and anti-apartheid fighters purely as criminals or terrorists, denying them any combatant protections regardless of how disciplined or law-compliant their forces were, and thereby permitting summary execution, torture, and denial of due process.
% FOUNDING_PROBLEM_CORROBORATION: Non-aligned movement states and international law scholars from outside the direct beneficiary movements (e.g., ICRC commentary and post-colonial legal scholarship) corroborate that mid-20th-century colonial conflicts saw systematic denial of any combatant protection to anti-colonial fighters, supporting the founding problem's historical reality. However, states that never ratified AP I (and their military legal establishments) corroborate a competing account: that the provision was from the outset a politically motivated redrafting of IHL's traditionally status-neutral, conduct-based test, designed to legitimize groups aligned with the negotiating majority rather than to solve a genuine protection gap — this is corroboration from outside the beneficiary set that directly disputes the founding narrative's framing, not merely its continued relevance.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, contested).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52 (moderate) reflecting that the benefit to qualifying liberation movements is real but conditional and narrow (most irregular fighters in practice fail the organizational/distinction bar and gain nothing), while the corresponding extraction from occupying powers is structurally substantial but bounded by widespread non-ratification and non-compliance — many occupying powers simply refuse to apply the standard, which caps the reading's effective global extraction relative to a universally-accepted rule. Suppression (0.68) is high because enforcement of this reading against a resistant occupying power requires active diplomatic and legal pressure (UN characterization, ICRC advocacy, international tribunals) with no independent enforcement mechanism of its own. Theater ratio (0.28) is moderate-low: genuine organizational discipline is incentivized in movements seeking recognition, but a meaningful share of invocation is symbolic/diplomatic positioning rather than operational change in battlefield conduct. Resistance (0.74) is high because occupying and colonial powers, and several major non-ratifying states, actively contest the reading's legitimacy as a matter of treaty law, not mere practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberation movement fighters and their political leadership sit near the beneficiary end: the reading extends them legal status and protection they would otherwise categorically lack, and leadership actively structures forces to qualify. Occupying power militaries and colonial regime security forces sit near the target end: the reading imposes an obligation on them they did not choose and, in the case of major non-ratifying states, explicitly reject as illegitimate — this is a case where the treaty text alone cannot fully derive directionality, since substantial state practice runs contrary to universal acceptance; the derivation is grounded in the reading's own internal logic (this is what the provision does if accepted) with the caveat carried in an omega about non-universal acceptance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — colonial and occupying powers denying any combatant protection to anti-colonial fighters regardless of their conduct — is genuinely contested as live or resolved: with large-scale decolonization substantially complete, the population of conflicts this reading was drafted for (classic colonial wars) has shrunk, while its invocation has migrated toward contemporary occupation and self-determination disputes (a different, harder-to-classify population) that the original drafters may not have anticipated. This is exactly the mandatrophy risk: an arrangement drafted for one historical population being extended, by the same legal machinery, to a structurally different population without renewed consensus. The tangled_rope classification (rather than snare) is preserved because a genuine coordination function persists — providing a legible path to lawful-combatant status that incentivizes organizational discipline — even as its extraction from non-consenting occupying powers is real and enforced only through diplomatic and reputational pressure, not treaty-wide consent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_acceptance_of_article_1_4,
    'Is Article 1(4) binding as a matter of customary international law on states that never ratified AP I, or does it apply only to states that accepted the treaty text?',
    'State practice and opinio juris surveys; ICJ or international tribunal rulings addressing whether Article 1(4) reflects customary law versus treaty-specific innovation; continued tracking of ratification patterns among major military powers.',
    'If customary status is established, the extraction from non-ratifying occupying powers becomes structurally undeniable rather than contested; if it remains treaty-specific, non-ratifying states'' rejection is a legitimate exit option that substantially narrows this reading''s real-world scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_acceptance_of_article_1_4, empirical, 'Whether Article 1(4) binds non-ratifying states as customary law or only as treaty obligation.').

omega_variable(
    criteria_manipulability,
    'Can the ''organized and command-controlled'' criteria be manipulated by movements to claim status without corresponding operational discipline, or does the criteria function as a genuine, verifiable filter?',
    'Comparative case analysis of tribunal and state determinations of qualifying versus non-qualifying groups; examination of whether recognized movements'' actual battlefield conduct diverged from claimed organizational discipline.',
    'If manipulable, theater_ratio should be revised upward and the coordination function is weaker than authored; if the criteria function as a genuine filter, the tangled_rope''s coordination component is more robust than a pure extraction reading would suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criteria_manipulability, empirical, 'Whether the qualifying criteria are a genuine discipline-inducing filter or a gameable label.').

omega_variable(
    sibling_reading_framing_choice,
    'Is the choice to treat national_liberation_reading, state_centric_reading, and functional_protection_reading as three coexisting readings (rather than one contested constraint with a single averaged ε) itself defensible, or does the underlying legal text support a single authoritative hierarchy among them (e.g., functional_protection as a floor beneath the other two)?',
    'Doctrinal analysis of whether Common Article 3 functions as a genuine floor that operates regardless of which combatant-status reading applies, versus whether the three readings are genuinely mutually exclusive framings competing for primacy in any given conflict.',
    'If functional_protection is properly a floor beneath both status readings rather than a coexisting sibling, the network relationship should be revised from coexists_with/influences toward a strict subordination, which would change how contamination propagation analysis treats the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_framing_choice, conceptual, 'Whether the three-reading decomposition reflects genuine framing plurality or masks an underlying hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 1977, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__national_liberation_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement_basis(comb_tr_t1977, observed).
narrative_ontology:measurement(comb_tr_t1987, combatant_status_definition__national_liberation_reading, theater_ratio, 1987, 0.19).
narrative_ontology:measurement_basis(comb_tr_t1987, observed).
narrative_ontology:measurement(comb_tr_t1997, combatant_status_definition__national_liberation_reading, theater_ratio, 1997, 0.22).
narrative_ontology:measurement_basis(comb_tr_t1997, observed).
narrative_ontology:measurement(comb_tr_t2007, combatant_status_definition__national_liberation_reading, theater_ratio, 2007, 0.25).
narrative_ontology:measurement_basis(comb_tr_t2007, observed).
narrative_ontology:measurement(comb_tr_t2016, combatant_status_definition__national_liberation_reading, theater_ratio, 2016, 0.27).
narrative_ontology:measurement_basis(comb_tr_t2016, observed).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__national_liberation_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(comb_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__national_liberation_reading, base_extractiveness, 1977, 0.4).
narrative_ontology:measurement_basis(comb_be_t1977, observed).
narrative_ontology:measurement(comb_be_t1987, combatant_status_definition__national_liberation_reading, base_extractiveness, 1987, 0.44).
narrative_ontology:measurement_basis(comb_be_t1987, observed).
narrative_ontology:measurement(comb_be_t1997, combatant_status_definition__national_liberation_reading, base_extractiveness, 1997, 0.47).
narrative_ontology:measurement_basis(comb_be_t1997, observed).
narrative_ontology:measurement(comb_be_t2007, combatant_status_definition__national_liberation_reading, base_extractiveness, 2007, 0.49).
narrative_ontology:measurement_basis(comb_be_t2007, observed).
narrative_ontology:measurement(comb_be_t2016, combatant_status_definition__national_liberation_reading, base_extractiveness, 2016, 0.51).
narrative_ontology:measurement_basis(comb_be_t2016, observed).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__national_liberation_reading, base_extractiveness, 2024, 0.52).
narrative_ontology:measurement_basis(comb_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__national_liberation_reading, suppression_requirement, 1977, 0.55).
narrative_ontology:measurement_basis(comb_su_t1977, observed).
narrative_ontology:measurement(comb_su_t1987, combatant_status_definition__national_liberation_reading, suppression_requirement, 1987, 0.6).
narrative_ontology:measurement_basis(comb_su_t1987, observed).
narrative_ontology:measurement(comb_su_t1997, combatant_status_definition__national_liberation_reading, suppression_requirement, 1997, 0.62).
narrative_ontology:measurement_basis(comb_su_t1997, observed).
narrative_ontology:measurement(comb_su_t2007, combatant_status_definition__national_liberation_reading, suppression_requirement, 2007, 0.65).
narrative_ontology:measurement_basis(comb_su_t2007, observed).
narrative_ontology:measurement(comb_su_t2016, combatant_status_definition__national_liberation_reading, suppression_requirement, 2016, 0.67).
narrative_ontology:measurement_basis(comb_su_t2016, observed).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__national_liberation_reading, suppression_requirement, 2024, 0.68).
narrative_ontology:measurement_basis(comb_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__national_liberation_reading, 0.12).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the combatant_status_definition kernel. state_centric_reading (Article 4, formal state militaries only) and functional_protection_reading (Common Article 3, status-independent humane treatment) are separate constraint stories with their own ε, metrics, and stakeholder sets. national_liberation_reading occupies a structurally distinct position: moderate ε for the beneficiary class (conditional, criteria-gated), high ε for occupying/colonial powers who bear an obligation many reject outright. The state_centric_reading likely shows lower extraction from powerful state militaries (it is their baseline framework) and higher exclusion effects on non-state actors; the functional_protection_reading likely shows the lowest overall ε of the three, since it applies regardless of contested status determinations and is comparatively uncontested as a humanitarian floor. Read together, the family traces how a single natural-language concept ('who counts as a combatant') decomposes into three structurally distinct legal claims with different beneficiaries, different victims, and different degrees of international consensus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
