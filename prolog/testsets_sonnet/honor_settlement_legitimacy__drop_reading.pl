% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__drop_reading
 *   human_readable: Honor-Settlement Legitimacy — Residual Dueling Practice (Drop Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   In the century following the general decline of dueling as normative
 *   conflict resolution, a fringe but real population of residual
 *   honor-culture adherents continued to treat the code duello as a
 *   legitimate settlement mechanism. This is not fossilized ritual observed
 *   from outside (which would suggest full theatrical atrophy) nor a live
 *   mainstream institution — it is a bounded, geographically and socially
 *   localized practice that persists because specific communities have not
 *   undergone the cognitive-framework transformation the surrounding society
 *   has. The structure functions as a piton: an institution that used to
 *   solve a real coordination problem for a much larger population, now
 *   surviving mostly through the inertia of niche identity-maintenance, with
 *   theater rising as the enclaves increasingly perform rather than
 *   substantively need the code.
 *
 * KEY AGENTS:
 *   - residual_honor_culture_elites: primary agenda-setters/beneficiaries (moderate/identity_locked) — collect status from administering a code most of society has abandoned
 *   - duel_seconds_and_code_arbiters: secondary beneficiaries (moderate/identity_locked) — professional niche exists only because the code persists
 *   - duelists_families: primary payers (powerless/constrained) — bear concrete costs of death or injury with no voice
 *   - junior_officers_and_students_under_code_pressure: coerced participants (powerless/trapped) — internal social coercion persists even though external legal coercion has vanished
 *   - national_legal_and_state_authorities: excluded institutional voice (institutional/analytical) — would deny legitimacy but does not enforce inside the niche
 *   - cultural_historians: analytical observer documenting non-uniform decline
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.42).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.58).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, piton).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Honor-Settlement Legitimacy — Residual Dueling Practice (Drop Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical_sociology/legal_history/cultural_anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, '12dac247-b545-4276-85ab-68e677102aa2').
narrative_ontology:cs_kernel_codification('12dac247-b545-4276-85ab-68e677102aa2', distributed).
narrative_ontology:cs_authority_grounding('12dac247-b545-4276-85ab-68e677102aa2', practice).
narrative_ontology:cs_interpretation_layer_present('12dac247-b545-4276-85ab-68e677102aa2').
narrative_ontology:cs_reading_relation('12dac247-b545-4276-85ab-68e677102aa2', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('12dac247-b545-4276-85ab-68e677102aa2', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('12dac247-b545-4276-85ab-68e677102aa2', foundational, honor_code_legitimacy_is_locally_bounded_not_universal).
narrative_ontology:cs_axiom_status(honor_code_legitimacy_is_locally_bounded_not_universal, holdable).
narrative_ontology:cs_axiom_grounding('12dac247-b545-4276-85ab-68e677102aa2', honor_code_legitimacy_is_locally_bounded_not_universal, conventional).
narrative_ontology:cs_axiom('12dac247-b545-4276-85ab-68e677102aa2', secondary, residual_practice_persistence_evidences_incomplete_not_absent_repertoire).
narrative_ontology:cs_axiom_status(residual_practice_persistence_evidences_incomplete_not_absent_repertoire, holdable).
narrative_ontology:cs_axiom_grounding('12dac247-b545-4276-85ab-68e677102aa2', residual_practice_persistence_evidences_incomplete_not_absent_repertoire, empirically_contingent).
narrative_ontology:cs_reference_frame('12dac247-b545-4276-85ab-68e677102aa2', code_duello_as_settled_honor_adjudication).
narrative_ontology:cs_drift_state('12dac247-b545-4276-85ab-68e677102aa2', early_twentieth_century_enclave_persistence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('12dac247-b545-4276-85ab-68e677102aa2', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, residual_honor_culture_elites).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, duel_seconds_and_code_arbiters).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, duelists_families).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, junior_officers_and_students_under_code_pressure).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__drop_reading, personal_honor_as_adjudicable_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Aristocratic, military-officer, and dueling-society remnants in specific regions (parts of the German Burschenschaften, some Latin American officer corps, isolated Southern US enclaves into the early 20th century) continue to treat the code duello as the legitimate mechanism for settling insult. They retain social standing within their niche by being seen as willing to duel and by administering the code's rules; their standing evaporates the moment the practice is treated as merely criminal or absurd.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, residual_honor_culture_elites, beneficiary,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__drop_reading, residual_honor_culture_elites, agenda_setter).

% Specialists who negotiate terms, certify satisfaction, and adjudicate whether honor has been restored occupy a durable niche role inside the residual practice. Their function only exists because the code persists; they have professional/reputational investment in maintaining its legitimacy within the shrinking community that still recognizes it.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, duel_seconds_and_code_arbiters, beneficiary,
    moderate, biographical, identity_locked, local).

% Wives, children, and dependents bear the concrete cost when a duel results in death or injury — loss of income, social disruption, occasionally destitution. They have no formal voice in whether the duel occurs; the code's legitimacy is adjudicated entirely among men who hold honor-culture standing.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, duelists_families, payer,
    powerless, biographical, constrained, local).

% Younger members of the residual honor community face direct coercive pressure: refusing a challenge inside these niches still carries severe reputational cost (expulsion from a corps, social death within a fraternity), so participation is not freely chosen even though the surrounding national legal system has long since criminalized dueling. Exit from the honor-culture identity entirely is the only real escape, and that exit is itself costly.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, junior_officers_and_students_under_code_pressure, payer,
    powerless, biographical, trapped, local).

% The surrounding state has criminalized dueling and largely ceased treating honor-based killing as legitimate, but has functionally ceded the specific niches where the practice persists — the state's authority is present on paper but is not exercised inside these enclaves, which is precisely what allows the fringe practice to survive. State actors are structurally excluded from the internal adjudication even though they would, if consulted, deny the practice's legitimacy outright.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, national_legal_and_state_authorities, excluded,
    institutional, generational, analytical, national).

% The broader society within which these enclaves are embedded has already made dueling cognitively unthinkable as normal conflict resolution (per the sibling contraction reading) — but this majority culture has no seat inside the honor-culture niche's internal legitimacy calculus. Its disapproval registers as external pressure, not as a party to the settlement.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, surrounding_bourgeois_and_legal_culture, excluded,
    organized, generational, mobile, national).

% Study why dueling did not vanish uniformly — documenting the specific enclaves (fraternity Mensur culture, Latin American military honor codes, isolated aristocratic remnants) where the practice survived as a live, if fringe, option rather than being cognitively foreclosed everywhere at once.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, cultural_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__drop_reading, residual_honor_culture_elites).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__drop_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within the residual honor-culture niche, the code duello still solves a real local coordination problem: it provides a mutually recognized, rule-bound procedure for settling insult claims that avoids open-ended, escalating private violence between the specific parties who still recognize honor as at stake.
% TRANSFER_FUNCTION: Moves physical risk, and sometimes death, from the abstract question of 'who was insulted' onto the bodies of the duelists, while moving the social costs of that risk (loss of income, orphaned dependents, community disruption) onto families who have no vote in whether the duel occurs. It also transfers social capital to seconds and arbiters who administer the surviving code.
% ABSENT_VOICES: Duelists' families and, more broadly, the state's legal apparatus are structurally absent from the niche's internal legitimacy conversation — the state would deny the practice's legitimacy outright if asked, and dependents bear costs with no standing to object within the honor-culture frame itself.
% DISAPPEARANCE_RATIONALE: For the surrounding national society, the practice's disappearance would change essentially nothing — it is already legally dead and socially marginal (this is the world the contraction reading describes). But within the specific residual enclaves, the practice's actual disappearance would collapse a live identity-marker and status-adjudication mechanism that these particular communities still depend on; members of those niches would experience real rearrangement even though the outside world would not notice.
% FOUNDING_PROBLEM: The original code duello was built to provide an alternative to escalating blood feuds among the nobility and officer class — a rule-bound, honor-preserving mechanism for settling insult that substituted for unregulated private war.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and state authorities attest the founding problem (unregulated aristocratic blood feud) has been dead for over a century, made obsolete by state monopolization of legitimate violence and criminal law's absorption of the insult-and-injury function; the residual honor-culture elites themselves attest the problem remains live within their niche, citing ongoing insult dynamics unresolved by civil courts. No corroboration exists from a source outside these two interested camps — no neutral third party affirms the practice still solves a genuine unmet coordination need.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, contested).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__drop_reading_tests).
:- end_tests(honor_settlement_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high because within the enclave the practice retains a real, if attenuated, coordination function — it is not pure predation, but it does transfer risk from abstract insult onto specific bodies and dependents who have no standing to object. Theater ratio rises substantially over the century (0.20 to 0.61) because as the surrounding society's cognitive framework shifts away from honor-violence (the contraction reading's domain), the residual practice increasingly becomes performative identity-marking rather than a functionally necessary risk-settlement mechanism — duels persist less because insult genuinely requires them and more because performing willingness-to-duel signals belonging to the shrinking honor-culture in-group. Suppression_requirement falls over time (0.72 to 0.58) because as mainstream society abandons the practice, external state suppression effort required to marginalize it can actually decrease — the practice self-marginalizes into smaller and smaller niches requiring less active state enforcement to contain, even as the practice itself persists within those niches.
 *
 * PERSPECTIVAL GAP:
 *   From the elite/arbiter seat, the persistence of the code looks like a legitimate, freely chosen cultural practice being unfairly stigmatized by a society that has lost its sense of honor. From the payer seats — families, pressured juniors — the same structure looks like coercive theater maintained by people with an identity stake in its survival, imposing real costs on people who never agreed to the code's terms. The engine's per-seat computation should reflect this: agenda-setter/beneficiary seats likely compute toward rope or scaffold-adjacent readings from their own vantage, while payer seats compute toward something closer to tangled_rope or snare. The story-level claimed_type (piton) reflects the observer/historian seat: an institution that used to serve a broader coordination function now surviving mainly through niche inertia and identity performance, extracting moderately from a small population while beneficiary capture is real but bounded and not expanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Residual honor-culture elites and code arbiters sit near the beneficiary end: they derive standing, professional function, and identity confirmation from the code's continued operation, and their exit from the honor-culture identity would cost them the very status the code provides (hence identity_locked, not simply mobile). Duelists' families and pressured junior members sit near the target end: they bear concrete physical, financial, and coercive costs with no adjudicating voice — families are constrained by dependency, junior members are trapped by identity/reputational stakes within their specific institutional context (officer corps, fraternity) even though they could theoretically exit the broader society's legal system without consequence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (aristocratic blood feud) is genuinely dead at the level of the surrounding society — courts and states solved it long ago. But within the specific residual niche, adherents can correctly say a live problem still exists FOR THEM: their community has not adopted the mainstream dispute-resolution apparatus as legitimate, so from inside the niche, dueling still functions as the only recognized settlement mechanism. Classifying this as piton rather than snare or mountain avoids two mislabeling errors: it is not simply extraction dressed as coordination (there is a genuine if shrinking coordination function operating inside the niche), and it is not natural law (it has victims, beneficiaries, and geographic/social contingency that a mountain claim would deny). The piton classification captures that no party is being extracted from severely enough to trigger reform, and no party benefits enough to expand the practice back into the mainstream — it simply persists, decreasingly functional, in a shrinking pocket.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_criterion,
    'Is the persistence of dueling in residual enclaves better modeled as a genuine surviving alternative within the normative repertoire (drop_reading), or as evidence that the cognitive-framework transformation described by the contraction_reading was never actually complete/universal, or as one strand within an overdetermined multi-mechanism decline (composite_reading)?',
    'Comparative historical analysis of whether enclave persistence correlates with specific structural features (military hierarchy, closed fraternity systems, aristocratic land-tenure remnants) that would support a bounded-drop model, versus diffuse persistence that would better fit an incomplete-contraction model.',
    'If persistence is structurally bounded to specific institutional niches (supporting drop_reading), the practice is best modeled as a distinct residual constraint with its own bounded ε, as authored here. If persistence turns out to be diffuse and unpredictable, the contraction_reading''s claim of near-universal cognitive foreclosure would be undermined and the composite_reading''s overdetermination account would be favored instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_criterion, conceptual, 'Which kernel reading best accounts for non-uniform decline of dueling.').

omega_variable(
    enclave_internal_legitimacy_versus_external_criminality,
    'Does the enclave''s internal legitimacy claim for the code duello carry any independent normative weight, or is it purely a captured local narrative sustained by identity-locked beneficiaries with no outside corroboration?',
    'Survey testimony from enclave members who have exited the honor-culture identity (voluntary defectors) about whether they retrospectively viewed the code as solving a real problem or as coercive theater they were trapped inside.',
    'If defectors uniformly describe the code as coercive theater in retrospect, this strengthens the piton/snare boundary case and would push effective classification for payer seats closer to snare. If defectors describe a genuine felt need the mainstream legal system did not address, the coordination function claim is stronger and the tangled_rope reading gains support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enclave_internal_legitimacy_versus_external_criminality, empirical, 'Whether the enclave''s coordination claim has any validity independent of its beneficiaries'' self-interest.').

omega_variable(
    geographic_boundary_naturalness,
    'Is the specific set of enclaves where dueling persisted (certain German fraternities, some Latin American officer corps, isolated aristocratic pockets) a naturally occurring residue, or was persistence actively cultivated/re-manufactured by interested elites as a status-signaling device once the practice''s original function had died?',
    'Track institutional records for evidence of deliberate revival or active gatekeeping (codified initiation requirements, formal societies dedicated to preserving the practice) versus organic continuity without institutional intervention.',
    'Evidence of deliberate cultivation would push the classification toward tangled_rope (active elite manufacture of extraction dressed as tradition) rather than piton (mere inertial residue); evidence of organic continuity without active promotion supports the piton reading as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_boundary_naturalness, empirical, 'Whether enclave persistence is organic residue or actively manufactured continuity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__drop_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(hono_tr_t0, observed).
narrative_ontology:measurement(hono_tr_t20, honor_settlement_legitimacy__drop_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(hono_tr_t20, observed).
narrative_ontology:measurement(hono_tr_t40, honor_settlement_legitimacy__drop_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(hono_tr_t40, observed).
narrative_ontology:measurement(hono_tr_t60, honor_settlement_legitimacy__drop_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement_basis(hono_tr_t60, observed).
narrative_ontology:measurement(hono_tr_t80, honor_settlement_legitimacy__drop_reading, theater_ratio, 80, 0.57).
narrative_ontology:measurement_basis(hono_tr_t80, observed).
narrative_ontology:measurement(hono_tr_t100, honor_settlement_legitimacy__drop_reading, theater_ratio, 100, 0.61).
narrative_ontology:measurement_basis(hono_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__drop_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(hono_be_t0, observed).
narrative_ontology:measurement(hono_be_t20, honor_settlement_legitimacy__drop_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement_basis(hono_be_t20, observed).
narrative_ontology:measurement(hono_be_t40, honor_settlement_legitimacy__drop_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement_basis(hono_be_t40, observed).
narrative_ontology:measurement(hono_be_t60, honor_settlement_legitimacy__drop_reading, base_extractiveness, 60, 0.39).
narrative_ontology:measurement_basis(hono_be_t60, observed).
narrative_ontology:measurement(hono_be_t80, honor_settlement_legitimacy__drop_reading, base_extractiveness, 80, 0.41).
narrative_ontology:measurement_basis(hono_be_t80, observed).
narrative_ontology:measurement(hono_be_t100, honor_settlement_legitimacy__drop_reading, base_extractiveness, 100, 0.42).
narrative_ontology:measurement_basis(hono_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_settlement_legitimacy__drop_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(hono_su_t0, observed).
narrative_ontology:measurement(hono_su_t20, honor_settlement_legitimacy__drop_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(hono_su_t20, observed).
narrative_ontology:measurement(hono_su_t40, honor_settlement_legitimacy__drop_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement_basis(hono_su_t40, observed).
narrative_ontology:measurement(hono_su_t60, honor_settlement_legitimacy__drop_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement_basis(hono_su_t60, observed).
narrative_ontology:measurement(hono_su_t80, honor_settlement_legitimacy__drop_reading, suppression_requirement, 80, 0.59).
narrative_ontology:measurement_basis(hono_su_t80, observed).
narrative_ontology:measurement(hono_su_t100, honor_settlement_legitimacy__drop_reading, suppression_requirement, 100, 0.58).
narrative_ontology:measurement_basis(hono_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__drop_reading, 0.08).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the honor_settlement_legitimacy kernel. contraction_reading claims dueling became cognitively unthinkable through wholesale framework transformation (near-universal foreclosure); composite_reading claims decline was overdetermined by multiple reinforcing mechanisms with an overall contraction edge; this drop_reading claims the practice simply dropped out of the mainstream repertoire while persisting, bounded, as a live option within specific residual enclaves. Each reading has a distinct ε: contraction_reading's ε is measured against the general society (near-zero extraction, mountain-adjacent, the practice is simply gone); this drop_reading's ε is measured only within the surviving enclaves (moderate extraction, piton, real victims and beneficiaries persist locally). They are not the same constraint measured differently — they describe different populations and different structural claims about what happened to the practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
