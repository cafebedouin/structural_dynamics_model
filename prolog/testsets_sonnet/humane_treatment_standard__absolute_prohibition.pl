% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Common Article 3 Absolute Prohibition on Torture and Degrading Treatment
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This story instantiates the absolute_prohibition reading of the
 *   humane_treatment_standard kernel: Common Article 3 as a non-derogable
 *   floor that admits no security exception. Under this reading detainees
 *   hold a complete, unconditional rights set against torture and degrading
 *   treatment; state interrogation methods are constrained absolutely rather
 *   than balanced against a threat assessment. The sibling readings —
 *   contextual_necessity (a security override exists) and
 *   proportionality_balancing (a case-by-case weighing test) — are
 *   structurally different constraints with different beneficiary/victim
 *   profiles and are NOT represented here; see commentary.kernel_context and
 *   the omega variables for how they diverge.
 *
 * KEY AGENTS:
 *   - detained_persons: primary beneficiary (powerless/trapped) — holds the protection but cannot enforce it directly
 *   - detaining_state_interrogators: agenda_setter and payer (institutional/constrained) — administers custody, bears the foreclosed-option cost
 *   - national_security_establishments: payer (powerful/constrained) — bears the perceived opportunity cost of forgone interrogation methods
 *   - icrc_and_monitoring_bodies: observer (organized/analytical) — verifies compliance, has institutional stake in a fixed rather than flexible standard
 *   - international_criminal_tribunals: agenda_setter and observer (institutional/analytical) — supplies the enforcement jurisprudence that gives the reading legal weight
 *   - future_reciprocal_detainees_of_all_parties: diffuse beneficiary (powerless/analytical) — the reciprocity logic that makes this a genuine coordination mechanism, not merely externally imposed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.12).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.35).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.12).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, rope).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Common Article 3 Absolute Prohibition on Torture and Degrading Treatment").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, '6dda4086-baff-46d3-b274-64ca4b3248de').
narrative_ontology:cs_kernel_codification('6dda4086-baff-46d3-b274-64ca4b3248de', fixed_text).
narrative_ontology:cs_authority_grounding('6dda4086-baff-46d3-b274-64ca4b3248de', lineage).
narrative_ontology:cs_interpretation_layer_present('6dda4086-baff-46d3-b274-64ca4b3248de').
narrative_ontology:cs_reading_relation('6dda4086-baff-46d3-b274-64ca4b3248de', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('6dda4086-baff-46d3-b274-64ca4b3248de', humane_treatment_standard__proportionality_balancing, influences).
narrative_ontology:cs_axiom('6dda4086-baff-46d3-b274-64ca4b3248de', foundational, no_circumstance_permits_crossing_the_threshold).
narrative_ontology:cs_axiom_status(no_circumstance_permits_crossing_the_threshold, holdable).
narrative_ontology:cs_axiom_grounding('6dda4086-baff-46d3-b274-64ca4b3248de', no_circumstance_permits_crossing_the_threshold, deontological).
narrative_ontology:cs_axiom('6dda4086-baff-46d3-b274-64ca4b3248de', secondary, detainee_status_confers_complete_rather_than_conditional_protection).
narrative_ontology:cs_axiom_status(detainee_status_confers_complete_rather_than_conditional_protection, holdable).
narrative_ontology:cs_axiom_grounding('6dda4086-baff-46d3-b274-64ca4b3248de', detainee_status_confers_complete_rather_than_conditional_protection, conventional).
narrative_ontology:cs_reference_frame('6dda4086-baff-46d3-b274-64ca4b3248de', unconditional_common_article_3_floor).
narrative_ontology:cs_drift_state('6dda4086-baff-46d3-b274-64ca4b3248de', post_9_11_enhanced_interrogation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6dda4086-baff-46d3-b274-64ca4b3248de', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detained_persons).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, captured_combatants).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, civilian_internees).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, future_reciprocal_detainees_of_all_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, detaining_state_interrogators).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, national_security_establishments).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, human_dignity_is_non_derogable).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, prohibition_of_torture_is_jus_cogens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held by a detaining power during armed conflict, entirely dependent on that power's compliance for physical safety. The absolute prohibition means no circumstance the detaining power invokes — imminent threat, ticking-bomb scenario, reciprocity failure by the adversary — legally opens a path to torture or degrading treatment against them. They cannot enforce this themselves; it depends entirely on external verification and the detaining power's own restraint.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detained_persons, beneficiary,
    powerless, immediate, trapped, global).

% Military and intelligence personnel who conduct interrogations and custody operations. Under this reading, the absolute threshold removes any legal room for methods they might otherwise argue are proportionate or necessary under battlefield time pressure. They administer detention day-to-day and could, if the norm eroded, gain operational flexibility they currently lack; the prohibition costs them a tool they might otherwise want in extremis.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detaining_state_interrogators, agenda_setter,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__absolute_prohibition, detaining_state_interrogators, payer).

% State security apparatuses that must operate within the absolute prohibition even when they assess a detainee holds actionable, time-critical intelligence. They bear the opportunity cost of foreclosed interrogation methods and argue, in the sibling readings, that this cost is sometimes unbearable; under this reading that argument has no legal purchase regardless of the stakes they perceive.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, national_security_establishments, payer,
    powerful, biographical, constrained, national).

% Conduct detention visits, document treatment, and report violations. Their institutional existence and mandate depend on the norm having a fixed, non-negotiable content — a norm that flexed by context would make their monitoring function ambiguous rather than a clear compliance check.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, icrc_and_monitoring_bodies, observer,
    organized, generational, analytical, global).

% Soldiers, operatives, and civilians of every party to a future conflict who will eventually be captured by someone. The absolute rule is the insurance policy each state's own personnel receive from binding itself now; the coordination logic is genuinely reciprocal even though most immediate operational pressure runs against it.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, future_reciprocal_detainees_of_all_parties, beneficiary,
    powerless, generational, analytical, global).

% Adjudicate violations after the fact and have consistently held the Common Article 3 minimum as non-derogable customary and jus cogens law. Their jurisprudence is what gives the absolute reading legal teeth beyond treaty text; they administer the enforcement layer but cannot themselves prevent violations in real time.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_criminal_tribunals, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__absolute_prohibition, international_criminal_tribunals, observer).

% Elected or appointed officials facing acute domestic pressure after an attack, who would argue for a security-exception carve-out but whose position is not represented within this reading's own legal framework — that argument belongs structurally to the contextual_necessity sibling, not to this constraint.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, domestic_political_leadership_under_threat, excluded,
    powerful, immediate, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, non-negotiable floor of treatment that removes torture and degrading treatment from the set of options any party to a conflict can lawfully consider against any detained person, regardless of who holds power at a given moment — solving the problem that any conditional standard collapses under the party who currently has the upper hand.
% TRANSFER_FUNCTION: Moves discretion away from detaining-power interrogators and toward a fixed external standard; the immediate operational flexibility a security establishment might otherwise claim is transferred into a guaranteed protection held by every detained person and, reciprocally, by that state's own personnel when captured in turn.
% ABSENT_VOICES: Domestic political leadership under acute post-attack pressure, and interrogators who believe a specific detainee holds time-critical intelligence, are not parties this reading's legal framework hears from on the question of exceptions — their argument is structurally routed to the sibling contextual_necessity reading, not adjudicated here.
% DISAPPEARANCE_RATIONALE: If the absolute prohibition disappeared, detaining powers would face a live legal argument for conditional treatment tied to perceived necessity in every conflict; interrogation practice, tribunal jurisprudence, and reciprocal protection for captured personnel of every state would all reorganize around a contestable threshold rather than a fixed floor.
% FOUNDING_PROBLEM: After 1949, states needed a treatment floor that would hold even in non-international armed conflicts where full Geneva Convention protections did not apply and where the parties (including non-state actors) had the weakest incentive to restrain themselves absent a hard external rule.
% FOUNDING_PROBLEM_CORROBORATION: International tribunals (ICTY, ICJ) and the ICRC's customary law study attest from outside any single state's security establishment that the non-derogable character of the prohibition remains operative and that no state practice has displaced its jus cogens status. Security establishments in several states have, through post-9/11 legal memoranda and public policy debate, argued the founding problem's boundaries no longer match a threat environment of transnational non-state actors — a position corroborated by their own internal legal opinions rather than by an outside source, which is why founding_problem_status is authored as contested rather than live or dead.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).
:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) because, under this reading, no party is described as extracting rents from the prohibition's operation — the norm imposes an opportunity cost on security establishments but does not transfer value to a beneficiary who administers it for gain. Suppression is moderate (0.35) reflecting the real enforcement apparatus (tribunals, monitoring bodies, treaty ratification pressure) required to hold the line against derogation attempts, particularly visible in the 2001-2005 spike tied to post-9/11 enhanced-interrogation debates. Theater ratio rose sharply in the same window (0.30 to 0.38) as some states maintained formal adherence to Common Article 3 language while constructing parallel legal theories (unlawful combatant status, extraterritorial detention) to route around it — a genuine Goodhart-style substitution the temporal series is built to surface. Accessibility collapse is authored high (0.72): once a state accepts the absolute-prohibition framing, no legally cognizable alternative remains inside that framework — this is a feature of the reading's logical structure, not evidence the constraint is a mountain (it very much is not naturally emergent; it is a chosen, defended legal commitment, hence claimed_type rope rather than mountain, and emerges_naturally is correctly absent).
 *
 * PERSPECTIVAL GAP:
 *   From detained persons' seat, the constraint reads unambiguously as protective coordination they depend on entirely and cannot themselves enforce. From national security establishments' seat under acute threat perception, the same absolute floor reads as an externally imposed constraint removing a tool they believe would serve immediate security — but this experienced cost does not convert the structure into extraction, because no party captures the value of the foreclosed option; it is simply forgone, not transferred.
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons and future reciprocal detainees sit at the low-d beneficiary end: the constraint subsidizes their safety directly and, through reciprocity, protects their own state's personnel later. Detaining-power interrogators and national security establishments sit closer to the payer end: the absolute threshold forecloses methods they might otherwise deploy under perceived necessity, though this is authored as an opportunity cost rather than extraction — no value flows FROM the constraint TO a beneficiary at their expense in the way a snare would require. This is why victims[] is left empty: the structure is coordination with a real cost to one party, not asymmetric extraction with an identifiable victim group, which is why the claim is rope rather than tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (restraining the strongest party in a conflict when no other restraint exists) remains structurally live wherever asymmetric power exists between detaining and detained parties — which is to say, in every armed conflict. Mandatrophy is not indicated here: unlike a scaffold whose transitional justification has expired, this reading's justification (that any conditional standard collapses under whoever holds power) does not diminish with time; if anything the post-9/11 theater-ratio spike demonstrates the justification's continued relevance rather than its obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolute_vs_contextual_kernel_contest,
    'Does Common Article 3''s non-derogable language admit any security-necessity exception, or is the absolute reading the only legally coherent interpretation of the kernel text?',
    'This is the live contest between this story''s reading and the sibling contextual_necessity reading. Resolution would require either a definitive ICJ ruling foreclosing the necessity-exception argument as a matter of customary international law, or sustained state practice (opinio juris) crystallizing around one reading such that the other is no longer a good-faith legal position.',
    'If the contextual_necessity reading were to displace this one as the dominant legal understanding, detained persons would exit the full rights-holder set this story describes and enter a conditional protection regime; if this reading is vindicated, any state practice invoking necessity would be a violation rather than a permitted exception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_vs_contextual_kernel_contest, conceptual, 'Committer-frame ambiguity: which kernel reading (absolute vs. contextual necessity) is the operative legal standard.').

omega_variable(
    proportionality_reading_intermediate_position,
    'Does the proportionality_balancing sibling reading represent a genuinely distinct third position, or is it functionally equivalent to contextual_necessity dressed in weighing-test language?',
    'Comparative analysis of case outcomes under jurisdictions that formally adopt a proportionality test versus jurisdictions that adopt an explicit necessity exception — if outcomes converge, the readings are not structurally distinct despite different framing.',
    'If proportionality_balancing collapses into contextual_necessity in practice, the kernel effectively has two live readings rather than three, which would change how contamination/influence propagates across the family in network analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_reading_intermediate_position, conceptual, 'Whether the three declared kernel readings are truly structurally distinct or partially redundant.').

omega_variable(
    enforcement_gap_vs_norm_content,
    'Where state practice violates the absolute prohibition (documented instances of torture by parties nominally bound by Common Article 3), does this represent an enforcement gap (the norm''s content is intact but compliance fails) or evidence that the contextual_necessity reading has de facto displaced this reading in operative state behavior?',
    'Track whether violating states (a) deny the conduct occurred, (b) admit conduct but reclassify detainees outside the protected category, or (c) openly argue a necessity exception. Pattern (c) would indicate the kernel contest is resolving toward contextual_necessity in practice even if not in doctrine.',
    'If states increasingly choose pattern (c) rather than (a) or (b), the absolute_prohibition reading''s descriptive accuracy (as opposed to its doctrinal status) is eroding, which the theater_ratio and suppression_requirement trajectories in this story are designed to surface.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_vs_norm_content, empirical, 'Whether documented violations reflect enforcement failure or a doctrinal shift toward the necessity-exception reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1949, humane_treatment_standard__absolute_prohibition, theater_ratio, 1949, 0.15).
narrative_ontology:measurement(huma_tr_t1975, humane_treatment_standard__absolute_prohibition, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(huma_tr_t2001, humane_treatment_standard__absolute_prohibition, theater_ratio, 2001, 0.3).
narrative_ontology:measurement(huma_tr_t2005, humane_treatment_standard__absolute_prohibition, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(huma_tr_t2015, humane_treatment_standard__absolute_prohibition, theater_ratio, 2015, 0.32).
narrative_ontology:measurement(huma_tr_t2025, humane_treatment_standard__absolute_prohibition, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(huma_be_t1949, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1949, 0.06).
narrative_ontology:measurement(huma_be_t1975, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1975, 0.07).
narrative_ontology:measurement(huma_be_t2001, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2001, 0.09).
narrative_ontology:measurement(huma_be_t2005, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2005, 0.11).
narrative_ontology:measurement(huma_be_t2015, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2015, 0.12).
narrative_ontology:measurement(huma_be_t2025, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2025, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1949, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1949, 0.2).
narrative_ontology:measurement(huma_su_t1975, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1975, 0.24).
narrative_ontology:measurement(huma_su_t2001, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2001, 0.4).
narrative_ontology:measurement(huma_su_t2005, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(huma_su_t2015, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(huma_su_t2025, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the humane_treatment_standard kernel. absolute_prohibition (this story) holds no security exception exists; contextual_necessity holds a necessity override exists; proportionality_balancing holds a case-by-case weighing test applies. Each reading has its own ε, beneficiary/victim structure, and classification — they are not the same constraint measured differently but three structurally distinct constraints sharing a contested kernel text. The absolute_prohibition reading, being the doctrinally dominant one (backed by tribunal jurisprudence and jus cogens status claims), structurally influences the legitimacy conditions available to the other two readings without foreclosing proportionality_balancing and while directly foreclosing contextual_necessity's core premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
