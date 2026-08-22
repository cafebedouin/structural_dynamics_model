% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__endogenous_climb_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__endogenous_climb_reading
 *   human_readable: Endogenous Climb: Legitimacy Won by Fringe Actors Through Demonstrated Superiority
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the endogenous-climb reading of the state
 *   commitment installation kernel: new legal codes, measurement standards,
 *   or administrative techniques originate at institutional fringes
 *   (provincial courts, merchant confraternities, dissenting factions) and
 *   ascend to central legitimacy by demonstrating superior outcomes, forcing
 *   incumbent apex officeholders to ratify what they did not design and
 *   initially resisted. This is not the exogenous-imposition reading
 *   (top-down mandate by transformation-mandated authority) nor the
 *   hybrid-cascade reading (apex-installed but fringe-validated) — those are
 *   separate constraints with their own ε and beneficiary structures. Here
 *   the fringe is the origin point and the primary beneficiary set; the apex
 *   is the resistant, extraction-bearing seat that eventually concedes.
 *
 * KEY AGENTS:
 *   - fringe_reform_networks: primary agenda_setter/beneficiary (moderate/constrained) — originates and advocates the climbing commitment
 *   - provincial_administrators_early_adopters: beneficiary (moderate/constrained) — bears pilot risk, gains career upside
 *   - merchant_guilds_backing_new_standards: beneficiary (organized/mobile) — funds and circulates, has real exit
 *   - incumbent_apex_officeholders: primary payer (institutional/constrained) — cedes prestige and control on ratification
 *   - legacy_credentialing_bodies: payer (organized/trapped) — devalued by the new standard, cannot exit their own displacement
 *   - peripheral_populations_under_forced_pilot_status: payer (powerless/trapped) — absorbs pilot-phase disruption without consent or record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, 0.42).
domain_priors:suppression_score(state_commitment_installation_mechanism__endogenous_climb_reading, 0.55).
domain_priors:theater_ratio(state_commitment_installation_mechanism__endogenous_climb_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__endogenous_climb_reading, "Endogenous Climb: Legitimacy Won by Fringe Actors Through Demonstrated Superiority").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__endogenous_climb_reading, '1fd51cb5-dff4-4ac2-8ff9-861eb808e6fd').
narrative_ontology:cs_kernel_codification('1fd51cb5-dff4-4ac2-8ff9-861eb808e6fd', distributed).
narrative_ontology:cs_authority_grounding('1fd51cb5-dff4-4ac2-8ff9-861eb808e6fd', practice).
narrative_ontology:cs_interpretation_layer_present('1fd51cb5-dff4-4ac2-8ff9-861eb808e6fd').
narrative_ontology:cs_reading_relation('1fd51cb5-dff4-4ac2-8ff9-861eb808e6fd', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_reading_relation('1fd51cb5-dff4-4ac2-8ff9-861eb808e6fd', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('1fd51cb5-dff4-4ac2-8ff9-861eb808e6fd', foundational, legitimacy_earned_through_demonstrated_performance).
narrative_ontology:cs_axiom_status(legitimacy_earned_through_demonstrated_performance, holdable).
narrative_ontology:cs_axiom_grounding('1fd51cb5-dff4-4ac2-8ff9-861eb808e6fd', legitimacy_earned_through_demonstrated_performance, empirically_contingent).
narrative_ontology:cs_axiom('1fd51cb5-dff4-4ac2-8ff9-861eb808e6fd', foundational, periphery_as_valid_originating_authority).
narrative_ontology:cs_axiom_status(periphery_as_valid_originating_authority, holdable).
narrative_ontology:cs_axiom_grounding('1fd51cb5-dff4-4ac2-8ff9-861eb808e6fd', periphery_as_valid_originating_authority, conventional).
narrative_ontology:cs_reference_frame('1fd51cb5-dff4-4ac2-8ff9-861eb808e6fd', merit_demonstrated_ascension_norm).
narrative_ontology:cs_drift_state('1fd51cb5-dff4-4ac2-8ff9-861eb808e6fd', post_bureaucratic_professionalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1fd51cb5-dff4-4ac2-8ff9-861eb808e6fd', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_reform_networks).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, provincial_administrators_early_adopters).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, merchant_guilds_backing_new_standards).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, incumbent_apex_officeholders).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, legacy_credentialing_bodies).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, peripheral_populations_under_forced_pilot_status).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__endogenous_climb_reading, meritocratic_legitimation_doctrine).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__endogenous_climb_reading, gradualist_institutional_change_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Originate the new commitment (a legal code, a measurement standard, a bureaucratic technique) outside the seat of central authority — in a provincial court, a merchant confraternity, a dissenting clerical faction. They demonstrate its superiority through visible results (better tax yield, fewer disputes, faster settlement) and lobby, publish, and litigate to have it adopted upward. Their leverage is evidence, not force; exit from the climb means abandoning decades of accumulated demonstration.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_reform_networks, agenda_setter,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_reform_networks, beneficiary).

% Adopt the fringe commitment early, running the pilot that produces the demonstration data. They gain reputational and career advantage if the climb succeeds, but bear the risk of being the visible outlier if the apex ultimately rejects the innovation and they must revert or be disciplined.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, provincial_administrators_early_adopters, beneficiary,
    moderate, biographical, constrained, regional).

% Fund and circulate the new commitment because it lowers their transaction costs (standardized weights, enforceable contracts, predictable adjudication). They can shift capital toward jurisdictions that adopt early, giving them real exit even while advocating for climb.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, merchant_guilds_backing_new_standards, beneficiary,
    organized, biographical, mobile, regional).

% Hold formal authority to ratify or block the ascending commitment. Ratification concedes that a fringe innovation outperformed the center's own arrangement, costing them prestige and sometimes patronage networks built on the old standard. They resist, stall, or co-opt credit for the innovation once resistance becomes untenable; their exit from the cost is limited because blocking indefinitely risks worse delegitimation.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, incumbent_apex_officeholders, payer,
    institutional, biographical, constrained, national).

% Guilds, faculties, or licensing colleges whose authority rested on the old commitment. When the new standard climbs and is ratified, their credentials devalue and their gatekeeping function is bypassed. They cannot exit the transition — their institutional identity is constituted by the arrangement being displaced.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, legacy_credentialing_bodies, payer,
    organized, biographical, trapped, national).

% Live under the pilot jurisdiction where the new commitment is first tested, absorbing the disruption, errors, and enforcement inconsistency of an unproven arrangement so that the fringe advocates can generate their demonstration data. They did not choose to be the proving ground and cannot leave the pilot's jurisdiction easily.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, peripheral_populations_under_forced_pilot_status, payer,
    powerless, biographical, trapped, regional).

% Record and later narrate the climb, often retroactively smoothing the contested, resisted ascent into a story of inevitable adoption by demonstrated merit — a narrative that itself becomes evidence cited in future climbs.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, court_chroniclers_and_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_reform_networks).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a channel by which genuinely superior administrative, legal, or commercial techniques discovered at the periphery can be tested at low central risk and adopted centrally once proven — avoiding the cost of centrally mandating untested reforms across an entire polity at once.
% TRANSFER_FUNCTION: Moves legitimacy and eventually formal authority from the center to the periphery-originated commitment, while moving the burden of proof, disruption, and pilot-phase instability from the center onto the peripheral population and the early-adopting administrators who run the demonstration.
% ABSENT_VOICES: The peripheral populations who bore the pilot's instability are rarely consulted on whether the demonstration was worth the disruption; their experience is folded into aggregate statistics (tax yield, dispute rates) used by fringe advocates and eventually cited by the ratifying center, without their testimony entering the legitimation record.
% DISAPPEARANCE_RATIONALE: If the endogenous-climb pathway vanished, fringe innovations would have no route to central legitimacy short of exogenous imposition or violent rupture; provincial administrators would lose their primary career ladder, merchant guilds would lose leverage over standard-setting, and incumbent apex officeholders would face far less pressure to adapt — the overall rate of institutional innovation would likely slow and shift toward top-down mandate instead.
% FOUNDING_PROBLEM: Centralized authorities historically lacked the local knowledge and low-risk testing ground to identify which administrative or legal innovations would actually work before committing the whole polity to them; the climb mechanism let promising innovations be vetted at the margins before central adoption.
% FOUNDING_PROBLEM_CORROBORATION: Provincial administrators and merchant guilds attest the climb mechanism remains functionally necessary — they cite ongoing cases where fringe-tested reforms outperform centrally designed ones. Legacy credentialing bodies and some independent historians of administrative law attest the mechanism has become a legitimation ritual: apex actors increasingly claim credit for reforms they resisted for decades, using the climb narrative to obscure that the actual adoption decision was driven by elite recalculation of advantage rather than the demonstrated evidence itself.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).
:- end_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) and rising over the interval reflect that even a genuinely merit-driven climb imposes real, growing costs — chiefly on the pilot populations and displaced credentialing bodies — as the mechanism matures and gets used more deliberately by fringe actors who learn to instrumentalize the pathway. Suppression is high early (0.65) because apex resistance is strongest at the outset, then falls (0.48) as the mechanism becomes an accepted route and resistance ritualizes rather than blocks. Theater ratio rises modestly (0.10→0.28) as the climb narrative becomes something apex actors perform retroactive credit-claiming around, rather than a live contest each time.
 *
 * PERSPECTIVAL GAP:
 *   From the fringe reform network's seat, the mechanism looks like a fair, evidence-based route to legitimacy — a rope. From the peripheral pilot population's seat, the same mechanism looks like being volunteered, without consent, as the proving ground for someone else's career advancement — closer to a snare. The engine should compute these divergent seat-level types from the structural power/exit data; the story's claimed type (tangled_rope) names the genuine coexistence of both functions rather than resolving the divergence in either direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Fringe reform networks and their early-adopter allies sit near the beneficiary end: they gain legitimacy and eventually authority through a process they control the timing and framing of. Peripheral pilot populations sit at the full-target end: they bear disruption they did not choose and whose benefits (if the reform succeeds) accrue to the advocates' reputations and the eventual central administration, not necessarily to themselves in proportion. Incumbent apex officeholders and legacy credentialing bodies are payers of a different kind — they pay in prestige, authority, and institutional relevance rather than material extraction, but the payment is real and the mechanism requires their eventual, if reluctant, capitulation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (centralized authorities lacking local knowledge to vet reforms before full commitment) remains partly live — provincial administrators and guilds still cite functional necessity — but is contested by legacy credentialing bodies and some historians who see the climb narrative increasingly used to launder elite recalculation as merit-demonstrated adoption. Classifying this as tangled_rope rather than snare or rope prevents both errors: treating it as pure coordination would erase the real cost borne by pilot populations and displaced credentialing bodies; treating it as pure extraction would erase the genuine information-discovery function fringe demonstration provides, which merchant guilds and early adopters actually rely on and would lose if the pathway closed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_climb_vs_retroactive_narrative,
    'Is the endogenous-climb pathway a genuine causal mechanism by which fringe-demonstrated superiority drives central adoption, or is it substantially a retroactive legitimating narrative applied after adoption decisions actually driven by elite recalculation of advantage?',
    'Comparative historical analysis tracing the actual sequence of documented apex deliberation against the timing and content of fringe demonstration data — does ratification track the evidence, or does the evidence get selectively cited after a decision made on other grounds?',
    'If substantially retroactive narrative, the claimed_type should shift toward snare (the coordination story is cover for what is actually an elite-driven, unevenly evidenced process); if the mechanism is genuinely evidence-driven, tangled_rope with a real coordination core is the more accurate reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_climb_vs_retroactive_narrative, conceptual, 'Whether demonstrated superiority causally drives adoption or is cited after the fact.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the endogenous_climb, exogenous_imposition, and hybrid_cascade readings disagree — is it about factual origin point (where the commitment first appears), about causal direction (who moves whom), or about which seat bears the burden of legitimation proof?',
    'Case-by-case historical coding of specific installation episodes against all three readings'' predicted signatures (origin locus, adoption curve shape, location of resistance) to see whether episodes cluster cleanly or require hybrid coding.',
    'If episodes cluster cleanly by reading, the three constraints describe genuinely distinct historical mechanisms operating in different cases; if most episodes require hybrid coding, the three readings may be describing phases of a single underlying process rather than alternative mechanisms, which would argue for a different decomposition than three parallel siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating the structural disagreement among the kernel''s three sibling readings.').

omega_variable(
    pilot_population_consent_absence,
    'Peripheral pilot populations bear disruption without any documented mechanism for consent or exit — is this absence itself evidence that the climb mechanism''s coordination framing understates its extractive character, or is it simply an artifact of premodern record-keeping that would show consent mechanisms if better sources existed?',
    'Archival search for petitions, complaints, or local records from pilot jurisdictions contemporaneous with the piloted reform, to establish whether consent or objection channels existed and were used.',
    'If genuine consent/objection channels existed and were exercised, resistance and directionality for this group should shift toward moderate rather than fully targeted; if no such channels existed, the current trapped/powerless characterization is conservative, not overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pilot_population_consent_absence, empirical, 'Whether pilot populations had any real voice in their own use as demonstration grounds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__endogenous_climb_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t10, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(stat_tr_t30, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(stat_tr_t50, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(stat_tr_t60, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(stat_be_t10, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 10, 0.27).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(stat_be_t30, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(stat_be_t50, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(stat_be_t60, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(stat_su_t10, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(stat_su_t30, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(stat_su_t50, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(stat_su_t60, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__endogenous_climb_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__endogenous_climb_reading, 0.12).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the state_commitment_installation_mechanism kernel, decomposed per the ε-invariance principle because the natural-language concept ('how do new state commitments gain legitimacy') covers structurally distinct claims about origin point and directionality. endogenous_climb_reading (this file): fringe-originated, gradually ascending, apex-resisted, ε=0.42, tangled_rope. exogenous_imposition_reading (sibling): center-originated, mandate-installed, no fringe beneficiary class, expected higher initial suppression and different victim set. hybrid_cascade_reading (sibling): apex-originated but requiring downstream fringe validation to stabilize — a structurally distinct dependency direction. All three are linked via affects_constraints; contamination or drift in one reading's empirical support should be checked against the others as competing historical hypotheses about the same episodes, not averaged into a single account.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
