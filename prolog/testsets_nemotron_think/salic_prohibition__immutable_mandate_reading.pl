% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__immutable_mandate_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Salic Law as Irrevocable Natural/Divine Law Embedded in Dynastic Constitution
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This reading instantiates the 'immutable mandate' interpretation of the
 *   Salic prohibition kernel. It asserts that the exclusion of female heirs
 *   from the French crown is not a revocable statute but an irrevocable
 *   natural/divine law embedded in the very constitution of the monarchy. The
 *   reading claims the constraint emerges naturally from the order of
 *   creation and cannot be altered by any sovereign act. Structurally, this
 *   is a Mountain claim (emerges_naturally: true) with high
 *   accessibility_collapse (0.88) and low resistance (0.45) — the law
 *   presents itself as immutable fact. However, the authored metrics reveal
 *   substantial extractiveness (0.72) and suppression (0.85): female heirs
 *   are categorically stripped of inheritance rights, their supporters face
 *   treason, and preventive wars are waged to enforce the exclusion.
 *   Beneficiaries (agnatic male heirs, collateral nobility, dynastic
 *   establishment) are identifiable and concentrated. This divergence between
 *   claim (mountain) and metrics (extractive, enforced, beneficiary-bearing)
 *   is the False Summit signature: a constructed constraint masquerading as
 *   natural law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.72).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.85).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, mountain).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Law as Irrevocable Natural/Divine Law Embedded in Dynastic Constitution").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).
domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, '2b533048-7b6d-47e3-9fa0-861428d3c84d').
narrative_ontology:cs_kernel_codification('2b533048-7b6d-47e3-9fa0-861428d3c84d', fixed_text).
narrative_ontology:cs_authority_grounding('2b533048-7b6d-47e3-9fa0-861428d3c84d', lineage).
narrative_ontology:cs_interpretation_layer_present('2b533048-7b6d-47e3-9fa0-861428d3c84d').
narrative_ontology:cs_reading_relation('2b533048-7b6d-47e3-9fa0-861428d3c84d', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('2b533048-7b6d-47e3-9fa0-861428d3c84d', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('2b533048-7b6d-47e3-9fa0-861428d3c84d', foundational, female_exclusion_necessary_universal_unalterable).
narrative_ontology:cs_axiom_status(female_exclusion_necessary_universal_unalterable, holdable).
narrative_ontology:cs_axiom_grounding('2b533048-7b6d-47e3-9fa0-861428d3c84d', female_exclusion_necessary_universal_unalterable, theological).
narrative_ontology:cs_axiom('2b533048-7b6d-47e3-9fa0-861428d3c84d', secondary, preventive_war_legitimate_for_agnatic_priority).
narrative_ontology:cs_axiom_status(preventive_war_legitimate_for_agnatic_priority, holdable).
narrative_ontology:cs_axiom_grounding('2b533048-7b6d-47e3-9fa0-861428d3c84d', preventive_war_legitimate_for_agnatic_priority, deontological).
narrative_ontology:cs_axiom('2b533048-7b6d-47e3-9fa0-861428d3c84d', foundational, salic_law_as_divine_constitutional_bedrock).
narrative_ontology:cs_axiom_status(salic_law_as_divine_constitutional_bedrock, overridden).
narrative_ontology:cs_axiom_grounding('2b533048-7b6d-47e3-9fa0-861428d3c84d', salic_law_as_divine_constitutional_bedrock, theological).
narrative_ontology:cs_reference_frame('2b533048-7b6d-47e3-9fa0-861428d3c84d', capetian_agnatic_primogeniture).
narrative_ontology:cs_drift_state('2b533048-7b6d-47e3-9fa0-861428d3c84d', post_1830_july_monarchy, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('2b533048-7b6d-47e3-9fa0-861428d3c84d', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_male_heirs).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_collateral_nobility).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, dynastic_establishment).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_dynastic_heirs).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_claimants).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, supporters_of_female_succession).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, divine_ordination_of_agnatic_succession).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, inalienability_of_crown_from_female_line).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, legitimacy_of_preventive_war_for_succession_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stand first in line to inherit the crown by virtue of male lineage. Their claim is treated as natural right rather than political arrangement. Exit from this identity would mean renouncing dynastic birthright itself — professionally and existentially impossible within the system.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_male_heirs, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__immutable_mandate_reading, agnatic_male_heirs, agenda_setter).

% Noble houses whose inheritance customs mirror agnatic priority. They benefit from the exclusion of female heirs because it preserves their own patrimonial structures and prevents crown lands from passing to foreign houses through female marriage. Their exit would require restructuring feudal tenure itself.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_collateral_nobility, beneficiary,
    organized, generational, constrained, national).

% The royal court, parlements, and administrative bodies that interpret and enforce the succession law. They administer the constraint and derive institutional authority from being its guardians. They can pivot to alternative readings when politically expedient (as seen in 1316, 1328, 1589, 1713).
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, dynastic_establishment, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Princesses and queens-regnant-in-waiting categorically excluded from succession regardless of proximity of blood. Their claims are delegitimized by the law itself. No legal exit exists within the system — only marriage to foreign rulers or renunciation.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_dynastic_heirs, payer,
    moderate, biographical, trapped, national).

% Male heirs through female lines (e.g., Edward III of England, Philip V of Spain) whose claims are voided by the Salic prohibition. They possess military resources and foreign alliances but must wage preventive war to press claims the law declares illegitimate.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, cognatic_claimants, payer,
    powerful, generational, constrained, continental).

% Nobles, jurists, and urban elites who argue for cognatic succession based on feudal custom, Roman law, or natural equity. They are excluded from the official interpretive community and face treason charges for advocating female inheritance.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, supporters_of_female_succession, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__immutable_mandate_reading, supporters_of_female_succession, excluded).

% Rulers of other realms who must decide whether to recognize the Salic exclusion when it affects their dynastic marriages and treaty obligations. They observe the constraint's operation from outside and can exploit succession crises.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, foreign_sovereigns, observer,
    powerful, generational, arbitrage, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a deterministic, uncontestable succession rule that prevents disputed successions and civil war by fixing inheritance to the male line alone.
% TRANSFER_FUNCTION: Moves the crown and all its appurtenances (territory, revenue, legitimacy, military command) from female heirs and their descendants to the nearest male agnate, enforced by the threat and practice of preventive war.
% ABSENT_VOICES: Female heirs themselves, whose voices are structurally silenced by the law that declares their exclusion a natural fact rather than a political choice. Also excluded: jurists of the Roman law tradition who maintained female succession rights, and the estates of regions (Navarre, Brittany, Champagne) where female succession was customary.
% DISAPPEARANCE_RATIONALE: If the Salic prohibition vanished overnight, the succession would immediately open to female heirs and their descendants, rewriting the dynastic map of Western Europe. The Hundred Years' War, the War of Spanish Succession, the Carlist Wars, and the July Monarchy's legitimacy crisis all trace to this constraint's operation. The world rearranges because the constraint actively structures who holds sovereign power.
% FOUNDING_PROBLEM: The extinction of the direct Capetian male line in 1316 created a succession vacuum. The Salic Law (a Frankish tribal code governing movable property) was invoked to exclude Joan of Navarre and later Edward III of England, establishing a French succession principle that prioritized agnatic proximity over proximity of blood.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Capetian extinction) was a specific historical contingency resolved within one generation. The constraint persisted for 500+ years after the problem vanished. Contemporary jurists (Du Tillet, Loyseau) and later historians (Viard, Lot) attest the Salic Law was a Frankish customary rule never intended for crown succession. The Parlement of Paris' 1317 registration explicitly cites 'ancient custom' not divine law. No corroborating source outside the French royal establishment treats the prohibition as natural/divine rather than positive law.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, ExtMetricName, E),
    domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.72) reflects the total transfer of sovereign power from female line to male line across centuries — the crown itself is the extracted asset. Suppression (0.85) is high because the constraint's persistence depends on active enforcement: the 1316/1328 exclusions, the Hundred Years' War (framed as enforcing Salic law), the Treaty of Troyes repudiation, the 1713 renunciation wars, the 1830 Carlist trigger. Theater_ratio (0.25) is moderate-low because the coordination function (succession certainty) is real but increasingly decoupled from the extraction — by 1713 the law serves Bourbon dynastic interest more than succession stability. Accessibility_collapse (0.88) is very high: once the law is accepted as natural/divine, alternatives (cognatic succession, election, designation) become unthinkable within the system. Resistance (0.45) is moderate: cognatic claimants wage war, jurists write counter-treatises, but the constraint's framing as natural law makes resistance appear as rebellion against nature.
 *
 * PERSPECTIVAL GAP:
 *   From the agnatic heir's seat, the constraint is a Mountain — the natural order of succession, providing certainty and legitimacy. From the female heir's seat, it is a Snare — total extraction with no exit, enforced by the threat of bastardy and treason charges. From the dynastic establishment's seat, it oscillates: Mountain when it secures their patron's line, Tangled Rope when they must enforce it against powerful cognatic claimants (Edward III, Philip V), Scaffold when they manage transitions (1589, 1713, 1830). The engine computes this divergence from the structural data; the authored claim (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic male heirs are full beneficiaries (d ≈ 0.1): they receive the crown without competition from half the dynasty. Collateral nobility are beneficiaries (d ≈ 0.2): their inheritance customs are validated and protected. Dynastic establishment are agenda_setters with arbitrage exit (d ≈ 0.15): they administer the law but can pivot readings when power shifts. Female heirs are trapped payers (d ≈ 0.95): no exit, total exclusion. Cognatic claimants are constrained payers (d ≈ 0.8): they have military power but must fight the law's legitimacy. Supporters of female succession are constrained payers/excluded (d ≈ 0.7): they can argue but face structural exclusion. Foreign sovereigns are analytical observers (d ≈ 0.5): they observe and exploit but don't directly bear the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Capetian male line extinction) was a one-time contingency in 1316. The constraint persisted for five centuries after the problem was resolved, expanding from a French royal custom to a claimed universal principle of dynastic legitimacy. The mandate atrophied into a tool for excluding rival claimants (especially Habsburg and Bourbon cognatic heirs) and justifying preventive war. The constraint became a Piton in its late phase (post-1713): the coordination function (succession certainty) was real but the extraction (permanent exclusion of female line) far exceeded coordination needs. The 1830 Carlist crisis shows the constraint's theatrical maintenance — the law was invoked to justify civil war over a succession the establishment had already pragmatically resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the Salic prohibition a genuine natural/divine law that would persist regardless of human enforcement, or a constructed constraint that benefits identifiable agents (agnatic heirs, collateral nobility, dynastic establishment)?',
    'Comparative analysis: if the constraint operates identically across cultures without contact (e.g., Japanese imperial succession, Hawaiian kapu system), natural law gains support. If it tracks dynastic interest (invoked 1316, 1328, 1589, 1713, 1830 only when male line is threatened), construction gains support.',
    'If natural law, the Mountain claim holds and FSM does not fire. If constructed, FSM reclassifies to tangled_rope (coordination + asymmetric extraction with active enforcement). The beneficiaries array and high suppression/enforcement measurements strongly suggest construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Whether the constraint''s natural-law framing is structural fact or ideological cover for extraction.').

omega_variable(
    salic_law_original_scope,
    'Did the historical Lex Salica (Frankish tribal code, c. 500 CE) actually govern crown succession, or was it a 14th-century juristic invention projecting a property rule onto the monarchy?',
    'Philological and diplomatic analysis of the Lex Salica manuscripts vs. the 1316-1328 Parlement registrations. The Lex Salica governs inheritance of allodial land among free Franks; no manuscript extends it to the crown.',
    'If the Lex Salica never governed succession, the ''ancient custom'' claim is fabricated, confirming the constraint as a 14th-century construction. This would strengthen the FSM case and the sovereign_override_reading''s historical premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(salic_law_original_scope, empirical, 'Whether the historical Salic Law ever applied to royal succession.').

omega_variable(
    coordination_extraction_boundary_shift,
    'At what point did the constraint''s coordination function (succession certainty) become subordinate to its extraction function (permanent agnatic monopoly)?',
    'Track the theater_ratio and suppression_requirement measurements: rising theater with stable coordination necessity indicates extraction dominance. The 1713-1789 period (Bourbon family compacts, Pragmatic Sanction workarounds) is the critical test.',
    'If coordination remained primary, the constraint stays closer to rope/tangled_rope. If extraction dominated by 1713, the constraint is a piton in its terminal phase — maintained theatrically for dynastic interest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_shift, empirical, 'Whether and when the constraint''s primary function shifted from coordination to extraction.').

omega_variable(
    committer_frame_disagreement_location,
    'Where exactly do the three readings of the salic_prohibition kernel structurally disagree?',
    'Map each reading''s structural commitments: immutable_mandate fixes female_exclusion = necessary + universal + unalterable. sovereign_override fixes female_exclusion = contingent + revisable_by_sovereign. cognatic_reversion fixes female_exclusion = inapplicable_outside_Frankish_core. The disagreement is on the modal status of the exclusion (necessary vs. contingent vs. inapplicable) and its spatial scope (universal vs. territorial).',
    'Clarifies that the kernel contest is not about ''what the law says'' but about the modal force and spatial reach of the exclusion principle. Each reading generates a different constraint with different ε, beneficiaries, and enforcement logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_disagreement_location, conceptual, 'Structural locus of disagreement among the three kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 1316, 1830).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salic_immutable_tr_t1316, salic_prohibition__immutable_mandate_reading, theater_ratio, 1316, 0.15).
narrative_ontology:measurement(salic_immutable_tr_t1422, salic_prohibition__immutable_mandate_reading, theater_ratio, 1422, 0.18).
narrative_ontology:measurement(salic_immutable_tr_t1516, salic_prohibition__immutable_mandate_reading, theater_ratio, 1516, 0.22).
narrative_ontology:measurement(salic_immutable_tr_t1593, salic_prohibition__immutable_mandate_reading, theater_ratio, 1593, 0.25).
narrative_ontology:measurement(salic_immutable_tr_t1713, salic_prohibition__immutable_mandate_reading, theater_ratio, 1713, 0.3).
narrative_ontology:measurement(salic_immutable_tr_t1830, salic_prohibition__immutable_mandate_reading, theater_ratio, 1830, 0.25).

% Extraction over time
narrative_ontology:measurement(salic_immutable_be_t1316, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1316, 0.55).
narrative_ontology:measurement(salic_immutable_be_t1422, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1422, 0.62).
narrative_ontology:measurement(salic_immutable_be_t1516, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1516, 0.68).
narrative_ontology:measurement(salic_immutable_be_t1593, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1593, 0.7).
narrative_ontology:measurement(salic_immutable_be_t1713, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1713, 0.75).
narrative_ontology:measurement(salic_immutable_be_t1830, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1830, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(salic_immutable_su_t1316, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1316, 0.7).
narrative_ontology:measurement(salic_immutable_su_t1422, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1422, 0.78).
narrative_ontology:measurement(salic_immutable_su_t1516, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1516, 0.82).
narrative_ontology:measurement(salic_immutable_su_t1593, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1593, 0.85).
narrative_ontology:measurement(salic_immutable_su_t1713, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1713, 0.88).
narrative_ontology:measurement(salic_immutable_su_t1830, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1830, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(salic_prohibition__immutable_mandate_reading, 0.1).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__cognatic_reversion_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, pragmatic_sanction_1713).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, carlist_succession_wars).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, french_revolutionary_succession_law).

% DUAL FORMULATION NOTE:
% This reading and its two siblings form the salic_prohibition constraint family. The immutable_mandate_reading claims the exclusion is a necessary natural law (mountain claim, high ε). The sovereign_override_reading claims it is revisable positive law (rope/tangled_rope claim, lower ε). The cognatic_reversion_reading claims it is territorially limited Frankish custom (snare claim when imposed beyond Frankish core, different ε). Their ε values differ because their referents differ: universal natural law vs. national positive law vs. territorial custom. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__immutable_mandate_reading, institutional, 0.15).
constraint_indexing:directionality_override(salic_prohibition__immutable_mandate_reading, powerful, 0.8).
constraint_indexing:directionality_override(salic_prohibition__immutable_mandate_reading, moderate, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
