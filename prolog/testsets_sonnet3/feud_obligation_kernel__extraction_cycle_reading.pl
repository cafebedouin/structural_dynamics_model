% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Kin-Group Blood Feud as Destructive Extraction Cycle
 *   domain: legal_anthropology/medieval_history/political_systems
 *
 * SUMMARY:
 *   This story reads the medieval kin-feud obligation not as coordination or
 *   as violation of divine law, but as a self-perpetuating extraction cycle:
 *   each retaliatory killing obligates further retaliation, consuming labor,
 *   livestock, and lives across generations while structurally preventing the
 *   territorial and administrative consolidation that would otherwise occur.
 *   Under this reading, feuding kin groups and their dependents are the
 *   victim set (depleted productive capacity, mortality, reproductive
 *   burden), while emerging royal authority is the beneficiary set — not
 *   because the crown orchestrates the feuds, but because it profits
 *   structurally from the exhaustion of rival kin-based power bases and later
 *   installs courts and taxation in the vacuum feud depletion leaves behind.
 *   This is a reading of the feud_obligation_kernel distinct from the
 *   stateless_coordination_reading (which treats the same practice as solving
 *   a genuine deterrence problem) and the christianized_pacification_reading
 *   (which treats it as violating a divinely grounded monopoly on legitimate
 *   violence). All three share the kernel — the standing practice of
 *   kin-based retaliatory obligation — but assign it different
 *   beneficiary/victim structures and different ε.
 *
 * KEY AGENTS:
 *   - feuding_kin_groups: primary target (moderate/identity_locked) — depleted productive capacity, mortality
 *   - agricultural_dependents: secondary target (powerless/trapped) — bear feud costs without honor stake
 *   - women_married_into_feuding_lineages: secondary target (powerless/trapped) — bear reproductive and loss costs without voice
 *   - emerging_royal_authority: structural beneficiary (institutional/arbitrage) — gains from kin exhaustion
 *   - weapon_and_wergild_intermediaries: incidental beneficiary (moderate/mobile) — profits from transaction volume
 *   - royal_courts_and_church_peace_movements: excluded alternative (organized/constrained) — offers arbitration, sidelined by honor logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.71).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.58).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Kin-Group Blood Feud as Destructive Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/medieval_history/political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, '73be6763-c1dc-440f-8ce1-4af3887e3de8').
narrative_ontology:cs_kernel_codification('73be6763-c1dc-440f-8ce1-4af3887e3de8', implicit).
narrative_ontology:cs_authority_grounding('73be6763-c1dc-440f-8ce1-4af3887e3de8', practice).
narrative_ontology:cs_interpretation_layer_present('73be6763-c1dc-440f-8ce1-4af3887e3de8').
narrative_ontology:cs_reading_relation('73be6763-c1dc-440f-8ce1-4af3887e3de8', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('73be6763-c1dc-440f-8ce1-4af3887e3de8', feud_obligation_kernel__christianized_pacification_reading, influences).
narrative_ontology:cs_axiom('73be6763-c1dc-440f-8ce1-4af3887e3de8', foundational, feud_persistence_beyond_functional_need_is_extractive).
narrative_ontology:cs_axiom_status(feud_persistence_beyond_functional_need_is_extractive, holdable).
narrative_ontology:cs_axiom_grounding('73be6763-c1dc-440f-8ce1-4af3887e3de8', feud_persistence_beyond_functional_need_is_extractive, empirically_contingent).
narrative_ontology:cs_axiom('73be6763-c1dc-440f-8ce1-4af3887e3de8', secondary, royal_consolidation_benefits_from_kin_exhaustion_independent_of_intent).
narrative_ontology:cs_axiom_status(royal_consolidation_benefits_from_kin_exhaustion_independent_of_intent, holdable).
narrative_ontology:cs_axiom_grounding('73be6763-c1dc-440f-8ce1-4af3887e3de8', royal_consolidation_benefits_from_kin_exhaustion_independent_of_intent, empirically_contingent).
narrative_ontology:cs_reference_frame('73be6763-c1dc-440f-8ce1-4af3887e3de8', kin_honor_retaliatory_obligation).
narrative_ontology:cs_drift_state('73be6763-c1dc-440f-8ce1-4af3887e3de8', post_royal_court_availability, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('73be6763-c1dc-440f-8ce1-4af3887e3de8', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, emerging_royal_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, weapon_and_wergild_intermediaries).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feuding_kin_groups).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, agricultural_dependents).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, women_married_into_feuding_lineages).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by kinship honor codes to avenge killings against agnatic relatives or accept humiliating settlement. Each retaliatory killing obligates the opposing lineage to retaliate in turn, consuming male labor, livestock paid as compensation, and land left fallow while men are absent, dead, or in hiding. Exit means abandoning kin identity and protection entirely — a cost most cannot bear.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feuding_kin_groups, payer,
    moderate, generational, identity_locked, regional).

% Tenants, laborers, and smallholders attached to feuding lineages who have no honor stake in the feud but whose fields go unworked, whose granaries are raided as feud tactics, and who are conscripted into kin militias. They cannot leave the land without losing subsistence.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, agricultural_dependents, payer,
    powerless, biographical, trapped, local).

% Married across lineage lines as alliance instruments, they are frequently caught between natal and marital kin obligations during feud escalation, lose husbands and sons to retaliatory killing, and bear the reproductive cost of replacing depleted male kin without any voice in whether the feud continues.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, women_married_into_feuding_lineages, payer,
    powerless, biographical, trapped, local).

% A consolidating crown observes the feud cycle destroying rival kin-based power bases, depopulating and impoverishing regions it wishes to tax and administer directly. It intervenes selectively — offering royal courts, wergild schedules, and peace ordinances — positioning itself as the alternative to feud violence, and uses the resulting dependency to install royal officials, courts, and tax assessors where kin authority previously governed. The crown gains whether it suppresses feuds successfully or lets them run long enough to exhaust rival lineages first.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, emerging_royal_authority, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, emerging_royal_authority, agenda_setter).

% Smiths, livestock brokers, and compensation negotiators who profit from arming feuding parties and brokering wergild settlements. Each cycle of violence and settlement generates transaction volume for them regardless of who wins.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, weapon_and_wergild_intermediaries, beneficiary,
    moderate, biographical, mobile, regional).

% Ecclesiastical peace-of-God movements and early royal justices attempt to insert arbitration and truce mechanisms but are frequently sidelined by kin groups who regard settlement imposed from outside as dishonorable capitulation; their voice enters the historical record mainly as complaint about feud persistence rather than as parties inside the feud logic itself.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_courts_and_church_peace_movements, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuine at the kin level under this reading — the retaliatory structure is read as a self-perpetuating extraction mechanism rather than a solved coordination problem; any deterrence effect is incidental to the cycle's persistence, not its purpose.
% TRANSFER_FUNCTION: Moves labor-years, livestock, land productivity, and lives from feuding lineages and their dependents into (a) destroyed capital that benefits no one, and (b) the political vacuum that royal authority fills as depleted kin groups become unable to resist centralized taxation and jurisdiction.
% ABSENT_VOICES: Agricultural dependents and married-in women bear the heaviest depletion costs but have no standing in feud honor logic to end it; royal courts and peace movements are structurally present as alternatives but excluded from feud decision-making until kin groups are exhausted enough to accept external arbitration.
% DISAPPEARANCE_RATIONALE: If feud obligation vanished overnight, kin lineages would retain productive capacity and population currently lost to retaliatory killing and land abandonment, regional power would remain distributed among intact kin networks rather than consolidating toward the crown, and the principal lever emerging monarchies used to install royal courts and taxation over the resulting vacuum would disappear.
% FOUNDING_PROBLEM: In the absence of a credible third-party enforcer, kin groups needed SOME mechanism to make violence against their members costly to the perpetrator's lineage — the feud began as a deterrence and restitution structure in a stateless setting.
% FOUNDING_PROBLEM_CORROBORATION: Royal chroniclers and church peace-movement records from the period attest that feud violence had, by the era in question, become self-sustaining honor competition disconnected from proportional deterrence — chronicled complaints about escalating cycles unresponsive to wergild settlement come from clerical and royal administrative sources outside the feuding lineages themselves, not from the kin groups who continued to assert the practice's necessity.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises across the interval (0.42 to 0.71) as this reading models feud intensity escalating from sporadic vengeance killing to entrenched multi-generational cycles — the depletion compounds as lineages lose successive rounds of productive-age men and retaliate further to avoid appearing weak. Suppression is substantial but not extreme (0.58 at end) because the enforcement mechanism is internal to kin honor codes rather than externally coerced — the suppression figure here captures how completely kin identity forecloses exit from the obligation, not state coercion. Theater ratio stays low throughout (0.22 at end) because under this reading almost none of the activity is performative — the killings, compensation payments, and land abandonment are real costs, not symbolic maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the royal authority's seat, the same feud practice reads as a governance problem to be managed opportunistically — coordination failure among subjects that clears the field for centralized administration. From the feuding kin groups' own seat under the stateless_coordination_reading (a sibling constraint, not this one), the identical practice would read as functional deterrence. This story authors only the extraction-cycle seat; the engine's per-seat computation on THIS story's structural data should show feuding kin groups and dependents computing as victims of a snare-like structure while royal authority computes as a clear beneficiary, without importing the sibling readings' logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Feuding kin groups and their dependents are declared victims because the extraction (lost labor, lost lives, lost land productivity) flows out of their structural position with no comparable return — their exit options are identity_locked or trapped, pushing their derived directionality toward the full-target end. Royal authority is declared beneficiary because it gains politically and fiscally from kin exhaustion regardless of whether it actively suppresses feuds or merely waits them out — its arbitrage-grade exit options and institutional power place it near the full-beneficiary end. Weapon and wergild intermediaries are a secondary, incidental beneficiary class: they do not orchestrate the cycle but profit from its transaction volume.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (deterring violence against kin absent a third-party enforcer) is read here as dead by the time this constraint is evaluated — royal courts and wergild schedules had begun offering a substitute enforcement mechanism, yet feud obligation persisted past the point of net collective benefit, consuming resources royal administration could otherwise tax and organize. This is exactly the mandatrophy pattern: an arrangement whose founding function has been superseded but which persists because it is embedded in kin identity and honor, not because it still solves the problem it was built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feud_obligation_kernel_reading_disagreement_location,
    'Is the blood-feud obligation better modeled as (a) a self-enforcing coordination mechanism functional in a stateless setting, (b) a destructive extraction cycle exploited by emerging royal power, or (c) a violation of divinely grounded legitimate-violence authority requiring ecclesiastical/royal correction?',
    'Comparative analysis of feud frequency and intensity before and after the introduction of viable third-party enforcement (royal courts, church peace movements): if feud intensity declines sharply once credible alternatives exist, this favors reading (b)/(c) over (a); if feud intensity remains stable regardless of alternative enforcement availability, this favors reading (a).',
    'Each reading assigns a different beneficiary/victim structure to the same standing kernel — the extraction_cycle_reading (this story) places royal authority as beneficiary and feuding kin groups as victims; the stateless_coordination_reading would classify feuding kin groups as net beneficiaries of a functional deterrence system; the christianized_pacification_reading would frame royal/ecclesiastical intervention as restoring legitimate authority rather than opportunistic extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feud_obligation_kernel_reading_disagreement_location, conceptual, 'Sibling readings of feud_obligation_kernel disagree about whether the feud practice is functional coordination, extraction, or a legitimacy violation — this omega documents where that disagreement is structurally located.').

omega_variable(
    royal_intent_vs_structural_benefit,
    'Did emerging royal authorities actively engineer or prolong feud conditions to weaken rival kin power, or did they merely benefit passively from a cycle they did not cause and eventually suppressed in good faith?',
    'Archival examination of royal correspondence, edicts, and court records for evidence of deliberate non-intervention or selective enforcement timed to kin exhaustion, versus records showing consistent early attempts at peace-imposition regardless of political cost to the crown.',
    'If deliberate, royal authority''s beneficiary status strengthens toward active extraction (supporting snare/tangled_rope classification with intent); if passive, the beneficiary relationship is structural but not exploitative in intent, which would soften the extraction framing without changing the measured outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(royal_intent_vs_structural_benefit, empirical, 'Whether royal benefit from feud depletion was engineered or incidental.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the suppression that keeps kin groups inside the feud obligation primarily structural (real material consequences of refusing to retaliate — loss of alliance, land claims, protection) or internalized (honor-identity fusion making exit psychologically unthinkable even where material alternatives exist)?',
    'Examine cases where material alternatives to retaliation existed (wergild offers, royal arbitration availability) and were refused: refusal despite available material alternative indicates internalized suppression; acceptance when alternatives exist indicates primarily structural suppression.',
    'If suppression is substantially internalized, the effective suppression driving the cycle''s persistence is higher than the structural measure alone suggests, and exit remains costly even after royal alternatives become materially available — extending the extraction cycle''s duration beyond what material conditions alone would predict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural versus internalized suppression in kin honor-obligation exit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(feud_tr_t60, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(feud_tr_t80, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(feud_be_t60, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement(feud_be_t80, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 100, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(feud_su_t60, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 60, 0.51).
narrative_ontology:measurement(feud_su_t80, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__extraction_cycle_reading, 0.1).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of feud_obligation_kernel, decomposed per the ε-invariance principle because the natural-language label 'blood feud' conflates structurally distinct claims with different ε values: the stateless_coordination_reading treats the practice as functional deterrence (low extraction, feuding kin groups as beneficiaries of a working system); the christianized_pacification_reading treats it as a legitimacy violation corrected by divinely-grounded authority (extraction framed as sin/disorder rather than royal opportunism); this extraction_cycle_reading treats it as a destructive cycle exploited by emerging state power (high extraction, feuding kin groups and dependents as victims, royal authority as beneficiary). All three describe the same standing kin-obligation practice but assign incompatible beneficiary/victim structures and different degrees of ε, hence three separate stories linked via affects_constraints rather than one story with an averaged or measurement-dependent ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
