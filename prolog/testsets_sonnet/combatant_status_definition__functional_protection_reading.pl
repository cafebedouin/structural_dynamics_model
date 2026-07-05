% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__functional_protection_reading, []).

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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Common Article 3 Status-Independent Minimum Protections
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This story instantiates the functional-protection reading of the
 *   contested combatant status kernel: Common Article 3 to the Geneva
 *   Conventions establishes that all persons detained in connection with
 *   armed conflict — regardless of whether they qualify as lawful combatants,
 *   POWs, or unprivileged belligerents — receive an irreducible floor of
 *   humane treatment, prohibition on violence and outrages upon dignity, and
 *   minimum judicial guarantees. This reading removes status determination as
 *   a precondition for baseline treatment: the floor attaches at capture, not
 *   after classification. The sibling readings (state_centric_reading,
 *   national_liberation_reading) address a different structural question
 *   entirely — who qualifies for full combatant/POW status and its associated
 *   privileges — and are NOT part of this constraint's own operation. This
 *   story's ε is low and stable because the functional floor is genuinely
 *   close to universal coordination: nearly every legal tradition and every
 *   state party has ratified Common Article 3, and its extraction profile is
 *   minimal (it restrains state power rather than transferring resources to a
 *   beneficiary class that extracts from others).
 *
 * KEY AGENTS:
 *   - all_detained_persons: primary beneficiary (powerless/trapped) — receives the protection floor regardless of status
 *   - detaining_power_military: agenda_setter (institutional/constrained) — bears the administrative and political cost of applying protections pre-classification
 *   - international_committee_of_the_red_cross: observer/agenda_setter (institutional/analytical) — monitors and asserts the floor's application
 *   - domestic_courts_and_tribunals: observer (institutional/analytical) — adjudicates treatment claims independent of status resolution
 *   - states_favoring_status_screening: excluded (powerful/mobile) — objects through the sibling state-centric reading, not through this one
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.12).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.28).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Common Article 3 Status-Independent Minimum Protections").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, '2dbbb7b9-51af-4a67-a98f-9cc247c98293').
narrative_ontology:cs_kernel_codification('2dbbb7b9-51af-4a67-a98f-9cc247c98293', fixed_text).
narrative_ontology:cs_authority_grounding('2dbbb7b9-51af-4a67-a98f-9cc247c98293', lineage).
narrative_ontology:cs_interpretation_layer_present('2dbbb7b9-51af-4a67-a98f-9cc247c98293').
narrative_ontology:cs_reading_relation('2dbbb7b9-51af-4a67-a98f-9cc247c98293', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('2dbbb7b9-51af-4a67-a98f-9cc247c98293', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('2dbbb7b9-51af-4a67-a98f-9cc247c98293', foundational, humane_treatment_precedes_status_determination).
narrative_ontology:cs_axiom_status(humane_treatment_precedes_status_determination, holdable).
narrative_ontology:cs_axiom_grounding('2dbbb7b9-51af-4a67-a98f-9cc247c98293', humane_treatment_precedes_status_determination, deontological).
narrative_ontology:cs_axiom('2dbbb7b9-51af-4a67-a98f-9cc247c98293', foundational, protection_floor_is_non_derogable_and_universal).
narrative_ontology:cs_axiom_status(protection_floor_is_non_derogable_and_universal, holdable).
narrative_ontology:cs_axiom_grounding('2dbbb7b9-51af-4a67-a98f-9cc247c98293', protection_floor_is_non_derogable_and_universal, conventional).
narrative_ontology:cs_reference_frame('2dbbb7b9-51af-4a67-a98f-9cc247c98293', geneva_1949_universal_floor_framework).
narrative_ontology:cs_drift_state('2dbbb7b9-51af-4a67-a98f-9cc247c98293', post_2001_counterterrorism_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2dbbb7b9-51af-4a67-a98f-9cc247c98293', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, all_detained_persons).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, captured_non_state_fighters).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, captured_state_combatants).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, civilian_internees).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, humane_treatment_is_status_independent).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, common_article_3_universal_floor_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held in detention during armed conflict of any classification. Under this reading, receives humane treatment, prohibition on violence to life and person, prohibition on outrages upon personal dignity, and judicial guarantees regardless of whether a tribunal ever determines their combatant status. Cannot negotiate exit from detention; the protection floor is the only leverage they hold.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, all_detained_persons, beneficiary,
    powerless, immediate, trapped, global).

% Operates detention facilities and makes day-to-day custody decisions. Under this reading, the detaining power cannot use unresolved status determination as a basis for withholding baseline humane treatment; must apply the floor immediately upon capture. Retains some latitude on classification-dependent privileges (POW combatant immunity, repatriation timing) but not on the minimum floor itself. Bears the administrative and political cost of applying protections before status is settled.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detaining_power_military, agenda_setter,
    institutional, biographical, constrained, national).

% Monitors detention conditions and asserts the Common Article 3 floor applies to everyone in the detaining power's hands, no exceptions. Has no coercive enforcement power itself but leverages visibility, reporting, and diplomatic pressure to keep detaining powers from conditioning treatment on status litigation outcomes.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, international_committee_of_the_red_cross, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, international_committee_of_the_red_cross, agenda_setter).

% Adjudicates individual habeas and treatment claims; under this reading, can order remedies for mistreatment without first resolving whether the detainee is a lawful combatant, POW, or unprivileged belligerent. Their reasoning either reinforces or erodes the status-independence premise case by case.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, domestic_courts_and_tribunals, observer,
    institutional, generational, analytical, national).

% Fighters for groups not meeting Article 4 organizational criteria, historically at greatest risk of falling into a protection gap between POW status and civilian protection. Under the functional reading, they receive the floor immediately, without waiting for a status tribunal that might otherwise never convene or might rule against them.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, captured_non_state_fighters, beneficiary,
    powerless, immediate, trapped, national).

% Would prefer combatant status be resolved before any treatment obligations attach, arguing this preserves incentives for lawful combatancy and denies legitimacy to irregular fighters. Their objection is structurally present in treaty negotiation history and in the state-centric sibling reading but is not part of the conversation this constraint's own operation instantiates — they operate through a different reading, not through modification of this one.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, states_favoring_status_screening, excluded,
    powerful, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__functional_protection_reading, diffuse).
narrative_ontology:fixing_cost_class(combatant_status_definition__functional_protection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees every detained person a floor of humane treatment and fair trial guarantees the moment they are in custody, eliminating the need to resolve contested and often unresolvable status questions before basic protections attach — solving the real coordination problem of what to do with detainees when classification is disputed, delayed, or impossible.
% TRANSFER_FUNCTION: Moves an obligation of restraint and minimum process from the detaining power onto itself: it cannot extract intelligence, punishment, or leverage through denial of basic humane treatment, regardless of who it has captured. No resource moves between private parties; the transfer is a constraint on state coercive capacity in favor of the detained individual.
% ABSENT_VOICES: States that prefer status-conditioned treatment (arguing that extending protections to fighters who don't meet formal combatant criteria removes incentives to comply with the laws of war) are structurally outside this reading's own framework — their position is expressed through the state-centric sibling reading, not through amendment of this one.
% DISAPPEARANCE_RATIONALE: If status-independence collapsed, detaining powers could lawfully withhold humane treatment and judicial guarantees pending status determination — a process states control and can indefinitely delay. Detainees currently protected by the floor (particularly non-state fighters and those in contested classification limbo) would fall into a protection gap; interrogation practices, indefinite detention without charge, and denial of judicial review would become legally available tools rather than violations.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions drafters recognized that internal and mixed conflicts produce detainees whose combatant status is genuinely contested or unresolvable, and that conditioning humane treatment on prior status resolution had produced atrocities in preceding conflicts (reprisals, denial of quarter, treatment of partisans as outside all legal protection).
% FOUNDING_PROBLEM_CORROBORATION: The ICRC, which has no direct stake in any state's detention policy outcomes, continues to document status-based treatment denial in contemporary conflicts (non-international armed conflicts, counterterrorism detention, contested-classification detainees) as an active, unresolved problem — corroboration from outside the population of protected detainees themselves, and independent of any detaining power's institutional interest.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).
:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) and essentially flat across 75 years because the functional floor does not transfer resources from a payer class to a beneficiary class in the extractive sense — it restrains the detaining power's coercive latitude, which is a cost the state bears rather than an extraction it collects. Suppression is moderate (0.28) reflecting genuine active enforcement requirement (states do not spontaneously apply the floor; ICRC monitoring, treaty ratification pressure, and judicial review are needed to keep detaining powers from conditioning treatment on status litigation). Theater ratio rises modestly (0.15 to 0.22) reflecting increasing gap between formal ratification/declaratory commitment and contested application in counterterrorism and non-international armed conflict contexts post-2001, without approaching piton-level performance.
 *
 * PERSPECTIVAL GAP:
 *   From the detaining power's seat, the floor appears as a cost imposed regardless of operational judgment about who was captured — friction against battlefield discretion. From the detained person's seat, the same floor is the only guarantee standing between them and status-conditioned treatment denial. The engine should compute both seats as experiencing a genuine coordination structure (low ε), differing mainly in how costly compliance feels rather than in whether extraction is occurring — because none is, under this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   All detained persons are beneficiaries under this reading by construction — the floor exists specifically to reach them regardless of classification, so their derived directionality sits near the full-beneficiary end even though they are individually powerless and trapped (the constraint subsidizes them structurally even though they hold no leverage). The detaining power sits as agenda_setter bearing a restraint cost, not extracting a rent — this is why no victims are declared: the story's transfer function moves an obligation onto the state, not a resource from one population to another. This is structurally a Rope, not a Tangled Rope, because there is no identifiable victim group paying through the same structure that produces the coordination benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protection gaps produced by contested status classification) remains live: contemporary counterterrorism detention and non-international armed conflicts continue to generate classification disputes that this floor exists to bypass. Because founding_problem_status is 'live' and disappearance_verdict is 'world_rearranges', there is no mandatrophy here — the mandate has not outlived its function; if anything the function has intensified as irregular conflict has become more common relative to inter-state war.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    floor_versus_full_status_boundary,
    'Where exactly does the status-independent floor end and status-dependent privilege (POW combatant immunity, repatriation rights, prisoner-of-war camp conditions) begin, and is that boundary itself stable across legal traditions?',
    'Comparative analysis of state practice and ICRC customary law study entries distinguishing Common Article 3 floor obligations from Third Geneva Convention POW-specific obligations across multiple conflicts and jurisdictions.',
    'If the boundary is unstable or contested in practice, some of what this reading treats as a clean floor may in practice be eroded by states re-importing status arguments through the back door (e.g., differential interrogation standards justified by ''unlawful combatant'' framing) — which would raise this constraint''s own ε above the low value currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(floor_versus_full_status_boundary, conceptual, 'Stability of the boundary between the universal floor and status-conditioned privilege.').

omega_variable(
    enforcement_capacity_versus_declaratory_commitment,
    'Is the near-universal state ratification of Common Article 3 tracked by near-universal compliance, or does the floor operate mostly as declaratory law with compliance concentrated among states already inclined toward humane treatment?',
    'Cross-referencing ICRC detention visit reports, UN human rights mechanisms, and post-conflict tribunal findings against ratification status to measure compliance gap.',
    'A large compliance gap would suggest the low authored extractiveness reflects legal design rather than operational reality, and that theater_ratio should be authored higher to capture the gap between formal commitment and applied protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_versus_declaratory_commitment, empirical, 'Gap between declaratory ratification and actual detention practice.').

omega_variable(
    kernel_reading_coexistence_versus_tension,
    'Do the three kernel readings (functional_protection, state_centric, national_liberation) genuinely coexist as non-competing tiers, or does the state_centric reading''s restrictive combatant definition create practical pressure to also restrict the functional floor''s application in contested cases (e.g., ''unlawful enemy combatant'' doctrines that attempted to narrow Common Article 3''s reach)?',
    'Historical case study of post-2001 detention policy debates (e.g., Guantanamo-era legal memoranda) to determine whether restrictive combatant-status arguments were used to argue the floor itself did not apply, not merely that full POW status did not apply.',
    'If state-centric restrictiveness bleeds into floor-denial in practice, the readings are not as cleanly separable as this story assumes, and the functional_protection_reading''s low ε may be more fragile / contested than the baseline authored here suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_versus_tension, conceptual, 'Whether restrictive full-status readings create downstream pressure against the floor reading itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1949, combatant_status_definition__functional_protection_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement(comb_tr_t1965, combatant_status_definition__functional_protection_reading, theater_ratio, 1965, 0.16).
narrative_ontology:measurement(comb_tr_t1980, combatant_status_definition__functional_protection_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(comb_tr_t1995, combatant_status_definition__functional_protection_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(comb_tr_t2010, combatant_status_definition__functional_protection_reading, theater_ratio, 2010, 0.21).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__functional_protection_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__functional_protection_reading, base_extractiveness, 1949, 0.1).
narrative_ontology:measurement(comb_be_t1965, combatant_status_definition__functional_protection_reading, base_extractiveness, 1965, 0.1).
narrative_ontology:measurement(comb_be_t1980, combatant_status_definition__functional_protection_reading, base_extractiveness, 1980, 0.11).
narrative_ontology:measurement(comb_be_t1995, combatant_status_definition__functional_protection_reading, base_extractiveness, 1995, 0.11).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__functional_protection_reading, base_extractiveness, 2010, 0.12).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__functional_protection_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__functional_protection_reading, suppression_requirement, 1949, 0.25).
narrative_ontology:measurement(comb_su_t1965, combatant_status_definition__functional_protection_reading, suppression_requirement, 1965, 0.26).
narrative_ontology:measurement(comb_su_t1980, combatant_status_definition__functional_protection_reading, suppression_requirement, 1980, 0.26).
narrative_ontology:measurement(comb_su_t1995, combatant_status_definition__functional_protection_reading, suppression_requirement, 1995, 0.27).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__functional_protection_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__functional_protection_reading, suppression_requirement, 2024, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the combatant_status_definition kernel. functional_protection_reading establishes a status-independent baseline of humane treatment that operates beneath and regardless of the status determination addressed by the other two readings. state_centric_reading and national_liberation_reading both concern who qualifies for full combatant/POW status and its associated privileges — a different, higher-tier structural question. All three readings can be simultaneously operative in the same legal system without contradiction, because they answer different questions (floor treatment vs. full-status privilege vs. the criteria for extending full status to non-state actors). Sibling files should each declare their own ε: this reading's ε is low and stable (0.12) reflecting near-universal ratification and minimal extraction; the state_centric and national_liberation readings are expected to show different ε profiles reflecting their more contested, unresolved status-determination function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
