% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: UN Charter Art. 2(4) / IHL Prohibition on Total War as Legitimate Statecraft
 *   domain: international_relations/law/strategic_studies
 *
 * SUMMARY:
 *   Between 1900 and 1945, total war — the deliberate targeting of civilian
 *   populations, unrestricted submarine warfare, strategic bombing of cities,
 *   starvation blockades, and wars of annexation — was a legitimate
 *   instrument of state policy that great powers openly pursued and defended
 *   in principle even when they lost. After 1945, the physical capacity for
 *   such warfare did not diminish (indeed nuclear weapons made it more
 *   destructive than ever), but its normative status collapsed: Article 2(4)
 *   of the UN Charter prohibited the threat or use of force against the
 *   territorial integrity of any state, and the subsequent development of
 *   international humanitarian law (Geneva Conventions 1949, Additional
 *   Protocols 1977, the Rome Statute establishing the ICC in 1998) built an
 *   increasingly dense apparatus criminalizing the specific conduct total war
 *   requires. States and non-state actors that pursue total-war strategies
 *   today must do so through denial, euphemism, or claimed exceptional
 *   circumstance rather than open doctrinal defense — the norm does not stop
 *   the war but it does deny it legitimate cover.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.28).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.42).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.28).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "UN Charter Art. 2(4) / IHL Prohibition on Total War as Legitimate Statecraft").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/law/strategic_studies").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, '5b29606e-ff5f-4f0c-9d0e-39f1f5134e71').
narrative_ontology:cs_kernel_codification('5b29606e-ff5f-4f0c-9d0e-39f1f5134e71', formalized).
narrative_ontology:cs_authority_grounding('5b29606e-ff5f-4f0c-9d0e-39f1f5134e71', extraction).
narrative_ontology:cs_interpretation_layer_present('5b29606e-ff5f-4f0c-9d0e-39f1f5134e71').
narrative_ontology:cs_reading_relation('5b29606e-ff5f-4f0c-9d0e-39f1f5134e71', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b29606e-ff5f-4f0c-9d0e-39f1f5134e71', total_war_winnability_post1945__strategic_culture_drift, influences).
narrative_ontology:cs_axiom('5b29606e-ff5f-4f0c-9d0e-39f1f5134e71', foundational, legal_codification_is_the_operative_restraint_mechanism).
narrative_ontology:cs_axiom_status(legal_codification_is_the_operative_restraint_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('5b29606e-ff5f-4f0c-9d0e-39f1f5134e71', legal_codification_is_the_operative_restraint_mechanism, conventional).
narrative_ontology:cs_axiom('5b29606e-ff5f-4f0c-9d0e-39f1f5134e71', foundational, total_war_illegitimacy_is_independent_of_weapons_technology).
narrative_ontology:cs_axiom_status(total_war_illegitimacy_is_independent_of_weapons_technology, holdable).
narrative_ontology:cs_axiom_grounding('5b29606e-ff5f-4f0c-9d0e-39f1f5134e71', total_war_illegitimacy_is_independent_of_weapons_technology, empirically_contingent).
narrative_ontology:cs_reference_frame('5b29606e-ff5f-4f0c-9d0e-39f1f5134e71', unrestricted_sovereign_war_prerogative).
narrative_ontology:cs_drift_state('5b29606e-ff5f-4f0c-9d0e-39f1f5134e71', contemporary_post_cold_war_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('5b29606e-ff5f-4f0c-9d0e-39f1f5134e71', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, small_and_middle_powers).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, un_charter_signatory_states).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, territorially_expansionist_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, great_power_militaries_inside_the_norm).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, great_power_militaries_inside_the_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct physical cost of any war fought near them and have no seat at treaty negotiations, but are the class whose survival odds rise when total-war conduct (city-leveling bombardment, unrestricted starvation blockades, genocide-as-strategy) is denied legal cover. They cannot enforce the norm themselves; they depend entirely on states and international bodies choosing to treat it as binding.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    powerless, civilizational, trapped, global).

% Ratified Article 2(4) and the subsequent Geneva Conventions/Additional Protocols apparatus, committing to renounce force against territorial integrity and to distinguish combatants from civilians even in existential wars. They administer the norm through UN Security Council referral, ICC jurisdiction, and reciprocal treaty enforcement, and bear the cost of occasionally restraining their own military options to remain inside the framework they built.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, un_charter_signatory_states, agenda_setter,
    institutional, generational, constrained, global).

% Lack the material capacity to deter a great power through balance-of-power alone; the normative prohibition on total war (backed by nuclear deterrence and collective security guarantees) is a large part of what keeps their territorial integrity intact against materially stronger neighbors. Their exit from the framework would leave them structurally exposed.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, small_and_middle_powers, beneficiary,
    moderate, generational, constrained, global).

% States seeking to redraw borders or eliminate rival populations by force retain the physical and industrial capacity for total war but pay a real cost for exercising it: diplomatic isolation, sanctions regimes, war-crimes tribunal exposure, and loss of legitimating narrative even among domestic elites. They can violate the norm (and periodically do), but violation now carries structured reputational and legal costs that did not attach to the same conduct before 1945.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers, payer,
    powerful, biographical, constrained, continental).

% Regimes whose strategic doctrine still contemplates unrestricted war against civilian populations or annexation by force find that doctrine now has to be laundered through euphemism, denial, or claimed self-defense, because the unrestricted version is no longer a legitimate move in the international system's discourse. The norm does not physically stop them but forecloses the honest public articulation of the strategy.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, territorially_expansionist_regimes, payer,
    powerful, biographical, constrained, regional).

% Adjudicate individual criminal responsibility for conduct that total war would have required (targeting civilians, disproportionate force, genocide) and issue indictments against heads of state and commanders. Their jurisdiction is incomplete and unevenly enforced against powerful states, but their existence is itself evidence that total-war conduct is now something to be prosecuted rather than merely won or lost.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_criminal_court_and_tribunals, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, international_criminal_court_and_tribunals, observer).

% Retain the physical capability for total war (nuclear arsenals, strategic bombing doctrine, blockade capacity) but train, plan, and legally review their operations under the assumption that unrestricted total war is not an available legitimate option except in extremis. They benefit from the reciprocal restraint of rivals but pay in operational flexibility and in the doctrinal argument that restraint should never bind first.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, great_power_militaries_inside_the_norm, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, great_power_militaries_inside_the_norm, payer).

% Domestic constituencies and state propagandists who would prefer total-war doctrine be openly legitimate again are not part of the treaty-drafting or enforcement conversation; their objection is that the norm is a victor's imposition that disarms revisionist ambition rhetorically while doing nothing to disarm it materially.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, war_crimes_denying_apologists, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article 2(4) and the humanitarian law corpus solve a genuine collective-action problem: without a shared, reciprocally-enforced prohibition on total war as a legitimate instrument of policy, every state would face a first-mover disadvantage in restraining itself while rivals did not, producing an equilibrium where total war remains the rational default. The treaty framework converts unilateral restraint into a coordinated, monitored, and reciprocally-enforced norm.
% TRANSFER_FUNCTION: Moves legitimacy and reputational capital away from states and regimes that pursue total-war strategies and toward states that observe the restraint, while moving physical security (freedom from unrestricted bombardment, starvation siege, and civilian targeting) toward populations in weaker or smaller states who could not have purchased that security through military strength alone.
% ABSENT_VOICES: Revisionist and territorially expansionist regimes participate in drafting the norm's language but are not the audience it was built to satisfy, and populations under their control who might prefer forceful resolution of a territorial dispute over the frozen status quo are almost never consulted; war-crimes-denying domestic constituencies within norm-violating states are structurally outside the enforcement conversation entirely.
% DISAPPEARANCE_RATIONALE: If Article 2(4) and the IHL prohibition on total-war conduct vanished overnight, the reputational and legal cost of unrestricted war would disappear, removing a real (if imperfect) brake on great-power and revisionist-power conduct toward weaker neighbors; states and militaries currently constrained by legal review, sanctions exposure, and tribunal risk would face a materially different calculus, and international institutions built to adjudicate and deter total-war conduct would lose their object.
% FOUNDING_PROBLEM: The unrestricted, civilian-targeting, annexation-by-force warfare of 1914-1945 (culminating in the Holocaust, the Rape of Nanking, strategic bombing campaigns, and the atomic bombings) demonstrated that total war without normative constraint produced catastrophic civilian death tolls and destabilized the entire international order; the founders of the postwar legal architecture built Article 2(4) and the Geneva/Additional Protocol regime specifically to deny total-war conduct legal and rhetorical legitimacy going forward.
% FOUNDING_PROBLEM_CORROBORATION: UN Charter drafters and ICJ jurisprudence attest the founding problem remains live — the norm continues to be invoked in active adjudication (ICJ genocide cases, ICC indictments) rather than treated as settled history. Independent conflict scholars and IHL monitors outside any signatory government (e.g., ICRC customary law studies, academic war-termination literature) corroborate that total-war-style conduct still recurs (Syria, Yemen, Ukraine, Gaza) and that the norm functions as a contested constraint on such conduct rather than a dead letter, though its enforcement is acknowledged by the same outside observers to be highly uneven across power asymmetries.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__normative_reading_drop, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.28: the norm imposes real constraint costs on revisionist and expansionist powers (isolation, sanctions, tribunal exposure) without those costs approaching confiscatory levels — powerful states that violate the norm still generally survive the violation. Suppression sits higher (0.42) because enforcement genuinely depends on active institutional machinery (UN Security Council referral, ICC prosecution, treaty reciprocity, sanctions regimes) rather than automatic physical law — this is precisely what distinguishes the normative reading from the structural_contraction sibling, where no comparable active enforcement apparatus is needed because physics itself does the restraining. Theater ratio is low-moderate (0.18) and rising slowly: enforcement is genuine but uneven, and its unevenness (Security Council veto-blocking, selective ICC jurisdiction) produces some performative gap between stated universality and actual application, which the slow upward drift in the measurement series is meant to capture honestly rather than flatteringly.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of global civilian populations and small/middle powers, this constraint reads unambiguously as coordination — a genuine solution to a first-mover restraint problem that produces real security gains. From the seat of a revisionist power contemplating annexation by force, the same structure reads as an externally imposed constraint that denies a strategic option previously available to great powers as a matter of course, imposed disproportionately by the very powers (the WWII victors, permanent Security Council members) who used total-war methods themselves before the norm was codified. The engine should register this asymmetry structurally: the beneficiary seats compute as rope-like, the payer seats compute closer to a constraint experienced as externally enforced restriction, even though both are looking at the identical treaty architecture.
 *
 * DIRECTIONALITY LOGIC:
 *   Global civilian populations and small/middle powers are the clearest beneficiaries: they gain security they could not have purchased through military strength alone, at essentially zero direct cost to themselves, so they sit near the full-beneficiary end of directionality. Revisionist and territorially expansionist powers are the targets: they retain the physical capacity the norm constrains and pay real reputational, legal, and diplomatic costs for exercising it, placing them near the target end. UN charter signatory states and the ICC/tribunal apparatus occupy the agenda-setter seat — they built and administer the constraint and bear the cost of occasionally restraining their own preferred options to remain credible enforcers of a norm they wrote.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — total war's catastrophic and legally unconstrained civilian toll in the 1914-1945 period — remains contested rather than dead: total-war-adjacent conduct (siege warfare, disproportionate civilian targeting, ethnic-cleansing campaigns) continues to recur in Syria, Yemen, Ukraine, and Gaza, meaning the founding problem has not been solved so much as partially suppressed by the norm's continued (if uneven) operation. This prevents mislabeling the constraint as either a fully-solved-and-obsolete scaffold (the problem is not dead) or a pure extraction mechanism serving only the powers that wrote it (the security benefit to civilian populations and weaker states is real and independently corroborated by conflict scholarship outside any signatory government's self-interest).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_vs_structural_vs_cultural_mechanism,
    'Is the post-1945 disappearance of legitimate total-war doctrine best explained by legal/normative prohibition (Article 2(4) and IHL), by structural physical contraction (nuclear deterrence making total war unsurvivable), or by strategic-culture drift (elite discourse evolution independent of formal law)? These are the three sibling readings of the total_war_winnability_post1945 kernel — this story instantiates only the normative reading.',
    'Comparative case analysis of total-war-adjacent conduct by nuclear-armed versus non-nuclear-armed revisionist states: if legal/normative constraint operates independently of nuclear deterrence (i.e., non-nuclear revisionist states also face the same reputational/legal cost structure), the normative reading is separately supported; if total-war restraint tracks nuclear possession status regardless of treaty ratification, the structural_contraction_reading better explains the same observations.',
    'If the structural reading fully explains the observed restraint, this story''s claimed beneficiary structure (the treaty apparatus as active cause) would be substantially overstated — the norm would be closer to an epiphenomenal ratification of a physical fact than an independent coordination mechanism. If the normative mechanism is shown to operate independently (constraining even non-nuclear revisionist states), this reading''s claimed_type and beneficiary declarations are well-supported as the primary causal story for at least a substantial share of the observed restraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_vs_structural_vs_cultural_mechanism, empirical, 'Whether legal/normative mechanism, nuclear structural mechanism, or strategic-culture mechanism carries the causal weight — the central committer-frame ambiguity this reading takes a position on.').

omega_variable(
    selective_enforcement_undermines_universality_claim,
    'Does the demonstrably uneven enforcement of Article 2(4) and IHL against powerful states (compared to weaker states) mean the norm is better described as a tangled rope (coordination for most, selective non-enforcement functioning as a benefit to permanent-Security-Council-member states) rather than a clean rope?',
    'Systematic comparison of ICC/ICJ case initiation and Security Council referral rates against P5-member conduct versus non-P5-member conduct in comparable circumstances.',
    'If enforcement asymmetry is severe and structural (not merely incidental), the constraint''s engine-computed type may diverge from this story''s claimed rope classification toward tangled_rope, with P5 states occupying a beneficiary seat that also partially escapes the constraint''s costs — exactly the divergence the framework is built to surface rather than paper over.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_enforcement_undermines_universality_claim, empirical, 'Whether uneven enforcement against powerful states converts the rope''s coordination function into a hybrid coordination/extraction structure.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the normative_reading_drop''s causal claim end and the strategic_culture_drift sibling''s claim begin? Both readings involve a change in what states consider legitimate, but this reading locates the mechanism in codified treaty law with enforcement machinery, while the sibling locates it in informal elite discourse and norm internalization without formal legal apparatus as the primary driver.',
    'Trace instances of total-war-adjacent restraint that occurred BEFORE formal codification (pre-1945 discourse shifts) versus restraint that only appeared after treaty ratification and enforcement infrastructure matured — if restraint preceded formal law, strategic-culture drift is doing more causal work than this reading credits.',
    'Clarifies whether this story''s claimed beneficiary/victim structure (built on active treaty enforcement) should be read as the dominant mechanism or as one layer atop a prior informal norm shift that the strategic_culture_drift sibling more accurately captures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Where the boundary between this reading and its strategic-culture-drift sibling actually lies in the historical record.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1960, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(tota_tr_t1975, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1975, 0.14).
narrative_ontology:measurement(tota_tr_t1990, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2005, 0.16).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(tota_be_t1960, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1960, 0.32).
narrative_ontology:measurement(tota_be_t1975, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1975, 0.3).
narrative_ontology:measurement(tota_be_t1990, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1990, 0.27).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2005, 0.26).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1945, 0.55).
narrative_ontology:measurement(tota_su_t1960, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1960, 0.5).
narrative_ontology:measurement(tota_su_t1975, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1975, 0.46).
narrative_ontology:measurement(tota_su_t1990, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(tota_su_t2025, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__normative_reading_drop, 0.12).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, strategic_culture_drift).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the total_war_winnability_post1945 kernel. normative_reading_drop (this story) claims the mechanism is codified legal prohibition (Article 2(4), IHL) with active enforcement machinery and moderate extraction (0.28) reflecting real but non-confiscatory constraint costs on revisionist powers. structural_contraction_reading claims the mechanism is nuclear deterrence making total war physically unsurvivable — expect near-zero extraction and mountain-class classification there since no active enforcement is needed for a physical fact. strategic_culture_drift claims the mechanism is elite discourse evolution independent of formal legal machinery — expect lower suppression than this reading since cultural norm-internalization requires less active coercive apparatus than treaty enforcement. All three share the same observable (total war has not recurred at 1939-45 scale since 1945) but assign different causal weight and produce different epsilon values, hence three separate constraint files linked by this network field rather than one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
