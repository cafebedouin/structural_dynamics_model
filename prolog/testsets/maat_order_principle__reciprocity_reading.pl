% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at Reciprocal Obligation (Pharaonic Governance)
 *   domain: political_philosophy/religious_studies/ancient_history
 *
 * SUMMARY:
 *   In ancient Egyptian political theology, Ma'at represents cosmic order,
 *   truth, justice, and reciprocal balance. This constraint story
 *   instantiates the RECIPROCITY READING: Ma'at imposes mutual obligations on
 *   Pharaoh and subjects. Pharaoh must provide justice, stable governance,
 *   and equitable resource distribution; in return, the population provides
 *   obedience, labor, and taxes. The constraint's legitimacy rests on the
 *   reciprocal framing: extraction is acceptable only if Pharaoh fulfills his
 *   Ma'at obligations. Failure to provide justice or allow famine without
 *   relief violates the cosmic covenant and justifies resistance or
 *   withdrawal of support by priests and regional governors. This reading
 *   sits between the divine_mandate_reading (which claims Pharaoh embodies
 *   Ma'at and cannot violate it) and the distributed_maintenance_reading
 *   (which claims all actors from Pharaoh to commoner sustain cosmic order
 *   through conduct in their station). The reciprocity reading is distinct:
 *   it subjects Pharaoh to external constraint (Ma'at law he does not make),
 *   acknowledges victims of extraction (marginalized classes,
 *   resource-deprived regions), and posits enforceability through ritual,
 *   priestly judgment, and elite defection. The claimed_type is Tangled Rope:
 *   genuine coordination (resource distribution, justice provision) layered
 *   with asymmetric extraction (tax, labor, conscription), requiring active
 *   enforcement (priestly judgment, ritual affirmation) to hold.
 *
 * KEY AGENTS:
 *   - Pharaoh: Institutional agenda-setter and identity-locked beneficiary. Subject to Ma'at constraints; cannot exit without abandoning divine office, but is held accountable through priestly judgment and elite defection risk.
 *   - Priestly class: Institutional agenda-setter. Interprets Ma'at compliance; can pressure Pharaoh through ritual withdrawal or public pronouncement of violation. Constrained exit (benefits from Pharaoh's continuation, cannot replace him without chaos).
 *   - General population: Powerless beneficiary and payer. Receives justice and resource distribution; bears labor obligation and tax. Trapped exit (territorial, identity-bound, no alternative sovereignty).
 *   - Marginalized classes: Powerless payer. Bears extraction (corvée, conscription, appropriation) with minimal reciprocal claim on justice. Structurally excluded from the reciprocity circle by status assignment.
 *   - Regional governors: Powerful agenda-setters. Administer Ma'at locally; maintain delegated authority but can pivot to rival power if Pharaoh visibly fails obligations. Contingently constrained exit.
 *   - Nobility and administrators: Organized beneficiaries. Collect wealth and status through Pharaonic appointment; enforce extraction downward. Constrained by patronage system; alternative bases suppressed.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.42).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.38).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at Reciprocal Obligation (Pharaonic Governance)").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "political_philosophy/religious_studies/ancient_history").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, '2119373f-6856-4295-954c-30b3b2c69b12').
narrative_ontology:cs_kernel_codification('2119373f-6856-4295-954c-30b3b2c69b12', fixed_text).
narrative_ontology:cs_authority_grounding('2119373f-6856-4295-954c-30b3b2c69b12', lineage).
narrative_ontology:cs_interpretation_layer_present('2119373f-6856-4295-954c-30b3b2c69b12').
narrative_ontology:cs_reading_relation('2119373f-6856-4295-954c-30b3b2c69b12', maat_order_principle__divine_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('2119373f-6856-4295-954c-30b3b2c69b12', maat_order_principle__distributed_maintenance_reading, influences).
narrative_ontology:cs_axiom('2119373f-6856-4295-954c-30b3b2c69b12', foundational, pharaoh_subject_to_external_constraint).
narrative_ontology:cs_axiom_status(pharaoh_subject_to_external_constraint, holdable).
narrative_ontology:cs_axiom_grounding('2119373f-6856-4295-954c-30b3b2c69b12', pharaoh_subject_to_external_constraint, deontological).
narrative_ontology:cs_axiom('2119373f-6856-4295-954c-30b3b2c69b12', foundational, reciprocal_obligation_binds_all_seats).
narrative_ontology:cs_axiom_status(reciprocal_obligation_binds_all_seats, holdable).
narrative_ontology:cs_axiom_grounding('2119373f-6856-4295-954c-30b3b2c69b12', reciprocal_obligation_binds_all_seats, conventional).
narrative_ontology:cs_reference_frame('2119373f-6856-4295-954c-30b3b2c69b12', reciprocal_cosmic_order).
narrative_ontology:cs_drift_state('2119373f-6856-4295-954c-30b3b2c69b12', late_pharaonic_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2119373f-6856-4295-954c-30b3b2c69b12', '').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, pharaonic_authority).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, general_population).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, marginalized_classes).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, resource_deprived_regions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__reciprocity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).
:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because reciprocity framing provides legitimate justification for extraction: Pharaoh's justice provision, resource distribution during famine, and military defense are real goods that offset the cost of taxation and labor obligation. However, the extraction is not ceiling-free: the reciprocity norm theoretically caps extraction at the point where reciprocal benefits fail to materialize. Suppression is lower (0.38) because the constraint's persistence depends less on brute coercion and more on elite consensus (priestly legitimacy, regional governor loyalty) around the reciprocal framing. Theater is low (0.22): the justice function and seasonal resource distribution are substantive, not purely performative, though priestly ritual affirms compliance. Accessibility of alternatives is moderate-high (0.68): the boundary of the Egyptian state is the boundary of the constraint; exiting the constraint means abandoning Egyptian identity and resources, which collapses practical alternatives for most agents. Resistance is significant (0.55): periodic priest-Pharaoh tensions, regional governor defections, and popular unrest during famine suggest the constraint meets real friction. The measurement series show extractiveness plateauing after period 15, suggesting the constraint reached a steady state: initial recruitment required visible compliance with reciprocal obligations, but once institutional structure solidified, extraction stabilized at its sustainable ceiling without further ratcheting.
 *
 * PERSPECTIVAL GAP:
 *   The Pharaoh and priestly class experience this constraint as coordination: they maintain cosmic order and extract rents as legitimate payment for provision of public goods (justice, stability, military defense, famine relief). Regional governors experience it as contingently constrained extraction: they benefit from central stability but can defect if Pharaoh visibly fails obligations. The general population experiences it as asymmetric burden: they bear extraction (labor, tax, conscription) and receive justice and resource distribution, with no voice in whether the bargain is fair. Marginalized classes experience it as pure extraction: structurally excluded from the reciprocity circle by status assignment, they bear conscription and appropriation with no reciprocal claim on Pharaoh's justice obligation. The engine computes these seat divergences from the structural data: beneficiary/victim declarations, power atoms, exit options, and time horizons all differ across seats, producing different directionalities (d values) and hence different per-seat classifications. The Pharaoh's seat likely computes toward Rope (coordinator, low extraction from his position). Payer seats (general population, marginalized classes) likely compute toward Snare or Tangled Rope (high extraction, suppressed exit, asymmetric benefit). The goal is to author structural data that drives this divergence without pre-adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh (institutional power, generational time horizon, identity-locked exit): d near 0.25–0.35 (beneficiary, though subject to constraint). Priestly class (institutional power, constrained exit): d near 0.35–0.45 (partly beneficiary, partly constrained enforcer—secondary extraction is required to adjudicate compliance). General population (powerless, trapped exit, biographical horizon): d near 0.65–0.75 (clear target, bear labor and tax costs, receive diffuse justice and resource distribution). Marginalized classes (powerless, trapped exit, immediate horizon): d near 0.80–0.90 (full target, extraction without reciprocal claim). Regional governors (powerful, contingent exit): d near 0.50–0.55 (symmetric—benefit from stability, can defect if obligations fail; their exit option is real but constrained, modulating d away from full target). The directionality_logic is grounded in the structural relationships: Pharaoh's identity is locked in the role (cannot exit without abandoning office), Priestly class's exit is constrained by dependence on Pharaonic legitimacy, General population's exit is trapped (territorial, identity-bound), Regional governors' exit is contingent (can shift loyalty if Pharaoh fails visibly). These exit-option differences propagate into the directionality derivation, producing the seat divergence the engine computes.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids the mandatrophy trap (mandate outliving function) because the reciprocity reading explicitly grounds Pharaoh's legitimacy in the *current* provision of goods (justice, resource distribution, military defense). If Pharaoh visibly failed these obligations—permitting systematic injustice, allowing famine without relief, losing wars—the reciprocity mandate would be violated and resistance would be justified. This is testable and reversible: a Pharaoh who restores justice provision can restore legitimacy. In contrast, the divine_mandate_reading is mandatrophy-prone: it claims Pharaoh embodies Ma'at and therefore cannot violate it, which means a Pharaoh who extracts ruthlessly can reframe the extraction as cosmically necessary, rendering the mandate unfalsifiable. The distributed_maintenance_reading avoids mandatrophy differently: by distributing accountability across all social layers, it also distributes legitimacy—no single Pharaoh can claim to be the whole system. The reciprocity reading's mandatrophy resistance is moderate: as long as Pharaoh provides visible justice (courts function, famines are relieved, military defends the border), the mandate persists. But if Pharaoh's power becomes overwhelming enough to suppress priestly judgment or regional governor defection, the mandate can become decorative—a symbolic affirmation of extraction rather than a constraint on it. The measurement trajectory's plateauing at steady-state extraction (rather than ratcheting upward) suggests the reciprocity constraint has found its equilibrium: Pharaoh extracts up to the point where reciprocal provision becomes credible, and no further without risking priestly and elite defection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_enforcement_gap,
    'Is the reciprocity obligation genuinely binding on Pharaoh, or is it a narrative frame that permits extraction without constraint when Pharaoh holds overwhelming military power?',
    'Historical analysis of cases where Pharaoh visibly violated Ma''at obligations (extractive taxation during famine, denial of justice to powerless) and tracking whether priestly or popular resistance followed or whether extraction continued unimpeded. Pattern of responses to violation reveals whether the constraint is enforced or merely rhetorical.',
    'If reciprocity is enforced: the constraint is genuine Tangled Rope—extraction is capped by the obligation to provide reciprocal benefit. If unenforceable when Pharaoh is strong: the constraint is a Snare—the reciprocity frame is cover for unlimited extraction, and the obligation''s visibility makes resistance harder by delegitimizing it as betrayal of cosmic order.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reciprocity_enforcement_gap, empirical, 'Whether Ma''at reciprocity functionally constrains Pharaoh''s extraction or is an unenforced narrative.').

omega_variable(
    marginal_classes_inclusion_ambiguity,
    'Are slaves, debt-bound laborers, and foreigners genuinely excluded from the Ma''at reciprocal circle by cosmic necessity, or are they excluded by pragmatic choice to avoid constraining the extraction mechanism?',
    'Comparative historical analysis: periods or regions where marginalized classes were granted greater claims on justice or resource distribution (e.g., jubilee practices, sanctuary rights, minimum subsistence guarantees) versus periods of tighter exclusion. Examination of justifying texts (whether exclusion is presented as natural/cosmic or pragmatic/administrative).',
    'If exclusion is presented as cosmic: the constraint frames a portion of the population as outside the reciprocity mechanism—a structural partition that narrows the beneficiary class. If exclusion is pragmatic: the constraint is even more extractive—it could accommodate broader reciprocity but chooses not to, and the cosmic framing obscures the choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_classes_inclusion_ambiguity, conceptual, 'Whether marginal-class exclusion is part of the constraint''s structure or a contingent enforcement choice.').

omega_variable(
    cosmic_order_versus_human_choice,
    'Is Ma''at a cosmic imperative that constrains all parties equally, or is it a human institutional choice by priesthood and Pharaoh that frames extraction as cosmic necessity?',
    'The kernel-level question (deferred to reading_relations and axioms): this reading instantiates reciprocity (mutual obligation, moderate extraction ceiling grounded in norm). The divine_mandate_reading claims cosmic flow through Pharaoh makes violation impossible by definition. The distributed_maintenance_reading claims all actors sustain Ma''at equally. This omega documents the irreducible framing ambiguity: the same observable facts (stability, ritual, resource distribution) are consistent with all three readings, and no evidence can prove which reading is cosmically true versus institutionally convenient.',
    'Classification hinges on this: if Ma''at is genuinely cosmic and reciprocal, Tangled Rope is correct. If it is human institutional choice using cosmic framing, the constraint is Snare (extraction with legitimating mythology). The engine computes the type from structural metrics (beneficiary/victim, enforcement, exit options); this omega documents why the structural data cannot disambiguate the cosmic from the institutional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmic_order_versus_human_choice, conceptual, 'The foundational ambiguity between cosmic order and institutional construction that all three readings share.').

omega_variable(
    distributed_maintenance_versus_pharaonic_monopoly,
    'Does the reciprocity reading genuinely accommodate distributed maintenance (all actors responsible for Ma''at, from Pharaoh to commoner), or does it concentrate responsibility and extraction power in the Pharaonic seat even as it frames it reciprocal?',
    'Textual and institutional analysis: what do actual Ma''at obligations demand of each social layer? Is the commoner''s obedience and ritual participation treated as equal in cosmological weight to Pharaoh''s justice provision? Can failure by commoner/administrator trigger the same sanctions as Pharaoh''s failure? Examination of whether the distributed_maintenance_reading and the reciprocity_reading make genuinely different structural claims or whether they agree on Pharaonic primacy while disagreeing on whether Pharaoh is constrained.',
    'If distributed maintenance is genuine: reciprocity_reading is closer to a genuine Rope (all parties coordinate, all are bounded). If Pharaonic monopoly on obligation is masked by reciprocity framing: reciprocity_reading is closer to Snare (extraction is presented as reciprocal but Pharaoh''s obligations are unenforced while subjects'' are absolute). The boundary is the structural relationship between pharaonic and distributed responsibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_maintenance_versus_pharaonic_monopoly, conceptual, 'Whether reciprocity is genuinely distributed or concentrated in Pharaonic monopoly masked by reciprocal framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(maat_tr_t5, maat_order_principle__reciprocity_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(maat_tr_t10, maat_order_principle__reciprocity_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(maat_tr_t15, maat_order_principle__reciprocity_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(maat_tr_t25, maat_order_principle__reciprocity_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__reciprocity_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(maat_be_t5, maat_order_principle__reciprocity_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(maat_be_t10, maat_order_principle__reciprocity_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(maat_be_t15, maat_order_principle__reciprocity_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(maat_be_t25, maat_order_principle__reciprocity_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__reciprocity_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(maat_su_t5, maat_order_principle__reciprocity_reading, suppression_requirement, 5, 0.34).
narrative_ontology:measurement(maat_su_t10, maat_order_principle__reciprocity_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(maat_su_t15, maat_order_principle__reciprocity_reading, suppression_requirement, 15, 0.37).
narrative_ontology:measurement(maat_su_t25, maat_order_principle__reciprocity_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__reciprocity_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(maat_order_principle__reciprocity_reading, 0.18).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% The maat_order_principle kernel decomposes into three structurally distinct constraints: divine_mandate_reading (Pharaoh embodies Ma'at, cannot violate it, classification approaches Mountain), distributed_maintenance_reading (all actors sustain Ma'at, flat accountability, classification approaches Rope or coordination), and reciprocity_reading (mutual obligations with enforcement, Pharaoh subject to external constraint, classification Tangled Rope). Each ε-value is stable within its reading: divine_mandate has near-zero extraction (no external constraint, Pharaoh is the constraint); distributed has moderate coordination extraction (distributed overhead); reciprocity has capped asymmetric extraction (coordination plus enforcement). The three readings coexist as live positions held by different parties (priestly theological traditions, different dynastic periods, regional interpretation variants); no reading forecloses another within a single framework—the disagreement is about which reading best captures Ma'at's true structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__reciprocity_reading, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
