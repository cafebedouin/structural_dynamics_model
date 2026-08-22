% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__amun_polytheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Amun-Ra Polytheistic Priestly Legitimation System
 *   domain: ancient_history/religious_studies/political_economy
 *
 * SUMMARY:
 *   In the New Kingdom Egyptian state, divine legitimacy is mediated by the
 *   Amun priesthood through a multi-deity cosmology with Amun-Ra as chief
 *   patron. The pharaoh, though claiming divine sonship, requires priestly
 *   oracle and ritual validation to secure bureaucratic and popular
 *   recognition. Temple estates accumulate substantial agricultural surplus
 *   and labor, while regional cults are accommodated beneath the Amun
 *   hierarchy. This reading treats the arrangement as a tangled rope: it
 *   coordinates a diverse, geographically extended polity under a unified
 *   symbolic order, but simultaneously extracts surplus and autonomy from
 *   pharaoh and populace to sustain the priestly class.
 *
 * KEY AGENTS:
 *   - amun_priesthood: Primary agenda-setter and beneficiary (institutional/constrained) â controls interpretive authority and temple economies.
 *   - pharaoh: Primary payer with secondary beneficiary status (powerful/identity_locked) â bears the cost of validation dependency but gains legitimacy.
 *   - provincial_populace: Secondary payer with secondary beneficiary status (powerless/trapped) â supplies labor and surplus, receives ritual protection and redistribution.
 *   - atenist_reformers: Excluded challenger (moderate/trapped) â represents the foreclosed atenist reading of the same kernel.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.58).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.65).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Amun-Ra Polytheistic Priestly Legitimation System").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "ancient_history/religious_studies/political_economy").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, '195f5ccd-68c5-41e2-8288-9388230ba640').
narrative_ontology:cs_kernel_codification('195f5ccd-68c5-41e2-8288-9388230ba640', fixed_text).
narrative_ontology:cs_authority_grounding('195f5ccd-68c5-41e2-8288-9388230ba640', lineage).
narrative_ontology:cs_interpretation_layer_present('195f5ccd-68c5-41e2-8288-9388230ba640').
narrative_ontology:cs_reading_relation('195f5ccd-68c5-41e2-8288-9388230ba640', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('195f5ccd-68c5-41e2-8288-9388230ba640', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('195f5ccd-68c5-41e2-8288-9388230ba640', foundational, divine_multiplicity_hierarchical).
narrative_ontology:cs_axiom_status(divine_multiplicity_hierarchical, holdable).
narrative_ontology:cs_axiom_grounding('195f5ccd-68c5-41e2-8288-9388230ba640', divine_multiplicity_hierarchical, theological).
narrative_ontology:cs_axiom('195f5ccd-68c5-41e2-8288-9388230ba640', foundational, priestly_mediation_necessary).
narrative_ontology:cs_axiom_status(priestly_mediation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('195f5ccd-68c5-41e2-8288-9388230ba640', priestly_mediation_necessary, theological).
narrative_ontology:cs_reference_frame('195f5ccd-68c5-41e2-8288-9388230ba640', new_kingdom_theocratic_order).
narrative_ontology:cs_drift_state('195f5ccd-68c5-41e2-8288-9388230ba640', amarna_heresy_period, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('195f5ccd-68c5-41e2-8288-9388230ba640', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, provincial_populace).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, provincial_populace).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, amun_supremacy_doctrine).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, divine_multiplicity_cosmology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the theological interpretation that places Amun-Ra at the apex of a multi-deity cosmos. Validates pharaonic legitimacy through oracle and ritual. Administers temple estates and redistributes offerings. Cannot abandon the Amun cult without forfeiting institutional authority.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, beneficiary).

% Claims divine sonship and rule by divine right, but requires priestly oracle and ritual enactment to validate that claim before the bureaucracy and populace. Bears the cost of massive temple construction and offerings. Cannot exit the role of living god without dissolving the monarchy itself.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, payer,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, beneficiary).

% Contributes labor and agricultural surplus to temple estates and pharaonic building projects. Receives ritual protection, regional cult accommodation, and redistributive grain in return. Geographically and socially bound to the Nile valley; no viable alternative political-religious order exists.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, provincial_populace, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, provincial_populace, beneficiary).

% Assert that divine legitimacy flows solely through the pharaoh's exclusive revelation of Aten. Are periodically purged or marginalized when the Amun priesthood holds state power; their reading is structurally barred from official discourse.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, atenist_reformers, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__amun_polytheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates a geographically elongated, regionally diverse kingdom along the Nile into a single political-religious order by hierarchizing local deities beneath Amun-Ra, enabling local cultic continuity while subordinating it to a unified cosmological and administrative framework.
% TRANSFER_FUNCTION: Transfers agricultural surplus and corvÃ©e labor from the provincial populace to temple estates, and transfers unilateral divine authority from the pharaoh to the priestly validation apparatus, in exchange for legitimation, cosmic order, and regional cult protection.
% ABSENT_VOICES: Atenist reformers who would center legitimacy solely on pharaonic revelation of a single deity are excluded from official discourse when the priesthood is dominant; folk practitioners whose purely local household rites bypass temple oversight are acknowledged but kept at the margins of the legitimacy economy.
% DISAPPEARANCE_RATIONALE: Without the Amun priestly apparatus, pharaonic legitimacy loses its oracular anchor, temple economies collapse and cannot redistribute grain or staff projects, regional cults fragment into uncoordinated local religions, and the ideological glue of the Upper-Lower Egyptian union dissolves.
% FOUNDING_PROBLEM: How to unify the Two Lands and their disparate regional cults under a single, scalable divine order without provoking constant theocratic rebellion or losing the symbolic loyalty of local populations.
% FOUNDING_PROBLEM_CORROBORATION: The material record of pre-unification regional shrines and the Narmer palette narrative corroborate the existence of a genuine integration problem from outside the Amun priesthood's own self-justification; however, the extent to which the priesthood's later extraction was necessary to solve that problem is disputed by economic historians who note the massive accumulation of temple wealth.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the substantial material flow to temples and the autonomy cost imposed on pharaonic authority, but stops short of pure extraction because the coordination function (imperial integration, regional accommodation) is genuine and non-trivial. Suppression (0.65) is high because the constraint's persistence depends on actively suppressing rival readings (Atenism) and maintaining orthodox interpretation. Theater ratio (0.45) recognizes that a significant share of priestly activity is performative ritual, yet this performance is structurally functional for political legitimacy in this historical context. Resistance (0.42) captures intermittent pharaonic pushback and regional cult persistence. The measurement grid is aligned: all three metrics share the same six time points.
 *
 * PERSPECTIVAL GAP:
 *   The amun_priesthood seat likely computes as tangled_rope or rope, seeing itself as necessary coordinator of cosmic and social order. The pharaoh seat likely computes closer to tangled_rope or snare, experiencing the validation requirement as extraction of unilateral authority. The provincial_populace seat, with powerless and trapped parameters, computes high effective extraction. The engine derives this divergence from the structural data rather than from authored claims.
 *
 * DIRECTIONALITY LOGIC:
 *   The amun_priesthood is the declared beneficiary (low d) because it receives material surplus and political influence from the constraint. The pharaoh and provincial_populace are declared victims and payers (high d) because they bear costs in autonomy and labor or surplus. The pharaoh's secondary beneficiary status moderates its d slightly, but identity_locked exit keeps it near the target end. The provincial_populace has trapped exit, pushing its d toward full target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â unifying regional cults under a scalable divine order â was plausibly live during the Early Dynastic transition. By the mature New Kingdom, the temple economy had grown to a scale that arguably exceeded the coordination need, yet the arrangement persisted because the priesthood had become structurally necessary for pharaonic legitimacy. founding_problem_status=contested signals this mandatrophy tension: the problem may be dead or transformed, while the apparatus persists and extracts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    priestly_extraction_vs_coordination,
    'Does the flow of material surplus to Amun temples represent payment for necessary coordination services (cosmic order, political legitimation, grain storage) or extractive rent collected by an entrenched interpretive class?',
    'Comparative economic analysis of temple redistribution records versus independent regional administration models in comparable ancient riverine societies.',
    'If the surplus flow exceeds the cost of coordination by a wide margin, the constraint shifts toward snare; if it approximates coordination cost, it remains tangled rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_extraction_vs_coordination, empirical, 'Coordination cost versus extraction rate in temple economy').

omega_variable(
    kernel_reading_boundary,
    'This constraint is the amun_polytheistic_reading of the divine_legitimacy_substrate kernel. How would structural classification change if the same institutional arrangement were read through the atenist_monotheistic_reading or folk_syncretistic_reading?',
    'Generate sibling constraint stories for the atenist and folk readings and compare computed seat classifications.',
    'The atenist reading would likely classify as snare (pure royal extraction) or scaffold (transitional reform), while the folk reading might classify as rope (bottom-up coordination) or piton (degraded custom); the kernel decomposition isolates where disagreement is located.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Sibling reading structural deltas for kernel decomposition').

omega_variable(
    pharaonic_identity_lock,
    'Is the pharaoh''s inability to exit the validation relationship due to structural political necessity or to identity fusion (the office IS divine kingship)?',
    'Comparative analysis of pharaohs who attempted to bypass Amun (e.g., Akhenaten) â did they retain legitimacy among non-priestly constituencies, and for how long?',
    'If identity-locked, the pharaoh''s effective extraction is higher than structural measures suggest because the constraint is carried internally even during attempted exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharaonic_identity_lock, conceptual, 'Identity fusion versus structural constraint on pharaonic exit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divine_amun_tr_t0, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(divine_amun_tr_t20, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(divine_amun_tr_t40, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(divine_amun_tr_t60, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 60, 0.44).
narrative_ontology:measurement(divine_amun_tr_t80, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 80, 0.46).
narrative_ontology:measurement(divine_amun_tr_t100, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(divine_amun_be_t0, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(divine_amun_be_t20, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(divine_amun_be_t40, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(divine_amun_be_t60, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(divine_amun_be_t80, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(divine_amun_be_t100, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(divine_amun_su_t0, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(divine_amun_su_t20, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(divine_amun_su_t40, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(divine_amun_su_t60, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(divine_amun_su_t80, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement(divine_amun_su_t100, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, atenist_monotheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% The divine_legitimacy_substrate kernel decomposes into three structurally distinct readings: amun_polytheistic_reading (priestly-mediated multi-deity hierarchy), atenist_monotheistic_reading (pharaonic sole revelation), and folk_syncretistic_reading (bottom-up pragmatic ritual). Each reading instantiates a different constraint with distinct epsilon, beneficiary/victim structure, and classification. They form a constraint family linked by shared kernel origin but divergent structural commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
