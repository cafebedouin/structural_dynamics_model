% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__property_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: animal_moral_status__property_reading
 *   human_readable: Animals as Property: Ontological Reading
 *   domain: applied_ethics/legal_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the property reading of the contested
 *   animal_moral_status kernel. It asserts that animals are
 *   property/resources with no independent moral standing; their interests
 *   are ontologically subordinate to human interests by definition. The
 *   reading is presented as a stable mountain — an irreducible structural
 *   fact about what animals are, not a human-authored choice. This JSON
 *   authorizes ONLY this reading. Sibling readings (welfare_reading,
 *   abolitionist_reading) are separate constraint stories, each with their
 *   own ε, stakeholder structures, and cs_structure.reading_relations
 *   entries. This reading's beneficiaries are property owners and extractive
 *   industries; animals do not appear in the victim set because the reading
 *   ontologically excludes them from moral standing.
 *
 * KEY AGENTS:
 *   - property_owners_and_users: Primary beneficiary (institutional) — can use animals as resources without moral constraint on use itself.
 *   - extractive_industries: Institutional beneficiary (global scope, generational horizon) — agricultural, pharmaceutical, research sectors operate under the property doctrine.
 *   - regulatory_apparatus: Agenda-setter (institutional, national-to-universal scope) — codifies and enforces property status through law and institutional recognition.
 *   - animal_ethicists_welfare_advocate: Observer (moderate power, biographical horizon) — analyzes the constraint from outside; cannot change it from within its frame.
 *   - abolitionist_theorists: Excluded (moderate power, biographical horizon) — would argue the property status itself is the violation, but this argument is foreclosed by the reading's ontological fiat.
 *   - animals_in_property_regimes: Excluded by definition (non-agent marker) — their sentience and suffering are biological facts external to the reading's logic, not moral facts that ground claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.18).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.12).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animals as Property: Ontological Reading").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/legal_philosophy").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, 'b6edfb07-3d3a-4b3a-8021-f2456923a174').
narrative_ontology:cs_kernel_codification('b6edfb07-3d3a-4b3a-8021-f2456923a174', fixed_text).
narrative_ontology:cs_authority_grounding('b6edfb07-3d3a-4b3a-8021-f2456923a174', lineage).
narrative_ontology:cs_interpretation_layer_present('b6edfb07-3d3a-4b3a-8021-f2456923a174').
narrative_ontology:cs_reading_relation('b6edfb07-3d3a-4b3a-8021-f2456923a174', animal_moral_status__welfare_reading, influences).
narrative_ontology:cs_reading_relation('b6edfb07-3d3a-4b3a-8021-f2456923a174', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('b6edfb07-3d3a-4b3a-8021-f2456923a174', foundational, animals_lack_independent_moral_standing).
narrative_ontology:cs_axiom_status(animals_lack_independent_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('b6edfb07-3d3a-4b3a-8021-f2456923a174', animals_lack_independent_moral_standing, deontological).
narrative_ontology:cs_axiom('b6edfb07-3d3a-4b3a-8021-f2456923a174', foundational, property_rights_override_sentience_claims).
narrative_ontology:cs_axiom_status(property_rights_override_sentience_claims, holdable).
narrative_ontology:cs_axiom_grounding('b6edfb07-3d3a-4b3a-8021-f2456923a174', property_rights_override_sentience_claims, conventional).
narrative_ontology:cs_reference_frame('b6edfb07-3d3a-4b3a-8021-f2456923a174', property_law_supremacy).
narrative_ontology:cs_drift_state('b6edfb07-3d3a-4b3a-8021-f2456923a174', contemporary_welfare_institutional_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b6edfb07-3d3a-4b3a-8021-f2456923a174', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, property_owners_and_users).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, extractive_industries).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, animal_property_doctrine).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, human_exceptionalism).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, property_rights_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use animals as resources (food production, labor, research, entertainment) without moral constraint on the use itself — only on efficiency and waste minimization. The property reading licenses this use as ontologically unproblematic: animals have no independent moral standing, so their use is not a rights violation. Benefits accrue directly through resource extraction, labor, and market value.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, property_owners_and_users, beneficiary,
    institutional, generational, arbitrage, universal).

% Agricultural, pharmaceutical, research, and entertainment sectors operationalize the property reading daily. The doctrine's affirmation that animals lack independent moral status removes a primary class of objections to their use. Benefits from the constraint: legal immunity for use practices, market structures built on animal commodity pricing, research methodologies that depend on unrestricted animal subjects.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, extractive_industries, beneficiary,
    institutional, generational, arbitrage, global).

% Analyzes and contests the property reading from outside it. Observes how the constraint structures the boundaries of permissible moral consideration. Cannot change the reading from within its own framework (animals cannot achieve standing by internal logic); must appeal to alternative readings or extra-framework authority.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_ethicists_welfare_advocate, observer,
    moderate, biographical, constrained, global).

% Would argue that the property status itself is the violation — that sentience or capacity to suffer grounds independent moral standing regardless of property assignment. Are excluded from the property reading's epistemic frame: the reading precludes their objection by ontological fiat (animals cannot have standing under this framework). Their position does not register as a legitimate alternative within the constraint's own authority structure.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, abolitionist_theorists, excluded,
    moderate, biographical, constrained, global).

% Codifies and enforces the property reading through property law, agricultural regulation, and research ethics frameworks. Sets the interpretive boundaries: animals are treated as owned entities whose use is constrained only by property norms (waste prevention, owner's prudent management), not by independent moral status. Maintains the constraint through legal and institutional recognition.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, regulatory_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Not authorized a seat in the property reading's framework by definition. Their sentience, suffering, or preferences are not epistemic facts that ground moral claims; they are biological facts external to the constraint's logic. They are mentioned here for completeness but do not author or defend the constraint.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animals_in_property_regimes, excluded,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(animal_moral_status__property_reading, animals_in_property_regimes).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable legal category (property/resource status) that permits intensive use of animal bodies and capacities without requiring continuous moral justification. Enables large-scale agricultural, research, and entertainment systems by settling the ontological question: animals are not moral patients, therefore their use is not inherently problematic.
% TRANSFER_FUNCTION: Transfers decision authority over animal bodies and lives from animals themselves (in any counterfactual rights-bearing frame) to property owners and regulatory authorities. Enables value extraction from animal labor, bodies, and biological systems that would be impermissible if animals held independent moral standing.
% ABSENT_VOICES: Animals themselves (excluded by definition from the reading's epistemic authority). Abolitionist theorists and deep ecologists (excluded from the property frame's legitimacy structure — they would argue the property status itself is the violation, but that argument is structurally foreclosed by the reading). Potential future frameworks granting animals moral standing (not yet institutionalized).
% DISAPPEARANCE_RATIONALE: If the property reading disappeared and animals were reconceptualized as morally considerable beings with independent standing, the entire apparatus of animal use would require ethical redesign. Agricultural systems, research methodologies, property law, and extractive industries would face immediate constraints on use, not merely on waste. The constraint's disappearance would not restore a pre-constraint state but would reorganize vast institutional structures around a different ontology.
% FOUNDING_PROBLEM: How should human societies organize the use of animals for food, labor, materials, and knowledge? The property reading solves this by categorizing animals as resources whose use requires no moral justification beyond property norms — efficiency, waste prevention, and owner welfare.
% FOUNDING_PROBLEM_CORROBORATION: Property owners and extractive industries attest the property reading is necessary to make animal-use industries economically and operationally feasible. Regulatory authorities cite it as the foundation of agricultural and research law. Animal ethicists and abolitionists outside the property frame explicitly contest both the problem framing and the solution — they argue the founding problem is a false premise, that animal sentience grounds moral standing independent of property assignment. Philosophers of animal studies cite this reading as a dominant historical framework now under systematic challenge from welfare and rights-based alternatives.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__property_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_moral_status__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_moral_status__property_reading),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_moral_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint claims to be an ontological fact, not a human-constructed extraction mechanism. If animals genuinely lack moral standing, then using them is not extractive in the sense of taking something wrongfully — no rights are violated. Suppression is also low (0.12) because the constraint does not actively suppress alternatives within the property frame; rather, it precludes their formulation. The accessibility_collapse is very high (0.92) because once the property reading is accepted as ontological fact, alternative readings (that animals have moral standing) collapse as logical impossibilities — they are not alternatives but category errors within this frame. Resistance is low (0.22) because the constraint persists mainly through institutional and legal entrenchment, not through active defense against powerful challengers. Theater_ratio is low (0.08) because the constraint is straightforward — there is little performative pretense, just straightforward assertion of property rights and economic use. The measurement series shows slight creeping upward in extractiveness (0.15→0.18) and rising suppression_requirement (0.08→0.12) over the interval, suggesting that as alternative readings (welfare, abolitionist) gain institutional traction, the property reading must do more active work to maintain itself — an incipient piton signature. Theater rises sharply in the projected period (0.05→0.14) as regulatory compromise language emerges: 'humane' use, 'animal welfare,' language that performs concern for animal interests while maintaining property status.
 *
 * PERSPECTIVAL GAP:
 *   From the property-owner and extractive-industry seats, this constraint is genuinely a mountain — an ontological fact about what animals are, not a human-designed extraction mechanism. From the abolitionist and animal-ethicist seats, this constraint is a false summit — a doctrine that benefits identifiable powerful parties (property owners, industries) while claiming naturalness. The engine computes this divergence per seat from the structural data: beneficiaries get low χ (the constraint subsidizes them, or is merely ontological); non-beneficiaries get higher χ. The gap is not a defect — it is the signal the corpus measures.
 *
 * DIRECTIONALITY LOGIC:
 *   The property_owners_and_users and extractive_industries seats compute as full beneficiaries (d→0.0) because they collect use rights and resource extraction without moral constraint on use itself. The regulatory_apparatus computes as aligned with beneficiaries (moderate d) because it maintains and enforces the property reading. Animal ethicists and abolitionists compute as partial targets (moderate-to-high d) because they bear the opportunity cost: they cannot advocate for animals' moral standing within this frame without category violation. Animals themselves are excluded from moral standing by definition, so they do not author a seat — they appear as the excluded-by-principle non-agent. The directionality structure is not hidden: beneficiaries enjoy unrestricted property rights; everyone else has constrained options within the property frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The property reading does not exhibit classic mandatrophy structure (founding problem solved, arrangement persists). The founding problem (how to organize animal use) remains contested: beneficiaries say the property reading solves it definitively; abolitionists say it misframes the problem entirely. The measurement series suggest an incipient shift: as welfare and rights-based readings gain institutional space, the property reading shifts from straightforward statement to defended position. Rising theater_ratio and suppression_requirement indicate the constraint is doing more work to maintain itself against alternative readings. This is not mandatrophy (the founding problem is not resolved) but rather a contested kernel under pressure from sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the property status of animals an ontological fact (animals naturally lack moral standing, independent of human frameworks) or a constructed legal/philosophical doctrine (humans assigned property status to animals for resource convenience)?',
    'Philosophical analysis of what ''natural'' moral standing would consist in (sentience-based? agency-based? species membership?). Historical reconstruction of how property doctrine emerged relative to animal sentience science. Cross-cultural comparison of alternative moral ontologies for animals.',
    'If animals'' lack of moral standing is natural/discovered, the property reading is a mountain — extractiveness is legitimately low because the constraint reflects irreducible structural fact. If property status is constructed/assigned, the low extractiveness measurement masks a false summit — the constraint benefits identifiable parties (property owners, extractive industries) while claiming naturalness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether the property reading reflects discovered ontological fact or constructed human doctrine.').

omega_variable(
    sentience_and_moral_standing_boundary,
    'Does sentience (capacity to feel pleasure and pain) entail any independent moral standing, or can sentience be morally inert when the sentient being is property?',
    'Philosophical argument: do we grant that sentient beings have ANY moral claim simply by virtue of sentience, or only claims constrained by property norms? Cross-reference with regulatory frameworks (welfare law) that DO recognize animal suffering as morally relevant while maintaining property status.',
    'If sentience entails at least minimal moral standing (even within property constraints), the property reading is incomplete and the abolitionist reading''s premise has traction. If sentience can be ontologically trumped by property assignment, the property reading is stable. The welfare reading occupies the middle position: sentience-based constraints on use, but property status maintained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sentience_and_moral_standing_boundary, conceptual, 'Whether sentience grounds any independent moral claim or can be entirely subordinated to property status.').

omega_variable(
    foreclosure_vs_coexistence_with_abolitionism,
    'Does the property reading logically foreclose the abolitionist reading (they cannot both be true in any coherent framework), or do they coexist as competing metaphysical/legal positions?',
    'Formal logical analysis: does ''animals have no independent moral standing'' entail ''animals cannot have independent moral standing''? Or is the disagreement about framework choice, not logical necessity?',
    'If foreclosure is structural (reading_relations: forecloses), the property reading and abolitionist reading cannot be held simultaneously by any single institutional authority. If they coexist (reading_relations: coexists_with), different jurisdictions and institutions can adopt different readings. This affects how institutional change happens: foreclosure requires one framework to win; coexistence permits pluralism or territorial division.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence_with_abolitionism, conceptual, 'Logical relationship between the property reading and abolitionist alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t20, animal_moral_status__property_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__property_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement_basis(anim_tr_t40, observed).
narrative_ontology:measurement(anim_tr_t60, animal_moral_status__property_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement_basis(anim_tr_t60, projected).
narrative_ontology:measurement(anim_tr_t80, animal_moral_status__property_reading, theater_ratio, 80, 0.11).
narrative_ontology:measurement_basis(anim_tr_t80, projected).
narrative_ontology:measurement(anim_tr_t100, animal_moral_status__property_reading, theater_ratio, 100, 0.14).
narrative_ontology:measurement_basis(anim_tr_t100, projected).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t20, animal_moral_status__property_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__property_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(anim_be_t40, observed).
narrative_ontology:measurement(anim_be_t60, animal_moral_status__property_reading, base_extractiveness, 60, 0.19).
narrative_ontology:measurement_basis(anim_be_t60, projected).
narrative_ontology:measurement(anim_be_t80, animal_moral_status__property_reading, base_extractiveness, 80, 0.2).
narrative_ontology:measurement_basis(anim_be_t80, projected).
narrative_ontology:measurement(anim_be_t100, animal_moral_status__property_reading, base_extractiveness, 100, 0.18).
narrative_ontology:measurement_basis(anim_be_t100, projected).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__property_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t20, animal_moral_status__property_reading, suppression_requirement, 20, 0.09).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__property_reading, suppression_requirement, 40, 0.11).
narrative_ontology:measurement_basis(anim_su_t40, observed).
narrative_ontology:measurement(anim_su_t60, animal_moral_status__property_reading, suppression_requirement, 60, 0.13).
narrative_ontology:measurement_basis(anim_su_t60, projected).
narrative_ontology:measurement(anim_su_t80, animal_moral_status__property_reading, suppression_requirement, 80, 0.15).
narrative_ontology:measurement_basis(anim_su_t80, projected).
narrative_ontology:measurement(anim_su_t100, animal_moral_status__property_reading, suppression_requirement, 100, 0.12).
narrative_ontology:measurement_basis(anim_su_t100, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(animal_moral_status__property_reading, 0.05).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__welfare_reading).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_moral_status kernel has three structurally distinct readings: property_reading (this constraint), welfare_reading, and abolitionist_reading. Each reading instantiates a different constraint with different ε values and beneficiary/victim structures. The property reading grounds its authority in property-law lineage and treats animals as ontologically unproblematic resources. The welfare reading acknowledges sentience as morally relevant but maintains property status with constraints on use. The abolitionist reading treats property status itself as the violation. The three constraints are linked via network.affects_constraints because they all interpret the same kernel (what is the moral status of animals?) but in mutually exclusive ways at the institutional level. A jurisdiction cannot simultaneously implement all three — only one reading's interpretation of the kernel can hold authority in any single legal/regulatory framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
