% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__sovereignty_guarantor_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity â Sovereignty Guarantor Reading
 *   domain: political/institutional
 *
 * SUMMARY:
 *   The EU Council unanimity rule in sovereignty-implicating domains (tax,
 *   foreign policy, enlargement, treaty revision) as read through the
 *   sovereignty-guarantor lens: each member state retains an absolute veto,
 *   ensuring that no state can be coerced into surrendering sovereignty by a
 *   majoritarian coalition of larger states. This reading frames the veto not
 *   as a bargaining chip but as a constitutional guarantee of Westphalian
 *   equality within the integration project. Small states are the primary
 *   beneficiaries; large states bear the coordination costs of slower
 *   decision-making but are not structurally extracted from because consent
 *   is a right, not a commodity. This reading competes with sibling readings
 *   that emphasize diplomatic capital accumulation and veto-trap extraction.
 *
 * KEY AGENTS:
 *   - small_member_states: Primary beneficiary (moderate/constrained) â veto subsidizes sovereignty against majoritarian coercion
 *   - large_member_states: Agenda setter (powerful/constrained) â majoritarian capacity is constitutionally checked, not extracted
 *   - european_commission: Institutional observer (institutional/analytical) â proposes and monitors, advocates for QMV expansion
 *   - european_parliament: Excluded voice (institutional/constrained) â sidelined in intergovernmental unanimity domains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.36).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.2).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity â Sovereignty Guarantor Reading").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "political/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, 'ebf65265-3040-4d92-9f44-05f147e48e14').
narrative_ontology:cs_kernel_codification('ebf65265-3040-4d92-9f44-05f147e48e14', formalized).
narrative_ontology:cs_authority_grounding('ebf65265-3040-4d92-9f44-05f147e48e14', lineage).
narrative_ontology:cs_interpretation_layer_present('ebf65265-3040-4d92-9f44-05f147e48e14').
narrative_ontology:cs_reading_relation('ebf65265-3040-4d92-9f44-05f147e48e14', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_reading_relation('ebf65265-3040-4d92-9f44-05f147e48e14', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_axiom('ebf65265-3040-4d92-9f44-05f147e48e14', foundational, veto_as_sovereignty_right).
narrative_ontology:cs_axiom_status(veto_as_sovereignty_right, holdable).
narrative_ontology:cs_axiom_grounding('ebf65265-3040-4d92-9f44-05f147e48e14', veto_as_sovereignty_right, deontological).
narrative_ontology:cs_axiom('ebf65265-3040-4d92-9f44-05f147e48e14', foundational, state_equality_irreducible).
narrative_ontology:cs_axiom_status(state_equality_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('ebf65265-3040-4d92-9f44-05f147e48e14', state_equality_irreducible, conventional).
narrative_ontology:cs_reference_frame('ebf65265-3040-4d92-9f44-05f147e48e14', westphalian_sovereign_equality).
narrative_ontology:cs_drift_state('ebf65265-3040-4d92-9f44-05f147e48e14', post_lisbon_enhanced_cooperation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ebf65265-3040-4d92-9f44-05f147e48e14', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains an absolute veto in sovereignty-implicating Council domains including tax, foreign policy, enlargement, and treaty revision. The veto blocks majoritarian outcomes that would override national preferences, effectively subsidizing their sovereignty against the structural power of larger states. Formal exit from the EU is available but economically and politically prohibitive.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    moderate, generational, constrained, continental).

% Participates in the same unanimity regime but primarily experiences the constraint as a procedural brake on its capacity to drive collective action. While retaining its own veto, its broader agenda-setting power is frequently checked by smaller states. It does not extract from the arrangement; rather, its majoritarian capacity is constitutionally limited by the consent requirement.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, agenda_setter,
    powerful, generational, constrained, continental).

% Proposes legislation and monitors treaty compliance but does not vote in Council unanimity proceedings. It regularly advocates for QMV expansion and observes veto use as a constraint on its supranational agenda, analyzing whether blockages serve legitimate sovereignty claims or narrower national interests.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, european_commission, observer,
    institutional, generational, analytical, continental).

% Legislative body that is structurally excluded from intergovernmental unanimity domains. It consistently advocates for the community method and codecision expansion but lacks formal standing in the Council rooms where unanimity is exercised, leaving its preferences unrepresented in the sovereign-consent bargaining core.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, european_parliament, excluded,
    institutional, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents majoritarian coercion in sovereignty-implicating domains by requiring each member state's explicit consent to collective action, preserving Westphalian equality of states within the integration framework and preventing the structurally powerful from overriding the weak.
% TRANSFER_FUNCTION: Moves the power to block collective decisions from the majority to each individual state; in practice transfers policy autonomy back to the national level when consensus cannot be reached, at the cost of collective speed and scale.
% ABSENT_VOICES: Supranational federalists and qualified-majority advocates within the European Parliament and among academic integrationists are structurally excluded from the intergovernmental unanimity core; they would argue for majoritarian efficiency and codecision but are not seated in the Council bargaining room.
% DISAPPEARANCE_RATIONALE: If unanimity vanished overnight in sovereignty-implicating domains, small states would lose their constitutional shield against majoritarian integration, large states could drive collective decisions over national objections in tax, foreign policy, and enlargement, and the EU's constitutional identity would shift decisively toward a federal model.
% FOUNDING_PROBLEM: How to achieve deep economic and political integration among sovereign states of radically unequal size and power without smaller states being structurally dominated by larger ones in decisions implicating core sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by historians of European integration and the treaty negotiation records of Rome (1957) and subsequent accords, where Benelux and other small states explicitly conditioned participation on veto guarantees in sensitive domains; contemporary international-relations scholars outside the direct beneficiary states continue to treat majoritarian coercion as a live risk in the EU.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.36, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).
:- end_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.36) because while no systematic rent extraction occurs, there are real coordination costs: legislative delay, suboptimal policy equilibria, and occasional side-payments to unblock vetoes. Suppression is low (0.20) because the constraint does not actively suppress alternatives; enhanced cooperation, passerelle clauses, and treaty revision provide lawful exit valves. Theater ratio is low (0.22) but rising slightly as veto rhetoric becomes more performative in an era of functional bypass mechanisms. Accessibility collapse is moderate (0.50) because exiting the unanimity regime requires unanimous treaty revision, creating a constitutional lock-in. Resistance is moderate (0.40) reflecting persistent federalist and large-state advocacy for QMV expansion.
 *
 * PERSPECTIVAL GAP:
 *   A large-state seat (e.g., France or Germany) experiences the constraint as frustrating efficiency and blocking desirable integration; a small-state seat (e.g., Malta or Luxembourg) experiences it as essential protection against coercion. The engine computes this divergence from the same structural data: small states are declared beneficiaries with low directionality, while large states sit closer to the symmetric-to-moderate range as agenda-setters whose capacity is checked but who are not treated as extraction targets. The European Parliament computes as excluded, with no stake in the sovereign-consent bargain.
 *
 * DIRECTIONALITY LOGIC:
 *   Small states derive low directionality (near-beneficiary) because the constraint subsidizes their sovereignty by blocking majoritarian outcomes that would disadvantage them. Large states derive low-to-moderate directionality as agenda-setters who collectively administer the constraint; their d is not at the target end because the reading does not treat their constrained majoritarian capacity as extraction but as a legitimate constitutional limitation inherent to the integration bargain. The Commission and Parliament are analytical or excluded seats with neutral or indeterminate directionality toward this specific constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The unanimity rule was built to solve the problem of sovereign consent in an integration project among unequal states. That problem remains live â there has been no irreversible shift to majority rule in the domains covered, and small states continue to treat the veto as existentially important. Therefore mandatrophy is not declared resolved. The constraint is not a piton because it retains genuine coordination function (preventing coercive integration) and is not primarily performative; nor is it a snare because the reading denies that blocking extracts systematic rents from large states.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unanimity_to_extraction_boundary,
    'Does the practice of side-payments and package deals to unblock vetoes convert the unanimity rule from a sovereignty guarantee into a mechanism for implicit extraction by blocking states?',
    'Cross-temporal analysis of Council negotiation records and budgetary flows to identify systematic patterns of side-payment correlation with veto threats, corroborated by diplomat interviews and leaked negotiating texts.',
    'If side-payments are systematic and coerced, reclassification toward tangled_rope would be warranted; if sporadic and reciprocal, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimity_to_extraction_boundary, empirical, 'Whether veto use correlates with extractive side-payments').

omega_variable(
    supranational_drift_vs_formal_text,
    'Have enhanced cooperation, passerelle clauses, and other bypass mechanisms functionally eroded unanimity without formal treaty revision?',
    'Empirical mapping of legislative acts adopted via enhanced cooperation or flexibility mechanisms against formal unanimity domains over the measurement interval.',
    'If bypasses are extensive, the constraint''s effective scope has collapsed and it may be degrading toward a piton (theatrical maintenance of a bypassed rule); if limited, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_drift_vs_formal_text, empirical, 'Functional erosion of unanimity via bypass mechanisms').

omega_variable(
    majoritarian_alternative_feasibility,
    'Would a qualified majority voting regime in current unanimity domains produce systematically coercive outcomes for small states, or has integration depth made multi-level governance safe for majoritarianism?',
    'Counterfactual simulation and comparative analysis of QMV outcomes in non-unanimity domains to assess small-state loss rates, combined with survey data on small-state trust in large-state coalitions.',
    'If majoritarianism would be coercive, the founding problem remains live and the rope is justified; if safe, the constraint may be a scaffold whose sunset has been indefinitely delayed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(majoritarian_alternative_feasibility, conceptual, 'Whether majoritarian alternatives would genuinely coerce small states').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_council_unanimity_sov_tr_t0, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(eu_council_unanimity_sov_tr_t13, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 13, 0.08).
narrative_ontology:measurement(eu_council_unanimity_sov_tr_t26, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 26, 0.11).
narrative_ontology:measurement(eu_council_unanimity_sov_tr_t39, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 39, 0.15).
narrative_ontology:measurement(eu_council_unanimity_sov_tr_t52, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 52, 0.19).
narrative_ontology:measurement(eu_council_unanimity_sov_tr_t65, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 65, 0.22).

% Extraction over time
narrative_ontology:measurement(eu_council_unanimity_sov_be_t0, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(eu_council_unanimity_sov_be_t13, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 13, 0.27).
narrative_ontology:measurement(eu_council_unanimity_sov_be_t26, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 26, 0.3).
narrative_ontology:measurement(eu_council_unanimity_sov_be_t39, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 39, 0.32).
narrative_ontology:measurement(eu_council_unanimity_sov_be_t52, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 52, 0.34).
narrative_ontology:measurement(eu_council_unanimity_sov_be_t65, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 65, 0.36).

% Suppression requirement over time
narrative_ontology:measurement(eu_council_unanimity_sov_su_t0, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(eu_council_unanimity_sov_su_t13, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 13, 0.12).
narrative_ontology:measurement(eu_council_unanimity_sov_su_t26, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 26, 0.14).
narrative_ontology:measurement(eu_council_unanimity_sov_su_t39, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 39, 0.17).
narrative_ontology:measurement(eu_council_unanimity_sov_su_t52, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 52, 0.19).
narrative_ontology:measurement(eu_council_unanimity_sov_su_t65, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 65, 0.21).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, identity_coordination).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% This story isolates the sovereignty-guarantor reading of the eu_council_unanimity kernel. Sibling readings isolate distinct structural claims: diplomatic_capital_reading (consensus-building function) and veto_trap_reading (minoritarian extraction). The kernel decomposes into three constraints because each reading carries a different epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
