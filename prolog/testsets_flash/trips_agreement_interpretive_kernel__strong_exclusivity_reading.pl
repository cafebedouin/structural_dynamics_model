% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__strong_exclusivity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Agreement: Strong Exclusivity Reading
 *   domain: international_trade_law/public_health_policy/intellectual_property_regime
 *
 * SUMMARY:
 *   This constraint represents the 'strong_exclusivity_reading' of the TRIPS
 *   Agreement, which interprets the text as mandating high, uniform patent
 *   protections globally, with minimal scope for public health flexibilities.
 *   This reading prioritizes incentivizing pharmaceutical innovation through
 *   robust intellectual property rights. It is a contested interpretation,
 *   with a sibling 'public_health_flexibility_reading' emphasizing access to
 *   medicines.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.75).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.88).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Agreement: Strong Exclusivity Reading").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '821b1baa-3f70-44f2-bfa4-7b3045504fd4').
narrative_ontology:cs_kernel_codification('821b1baa-3f70-44f2-bfa4-7b3045504fd4', fixed_text).
narrative_ontology:cs_authority_grounding('821b1baa-3f70-44f2-bfa4-7b3045504fd4', lineage).
narrative_ontology:cs_interpretation_layer_present('821b1baa-3f70-44f2-bfa4-7b3045504fd4').
narrative_ontology:cs_reading_relation('821b1baa-3f70-44f2-bfa4-7b3045504fd4', trips_agreement_interpretive_kernel__public_health_flexibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('821b1baa-3f70-44f2-bfa4-7b3045504fd4', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('821b1baa-3f70-44f2-bfa4-7b3045504fd4', foundational, strong_ip_drives_innovation).
narrative_ontology:cs_axiom_status(strong_ip_drives_innovation, holdable).
narrative_ontology:cs_axiom_grounding('821b1baa-3f70-44f2-bfa4-7b3045504fd4', strong_ip_drives_innovation, empirically_contingent).
narrative_ontology:cs_axiom('821b1baa-3f70-44f2-bfa4-7b3045504fd4', secondary, patent_rights_are_absolute).
narrative_ontology:cs_axiom_status(patent_rights_are_absolute, holdable).
narrative_ontology:cs_axiom_grounding('821b1baa-3f70-44f2-bfa4-7b3045504fd4', patent_rights_are_absolute, deontological).
narrative_ontology:cs_reference_frame('821b1baa-3f70-44f2-bfa4-7b3045504fd4', maximal_patent_protection_framework).
narrative_ontology:cs_drift_state('821b1baa-3f70-44f2-bfa4-7b3045504fd4', post_doha_declaration_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('821b1baa-3f70-44f2-bfa4-7b3045504fd4', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_nations).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_developing_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from extended market exclusivity and high profits on patented medicines, globally. They actively lobby for strong IP enforcement and interpret TRIPS in favor of maximal protection.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Advocate for strong IP protection, aligning with their domestic pharmaceutical industries. They benefit from the global enforcement of these standards, which supports their knowledge-based economies.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_nations, beneficiary,
    institutional, generational, mobile, global).

% Bear the cost of high drug prices and face significant barriers to producing or importing affordable generic medicines, impacting public health budgets and access to care. Their ability to use TRIPS flexibilities is severely constrained by this reading.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states, payer,
    powerless, generational, trapped, national).

% Are direct victims of high drug prices, often lacking access to essential medicines due to patent monopolies. Their health outcomes are directly impacted by the strong exclusivity interpretation.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_developing_countries, payer,
    powerless, immediate, trapped, local).

% Are prevented from producing and distributing affordable versions of patented drugs, limiting their market access and ability to contribute to public health solutions. They face legal challenges and trade barriers under this interpretation.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_manufacturers, payer,
    moderate, biographical, constrained, global).

% Interpret the TRIPS Agreement and issue binding rulings, often favoring strong patent protection and narrow interpretations of flexibilities, backed by the threat of trade sanctions. They are the primary enforcers of this reading.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_panels, agenda_setter,
    institutional, generational, analytical, global).

% Actively campaign for broader interpretation of TRIPS flexibilities and greater access to medicines, but their direct influence on WTO dispute settlement is limited. They are often outside the formal decision-making processes.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__strong_exclusivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global baseline for intellectual property protection, aiming to coordinate innovation incentives and cross-border trade in knowledge-intensive goods.
% TRANSFER_FUNCTION: Transfers economic rents from consumers and national health systems (especially in developing countries) to pharmaceutical patent holders, in exchange for the promise of future innovation.
% ABSENT_VOICES: Public health advocates and patient groups, particularly from low-income countries, are largely excluded from the formal WTO dispute settlement processes where this reading is solidified. They would argue for a human rights-based approach to IP and public health.
% DISAPPEARANCE_RATIONALE: If the strong exclusivity reading of TRIPS vanished, many developing countries would immediately implement broader compulsory licensing and parallel import policies, leading to a rapid increase in generic drug availability and a significant restructuring of the global pharmaceutical market. Patent holders would face substantial revenue losses, and the WTO's enforcement authority in IP would be severely diminished.
% FOUNDING_PROBLEM: The problem of insufficient global intellectual property protection, leading to disincentives for innovation and widespread counterfeiting, particularly in pharmaceuticals.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is attested as 'live' by developed nations and pharmaceutical industry associations, citing ongoing R&D costs and the need for innovation. However, public health organizations, developing country governments, and independent economists provide corroboration that the problem is 'dead' or 'contested' in its current form, arguing that the current IP regime creates more problems than it solves for global health, as evidenced by high drug prices and limited access.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.75) is high due to the direct transfer of wealth from patients and states to patent holders through high drug prices. Suppression (0.88) is severe because the WTO dispute settlement mechanism, backed by trade sanctions, actively enforces these patent rights and limits the use of flexibilities like compulsory licensing. The theater ratio (0.15) is low, as the enforcement is direct and effective, not merely performative. Accessibility collapse is significant (0.7) as generic alternatives are largely blocked, and resistance (0.6) is notable from public health advocates and developing nations.
 *
 * PERSPECTIVAL GAP:
 *   Pharmaceutical patent holders and developed nations experience this as a legitimate coordination mechanism for innovation and trade. Low-income states and patients, however, experience it as a highly extractive and suppressive regime that limits access to essential medicines. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical patent holders and developed nations are clear beneficiaries (d near 0.0) as they directly profit from extended monopolies and the global enforcement of their IP. Low-income states, patients in developing countries, and generic drug manufacturers are victims (d near 1.0) due to high drug prices, limited generic competition, and constrained policy space for public health interventions. The WTO dispute panels act as agenda-setters, enforcing the strong exclusivity interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to incentivize innovation is still 'live' for its beneficiaries, but its status is 'contested' by victims who argue the problem of access to medicines has become more pressing than the problem of innovation incentives, or that the current regime over-incentivizes. The high extractiveness and suppression, coupled with the contested founding problem status, suggest a drift towards a snare, despite the claimed 'rope' (coordination) function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the TRIPS Agreement primarily a mechanism for strong patent protection or for public health flexibility?',
    'Evolution of WTO dispute settlement rulings and subsequent international agreements (e.g., Doha Declaration on TRIPS and Public Health) that clarify the scope of flexibilities.',
    'If the public_health_flexibility_reading gains dominance, the constraint''s effective extractiveness would decrease for low-income states and patients, and its classification would shift towards a more balanced rope or even a scaffold if flexibilities are temporary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is the ''strong_exclusivity_reading'' of the TRIPS Agreement interpretive kernel, which emphasizes high patent protection. The ''public_health_flexibility_reading'' is a sibling interpretation.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the high level of patent protection mandated by TRIPS a natural consequence of incentivizing innovation, or a constructed constraint benefiting identifiable agents?',
    'Empirical studies on the relationship between patent strength, R&D investment, and public health outcomes in different economic contexts.',
    'If primarily constructed, the ''mountain'' claim of innovation necessity is a false summit, and the constraint''s extractive nature is more clearly revealed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, empirical, 'Ambiguity between innovation incentive as natural law and patent protection as a constructed benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t0, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(trip_tr_t5, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(trip_tr_t10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(trip_be_t0, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(trip_be_t5, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(trip_be_t10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 10, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t0, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(trip_su_t5, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(trip_su_t10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 10, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the TRIPS Agreement interpretive kernel. Its sibling, 'public_health_flexibility_reading', offers a contrasting interpretation of the same text, leading to different beneficiary/victim structures and extractiveness profiles. Both are influenced by the 'dispute_settlement_interpretive_authority' constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
