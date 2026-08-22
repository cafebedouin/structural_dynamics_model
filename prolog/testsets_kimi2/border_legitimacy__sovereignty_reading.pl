% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_reading, []).

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
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: Territorial Sovereignty Border Exclusion
 *   domain: political/international_law/migration
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty reading of the
 *   border_legitimacy kernel: the claim that border authority derives from
 *   territorial sovereignty and that the state possesses a legitimate right
 *   to exclude non-members. Under this reading, excluded migrants constitute
 *   the primary victim set, the sovereign state is the agenda-setting
 *   beneficiary, and citizens receive the putative coordination goods of
 *   bounded membership. The constraint exhibits high extractiveness toward
 *   those excluded, sustained by active enforcement. Sibling readings
 *   (freedom_of_movement, humanitarian_obligation) are separate constraints
 *   in the same family and are not folded into this classification per the
 *   Îµ-invariance principle.
 *
 * KEY AGENTS:
 *   - sovereign_state: Primary agenda-setter (institutional/constrained) â administers exclusion and claims territorial sovereignty legitimacy
 *   - excluded_migrants: Primary target (powerless/trapped) â bear the costs of border enforcement and territorial exclusion
 *   - citizens_residents: Beneficiary (organized/constrained) â receive bounded public goods and labor market protection
 *   - human_rights_observers: Analytical observer (analytical/analytical) â document harms and contest legitimacy from outside the sovereignty framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.78).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.82).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Territorial Sovereignty Border Exclusion").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political/international_law/migration").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, '30dce1ec-8250-4da7-a25a-b948411b568f').
narrative_ontology:cs_kernel_codification('30dce1ec-8250-4da7-a25a-b948411b568f', formalized).
narrative_ontology:cs_authority_grounding('30dce1ec-8250-4da7-a25a-b948411b568f', lineage).
narrative_ontology:cs_interpretation_layer_present('30dce1ec-8250-4da7-a25a-b948411b568f').
narrative_ontology:cs_reading_relation('30dce1ec-8250-4da7-a25a-b948411b568f', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_reading_relation('30dce1ec-8250-4da7-a25a-b948411b568f', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('30dce1ec-8250-4da7-a25a-b948411b568f', foundational, state_territorial_exclusivity).
narrative_ontology:cs_axiom_status(state_territorial_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('30dce1ec-8250-4da7-a25a-b948411b568f', state_territorial_exclusivity, conventional).
narrative_ontology:cs_axiom('30dce1ec-8250-4da7-a25a-b948411b568f', foundational, right_to_exclude_derived_from_sovereignty).
narrative_ontology:cs_axiom_status(right_to_exclude_derived_from_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('30dce1ec-8250-4da7-a25a-b948411b568f', right_to_exclude_derived_from_sovereignty, conventional).
narrative_ontology:cs_reference_frame('30dce1ec-8250-4da7-a25a-b948411b568f', westphalian_territorial_state).
narrative_ontology:cs_drift_state('30dce1ec-8250-4da7-a25a-b948411b568f', contemporary_globalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('30dce1ec-8250-4da7-a25a-b948411b568f', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, sovereign_state).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizens_residents).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the border regime and claims exclusive authority over territory through the Westphalian sovereignty framework. Enforces exclusion via visas, border patrols, detention, and deportation. Derives political legitimacy and territorial control from the constraint.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, sovereign_state, agenda_setter,
    institutional, generational, constrained, national).

% Subject to exclusion from the state's territory. Bear the direct costs of border enforcement, including detention, deportation, family separation, and loss of economic opportunity. Have no viable legal path to circumvent the state's exclusionary authority.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_migrants, payer,
    powerless, immediate, trapped, national).

% Receive the putative benefits of territorial sovereignty: access to public goods, labor markets, and political self-determination premised on bounded membership. Do not administer the constraint but constitute the constituency for whom exclusion is justified.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizens_residents, beneficiary,
    organized, biographical, constrained, national).

% Monitor border practices against international human rights standards. Document the harms of exclusion and contest the legitimacy of the sovereignty reading from outside the Westphalian framework. Neither bear nor collect from the constraint.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, human_rights_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Delimiting exclusive territorial jurisdiction to enable state capacity, public goods provision, and democratic self-determination for a bounded citizenry.
% TRANSFER_FUNCTION: Moves access to territory, labor markets, and political membership from would-be entrants to the sovereign state and its citizens; moves the costs of enforcement, detention, and deportation onto excluded migrants.
% ABSENT_VOICES: Excluded migrants are structurally absent from sovereignty-framed legitimacy debates; indigenous nations whose territories were partitioned by colonial borders are also excluded from the territorial framing.
% DISAPPEARANCE_RATIONALE: If territorial sovereignty and the associated right to exclude vanished overnight, the global state system would reorganize: jurisdiction would fragment or consolidate along different axes, citizenship regimes would collapse, and the current architecture of migration control would be replaced by open movement or alternative membership systems.
% FOUNDING_PROBLEM: The Thirty Years' War and the collapse of overlapping feudal and religious authorities in Europe, creating a need for a stable system of exclusive territorial jurisdiction to end chronic violent conflict.
% FOUNDING_PROBLEM_CORROBORATION: International relations historians attest the Westphalian origin. Humanitarian organizations and critical migration scholars contest that contemporary border enforcement addresses that original problem, arguing instead that it serves global labor stratification. Corroboration from outside the benefiting parties is split and politically charged.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__sovereignty_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint systematically transfers territory, labor market access, and membership goods away from would-be migrants toward the sovereign state and its citizenry. Suppression (0.82) is higher still because the arrangement depends on active enforcement â border patrols, visa regimes, detention, and deportation â rather than voluntary coordination. Theater_ratio (0.45) reflects that while enforcement is materially severe, an increasing share of border activity is performative sovereignty signaling that exceeds the marginal security contribution. Accessibility_collapse (0.80) captures the near-total closure of legal alternatives for most would-be entrants once the constraint is understood. Resistance (0.58) is moderate: migrant caravans, sanctuary movements, and legal challenges exist but are largely contained by the enforcement apparatus.
 *
 * PERSPECTIVAL GAP:
 *   The sovereign_state seat experiences the constraint as legitimate authority and necessary coordination; the excluded_migrants seat experiences it as violent extraction with no exit. Citizens_residents occupy a hybrid position, receiving coordination benefits while being largely shielded from costs. The engine computes this divergence from the structural data â the high extraction and suppression scores amplify the effective extraction for the powerless/trapped payer seat while dampening it for the institutional agenda setter.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (sovereign_state, citizens_residents) derive low directionality: the constraint subsidizes their territorial control and membership advantages. The excluded_migrants payer carries high directionality: they are the direct target of extraction. The asymmetry is reinforced by the exit gap â the state is constrained but institutionally powerful, while migrants are powerless and trapped. No directionality overrides are required because the structural derivation captures the relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â the Thirty Years' War and the collapse of overlapping feudal authority â is contested as still live. The constraint prevents mislabeling as pure coordination (Rope) because the extraction is asymmetric: citizens and the state collect bounded-jurisdiction benefits while migrants bear the full costs of exclusion. It prevents mislabeling as pure extraction (Snare) because the coordination function (state capacity, public goods, democratic boundaries) is structurally real, even if its magnitude is disputed. The Tangled Rope classification captures this hybridity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_kernel_reading,
    'This constraint is the sovereignty reading of the border_legitimacy kernel. How would classification shift if the freedom_of_movement reading were adopted instead?',
    'Compare this constraint with its sibling in the constraint family; the freedom_of_movement reading removes excluded_migrants from the victim set and reclassifies state enforcement as the payer.',
    'Would reclassify from extractive to liberatory from the migrant seat; the state seat would invert from beneficiary to payer, likely producing a computed type of Snare or Tangled Rope from the state perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_kernel_reading, conceptual, 'Sibling reading ambiguity for the border legitimacy kernel').

omega_variable(
    westphalian_naturalness,
    'Is territorial sovereignty a contingent constructed norm of international society or a necessary feature of legitimate political order?',
    'Historical comparative analysis of non-Westphalian political arrangements and their functional capacity to provide public goods and resolve violent conflict.',
    'If sovereignty is a necessary natural feature of political order, the constraint trends toward Mountain; if it is a constructed convention, it remains Tangled Rope or Snare depending on the coordination-extraction balance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(westphalian_naturalness, conceptual, 'Whether sovereignty is natural law or constructed convention').

omega_variable(
    enforcement_cost_distribution,
    'To what extent do citizens bear the costs of border enforcement (taxation, surveillance, internal checkpoints) versus externalizing all costs to migrants?',
    'Fiscal analysis of border enforcement budgets and empirical measurement of citizen liberty trade-offs arising from the sovereignty regime.',
    'If citizens bear significant costs, directionality for citizens shifts toward symmetric and the effective extraction differential narrows; if costs are fully externalized to migrants, the asymmetry is maximized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_distribution, empirical, 'Distribution of enforcement costs between citizens and migrants').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bord_tr_t6, border_legitimacy__sovereignty_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(bord_tr_t12, border_legitimacy__sovereignty_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(bord_tr_t18, border_legitimacy__sovereignty_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(bord_tr_t24, border_legitimacy__sovereignty_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement(bord_tr_t30, border_legitimacy__sovereignty_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__sovereignty_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(bord_be_t6, border_legitimacy__sovereignty_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(bord_be_t12, border_legitimacy__sovereignty_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(bord_be_t18, border_legitimacy__sovereignty_reading, base_extractiveness, 18, 0.7).
narrative_ontology:measurement(bord_be_t24, border_legitimacy__sovereignty_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(bord_be_t30, border_legitimacy__sovereignty_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__sovereignty_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bord_su_t6, border_legitimacy__sovereignty_reading, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(bord_su_t12, border_legitimacy__sovereignty_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(bord_su_t18, border_legitimacy__sovereignty_reading, suppression_requirement, 18, 0.76).
narrative_ontology:measurement(bord_su_t24, border_legitimacy__sovereignty_reading, suppression_requirement, 24, 0.8).
narrative_ontology:measurement(bord_su_t30, border_legitimacy__sovereignty_reading, suppression_requirement, 30, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept of border legitimacy decomposes into three structurally distinct constraints: the sovereignty reading (this file), which grounds exclusion in territorial sovereignty; the freedom of movement reading, which treats borders as presumptively illegitimate restrictions on a human right; and the humanitarian obligation reading, which admits a duty toward refugees while retaining exclusion for general economic migrants. Each has distinct beneficiary/victim structures and Îµ values. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
