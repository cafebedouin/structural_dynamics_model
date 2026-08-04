% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Taboo as Normative Prohibition on Total War
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the nuclear_taboo_reading of the
 *   total_war_possibility_space kernel. It treats the absence of total war
 *   since 1945 as the product of a constructed normative taboo that prohibits
 *   nuclear use and makes total war unthinkable as policy, independently of
 *   whether material capabilities would permit it. The taboo is enforced
 *   through the NPT, non-proliferation sanctions, no-first-use pledges, and a
 *   global discourse industry. The reading predicts the taboo weakens if norm
 *   entrepreneurs exit and that non-nuclear powers experience a different
 *   constraint structure than nuclear powers. This is one reading; sibling
 *   readings (deterrence_equilibrium, space_contraction) are modeled as
 *   separate constraints.
 *
 * KEY AGENTS:
 *   - nuclear_weapons_states: Agenda-setter/beneficiary (institutional/constrained exit) — maintains oligopoly and enforces non-proliferation
 *   - non_nuclear_weapons_states: Payer (moderate/constrained exit) — bears compliance costs and foregone deterrence option
 *   - sanctioned_proliferators: Payer (powerless/trapped exit) — bears maximal sanctions and isolation for defying taboo
 *   - norm_entrepreneurs: Beneficiary (moderate/mobile exit) — extracts status and resources from taboo maintenance discourse
 *   - extended_deterrence_clients: Dual-positioned beneficiary/payer (moderate/constrained) — receives security guarantee at cost of sovereign nuclear choice
 *   - regime_administrators: Agenda-setter (institutional/constrained) — enforces safeguards without independent rule-making power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.72).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.76).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo as Normative Prohibition on Total War").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, 'df2aada6-e693-4dc9-b6b6-47ce8bd59eeb').
narrative_ontology:cs_kernel_codification('df2aada6-e693-4dc9-b6b6-47ce8bd59eeb', formalized).
narrative_ontology:cs_authority_grounding('df2aada6-e693-4dc9-b6b6-47ce8bd59eeb', practice).
narrative_ontology:cs_interpretation_layer_present('df2aada6-e693-4dc9-b6b6-47ce8bd59eeb').
narrative_ontology:cs_reading_relation('df2aada6-e693-4dc9-b6b6-47ce8bd59eeb', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('df2aada6-e693-4dc9-b6b6-47ce8bd59eeb', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('df2aada6-e693-4dc9-b6b6-47ce8bd59eeb', foundational, total_war_prohibited_by_constructed_taboo).
narrative_ontology:cs_axiom_status(total_war_prohibited_by_constructed_taboo, holdable).
narrative_ontology:cs_axiom_grounding('df2aada6-e693-4dc9-b6b6-47ce8bd59eeb', total_war_prohibited_by_constructed_taboo, conventional).
narrative_ontology:cs_axiom('df2aada6-e693-4dc9-b6b6-47ce8bd59eeb', foundational, material_capability_irrelevant_to_normative_prohibition).
narrative_ontology:cs_axiom_status(material_capability_irrelevant_to_normative_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('df2aada6-e693-4dc9-b6b6-47ce8bd59eeb', material_capability_irrelevant_to_normative_prohibition, empirically_contingent).
narrative_ontology:cs_reference_frame('df2aada6-e693-4dc9-b6b6-47ce8bd59eeb', customary_non_use_framework).
narrative_ontology:cs_drift_state('df2aada6-e693-4dc9-b6b6-47ce8bd59eeb', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('df2aada6-e693-4dc9-b6b6-47ce8bd59eeb', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapons_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, sanctioned_proliferators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_clients).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_clients).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, nuclear_non_proliferation_treaty_legitimacy).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, humanitarian_prohibition_of_nuclear_weapons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals and permanent Security Council seats. Set the terms of the NPT as recognized nuclear weapons states, maintain exclusive rights to possess nuclear weapons under the treaty, and enforce non-proliferation through sanctions and diplomacy. Their security is guaranteed by arsenals they refuse to disband.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapons_states, agenda_setter,
    institutional, civilizational, constrained, global).

% Comprise the vast majority of NPT signatories. Accept intrusive IAEA safeguards, forgo the nuclear weapons option, and depend on the nuclear umbrella of allies or the non-use taboo for security. They bear the compliance costs of a regime that permanently categorizes them as non-nuclear.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapons_states, payer,
    moderate, generational, constrained, global).

% Scholars, advocacy networks, and diplomats who construct, propagate, and institutionalize the anti-nuclear norm. Their careers, funding, and influence depend on the taboo's continued salience. They organize conferences, publish research, and lobby for treaty adherence.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs, beneficiary,
    moderate, biographical, mobile, global).

% States that have pursued or acquired nuclear weapons outside the NPT framework or faced severe sanctions for suspected programs. They bear the highest costs of the taboo: economic strangulation, threat of preventive military action, and diplomatic pariah status. Their pursuit of deterrence is framed as taboo violation.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, sanctioned_proliferators, payer,
    powerless, immediate, trapped, national).

% Allies under the nuclear umbrella. They benefit from extended deterrence and thus from the taboo's general restraint, but they pay in constrained sovereignty: they host foreign weapons or bases, forego independent nuclear programs, and accept vulnerability to allied decision-making about nuclear use.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_clients, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_clients, payer).

% IAEA and NPT review conference secretariats that administer safeguards, conduct inspections, and organize review cycles. They enforce the technical and legal infrastructure of the taboo but lack independent authority to set rules or sanction violators without state consent.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, regime_administrators, agenda_setter,
    institutional, generational, constrained, global).

% International relations scholars and strategic analysts who study the nuclear taboo. They track compliance, debate causality, and assess whether non-use is driven by norms, deterrence, or unthinkability. They do not collect from or pay into the constraint directly.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, ir_analytical_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapons_states).
narrative_ontology:fixing_cost_class(total_war_possibility_space__nuclear_taboo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents nuclear total war by constructing a shared normative prohibition on first use and escalation, solving the collective survival problem in an anarchic system where mutual vulnerability alone might not prevent deliberate or accidental catastrophe.
% TRANSFER_FUNCTION: Moves security autonomy, technological sovereignty, and compliance burdens from non-nuclear states and aspiring proliferators to the non-proliferation regime and the recognized nuclear weapons states; moves professional status and institutional resources to norm entrepreneurs who maintain the prohibition discourse.
% ABSENT_VOICES: States and movements that view nuclear weapons as legitimate equalizers of conventional inferiority, and voices arguing that the taboo is a discriminatory oligopoly masquerading as humanitarian law; they are excluded from NPT review conference agenda-setting and from the core deterrence-taboo scholarly debate.
% DISAPPEARANCE_RATIONALE: If the taboo vanished overnight, nuclear use would re-enter strategic planning, extended deterrence alliances would fracture as clients pursued independent arsenals, the non-proliferation regime would collapse, and total war would return as a thinkable policy option — the international security architecture would reorganize around latent or active nuclear use.
% FOUNDING_PROBLEM: Prevention of nuclear total war in an anarchic international system after 1945, where technological breakthrough created the possibility of civilization-ending conflict in a single strike.
% FOUNDING_PROBLEM_CORROBORATION: NWS and humanitarian advocates attest the problem remains live, citing existential risk. Independent security studies scholars and many NNWS attest that while the existential risk is real, the specific arrangement has drifted into maintaining a discriminatory oligopoly; the Ban Treaty movement and critical IR scholarship corroborate the founding-problem-alive but arrangement-captured reading from outside the NWS beneficiary set.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the taboo permanently freezes a 1945 power distribution: five states hold nuclear weapons legally while all others accept intrusive inspections and a denied weapons option. Suppression (0.76) is higher still because the constraint depends on actively sanctioning defiers, threatening preventive war, and excluding alternative security architectures. Theater_ratio (0.50) reflects the increasing performativity of NPT review conferences and humanitarian initiatives that produce no disarmament progress but reproduce the regime's legitimacy. Accessibility_collapse (0.80) is high because, within the taboo framework, nuclear use is literally unthinkable for policymakers and proliferation is framed as rogue behavior rather than sovereign right. Resistance (0.45) is moderate: defiers exist but are isolated; the Ban Treaty movement offers institutional resistance but lacks enforcement power.
 *
 * PERSPECTIVAL GAP:
 *   The NWS seat experiences the taboo as a beneficial coordination mechanism it built and maintains for global survival; the NNWS and sanctioned-proliferator seats experience it as an enforced hierarchy that extracts their security autonomy. The norm-entrepreneur seat experiences it as a genuine moral achievement. The engine computes this divergence from the structural data — identical metrics produce different per-seat classifications based on directionality and power.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS and norm_entrepreneurs are structural beneficiaries (low d): the taboo subsidizes their status and security position. NNWS and sanctioned_proliferators are structural targets (high d): they pay the costs of compliance and exclusion. Extended_deterrence_clients sit near symmetric (d ≈ 0.5) because they receive a real coordination benefit (security guarantee) while paying sovereignty costs. The trapped exit of sanctioned proliferators amplifies their effective extraction; the mobile exit of norm entrepreneurs dampens theirs.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the taboo as pure extraction (snare) by preserving its genuine coordination function: it demonstrably reduces the probability of nuclear war, which is a real public good. It prevents mislabeling it as pure coordination (rope) by naming the asymmetric extraction: the five-state oligopoly, the coercion of non-nuclear states, and the capture of the regime by beneficiaries who resist disarmament. The founding problem (preventing nuclear war) is live, but the arrangement has drifted toward oligopoly maintenance, satisfying the tangled_rope gate rather than scaffold or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_divergence,
    'Does the absence of total war reflect a constructed normative taboo, a material deterrence equilibrium, or a contracted strategic possibility space?',
    'Comparative process-tracing of near-use crises (Cuban Missile Crisis, 1973 Yom Kippur War, 1983 Able Archer) to adjudicate whether decision-makers were constrained by norms, material costs, or cognitive unthinkability.',
    'If deterrence or unthinkability explains non-use better than taboo, this constraint dissolves into a different classification (rope or mountain-of-technology); if taboo is independently causal, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_divergence, conceptual, 'Structural ambiguity between normative, material, and cognitive readings of the nuclear age').

omega_variable(
    enforcement_vs_internalization,
    'Is the taboo''s persistence driven by coercive non-proliferation enforcement or by genuine normative internalization among elites?',
    'Elite interview studies and documentary analysis of nuclear decision-making in NWS and NNWS to separate fear of sanctions from belief in prohibition.',
    'If coercive enforcement dominates, suppression is higher than measured and the constraint leans toward snare; if internalization dominates, it leans toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_internalization, empirical, 'Coercive enforcement versus genuine normative internalization').

omega_variable(
    taboo_entrepreneur_dependency,
    'Does the taboo depend on continuous agency by norm entrepreneurs, or has it become self-sustaining through institutionalization?',
    'Track taboo strength across periods of high and low norm-entrepreneur mobilization (e.g., post-Cold War decline vs. Humanitarian Initiative surge).',
    'If entrepreneur-dependent, the constraint is a fragile tangled_rope or scaffold; if institutionalized, it is stable tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_entrepreneur_dependency, empirical, 'Norm entrepreneur dependency versus institutionalization of the taboo').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tota_tr_t16, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(tota_tr_t32, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(tota_tr_t48, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 48, 0.38).
narrative_ontology:measurement(tota_tr_t64, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 64, 0.45).
narrative_ontology:measurement(tota_tr_t80, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 80, 0.5).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(tota_be_t16, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 16, 0.32).
narrative_ontology:measurement(tota_be_t32, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 32, 0.46).
narrative_ontology:measurement(tota_be_t48, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 48, 0.58).
narrative_ontology:measurement(tota_be_t64, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 64, 0.66).
narrative_ontology:measurement(tota_be_t80, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 80, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(tota_su_t16, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(tota_su_t32, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement(tota_su_t48, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 48, 0.63).
narrative_ontology:measurement(tota_su_t64, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 64, 0.7).
narrative_ontology:measurement(tota_su_t80, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 80, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, space_contraction_reading).

% DUAL FORMULATION NOTE:
% The total_war_possibility_space kernel decomposes into three structurally distinct constraints: deterrence_equilibrium_reading (material balance), nuclear_taboo_reading (normative prohibition), and space_contraction_reading (cognitive unthinkability). Each has a different epsilon, beneficiary structure, and classification. They compete as explanations for the same observed phenomenon (absence of nuclear war since 1945).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
