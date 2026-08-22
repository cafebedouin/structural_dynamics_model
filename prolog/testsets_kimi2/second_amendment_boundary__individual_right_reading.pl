% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__individual_right_reading, []).

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
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   The Second Amendment's operative clause ('the right of the people to keep
 *   and bear Arms, shall not be infringed') is read by the individual-right
 *   reading as establishing a pre-existing individual right to possess
 *   firearms, principally for self-defense within the home. The prefatory
 *   clause ('A well regulated Militia, being necessary to the security of a
 *   free State') states a purpose but is grammatically and legally
 *   non-limiting under this reading. This interpretive constraint removes
 *   most regulatory options from state legislatures, shielding the firearms
 *   market from product-safety, licensing, and possession restrictions. The
 *   victim setâmass shooting victims, domestic violence victims, and
 *   firearm suicide victimsâbears the cost of the regulatory vacuum. The
 *   constraint is actively enforced by federal courts striking down gun laws
 *   under the Heller/McDonald/Bruen line. This is a kernel reading: the
 *   sibling militia-conditioned reading treats the prefatory clause as
 *   definitional, and the insurrectionist reading treats the right as
 *   instrumental to overthrow. This reading instantiates a commitment-system
 *   constraint grounded in fixed constitutional text, mediated by an
 *   originalist interpretive layer.
 *
 * KEY AGENTS:
 *   - Federal judiciary (institutional/constrained): agenda-setter enforcing the constitutional boundary
 *   - Individual gun owners (powerful/mobile): primary beneficiary, possession shielded
 *   - Firearms industry (powerful/mobile): beneficiary, market shielded from regulation
 *   - Gun rights organizations (organized/mobile): beneficiary with agenda-setting influence
 *   - State governments (institutional/constrained): payer, regulatory authority constrained
 *   - Mass shooting victims (powerless/trapped): payer, bear mortality costs
 *   - Domestic violence victims (powerless/trapped): payer, bear elevated homicide risk
 *   - Firearm suicide victims (powerless/trapped): payer, bear terminal cost of unregulated access
 *   - Gun control advocates (organized/constrained): excluded from constitutional legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.72).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.78).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment Individual Right Reading").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, '731ca8ae-3587-4de8-925b-72691607f3b4').
narrative_ontology:cs_kernel_codification('731ca8ae-3587-4de8-925b-72691607f3b4', fixed_text).
narrative_ontology:cs_authority_grounding('731ca8ae-3587-4de8-925b-72691607f3b4', lineage).
narrative_ontology:cs_interpretation_layer_present('731ca8ae-3587-4de8-925b-72691607f3b4').
narrative_ontology:cs_reading_relation('731ca8ae-3587-4de8-925b-72691607f3b4', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('731ca8ae-3587-4de8-925b-72691607f3b4', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('731ca8ae-3587-4de8-925b-72691607f3b4', foundational, operative_clause_independent_right).
narrative_ontology:cs_axiom_status(operative_clause_independent_right, holdable).
narrative_ontology:cs_axiom_grounding('731ca8ae-3587-4de8-925b-72691607f3b4', operative_clause_independent_right, conventional).
narrative_ontology:cs_axiom('731ca8ae-3587-4de8-925b-72691607f3b4', foundational, prefatory_clause_non_limiting).
narrative_ontology:cs_axiom_status(prefatory_clause_non_limiting, holdable).
narrative_ontology:cs_axiom_grounding('731ca8ae-3587-4de8-925b-72691607f3b4', prefatory_clause_non_limiting, conventional).
narrative_ontology:cs_reference_frame('731ca8ae-3587-4de8-925b-72691607f3b4', pre_existing_individual_right).
narrative_ontology:cs_drift_state('731ca8ae-3587-4de8-925b-72691607f3b4', post_bruen_resistance_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('731ca8ae-3587-4de8-925b-72691607f3b4', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, gun_rights_organizations).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, mass_shooting_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, domestic_violence_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, firearm_suicide_victims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, state_governments).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, originalism_jurisprudence).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, textualism_methodology).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, history_and_tradition_test).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the Second Amendment boundary, striking down state and federal firearm regulations that infringe the individual right. Bound by its own precedents and originalist methodology; justices can shift interpretation only through new majority formation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, constrained, national).

% Possess firearms under constitutional protection; the constraint shields acquisition and possession from prohibitory regulation. They vote and organize to maintain the interpretive framework.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, individual_gun_owners, beneficiary,
    powerful, biographical, mobile, national).

% Manufactures and sells firearms in a market shielded from product-safety regulation, licensing ceilings, and tort liability that would apply under a different constitutional reading. Profits from volume and market access.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_industry, beneficiary,
    powerful, biographical, mobile, national).

% Lobby, litigate, and mobilize public support to maintain and expand the individual right reading. Collect membership dues and exercise agenda-setting influence over judicial nominations and legislation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_rights_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__individual_right_reading, gun_rights_organizations, agenda_setter).

% Enact and enforce criminal and regulatory law within a shrinking zone of constitutional permission; comprehensive licensing, waiting periods, and categorical bans are struck down or chilled by pre-enforcement challenge.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, state_governments, payer,
    institutional, generational, constrained, national).

% Bear the mortality and injury costs of high-capacity magazine and assault-weapon availability that the constitutional shield prevents states from restricting.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, mass_shooting_victims, payer,
    powerless, immediate, trapped, local).

% Face elevated homicide risk because restraining-order-based firearm removals and domestic-violence misdemeanant prohibitions are litigated under strict constitutional scrutiny that narrows regulatory options.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, domestic_violence_victims, payer,
    powerless, immediate, trapped, local).

% Complete suicide using firearms acquired without waiting periods or red-flag interventions that the constitutional shield blocks; bear the terminal cost of unregulated access during acute crisis.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearm_suicide_victims, payer,
    powerless, immediate, trapped, local).

% Argue for regulatory alternatives but are structurally excluded from constitutional legitimacy under the individual right framework; their legislative victories are struck down or chilled as presumptive infringement.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_control_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__individual_right_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_boundary__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles a contested constitutional boundary by providing a stable, judicially enforceable rule that individuals may possess firearms for self-defense, reducing legal uncertainty for owners, manufacturers, and law enforcement about the outer limit of permissible state action.
% TRANSFER_FUNCTION: Transfers regulatory authority from state legislatures to individual possessors; transfers physical security risk from the general population, concentrated on the victim set, to the unregulated firearms market.
% ABSENT_VOICES: Victims of gun violence are structurally underrepresented in Second Amendment litigation because they rarely have standing in facial challenges. Public health experts are marginalized by the history-and-tradition interpretive framework. Gun control advocates participate in public debate but are excluded from constitutional legitimacy.
% DISAPPEARANCE_RATIONALE: If the individual right reading vanished overnight, state handgun bans and comprehensive licensing schemes would survive constitutional challenge, the firearms industry would reorganize around product safety and sales restrictions, digital and physical prices would shift under new compliance regimes, and individual owners would face new regulatory barriers to acquisition.
% FOUNDING_PROBLEM: Early republican fear of standing armies and tyranny, requiring armed citizens for collective defense; also protection of an individual right of self-defense.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding period attest to the anti-standing-army motivation from outside the benefiting parties. Public health researchers attest that modern self-defense needs do not require the current regulatory shield. Gun rights groups attest the problem is still live; gun control groups attest it is transformed by modern policing and military structure.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__individual_right_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.72) is high because the constraint blocks regulatory mechanismsâwaiting periods, safe-storage laws, assault-weapon bansâthat would reduce mortality, transferring physical security risk to the victim set. Suppression (0.78) is high because the constraint actively suppresses alternative regulatory regimes through judicial review and pre-enforcement striking, not merely by persuasion. Theater ratio (0.52) reflects the performative aspect of originalist history-and-tradition methodology, which selectively cites historical regulations while ignoring others. Accessibility collapse (0.75) is high because, once the individual right framework is accepted, comprehensive regulatory alternatives are constitutionally foreclosed. Resistance (0.70) is high because state legislatures and gun-control advocates actively contest the boundary through litigation and creative compliance. The temporal series show extraction and suppression jumping at Heller (t=28) and Bruen (t=42), with a slight pullback at Rahimi (t=45).
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary and gun-owning beneficiaries experience this constraint as a genuine rights-protecting coordination mechanism that settles constitutional uncertainty and restrains government overreach. The victim set and constrained state legislatures experience it as an enforced extraction of their physical security and regulatory sovereignty. The engine computes this divergence from the structural asymmetry in power, exit options, and directional position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (individual gun owners, firearms industry, gun rights organizations) have mobile exit and institutional or organized power; they sit near the full-beneficiary end (low d), so effective extraction is damped or inverted into subsidy for them. Victims (mass shooting victims, domestic violence victims, firearm suicide victims) are powerless and trapped; they sit near the full-target end (high d), so the same base extraction is amplified for them. State governments are institutional but constrained by judicial supremacy; they sit in the middle-high range. Gun control advocates are excluded from legitimacy and carry constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling as pure extraction because it carries a genuine coordination function: it resolves a contested constitutional boundary and provides a stable rule of law for millions of gun owners and law enforcement. However, the asymmetric victim costs prevent classifying it as pure coordination (Rope). The Tangled Rope classification captures that the same structure both coordinates rights and extracts security from identifiable victims. A Snare classification would be inaccurate because the rights-coordination story is not mere cover; a Rope classification would erase the victim set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the Second Amendment text inherently support the individual right reading, or is this reading one of several structurally viable constructions of the same kernel?',
    'Corpus linguistics analysis of 18th-century ''bear arms'' usage; examination of state ratifying convention records and pamphlet literature.',
    'If the text is structurally ambiguous, the individual right reading is a constructed constraint with high extraction rather than a uniquely correct interpretation; if the text is unambiguously individual, the reading is more naturally justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel text inherently supports this reading or permits siblings').

omega_variable(
    victim_causation_gap,
    'Does the constitutional shield against firearm regulation causally produce the victim harms, or would regulatory alternatives be ineffective even if permitted?',
    'Comparative international public health studies; natural experiments from state-level regulatory variation pre-Heller.',
    'If regulation would reduce harms, extraction is high and the victim set is structurally sound; if not, the victim set bears costs from a different cause and the constraint''s extraction is lower than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_causation_gap, empirical, 'Causal efficacy of blocked regulation on victim harms').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression purely structural (courts striking down laws) or is it also internalized (legislators self-censoring proposed regulations due to anticipated constitutional loss)?',
    'Legislative diary studies, interview data from state legislators about constitutional risk assessment, and bill-drafting records showing pre-emptive narrowing.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggestsâmany regulatory alternatives are abandoned before they reach a courtroom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__individual_right_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(seco_tr_t5, second_amendment_boundary__individual_right_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(seco_tr_t10, second_amendment_boundary__individual_right_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(seco_tr_t15, second_amendment_boundary__individual_right_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(seco_tr_t20, second_amendment_boundary__individual_right_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(seco_tr_t25, second_amendment_boundary__individual_right_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(seco_tr_t28, second_amendment_boundary__individual_right_reading, theater_ratio, 28, 0.38).
narrative_ontology:measurement(seco_tr_t30, second_amendment_boundary__individual_right_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(seco_tr_t35, second_amendment_boundary__individual_right_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(seco_tr_t40, second_amendment_boundary__individual_right_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(seco_tr_t42, second_amendment_boundary__individual_right_reading, theater_ratio, 42, 0.55).
narrative_ontology:measurement(seco_tr_t45, second_amendment_boundary__individual_right_reading, theater_ratio, 45, 0.52).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__individual_right_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(seco_be_t5, second_amendment_boundary__individual_right_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(seco_be_t10, second_amendment_boundary__individual_right_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(seco_be_t15, second_amendment_boundary__individual_right_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(seco_be_t20, second_amendment_boundary__individual_right_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(seco_be_t25, second_amendment_boundary__individual_right_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(seco_be_t28, second_amendment_boundary__individual_right_reading, base_extractiveness, 28, 0.65).
narrative_ontology:measurement(seco_be_t30, second_amendment_boundary__individual_right_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(seco_be_t35, second_amendment_boundary__individual_right_reading, base_extractiveness, 35, 0.7).
narrative_ontology:measurement(seco_be_t40, second_amendment_boundary__individual_right_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(seco_be_t42, second_amendment_boundary__individual_right_reading, base_extractiveness, 42, 0.75).
narrative_ontology:measurement(seco_be_t45, second_amendment_boundary__individual_right_reading, base_extractiveness, 45, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__individual_right_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(seco_su_t5, second_amendment_boundary__individual_right_reading, suppression_requirement, 5, 0.22).
narrative_ontology:measurement(seco_su_t10, second_amendment_boundary__individual_right_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(seco_su_t15, second_amendment_boundary__individual_right_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(seco_su_t20, second_amendment_boundary__individual_right_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(seco_su_t25, second_amendment_boundary__individual_right_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(seco_su_t28, second_amendment_boundary__individual_right_reading, suppression_requirement, 28, 0.65).
narrative_ontology:measurement(seco_su_t30, second_amendment_boundary__individual_right_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(seco_su_t35, second_amendment_boundary__individual_right_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(seco_su_t40, second_amendment_boundary__individual_right_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(seco_su_t42, second_amendment_boundary__individual_right_reading, suppression_requirement, 42, 0.8).
narrative_ontology:measurement(seco_su_t45, second_amendment_boundary__individual_right_reading, suppression_requirement, 45, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, insurrectionist_reading).

% DUAL FORMULATION NOTE:
% The second_amendment_boundary kernel decomposes into three structurally distinct readings: individual_right_reading (this file), militia_conditioned_reading, and insurrectionist_reading. Each reading has a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
