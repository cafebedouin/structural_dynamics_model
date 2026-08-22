% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment as Guarantor of Citizen-Soldier Capacity (Civic Virtue Reading)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story generates ONE reading of the contested Second Amendment
 *   kernel: the originalist civic-virtue reading, which holds that the
 *   founding-era 'militia' referenced in the amendment's prefatory clause
 *   meant the whole body of able-bodied citizens, not a specially organized
 *   or state-selected corps, and that the operative clause protects a
 *   citizen-soldier capacity essential to republican self-government and
 *   resistance to standing-army tyranny. This reading is distinct from the
 *   collective_security_reading (which reads the militia clause as
 *   conditioning the right on organized state-run defense, permitting broader
 *   regulation) and the individual_right_reading (which severs the right from
 *   any militia rationale entirely and grounds it in personal self-defense).
 *   Under the ε-invariance principle, these are three different constraints
 *   sharing a text, not one constraint measured three ways; each carries its
 *   own ε, beneficiary structure, and stakeholder set.
 *
 * KEY AGENTS:
 *   - political_community_as_self_governing_body: diffuse beneficiary of a structural check envisioned by this reading
 *   - originalist_legal_scholars: agenda-setters who supply and revise the historical interpretive account
 *   - civic_militia_tradition_advocates: organized promoters of citizen-soldier ideology
 *   - modern_standing_military_and_police_establishment: excluded institutional actor whose existence sits in tension with this reading's premise
 *   - unorganized_citizens_without_militia_participation: excluded from having their actual motives for arms ownership queried
 *   - constitutional_historians: analytical observers of the underlying, genuinely contested historical record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.28).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.22).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment as Guarantor of Citizen-Soldier Capacity (Civic Virtue Reading)").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional_law/political_theory/firearms_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, '2bfd828a-21bc-4aee-bb5e-599043df37b4').
narrative_ontology:cs_kernel_codification('2bfd828a-21bc-4aee-bb5e-599043df37b4', fixed_text).
narrative_ontology:cs_authority_grounding('2bfd828a-21bc-4aee-bb5e-599043df37b4', lineage).
narrative_ontology:cs_interpretation_layer_present('2bfd828a-21bc-4aee-bb5e-599043df37b4').
narrative_ontology:cs_reading_relation('2bfd828a-21bc-4aee-bb5e-599043df37b4', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('2bfd828a-21bc-4aee-bb5e-599043df37b4', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_axiom('2bfd828a-21bc-4aee-bb5e-599043df37b4', foundational, militia_denotes_universal_citizenry_not_select_corps).
narrative_ontology:cs_axiom_status(militia_denotes_universal_citizenry_not_select_corps, holdable).
narrative_ontology:cs_axiom_grounding('2bfd828a-21bc-4aee-bb5e-599043df37b4', militia_denotes_universal_citizenry_not_select_corps, empirically_contingent).
narrative_ontology:cs_axiom('2bfd828a-21bc-4aee-bb5e-599043df37b4', foundational, standing_armies_are_structural_threat_to_republican_liberty).
narrative_ontology:cs_axiom_status(standing_armies_are_structural_threat_to_republican_liberty, holdable).
narrative_ontology:cs_axiom_grounding('2bfd828a-21bc-4aee-bb5e-599043df37b4', standing_armies_are_structural_threat_to_republican_liberty, conventional).
narrative_ontology:cs_axiom('2bfd828a-21bc-4aee-bb5e-599043df37b4', secondary, right_is_instrumental_to_civic_defense_capacity_not_personal_defense).
narrative_ontology:cs_axiom_status(right_is_instrumental_to_civic_defense_capacity_not_personal_defense, holdable).
narrative_ontology:cs_axiom_grounding('2bfd828a-21bc-4aee-bb5e-599043df37b4', right_is_instrumental_to_civic_defense_capacity_not_personal_defense, instrumental).
narrative_ontology:cs_reference_frame('2bfd828a-21bc-4aee-bb5e-599043df37b4', founding_era_universal_militia_practice).
narrative_ontology:cs_drift_state('2bfd828a-21bc-4aee-bb5e-599043df37b4', post_militia_act_professionalization_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('2bfd828a-21bc-4aee-bb5e-599043df37b4', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, political_community_as_self_governing_body).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, civic_militia_tradition_advocates).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, originalist_legal_scholars).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, republican_self_government_requires_armed_citizenry).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, standing_armies_pose_threat_to_liberty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, the citizenry collectively is the entity the right is written to preserve: a body of armed, civically-engaged citizens capable of constituting the militia and thereby resisting both foreign invasion and domestic tyranny without dependence on a professional standing army. Benefits diffusely through the preservation of a structural check rather than through any transfer of resources.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, political_community_as_self_governing_body, beneficiary,
    organized, civilizational, analytical, national).

% Advance the civic-republican historical account through law review scholarship, amicus briefs, and judicial appointments, arguing that founding-era 'militia' meant the whole body of able-bodied citizens rather than a select, state-organized corps. They administer the interpretive frame by supplying its historical evidentiary basis and can revise it if new founding-era evidence emerges.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, originalist_legal_scholars, agenda_setter,
    institutional, generational, mobile, national).

% Civic organizations and political movements that invoke the citizen-soldier ideal to argue for widespread armed capacity as a civic duty and a bulwark of republican government. They promote training, organization, and readiness framed as public virtue rather than private convenience.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, civic_militia_tradition_advocates, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__originalist_civic_virtue_reading, civic_militia_tradition_advocates, agenda_setter).

% The professionalized military and police apparatus that has, in practice, supplanted the founding-era militia function this reading privileges. Their institutional existence sits uneasily with the reading's premise that citizen-soldiers should substitute for standing forces, but they are not part of the interpretive conversation this reading conducts and have no voice in it.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, modern_standing_military_and_police_establishment, excluded,
    institutional, generational, analytical, national).

% The vast majority of the modern citizenry who own firearms, if at all, for reasons unrelated to militia service or civic-defense readiness. This reading's core justification does not describe their actual relationship to arms, but they are swept into its beneficiary class by definitional fiat rather than by anything they've asserted about their own purposes.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, unorganized_citizens_without_militia_participation, excluded,
    powerless, biographical, constrained, national).

% Study founding-era militia statutes, debates, and practice to assess whether 'militia' denoted the general population or an organized, state-regulated body. Their historical findings are cited by all three sibling readings but adjudicated by none; this reading's account is one contested interpretation of a genuinely disputed historical record.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under the civic-virtue account, the amendment coordinates the maintenance of a distributed, citizen-based defense capacity so the polity is not dependent on a professional standing army, which founding-era thought regarded as a structural threat to republican liberty.
% TRANSFER_FUNCTION: This reading identifies no transfer of resources between named parties; it asserts a structural entitlement running from the constitutional text to the political community as a whole, realized through diffuse civic capacity rather than any specific redistribution.
% ABSENT_VOICES: The modern institutional military and police, whose existence this reading's founding logic arguably counsels against, are not consulted. Ordinary gun owners who hold arms for reasons wholly disconnected from civic-militia readiness are folded into the reading's beneficiary class without being asked whether that account describes them.
% DISAPPEARANCE_RATIONALE: Proponents of this reading would say its disappearance removes a structural check envisioned by the founders, altering the constitutional self-understanding of citizen relation to state power. Critics would say the citizen-soldier militia function is already historically defunct, superseded by the professional military centuries ago, so its formal removal from doctrine would change little in practice — hence the verdict is genuinely contested rather than settled either way.
% FOUNDING_PROBLEM: Founding-era political theory distrusted standing armies as instruments of tyranny and sought to preserve an alternative: a body of armed citizens capable of common defense and, if necessary, resistance to domestic usurpation, without creating a permanent professional military class.
% FOUNDING_PROBLEM_CORROBORATION: Military historians and defense-policy scholars outside the originalist legal tradition broadly attest that the citizen-militia function was rendered obsolete by 19th- and 20th-century military professionalization, federalization of the National Guard, and the rise of a standing volunteer military — a conclusion the civic-virtue reading's own proponents do not dispute as a matter of military history, distinguishing it from readings that would claim the function remains operationally live.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 at present) because this reading does not identify a transfer mechanism moving resources from an identifiable victim class to an identifiable beneficiary class — its coordination story runs on diffuse civic capacity, not rent extraction. Suppression is authored moderate-low (0.22): the reading does not require coercive enforcement against dissenters so much as it competes for interpretive dominance in courts and scholarship. Theater ratio is authored moderate-high (0.4) because the citizen-soldier function this reading defends has been substantially superseded by professional military and police institutions since the late 19th century — much of what the reading defends today is rhetorical and doctrinal rather than operational; the militia function it describes is not actually performed by most people it names as beneficiaries. Accessibility collapse is moderate (0.35): rival readings remain fully live and litigated, so alternatives have not collapsed. Resistance is moderate-high (0.55): the collective-security and individual-right readings both actively contest this account in courts, legislatures, and scholarship.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of originalist legal scholars and civic-militia advocates, this reading describes a structural safeguard of republican government that the modern world has allowed to atrophy through institutional neglect. From the seat of constitutional historians and the excluded institutional military, the citizen-soldier function this reading valorizes was rendered practically obsolete over a century ago, making the reading's contemporary invocation substantially performative — a claim about 1791 pressed into service to answer 2026 policy disputes it was not built to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   The political community as a whole is coded as beneficiary because the reading's entire justificatory structure runs through diffuse civic benefit rather than individual transfer. Originalist scholars and civic-militia advocates are coded as agenda-setters/beneficiaries because they actively construct and promote the interpretive account and derive professional and ideological standing from its success. No victim group is authored: this reading, unlike a regulatory or extraction-based constraint, identifies no party from whom resources or capacity are extracted to fund the benefit — its cost, if any, is opportunity cost in constitutional doctrine space, which is not a stakeholder harm in the sense this schema tracks. The excluded seats (military/police establishment, ordinary non-militia gun owners) are excluded from the conversation, not victimized by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem this reading answers to (distrust of standing armies, need for citizen-based common defense) is authored as dead by outside military-historical corroboration, while the reading itself persists and remains doctrinally active — a status/verdict combination the R5 apparatus is built to flag as a potential capture or zombie-mandate signal. This does not mean the reading should be dismissed: a rope whose founding coordination problem has lapsed can still perform a residual expressive or structural function (here, articulating a vision of citizen relation to state power) without that function being extractive. The classification task is to avoid two errors: mistaking a still-live civic-ideal function for pure theater, and mistaking honest historical obsolescence for active extraction. This reading's low extraction and absent victim class argue against snare or tangled_rope; its theater ratio and dead founding problem argue for caution against treating it as a live, functioning coordination mechanism rather than a substantially inertial one — closer to a scaffold whose transitional purpose expired without a formal sunset, though this reading does not claim a sunset clause and so is not authored as scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_militia_meaning_ambiguity,
    'Did founding-era usage of ''militia'' denote the entire able-bodied citizenry (this reading''s premise) or a more select, organized, state-regulated body subject to muster and training requirements (the collective_security_reading''s premise)?',
    'Comprehensive analysis of state militia statutes, muster rolls, and founding-era debates (Federalist/Anti-Federalist exchanges, state ratification debates) already exists but is genuinely contested among historians; no single additional document is likely to resolve it, though continued archival work narrows the range of defensible readings.',
    'If the select-and-organized reading is historically correct, this reading''s beneficiary structure (the whole political community) and its civic-virtue justification lose their historical grounding, and the constraint this file describes would need to be substantially revised or would collapse into the collective_security_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_militia_meaning_ambiguity, empirical, 'Contested founding-era meaning of ''militia'' underlying this reading''s entire premise.').

omega_variable(
    civic_function_obsolescence_vs_persistence,
    'Has the citizen-soldier civic function this reading protects actually lapsed into obsolescence (as the founding_problem_status of ''dead'' asserts), or does it persist in attenuated form through modern civilian firearms ownership, state defense forces, and unorganized militia statutes still on the books in most states?',
    'Survey of state unorganized militia statutes'' actual invocation and operational relevance; comparison of civic-militia rhetoric to any documented instances of citizen-soldier mobilization outside the formal National Guard structure in the last century.',
    'If the function persists in meaningful attenuated form, the theater_ratio authored here (0.4) overstates performativity and the reading is closer to a genuinely functioning (if diminished) rope; if the function is wholly formal, the theater_ratio may understate it and the reading trends toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_function_obsolescence_vs_persistence, empirical, 'Whether the civic-militia function is genuinely dormant or merely under-recognized.').

omega_variable(
    kernel_framing_under_determination,
    'Is the correct unit of analysis a single Second Amendment kernel with three competing readings (as authored here), or are the prefatory and operative clauses better modeled as two separable sub-kernels (a militia-clause kernel and a keep-and-bear-arms kernel) each with their own reading contests?',
    'Compare classification outcomes under the single-kernel/three-reading model against a two-sub-kernel decomposition; assess whether any reading''s classification changes under the alternative framing.',
    'If the two-sub-kernel decomposition is adopted, this reading might further fracture into a civic-virtue reading of the militia clause and a distinct reading of the operative clause''s scope, potentially changing its beneficiary set and vindicated propositions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the single-kernel three-reading model or a clause-level sub-kernel decomposition is the more structurally accurate framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1860, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1860, 0.1).
narrative_ontology:measurement(seco_tr_t1903, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1903, 0.3).
narrative_ontology:measurement(seco_tr_t1940, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1940, 0.4).
narrative_ontology:measurement(seco_tr_t1990, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2026, 0.4).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1791, 0.1).
narrative_ontology:measurement(seco_be_t1860, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1860, 0.12).
narrative_ontology:measurement(seco_be_t1903, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1903, 0.15).
narrative_ontology:measurement(seco_be_t1940, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1940, 0.18).
narrative_ontology:measurement(seco_be_t1990, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2008, 0.26).
narrative_ontology:measurement(seco_be_t2026, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2026, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_text__originalist_civic_virtue_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__individual_right_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'the Second Amendment right,' per the ε-invariance principle: collective_security_reading, individual_right_reading, and this file (originalist_civic_virtue_reading) share a contested kernel (second_amendment_text) but instantiate structurally distinct constraints with different beneficiary structures, different vindicated propositions, and different relationships to state regulatory authority. They are linked here rather than merged because averaging or hedging across their differing premises would violate DP-001 (ε-invariance): each reading's ε is a stable property of that reading's own account of the standing constitutional arrangement, not a blend across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
