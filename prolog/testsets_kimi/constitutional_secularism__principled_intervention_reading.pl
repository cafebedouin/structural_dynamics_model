% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__principled_intervention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__principled_intervention_reading, []).

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
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: State Intervention in Religious Affairs for Social Reform (Principled Intervention Reading)
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint instantiates the principled intervention reading of
 *   constitutional secularism: the state claims authority to intervene in
 *   religious affairs to advance social reform and protect weaker sections
 *   within communities. It is distinguished from strict neutrality
 *   (non-interference) by its affirmative authorization of state power into
 *   religious domains, and from the stronger reformist reading by its
 *   permissive rather than mandatory framing. The kernel contest centers on
 *   whether constitutional secularism requires distance from religion or
 *   permits differential treatment for reform ends. This reading expands
 *   state jurisdiction and exposes minority religious communities to
 *   asymmetric oversight risks, generating genuine seat divergence between
 *   protected weaker sections and autonomy-bearing religious institutions.
 *
 * KEY AGENTS:
 *   - state_reform_apparatus (agenda_setter, institutional): sets the rules of intervention and enforces through legislation and courts
 *   - weaker_sections_within_communities (beneficiary, powerless): receive protection from intra-community oppression
 *   - minority_religious_communities (payer, organized): bear costs of reduced autonomy and asymmetric intervention risk
 *   - conservative_religious_authorities (payer, organized): lose jurisdictional control over community practices
 *   - liberal_constitutional_advocates (observer, analytical): analyze and advocate for principled boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, 0.65).
domain_priors:suppression_score(constitutional_secularism__principled_intervention_reading, 0.6).
domain_priors:theater_ratio(constitutional_secularism__principled_intervention_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "State Intervention in Religious Affairs for Social Reform (Principled Intervention Reading)").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, 'f2565c3c-2f97-44ec-a62c-0bb615dd1f82').
narrative_ontology:cs_kernel_codification('f2565c3c-2f97-44ec-a62c-0bb615dd1f82', formalized).
narrative_ontology:cs_authority_grounding('f2565c3c-2f97-44ec-a62c-0bb615dd1f82', lineage).
narrative_ontology:cs_interpretation_layer_present('f2565c3c-2f97-44ec-a62c-0bb615dd1f82').
narrative_ontology:cs_reading_relation('f2565c3c-2f97-44ec-a62c-0bb615dd1f82', constitutional_secularism__strict_neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('f2565c3c-2f97-44ec-a62c-0bb615dd1f82', constitutional_secularism__reformist_reading, influences).
narrative_ontology:cs_axiom('f2565c3c-2f97-44ec-a62c-0bb615dd1f82', foundational, state_may_intervene_for_social_reform).
narrative_ontology:cs_axiom_status(state_may_intervene_for_social_reform, holdable).
narrative_ontology:cs_axiom_grounding('f2565c3c-2f97-44ec-a62c-0bb615dd1f82', state_may_intervene_for_social_reform, conventional).
narrative_ontology:cs_axiom('f2565c3c-2f97-44ec-a62c-0bb615dd1f82', foundational, weaker_section_protection_over_community_autonomy).
narrative_ontology:cs_axiom_status(weaker_section_protection_over_community_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('f2565c3c-2f97-44ec-a62c-0bb615dd1f82', weaker_section_protection_over_community_autonomy, deontological).
narrative_ontology:cs_reference_frame('f2565c3c-2f97-44ec-a62c-0bb615dd1f82', principled_interventionist_secularism).
narrative_ontology:cs_drift_state('f2565c3c-2f97-44ec-a62c-0bb615dd1f82', contemporary_majoritarian_politics, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f2565c3c-2f97-44ec-a62c-0bb615dd1f82', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, weaker_sections_within_communities).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, minority_religious_communities).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, conservative_religious_authorities).
narrative_ontology:constraint_vindicates(constitutional_secularism__principled_intervention_reading, constitutional_secularism_interventionist).
narrative_ontology:constraint_vindicates(constitutional_secularism__principled_intervention_reading, social_reform_through_state_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possesses constitutional and legislative authority to regulate religious practices in the name of social reform and weaker-section protection. Frames interventions as constitutional duty and progressive obligation. Cannot easily exit this role without constitutional amendment or reversal of judicial precedent; political costs attach to both intervention and non-intervention.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, state_reform_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Members of religious communities who face discriminatory practices such as caste discrimination or gender-discriminatory personal laws. Receive state-backed legal protections and potential relief from oppressive customs. Often economically and socially dependent on their communities; exit from community is costly or impossible.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, weaker_sections_within_communities, beneficiary,
    powerless, biographical, trapped, local).

% Religious minority groups whose communal autonomy is subject to state oversight and intervention. Bear the risk that reform-oriented intervention will be asymmetrically applied to their practices while majority-community practices are normalized or exempted. Cannot exit the nation-state framework; political resistance is constrained by minority status.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, minority_religious_communities, payer,
    organized, generational, constrained, national).

% Traditional religious leadership and institutions whose authority over personal law, custom, and community practice is eroded by state intervention. Lose jurisdictional control over community members. Exit is constrained because their authority is territorially and communally bound.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, conservative_religious_authorities, payer,
    organized, generational, constrained, regional).

% Constitutional scholars, human rights advocates, and public intellectuals who track the boundary between legitimate reform intervention and majoritarian overreach. They analyze jurisprudential trends and advocate for principled boundaries. Neither collect from nor bear the direct costs of the constraint.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, liberal_constitutional_advocates, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an external, state-backed enforcement mechanism to protect marginalized members from intra-community discrimination and oppressive practices when internal community checks have failed.
% TRANSFER_FUNCTION: Moves regulatory authority over religious and community practices from religious institutions and communal autonomy to state bodies and courts; moves protection and legal standing to weaker sections within communities.
% ABSENT_VOICES: Strict neutrality advocates who oppose any state entanglement with religion; minority community voices who fear majoritarian misuse but are marginalized in reform discourse; traditional authorities whose autonomy claims are overridden.
% DISAPPEARANCE_RATIONALE: If the intervention authority disappeared, community autonomy over personal law and custom would re-expand, weaker sections would lose state-backed protections against intra-community oppression, and the balance between religious freedom and reform would shift toward institutional pluralism without state oversight.
% FOUNDING_PROBLEM: Intra-community oppression of marginalized membersâsuch as caste discrimination within religious communities or gender-discriminatory personal lawsâwhere community self-regulation and internal reform failed to protect weaker sections.
% FOUNDING_PROBLEM_CORROBORATION: Social reform movements and marginalized group advocates attest the problem remains live from outside the state apparatus. Minority community representatives and strict neutrality scholars contest both the severity of the problem and the state's competence to solve it without majoritarian bias. Judicial and legislative records provide mixed corroboration, with significant variation across jurisdictions and time periods.
narrative_ontology:disappearance_verdict(constitutional_secularism__principled_intervention_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__principled_intervention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__principled_intervention_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__principled_intervention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__principled_intervention_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__principled_intervention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__principled_intervention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the substantial transfer of authority from religious communities to state institutions, with the risk of majoritarian capture amplifying asymmetric extraction. Suppression (0.60) captures the active suppression of religious autonomy claims and alternative community governance. Theater ratio (0.52 at interval end) acknowledges that a growing share of interventions are performative assertions of state supremacy rather than targeted reform. Accessibility collapse (0.70) is high because once the state claims intervention authority, community self-regulation alternatives collapse as legal options. Resistance (0.75) is high due to persistent religious and minority-community opposition. The temporal series shows extraction and theater rising as the intervention authority matures and politicizes over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The state reform apparatus experiences this constraint as a legitimate constitutional tool for progressive protection; weaker sections experience it as needed relief; minority religious communities and conservative authorities experience it as external domination and asymmetric extraction. The engine computes these divergent seat classifications from the structural dataâbeneficiaries versus victims, power levels, and exit optionsâwithout requiring a single authoritative perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus sits near the beneficiary end (d low) because the constraint subsidizes its authority and jurisdiction. Weaker sections also sit near beneficiary (d low) because the constraint is structurally designed to transfer protection to them. Minority religious communities and conservative authorities sit near the target end (d high) because the constraint extracts autonomy and jurisdictional control from them. The divergence between protected beneficiaries and autonomy-losing payers is the central structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by separating the genuine coordination function (protecting weaker sections from intra-community oppression, which is documented and real) from the extractive function (state authority expansion and majoritarian capture risk). A pure snare reading would ignore the documented benefits to marginalized group members; a pure rope reading would ignore the asymmetric autonomy costs and capture risks. Tangled rope captures both: the constraint solves a coordination problem for weaker sections while extracting from religious autonomy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_capture_ambiguity,
    'Does state intervention under this reading primarily protect weaker sections within communities, or has it been captured by majoritarian political interests to consolidate state control over minority religious practices?',
    'Comparative analysis of intervention patterns across majority- versus minority-community practices; outcome tracking for marginalized group members; political economy analysis of who initiates specific interventions and who benefits.',
    'If capture is dominant, the constraint shifts toward snare classification; if principled protection dominates, it remains tangled rope with substantiated coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_capture_ambiguity, empirical, 'Ambiguity between reformist protection and majoritarian capture').

omega_variable(
    intervention_efficacy,
    'Does state intervention in religious affairs produce measurably better outcomes for weaker sections compared to community-led reform or non-interference?',
    'Longitudinal outcome studies of marginalized group members in jurisdictions with varying intervention levels; comparison of social indicators pre- and post-intervention.',
    'If interventions are ineffective or harmful, the coordination story weakens and the constraint approaches pure extraction; if effective, the coordination function is substantiated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_efficacy, empirical, 'Whether state intervention actually improves outcomes for weaker sections').

omega_variable(
    framing_underdetermination,
    'Does the CS framing of this reading as grounded in constitutional lineage fully capture the authority structure, or does the constraint also depend on a diffuse epistemic claim that secular reason is inherently superior to religious authority?',
    'Discourse analysis of judicial and legislative justification patterns; tracking whether authority is claimed from constitutional text alone or from a broader modernist rationality narrative.',
    'If the latter, the authority_grounding shifts from lineage toward diffuse_epistemic, altering the foreclosure topology and drift predictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_underdetermination, conceptual, 'Whether the reading''s authority is purely textual or incorporates modernist rationality claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__principled_intervention_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__principled_intervention_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cons_tr_t8, constitutional_secularism__principled_intervention_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(cons_tr_t16, constitutional_secularism__principled_intervention_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(cons_tr_t24, constitutional_secularism__principled_intervention_reading, theater_ratio, 24, 0.47).
narrative_ontology:measurement(cons_tr_t32, constitutional_secularism__principled_intervention_reading, theater_ratio, 32, 0.51).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__principled_intervention_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__principled_intervention_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cons_be_t8, constitutional_secularism__principled_intervention_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(cons_be_t16, constitutional_secularism__principled_intervention_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(cons_be_t24, constitutional_secularism__principled_intervention_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(cons_be_t32, constitutional_secularism__principled_intervention_reading, base_extractiveness, 32, 0.64).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__principled_intervention_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__principled_intervention_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cons_su_t8, constitutional_secularism__principled_intervention_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(cons_su_t16, constitutional_secularism__principled_intervention_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(cons_su_t24, constitutional_secularism__principled_intervention_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(cons_su_t32, constitutional_secularism__principled_intervention_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__principled_intervention_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, reformist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_secularism kernel. The principled_intervention_reading authorizes state intervention for reform and protection, occupying a middle position between strict_neutrality_reading (non-interference) and reformist_reading (affirmative duty to eliminate oppression). Each reading has distinct epsilon, beneficiaries, and victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
