% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: Constitutional Secularism — Reformist Reading (Affirmative State Duty to Eliminate Oppressive Religious Practice)
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint is the reformist reading of the constitutional secularism
 *   kernel: the claim that the state carries an affirmative duty to eliminate
 *   religious practices that oppress marginalized groups, and that this duty
 *   supersedes religious-community autonomy claims wherever the two conflict.
 *   It is the most extractive of the three readings of this kernel — it does
 *   not merely permit intervention (the principled_intervention_reading) or
 *   require equal distance (the strict_neutrality_reading), it mandates
 *   active dismantlement of specific doctrinal practices whenever the
 *   affirmative-duty test is met. The doctrine has demonstrable coordination
 *   value for scheduled castes and women previously locked out of ritual life
 *   and religious office, and demonstrable extraction from religious
 *   institutions and their conservative constituencies, whose control over
 *   what counts as 'essential' to their own faith is transferred to state
 *   courts. This story generates only the reformist reading as a clean,
 *   ε-invariant constraint per Rule 1; the sibling readings are separate
 *   constraints linked via network.affects_constraints, not alternative
 *   measurements of this one.
 *
 * KEY AGENTS:
 *   - scheduled_caste_worshippers: primary beneficiary (powerless/trapped) — gains ritual access and institutional inclusion
 *   - women_seeking_religious_office_and_access: primary beneficiary (powerless/constrained) — gains office and ritual access previously barred
 *   - reformist_judiciary: agenda_setter (institutional/analytical) — sets and administers the affirmative-duty doctrine
 *   - state_reform_apparatus: agenda_setter/beneficiary (institutional/analytical) — enforces and gains jurisdiction
 *   - religious_conservatives_across_communities: primary target (organized/constrained) — bears doctrinal and institutional loss
 *   - temple_and_denominational_trust_administrators: primary target (organized/constrained) — loses administrative control
 *   - minority_religious_institutions_claiming_autonomy: secondary target (moderate/constrained) — loses autonomy shield
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.68).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.71).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "Constitutional Secularism — Reformist Reading (Affirmative State Duty to Eliminate Oppressive Religious Practice)").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, '77471839-15e8-47a6-b051-9b07e6b07e1c').
narrative_ontology:cs_kernel_codification('77471839-15e8-47a6-b051-9b07e6b07e1c', fixed_text).
narrative_ontology:cs_authority_grounding('77471839-15e8-47a6-b051-9b07e6b07e1c', lineage).
narrative_ontology:cs_interpretation_layer_present('77471839-15e8-47a6-b051-9b07e6b07e1c').
narrative_ontology:cs_reading_relation('77471839-15e8-47a6-b051-9b07e6b07e1c', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('77471839-15e8-47a6-b051-9b07e6b07e1c', constitutional_secularism__principled_intervention_reading, influences).
narrative_ontology:cs_axiom('77471839-15e8-47a6-b051-9b07e6b07e1c', foundational, equality_and_dignity_override_group_autonomy).
narrative_ontology:cs_axiom_status(equality_and_dignity_override_group_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('77471839-15e8-47a6-b051-9b07e6b07e1c', equality_and_dignity_override_group_autonomy, deontological).
narrative_ontology:cs_axiom('77471839-15e8-47a6-b051-9b07e6b07e1c', foundational, state_duty_to_intervene_is_affirmative_not_discretionary).
narrative_ontology:cs_axiom_status(state_duty_to_intervene_is_affirmative_not_discretionary, holdable).
narrative_ontology:cs_axiom_grounding('77471839-15e8-47a6-b051-9b07e6b07e1c', state_duty_to_intervene_is_affirmative_not_discretionary, conventional).
narrative_ontology:cs_reference_frame('77471839-15e8-47a6-b051-9b07e6b07e1c', post_independence_anti_untouchability_settlement).
narrative_ontology:cs_drift_state('77471839-15e8-47a6-b051-9b07e6b07e1c', contemporary_jurisdictional_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('77471839-15e8-47a6-b051-9b07e6b07e1c', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_caste_worshippers).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_seeking_religious_office_and_access).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, reformist_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, state_reform_apparatus).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservatives_across_communities).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, temple_and_denominational_trust_administrators).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, minority_religious_institutions_claiming_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically excluded from temple entry, priesthood, and full ritual participation. The reformist reading gives courts and legislatures a constitutional mandate to strike down exclusionary practices as untouchability or discrimination rather than protected religious doctrine. Cannot exit the religious community without losing social and material ties, so the affirmative duty is often their only route to inclusion.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_caste_worshippers, beneficiary,
    powerless, generational, trapped, national).

% Barred by custom from entering certain shrines, holding certain offices, or performing certain rites during proscribed life stages. The reformist duty authorizes courts to override these exclusions as constitutionally impermissible discrimination, notwithstanding religious denomination claims to manage their own affairs.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_seeking_religious_office_and_access, beneficiary,
    powerless, generational, constrained, national).

% Interprets constitutional provisions on equality and untouchability as imposing an affirmative duty on the state to intervene, using tests like 'essential religious practice' to override religious-autonomy claims when they intersect with caste or gender exclusion. Sets and administers the doctrine; not personally exposed to the costs it imposes on religious institutions.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, reformist_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Legislatures and administrative bodies pass and enforce temple-entry acts, endowment boards, and anti-discrimination statutes under this reading's mandate. Gains legitimacy and expanded regulatory jurisdiction over religious institutions as the enforcing arm of the affirmative duty.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, state_reform_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__reformist_reading, state_reform_apparatus, beneficiary).

% Hold that certain exclusions are essential doctrinal practice, not discrimination, and that the state's affirmative duty framing strips their community of self-governance. Can litigate and organize politically but cannot exit the jurisdiction of the constitutional order; every doctrinal defense must be litigated in a forum (the courts) that has already adopted the framework working against them.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_conservatives_across_communities, payer,
    organized, generational, constrained, national).

% Manage endowments, appointments, and ritual calendars under increasing statutory and judicial oversight. Face loss of administrative control, financial penalty, or removal when practices are found to violate the affirmative duty; can appeal but cannot decline the state's jurisdiction over what counts as 'essential' to their own faith.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, temple_and_denominational_trust_administrators, payer,
    organized, biographical, constrained, regional).

% Argue that minority-community autonomy protections were meant as a shield against majoritarian and state interference, and that the reformist reading's supersession logic erodes that shield community by community. Politically weaker than the state and unable to opt out of constitutional review of their internal practices.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, minority_religious_institutions_claiming_autonomy, payer,
    moderate, generational, constrained, national).

% The constitutional provisions on equality, untouchability abolition, and religious freedom that the reformist reading interprets as jointly compelling affirmative state action. Not an actor; provides the textual surface all readings contest.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, constitutional_text_and_precedent, observer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(constitutional_secularism__reformist_reading, constitutional_text_and_precedent).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__reformist_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_secularism__reformist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state power behind the elimination of specific, named forms of religious exclusion (untouchability, caste-based temple bars, gender-based ritual exclusion) that would otherwise persist indefinitely under a norm of religious non-interference, by giving courts and legislatures a standing constitutional mandate rather than requiring case-by-case political mobilization each time.
% TRANSFER_FUNCTION: Moves ritual access, institutional authority over doctrine, and administrative control of religious property from religious governing bodies and their conservative constituencies to the state and to the previously excluded groups the state's intervention benefits.
% ABSENT_VOICES: Ordinary lay adherents who hold the excluded practice as central to felt religious meaning but lack organizational capacity to litigate are rarely heard directly — their views arrive filtered through denominational leadership, which has its own institutional interest in resisting oversight and may not represent the median adherent.
% DISAPPEARANCE_RATIONALE: If the affirmative-duty doctrine disappeared, temple-entry rulings, endowment oversight, and anti-untouchability enforcement grounded in it would lose their constitutional footing overnight; scheduled-caste and women's access claims would revert to relying on ordinary anti-discrimination statute or political majorities in each legislature, a much weaker and slower-moving guarantee, while religious institutions would regain de facto control over internal exclusion practices.
% FOUNDING_PROBLEM: Untouchability and caste-based exclusion from temples, along with gender-based ritual exclusion, persisted as 'internal religious matters' under a pure non-interference norm, leaving constitutionally promised equality unenforceable wherever custom and doctrine could be invoked to block it.
% FOUNDING_PROBLEM_CORROBORATION: Scheduled-caste rights organizations and feminist legal scholars outside the judiciary attest the exclusion problem remains substantially live in many regions and that the affirmative duty is still doing real work. Independent comparative constitutional scholars and minority-rights advocates, who are not parties benefiting from either the state's expanded jurisdiction or the excluded groups' gains, note the doctrine has also become a vehicle for state assertion of authority over religious administration well beyond the original caste/gender exclusion cases — a genealogy neither the state nor the beneficiary groups have strong incentive to flag themselves.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__reformist_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored as substantial (0.68 at interval end) and rising because the doctrine's application has expanded from narrow untouchability cases toward broader assertions of state authority over religious administration — this is the accumulation pattern the temporal series traces. Suppression is high (0.71) because the doctrine's persistence depends on courts continuing to override religious-autonomy claims by active judicial and administrative enforcement, not on voluntary compliance. Theater is low-moderate (0.22) because the coordination function (eliminating caste and gender exclusion) is largely genuine and not merely performative, even as the doctrine's scope creeps. Accessibility collapse is moderate (0.48): religious institutions retain some doctrinal argument space and appellate avenues, so alternatives have not fully collapsed. Resistance is high (0.79), reflecting sustained organized pushback from religious conservative constituencies across multiple communities.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist judiciary's seat, this reading is the fulfillment of a constitutional promise unfulfilled under pure non-interference. From the religious conservative and trust-administrator seats, the identical doctrine appears as majoritarian-legitimated seizure of internal community governance under an equality rhetoric that the community itself never consented to as the frame for adjudicating its own doctrine. The engine computes this divergence from the structural power/exit data on each seat; the claim itself does not resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Scheduled-caste worshippers and women seeking access are structural beneficiaries with low derived d — the doctrine subsidizes their inclusion, though their exit options (trapped/constrained) mean they cannot simply leave the religious community to escape exclusion absent the doctrine, which strengthens the case for state intervention as their only lever. Religious conservatives and trust administrators are structural targets with high derived d — they bear the transfer of authority and cannot exit the constitutional jurisdiction that adjudicates their internal practices. The reformist judiciary and state apparatus set the terms without bearing the costs, an analytical/institutional seat with derived d near zero on this constraint despite not literally 'benefiting' financially — their institutional authority and legitimacy expand.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (untouchability and gender exclusion as unenforceable 'internal religious matters') retains genuine corroborated life in many contexts, which argues against treating this as pure mandatrophy — the doctrine still does real anti-discrimination work. But the founding_problem_status is authored as contested precisely because the doctrine's scope has visibly expanded beyond the original caste/gender exclusion cases into broader assertions of jurisdiction over religious administration generally, a expansion that outside comparative-law observers flag as running ahead of the original justifying problem. Classifying this as tangled_rope rather than snare or mountain preserves both readings: real coordination benefit for the named beneficiary groups, and real, actively enforced extraction from religious institutions — exactly the hybrid the tangled_rope category exists to hold without collapsing into either pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reformist_reading_is_one_of_three,
    'Is the reformist reading (affirmative duty superseding autonomy) the correct account of what the constitutional secularism kernel requires, or do the strict_neutrality_reading and principled_intervention_reading better capture the kernel''s actual commitment?',
    'No empirical resolution mechanism exists; this is a live doctrinal and political dispute adjudicated through constitutional litigation, legislative action, and scholarly argument, not through data. Track which reading commands majority judicial opinion across successive constitutional benches as an institutional (not truth) indicator.',
    'If the strict_neutrality_reading is adopted instead, courts would decline to override religious autonomy on affirmative-duty grounds, sharply reducing extraction from religious institutions but also removing the primary current lever for scheduled-caste and women''s access claims. If principled_intervention_reading prevails, intervention becomes permissive and case-specific rather than a standing affirmative duty, producing a materially less extractive, less suppression-dependent constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_reading_is_one_of_three, conceptual, 'Which of the three kernel readings the constitutional order actually instantiates is unsettled and contested across judicial eras.').

omega_variable(
    essential_practice_test_manipulability,
    'Is the ''essential religious practice'' test that operationalizes the affirmative duty a principled doctrinal instrument, or is it manipulable enough that courts effectively decide outcomes first and reason to the test''s application afterward?',
    'Comparative analysis of case outcomes against articulated doctrinal reasoning across a large sample of rulings; look for correlation between outcome direction and post-hoc reasoning quality.',
    'If the test is substantially manipulable, the doctrine''s extractiveness is understated by metrics that treat judicial reasoning as a neutral filter — actual extraction may be closer to unconstrained state discretion dressed in doctrinal language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(essential_practice_test_manipulability, empirical, 'Whether the operationalizing legal test constrains outcomes or merely rationalizes them.').

omega_variable(
    scope_creep_from_original_mandate,
    'Has the affirmative duty doctrine''s application meaningfully expanded beyond its founding cases (untouchability, gender-based temple exclusion) into broader state assertion of authority over religious administration generally?',
    'Longitudinal case-law tracing: compare the doctrinal basis and remedy scope of early founding-era cases against contemporary applications; look for cases where the affirmative-duty rationale is invoked absent a caste or gender exclusion fact pattern.',
    'If scope creep is confirmed, the mandatrophy signal strengthens: the doctrine increasingly serves state jurisdictional expansion rather than the founding anti-discrimination purpose, supporting reclassification pressure toward snare in its most expanded applications while the narrow original applications remain closer to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_creep_from_original_mandate, empirical, 'Whether the doctrine''s application has drifted beyond its founding justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__reformist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t8, constitutional_secularism__reformist_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(cons_tr_t16, constitutional_secularism__reformist_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement(cons_tr_t24, constitutional_secularism__reformist_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(cons_tr_t32, constitutional_secularism__reformist_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__reformist_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__reformist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cons_be_t8, constitutional_secularism__reformist_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(cons_be_t16, constitutional_secularism__reformist_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(cons_be_t24, constitutional_secularism__reformist_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(cons_be_t32, constitutional_secularism__reformist_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__reformist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__reformist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cons_su_t8, constitutional_secularism__reformist_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(cons_su_t16, constitutional_secularism__reformist_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(cons_su_t24, constitutional_secularism__reformist_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(cons_su_t32, constitutional_secularism__reformist_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__reformist_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__principled_intervention_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language concept 'constitutional secularism' per the ε-invariance principle: strict_neutrality_reading (state maintains equal distance, minimal intervention, lowest extractiveness), principled_intervention_reading (state may intervene for reform, moderate extractiveness), and this reformist_reading (state has affirmative duty superseding autonomy, highest extractiveness). Each reading has its own stable ε and its own beneficiary/victim structure derived from the same underlying constitutional text; they are linked, not merged, because measuring 'the' constitutional secularism doctrine one way versus another yields materially different ε values — exactly the signal that indicates decomposition rather than a single observer-relative story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
