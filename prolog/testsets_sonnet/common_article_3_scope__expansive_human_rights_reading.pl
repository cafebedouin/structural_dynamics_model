% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__expansive_human_rights_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: common_article_3_scope__expansive_human_rights_reading
 *   human_readable: Common Article 3 — Expansive Human-Rights-Floor Reading
 *   domain: international_humanitarian_law/legal
 *
 * SUMMARY:
 *   This story instantiates the expansive human-rights reading of the Common
 *   Article 3 scope kernel: CA3 applies as a humanitarian floor to any
 *   organized armed violence, regardless of how states or armed groups
 *   formally classify the situation. This is one of three structurally
 *   distinct readings of the same kernel text — the sibling readings
 *   (state-centric threshold reading, ICRC customary-practice reading) are
 *   separate constraint stories with their own ε values, beneficiary/victim
 *   sets, and classifications. This reading is authored as a clean,
 *   ε-invariant constraint on its own terms: broad applicability functions as
 *   genuine coordination (closing protection gaps) but also imposes real,
 *   asymmetric costs on state security apparatuses and armed group commanders
 *   who face expanded monitoring and prosecution exposure they did not
 *   consent to and cannot exit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.28).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.4).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "Common Article 3 — Expansive Human-Rights-Floor Reading").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "international_humanitarian_law/legal").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, '1cb65b04-7b6c-4b3c-b402-a0eac1bf0290').
narrative_ontology:cs_kernel_codification('1cb65b04-7b6c-4b3c-b402-a0eac1bf0290', fixed_text).
narrative_ontology:cs_authority_grounding('1cb65b04-7b6c-4b3c-b402-a0eac1bf0290', distributed).
narrative_ontology:cs_reading_relation('1cb65b04-7b6c-4b3c-b402-a0eac1bf0290', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('1cb65b04-7b6c-4b3c-b402-a0eac1bf0290', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('1cb65b04-7b6c-4b3c-b402-a0eac1bf0290', foundational, protection_independent_of_classification).
narrative_ontology:cs_axiom_status(protection_independent_of_classification, holdable).
narrative_ontology:cs_axiom_grounding('1cb65b04-7b6c-4b3c-b402-a0eac1bf0290', protection_independent_of_classification, deontological).
narrative_ontology:cs_axiom('1cb65b04-7b6c-4b3c-b402-a0eac1bf0290', secondary, humanitarian_floor_applies_below_intensity_threshold).
narrative_ontology:cs_axiom_status(humanitarian_floor_applies_below_intensity_threshold, holdable).
narrative_ontology:cs_axiom_grounding('1cb65b04-7b6c-4b3c-b402-a0eac1bf0290', humanitarian_floor_applies_below_intensity_threshold, deontological).
narrative_ontology:cs_reference_frame('1cb65b04-7b6c-4b3c-b402-a0eac1bf0290', post_1949_geneva_minimum_floor_consensus).
narrative_ontology:cs_drift_state('1cb65b04-7b6c-4b3c-b402-a0eac1bf0290', contemporary_counterterrorism_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1cb65b04-7b6c-4b3c-b402-a0eac1bf0290', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, detained_persons).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, civilian_populations_in_low_intensity_violence).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, human_rights_monitoring_bodies).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, international_criminal_tribunals).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_security_forces).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, counterinsurgency_ministries).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, non_state_armed_group_commanders).
narrative_ontology:constraint_vindicates(common_article_3_scope__expansive_human_rights_reading, universal_humanitarian_floor_doctrine).
narrative_ontology:constraint_vindicates(common_article_3_scope__expansive_human_rights_reading, protection_should_not_depend_on_classification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held by state or non-state forces in situations that a state-centric reading might not classify as armed conflict at all — riot suppression, contested policing operations, low-level insurgency. Under this expansive reading, they are entitled to the CA3 floor (no torture, no summary execution, humane treatment) regardless of how the violence is formally classified. They have no capacity to invoke the protection themselves; it depends entirely on external recognition and monitoring.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, detained_persons, beneficiary,
    powerless, immediate, trapped, national).

% Live in zones where organized armed violence occurs below the intensity/organization threshold a state-centric reading would require for CA3 to attach. Under the expansive reading they receive protection even in ambiguous, low-boil conflicts (gang warfare treated as organized violence, contested internal unrest) that would otherwise fall into a protection gap between human rights law and IHL.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, civilian_populations_in_low_intensity_violence, beneficiary,
    powerless, biographical, trapped, regional).

% Conduct counterinsurgency, internal security, and detention operations. Under the expansive reading, virtually any organized violence they engage in — even operations they characterize as ordinary law enforcement against criminal gangs — can be pulled into the CA3 floor, exposing personnel and commanders to external monitoring, fact-finding missions, and potential prosecution. They cannot exit the constraint; they can only contest its applicability case by case, which is itself costly.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_security_forces, payer,
    institutional, biographical, constrained, national).

% Set doctrine and rules of engagement. The expansive reading forces them to plan for the possibility that any sustained internal operation will be treated as covered by CA3, raising legal exposure and operational cost, and their preferred classification (law enforcement, not armed conflict) is treated as non-dispositive — their voice in the classification question is systematically discounted relative to monitoring bodies.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, counterinsurgency_ministries, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, counterinsurgency_ministries, excluded).

% Lead groups engaged in organized violence against the state. The expansive reading holds them to the same minimum humanitarian floor regardless of their group's formal recognition or the intensity of the specific engagement, foreclosing the argument that low-level or diffuse operations fall outside any humanitarian regulation at all.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, non_state_armed_group_commanders, payer,
    organized, biographical, constrained, regional).

% NGOs, UN special rapporteurs, and treaty bodies that apply and advocate for the expansive reading. They investigate, report, and press for accountability whenever organized violence occurs, treating classification disputes as attempts to evade the floor. Their institutional mandate and funding are tied to broad applicability of humanitarian protections; narrower readings shrink their jurisdiction.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, human_rights_monitoring_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Courts and tribunals that rely on the expansive reading to assert jurisdiction over conduct in ambiguous conflicts. A broad CA3 scope expands the universe of prosecutable violations and strengthens their authority to adjudicate internal violence that a state-centric reading would place outside international scrutiny.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_criminal_tribunals, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, international_criminal_tribunals, agenda_setter).

% Track evolving state practice and opinio juris to determine customary scope empirically rather than through either the expansive or state-centric a priori position. This reading treats their evidentiary, practice-based method as subordinate to the rights-floor argument — their voice would push toward a narrower, practice-verified scope in specific cases, but the expansive reading proceeds from the humanitarian floor principle regardless of what practice shows.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, icrc_customary_law_trackers, excluded,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, non-negotiable minimum standard of humane treatment that applies the moment organized armed violence occurs, so that protection does not depend on states and armed groups first agreeing on how to characterize the violence — closing the gap where classification disputes could otherwise leave victims with no applicable protection at all.
% TRANSFER_FUNCTION: Moves legal exposure, monitoring burden, and reputational risk from affected populations and detainees onto state security forces and non-state commanders; moves interpretive authority over what counts as 'covered violence' from the state's own classification apparatus to international monitoring bodies and tribunals.
% ABSENT_VOICES: Frontline security personnel facing prosecution risk for operations they characterize as ordinary law enforcement rarely have a seat in the treaty-interpretation process; ICRC customary-law trackers, whose practice-based method this reading effectively overrides in contested cases, are also structurally sidelined by the a priori rights-floor logic.
% DISAPPEARANCE_RATIONALE: If the expansive reading disappeared and only the state-centric threshold reading governed, a substantial class of low-intensity violence, contested internal unrest, and ambiguous detention operations would exit humanitarian scrutiny entirely — states and armed groups would gain a wide space to classify violence as 'below threshold' and thereby escape both monitoring and the floor obligations; detained persons in those gaps would lose their only applicable protection.
% FOUNDING_PROBLEM: Common Article 3 was drafted to ensure that internal, non-international armed conflicts — historically excluded from full Geneva Convention protection because states resisted external interference in internal affairs — would still be bound by minimum humanitarian rules; the expansive reading was built to prevent states from using classification disputes to evade even that minimum floor.
% FOUNDING_PROBLEM_CORROBORATION: ICRC commentaries and UN human rights bodies (parties that benefit from broad scope) attest the classification-evasion problem remains live, citing ongoing state practice of labeling internal violence as mere law enforcement to avoid IHL scrutiny. Independent corroboration comes from academic IHL scholarship documenting persistent state resistance to CA3 attachment in ambiguous conflicts (e.g. drug-war violence, prolonged internal unrest) — a pattern noted by scholars with no institutional stake in either monitoring-body budgets or state defense postures.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__expansive_human_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__expansive_human_rights_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__expansive_human_rights_reading_tests).
:- end_tests(common_article_3_scope__expansive_human_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and rising slowly (0.12 to 0.28 over the interval) — the expansive reading imposes real compliance costs on state and non-state armed actors as human rights monitoring infrastructure has matured since 1949, but it is not a rent-extraction mechanism in the classic sense; it redistributes legal exposure rather than resources. Suppression is moderate (0.4): the reading depends on active enforcement by monitoring bodies and tribunals to prevent states from re-classifying violence out of scope, and that enforcement pressure has intensified as international criminal law institutions matured (ICTY, ICTR, ICC jurisprudence). Theater ratio is low (0.15) — the coordination function (closing the protection gap) remains substantially real and functional, not merely performative. Resistance is high (0.72) because states persistently and vigorously contest CA3 attachment in ambiguous internal-violence situations — this is the central site of the kernel contest itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons and civilian populations in low-intensity violence zones are the clear beneficiaries: the expansive reading extends protection to them precisely in the gap cases a narrower reading would leave uncovered, and they have no capacity to exit or contest the classification themselves (trapped). State security forces, counterinsurgency ministries, and non-state commanders are the payers: they bear expanded monitoring, prosecution risk, and operational constraint, and their exit option is only 'constrained' — they can contest classification case by case but cannot escape the framework. Monitoring bodies and tribunals are structural beneficiaries in a second sense: broad scope is also broad jurisdiction for them, which is why their institutional interest is not neutral with respect to which reading prevails — this is worth flagging as a directionality consideration even though they are not victims of extraction themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — states evading minimum humanitarian obligations by disputing classification — remains live by the corroboration of independent IHL scholarship, not merely the say of the monitoring bodies that benefit from broad jurisdiction. This blocks a mandatrophy misread in either direction: the arrangement is not empty theater sustained only by institutional self-interest (the classification-evasion pattern is independently documented), nor is it costless coordination for state actors (real prosecution and monitoring exposure is the payer side). The classification as tangled_rope rather than pure rope reflects that genuine coordination (closing a protection gap) coexists with genuine, asymmetric cost imposed on a specific payer class through the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ca3_scope,
    'Is the expansive human-rights reading, the state-centric threshold reading, or the ICRC customary-practice reading the structurally correct account of CA3''s scope — or do all three persist as genuinely coexisting positions with no resolution mechanism?',
    'This is not resolvable by further legal argument alone; it would require either (a) a binding international judicial ruling authoritative across jurisdictions that settles scope definitively, or (b) convergent state practice over time that the customary reading could then certify — but the expansive reading''s normative claim (protection should not depend on classification) is partly a preference claim that empirical convergence cannot fully settle.',
    'If the state-centric reading were authoritative, the beneficiary set here (civilian populations and detainees in low-intensity/ambiguous violence) would substantially shrink, and state_security_forces'' payer burden would substantially decrease. If the ICRC customary reading were authoritative, scope would vary case-by-case based on practice evidence, producing a much narrower and less stable ε than either fixed-position reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ca3_scope, conceptual, 'Which reading of the CA3 scope kernel governs, and whether the three readings are genuinely irreducible coexisting positions.').

omega_variable(
    monitoring_body_institutional_interest,
    'To what extent does the expansive reading persist because it is the correct humanitarian-law interpretation, versus because human rights monitoring bodies and international tribunals have an institutional interest in maximal jurisdiction?',
    'Compare advocacy patterns and interpretive positions of monitoring bodies against jurisdictions/situations where broadening scope does NOT expand their own mandate or funding — if the expansive position is argued as consistently in low-visibility, low-funding contexts as in high-visibility ones, institutional interest is less explanatory.',
    'If institutional interest substantially drives the reading''s persistence independent of its interpretive merit, the beneficiary role of monitoring_bodies and tribunals shifts from incidental to load-bearing, and the coordination/extraction balance shifts further toward extraction (their jurisdiction is itself something they collect).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monitoring_body_institutional_interest, empirical, 'Whether the expansive reading''s persistence is explained by its interpretive merit or by monitoring-body institutional self-interest in broad jurisdiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1949, 0.08).
narrative_ontology:measurement(comm_tr_t1977, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1977, 0.09).
narrative_ontology:measurement(comm_tr_t1995, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1995, 0.11).
narrative_ontology:measurement(comm_tr_t2005, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2005, 0.13).
narrative_ontology:measurement(comm_tr_t2015, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2015, 0.14).
narrative_ontology:measurement(comm_tr_t2025, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1949, 0.12).
narrative_ontology:measurement(comm_be_t1977, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1977, 0.15).
narrative_ontology:measurement(comm_be_t1995, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1995, 0.19).
narrative_ontology:measurement(comm_be_t2005, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2005, 0.22).
narrative_ontology:measurement(comm_be_t2015, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2015, 0.26).
narrative_ontology:measurement(comm_be_t2025, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1949, 0.2).
narrative_ontology:measurement(comm_su_t1977, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1977, 0.25).
narrative_ontology:measurement(comm_su_t1995, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement(comm_su_t2005, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(comm_su_t2015, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(comm_su_t2025, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2025, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__expansive_human_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__icrc_customary_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the common_article_3_scope kernel, each authored as its own ε-invariant constraint with its own beneficiary/victim structure. The expansive reading has the broadest beneficiary set (any organized violence triggers protection) and the highest payer exposure for state security forces; the state-centric reading narrows both; the customary reading makes scope empirically contingent on practice rather than fixed by either a priori principle. All three are linked via affects_constraints because a shift in dominant interpretation in one jurisdiction's case law creates precedential pressure on how the others are argued elsewhere.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
