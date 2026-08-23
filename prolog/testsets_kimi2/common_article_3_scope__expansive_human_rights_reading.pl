% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: common_article_3_scope__expansive_human_rights_reading
 *   human_readable: Expansive Human Rights Reading of Common Article 3 Scope
 *   domain: international_law/humanitarian
 *
 * SUMMARY:
 *   This constraint story captures the expansive human-rights reading of
 *   Common Article 3 (CA3) of the 1949 Geneva Conventions, under which CA3
 *   applies to any organized armed violence as a floor of minimum
 *   humanitarian standards regardless of formal conflict classification. The
 *   reading treats CA3 as a universally applicable legal threshold that
 *   follows the facts of violence rather than state typologies, bringing
 *   counter-terrorism operations, low-intensity internal security campaigns,
 *   and other gray-zone violence under humanitarian law. It is one reading of
 *   a contested kernel; the sibling state-centric reading restricts CA3 to
 *   conflicts meeting intensity and organization thresholds, while the ICRC
 *   customary reading tracks scope through evolving state practice and opinio
 *   juris. The expansive reading structurally benefits detainees and affected
 *   populations by collapsing legal black holes, while asymmetrically
 *   extracting compliance costs, sovereignty, and prosecution exposure from
 *   state security operators. It requires active enforcement through
 *   international courts and monitoring bodies to persist against state
 *   resistance.
 *
 * KEY AGENTS:
 *   - detainees_and_affected_populations: Primary beneficiary (powerless/trapped) â receive protective floor
 *   - human_rights_advocacy_ngos: Secondary beneficiary (organized/mobile) â derive institutional purpose from norm expansion
 *   - state_security_operators: Primary payer/target (institutional/constrained) â bear sovereignty costs and prosecution risk
 *   - human_rights_monitoring_bodies: Agenda-setter (institutional/analytical) â construct expansive interpretation
 *   - international_criminal_courts: Agenda-setter (institutional/constrained) â adjudicate and enforce scope
 *   - non_state_armed_groups: Excluded voice (organized/constrained) â bound by rules but absent from norm-setting forums
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.58).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.55).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "Expansive Human Rights Reading of Common Article 3 Scope").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "international_law/humanitarian").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, 'c19c9491-5bf2-46c1-8986-a996b721fb1e').
narrative_ontology:cs_kernel_codification('c19c9491-5bf2-46c1-8986-a996b721fb1e', fixed_text).
narrative_ontology:cs_authority_grounding('c19c9491-5bf2-46c1-8986-a996b721fb1e', lineage).
narrative_ontology:cs_interpretation_layer_present('c19c9491-5bf2-46c1-8986-a996b721fb1e').
narrative_ontology:cs_reading_relation('c19c9491-5bf2-46c1-8986-a996b721fb1e', common_article_3_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('c19c9491-5bf2-46c1-8986-a996b721fb1e', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('c19c9491-5bf2-46c1-8986-a996b721fb1e', foundational, humanitarian_floor_universal).
narrative_ontology:cs_axiom_status(humanitarian_floor_universal, holdable).
narrative_ontology:cs_axiom_grounding('c19c9491-5bf2-46c1-8986-a996b721fb1e', humanitarian_floor_universal, deontological).
narrative_ontology:cs_axiom('c19c9491-5bf2-46c1-8986-a996b721fb1e', foundational, conflict_classification_irrelevance).
narrative_ontology:cs_axiom_status(conflict_classification_irrelevance, holdable).
narrative_ontology:cs_axiom_grounding('c19c9491-5bf2-46c1-8986-a996b721fb1e', conflict_classification_irrelevance, conventional).
narrative_ontology:cs_reference_frame('c19c9491-5bf2-46c1-8986-a996b721fb1e', geneva_conventions_protective_mandate).
narrative_ontology:cs_drift_state('c19c9491-5bf2-46c1-8986-a996b721fb1e', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c19c9491-5bf2-46c1-8986-a996b721fb1e', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, detainees_and_affected_populations).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, human_rights_advocacy_ngos).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_security_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons in the hands of parties to organized armed violence who receive minimum humanitarian guaranteesâprotection from murder, torture, cruel treatment, and unfair trialâregardless of whether the violence is classified as armed conflict, internal disturbance, or law enforcement. They cannot opt out of the framework and depend on external enforcement for its realization.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, detainees_and_affected_populations, beneficiary,
    powerless, immediate, trapped, universal).

% Advocacy organizations that campaign for expansive CA3 application and derive institutional purpose, funding, and legitimacy from the existence of broad humanitarian legal standards. They can shift issue focus but are structurally invested in the norm's expansion.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, human_rights_advocacy_ngos, beneficiary,
    organized, generational, mobile, global).

% States and their security forces conducting organized armed violence who must afford CA3 protections even when they classify operations as counter-terrorism, law enforcement, or internal security. They bear sovereignty costs, operational restrictions, and individual criminal prosecution risks under universal jurisdiction.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_security_operators, payer,
    institutional, generational, constrained, national).

% UN treaty bodies and special procedures that interpret CA3 expansively, monitor state compliance, and build normative pressure. They set the interpretive agenda but do not themselves bear the constraint's costs or collect its direct protective benefits.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, human_rights_monitoring_bodies, agenda_setter,
    institutional, generational, analytical, global).

% International courts and tribunals that adjudicate CA3 violations under expansive jurisdictional interpretations, subjecting state and non-state actors to individual criminal responsibility regardless of conflict classification. Their mandate constrains them to apply the law as they interpret it.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_criminal_courts, agenda_setter,
    institutional, generational, constrained, global).

% Armed groups bound by CA3 obligations under the expansive reading but structurally excluded from the treaty-drafting and interpretive forums that define its scope. They would contest the asymmetric application that holds them accountable without granting them law-making participation.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups, excluded,
    organized, immediate, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__expansive_human_rights_reading, diffuse).
narrative_ontology:fixing_cost_class(common_article_3_scope__expansive_human_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universally applicable floor of humane treatment for all persons in enemy hands during organized armed violence, eliminating legal black holes that arise when states classify violence as law enforcement or internal security rather than armed conflict.
% TRANSFER_FUNCTION: Transfers sovereignty and operational autonomy from state security operators to protected persons and international oversight bodies; moves authority to define the scope of regulated violence from states to international courts and monitoring mechanisms.
% ABSENT_VOICES: States advocating narrow sovereignty readings and non-state armed groups are structurally underrepresented in the interpretive bodies that construct the expansive reading; military commanders who bear operational costs are heard primarily through state proxies rather than as independent seats.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished overnight, states would narrow conflict classifications to exclude more violence from humanitarian oversight, detainees would lose protections in gray-zone operations, international criminal tribunals would lose a core jurisdictional hook, and the global human rights monitoring architecture would lose a primary legal lever.
% FOUNDING_PROBLEM: The absence of treaty-based humanitarian protections for victims of non-international armed conflicts under the 1949 Geneva Conventions framework.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the 1949 Diplomatic Conference corroborate a narrow state-consent intent. Human rights bodies and the ICRC attest the problem evolved with new forms of violence; states contest that the expansive reading exceeds the original mandate. Independent legal historians support the narrow original intent, while international jurisprudence has progressively challenged it.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__expansive_human_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__expansive_human_rights_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__expansive_human_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__expansive_human_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58) reflects the significant but bounded cost imposed on state security operators: operational planning must accommodate humane-treatment minima, classification games lose legal effect, and officials face universal-jurisdiction exposure. Suppression (0.55) is a raw structural property reflecting moderate but patchy active enforcement through international criminal tribunals, treaty-body pressure, and diplomatic shaming rather than continuous direct coercion. Theater ratio (0.22) is low because the constraint generates substantive protective effects, though some state compliance is performative (signaling adherence without behavioral change). Accessibility collapse (0.72) is high because the expansive reading forecloses the legal alternative of treating organized violence as unregulated law enforcement. Resistance (0.62) is substantial: powerful states consistently argue for narrow, consent-based thresholds and resist external monitoring. The measurement series share a single time grid (0â75) tracking the post-1949 evolution of CA3 interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The state_security_operator seat experiences the constraint as an externally imposed sovereignty tax that erodes operational autonomy and exposes personnel to prosecution; its computed type will skew toward high-extraction tangled rope. The detainee_and_affected_population seat experiences the same structure as a protective ceiling against arbitrary violence; its computed type will skew toward rope or low-extraction coordination. The human_rights_monitoring_body seat sees necessary equilibrium; the agenda-setter directionality moderates its extraction signal. The engine is designed to produce divergent per-seat classifications from this common structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to structural position: detainees_and_affected_populations and human_rights_advocacy_ngos occupy the low-directionality end because the constraint subsidizes their security and institutional purpose. The state_security_operators occupy the high-directionality end because the constraint extracts sovereignty, operational flexibility, and legal immunity from them. Agenda-setters (monitoring bodies and courts) sit near the midpoint: they administer the constraint and derive institutional authority from it, but do not personally bear its costs or receive its protective benefits. Non_state_armed_groups are excluded from the interpretive process but are bound by outcomes, giving them a contested directionality that reverts to the power-atom default.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâabsence of treaty protections in non-international armed conflictsâwas genuine in 1949. Under the expansive reading, the mandate has drifted beyond its original state-consent architecture to cover any organized armed violence. The R5 genealogy flags this as contested: states argue the problem is solved for NIACs but that the expansive reading exceeds the mandate; human rights bodies argue the problem evolves with new forms of violence. The classification as tangled_rope, rather than snare, prevents mislabeling the genuine coordination function (protecting detainees) as mere cover. Were the protective function illusory, the constraint would be a snare; because the protective floor is real and valued by beneficiaries, the hybrid tangled-rope classification is warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ca3_expansive_natural_law_or_construct,
    'Is the expansive application of CA3 a discovery of inherent treaty meaning, or a constructive evolution beyond the 1949 state-consent framework?',
    'Historical-legal analysis of the 1949 travaux prÃ©paratoires combined with systematic review of subsequent state practice and judicial interpretation to determine whether the expansive reading is interpretively derivable or normatively innovative.',
    'If purely constructive, the constraint''s legitimacy depends on ongoing acceptance rather than textual authority, increasing its exposure to repudiation_pressure and affecting drift_state classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ca3_expansive_natural_law_or_construct, conceptual, 'Natural law vs constructive evolution of CA3 scope').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (international tribunal capacity and universal jurisdiction) or internalized (state self-regulation due to legitimacy pressure)?',
    'Post-violation enforcement trajectory: compare rates of CA3 prosecution against rates of state self-investigation; if suppression persists in the absence of external enforcement, reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests and the constraint behaves more like a rope with normative uptake; if purely structural, it remains a tangled rope dependent on active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    organized_armed_violence_boundary,
    'Where does ''organized armed violence'' end and ordinary law enforcement begin under the expansive reading?',
    'Comparative jurisprudential analysis across international courts and treaty bodies to identify the operational criteria used to mark the boundary.',
    'If the boundary is inherently indeterminate, the constraint''s scope is plastic and subject to interpretive expansion or contraction, affecting effective extractiveness for state_security_operators.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organized_armed_violence_boundary, conceptual, 'Indeterminacy of the organized armed violence threshold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ca3_expansive_tr_t0, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ca3_expansive_tr_t15, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(ca3_expansive_tr_t30, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(ca3_expansive_tr_t45, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 45, 0.2).
narrative_ontology:measurement(ca3_expansive_tr_t60, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(ca3_expansive_tr_t75, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 75, 0.24).

% Extraction over time
narrative_ontology:measurement(ca3_expansive_be_t0, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ca3_expansive_be_t15, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(ca3_expansive_be_t30, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(ca3_expansive_be_t45, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 45, 0.52).
narrative_ontology:measurement(ca3_expansive_be_t60, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(ca3_expansive_be_t75, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 75, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ca3_expansive_su_t0, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ca3_expansive_su_t15, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(ca3_expansive_su_t30, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(ca3_expansive_su_t45, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 45, 0.58).
narrative_ontology:measurement(ca3_expansive_su_t60, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(ca3_expansive_su_t75, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 75, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__expansive_human_rights_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
