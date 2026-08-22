% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: Common Article 3 Scope (State-Centric Threshold Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint instantiates the state-centric reading of Common Article
 *   3's scope: CA3 applies only when internal armed conflict meets both an
 *   intensity threshold (sustained, widespread violence) and an organization
 *   threshold (armed groups with command structure and territorial control).
 *   Below these thresholds, violence falls to ordinary law enforcement and
 *   criminal law, which may not provide humanitarian protections. This
 *   reading is one of three contested interpretations of the same kernel (the
 *   CA3 text itself). The state-centric reading privileges governmental
 *   discretion and operational flexibility; expansive and customary-law
 *   readings argue for automatic application to all organized violence. The
 *   claim/metric gap is intentional: this reading is CLAIMED as tangled_rope
 *   (coordination + extraction) while authored metrics describe highly
 *   extractive, actively suppressed operation. The state benefits from
 *   discretionary threshold-setting; irregular combatants and detainees bear
 *   the cost. The engine computes per-seat classification from the structural
 *   data; divergence from the claim reveals how the constraint is experienced
 *   differently across institutional seats.
 *
 * KEY AGENTS:
 *   - state_governments: structural beneficiary (control threshold determination, operational discretion) — power=institutional, exit=arbitrage
 *   - irregular_combatants_below_threshold: structural target (excluded from protections, trapped in legal gray zone) — power=powerless, exit=trapped
 *   - internal_security_detainees: secondary target (identity-locked in unreviewed detention) — power=powerless, exit=identity_locked
 *   - international_humanitarian_organizations: excluded from threshold-setting (ICRC, UN bodies) — power=organized, exit=constrained
 *   - human_rights_advocacy_organizations: excluded from threshold-setting (contesters of state-centric reading) — power=organized, exit=constrained
 *   - international_treaty_bodies: observer seat (ICC, regional courts review post-facto) — power=institutional, exit=analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.81).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.88).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Scope (State-Centric Threshold Reading)").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, 'be3bd72a-eff7-4b5b-81d9-514bb8dd4924').
narrative_ontology:cs_kernel_codification('be3bd72a-eff7-4b5b-81d9-514bb8dd4924', fixed_text).
narrative_ontology:cs_authority_grounding('be3bd72a-eff7-4b5b-81d9-514bb8dd4924', extraction).
narrative_ontology:cs_interpretation_layer_present('be3bd72a-eff7-4b5b-81d9-514bb8dd4924').
narrative_ontology:cs_reading_relation('be3bd72a-eff7-4b5b-81d9-514bb8dd4924', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('be3bd72a-eff7-4b5b-81d9-514bb8dd4924', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('be3bd72a-eff7-4b5b-81d9-514bb8dd4924', foundational, states_sole_threshold_authority).
narrative_ontology:cs_axiom_status(states_sole_threshold_authority, holdable).
narrative_ontology:cs_axiom_grounding('be3bd72a-eff7-4b5b-81d9-514bb8dd4924', states_sole_threshold_authority, conventional).
narrative_ontology:cs_axiom('be3bd72a-eff7-4b5b-81d9-514bb8dd4924', secondary, threshold_discretion_protects_law_enforcement).
narrative_ontology:cs_axiom_status(threshold_discretion_protects_law_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('be3bd72a-eff7-4b5b-81d9-514bb8dd4924', threshold_discretion_protects_law_enforcement, instrumental).
narrative_ontology:cs_reference_frame('be3bd72a-eff7-4b5b-81d9-514bb8dd4924', state_sovereignty_over_internal_security_classification).
narrative_ontology:cs_drift_state('be3bd72a-eff7-4b5b-81d9-514bb8dd4924', contemporary_post_cold_war, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('be3bd72a-eff7-4b5b-81d9-514bb8dd4924', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_governments).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, internal_security_detainees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, conflict_affected_civilians).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_legal_authorities).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, conflict_affected_civilians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines whether an internal armed conflict meets the 'intensity and organization' threshold that triggers CA3 application. Retains discretion over what counts as 'armed conflict' versus 'law enforcement' or 'criminal activity.' Structures the threshold to permit maximum operational flexibility in counterinsurgency, counter-terrorism, and crowd control operations. Benefits from exclusion of low-level violence and irregular combatants from humanitarian protections.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Organized armed groups (or individuals in organized units) below the state-declared threshold receive no minimum humanitarian guarantees: no protection from torture, summary execution, enforced disappearance, or denial of medical care. Once classified as 'below threshold,' they fall outside CA3 and are treated as common criminals or security threats, subject only to ordinary criminal law (which may not be applied or may be applied discriminatorily). Exit requires disarming or abandoning the organization, both of which invite arrest or death.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold, payer,
    powerless, immediate, trapped, local).

% Persons detained in internal security operations (counterinsurgency, counter-terrorism) are held in the legal gray zone: if the conflict is classified as below-threshold, no CA3 protections apply and detention can proceed indefinitely without review, counsel, or due process. Their identity as a detainee is fused with the state's classification of the conflict; challenging that classification means challenging their own legal status.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, internal_security_detainees, payer,
    powerless, biographical, identity_locked, national).

% The ICRC and UN bodies that monitor humanitarian compliance are excluded from determining threshold compliance. States retain gatekeeping authority; IHL organizations can only document violations after classification is settled. Their expertise in identifying armed conflict is systematically sidelined by state-determined thresholds that often keep conflicts classified below the trigger point.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_humanitarian_organizations, excluded,
    organized, generational, constrained, global).

% Human rights bodies that argue for expansive CA3 application are excluded from the threshold-setting process. They contest the state-centric reading, noting that the threshold is weaponized to permit brutality against opposition groups classified as below-conflict. Their advocacy is met with state sovereignty assertions.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, human_rights_advocacy_organizations, excluded,
    organized, generational, constrained, global).

% Civilians in conflict zones benefit from humanitarian services organized under CA3 (shelters, medical triage, neutrality guarantees for aid organizations). However, if the conflict is classified below-threshold, aid organizations lack legal standing to operate under IHL protections, reducing aid flow and increasing civilian exposure to violence.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, conflict_affected_civilians, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__state_centric_reading, conflict_affected_civilians, payer).

% Courts and legal systems benefit from the threshold reading: they can classify internal violence as criminal rather than armed conflict, avoiding the administrative burden of CA3 compliance, detention review, and humanitarian oversight. The threshold maintains their jurisdiction over security matters without humanitarian-law constraints.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_legal_authorities, beneficiary,
    institutional, generational, analytical, national).

% International courts (ICC, regional human rights courts) that interpret IHL and customary international law observe state threshold determinations and assess whether they comply with treaty obligations. They can review threshold classifications post-facto but lack ex-ante authority to set the threshold.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__state_centric_reading, state_governments).
narrative_ontology:fixing_cost_class(common_article_3_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes minimum humanitarian protections for armed conflict participants and affected civilians by setting an intensity-and-organization threshold that triggers CA3's mandated standards (treatment of detainees, medical care, protection from torture, prohibition of summary execution).
% TRANSFER_FUNCTION: Transfers operational discretion from international humanitarian law to state governments: governments gain authority to classify which internal armed violence triggers CA3, and consequently which violence can proceed under ordinary criminal law without humanitarian constraints. This transfer moves accountability from multinational treaty bodies to state courts.
% ABSENT_VOICES: Irregular combatants and security detainees cannot participate in threshold determination; international humanitarian organizations and human rights bodies are systematically excluded from the threshold-setting conversation. Only state governments speak authoritatively on the threshold question, despite being the parties with the strongest incentive to set it high (permitting maximum operational discretion). If the ICRC, regional human rights commissions, or detained-person advocates were present, they would argue for automatic application to all organized violence.
% DISAPPEARANCE_RATIONALE: If the state-centric threshold reading disappeared (replaced by expansive or customary-law readings), internal conflicts would be automatically classified as armed conflicts subject to CA3 minimum protections. Governments would lose the discretion to classify violence as law enforcement; detainee detention would require review, medical care, and humanitarian oversight. Military operations would be constrained by IHL standards rather than ordinary criminal law. The internal security apparatus would reorganize around humanitarian-law compliance. Threshold determinations would move from state gatekeepers to multinational courts and ICRC analysis.
% FOUNDING_PROBLEM: Early CA3 interpretation left scope ambiguous: did it apply to all organized armed violence, or only to interstate wars? States argued for narrow scope to preserve discretion; humanitarian organizations argued for broad floor to ensure protections. The threshold compromise (intensity + organization) was meant to resolve this: apply CA3 when violence is serious enough and organized enough to warrant it, while preserving state discretion over classification.
% FOUNDING_PROBLEM_CORROBORATION: States attest the founding problem is still live and the threshold reading solves it: without discretion, they argue, every street protest or criminal gang would trigger humanitarian restrictions on law enforcement. Human rights organizations and the ICRC attest the founding problem persists precisely because governments abuse the threshold to exclude systematic violence from humanitarian scope. The ICRC's customary-law documentation shows state practice increasingly ignores the threshold and applies CA3 de facto to most organized armed violence, contradicting the state-centric reading's operative reality. Academic analysis and NGO reports document threshold evasion in specific conflicts (Syria, Yemen, Myanmar, Afghanistan) where governments classify sustained, organized violence as below-threshold to avoid inspection and accountability.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) because the state-centric reading transfers authority from multinational humanitarian bodies to states, maximizing state discretion over which violence triggers protections. Suppression is highest (0.88): the constraint's persistence depends on actively excluding irregular combatants from victim sets and IHL-organization participation, not on participant preference or demonstrable threshold-crossing. Theater is moderate (0.42): the intensity-and-organization criteria appear neutral and technical, but their application is systematically biased toward permitting state violence while restricting protections for non-state actors. The temporal series show extraction and suppression rising together from 1949 to 2024 (observed data from post-Cold War period onward): states refined threshold-setting practices to exclude more violence, and enforcement machinery hardened to keep excluded actors outside the humanitarian scope. The 1970–1990 inflection point marks the shift from Cold War proxy conflicts (which governments classified below-threshold despite high organization and intensity) to post-Cold War counterinsurgency and counter-terrorism (where governments applied similar threshold-evasion to exclude non-state security threats). Theater ratio rising over time indicates that the technical-sounding threshold criteria increasingly serve performative legitimacy: they are cited to justify exclusions that have already been decided on political grounds.
 *
 * PERSPECTIVAL GAP:
 *   The state-centric reading creates maximum seat divergence: the beneficiary seat (governments) experiences this as necessary sovereignty and operational discretion; the target seat (irregular combatants and detainees) experiences this as arbitrary exclusion from protections they would receive if the expansive reading held. The intermediate seat (international organizations) experiences this as delegitimization: they possess humanitarian expertise but are structurally barred from authoritatively determining when their expertise applies. The engine computes this divergence from the structural data; it is the point of the analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments are declared beneficiaries (control threshold, operational flexibility) and agenda-setters (determine what counts as armed conflict). Irregular combatants and detainees are declared victims (excluded from protections, trapped, identity-locked). International humanitarian organizations are excluded, not beneficiaries or victims: they are sidelined from threshold-setting but not directly extracted from. The directionality values should reflect this asymmetry: governments move from beneficiary (d~0.0) to moderate (d~0.3–0.4) when accounting for counter-litigation costs and international-court oversight. Detainees and irregular combatants move from target (d~1.0) toward trapped+identity-locked mechanics that amplify d upward (1.0+ in effective extraction). International organizations sit near neutral (d~0.5) because the constraint coordinates humanitarian response while excluding them from the threshold threshold debate itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (resolving ambiguity in CA3 scope) remains live but the solution is contested. The state-centric reading has NOT resolved the problem — human rights organizations and the ICRC continue to argue that the threshold is weaponized to exclude violence that meets humanitarian criteria. The constraint persists not because it solved the founding problem but because states enforce it as a beneficiary group. Theater is rising because the technical-sounding criteria are increasingly invoked to justify decisions that are already made on political grounds: the threshold becomes a rationalization rather than a decision procedure. This trajectory (rising theater, contested founding-problem status, beneficiary-group enforcement without problem-resolution) is the mandatrophy signature: a constraint whose original function (clarifying when CA3 applies) has atrophied, replaced by a function (protecting state operational discretion) that did not exist at founding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_application_practice_divergence,
    'Do states actually apply the intensity-and-organization threshold objectively, or do they systematically classify politically inconvenient violence below-threshold to avoid CA3 constraints?',
    'Comparative analysis of state threshold determinations across conflicts: do threshold decisions track objective metrics (casualty counts, organization level) or do they correlate with state geopolitical interest? Longitudinal study of reclassifications when political conditions change (threshold rises when government succeeds, falls when pressure increases).',
    'If states apply thresholds objectively, the state-centric reading models how sovereignty operates in practice. If threshold decisions are systematically biased, the reading is a cover story for discretionary exclusion and should reclassify from tangled_rope (coordination + extraction) toward snare (pure extraction justified by false criteria). This omega is the empirical test of the constraint''s actual structural type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_application_practice_divergence, empirical, 'Whether state threshold determinations track objective criteria or serve discretionary exclusion.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of irregular combatants and detainees structural (legal barriers, military advantage, institutional machinery) or internalized (irregular fighters accept their own legal-status inferiority, detainees believe they deserve detention without review)?',
    'Post-conflict interviews and ethnographic study of detained and formerly-detained populations: do suppressively mechanisms persist after legal framework removal? If detainees continue to accept denial of counsel and review after the conflict ends and the threshold is lifted, suppression is partially internalized; if acceptance ends immediately, suppression is primarily structural.',
    'If internalized, the effective suppression is higher than the scalar measure suggests — targets carry suppression into post-conflict contexts. If structural, the suppression evaporates with legal status change. This affects the constraint''s persistence prediction and the classification of exclusion as internalized vs. imposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism in exclusion of irregular combatants.').

omega_variable(
    reading_container_foreclosure,
    'Does the state-centric reading logically foreclose the expansive_human_rights_reading, or do they coexist as live positions held by different parties?',
    'Examine the core premises: state-centric asserts ''CA3 applies only when intensity+organization thresholds are met.'' Expansive asserts ''CA3 applies to all organized violence.'' These differ on the scope boundary, but neither logically rules out the other in a single legal framework — a court could hold both (apply CA3 when thresholds meet, AND apply its minimum standards to all organized violence as a floor). The relation is coexists_with, not forecloses, unless the foundational axiom of state-centric reading asserts that states CANNOT be bound by standards when they haven''t classified the conflict as armed — which would foreclose expansive.',
    'If forecloses, the readings are in direct logical conflict and cannot be held simultaneously. If coexists_with, they are strategic positions held by different parties (governments vs. human rights advocates) within overlapping frameworks. The relation type determines the reading-family''s type: foreclosure patterns suggest irreconcilable conflict; coexistence patterns suggest strategic dispute within a shared commitment system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_container_foreclosure, conceptual, 'Logical structure of the state-centric reading''s relationship to expansive-human-rights reading.').

omega_variable(
    customary_law_contra_state_centric,
    'Does the ICRC''s customary international law reading (icrc_customary_reading) actually support the state-centric reading or contradict it?',
    'Examine ICRC customary law study (IHL Database) documentation of state practice on CA3 scope: does the documented opinio juris track the state-centric threshold reading, or does it show states applying CA3 more expansively in practice than the formal threshold reading permits?',
    'If customary practice aligns with state-centric reading, the reading has legitimacy from state practice. If customary practice diverges (states apply CA3 de facto to more violence than the threshold reading formally permits), the customary reading influences the state-centric reading: it creates structural pressure for formal alignment with practice, potentially forcing a drift from threshold-strictness toward expansive application. The relation would shift from coexists_with to influences (customary reading influences state-centric, not vice versa).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(customary_law_contra_state_centric, empirical, 'Whether customary international law practice aligns with or diverges from the state-centric threshold reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__state_centric_reading, theater_ratio, 1949, 0.18).
narrative_ontology:measurement(comm_tr_t1970, common_article_3_scope__state_centric_reading, theater_ratio, 1970, 0.24).
narrative_ontology:measurement(comm_tr_t1990, common_article_3_scope__state_centric_reading, theater_ratio, 1990, 0.32).
narrative_ontology:measurement(comm_tr_t2005, common_article_3_scope__state_centric_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(comm_tr_t2015, common_article_3_scope__state_centric_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__state_centric_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__state_centric_reading, base_extractiveness, 1949, 0.65).
narrative_ontology:measurement(comm_be_t1970, common_article_3_scope__state_centric_reading, base_extractiveness, 1970, 0.71).
narrative_ontology:measurement(comm_be_t1990, common_article_3_scope__state_centric_reading, base_extractiveness, 1990, 0.76).
narrative_ontology:measurement(comm_be_t2005, common_article_3_scope__state_centric_reading, base_extractiveness, 2005, 0.79).
narrative_ontology:measurement(comm_be_t2015, common_article_3_scope__state_centric_reading, base_extractiveness, 2015, 0.8).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__state_centric_reading, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__state_centric_reading, suppression_requirement, 1949, 0.58).
narrative_ontology:measurement(comm_su_t1970, common_article_3_scope__state_centric_reading, suppression_requirement, 1970, 0.68).
narrative_ontology:measurement(comm_su_t1990, common_article_3_scope__state_centric_reading, suppression_requirement, 1990, 0.76).
narrative_ontology:measurement(comm_su_t2005, common_article_3_scope__state_centric_reading, suppression_requirement, 2005, 0.82).
narrative_ontology:measurement(comm_su_t2015, common_article_3_scope__state_centric_reading, suppression_requirement, 2015, 0.85).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__state_centric_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__state_centric_reading, 0.18).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__icrc_customary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested Common Article 3 scope kernel. The state-centric reading emphasizes governmental discretion over threshold determination; the expansive reading and customary-law reading dispute the threshold's legitimacy and application. All three readings share the same referent (CA3 text and its scope question) but have different beneficiary structures and suppression profiles. The constraint family is linked via network.affects_constraints: the state-centric reading influences (and is influenced by) the customary-reading's documentation of actual state practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__state_centric_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
