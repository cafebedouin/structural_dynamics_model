% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Divine Marriage Command â Coercion Visibility Reading
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   This constraint story models the divine marriage command under the
 *   coercion visibility reading of the Mormon Manifesto (1890). Under this
 *   reading, the Manifesto is acknowledged as a direct response to federal
 *   anti-polygamy coercion, and the resulting monogamy command derives its
 *   theological legitimacy from institutional survival necessity rather than
 *   from superseding revelation. The M-set gap between stated and actual
 *   causation is closed: the authority structure openly admits (or is
 *   compelled to admit) non-revelatory, exogenous grounds for the doctrinal
 *   shift. This produces a structurally unusual commitment system in which
 *   the coordination function (collective survival) and the extraction
 *   function (theological and relational costs borne by practitioners and
 *   traditionalists) are both visible and acknowledged. The claimed type is
 *   tangled rope: genuine coordination in preventing institutional
 *   destruction, but asymmetric extraction in demanding doctrinal and marital
 *   compliance on grounds that undermine the tradition's own revelatory
 *   epistemology.
 *
 * KEY AGENTS:
 *   - Ecclesiastical hierarchy: agenda-setter and primary beneficiary (institutional/identity_locked) â administers the command, collects institutional survival.
 *   - Plural marriage practitioners: primary payer and victim (powerless/trapped) â forced to abandon practice under dual ecclesiastical and federal threat.
 *   - Theological traditionalists: secondary payer and victim (moderate/identity_locked) â bear epistemic costs of accepting survival necessity as legitimate theological input.
 *   - Rank-and-file membership: diffuse beneficiary and incidental payer (organized/identity_locked) â benefit from institutional continuity, pay in theological coherence.
 *   - Federal enforcement apparatus: external observer (institutional/analytical) â applied the coercion that determined the constraint's boundary conditions without entering its theological discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.78).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.52).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Divine Marriage Command â Coercion Visibility Reading").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, '6c624b69-65d6-4012-be3b-b86a77fab45a').
narrative_ontology:cs_kernel_codification('6c624b69-65d6-4012-be3b-b86a77fab45a', fixed_text).
narrative_ontology:cs_authority_grounding('6c624b69-65d6-4012-be3b-b86a77fab45a', lineage).
narrative_ontology:cs_interpretation_layer_present('6c624b69-65d6-4012-be3b-b86a77fab45a').
narrative_ontology:cs_reading_relation('6c624b69-65d6-4012-be3b-b86a77fab45a', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c624b69-65d6-4012-be3b-b86a77fab45a', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_axiom('6c624b69-65d6-4012-be3b-b86a77fab45a', foundational, institutional_survival_as_legitimating_ground).
narrative_ontology:cs_axiom_status(institutional_survival_as_legitimating_ground, holdable).
narrative_ontology:cs_axiom_grounding('6c624b69-65d6-4012-be3b-b86a77fab45a', institutional_survival_as_legitimating_ground, instrumental).
narrative_ontology:cs_axiom('6c624b69-65d6-4012-be3b-b86a77fab45a', foundational, manifesto_as_coerced_response).
narrative_ontology:cs_axiom_status(manifesto_as_coerced_response, holdable).
narrative_ontology:cs_axiom_grounding('6c624b69-65d6-4012-be3b-b86a77fab45a', manifesto_as_coerced_response, empirically_contingent).
narrative_ontology:cs_reference_frame('6c624b69-65d6-4012-be3b-b86a77fab45a', revelatory_authority_framework).
narrative_ontology:cs_drift_state('6c624b69-65d6-4012-be3b-b86a77fab45a', post_manifesto_acknowledgment, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('6c624b69-65d6-4012-be3b-b86a77fab45a', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, rank_and_file_membership).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, plural_marriage_practitioners).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, theological_traditionalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, rank_and_file_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the divine marriage command, acknowledges the Manifesto as a response to federal coercion, and justifies continued monogamy doctrine by institutional survival necessity. Its authority is fused with the institution's continuity; abandoning the survival-necessity frame would require a complete re-legitimation of the hierarchy.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, ecclesiastical_hierarchy, beneficiary).

% Were commanded to enter plural marriage under prior revelation and later commanded to abandon it under threat of federal imprisonment and ecclesiastical sanction. Many dissolved families or entered hiding. Exit meant federal prosecution or excommunication; remaining meant disobedience to the new command.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, plural_marriage_practitioners, payer,
    powerless, biographical, trapped, local).

% Hold that only revelation can rescind revelation, and that federal coercion cannot be a legitimate theological input. Bear epistemic costs by remaining in a community whose authority structure admits non-revelatory grounds for doctrinal change. Their dissent is marginalized within authoritative discourse.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, theological_traditionalists, payer,
    moderate, generational, identity_locked, national).

% Benefit from the institutional survival secured by the Manifesto and subsequent integration into American civic life. Pay diffuse theological costs as the revelatory authority framework is partially replaced by survival-necessity reasoning. Their religious identity is tied to the institution's continuity, making exit costly regardless of doctrinal coherence.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, rank_and_file_membership, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, rank_and_file_membership, payer).

% Applied anti-polygamy statutes, seized property, and disincorporated the church, creating the coercive boundary conditions that produced the Manifesto. Does not participate in the theological constraint but structurally determines which doctrines are politically viable.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_enforcement_apparatus, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__coercion_visibility_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(divine_marriage_command__coercion_visibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the institutional church from federal dissolution by aligning marriage practice with federal law, thereby securing legal existence and property for the collective membership.
% TRANSFER_FUNCTION: Moves doctrinal legitimacy from claims of direct revelatory supersession to claims of institutional survival necessity; moves plural marriage practitioners into monogamy or exile under threat of sanction; moves institutional continuity to the hierarchy while transferring theological dissonance to traditionalists.
% ABSENT_VOICES: Theological traditionalists who maintain that only revelation can rescind revelation are marginalized in authoritative discourse; plural wives and children displaced by the prohibition were not parties to the decision; federal actors determined the boundary conditions without entering the theological conversation as interlocutors.
% DISAPPEARANCE_RATIONALE: If the divine marriage command under this reading vanished, the institution would lose the doctrinal mechanism that secured its legal existence in 1890. Either federal coercion would resume, or the church would need to openly embrace an alternative legitimating frameârevelation, continuation, or repudiationâfundamentally rearranging its authority structure and marriage practice.
% FOUNDING_PROBLEM: Federal anti-polygamy legislation and enforcement threatened the institutional church with property seizure, disincorporation, and effective destruction unless plural marriage was abandoned.
% FOUNDING_PROBLEM_CORROBORATION: Federal statutes, court records, and contemporary newspaper accounts corroborate the external coercion. Non-Mormon political actors and secular historians attest to the existential threat. The ecclesiastical authority itself acknowledged the federal pressure in official communications. However, the claim that survival necessity is a theologically valid legitimating ground is asserted by the benefiting authority structure and scholars of this reading, with no independent corroboration from outside the beneficiary set that political survival constitutes divine legitimacy.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the command demands total compliance in marriage practice while openly grounding legitimacy in political survival rather than divine mandate, exacting severe theological costs. Suppression is moderate (0.52) because overt enforcement has normalized over the interval; federal coercion has receded and ecclesiastical discipline operates more through identity maintenance than overt sanction. Theater ratio is moderately high (0.62) and rising: as the survival threat recedes into history, continued justification of the command on survival grounds becomes increasingly performative. Accessibility collapse (0.70) is high because once the survival-necessity frame is accepted, alternatives (return to plural marriage, rejection of the hierarchy) collapse within the institutional framework. Resistance (0.45) reflects ongoing schismatic movements and internal traditionalist dissent that the constraint has not fully suppressed. The metrics and claim are independent: the story claims tangled rope on structural grounds (coordination + asymmetric extraction + enforcement) while the metrics describe a constraint that has grown more extractive and theatrical over time as its founding problem faded.
 *
 * PERSPECTIVAL GAP:
 *   The ecclesiastical hierarchy experiences the constraint as successful coordination that saved the institution; its directionality is near-beneficiary. Plural marriage practitioners experience it as enforced abandonment of religious duty under duress; their directionality is near-target. Theological traditionalists experience it as an epistemic wound to revelatory authority; they are also near-target but with different exit options (internal dissent versus external schism). The engine computes these divergent seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the hierarchy and the general membership who receive institutional continuity. Victims are the practitioners forced to dissolve families and the traditionalists forced to accept that federal coercion can legitimately alter divine command. Directionality for the hierarchy is low (beneficiary); for practitioners and traditionalists it is high (target). The federal apparatus is outside the constraint's directionality derivation because it is an observer, not a party governed by the command.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâfederal destruction of the institutionâis dead, yet the constraint persists. The coercion visibility reading closes the naive mandatrophy gap by admitting the non-revelatory cause, but it opens a deeper one: if survival necessity is the ground, and survival is no longer at stake, the mandate is either a piton (inertial maintenance) or a covert snare (hierarchy extracting compliance on a false premise). The tangled rope classification captures the intermediate state: the coordination was genuine, but its persistence after the threat has become increasingly extractive. The engine should flag this for piton drift if theater_ratio and extraction continue rising while suppression decays.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading (coercion_visibility_reading) of the kernel divine_marriage_command; how would the classification change if the continuationist or substitutionist reading were adopted instead?',
    'Comparative analysis of sibling constraint stories in the same kernel family; evaluate which structural elements (beneficiary/victim sets, enforcement mechanisms, legitimacy claims) shift across readings.',
    'Adopting the substitutionist reading would eliminate the legitimacy-crisis omega and recast the constraint as revelatory scaffold or rope; adopting the continuationist reading would reintroduce the doctrinal validity of polygamy and shift victimhood onto those prohibited from practicing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame omega locating this reading within the contested kernel').

omega_variable(
    coercion_as_valid_theological_input,
    'If federal coercion is admitted as a valid proximate cause for doctrinal change, does any doctrine remain non-contingent, or does the authority structure permanently subordinate revelation to survival?',
    'Trace subsequent doctrinal developments: if later policy shifts also track external political pressure rather than independent revelatory claims, the contingency thesis is corroborated; if later shifts are demonstrably pressure-independent, the two sources may be partially reconciled.',
    'If all doctrine becomes contingent on external pressure, the authority grounding collapses toward pure extraction or conventional legitimacy; if coercion is cabined to the Manifesto alone, the constraint remains a bounded tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_as_valid_theological_input, empirical, 'Whether survival-necessity justification generalizes beyond the Manifesto').

omega_variable(
    mandatrophy_or_permanent_shift,
    'Is the Manifesto-era survival necessity a dead founding problem (producing mandatrophy/piton dynamics) or an ongoing legitimating frame (producing active tangled rope dynamics)?',
    'Evaluate whether contemporary ecclesiastical discourse still invokes institutional survival to justify the monogamy command, or whether the justification has migrated to revelation, tradition, or natural law.',
    'If survival is no longer invoked, the constraint may have atrophied into a piton with high theater_ratio; if survival remains the implicit or explicit ground, the constraint is an active tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_or_permanent_shift, empirical, 'Whether the founding survival problem is live or dead').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(div_mar_cvis_tr_t0, divine_marriage_command__coercion_visibility_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(div_mar_cvis_tr_t20, divine_marriage_command__coercion_visibility_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(div_mar_cvis_tr_t40, divine_marriage_command__coercion_visibility_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(div_mar_cvis_tr_t60, divine_marriage_command__coercion_visibility_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(div_mar_cvis_tr_t80, divine_marriage_command__coercion_visibility_reading, theater_ratio, 80, 0.55).
narrative_ontology:measurement(div_mar_cvis_tr_t100, divine_marriage_command__coercion_visibility_reading, theater_ratio, 100, 0.62).

% Extraction over time
narrative_ontology:measurement(div_mar_cvis_be_t0, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(div_mar_cvis_be_t20, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(div_mar_cvis_be_t40, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(div_mar_cvis_be_t60, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(div_mar_cvis_be_t80, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 80, 0.75).
narrative_ontology:measurement(div_mar_cvis_be_t100, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(div_mar_cvis_su_t0, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(div_mar_cvis_su_t20, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(div_mar_cvis_su_t40, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(div_mar_cvis_su_t60, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(div_mar_cvis_su_t80, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement(div_mar_cvis_su_t100, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
