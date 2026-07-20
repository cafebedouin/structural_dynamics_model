% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__repudiation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__repudiation_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Versailles Reparations Clauses â Repudiation Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Versailles Treaty reparations clauses (Article 231 and associated
 *   schedules) imposed massive financial obligations on Germany following
 *   World War I. This constraint story instantiates the REPUDIATION READING
 *   of that kernel: the claim that the treaty was imposed under duress, is
 *   therefore illegitimate ab initio, and creates no binding obligation
 *   beyond token gestures. Under this reading, Germany systematically
 *   nullified Allied creditor claims from 1919 through 1939, redirecting
 *   fiscal capacity toward rearmament and domestic recovery. The kernel
 *   decomposes into three readings â punitive liability (unlimited
 *   obligation), limited responsibility (capacity-bounded obligation), and
 *   repudiation (void ab initio) â each with distinct Îµ, stakeholder
 *   structures, and directionalities. This reading is the most extractive of
 *   the three, characterized by total rejection of payment obligations and
 *   complete suppression of creditor enforcement.
 *
 * KEY AGENTS:
 *   - German state (agenda_setter/institutional): administers repudiation, enforces non-payment, captures fiscal and military benefits
 *   - German domestic constituency (beneficiary/organized): receives tax relief and rearmament employment from freed fiscal capacity
 *   - Allied creditor governments (payer/institutional): hold nullified treaty claims, face collapsing enforcement options
 *   - Allied civilian claimants (payer/powerless): war victims and property owners whose individual compensation was extinguished
 *   - International legal community (observer/analytical): debates duress and treaty validity without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.82).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.88).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Versailles Reparations Clauses â Repudiation Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, 'da4b34d6-19eb-402c-a518-0896d8dfcc77').
narrative_ontology:cs_kernel_codification('da4b34d6-19eb-402c-a518-0896d8dfcc77', fixed_text).
narrative_ontology:cs_authority_grounding('da4b34d6-19eb-402c-a518-0896d8dfcc77', extraction).
narrative_ontology:cs_interpretation_layer_present('da4b34d6-19eb-402c-a518-0896d8dfcc77').
narrative_ontology:cs_reading_relation('da4b34d6-19eb-402c-a518-0896d8dfcc77', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('da4b34d6-19eb-402c-a518-0896d8dfcc77', versailles_reparations_clauses__limited_responsibility_reading, coexists_with).
narrative_ontology:cs_axiom('da4b34d6-19eb-402c-a518-0896d8dfcc77', foundational, duress_voids_treaty_obligation).
narrative_ontology:cs_axiom_status(duress_voids_treaty_obligation, holdable).
narrative_ontology:cs_axiom_grounding('da4b34d6-19eb-402c-a518-0896d8dfcc77', duress_voids_treaty_obligation, deontological).
narrative_ontology:cs_axiom('da4b34d6-19eb-402c-a518-0896d8dfcc77', foundational, great_power_sovereignty_incompatible_with_subjugation).
narrative_ontology:cs_axiom_status(great_power_sovereignty_incompatible_with_subjugation, holdable).
narrative_ontology:cs_axiom_grounding('da4b34d6-19eb-402c-a518-0896d8dfcc77', great_power_sovereignty_incompatible_with_subjugation, deontological).
narrative_ontology:cs_reference_frame('da4b34d6-19eb-402c-a518-0896d8dfcc77', sovereign_equality_no_duress).
narrative_ontology:cs_drift_state('da4b34d6-19eb-402c-a518-0896d8dfcc77', interwar_partial_compliance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da4b34d6-19eb-402c-a518-0896d8dfcc77', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_state).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_domestic_constituency).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, allied_creditor_governments).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, allied_civilian_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the repudiation policy through diplomatic non-compliance, legislative suspension of reparations payments, and ultimately unilateral treaty denunciation. Redirects fiscal resources from external reparations to internal rearmament and domestic programs. Exit from this policy would mean accepting Versailles liability and massive wealth transfers abroad, which is domestically untenable.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_state, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from retained fiscal capacity within Germany â reduced tax burden relative to reparations-funded extraction, employment in rearmament industries, and symbolic restoration of national honor. They cannot individually exit the constraint; their welfare rises and falls with Germany's international negotiating position.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_domestic_constituency, beneficiary,
    organized, biographical, constrained, national).

% Hold treaty-based reparations claims that are systematically nullified by German repudiation. They attempted enforcement via sanctions and occupation (Ruhr 1923) but faced escalating costs and international opposition. Their exit options narrow as German military strength recovers and the legal basis for claims is denied.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_creditor_governments, payer,
    institutional, generational, constrained, continental).

% War victims, widows, and owners of destroyed property in France and Belgium who held individual compensation claims against Germany. Their claims were aggregated into state-level reparations demands and effectively extinguished by German repudiation. They have no individual enforcement mechanism and no exit from the loss.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_civilian_claimants, payer,
    powerless, biographical, trapped, regional).

% Scholars and jurists debating whether Article 231 and the reparations clauses are enforceable under conditions of duress. They document the treaty's imposition process, assess customary international law on coercion, and produce divergent opinions that fuel or undermine the repudiation reading without directly controlling state policy.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, international_legal_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__repudiation_reading, german_state).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__repudiation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Restores German fiscal and military sovereignty by collectively rejecting externally imposed financial obligations, enabling domestic resource redirection toward national reconstruction and rearmament.
% TRANSFER_FUNCTION: Moves the full value of Allied reparations claims from creditor governments and civilian claimants to the German state budget and military-industrial sector, via legal nullification rather than payment.
% ABSENT_VOICES: French and Belgian war widows, Ruhr industrial workers affected by German default, and smaller Allied creditors without great-power diplomatic leverage were excluded from the renegotiation tables where repudiation was operationalized.
% DISAPPEARANCE_RATIONALE: If the repudiation reading vanished and Germany accepted full liability, the Reich would redirect massive fiscal flows abroad, Allied occupation threats would recede, French security policy would reorganize around receipt rather than prevention of payment, and German rearmament would face hard budgetary constraints.
% FOUNDING_PROBLEM: Germany sought to escape the 'war guilt' clause and crushing reparations imposed at Versailles, which were viewed domestically as national humiliation and economically as unsustainable extraction imposed by victors' fiat.
% FOUNDING_PROBLEM_CORROBORATION: German governments across the Weimar and early National Socialist periods attested the problem from the benefiting side. Allied creditor governments and the Reparations Commission attested the opposite â that the problem was German refusal, not treaty illegitimacy. Independent legal scholars and later historical consensus (outside both beneficiary and victim camps) corroborate that the treaty was imposed under significant duress, though they dispute whether that voids obligation.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__repudiation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__repudiation_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__repudiation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__repudiation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the repudiation reading structurally nullifies the entire value of Allied reparations claims, transferring that value to German fiscal and military budgets. Suppression is very high (0.88) because the constraint's persistence depends on actively suppressing creditor enforcement mechanisms â diplomatic stonewalling, legislative suspension, and ultimately military deterrence against renewed occupation. Theater ratio is moderate (0.45): the duress argument has genuine legal and historical substance, but its performative deployment in domestic propaganda and international forums increasingly exceeded its juridical content. Accessibility collapse is high (0.78) because once Germany fully committed to repudiation under the Nazi regime, creditor alternatives narrowed to war or acceptance. Resistance is substantial (0.72) from France, the UK, and the Reparations Commission, but ultimately ineffective.
 *
 * PERSPECTIVAL GAP:
 *   The German state and domestic constituency experience this constraint as sovereignty restoration and fiscal liberation â the engine will compute their seats near the beneficiary end. Allied creditors experience the identical structural arrangement as total extraction â their claims are voided, their enforcement options exhausted. The engine computes this divergence from the beneficiary/victim declarations and the asymmetric exit options: Germany could always reverse the policy (constrained exit), while creditors could not enforce claims without prohibitive cost (constrained/trapped).
 *
 * DIRECTIONALITY LOGIC:
 *   The German state is the primary beneficiary and agenda-setter (low d): it sets the repudiation policy and captures the freed resources directly. The domestic constituency is a secondary beneficiary (low d). Allied creditor governments are primary targets (high d): they bear the cost of nullified claims and exhausted enforcement. Civilian claimants are trapped targets (highest d): they are identity-locked into their losses as war victims with no recourse. The international legal community sits at the analytical pole (no d computation).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by distinguishing genuine sovereignty restoration from pure strategic default. If Germany had merely objected to the amount while accepting the obligation (limited responsibility), the constraint would sit lower on the extraction scale. The repudiation reading's total rejection of legal obligation â combined with active enforcement of non-payment and systematic suppression of creditor alternatives â places it in tangled rope territory: the coordination function (German national unity, fiscal autonomy) is real, but it is inseparable from the asymmetric extraction (Allied claims nullified). A snare classification would miss the genuine coordination; a rope classification would miss the creditor victimization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duress_legal_effect_ambiguity,
    'Does duress under international law void a treaty entirely, or merely entitle the coerced party to seek termination or modification?',
    'Comparative analysis of treaty law and international court rulings on coercion in treaty formation.',
    'If duress merely permits termination, the repudiation reading overstates its case and slides toward limited_responsibility territory; if void ab initio, the reading maintains its structural distinctiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_legal_effect_ambiguity, conceptual, 'Whether duress annihilates obligation or merely opens a procedural exit.').

omega_variable(
    token_gesture_boundary,
    'What distinguishes a ''token gesture'' of payment from a binding partial fulfillment that would reconstitute obligation?',
    'Historical tracing of which transfers Germany classified as political gesture versus which creditors treated as acknowledgment of liability.',
    'If token payments were accepted as partial fulfillment, the repudiation reading''s claim of ''no binding obligation'' weakens and its Îµ may drop.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(token_gesture_boundary, empirical, 'Boundary between symbolic payment and obligation-reconstituting performance.').

omega_variable(
    repudiation_coordination_genuineness,
    'Is the national unity produced by repudiation a genuine coordination benefit, or does it function primarily as cover for strategic default and rearmament?',
    'Counterfactual analysis of German domestic politics under hypothetical acceptance of Versailles liability.',
    'If purely cover, reclassify toward snare; if genuine sovereignty restoration, tangled_rope remains correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(repudiation_coordination_genuineness, conceptual, 'Whether identity coordination is structurally genuine or extraction cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t0, versailles_reparations_clauses__repudiation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(vers_tr_t4, versailles_reparations_clauses__repudiation_reading, theater_ratio, 4, 0.4).
narrative_ontology:measurement(vers_tr_t8, versailles_reparations_clauses__repudiation_reading, theater_ratio, 8, 0.5).
narrative_ontology:measurement(vers_tr_t12, versailles_reparations_clauses__repudiation_reading, theater_ratio, 12, 0.45).
narrative_ontology:measurement(vers_tr_t16, versailles_reparations_clauses__repudiation_reading, theater_ratio, 16, 0.6).
narrative_ontology:measurement(vers_tr_t20, versailles_reparations_clauses__repudiation_reading, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(vers_be_t0, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(vers_be_t4, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(vers_be_t8, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(vers_be_t12, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(vers_be_t16, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 16, 0.85).
narrative_ontology:measurement(vers_be_t20, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 20, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t0, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vers_su_t4, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 4, 0.7).
narrative_ontology:measurement(vers_su_t8, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(vers_su_t12, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(vers_su_t16, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 16, 0.85).
narrative_ontology:measurement(vers_su_t20, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 20, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, identity_coordination).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).

% DUAL FORMULATION NOTE:
% The Versailles reparations clauses kernel decomposes into three structurally distinct constraints: punitive_liability (unlimited obligation), limited_responsibility (capacity-bounded obligation), and repudiation (void ab initio). Each reading has a different Îµ, victim set, and beneficiary set. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
