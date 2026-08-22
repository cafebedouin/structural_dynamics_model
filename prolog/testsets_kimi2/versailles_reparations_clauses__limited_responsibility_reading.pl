% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__limited_responsibility_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Versailles Reparations Clauses â Limited Responsibility Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the limited_responsibility_reading of
 *   the versailles_reparations_clauses kernel. The reading treats Article 231
 *   of the Treaty of Versailles as a legal formality establishing formal
 *   liability without grounding a moral or unlimited financial judgment, and
 *   insists that reparations payments must be bounded by German economic
 *   viability. It functioned as a partial constraint on Allied maximalism,
 *   gaining institutional force through the Dawes Plan (1924), Young Plan
 *   (1929), and the Hoover Moratorium (1931). It coordinates European
 *   stability by preventing German economic collapse, but asymmetrically
 *   extracts from Allied creditors and occupied territories who receive less
 *   than their claimed restoration costs. Sibling readings include the
 *   punitive_liability_reading (unbounded moral responsibility) and the
 *   repudiation_reading (treaty illegitimacy).
 *
 * KEY AGENTS:
 *   - German elites (beneficiary/moderate/constrained): liability capped, leverage to reschedule
 *   - Allied creditors (payer/powerful/constrained): recovery constrained by viability principle
 *   - Occupied territories (payer/powerless/trapped): uncompensated devastation, no commission voice
 *   - International mediators (agenda_setter/institutional/analytical): administer Dawes/Young viability ceilings
 *   - US government (observer/institutional/analytical): facilitates framework without direct creditor stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.62).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.6).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations Clauses â Limited Responsibility Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, '3fc25d4a-aacf-4242-8e6f-fd642c6b55ff').
narrative_ontology:cs_kernel_codification('3fc25d4a-aacf-4242-8e6f-fd642c6b55ff', fixed_text).
narrative_ontology:cs_authority_grounding('3fc25d4a-aacf-4242-8e6f-fd642c6b55ff', lineage).
narrative_ontology:cs_interpretation_layer_present('3fc25d4a-aacf-4242-8e6f-fd642c6b55ff').
narrative_ontology:cs_reading_relation('3fc25d4a-aacf-4242-8e6f-fd642c6b55ff', versailles_reparations_clauses__punitive_liability_reading, coexists_with).
narrative_ontology:cs_reading_relation('3fc25d4a-aacf-4242-8e6f-fd642c6b55ff', versailles_reparations_clauses__repudiation_reading, influences).
narrative_ontology:cs_axiom('3fc25d4a-aacf-4242-8e6f-fd642c6b55ff', foundational, article_231_legal_not_moral).
narrative_ontology:cs_axiom_status(article_231_legal_not_moral, holdable).
narrative_ontology:cs_axiom_grounding('3fc25d4a-aacf-4242-8e6f-fd642c6b55ff', article_231_legal_not_moral, conventional).
narrative_ontology:cs_axiom('3fc25d4a-aacf-4242-8e6f-fd642c6b55ff', foundational, economic_viability_ceiling).
narrative_ontology:cs_axiom_status(economic_viability_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('3fc25d4a-aacf-4242-8e6f-fd642c6b55ff', economic_viability_ceiling, instrumental).
narrative_ontology:cs_reference_frame('3fc25d4a-aacf-4242-8e6f-fd642c6b55ff', post_war_economic_viability).
narrative_ontology:cs_drift_state('3fc25d4a-aacf-4242-8e6f-fd642c6b55ff', interwar_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3fc25d4a-aacf-4242-8e6f-fd642c6b55ff', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_elites).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, allied_creditors).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defeated German leadership whose war liability is capped by economic viability; they negotiate rescheduling through Dawes and Young Plans, securing foreign loans and reduced transfers. They cannot exit the treaty system entirely but operate within it to minimize outflows.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_elites, beneficiary,
    moderate, biographical, constrained, national).

% Governments and private bondholders owed war debts and reconstruction costs by Germany; they see recovery constrained by the viability principle, accepting reduced or rescheduled payments under international commission oversight rather than enforcing full contractual claims.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_creditors, payer,
    powerful, biographical, constrained, global).

% Regions devastated by wartime occupation bear reconstruction costs that exceed reparations received; they have no direct voice in commission negotiations and are structurally unable to claim full compensation once liability is capped by German capacity.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories, payer,
    powerless, generational, trapped, regional).

% Dawes and Young Plan administrators, League of Nations financial committees, and US Treasury officials who set transfer protection clauses, arbitrate viability disputes, and enforce the ceiling on German payments through structured intervention.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, international_mediators, agenda_setter,
    institutional, generational, analytical, global).

% American officials who facilitate mediation and extend credit to Germany but disclaim direct treaty obligations; they observe and shape the viability framework without being a direct creditor party to reparations.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, us_government, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__limited_responsibility_reading, german_elites).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents total German economic collapse and maintains European financial stability by bounding sovereign liability to sustainable transfer capacity, coordinating a payment schedule that does not destroy the debtor economy.
% TRANSFER_FUNCTION: Moves potential reconstruction wealth from Allied creditors and occupied territories to German retainable surplus by capping reparations at viability-tested levels and rescheduling around capacity.
% ABSENT_VOICES: Private holders of German sovereign and municipal debt; small-scale French and Belgian property owners in occupied zones; they would demand full restoration but were excluded from commission arbitration and treated as aggregated creditors.
% DISAPPEARANCE_RATIONALE: If the viability constraint vanished, Allied maximalists would demand immediate full payment, German transfer capacity would collapse into hyperinflation or default, the Dawes and Young architectures would dissolve, and inter-allied debt diplomacy would revert to 1919 zero-sum bargaining.
% FOUNDING_PROBLEM: Post-war German economic collapse threatening European recovery and revanchism; the need to extract reparations without destroying the German economy that must generate them.
% FOUNDING_PROBLEM_CORROBORATION: British Treasury and US State Department officials attested the problem from outside the direct beneficiary set; French military occupation authorities and German industrialists contested the framing, the former arguing extraction capacity was higher, the latter arguing the constraint was still too severe.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__limited_responsibility_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the viability constraint systematically transfers wealth from creditors and victims to German retainable surplus by capping recovery. Suppression (0.60) is high because the constraint's persistence depends on active international enforcementâAllied creditors do not voluntarily accept haircuts without commission pressure and US financial leverage. Theater ratio (0.50) reflects heavy diplomatic performance around rescheduling conferences whose economic substance was increasingly detached from the original liability claims. Accessibility collapse (0.60) captures how, once the viability framework was institutionalized, the alternative of full restoration largely disappeared from actionable policy. Resistance (0.70) is high because French occupation authorities and creditor groups actively contested the liability cap throughout the interval.
 *
 * PERSPECTIVAL GAP:
 *   The German seat experiences the constraint as a protective ceiling that prevents national ruin; the creditor and occupied-territory seats experience the same structure as an enforced deprivation of just compensation. The mediator seat sees a necessary coordination device; the victim seats see an extractive ceiling. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   German elites are declared beneficiaries (low directionality: the constraint subsidizes their fiscal position by limiting outflows). Allied creditors and occupied territories are declared victims (high directionality: the constraint extracts from them by denying full recovery). International mediators sit near symmetricâthey neither pay nor benefit directly, but administer the asymmetry. US observers are analytical with near-zero directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling as a pure rope because it has identifiable victims (creditors and occupied territories) who bear real costs through the same structure that coordinates stability. It prevents mislabeling as a pure snare because the coordination functionâpreventing German collapse and European financial contagionâis structurally genuine and acknowledged even by the paying parties. The active enforcement requirement (Dawes/Young commissions, transfer protection) confirms tangled_rope rather than rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_231_textual_basis,
    'Does the limited responsibility readingâthat Article 231 is a legal formality not grounding moral or unlimited liabilityâaccurately reflect the treaty text, or does it require retrospective reinterpretation unsupported by the 1919 diplomatic record?',
    'Archival diplomatic history of the treaty negotiation, comparing drafting-committee intent with subsequent legal commentary.',
    'If the text genuinely supports unlimited liability, this reading is a constructed false summit; if the text is ambiguous, the reading is a permissible interpretation with no textual foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_231_textual_basis, empirical, 'Whether Article 231 textually supports the limited liability reading.').

omega_variable(
    us_mediation_neutrality,
    'Was American mediation of the viability framework structurally neutral coordination, or did it serve American financial interests by stabilizing German debt service to US lenders?',
    'Analysis of US private lending flows to Germany concurrent with Dawes and Young Plan schedules; comparison of American banking exposure with mediation positions.',
    'If US mediation was creditor-biased, the constraint''s directionality for German elites shifts from pure beneficiary to partially subsidized pawn, and the coordination story becomes more tangled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_mediation_neutrality, empirical, 'Whether US mediation neutrality was captured by private financial interest.').

omega_variable(
    viability_measurement_contest,
    'Who measured German economic capacity, and was the measurement methodology politically captured by the beneficiary party?',
    'Independent economic reconstruction of German taxable capacity 1924â1932 against contemporaneous commission estimates.',
    'If capacity was systematically underestimated, extraction from creditors is higher than the authored epsilon suggests; if overestimated, the constraint was closer to a genuine scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(viability_measurement_contest, empirical, 'Political economy of German capacity measurement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 0, 13).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t0, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(vers_tr_t3, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 3, 0.4).
narrative_ontology:measurement(vers_tr_t5, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(vers_tr_t8, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 8, 0.52).
narrative_ontology:measurement(vers_tr_t11, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 11, 0.58).
narrative_ontology:measurement(vers_tr_t13, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 13, 0.5).

% Extraction over time
narrative_ontology:measurement(vers_be_t0, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(vers_be_t3, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(vers_be_t5, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(vers_be_t8, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(vers_be_t11, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 11, 0.58).
narrative_ontology:measurement(vers_be_t13, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 13, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t0, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(vers_su_t3, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 3, 0.35).
narrative_ontology:measurement(vers_su_t5, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(vers_su_t8, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(vers_su_t11, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 11, 0.62).
narrative_ontology:measurement(vers_su_t13, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 13, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, repudiation_reading).

% DUAL FORMULATION NOTE:
% The Versailles reparations clauses decompose into three structurally distinct constraints: limited_responsibility_reading (viability-bounded liability), punitive_liability_reading (unbounded moral liability), and repudiation_reading (illegitimate treaty). They share the kernel (Article 231) but have different epsilon values, beneficiary/victim structures, and institutional dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
