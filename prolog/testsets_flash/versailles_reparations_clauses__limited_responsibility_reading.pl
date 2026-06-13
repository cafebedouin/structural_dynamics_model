% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Versailles Reparations: Limited Responsibility Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'limited responsibility' reading of the
 *   Versailles reparations clauses, which argued that Germany's reparations
 *   payments must be aligned with its economic capacity, and that Article 231
 *   (the 'war guilt' clause) was a legal formality for liability, not a moral
 *   judgment. This reading led to a series of downward revisions of payment
 *   schedules (Dawes Plan, Young Plan) and ultimately the effective
 *   cancellation of most payments by 1932. It was a contested interpretation
 *   that significantly reduced the extractive pressure on Germany, shifting
 *   some of the burden onto Allied creditors and occupied territories.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.45).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.3).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations: Limited Responsibility Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, '61185232-c8e0-4a8c-b476-a5bbba408b70').
narrative_ontology:cs_kernel_codification('61185232-c8e0-4a8c-b476-a5bbba408b70', fixed_text).
narrative_ontology:cs_authority_grounding('61185232-c8e0-4a8c-b476-a5bbba408b70', lineage).
narrative_ontology:cs_interpretation_layer_present('61185232-c8e0-4a8c-b476-a5bbba408b70').
narrative_ontology:cs_reading_relation('61185232-c8e0-4a8c-b476-a5bbba408b70', versailles_reparations_clauses__punitive_liability_reading, influences).
narrative_ontology:cs_reading_relation('61185232-c8e0-4a8c-b476-a5bbba408b70', versailles_reparations_clauses__repudiation_reading, coexists_with).
narrative_ontology:cs_axiom('61185232-c8e0-4a8c-b476-a5bbba408b70', foundational, reparations_must_align_with_economic_capacity).
narrative_ontology:cs_axiom_status(reparations_must_align_with_economic_capacity, holdable).
narrative_ontology:cs_axiom_grounding('61185232-c8e0-4a8c-b476-a5bbba408b70', reparations_must_align_with_economic_capacity, empirically_contingent).
narrative_ontology:cs_axiom('61185232-c8e0-4a8c-b476-a5bbba408b70', foundational, article_231_is_legal_formality_not_moral_judgment).
narrative_ontology:cs_axiom_status(article_231_is_legal_formality_not_moral_judgment, holdable).
narrative_ontology:cs_axiom_grounding('61185232-c8e0-4a8c-b476-a5bbba408b70', article_231_is_legal_formality_not_moral_judgment, conventional).
narrative_ontology:cs_reference_frame('61185232-c8e0-4a8c-b476-a5bbba408b70', economic_viability_framework).
narrative_ontology:cs_drift_state('61185232-c8e0-4a8c-b476-a5bbba408b70', post_young_plan_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('61185232-c8e0-4a8c-b476-a5bbba408b70', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_elites).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_economy).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, allied_creditors).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocated for and benefited from the interpretation that reparations must be limited by Germany's economic capacity, gaining leverage in negotiations and reducing the burden on the German state and industry. Their political survival depended on mitigating the reparations burden.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_elites, beneficiary,
    powerful, biographical, constrained, national).

% Benefited from reduced and restructured payment schedules, which prevented total collapse and allowed for some recovery, albeit under significant strain. The viability of the economy was the central argument for limiting reparations.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_economy, beneficiary,
    institutional, generational, constrained, national).

% Received less compensation than initially demanded, leading to financial strain and political discontent in their home countries. They were forced to accept revised payment plans due to the perceived impossibility of extracting more from Germany without risking its collapse.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_creditors, payer,
    institutional, generational, constrained, global).

% Suffered immense damage during the war and were promised reparations for reconstruction. The reduction in German payments meant their own recovery was delayed or incomplete, leaving them as indirect victims of the limited responsibility reading.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories, payer,
    powerless, generational, trapped, regional).

% Played a crucial role in shaping the 'limited responsibility' reading by providing economic analyses that argued for the necessity of linking reparations to Germany's capacity to pay, influencing the Dawes and Young Plans. They acted as technical arbiters of economic viability.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, international_financial_experts, agenda_setter,
    organized, biographical, analytical, global).

% Initially pushed for maximalist reparations based on Germany's moral culpability and were resistant to any reductions. They were eventually sidelined or forced to compromise by the economic realities and the 'limited responsibility' arguments, but their voice remained a significant counterpoint.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, french_hardliners, excluded,
    powerful, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aimed to coordinate the post-war European economic recovery by establishing a reparations schedule that was theoretically sustainable for Germany, thereby preventing its economic collapse and potential political instability, which would have destabilized the entire continent.
% TRANSFER_FUNCTION: Transferred a reduced and restructured amount of financial compensation from Germany to the Allied powers, primarily France and Belgium, and indirectly to the United States (for inter-Allied war debts).
% ABSENT_VOICES: The voices of those who suffered most directly from German occupation and destruction, particularly in Eastern Europe, were largely absent from the high-level negotiations that shaped the limited responsibility reading. They would have argued for greater compensation based on their direct losses, rather than Germany's capacity to pay.
% DISAPPEARANCE_RATIONALE: If the limited responsibility reading had never gained traction, the initial punitive demands might have been enforced more rigorously, potentially leading to an earlier and more severe German economic collapse, or prolonged occupation, fundamentally altering the interwar political and economic landscape of Europe.
% FOUNDING_PROBLEM: The initial reparations demands were economically unfeasible for post-WWI Germany, threatening to destabilize the German economy and, by extension, the European financial system, risking further conflict.
% FOUNDING_PROBLEM_CORROBORATION: International financial experts and economists, as well as historical analyses from neutral observers, corroborate that the initial demands were indeed unsustainable and that the problem of German economic viability was a genuine concern. Allied creditors, while unhappy with reduced payments, largely acknowledged the economic realities.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).
:- end_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the substantial, but not maximal, burden placed on Germany, which was continuously negotiated downward. Suppression (0.30) was moderate, as Germany had some leverage through the threat of economic collapse, but was still subject to Allied enforcement. The theater ratio (0.10) was low, as the economic arguments for limitation were genuinely influential, not merely performative. The decreasing extractiveness and suppression over the interval reflect the success of this reading in reducing the burden on Germany.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of German elites, this reading was a necessary act of economic realism and a defense against punitive extraction. From the perspective of French hardliners and occupied territories, it was a betrayal of justice and a failure to hold Germany fully accountable. The engine's classification will reflect the structural reality of reduced extraction, which aligns with the German perspective, while acknowledging the costs borne by others.
 *
 * DIRECTIONALITY LOGIC:
 *   German elites and the German economy were the primary beneficiaries, as this reading directly reduced their financial obligations. Allied creditors and occupied territories were the victims, as they received less compensation than initially sought or promised. International financial experts acted as agenda-setters by providing the economic rationale for this reading. French hardliners were excluded from the dominant discourse as their punitive stance became economically untenable.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_capacity_objectivity,
    'To what extent was ''German economic capacity'' an objective, empirically determinable limit, versus a politically negotiated construct influenced by German lobbying and Allied self-interest?',
    'Counterfactual economic modeling of alternative payment structures and their impact on German and European economies, alongside archival research into the political pressures on economic experts.',
    'If largely a political construct, the ''limited responsibility'' reading''s coordination function is weaker, and its extractive component (from Allied creditors) is more pronounced, potentially shifting its classification closer to a Snare for the Allied side. If objective, it reinforces the Rope/Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_capacity_objectivity, empirical, 'Ambiguity of ''economic capacity'' as a constraint on reparations.').

omega_variable(
    article_231_interpretation,
    'Was the interpretation of Article 231 as a ''legal formality'' a genuine legal consensus, or a strategic re-framing by German and sympathetic parties to reduce liability?',
    'Analysis of contemporary legal scholarship and diplomatic correspondence from diverse national perspectives, particularly from neutral legal experts.',
    'If a strategic re-framing, it highlights the performative aspect of the constraint (higher theater_ratio) and the suppression of alternative legal interpretations, potentially increasing its effective extractiveness from Allied creditors. If a genuine legal consensus, it reinforces the constraint''s legitimacy as a coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_231_interpretation, conceptual, 'Legal vs. political interpretation of Article 231.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1919, 0.05).
narrative_ontology:measurement_basis(vers_tr_t1919, observed).
narrative_ontology:measurement(vers_tr_t1924, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1924, 0.1).
narrative_ontology:measurement_basis(vers_tr_t1924, observed).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1929, 0.15).
narrative_ontology:measurement_basis(vers_tr_t1929, observed).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1932, 0.2).
narrative_ontology:measurement_basis(vers_tr_t1932, observed).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1919, 0.65).
narrative_ontology:measurement_basis(vers_be_t1919, observed).
narrative_ontology:measurement(vers_be_t1924, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1924, 0.45).
narrative_ontology:measurement_basis(vers_be_t1924, observed).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1929, 0.35).
narrative_ontology:measurement_basis(vers_be_t1929, observed).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1932, 0.2).
narrative_ontology:measurement_basis(vers_be_t1932, observed).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1919, 0.7).
narrative_ontology:measurement_basis(vers_su_t1919, observed).
narrative_ontology:measurement(vers_su_t1924, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1924, 0.5).
narrative_ontology:measurement_basis(vers_su_t1924, observed).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1929, 0.3).
narrative_ontology:measurement_basis(vers_su_t1929, observed).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1932, 0.15).
narrative_ontology:measurement_basis(vers_su_t1932, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Versailles Reparations Clauses kernel. This 'limited responsibility' reading directly influenced the practical implementation of reparations, reducing the burden on Germany compared to the 'punitive liability' reading, and was itself challenged by the 'repudiation' reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
