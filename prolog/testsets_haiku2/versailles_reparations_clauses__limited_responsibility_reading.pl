% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Versailles Reparations Under Capacity Constraint (Limited Responsibility Reading)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Treaty of Versailles imposed reparations on Germany to compensate
 *   Allied war costs and occupied-territory reconstruction. The limited
 *   responsibility reading interprets Article 231 (the 'war guilt clause') as
 *   a legal formality establishing liability, not a moral judgment of unique
 *   culpability. Under this reading, Germany bears an obligation bounded by
 *   economic capacity: payment schedules must remain viable to ensure actual
 *   transfers occur. This reading grounds legitimacy in technical economic
 *   analysis and the principle of sustainable extraction rather than in
 *   punitive doctrine. The constraint is the capacity ceiling itself — the
 *   reading treats the binding constraint as economic viability, not Allied
 *   maximalism. The Dawes Plan (1924) instantiated this reading, revising
 *   downward the initial extraction schedule. The punitive reading, by
 *   contrast, treats Article 231 as grounding quasi-unlimited claims; the
 *   repudiation reading treats the treaty itself as illegitimate.
 *
 * KEY AGENTS:
 *   - German government (Weimar): bound by treaty, must extract from taxpayers; constrained by economic capacity
 *   - German taxpayers and working class: ultimate payers; identity-locked to German state
 *   - Allied creditor governments: beneficiaries; agenda-setters; arbitraging between maximalist demands and collection reality
 *   - Occupied territories (Belgium, France): beneficiaries; constrained by German capacity to pay
 *   - Neutral economists: observers; provide technical authority for capacity constraints
 *   - German economic elites: excluded from negotiation but implicitly protected by capacity constraint (prevents confiscatory extraction)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.58).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.42).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations Under Capacity Constraint (Limited Responsibility Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, '8957c6ec-06a6-4c51-be1f-859e45a2acf3').
narrative_ontology:cs_kernel_codification('8957c6ec-06a6-4c51-be1f-859e45a2acf3', fixed_text).
narrative_ontology:cs_authority_grounding('8957c6ec-06a6-4c51-be1f-859e45a2acf3', lineage).
narrative_ontology:cs_interpretation_layer_present('8957c6ec-06a6-4c51-be1f-859e45a2acf3').
narrative_ontology:cs_reading_relation('8957c6ec-06a6-4c51-be1f-859e45a2acf3', versailles_reparations_clauses__punitive_liability_reading, coexists_with).
narrative_ontology:cs_reading_relation('8957c6ec-06a6-4c51-be1f-859e45a2acf3', versailles_reparations_clauses__repudiation_reading, influences).
narrative_ontology:cs_axiom('8957c6ec-06a6-4c51-be1f-859e45a2acf3', foundational, capacity_bounds_enforceable_liability).
narrative_ontology:cs_axiom_status(capacity_bounds_enforceable_liability, holdable).
narrative_ontology:cs_axiom_grounding('8957c6ec-06a6-4c51-be1f-859e45a2acf3', capacity_bounds_enforceable_liability, empirically_contingent).
narrative_ontology:cs_axiom('8957c6ec-06a6-4c51-be1f-859e45a2acf3', foundational, article_231_legal_not_moral_determination).
narrative_ontology:cs_axiom_status(article_231_legal_not_moral_determination, holdable).
narrative_ontology:cs_axiom_grounding('8957c6ec-06a6-4c51-be1f-859e45a2acf3', article_231_legal_not_moral_determination, deontological).
narrative_ontology:cs_reference_frame('8957c6ec-06a6-4c51-be1f-859e45a2acf3', capacity_constrained_reparations_framework).
narrative_ontology:cs_drift_state('8957c6ec-06a6-4c51-be1f-859e45a2acf3', post_1929_great_depression, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8957c6ec-06a6-4c51-be1f-859e45a2acf3', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_governments).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, occupied_territory_governments).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, german_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, german_working_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, german_government_weimar).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the obligation to pay reparations from a war it lost and a treaty it was forced to sign. Under the limited responsibility reading, payment schedules are bounded by actual economic capacity — the German state must extract the funds from its population and economy, but cannot be compelled to pay beyond what the economy can sustainably bear. The reading constrains Allied maximalism by insisting viability discipline.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_government_weimar, payer,
    moderate, biographical, constrained, national).

% Ultimately bear the reparations burden through taxation and inflation. Under the limited responsibility reading, the constraint is that payments must stay within economic bounds — but they still bear the cost, just at a modulated level rather than the confiscatory rates the punitive reading would impose. Exit is impossible: they cannot leave the territory, cannot refuse contribution to state obligations.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_taxpayers, payer,
    powerless, biographical, trapped, national).

% Wage earners and pensioners whose real income is eroded by the state's reparations extraction and the inflation that accompanies it. The limited responsibility reading moderates the extraction compared to punitive readings, but does not eliminate it. Their identity as German citizens (inability to exit or claim exemption) makes them permanently liable. They experience the constraint as a permanent drag on living standards.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_working_class, payer,
    powerless, biographical, identity_locked, national).

% Receive reparations payments and forgiven war debts from Germany and from each other. Under the limited responsibility reading, they set a payment schedule that aligns with German capacity rather than maximizing extraction immediately. This is a moderation compared to the punitive reading, but they retain the power to enforce the schedule and renegotiate it if circumstances change. They arbitrage between the German capacity constraint and their own creditor interests.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_governments, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_governments, agenda_setter).

% Belgium, France, and other occupied/devastated territories receive direct reparations for war damage and occupation costs. Under the limited responsibility reading, they receive compensation bounded by German economic viability — less than maximalist readings would demand, but structured to reflect their demonstrated losses. They are constrained by the fact that squeezing Germany beyond viability prevents Germany from actually paying.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, occupied_territory_governments, beneficiary,
    moderate, generational, constrained, regional).

% Industrial and financial elites can shift capital, relocate operations, or exit to neutral countries to avoid the reparations burden. The limited responsibility reading does not directly address their exit, but the capacity constraint implicitly protects them by preventing the state from confiscatory extraction that would force asset liquidation. Their exclusion from the negotiation table means their interests (minimizing capital flight) shape the reading's negotiating logic without being explicitly represented.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_economic_elites, excluded,
    powerful, generational, mobile, global).

% The League's role is to oversee the reparations process and arbitrate disputes. Under the limited responsibility reading, the League becomes the institutional mediator ensuring that capacity constraints are respected and that the schedule remains viable. The reading grounds legitimacy in technical economic analysis, not moral judgment.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, league_of_nations, observer,
    institutional, generational, analytical, global).

% Economic advisors and technical experts who measure Germany's capacity and recommend sustainable payment schedules. Under the limited responsibility reading, their voice becomes authoritative: economic viability, not liability doctrine, determines the binding constraint. They operate from outside the belligerent parties and claim to render judgment on facts, not values.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, neutral_economists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_governments).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__limited_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles the requirement to compensate war damage (a genuine coordination problem: how to distribute the costs of war across responsible and affected parties) with the economic reality that extraction beyond viability prevents actual payment and destabilizes the paying state, creating cascading defaults and political instability.
% TRANSFER_FUNCTION: Transfers capital, goods, and future tax revenue from the German state to Allied creditor governments and occupied territories. Under the limited responsibility reading, the flow is modulated: bounded by sustainable extraction levels rather than maximalist confiscation. The transfer is from German taxpayers and workers to foreign governments and territories.
% ABSENT_VOICES: German citizens in occupied territories (the Rhineland, Saar) have no representation in the reparations negotiation; they bear occupation costs and reparations extraction simultaneously but cannot voice their interests. German economic elites, though powerful, are excluded from formal negotiation, meaning their capital-flight concerns shape the reading's logic as a constraint rather than as explicit representation.
% DISAPPEARANCE_RATIONALE: If the reparations obligation disappeared overnight, Allied governments would lose the expected revenue stream, occupied territories would lose compensation for war damage, and Germany's fiscal and monetary policy would shift radically — the constraint's disappearance would require massive institutional renegotiation across Europe's post-war settlement.
% FOUNDING_PROBLEM: Germany initiated total war and lost it. The Allies sustained enormous casualties, displaced populations, and material destruction. A binding obligation to compensate war damage serves the principle that the aggressor-loser funds the restoration of the aggressed-upon. The limited responsibility reading frames the founding problem narrowly: the need for compensation bounded by the paying state's capacity to actually deliver it, preventing the trap of perpetual default and instability.
% FOUNDING_PROBLEM_CORROBORATION: The limited responsibility reading invokes economic evidence from Keynes, the Dawes Plan economists, and later League of Nations technical committees, all attesting that payment schedules above German capacity lead to default and economic collapse rather than sustained transfers. The punitive reading disputes this, claiming moral responsibility overrides economic reality. Occupied territories and Allied governments differ internally on whether capacity or liability is the binding constraint.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__limited_responsibility_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.58-0.72) because the constraint transfers substantial capital from Germany to others, and this transfer is sustained by coercive treaty enforcement. Suppression is moderate (0.42-0.68) because the constraint does not require intrusive domestic surveillance — it operates through the state's fiscal machinery and trade restrictions — but compliance is backed by Allied occupying forces in the Rhineland. Theater is low-to-moderate (0.15-0.32) because the reparations transfers are real economic flows, though the 'capacity constraint' framing carries theatrical elements (the appeal to technical neutrality obscures the power asymmetry in setting schedules). The measurement trajectory shows extraction declining from 1919 (Versailles initial schedule) to 1928 (Dawes Plan era stability), then rising again in 1930 (Young Plan and Great Depression instability). Suppression follows the opposite pattern: high under the initial draconian schedules (1919-1921), declining as the capacity-constraint reading gains acceptance (1924-1928), rising again in 1930 as compliance pressure mounts during economic crisis.
 *
 * PERSPECTIVAL GAP:
 *   From the Allied agenda-setter seat, the constraint is a capacity-bounded transfer mechanism that honors both Germany's liability and economic reality — coordination between creditors and debtor. From the German payer seat (taxpayers and workers), the constraint is extraction enforced by an external power, modulated but not eliminated by capacity considerations. From the occupied-territory seat, the constraint is compensation that falls short of actual damage because Germany's capacity is the binding limit, not Allied need. From the neutral-economist seat, the constraint is a technical truth: payments above capacity lead to default, inflation, and instability, so capacity is simply the constraint that facts impose. The engine computes each seat's type from the structural data; the reading's claim is that capacity-constraint logic gives all seats a reason to treat the constraint as a bounded tangled rope (real coordination via sustainable extraction) rather than as a snare (pure confiscatory extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Allied governments and occupied territories are the beneficiaries (receive transfers) with high institutional power and arbitrage options — low directionality (d near 0.1-0.25 range). German taxpayers and workers are the victims (bear the extraction) with powerless position and identity-locked exit — high directionality (d near 0.8-0.95 range). The German government is partially constrained (must extract but cannot exceed capacity) and partially agenda-setting (negotiates schedules), placing it around d=0.55. The limited responsibility reading's key move is to argue that the capacity constraint, while extractive, prevents worse outcomes and ensures actual payment — it frames d as bounded by viability rather than maximalist extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (compensation for war damage) was live in 1919-1924 but contested by 1928 and dead by 1932 (as economic depression made any meaningful reparations payment impossible). Yet the constraint persists theatrically — the Young Plan (1929) revises schedules again, creating the appearance of ongoing compensation while actual transfers shrink. By 1932, Germany defaults and the constraint's function has atrophied. The limited responsibility reading itself becomes mandatrophic: it began as a technical solution (capacity bounds extraction) and ended as a cover story for non-payment. The theater_ratio trajectory captures this: rising from 1924 onward as the capacity-constraint reading becomes more theatrical (schedules revised repeatedly, transfers fall short, blame deflects to economic 'necessity') even as actual reparations decline. Mandatrophy is resolved when Germany explicitly repudiates the obligation in 1933 — the constraint vanishes not through reform but through denial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_231_interpretive_scope,
    'Does Article 231 establish a legal formula for calculating unlimited liability, or does it merely ground the principle of German responsibility while leaving the amount to separate negotiation on capacity constraints?',
    'Textual analysis of Article 231''s language in original treaty, comparison with drafting intent (Clemenceau, Wilson, Lloyd George statements), and subsequent Allied reinterpretation across the Dawes and Young Plans.',
    'If Article 231 is read as procedural (grounds liability, amount negotiable), the limited responsibility reading is structurally sound. If read as substantive (implies quasi-unlimited quantum), the punitive reading becomes the primary one and the limited responsibility reading is subordinate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_231_interpretive_scope, conceptual, 'Whether Article 231 determines the method of reparations calculation or merely assigns liability.').

omega_variable(
    german_actual_capacity_measurement,
    'What was Germany''s genuine economic capacity to sustain reparations payments over the 1919-1932 period? Did the capacity constraints invoked by the limited responsibility reading reflect objective economic limits or political negotiating positions?',
    'Economic historians (Kindleberger, Temin, Eichengreen) have conducted detailed analysis of German fiscal, monetary, and trade capacity across the period. Cross-reference actual capacity against claimed capacity in Dawes and Young Plans.',
    'If Germany''s actual capacity exceeded the claimed limits, the limited responsibility reading was deployed as a cover story for Allied concession (makes it a snare, not a tangled rope). If actual capacity was near the limits claimed, the reading is technically sound (genuine tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(german_actual_capacity_measurement, empirical, 'Whether capacity constraints were objective limits or strategic negotiating positions.').

omega_variable(
    suppression_mechanism_origin,
    'Is the suppression in the reparations constraint structural (backed by Allied occupying forces and trade sanctions) or internalized (German governments and elites accept the obligation as morally binding)?',
    'Historical examination of resistance moments: the Ruhr occupation (1923) tested structural enforcement; German political discourse (Weimar debates, Nazi repudiation rhetoric) tested internalization. Post-exit behavior (Nazi repudiation followed by rearmament, not compliance resumption) shows suppression was primarily structural, not internalized.',
    'If primarily structural, the measured suppression (0.42-0.68) overstates the constraint''s internalized legitimacy and makes it closer to a snare than a tangled rope. If substantially internalized, the constraint''s persistence relies partly on German acceptance of the obligation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_origin, empirical, 'Whether suppression in the reparations constraint is structural or internalized.').

omega_variable(
    reading_kernel_boundary_ambiguity,
    'Is the limited_responsibility_reading a distinct reading of the same kernel (the Treaty''s reparations text), or is it a separate constraint entirely (an economic-viability rule superimposed on the treaty)?',
    'Examine whether the reading is grounded in plausible interpretation of Article 231''s text (unified kernel interpretation) or whether capacity constraints are external principles imposed by Dawes Committee economists (separate constraint logic).',
    'If grounded in Article 231''s interpretation, it is one reading of the kernel. If external, the constraint is not a reading of the treaty but a separate economic principle. This affects how the sibling-reading relations are framed (are the readings competing interpretations of one text, or are they separate claims?).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_boundary_ambiguity, conceptual, 'Whether the limited responsibility reading is a textual interpretation or an external principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1919, 0.15).
narrative_ontology:measurement_basis(vers_tr_t1919, observed).
narrative_ontology:measurement(vers_tr_t1921, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1921, 0.18).
narrative_ontology:measurement_basis(vers_tr_t1921, observed).
narrative_ontology:measurement(vers_tr_t1924, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1924, 0.24).
narrative_ontology:measurement_basis(vers_tr_t1924, observed).
narrative_ontology:measurement(vers_tr_t1928, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1928, 0.28).
narrative_ontology:measurement_basis(vers_tr_t1928, observed).
narrative_ontology:measurement(vers_tr_t1930, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1930, 0.32).
narrative_ontology:measurement_basis(vers_tr_t1930, observed).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1932, 0.28).
narrative_ontology:measurement_basis(vers_tr_t1932, observed).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1919, 0.72).
narrative_ontology:measurement_basis(vers_be_t1919, observed).
narrative_ontology:measurement(vers_be_t1921, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1921, 0.68).
narrative_ontology:measurement_basis(vers_be_t1921, observed).
narrative_ontology:measurement(vers_be_t1924, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1924, 0.62).
narrative_ontology:measurement_basis(vers_be_t1924, observed).
narrative_ontology:measurement(vers_be_t1928, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1928, 0.58).
narrative_ontology:measurement_basis(vers_be_t1928, observed).
narrative_ontology:measurement(vers_be_t1930, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1930, 0.65).
narrative_ontology:measurement_basis(vers_be_t1930, observed).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1932, 0.58).
narrative_ontology:measurement_basis(vers_be_t1932, observed).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1919, 0.68).
narrative_ontology:measurement_basis(vers_su_t1919, observed).
narrative_ontology:measurement(vers_su_t1921, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1921, 0.62).
narrative_ontology:measurement_basis(vers_su_t1921, observed).
narrative_ontology:measurement(vers_su_t1924, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1924, 0.48).
narrative_ontology:measurement_basis(vers_su_t1924, observed).
narrative_ontology:measurement(vers_su_t1928, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1928, 0.42).
narrative_ontology:measurement_basis(vers_su_t1928, observed).
narrative_ontology:measurement(vers_su_t1930, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1930, 0.55).
narrative_ontology:measurement_basis(vers_su_t1930, observed).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1932, 0.42).
narrative_ontology:measurement_basis(vers_su_t1932, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__limited_responsibility_reading, 0.18).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, german_hyperinflation_1921_1923).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, dawes_plan_restructuring).

% DUAL FORMULATION NOTE:
% The versailles_reparations_clauses kernel decomposes into three constraint stories, each a reading of the contested Article 231 and the reparations schedule. The limited_responsibility_reading frames the constraint as a capacity-bounded transfer mechanism. The punitive_liability_reading frames it as quasi-unlimited liability for total war costs. The repudiation_reading treats the treaty itself as illegitimate. Each reading has a different ε, different beneficiary/victim structure, and different persistence logic. The limited_responsibility_reading influences the other two: if capacity constraints prove binding, the punitive reading becomes politically untenable; if capacity can be exceeded, the repudiation reading gains force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, powerful, 0.15).
constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, moderate, 0.52).
constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, powerless, 0.87).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
