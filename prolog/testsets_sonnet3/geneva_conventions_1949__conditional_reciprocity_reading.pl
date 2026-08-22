% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions — Conditional Reciprocity Reading (Article 4 POW-Status Gate)
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the conditional-reciprocity reading of the
 *   Geneva Conventions kernel: the Conventions function as a mutual-restraint
 *   bargain that applies fully only when both sides comply with its
 *   organizational preconditions (Article 4's command structure, insignia,
 *   and open-carry requirements), and non-compliance by irregular forces
 *   licenses proportional degradation of the protections they would otherwise
 *   receive. This is a state-military-doctrine reading, distinct from the
 *   humanitarian-ceiling reading (which treats the protections as
 *   unconditional floors regardless of adversary conduct) and the
 *   security-maximization reading (which treats the Conventions as
 *   aspirational constraints properly suspended under operational necessity
 *   in asymmetric conflict). The three readings are not the same constraint
 *   measured differently — they have different ε values, different
 *   beneficiary/victim structures, and different classification, because they
 *   encode different structural claims about when and how far restraint
 *   extends.
 *
 * KEY AGENTS:
 *   - state_militaries: agenda_setter/beneficiary — set and interpret Article 4 criteria, retain unilateral classification authority
 *   - regular_uniformed_combatants: beneficiary — automatically satisfy the criteria by institutional form
 *   - captured_irregular_fighters: payer — forfeit POW protection when organizational form does not match the template
 *   - civilian_populations_in_asymmetric_conflict: payer — bear expanded proportionality tolerances when reciprocity is invoked
 *   - international_committee_of_the_red_cross: observer/excluded — monitors but cannot bind classification decisions
 *   - irregular_force_leadership: excluded — structurally unable to meet criteria, absent from drafting and classification process
 *   - military_legal_advisors: agenda_setter/observer — operationalize the reciprocity doctrine in the field
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.52).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.61).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions — Conditional Reciprocity Reading (Article 4 POW-Status Gate)").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, '599f3985-9e86-4a74-a76d-4d2692da7186').
narrative_ontology:cs_kernel_codification('599f3985-9e86-4a74-a76d-4d2692da7186', fixed_text).
narrative_ontology:cs_authority_grounding('599f3985-9e86-4a74-a76d-4d2692da7186', lineage).
narrative_ontology:cs_interpretation_layer_present('599f3985-9e86-4a74-a76d-4d2692da7186').
narrative_ontology:cs_reading_relation('599f3985-9e86-4a74-a76d-4d2692da7186', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('599f3985-9e86-4a74-a76d-4d2692da7186', geneva_conventions_1949__security_maximization_reading, influences).
narrative_ontology:cs_axiom('599f3985-9e86-4a74-a76d-4d2692da7186', foundational, protection_conditioned_on_reciprocal_compliance).
narrative_ontology:cs_axiom_status(protection_conditioned_on_reciprocal_compliance, holdable).
narrative_ontology:cs_axiom_grounding('599f3985-9e86-4a74-a76d-4d2692da7186', protection_conditioned_on_reciprocal_compliance, conventional).
narrative_ontology:cs_axiom('599f3985-9e86-4a74-a76d-4d2692da7186', secondary, organizational_form_as_lawful_combatancy_proxy).
narrative_ontology:cs_axiom_status(organizational_form_as_lawful_combatancy_proxy, holdable).
narrative_ontology:cs_axiom_grounding('599f3985-9e86-4a74-a76d-4d2692da7186', organizational_form_as_lawful_combatancy_proxy, instrumental).
narrative_ontology:cs_reference_frame('599f3985-9e86-4a74-a76d-4d2692da7186', id_1949_conventional_state_conflict_baseline).
narrative_ontology:cs_drift_state('599f3985-9e86-4a74-a76d-4d2692da7186', post_2001_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('599f3985-9e86-4a74-a76d-4d2692da7186', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, regular_uniformed_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, captured_irregular_fighters).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations_in_asymmetric_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft, ratify, and operationalize the criteria that determine who receives POW protection. Interpret Article 4's organized-command, insignia, and open-carry requirements in the field, classify detainees accordingly, and retain wide discretion because the classification decision is made unilaterally by the detaining power. Benefit from a framework that treats their own uniformed forces as automatically protected while withholding equivalent protection from adversaries who do not organize along state-military lines.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries, beneficiary).

% Serve in structures that satisfy the Article 4 criteria by construction — a standing chain of command, standard-issue insignia, openly carried weapons. Receive full POW protection upon capture as a near-automatic consequence of their institutional form, regardless of individual conduct.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, regular_uniformed_combatants, beneficiary,
    organized, immediate, constrained, national).

% Fight without the organizational trappings the Convention treats as proxies for lawful status — often because clandestine operation is a survival necessity against a technologically superior adversary, not a choice to evade restraint. Upon capture, are frequently classified as unlawful or unprivileged combatants, forfeiting POW protections and becoming subject to domestic criminal prosecution, prolonged detention without POW status, and interrogation regimes the Convention would otherwise bar. Have no practical route to contest the classification before an impartial body in most conflicts.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, captured_irregular_fighters, payer,
    powerless, immediate, trapped, regional).

% Live in the theaters where irregular forces operate. Bear the proportionality calculations this reading permits: when adversary non-compliance is invoked, the threshold for what counts as an acceptable collateral cost shifts, and civilian harm that would be barred under a strict humanitarian-floor reading becomes defensible under a degradation-permitted reading. Cannot exit the conflict zone in most cases and have no seat in either belligerent's classification or targeting decisions.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations_in_asymmetric_conflict, payer,
    powerless, biographical, trapped, regional).

% Monitors compliance, visits detainees, and advocates for humanitarian-floor interpretations, but has no enforcement power over how a detaining state classifies captured irregulars. Its findings are advisory; states retain final unilateral classification authority, so the ICRC's objections function as moral pressure rather than a binding check.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_committee_of_the_red_cross, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, international_committee_of_the_red_cross, excluded).

% Structurally unable to satisfy the Article 4 criteria in many asymmetric conflicts — visible insignia and open carry are often tactically suicidal against a superior force — and are not party to the treaty-drafting or field-classification process. Their argument that the criteria encode a specific mid-20th-century state-military template as the definition of 'lawful' is not represented in the classification apparatus itself.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_force_leadership, excluded,
    organized, biographical, constrained, regional).

% Interpret and apply the reciprocity-conditioned framework in operational law, advising commanders on proportionality calculations and detainee classification. Operate inside the doctrine as a matter of professional obligation and have institutional incentive to preserve the discretion the conditional-reciprocity reading grants.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, military_legal_advisors, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, military_legal_advisors, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__conditional_reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a mutual-restraint bargain among belligerents: each side foregoes certain forms of violence and grants certain protections in exchange for the expectation that the adversary will do the same, making warfare somewhat more survivable and predictable for combatants and civilians who fall under it.
% TRANSFER_FUNCTION: Moves protective status (POW treatment, detention safeguards, proportionality-limited targeting) toward parties whose military organization mirrors the state form, and away from parties whose organization does not — with the reciprocity condition permitting degradation of restraint toward the latter group and, derivatively, toward civilian populations in the theaters where they operate.
% ABSENT_VOICES: Irregular force leadership and the civilian populations embedded in asymmetric conflict zones have no seat in either the treaty's drafting history or the unilateral field classification process; their structural objection — that the Article 4 criteria encode a specific organizational template as the price of protection — is not represented in the apparatus that applies the criteria to them.
% DISAPPEARANCE_RATIONALE: State militaries and their legal advisors would argue the world barely changes — customary international law and domestic rules of engagement would substantially persist. Captured irregular fighters and civilian-protection advocates would argue the world changes substantially: without the conditional-reciprocity reading's classification gate, either a humanitarian floor applies unconditionally (better for irregulars and civilians) or protections collapse further toward pure operational necessity (worse) — the direction of change is itself disputed, which is why the verdict is contested rather than settled either way.
% FOUNDING_PROBLEM: Post-WWII drafters sought to prevent atrocities against captured combatants and civilians by codifying reciprocal restraints that both regular armies could be expected to honor and verify, addressing behavior seen in the preceding war (mistreatment of POWs, targeting of civilians) among conventional state militaries.
% FOUNDING_PROBLEM_CORROBORATION: Military legal scholars and state defense establishments attest the reciprocity logic remains necessary to prevent unilateral disarmament of restraint in the field. Human rights organizations, ICRC commentary, and international law scholars outside state military establishments attest the founding problem has shifted: contemporary conflict is dominated by asymmetric warfare the reciprocity framework was not built for, and the conditional gate now functions less to prevent atrocity and more to license it against a structurally disadvantaged class of fighters.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, contested).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) reflects a genuine but partial coordination function: the reciprocal-restraint bargain does moderate state violence relative to no framework at all, but the reciprocity condition creates an asymmetric extraction channel — captured irregulars and the civilians around them bear costs (denied POW status, expanded proportionality tolerance) that regular combatants and the drafting/enforcing states do not. Suppression (0.61) is substantial because the classification decision is made unilaterally by the detaining power with no binding external check — the ICRC's monitoring role is advisory, not adjudicative. Theater ratio (0.30) is moderate: the humanitarian-monitoring apparatus (ICRC visits, reporting) is real but has grown as a proportion of total activity relative to substantive protection outcomes, especially post-2001 as asymmetric conflict became the dominant conflict form the reading was not originally built for.
 *
 * PERSPECTIVAL GAP:
 *   From the state-military seat, this reading is a workable, verifiable bargain: reciprocity is what makes restraint sustainable rather than a unilateral disarmament. From the captured-irregular seat, the identical structure operates as a trap — the criteria for protection are set by the more powerful party, applied by the more powerful party, and satisfied disproportionately by the more powerful party's own forces. The engine computing different types across these seats from the same structural data is the intended signal, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries and regular uniformed combatants sit near the beneficiary end: the criteria for protection were drafted around, and are satisfied by, the standard state-military organizational form, so protection flows to them close to automatically. Captured irregular fighters sit near the full-target end: their organizational necessities (clandestine operation against a superior force) place them outside the criteria by structural design, not by culpable choice, and the classification decision that strips their protection is made by the same party that benefits from stripping it. Civilian populations are targets by proximity — they do not choose their combatants' organizational form but absorb the proportionality consequences of it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing WWII-style atrocities against captured conventional combatants — is largely solved for the population the framework was built around (state-to-state conventional war). Applied to asymmetric conflict, the same criteria now function differently: what began as a floor-raising bargain for a narrow class of conflict has been extended, largely unmodified, to a conflict form (irregular/asymmetric warfare) it was not designed for, producing a mismatch between the doctrine's original coordination purpose and its current field application. This is not full mandatrophy — the reciprocity logic still serves a coordination function among states with organized militaries — but the founding-problem status is genuinely contested rather than settled, which is why founding_problem_status is authored as 'contested' rather than 'dead'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_condition_legitimacy,
    'Is conditioning full protection on adversary compliance a legitimate feature of a genuinely reciprocal bargain, or is it a mechanism that transfers the cost of one party''s organizational disadvantage onto captured individuals who did not choose that disadvantage?',
    'Comparative analysis of state practice: do states applying the conditional-reciprocity reading against irregular forces also apply proportional degradation against each other in inter-state conflicts with comparable non-compliance, or is the degradation asymmetric by conflict type?',
    'If degradation is applied symmetrically across conflict types, the reciprocity logic is more defensible as principled coordination. If applied predominantly against irregular/asymmetric adversaries specifically, the reading functions closer to a targeted extraction mechanism dressed as reciprocity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_condition_legitimacy, conceptual, 'Whether the conditional-reciprocity logic is symmetric coordination or asymmetric extraction in practice.').

omega_variable(
    article_4_criteria_neutrality,
    'Are the Article 4 organizational criteria (command structure, insignia, open carry) neutral proxies for combatant identifiability, or do they encode the organizational form of state militaries as the definition of lawful combatancy, structurally disadvantaging any force that must operate clandestinely to survive?',
    'Historical and doctrinal analysis of the 1949 drafting record and subsequent state practice: were the criteria selected for genuinely neutral identifiability purposes, or did they track the organizational form the drafting states already possessed?',
    'If the criteria are neutral, the conditional-reciprocity reading is a defensible operationalization of a real distinguishability concern. If the criteria encode a specific organizational template, the reading''s classification gate functions partly as institutional self-dealing by state militaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_4_criteria_neutrality, conceptual, 'Whether Article 4''s criteria are neutral or state-military-biased by design.').

omega_variable(
    framing_choice_kernel_vs_reading,
    'Should this constraint be authored at the level of the reciprocity DOCTRINE (as done here) or at the level of a specific enforcement EPISODE (e.g., a particular conflict''s classification decisions), given that ε could plausibly differ across episodes even within this reading?',
    'Track ε across multiple documented classification episodes under this reading (e.g., different conflicts, different detaining states) to test whether ε is stable at the doctrine level or varies enough to require further decomposition into episode-level stories.',
    'If ε varies substantially across episodes within this reading, the reading itself should be decomposed further per the ε-invariance principle, rather than treated as one stable constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_choice_kernel_vs_reading, conceptual, 'Whether the doctrine-level framing chosen here is the correct grain, or whether episode-level decomposition is required.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1949, 0.12).
narrative_ontology:measurement(gene_tr_t1965, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(gene_tr_t1980, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2001, 0.24).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1949, 0.28).
narrative_ontology:measurement(gene_be_t1965, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1965, 0.33).
narrative_ontology:measurement(gene_be_t1980, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2001, 0.46).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1949, 0.4).
narrative_ontology:measurement(gene_su_t1965, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1965, 0.44).
narrative_ontology:measurement(gene_su_t1980, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2001, 0.56).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2010, 0.59).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2024, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__conditional_reciprocity_reading, 0.1).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, security_maximization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept 'the Geneva Conventions govern armed conflict.' Each reading (conditional_reciprocity_reading, humanitarian_ceiling_reading, security_maximization_reading) has a distinct ε, distinct beneficiary/victim structure, and distinct claimed type, because each encodes a structurally different claim about when and how far the Conventions' restraints extend. They are linked here rather than merged because ε is not invariant across the readings — averaging or hedging across them would violate the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
