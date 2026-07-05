% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Exclusion as Non-Binding Frankish Particularism (Cognatic Reversion Reading)
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This story instantiates the cognatic-reversion reading of the contested
 *   Salic-prohibition kernel: the claim that Salic exclusion of female
 *   succession is a Frankish tribal-inheritance rule from the Pactus Legis
 *   Salicae, never properly received as binding constitutional law in
 *   territories outside the original Frankish successoral order, and
 *   therefore inapplicable when invoked against a direct female or cognatic
 *   heir in those territories. Under this reading, jurisdictional territorial
 *   integrity and the indigenous customary line take priority over agnatic
 *   purity; the eldest child, regardless of sex, is the rightful heir absent
 *   genuine local reception of the Frankish rule. The sibling readings — that
 *   Salic Law is an immutable natural/divine dynastic constitution
 *   (immutable_mandate_reading), and that it is a revocable positive law
 *   subject to sovereign amendment (sovereign_override_reading) — are
 *   separate constraints, not alternate measurements of this one; each
 *   carries its own epsilon and stakeholder structure.
 *
 * KEY AGENTS:
 *   - male_collateral_claimants: Primary beneficiary (powerful/arbitrage) — inherits the crown via the exclusion
 *   - agnatic_court_factions: Agenda-setter (organized/constrained) — administers and selectively invokes the rule
 *   - female_dynastic_heirs: Primary target (powerless/trapped) — displaced from rightful succession
 *   - cognatic_line_territories: Secondary payer (moderate/constrained) — bears fractured territorial integrity
 *   - foreign_dynastic_courts: Excluded institutional actor (institutional/mobile) — has recognition interest but no seat at adjudication
 *   - legal_historians: Analytical observer (analytical/analytical) — traces genealogy of the rule's origin and reception
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.61).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.58).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Exclusion as Non-Binding Frankish Particularism (Cognatic Reversion Reading)").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__cognatic_reversion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, '9eb806f4-a836-4416-b8ba-32881c61c490').
narrative_ontology:cs_kernel_codification('9eb806f4-a836-4416-b8ba-32881c61c490', fixed_text).
narrative_ontology:cs_authority_grounding('9eb806f4-a836-4416-b8ba-32881c61c490', extraction).
narrative_ontology:cs_interpretation_layer_present('9eb806f4-a836-4416-b8ba-32881c61c490').
narrative_ontology:cs_reading_relation('9eb806f4-a836-4416-b8ba-32881c61c490', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('9eb806f4-a836-4416-b8ba-32881c61c490', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_axiom('9eb806f4-a836-4416-b8ba-32881c61c490', foundational, salic_law_requires_documented_territorial_reception).
narrative_ontology:cs_axiom_status(salic_law_requires_documented_territorial_reception, holdable).
narrative_ontology:cs_axiom_grounding('9eb806f4-a836-4416-b8ba-32881c61c490', salic_law_requires_documented_territorial_reception, empirically_contingent).
narrative_ontology:cs_axiom('9eb806f4-a836-4416-b8ba-32881c61c490', foundational, territorial_integrity_outranks_agnatic_purity).
narrative_ontology:cs_axiom_status(territorial_integrity_outranks_agnatic_purity, holdable).
narrative_ontology:cs_axiom_grounding('9eb806f4-a836-4416-b8ba-32881c61c490', territorial_integrity_outranks_agnatic_purity, instrumental).
narrative_ontology:cs_reference_frame('9eb806f4-a836-4416-b8ba-32881c61c490', frankish_tribal_inheritance_custom).
narrative_ontology:cs_drift_state('9eb806f4-a836-4416-b8ba-32881c61c490', post_reception_dispute_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('9eb806f4-a836-4416-b8ba-32881c61c490', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, male_collateral_claimants).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, agnatic_court_factions).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, female_dynastic_heirs).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, cognatic_line_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A collateral male line invokes Salic exclusion to displace a direct female heir, even though the territory in question was never part of the original Frankish successoral order. They gain the crown, the treasury, and the loyalty of agnatic-leaning nobility by asserting a rule whose jurisdictional reach over this specific territory is itself contested.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, male_collateral_claimants, beneficiary,
    powerful, biographical, arbitrage, national).

% Jurists, heralds, and court factions who administer and re-assert the exclusion rule at each succession crisis, selectively invoking it where convenient and silent where a female or cognatic claim would serve their faction's interests. They control the interpretive apparatus that decides whether Salic Law 'applies' to this particular territory.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, agnatic_court_factions, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__cognatic_reversion_reading, agnatic_court_factions, beneficiary).

% A direct-line eldest daughter or granddaughter is displaced from a succession that, by cognatic primogeniture or by the territory's own indigenous customary law, would run to her. She has no independent court of appeal outside the very agnatic apparatus that excludes her; her only recourse is war, marriage alliance, or foreign patronage.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, female_dynastic_heirs, payer,
    powerless, biographical, trapped, national).

% Provinces and client territories whose own customary succession law never adopted Salic exclusion find a foreign rule imported wholesale to override local practice, fracturing territorial integrity and inviting partition wars when the direct line is female. Their institutions bear the cost of a jurisdictional import they never consented to.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, cognatic_line_territories, payer,
    moderate, generational, constrained, regional).

% Allied or rival royal houses that would recognize the cognatic claim under their own successoral norms, and would press it diplomatically or militarily, are shut out of the internal adjudication entirely — the exclusion is decided by the excluding faction's own jurists, not by any neutral or shared tribunal.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, foreign_dynastic_courts, excluded,
    institutional, generational, mobile, continental).

% Trace the Lex Salica's sixth-century Frankish tribal origin and its selective post-hoc invocation centuries later in territories (Iberia, parts of Italy, various German principalities) that were never under Frankish successoral custom, documenting where the rule was imported as pretext rather than continuous binding law.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, legal_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__cognatic_reversion_reading, male_collateral_claimants).
narrative_ontology:fixing_cost_class(salic_prohibition__cognatic_reversion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, dispute-minimizing succession rule that avoids costly contested elections or multi-claimant civil wars by fixing eligibility to a single, legible criterion (male agnatic descent) — genuinely useful where a territory's own customary law is itself unsettled or contested.
% TRANSFER_FUNCTION: Moves the crown, its revenues, and its patronage network from the direct female (or cognatic) heir and her line to the nearest qualifying male collateral relative and his faction; moves territorial sovereignty away from a polity's own indigenous succession custom toward an imported Frankish rule asserted without local jurisdictional grounding.
% ABSENT_VOICES: The excluded female heir has no independent tribunal; the territory's own customary-law jurists (where a native cognatic or gender-neutral primogeniture tradition existed) are not consulted on whether Salic Law was ever properly received into that jurisdiction; foreign courts with a competing dynastic-recognition interest are diplomatically present but not admitted to the internal succession adjudication.
% DISAPPEARANCE_RATIONALE: If the exclusion rule were recognized as never having validly applied to the non-Frankish territory in question, the female or cognatic heir's claim would stand, the collateral male line would lose the crown and its patronage apparatus, and territorial integrity would likely be preserved rather than split by a rival agnatic claimant contesting a female-inherited domain.
% FOUNDING_PROBLEM: Sixth-century Frankish tribal law (Lex Salica) addressed inheritance of allodial land within Frankish kin-groups; centuries later it was retrofitted as a dynastic succession doctrine to resolve or preempt succession disputes in territories with no historical connection to Frankish tribal custom.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the beneficiary factions (working from surviving charters, the Pactus Legis Salicae itself, and comparative studies of the territory's own indigenous succession custom) attest that the original Frankish provision concerned private inheritance of land, not crown succession, and that its application to this specific non-Frankish territory has no continuous documented reception — it appears only at the moment a male collateral claimant needed it.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__cognatic_reversion_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.61) reflects that the exclusion transfers a real, valuable good (crown, revenue, patronage) from the legitimate cognatic heir to a collateral male line, but the transfer rides on a genuine coordination good (a bright-line succession rule reduces civil-war risk) — hence tangled_rope rather than pure snare. Suppression (0.58) is moderate-high: the excluded heir's recourse is limited to war or foreign alliance, not legal process, because the adjudicating jurists are drawn from the same faction that benefits. Theater ratio (0.44) is elevated because much of the invocation is performative — the rule is asserted with confident antiquity ('this has always applied here') precisely in territories where its reception is thinnest, which is the diagnostic signature this reading exists to name.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter's seat, the exclusion is presented as continuous, ancient, self-evidently binding constitutional law — a mountain. From the excluded heir's seat, the same structure is a freshly asserted, factionally convenient claim invoked only because no neutral tribunal exists to test its jurisdictional reach. The engine computes this divergence from the structural data (trapped vs. arbitrage exit, powerless vs. powerful) rather than from either party's own characterization.
 *
 * DIRECTIONALITY LOGIC:
 *   Male collateral claimants and the agnatic court factions sit near the full-beneficiary end: they set the interpretive terms and collect the crown. Female dynastic heirs sit near the full-target end: trapped, powerless, with no independent tribunal. Cognatic-line territories are intermediate targets — moderate power, constrained exit, generational time horizon — bearing the cost of an imported jurisdictional claim rather than direct personal displacement. Foreign dynastic courts are excluded rather than coordinated: their absence from adjudication is structural, not incidental.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (resolving Frankish tribal land inheritance) is dead in this territory's context — it never had a documented living function here, only a retrofitted invocation. The status=dead paired with disappearance_verdict=world_rearranges flags exactly the capture/zombie pattern this framework is built to surface: an arrangement whose stated founding problem does not exist in this jurisdiction, yet whose removal would still rearrange concrete arrangements (crown, treasury, territorial boundary) — because what persists is not the original coordination function but the extraction riding on its borrowed legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reception_evidentiary_gap,
    'Was Salic Law ever formally and continuously received as binding successoral law in this specific non-Frankish territory, or was it invoked ad hoc at the moment of a convenient male claim?',
    'Charter and legal-code archival review: search for continuous documented application of Salic exclusion in this territory''s own successoral disputes prior to the contested case, versus a single retroactive invocation coinciding with a male collateral interest.',
    'If no continuous reception is found, this reading''s core claim (never properly binding) is strongly corroborated and the constraint computes closer to snare; if genuine longstanding local reception is found, the constraint shifts toward the sovereign_override or immutable_mandate readings'' territory and this story''s beneficiary/victim structure would need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reception_evidentiary_gap, empirical, 'Whether documentary evidence supports or undermines the claim of non-reception in this jurisdiction.').

omega_variable(
    cognatic_custom_baseline,
    'Did the territory in question possess its own indigenous customary succession law prior to the Salic invocation, and did that custom permit female or cognatic inheritance?',
    'Comparative customary-law analysis of pre-invocation succession practice in the territory (local charters, prior successions, regional legal treatises).',
    'A documented indigenous cognatic custom strengthens the claim that the exclusion is a foreign import rather than a restatement of local law, sharpening the victim status of female_dynastic_heirs and cognatic_line_territories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognatic_custom_baseline, empirical, 'Whether an indigenous gender-neutral or cognatic succession custom predates the Salic invocation.').

omega_variable(
    kernel_framing_choice,
    'Is the choice of the cognatic_reversion_reading over the immutable_mandate_reading or sovereign_override_reading itself a contested political act, or does the historical evidence clearly favor one reading?',
    'Cross-reading comparison: examine which reading''s jurists controlled the court apparatus at the time of the succession dispute, and whether rival dynastic courts abroad recognized a different reading.',
    'If the reading choice tracks factional interest rather than evidentiary weight, all three kernel readings should be treated as live, mutually contesting constraints rather than one being the ''correct'' historical account — reinforcing that this story is one reading among structurally coequal alternatives, not the resolved truth of the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether reading selection tracks evidence or faction interest — the framing-choice omega for this kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__cognatic_reversion_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sali_tr_t8, salic_prohibition__cognatic_reversion_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(sali_tr_t16, salic_prohibition__cognatic_reversion_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(sali_tr_t24, salic_prohibition__cognatic_reversion_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(sali_tr_t32, salic_prohibition__cognatic_reversion_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__cognatic_reversion_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sali_be_t8, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(sali_be_t16, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(sali_be_t24, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(sali_be_t32, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sali_su_t8, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(sali_su_t16, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(sali_su_t24, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(sali_su_t32, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 32, 0.57).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__sovereign_override_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the salic_prohibition kernel. cognatic_reversion_reading treats the rule as a non-binding Frankish anachronism outside its original jurisdiction (this file, tangled_rope: real bright-line coordination value plus asymmetric extraction from displaced female/cognatic heirs). immutable_mandate_reading treats it as irrevocable natural/divine dynastic constitution (a separate file, expected to compute closer to a defended mountain-claim with high resistance and suppression). sovereign_override_reading treats it as ordinary positive law a sovereign may revoke (a separate file, expected to compute with lower suppression and a live legislative-override exit option). Each reading has its own epsilon, its own beneficiary/victim structure, and its own claimed_type; they are linked here, not merged, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__cognatic_reversion_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
