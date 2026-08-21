% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Jurisdiction: Hybrid Complementarity Reading
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This constraint story models the 'hybrid complementarity' reading of the
 *   Rome Statute's jurisdiction, which posits that the International Criminal
 *   Court (ICC) possesses residual universal authority to prosecute grave
 *   international crimes, but its operational jurisdiction is primarily
 *   constrained by the principle of complementarity. This means the ICC
 *   defers to national jurisdictions that are genuinely willing and able to
 *   investigate and prosecute such crimes. Jurisdiction exists, but
 *   enforcement often depends on state cooperation, reflecting an authority
 *   grounded in a hybrid of natural law aspiration for justice and
 *   treaty-based sovereign consent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.6).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.5).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Jurisdiction: Hybrid Complementarity Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, 'bb40de41-7312-4db8-8e4a-0393dd3c379c').
narrative_ontology:cs_kernel_codification('bb40de41-7312-4db8-8e4a-0393dd3c379c', fixed_text).
narrative_ontology:cs_authority_grounding('bb40de41-7312-4db8-8e4a-0393dd3c379c', lineage).
narrative_ontology:cs_interpretation_layer_present('bb40de41-7312-4db8-8e4a-0393dd3c379c').
narrative_ontology:cs_reading_relation('bb40de41-7312-4db8-8e4a-0393dd3c379c', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb40de41-7312-4db8-8e4a-0393dd3c379c', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_axiom('bb40de41-7312-4db8-8e4a-0393dd3c379c', foundational, complementarity_as_primary_jurisdiction_filter).
narrative_ontology:cs_axiom_status(complementarity_as_primary_jurisdiction_filter, holdable).
narrative_ontology:cs_axiom_grounding('bb40de41-7312-4db8-8e4a-0393dd3c379c', complementarity_as_primary_jurisdiction_filter, conventional).
narrative_ontology:cs_axiom('bb40de41-7312-4db8-8e4a-0393dd3c379c', foundational, icc_as_court_of_last_resort).
narrative_ontology:cs_axiom_status(icc_as_court_of_last_resort, holdable).
narrative_ontology:cs_axiom_grounding('bb40de41-7312-4db8-8e4a-0393dd3c379c', icc_as_court_of_last_resort, deontological).
narrative_ontology:cs_reference_frame('bb40de41-7312-4db8-8e4a-0393dd3c379c', balanced_sovereignty_and_justice).
narrative_ontology:cs_drift_state('bb40de41-7312-4db8-8e4a-0393dd3c379c', contemporary_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('bb40de41-7312-4db8-8e4a-0393dd3c379c', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, international_justice_advocates).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_of_atrocities).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, sovereign_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, signatory_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Rome Statute, interprets the complementarity principle, and issues warrants for grave international crimes. Its authority is derived from the treaty and its member states, but it acts as an independent judicial body.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, icc, agenda_setter,
    institutional, generational, analytical, global).

% Have ratified the Rome Statute, accepting ICC jurisdiction under the principle of complementarity. They bear the costs of potential ICC intervention, cooperation with investigations, and domestic legal reforms to align with international criminal law. They can avoid ICC jurisdiction by genuinely prosecuting crimes themselves.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, signatory_states, payer,
    institutional, generational, constrained, national).

% Have not ratified the Rome Statute and generally do not recognize ICC jurisdiction. They actively resist any attempts to assert ICC authority over their nationals or territory, viewing it as an infringement on their sovereignty.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, non_signatory_states, excluded,
    institutional, generational, mobile, national).

% Seek justice for grave crimes when national systems are unwilling or unable to provide it. They benefit from the ICC's existence as a court of last resort, offering a path to accountability and redress.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_of_atrocities, beneficiary,
    powerless, biographical, trapped, global).

% Promote the ICC's mandate, support its investigations, and advocate for its universal ratification. They benefit from the ICC's role in advancing international criminal justice and combating impunity.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_justice_advocates, beneficiary,
    organized, biographical, analytical, global).

% Face potential investigation, arrest, and prosecution by the ICC for grave international crimes. They bear the direct costs of legal defense and potential imprisonment, often with limited recourse once an ICC warrant is issued.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_individuals, payer,
    powerless, immediate, trapped, global).

% Monitor human rights situations, document alleged crimes, and provide information to the ICC. They play a critical role in informing the court's decisions and advocating for its effective functioning.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, human_rights_organizations, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to prosecute perpetrators of genocide, war crimes, and crimes against humanity, ensuring accountability when national systems are unwilling or unable to do so, thereby preventing impunity.
% TRANSFER_FUNCTION: Transfers the authority to prosecute grave international crimes from national jurisdictions (when they fail) to the ICC, imposing costs on states (cooperation, potential surrender) and individuals (prosecution), while providing a mechanism for justice to victims.
% ABSENT_VOICES: States that refuse to ratify the Rome Statute or withdraw from it, arguing for absolute sovereign immunity. They would object to any perceived overreach of ICC jurisdiction and advocate for purely national control over criminal justice.
% DISAPPEARANCE_RATIONALE: If the ICC and its complementarity mechanism vanished overnight, the international legal landscape for prosecuting grave crimes would revert to ad hoc tribunals or purely national efforts, leading to greater impunity, a significant gap in international justice, and a loss of a crucial deterrent against mass atrocities.
% FOUNDING_PROBLEM: The widespread failure of national jurisdictions to prosecute perpetrators of genocide, war crimes, and crimes against humanity, leading to pervasive impunity and a lack of deterrence for such grave crimes.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, UN bodies, and independent legal scholars consistently document ongoing instances of impunity for grave crimes across the globe, corroborating the continued relevance and necessity of the ICC's mandate as a court of last resort.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.6) because while the ICC can assert jurisdiction, the complementarity principle means it's not always actively extracting from states. However, the potential for intervention and the obligation to cooperate represent a significant cost to state sovereignty. Suppression is moderate (0.5) as the ICC lacks its own enforcement mechanisms and relies on states for arrests and evidence, but its warrants carry international legal weight. Theater ratio is low (0.2) because the complementarity principle is a genuinely active and debated mechanism, not mere performance. Accessibility collapse is moderate (0.4) as states have a clear alternative: genuinely prosecuting crimes themselves.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ICC and justice advocates, the hybrid complementarity mechanism is a necessary and legitimate balance to ensure accountability. From the perspective of some sovereign states, particularly those facing potential ICC scrutiny, it can be seen as an infringement on national sovereignty, even with the complementarity safeguard. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC acts as the agenda-setter, interpreting and applying the Statute. Victims of atrocities and international justice advocates are beneficiaries, as the ICC provides a mechanism for accountability. Sovereign states (especially signatory ones) are payers, bearing the costs of potential ICC intervention and cooperation. Accused individuals are also payers, facing direct prosecution. Non-signatory states are excluded, as they do not recognize the ICC's authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The complementarity mechanism is actively debated, applied, and challenged in ongoing cases, demonstrating that its mandate is very much live. There is no evidence of mandatrophy; its function as a balance between international justice and state sovereignty is continuously tested and refined, not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_effectiveness_ambiguity,
    'How effectively does the complementarity principle prevent ICC overreach while genuinely ensuring accountability for grave crimes?',
    'Empirical analysis of ICC case outcomes, national prosecutions triggered by ICC pressure, and independent assessments of state willingness and ability to prosecute.',
    'If complementarity is found to be consistently ineffective at ensuring accountability, the constraint leans towards a more universalist (and potentially more extractive) interpretation. If it''s found to be overly deferential, it leans towards a more sovereigntist (and less extractive) interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_effectiveness_ambiguity, empirical, 'The practical balance between deference and accountability in complementarity.').

omega_variable(
    state_cooperation_impact_ambiguity,
    'To what extent does state non-cooperation (e.g., refusal to arrest, provide evidence) undermine the ICC''s effective jurisdiction and enforcement capacity?',
    'Analysis of arrest warrant execution rates, evidence collection challenges, and the impact of non-cooperation on trial proceedings and outcomes.',
    'High levels of non-cooperation would indicate that the ICC''s effective suppression and extractiveness are lower than its formal powers suggest, potentially shifting the classification towards a more ''piton-like'' state where formal authority lacks real-world bite.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_cooperation_impact_ambiguity, empirical, 'Impact of state non-cooperation on ICC''s effective power.').

omega_variable(
    reading_framing_ambiguity,
    'Is this ''hybrid complementarity'' reading a genuine structural balance, or is it a conceptual framing that masks a de facto leaning towards either universalism or sovereigntism in practice?',
    'Longitudinal analysis of ICC jurisprudence and state practice, comparing the outcomes against the core tenets of the universalist and sovereigntist readings. If outcomes consistently align with one sibling reading, the ''hybrid'' framing may be conceptually overridden.',
    'If the reading is found to consistently lean towards universalism, the effective extractiveness and suppression would be higher. If it leans towards sovereigntism, these metrics would be lower, and the constraint would be less coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Whether the ''hybrid'' framing accurately reflects the practical balance of power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 1998, 0.15).
narrative_ontology:measurement(rome_tr_t2004, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2004, 0.17).
narrative_ontology:measurement(rome_tr_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(rome_tr_t2016, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 1998, 0.5).
narrative_ontology:measurement(rome_be_t2004, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2004, 0.53).
narrative_ontology:measurement(rome_be_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement(rome_be_t2016, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2016, 0.58).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 1998, 0.4).
narrative_ontology:measurement(rome_su_t2004, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2004, 0.43).
narrative_ontology:measurement(rome_su_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2010, 0.46).
narrative_ontology:measurement(rome_su_t2016, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2016, 0.48).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2024, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
