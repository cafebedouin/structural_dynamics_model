% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__international_oversight_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: article_17_complementarity__international_oversight_reading
 *   human_readable: ICC Article 17 Complementarity (International Oversight Reading)
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'international oversight' reading of
 *   Article 17 complementarity in the Rome Statute, which establishes the
 *   jurisdiction of the International Criminal Court (ICC). In this reading,
 *   complementarity functions as an accountability-trigger mechanism,
 *   allowing the ICC to intervene when national courts are 'unwilling or
 *   unable' to genuinely prosecute grave international crimes. The
 *   interpretation of 'unwilling or unable' is broad, designed to capture
 *   scenarios of 'victor's justice,' elite immunity, or sham prosecutions,
 *   positioning the ICC as a guardian against impunity when states fail. This
 *   reading emphasizes the ICC's proactive role in ensuring justice, even at
 *   the expense of state sovereignty.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.75).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.8).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "ICC Article 17 Complementarity (International Oversight Reading)").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, '17af1274-6cb3-48e9-b26a-d1a30a5bb377').
narrative_ontology:cs_kernel_codification('17af1274-6cb3-48e9-b26a-d1a30a5bb377', fixed_text).
narrative_ontology:cs_authority_grounding('17af1274-6cb3-48e9-b26a-d1a30a5bb377', lineage).
narrative_ontology:cs_interpretation_layer_present('17af1274-6cb3-48e9-b26a-d1a30a5bb377').
narrative_ontology:cs_reading_relation('17af1274-6cb3-48e9-b26a-d1a30a5bb377', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('17af1274-6cb3-48e9-b26a-d1a30a5bb377', foundational, icc_as_guardian_against_impunity).
narrative_ontology:cs_axiom_status(icc_as_guardian_against_impunity, holdable).
narrative_ontology:cs_axiom_grounding('17af1274-6cb3-48e9-b26a-d1a30a5bb377', icc_as_guardian_against_impunity, deontological).
narrative_ontology:cs_axiom('17af1274-6cb3-48e9-b26a-d1a30a5bb377', foundational, broad_interpretation_of_unwilling_unable).
narrative_ontology:cs_axiom_status(broad_interpretation_of_unwilling_unable, holdable).
narrative_ontology:cs_axiom_grounding('17af1274-6cb3-48e9-b26a-d1a30a5bb377', broad_interpretation_of_unwilling_unable, conventional).
narrative_ontology:cs_reference_frame('17af1274-6cb3-48e9-b26a-d1a30a5bb377', rome_statute_founding_principles).
narrative_ontology:cs_drift_state('17af1274-6cb3-48e9-b26a-d1a30a5bb377', contemporary_icc_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('17af1274-6cb3-48e9-b26a-d1a30a5bb377', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_of_atrocities).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, international_justice_advocates).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, complicit_states).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, elites_seeking_impunity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, national_judiciaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 17 broadly to assert jurisdiction when states fail to genuinely prosecute grave international crimes, acting as a guardian against impunity. It sets the standards for 'unwilling or unable'.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, icc, agenda_setter,
    institutional, generational, analytical, global).

% Receive a pathway to justice and accountability when their national systems are unwilling or unable to prosecute grave crimes, preventing their cases from being permanently shelved.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_of_atrocities, beneficiary,
    powerless, biographical, constrained, global).

% Promote the ICC's assertive role as a backstop against impunity, supporting a broad interpretation of complementarity to ensure accountability for grave crimes worldwide.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, international_justice_advocates, beneficiary,
    organized, generational, mobile, global).

% Face the prospect of ICC intervention and loss of jurisdiction when they fail to genuinely prosecute grave crimes, especially those involving state actors or political elites. This extracts their sovereign right to exclusive jurisdiction.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, complicit_states, payer,
    institutional, generational, constrained, national).

% Are targeted by the ICC's jurisdiction when their national systems fail to protect them from prosecution for grave crimes, losing their de facto immunity.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, elites_seeking_impunity, payer,
    powerful, biographical, trapped, national).

% Must demonstrate genuine willingness and ability to prosecute grave crimes to avoid ICC intervention, facing scrutiny over their independence, impartiality, and effectiveness. This imposes a higher standard of accountability.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, national_judiciaries, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__international_oversight_reading, national_judiciaries, agenda_setter).

% Reject any international jurisdiction over national matters, viewing ICC intervention as an infringement on state sovereignty. Their arguments are often sidelined in favor of the ICC's mandate to end impunity.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, sovereignty_absolutists, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to ensure accountability for grave international crimes when national systems are unwilling or unable to genuinely prosecute, establishing a global backstop against impunity.
% TRANSFER_FUNCTION: Transfers jurisdiction and legitimacy for prosecuting international crimes from failing national systems to the ICC, and transfers the burden of genuine accountability to states, extracting their ability to grant impunity.
% ABSENT_VOICES: States that assert absolute sovereignty and reject international jurisdiction, as well as political and military elites who benefit from impunity, are structurally excluded from the decision-making process regarding ICC intervention.
% DISAPPEARANCE_RATIONALE: If the ICC's complementary jurisdiction vanished overnight, many grave international crimes would go unpunished, leading to a resurgence of impunity, cycles of violence, and a significant erosion of international criminal justice norms. The global landscape of accountability would fundamentally reorganize.
% FOUNDING_PROBLEM: The historical failure of states to prosecute grave international crimes (genocide, crimes against humanity, war crimes), leading to widespread impunity for perpetrators and a lack of justice for victims.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, UN reports, and victim groups consistently document ongoing impunity for grave crimes in various states, corroborating the live status of the founding problem. Independent legal scholars also attest to the continuing need for an international backstop.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the significant transfer of sovereign authority from states to the ICC, particularly for states that would prefer to shield their elites or conduct superficial proceedings. Suppression (0.80) is high because this reading actively suppresses state attempts to avoid genuine accountability, requiring states to meet a high bar for 'willingness and ability.' The moderate theater ratio (0.40) acknowledges that states may still attempt performative prosecutions to deter ICC intervention, but the broad interpretation of 'unwilling or unable' aims to see through such attempts. Resistance (0.85) is very high due to the inherent tension with state sovereignty and the political costs of ICC intervention.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ICC and victims, this reading is a necessary mechanism for justice, ensuring that grave crimes do not go unpunished. From the perspective of states asserting strong sovereignty, it is an overreach that infringes on national prerogatives. The engine's classification will highlight this divergence, showing the ICC as a coordinator of justice and states as targets of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC, victims of atrocities, and international justice advocates are beneficiaries, as this reading empowers the ICC and provides a path to justice. Complicit states, elites seeking impunity, and national judiciaries are payers, as they bear the costs of potential ICC intervention, loss of jurisdiction, and increased scrutiny. Sovereignty absolutists are excluded, as their fundamental objections to international jurisdiction are overridden by the imperative to end impunity.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of complementarity actively prevents mandatrophy by ensuring the ICC's mandate to end impunity remains live and effective. It counters attempts by states to render the 'unwilling or unable' clause inert through narrow interpretations or sham proceedings, thus preventing the constraint from atrophying into a mere theatrical performance of state sovereignty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unwilling_unable_interpretation_ambiguity,
    'How broadly should ''unwilling or unable'' be interpreted to effectively balance state sovereignty and the imperative to end impunity?',
    'Consistent jurisprudence from the ICC''s Appeals Chamber, or a consensus among state parties on interpretive guidelines that clarify the threshold for intervention.',
    'A narrower interpretation would shift the constraint towards the national_primacy_reading, reducing ICC intervention and effective extraction from states; a broader interpretation (as in this reading) increases ICC intervention and effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unwilling_unable_interpretation_ambiguity, conceptual, 'Ambiguity in the interpretation of the ''unwilling or unable'' clause.').

omega_variable(
    genuine_prosecution_assessment_challenge,
    'How reliably can the ICC assess the genuineness and independence of national proceedings, especially when states engage in ''victor''s justice'' or ''elite immunity'' sham trials?',
    'Development of robust, objective criteria and investigative methodologies for evaluating national proceedings, coupled with independent monitoring mechanisms and transparent reporting.',
    'If assessment is unreliable, the constraint''s suppression of impunity is undermined, and its claimed coordination function becomes theatrical; if reliable, the constraint effectively targets sham proceedings and ensures genuine accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_prosecution_assessment_challenge, empirical, 'Challenges in assessing the genuineness of national prosecutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1998, article_17_complementarity__international_oversight_reading, theater_ratio, 1998, 0.2).
narrative_ontology:measurement(arti_tr_t2004, article_17_complementarity__international_oversight_reading, theater_ratio, 2004, 0.25).
narrative_ontology:measurement(arti_tr_t2010, article_17_complementarity__international_oversight_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(arti_tr_t2016, article_17_complementarity__international_oversight_reading, theater_ratio, 2016, 0.35).
narrative_ontology:measurement(arti_tr_t2024, article_17_complementarity__international_oversight_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t1998, article_17_complementarity__international_oversight_reading, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(arti_be_t2004, article_17_complementarity__international_oversight_reading, base_extractiveness, 2004, 0.6).
narrative_ontology:measurement(arti_be_t2010, article_17_complementarity__international_oversight_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(arti_be_t2016, article_17_complementarity__international_oversight_reading, base_extractiveness, 2016, 0.7).
narrative_ontology:measurement(arti_be_t2024, article_17_complementarity__international_oversight_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1998, article_17_complementarity__international_oversight_reading, suppression_requirement, 1998, 0.6).
narrative_ontology:measurement(arti_su_t2004, article_17_complementarity__international_oversight_reading, suppression_requirement, 2004, 0.68).
narrative_ontology:measurement(arti_su_t2010, article_17_complementarity__international_oversight_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(arti_su_t2016, article_17_complementarity__international_oversight_reading, suppression_requirement, 2016, 0.78).
narrative_ontology:measurement(arti_su_t2024, article_17_complementarity__international_oversight_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
