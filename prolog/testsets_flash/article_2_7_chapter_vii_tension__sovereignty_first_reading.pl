% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__sovereignty_first_reading, []).

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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: Sovereignty-First Reading of UN Article 2(7) and Chapter VII
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty-first' reading of UN Article
 *   2(7) and Chapter VII, which prioritizes state sovereignty and
 *   non-intervention in internal affairs, limiting international intervention
 *   to cases of explicit state consent or Chapter VII authorization for
 *   inter-state aggression. It is a reading of the
 *   'article_2_7_chapter_vii_tension' kernel, contrasting with the
 *   'r2p_reading' which posits conditional sovereignty. This reading results
 *   in high extraction from populations under domestic atrocity, as it
 *   effectively shields states from accountability for internal human rights
 *   abuses.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.85).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.75).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "Sovereignty-First Reading of UN Article 2(7) and Chapter VII").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'c70961b8-8838-4589-baf6-ce62885a21b6').
narrative_ontology:cs_kernel_codification('c70961b8-8838-4589-baf6-ce62885a21b6', fixed_text).
narrative_ontology:cs_authority_grounding('c70961b8-8838-4589-baf6-ce62885a21b6', lineage).
narrative_ontology:cs_interpretation_layer_present('c70961b8-8838-4589-baf6-ce62885a21b6').
narrative_ontology:cs_reading_relation('c70961b8-8838-4589-baf6-ce62885a21b6', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('c70961b8-8838-4589-baf6-ce62885a21b6', foundational, state_sovereignty_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('c70961b8-8838-4589-baf6-ce62885a21b6', state_sovereignty_absolute, deontological).
narrative_ontology:cs_axiom('c70961b8-8838-4589-baf6-ce62885a21b6', foundational, non_intervention_unconditional).
narrative_ontology:cs_axiom_status(non_intervention_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('c70961b8-8838-4589-baf6-ce62885a21b6', non_intervention_unconditional, conventional).
narrative_ontology:cs_reference_frame('c70961b8-8838-4589-baf6-ce62885a21b6', westphalian_state_system).
narrative_ontology:cs_drift_state('c70961b8-8838-4589-baf6-ce62885a21b6', contemporary_human_rights_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('c70961b8-8838-4589-baf6-ce62885a21b6', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_states).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the strong non-intervention principle, which shields their internal affairs from external scrutiny or intervention, even in cases of severe human rights abuses. They actively champion this reading in international forums.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_states, beneficiary,
    institutional, generational, mobile, global).

% Strongly advocate for this reading as a defense against neo-colonial interventions and a guarantee of their hard-won independence. They see any erosion of absolute sovereignty as a threat to their national self-determination.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states, beneficiary,
    organized, generational, mobile, global).

% Bear the direct and severe costs of this constraint, as it prevents external intervention that could protect them from mass atrocities committed by their own governments. Their suffering is often invisible or dismissed under the guise of internal affairs.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity, payer,
    powerless, immediate, trapped, local).

% Work to highlight abuses and push for intervention, but are consistently frustrated by the legal and political barriers erected by this sovereignty-first interpretation. They bear the cost of inaction and the moral burden of unaddressed suffering.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, human_rights_advocates, payer,
    moderate, generational, constrained, global).

% Is the primary body authorized to sanction interventions under Chapter VII, but its actions are constrained by the veto power of permanent members, many of whom align with the sovereignty-first reading. It administers the constraint by selectively authorizing interventions.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Often find themselves in a dilemma, balancing their stated commitment to human rights with the legal and political implications of violating state sovereignty. They are often the ones pushing for a more expansive interpretation of Chapter VII but are constrained by the existing framework.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, western_liberal_democracies, observer,
    institutional, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear legal framework for non-intervention in the internal affairs of states, aiming to prevent unilateral military actions and maintain international order based on state equality and territorial integrity.
% TRANSFER_FUNCTION: Transfers the absolute right to manage internal affairs, including the treatment of populations, to the sovereign state, effectively transferring the burden of domestic atrocities onto the affected populations and away from international accountability.
% ABSENT_VOICES: The direct victims of domestic atrocities are largely absent from the international legal and political discourse that upholds this reading. Their voices are mediated through NGOs and advocacy groups, which lack direct agency in the UN system.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the international legal landscape would fundamentally shift. The threshold for intervention would lower, potentially leading to more frequent and less constrained interventions, fundamentally altering state behavior and international relations. The UN Security Council's role would be drastically reconfigured.
% FOUNDING_PROBLEM: The UN Charter was established in the aftermath of World War II to prevent future inter-state aggression and maintain international peace and security, emphasizing state sovereignty to avoid a return to colonial interventions and power politics.
% FOUNDING_PROBLEM_CORROBORATION: The problem of inter-state aggression remains live, as evidenced by ongoing conflicts. Post-colonial states and many developing nations consistently corroborate the need for strong sovereignty protections to prevent external interference. However, human rights organizations and some Western states contest whether this reading adequately addresses intra-state atrocities, arguing it prioritizes state stability over human lives.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading allows states to commit atrocities against their own populations with minimal risk of external intervention, effectively extracting the right to life and security from these populations. Suppression (0.75) is also high, as it actively suppresses any legal or political avenues for intervention, relying on the UN Charter's non-intervention principle. The theater ratio is low (0.15) because the principle is genuinely applied, even if its consequences are severe for some populations; it's not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiary states, this is a foundational principle of international law, a 'rope' ensuring stability and self-determination. From the perspective of victim populations, it is a 'snare' that traps them under oppressive regimes. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian and post-colonial states are clear beneficiaries (d=0.0-0.2), as this reading grants them broad immunity from external interference. Populations under domestic atrocity and human rights advocates are the primary targets (d=0.8-1.0), bearing the costs of non-intervention. The UN Security Council acts as an agenda-setter, but its actions are heavily influenced by the beneficiaries of this reading, leading to constrained intervention.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights_priority,
    'Is the principle of state sovereignty an absolute, unconditional right, or is it conditional on a state''s adherence to fundamental human rights obligations?',
    'Evolution of customary international law through state practice and opinio juris, or a UN Charter amendment explicitly clarifying the relationship.',
    'If sovereignty is deemed conditional, this reading''s extractiveness would be significantly reduced, and its classification would shift towards a ''tangled_rope'' or even ''rope'' for states that uphold human rights, but a ''snare'' for those that do not. If absolute, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_priority, conceptual, 'The fundamental conceptual tension between state sovereignty and human rights.').

omega_variable(
    intervention_trigger_ambiguity,
    'What constitutes ''inter-state aggression'' versus ''internal affairs'' when domestic conflicts have regional spillover effects or involve cross-border non-state actors?',
    'Clearer legal definitions and precedents from the International Court of Justice or a more consistent and less politicized application of Chapter VII by the UN Security Council.',
    'A clearer definition could either expand or contract the scope for legitimate intervention, directly impacting the extractiveness from affected populations and the suppression of state actions. Ambiguity allows for selective application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_trigger_ambiguity, empirical, 'Ambiguity in defining triggers for Chapter VII intervention.').

omega_variable(
    r2p_reading_impact,
    'How would the structural classification of this ''sovereignty_first_reading'' change if the ''r2p_reading'' gained widespread acceptance and legal force?',
    'Observation of state behavior, UN Security Council resolutions, and international legal scholarship following a hypothetical shift in the dominant interpretation.',
    'If the ''r2p_reading'' became dominant, this ''sovereignty_first_reading'' would likely be reclassified as a ''piton'' or ''snare'' for states committing atrocities, as its protective function for such states would atrophy or be actively challenged. Its extractiveness would be seen as illegitimate rather than a necessary cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(r2p_reading_impact, conceptual, 'Impact of a competing reading on this constraint''s classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(arti_tr_t1970, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(arti_tr_t1995, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1995, 0.14).
narrative_ontology:measurement(arti_tr_t2024, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(arti_be_t1970, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1970, 0.75).
narrative_ontology:measurement(arti_be_t1995, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1995, 0.8).
narrative_ontology:measurement(arti_be_t2024, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(arti_su_t1970, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(arti_su_t1995, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(arti_su_t2024, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.1).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, r2p_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'article_2_7_chapter_vii_tension' kernel. It emphasizes state sovereignty as foundational, contrasting with the 'r2p_reading' which posits conditional sovereignty. Both are distinct constraints arising from the same contested legal text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
