% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__r2p_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: Responsibility to Protect (R2P) Doctrine
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This constraint represents the 'Responsibility to Protect' (R2P) doctrine
 *   as a reading of the tension between UN Charter Article 2(7)
 *   (non-interference) and Chapter VII (UNSC enforcement powers). This
 *   reading asserts that sovereignty is conditional on a state's
 *   responsibility to protect its own population from mass atrocities, and if
 *   it fails, the international community has a responsibility to intervene.
 *   This interpretation significantly expands the grounds for intervention
 *   beyond traditional inter-state aggression, making it a 'tangled rope' due
 *   to its genuine coordination function (protecting populations) intertwined
 *   with substantial extraction (erosion of state sovereignty for target
 *   states) and requiring active enforcement.
 *
 * KEY AGENTS:
 *   - persecuted_populations: Primary beneficiary (powerless/trapped) — bears atrocities, receives protection
 *   - un_security_council: Agenda setter (institutional/constrained) — authorizes intervention, constrained by veto
 *   - target_states_committing_atrocities: Payer (powerful/constrained) — faces intervention, resists loss of sovereignty
 *   - humanitarian_intervention_advocates: Beneficiary (organized/mobile) — champions R2P, gains moral standing
 *   - traditional_sovereignty_advocates: Payer (institutional/identity_locked) — resists erosion of sovereignty norm
 *   - intervening_states: Agenda setter/Payer (institutional/mobile) — commits resources, gains influence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.7).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.6).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility to Protect (R2P) Doctrine").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, 'be2eeb95-c49e-43d4-8ba3-ee6938c4475a').
narrative_ontology:cs_kernel_codification('be2eeb95-c49e-43d4-8ba3-ee6938c4475a', formalized).
narrative_ontology:cs_authority_grounding('be2eeb95-c49e-43d4-8ba3-ee6938c4475a', lineage).
narrative_ontology:cs_interpretation_layer_present('be2eeb95-c49e-43d4-8ba3-ee6938c4475a').
narrative_ontology:cs_reading_relation('be2eeb95-c49e-43d4-8ba3-ee6938c4475a', article_2_7_chapter_vii_tension__sovereignty_first_reading, influences).
narrative_ontology:cs_axiom('be2eeb95-c49e-43d4-8ba3-ee6938c4475a', foundational, sovereignty_is_conditional).
narrative_ontology:cs_axiom_status(sovereignty_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('be2eeb95-c49e-43d4-8ba3-ee6938c4475a', sovereignty_is_conditional, deontological).
narrative_ontology:cs_axiom('be2eeb95-c49e-43d4-8ba3-ee6938c4475a', foundational, international_community_has_responsibility_to_protect).
narrative_ontology:cs_axiom_status(international_community_has_responsibility_to_protect, holdable).
narrative_ontology:cs_axiom_grounding('be2eeb95-c49e-43d4-8ba3-ee6938c4475a', international_community_has_responsibility_to_protect, deontological).
narrative_ontology:cs_reference_frame('be2eeb95-c49e-43d4-8ba3-ee6938c4475a', post_rwanda_never_again_consensus).
narrative_ontology:cs_drift_state('be2eeb95-c49e-43d4-8ba3-ee6938c4475a', post_libya_syria_intervention_fatigue, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('be2eeb95-c49e-43d4-8ba3-ee6938c4475a', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, human_rights_advocates).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_of_target_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, traditional_international_law_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, humanitarian_intervention_advocates).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, target_states_committing_atrocities).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, traditional_sovereignty_advocates).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, intervening_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These populations are the primary intended beneficiaries of R2P, receiving protection from mass atrocities when their own state fails to provide it. Their existence is often under direct threat, with no viable exit options.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations, beneficiary,
    powerless, immediate, trapped, local).

% The UNSC is the primary body authorized to invoke Chapter VII powers to implement R2P, deciding when and how to intervene. Its actions are constrained by the veto power of permanent members and geopolitical considerations.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% These states face potential loss of sovereignty and external intervention if they fail to protect their populations from mass atrocities. They resist intervention, citing Article 2(7) of the UN Charter (non-interference in domestic affairs).
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, target_states_committing_atrocities, payer,
    powerful, biographical, constrained, national).

% These groups and states champion the R2P doctrine, seeing it as a moral imperative and a necessary evolution of international law. They benefit from its legitimization of intervention in cases of mass atrocities.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, humanitarian_intervention_advocates, beneficiary,
    organized, generational, mobile, global).

% These states and scholars prioritize state sovereignty as the bedrock of international order, viewing R2P as an erosion of this principle and a potential pretext for intervention. They bear the cost of the norm's reinterpretation.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, traditional_sovereignty_advocates, payer,
    institutional, civilizational, identity_locked, global).

% States willing and able to commit military and diplomatic resources to R2P interventions. They bear the costs of intervention (financial, human, political) but also gain influence and moral standing.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, intervening_states, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, intervening_states, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate international action to prevent and respond to mass atrocities (genocide, war crimes, ethnic cleansing, crimes against humanity) when individual states fail to protect their own populations.
% TRANSFER_FUNCTION: Transfers the responsibility for population protection from a sovereign state (when it fails) to the international community, potentially involving military intervention, and transfers political capital and resources from intervening states to affected populations.
% ABSENT_VOICES: Populations in states that fear R2P could be used as a pretext for intervention, particularly those with historical grievances against powerful states. They would argue for stronger safeguards against abuse and for non-military solutions.
% DISAPPEARANCE_RATIONALE: If R2P vanished, the international community would lose a key (albeit imperfect) framework for responding to mass atrocities. Persecuted populations would have fewer avenues for protection, and the debate over humanitarian intervention would revert to a more traditional, sovereignty-first paradigm, likely leading to more inaction in the face of atrocities.
% FOUNDING_PROBLEM: The failure of the international community to prevent or respond effectively to genocides and mass atrocities in Rwanda and Bosnia in the 1990s, despite the 'never again' promise after the Holocaust.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing occurrence of mass atrocities globally, and the continued debate over intervention in places like Syria and Myanmar, corroborate that the founding problem remains live. Human rights organizations, international legal bodies, and academic scholars outside the immediate beneficiaries consistently attest to this.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high because R2P imposes a significant cost on target states by potentially overriding their sovereignty. Suppression (0.6) is moderate, reflecting the active diplomatic and military efforts required to overcome resistance to intervention. Theater ratio (0.4) indicates that while R2P has a genuine protective function, its application is often performative or selectively applied due to geopolitical interests. Accessibility collapse (0.3) is low because alternative interpretations (sovereignty-first) and resistance to intervention remain strong. Resistance (0.75) is high, reflecting the strong opposition from states prioritizing traditional sovereignty and those fearing intervention.
 *
 * PERSPECTIVAL GAP:
 *   Persecuted populations experience R2P as a lifeline, a potential 'rope' offering protection. Intervening states may see it as a 'rope' for coordinating moral action, albeit with costs. However, target states and traditional sovereignty advocates experience it as a 'snare' or 'tangled rope,' extracting sovereignty and imposing external will. The UNSC's perspective is complex, balancing the 'rope' of collective security with the 'snare' of geopolitical constraints and the 'tangled rope' of balancing competing norms.
 *
 * DIRECTIONALITY LOGIC:
 *   Persecuted populations are full beneficiaries (d=0.0) as R2P is designed to protect them. The UNSC and intervening states are closer to symmetric (d=0.5) as they bear costs but also gain influence/legitimacy. Target states and traditional sovereignty advocates are targets (d=1.0) as they bear the direct costs of intervention or the erosion of a foundational norm. Humanitarian intervention advocates are beneficiaries (d=0.0) as their normative agenda is advanced.
 *
 * MANDATROPHY ANALYSIS:
 *   R2P is not mandatrophic; its founding problem (mass atrocities) remains live. The classification as 'tangled_rope' prevents mislabeling it as a 'snare' (ignoring its genuine protective function) or a 'rope' (ignoring its coercive, sovereignty-eroding aspects). The tension between protection and sovereignty is inherent to its structure, not a sign of atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    r2p_abuse_potential,
    'To what extent is R2P vulnerable to abuse as a pretext for interventions driven by national interest rather than humanitarian concern?',
    'Empirical analysis of past R2P-invoked interventions, examining the correlation between stated humanitarian goals and actual geopolitical outcomes, and the consistency of application across similar atrocity situations.',
    'If abuse is widespread, the effective extractiveness for target states is higher, and the constraint leans more towards a ''snare'' for them, as the coordination narrative becomes a cover for power projection. If abuse is rare, its ''tangled rope'' nature is more stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(r2p_abuse_potential, empirical, 'Assessing the risk of R2P being used as a pretext for non-humanitarian interventions.').

omega_variable(
    sovereignty_redefinition_ambiguity,
    'Is R2P a redefinition of sovereignty (from absolute to conditional) or an exception to it?',
    'Conceptual analysis of international legal scholarship and state practice over time: if states consistently articulate sovereignty as inherently conditional, it''s a redefinition; if they treat R2P as a regrettable but necessary exception, it''s an exception.',
    'If a redefinition, the ''extraction'' from target states is less a violation and more a consequence of a new, accepted norm, potentially lowering the perceived extractiveness over time. If an exception, the tension remains high, and the ''tangled rope'' classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_redefinition_ambiguity, conceptual, 'Whether R2P fundamentally redefines or merely creates an exception to state sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 2005, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(arti_tr_t2010, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(arti_tr_t2015, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2015, 0.45).
narrative_ontology:measurement(arti_tr_t2020, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(arti_tr_t2024, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t2005, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(arti_be_t2010, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(arti_be_t2015, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(arti_be_t2020, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(arti_be_t2024, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2005, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(arti_su_t2010, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(arti_su_t2015, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(arti_su_t2020, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(arti_su_t2024, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_first_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'Responsibility to Protect' (R2P) reading of the kernel 'Article 2(7) / Chapter VII Tension'. It is linked to the 'sovereignty_first_reading' as a sibling interpretation of the same foundational tension in international law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
