% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__thinkability_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: IP Category Emergence: Thinkability Reading
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the conceptual emergence of 'intellectual
 *   property' as a legally coherent category, specifically tied to the
 *   Statute of Anne in 1710. Prior to this, disputes over copying were framed
 *   in terms of guild privileges or censorship, not an author's inherent
 *   right to their expression. The constraint is a Mountain because it
 *   describes a historical conceptual shift that, once it occurred, became an
 *   unchangeable fact of legal history. Its 'extractiveness' is minimal,
 *   reflecting the conceptual cost of adopting a new legal framework, not an
 *   active extraction from parties.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.05).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.02).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, mountain).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "IP Category Emergence: Thinkability Reading").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:emerges_naturally(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, '57002aa1-adce-4c57-a8bf-5f476d0cf75b').
narrative_ontology:cs_kernel_codification('57002aa1-adce-4c57-a8bf-5f476d0cf75b', fixed_text).
narrative_ontology:cs_authority_grounding('57002aa1-adce-4c57-a8bf-5f476d0cf75b', lineage).
narrative_ontology:cs_interpretation_layer_present('57002aa1-adce-4c57-a8bf-5f476d0cf75b').
narrative_ontology:cs_reading_relation('57002aa1-adce-4c57-a8bf-5f476d0cf75b', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_reading_relation('57002aa1-adce-4c57-a8bf-5f476d0cf75b', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('57002aa1-adce-4c57-a8bf-5f476d0cf75b', foundational, legal_concepts_are_historically_contingent).
narrative_ontology:cs_axiom_status(legal_concepts_are_historically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('57002aa1-adce-4c57-a8bf-5f476d0cf75b', legal_concepts_are_historically_contingent, conventional).
narrative_ontology:cs_axiom('57002aa1-adce-4c57-a8bf-5f476d0cf75b', foundational, statute_of_anne_created_new_legal_category).
narrative_ontology:cs_axiom_status(statute_of_anne_created_new_legal_category, holdable).
narrative_ontology:cs_axiom_grounding('57002aa1-adce-4c57-a8bf-5f476d0cf75b', statute_of_anne_created_new_legal_category, conventional).
narrative_ontology:cs_reference_frame('57002aa1-adce-4c57-a8bf-5f476d0cf75b', post_statute_of_anne_conceptual_space).
narrative_ontology:cs_drift_state('57002aa1-adce-4c57-a8bf-5f476d0cf75b', contemporary_legal_philosophy, gap(stable, minor, true)).
narrative_ontology:cs_created_at('57002aa1-adce-4c57-a8bf-5f476d0cf75b', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, conceptual_history_of_law).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, legal_positivism_as_social_fact).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Analyze the historical development of legal concepts, identifying shifts in legal coherence and the emergence of new categories of 'ownable' rights. Their work is to describe the conceptual landscape.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_historians, observer,
    analytical, generational, analytical, global).

% Examine the philosophical and historical foundations of intellectual property, often debating whether IP rights are 'natural' or socially constructed. This reading informs their understanding of IP's origins.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, intellectual_property_scholars, observer,
    analytical, generational, analytical, global).

% Operated within a system of guild privileges and censorship, where 'rights' to copy were held by printers or granted by royal decree, not by authors as creators of original expression. They lacked the conceptual framework to claim 'intellectual property' as it is understood post-1710.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, pre_1710_authors_and_printers, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This constraint describes the conceptual coordination that allowed for a shared understanding of 'ownable expression' to emerge, enabling subsequent legal and economic coordination around intellectual property.
% TRANSFER_FUNCTION: It marks a conceptual shift, not a direct transfer of goods. It enabled the future transfer of economic value from users of expression to its creators/owners.
% ABSENT_VOICES: Pre-1710 authors and printers, if they could articulate their situation with post-1710 vocabulary, would highlight the absence of a coherent legal category for 'ownable expression' and the limitations imposed by guild systems. Their 'voice' is the historical record of their inability to make such claims.
% DISAPPEARANCE_RATIONALE: The historical fact of a conceptual shift in legal thought cannot 'disappear'. If this reading vanished, the historical record of the Statute of Anne and its conceptual impact would remain, but the interpretation of it as a 'category emergence' would be lost, potentially leading to different historical narratives of IP's origins.
% FOUNDING_PROBLEM: The problem was the lack of a coherent legal framework to recognize and protect authors' rights to their original works, leading to disputes over copying and control that could not be resolved by existing guild or censorship privileges.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians widely corroborate that the pre-1710 legal landscape lacked the conceptual tools for modern IP. The problem of 'thinkability' was resolved by the Statute of Anne, which introduced the concept of 'copy right' for authors. This is attested by analysis of legal texts and historical commentary from academic observers, not by parties who directly benefited from the Statute itself.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_unchanged).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__thinkability_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, ExtMetricName, E),
    domain_priors:suppression_score(ip_category_emergence__thinkability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ip_category_emergence__thinkability_reading),
    narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ip_category_emergence__thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness, suppression, and theater ratio reflect that this constraint is about a conceptual shift in legal thought, not an active mechanism of extraction or coercion. The 'thinkability' of ownable expression became a new conceptual point in the legal landscape. Accessibility collapse is high because, once the concept emerged, it fundamentally altered the legal possibilities for expression, making pre-1710 conceptualizations less accessible for understanding post-1710 legal claims. Resistance is low because it describes a historical conceptual shift, not an actively contested policy.
 *
 * PERSPECTIVAL GAP:
 *   As a conceptual historical constraint, there is little perspectival gap among analytical observers. The 'gap' exists between the pre-1710 and post-1710 conceptual worlds, where the former lacked the very vocabulary to articulate the latter's claims.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint primarily benefits analytical observers (legal historians, IP scholars) by providing a framework for understanding legal evolution. It does not directly extract from or subsidize any active parties in the present, as it describes a past conceptual event. Pre-1710 actors are 'excluded' because the conceptual framework did not exist for them.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the typical sense, as it describes a historical conceptual emergence. Its 'mandate' is to accurately describe a past event. The classification as a Mountain reflects its fixed, unchangeable nature as a historical fact, preventing mislabeling it as an active, extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thinkability_vs_first_holding,
    'Is the emergence of ''thinkability'' (conceptual coherence) distinct from the ''first holding'' (who first legitimately claimed the right)?',
    'Detailed historical-legal analysis comparing the conceptual arguments for the Statute of Anne with the actual legal claims and enforcement actions immediately following its enactment. If the conceptual space for ''ownable expression'' was clearly articulated before authors consistently won cases, they are distinct.',
    'If distinct, this reading stands as a pure conceptual Mountain. If inseparable, the ''thinkability'' reading would be more closely tied to the ''first_holding_reading'', potentially shifting its classification towards a Rope or Tangled Rope if the ''first holding'' involved active coordination or extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(thinkability_vs_first_holding, conceptual, 'Distinguishing the conceptual emergence of IP from its initial legal application.').

omega_variable(
    pre_1710_proto_ip_claims,
    'To what extent did pre-1710 legal disputes or practices contain ''proto-IP'' claims that were conceptually similar to post-1710 ''ownable expression'', even if not explicitly named as such?',
    'Comparative legal history examining the functional equivalence of pre-1710 guild privileges or royal grants to later IP rights, focusing on the underlying rationale and effects rather than explicit terminology.',
    'If significant proto-IP claims existed, the ''thinkability'' reading''s claim of a sharp conceptual break in 1710 would be weakened, potentially shifting the constraint''s ''emerges_naturally'' status to ''contested'' and increasing its extractiveness if these proto-claims involved active enforcement and benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_1710_proto_ip_claims, empirical, 'Assessing the novelty of the 1710 conceptual shift versus earlier, unacknowledged forms of IP.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 1650, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1650, ip_category_emergence__thinkability_reading, theater_ratio, 1650, 0.0).
narrative_ontology:measurement(ip_c_tr_t1680, ip_category_emergence__thinkability_reading, theater_ratio, 1680, 0.0).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__thinkability_reading, theater_ratio, 1710, 0.01).
narrative_ontology:measurement(ip_c_tr_t1720, ip_category_emergence__thinkability_reading, theater_ratio, 1720, 0.01).
narrative_ontology:measurement(ip_c_tr_t1750, ip_category_emergence__thinkability_reading, theater_ratio, 1750, 0.01).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1650, ip_category_emergence__thinkability_reading, base_extractiveness, 1650, 0.0).
narrative_ontology:measurement(ip_c_be_t1680, ip_category_emergence__thinkability_reading, base_extractiveness, 1680, 0.0).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1710, 0.05).
narrative_ontology:measurement(ip_c_be_t1720, ip_category_emergence__thinkability_reading, base_extractiveness, 1720, 0.05).
narrative_ontology:measurement(ip_c_be_t1750, ip_category_emergence__thinkability_reading, base_extractiveness, 1750, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1650, ip_category_emergence__thinkability_reading, suppression_requirement, 1650, 0.0).
narrative_ontology:measurement(ip_c_su_t1680, ip_category_emergence__thinkability_reading, suppression_requirement, 1680, 0.0).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__thinkability_reading, suppression_requirement, 1710, 0.02).
narrative_ontology:measurement(ip_c_su_t1720, ip_category_emergence__thinkability_reading, suppression_requirement, 1720, 0.02).
narrative_ontology:measurement(ip_c_su_t1750, ip_category_emergence__thinkability_reading, suppression_requirement, 1750, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, information_standard).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ip_category_emergence' kernel, focusing on the conceptual emergence of 'ownable expression' as a legally coherent category. It is linked to sibling readings that emphasize the first legitimate claimant or the relationship between conceptual and practical shifts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
