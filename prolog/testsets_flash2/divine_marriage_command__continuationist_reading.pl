% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Divine Marriage Command (Continuationist Reading)
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story models the 'continuationist' reading of a divine
 *   marriage command within a specific religious tradition. This reading
 *   asserts that the original command for polygamy remains doctrinally valid,
 *   and the historical 'Manifesto' (a declaration suspending the practice)
 *   was a prudential response to federal legal pressure, not a doctrinal
 *   rescission. This interpretation allows for the theological legitimacy of
 *   past and potential future polygamous practices, distinguishing it from
 *   readings that declare monogamy as a new, superseding divine command. The
 *   constraint operates as a Tangled Rope, coordinating theological
 *   continuity while extracting costs from those who adhere to the practice
 *   in defiance of secular law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.65).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.78).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Divine Marriage Command (Continuationist Reading)").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, '808a5542-0a58-4887-a313-f84720560a5e').
narrative_ontology:cs_kernel_codification('808a5542-0a58-4887-a313-f84720560a5e', fixed_text).
narrative_ontology:cs_authority_grounding('808a5542-0a58-4887-a313-f84720560a5e', lineage).
narrative_ontology:cs_interpretation_layer_present('808a5542-0a58-4887-a313-f84720560a5e').
narrative_ontology:cs_reading_relation('808a5542-0a58-4887-a313-f84720560a5e', divine_marriage_command__substitutionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('808a5542-0a58-4887-a313-f84720560a5e', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('808a5542-0a58-4887-a313-f84720560a5e', foundational, divine_command_is_eternal).
narrative_ontology:cs_axiom_status(divine_command_is_eternal, holdable).
narrative_ontology:cs_axiom_grounding('808a5542-0a58-4887-a313-f84720560a5e', divine_command_is_eternal, theological).
narrative_ontology:cs_axiom('808a5542-0a58-4887-a313-f84720560a5e', foundational, manifesto_is_prudential_not_doctrinal).
narrative_ontology:cs_axiom_status(manifesto_is_prudential_not_doctrinal, holdable).
narrative_ontology:cs_axiom_grounding('808a5542-0a58-4887-a313-f84720560a5e', manifesto_is_prudential_not_doctrinal, conventional).
narrative_ontology:cs_reference_frame('808a5542-0a58-4887-a313-f84720560a5e', original_divine_command_for_plural_marriage).
narrative_ontology:cs_drift_state('808a5542-0a58-4887-a313-f84720560a5e', contemporary_legal_and_social_context, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('808a5542-0a58-4887-a313-f84720560a5e', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, continuationist_adherents).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, fundamentalist_splinter_groups).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, polygamous_families_under_prosecution).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, women_in_polygamous_marriages).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the theological validity of polygamy as a divine command, viewing the Manifesto as a temporary, prudential suspension under duress. Their identity is deeply tied to this interpretation, which preserves the possibility of future practice or the legitimacy of past practice. They benefit from the doctrinal continuity.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, continuationist_adherents, beneficiary,
    moderate, generational, identity_locked, local).

% Actively practice polygamy, claiming direct continuity with original revelation and rejecting the mainstream church's interpretation of the Manifesto as a doctrinal shift. They enforce their interpretation within their communities, often facing legal prosecution.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_splinter_groups, agenda_setter,
    organized, generational, constrained, regional).

% Bear the direct legal and social costs of practicing polygamy in jurisdictions where it is illegal. They are often isolated and face severe penalties, yet their commitment to the continuationist reading binds them to the practice.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, polygamous_families_under_prosecution, payer,
    powerless, biographical, trapped, local).

% Often experience significant social, economic, and psychological costs within polygamous structures, including limited autonomy and resources. Their identity and social standing are frequently tied to their marital status within the community, making exit extremely difficult.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, women_in_polygamous_marriages, payer,
    powerless, biographical, identity_locked, local).

% While officially adhering to the Manifesto as a suspension, they navigate the tension between historical doctrine and contemporary legal/social norms. They enforce monogamy within the mainstream church but often avoid direct condemnation of continuationist groups to maintain a degree of theological continuity.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_church_leadership, agenda_setter,
    institutional, civilizational, constrained, global).

% Enforces anti-polygamy laws, prosecuting individuals and groups that practice it. From its perspective, the Manifesto was a necessary adaptation to legal reality, and any continued practice is a violation of secular law.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, federal_legal_system, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the theological understanding of marriage within a religious tradition, allowing adherents to reconcile historical practice with contemporary institutional requirements by framing the Manifesto as a temporary suspension rather than a doctrinal change.
% TRANSFER_FUNCTION: Transfers theological legitimacy and historical continuity to continuationist adherents and splinter groups, while transferring legal and social costs to those who actively practice polygamy.
% ABSENT_VOICES: Former members of polygamous communities who have left the faith or the practice, and secular critics of religious fundamentalism, would argue that the continuationist reading perpetuates harm and enables coercive practices, but they are excluded from the internal theological discourse.
% DISAPPEARANCE_RATIONALE: If the continuationist reading vanished, it would fundamentally alter the theological landscape for a significant religious tradition. Splinter groups would lose their doctrinal justification, mainstream leadership would face pressure to either fully rescind or re-embrace polygamy, and the identities of many adherents would be destabilized. The legal and social status of polygamous families would also be profoundly impacted.
% FOUNDING_PROBLEM: The original divine command for plural marriage was understood as a means of building a righteous lineage and fulfilling a covenant with God, particularly in a context of persecution and demographic imbalance.
% FOUNDING_PROBLEM_CORROBORATION: Continuationist adherents and splinter groups attest that the divine command remains live and relevant for spiritual progression. Mainstream church historians acknowledge the historical context of the original command. Critics, however, argue that the 'problem' is now a justification for social control, not a genuine spiritual necessity, and that the original context is no longer applicable.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the ongoing legal and social penalties borne by those who practice polygamy under this reading, as well as the internal social costs for women within such marriages. Suppression (0.78) is high due to both external legal enforcement and internal identity-locking mechanisms within continuationist communities. The theater ratio (0.4) reflects the performative aspect of maintaining doctrinal continuity while largely suspending the practice, with some enforcement directed at managing the boundary with splinter groups rather than fully rescinding the doctrine. The claimed type is 'rope' from the perspective of the adherents who see it as coordinating their faith, but the metrics reveal it as a 'tangled_rope' due to the asymmetric extraction and active enforcement required to maintain the distinction between doctrinal validity and prudential suspension.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of continuationist adherents, this constraint is a 'rope' that coordinates their theological understanding and preserves a sacred principle. From the perspective of polygamous families under prosecution or women within these marriages, it is a 'snare' that extracts severe costs and suppresses alternatives. The mainstream church leadership might view it as a 'scaffold' that allowed for a necessary transition, but the continuationist reading denies the 'sunset' of the doctrine itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Continuationist adherents and fundamentalist splinter groups are beneficiaries (low d) as they gain theological legitimacy and continuity. Polygamous families and women in these marriages are targets (high d) as they bear the direct legal, social, and personal costs. Mainstream church leadership is an agenda-setter, navigating the tension and enforcing a 'prudential suspension' that benefits the institution's survival. The federal legal system acts as an external enforcer, targeting the practice itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (which would ignore the substantial extraction and suppression) or a pure Snare (which would ignore the genuine coordination function of maintaining theological continuity for adherents). It highlights how the constraint's original mandate (divine command for plural marriage) is now 'contested' in its status, with the 'prudential suspension' acting as a mechanism to manage the tension between the original mandate and external pressures, while still allowing for extraction from those who adhere to the original practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_vs_prudential_distinction,
    'Is the distinction between ''doctrinal validity'' and ''prudential suspension'' genuinely stable, or does it inevitably collapse under sustained external pressure or internal dissent?',
    'Long-term observation of the mainstream church''s internal theological discourse and enforcement actions regarding splinter groups; analysis of whether the ''suspension'' is ever formally rescinded or re-affirmed as a permanent doctrinal shift.',
    'If the distinction collapses, the constraint would either become a ''snare'' (if the suspension is revealed as a cover for continued extraction) or a ''rope'' (if the suspension becomes a genuine, universally accepted doctrinal shift to monogamy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_vs_prudential_distinction, conceptual, 'Ambiguity in the theological status of polygamy''s suspension.').

omega_variable(
    identity_lock_vs_coercion,
    'For women in polygamous marriages, what proportion of their ''identity_locked'' exit option is due to internalized theological belief versus structural coercion (e.g., social isolation, economic dependency, lack of secular support networks)?',
    'Qualitative sociological studies of ex-members, analysis of support networks for women leaving polygamous communities, and legal aid access for those seeking exit.',
    'If primarily structural, the ''suppression'' metric''s effective impact is higher and more directly attributable to the constraint''s operation. If primarily internalized, the constraint''s persistence is more deeply embedded in individual belief systems, making external remedies less effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_coercion, empirical, 'Structural vs. internalized suppression mechanism for identity-locked agents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__continuationist_reading, theater_ratio, 1890, 0.2).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__continuationist_reading, theater_ratio, 1920, 0.3).
narrative_ontology:measurement(divi_tr_t1950, divine_marriage_command__continuationist_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(divi_tr_t1980, divine_marriage_command__continuationist_reading, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(divi_tr_t2000, divine_marriage_command__continuationist_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(divi_tr_t2024, divine_marriage_command__continuationist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__continuationist_reading, base_extractiveness, 1890, 0.5).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__continuationist_reading, base_extractiveness, 1920, 0.58).
narrative_ontology:measurement(divi_be_t1950, divine_marriage_command__continuationist_reading, base_extractiveness, 1950, 0.62).
narrative_ontology:measurement(divi_be_t1980, divine_marriage_command__continuationist_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(divi_be_t2000, divine_marriage_command__continuationist_reading, base_extractiveness, 2000, 0.67).
narrative_ontology:measurement(divi_be_t2024, divine_marriage_command__continuationist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__continuationist_reading, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__continuationist_reading, suppression_requirement, 1920, 0.75).
narrative_ontology:measurement(divi_su_t1950, divine_marriage_command__continuationist_reading, suppression_requirement, 1950, 0.78).
narrative_ontology:measurement(divi_su_t1980, divine_marriage_command__continuationist_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(divi_su_t2000, divine_marriage_command__continuationist_reading, suppression_requirement, 2000, 0.79).
narrative_ontology:measurement(divi_su_t2024, divine_marriage_command__continuationist_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'divine_marriage_command' kernel. Its sibling readings, 'substitutionist_reading' and 'coercion_visibility_reading', offer alternative interpretations of the Manifesto's impact on polygamy's doctrinal status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
