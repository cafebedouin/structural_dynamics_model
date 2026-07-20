% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__baronial_privilege_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta 1215 â Baronial Privilege Reading
 *   domain: constitutional/legal_history
 *
 * SUMMARY:
 *   This constraint story instantiates the baronial privilege reading of the
 *   Magna Carta 1215 kernel. Under this reading, the charter is a narrow
 *   feudal contract between King John and the landholding baronage: 'free
 *   men' denotes the baronial class, protections are limited to contracting
 *   parties, and commoners, women, and non-landowners are structurally
 *   excluded. This is one of three readings of the contested Magna Carta
 *   kernel; the universal rights reading and living document reading are
 *   sibling constraints. The authored metrics describe the constraint's
 *   operation during its immediate 1215-1225 enforcement period, where
 *   genuine baronial coordination coexisted with the extraction of privilege
 *   from the majority of the population.
 *
 * KEY AGENTS:
 *   - Crown (King John): Agenda-setter forced to surrender arbitrary prerogative; bears the cost of limited royal power.
 *   - Landowning barons: Primary beneficiaries receiving legal protection and consent rights.
 *   - Commoners: Excluded majority bearing the cost of continued feudal subjugation without charter safeguards.
 *   - Women: Excluded group whose legal subordination is preserved by the charter's silence and operative language.
 *   - Non-landowners: Excluded unfree population explicitly left outside the protection set.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.45).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.48).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta 1215 â Baronial Privilege Reading").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional/legal_history").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, '12994871-c5df-4cf1-8d43-fba15d25ec2e').
narrative_ontology:cs_kernel_codification('12994871-c5df-4cf1-8d43-fba15d25ec2e', fixed_text).
narrative_ontology:cs_authority_grounding('12994871-c5df-4cf1-8d43-fba15d25ec2e', lineage).
narrative_ontology:cs_interpretation_layer_present('12994871-c5df-4cf1-8d43-fba15d25ec2e').
narrative_ontology:cs_reading_relation('12994871-c5df-4cf1-8d43-fba15d25ec2e', magna_carta_1215__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('12994871-c5df-4cf1-8d43-fba15d25ec2e', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('12994871-c5df-4cf1-8d43-fba15d25ec2e', foundational, free_men_denotes_baronial_class).
narrative_ontology:cs_axiom_status(free_men_denotes_baronial_class, holdable).
narrative_ontology:cs_axiom_grounding('12994871-c5df-4cf1-8d43-fba15d25ec2e', free_men_denotes_baronial_class, empirically_contingent).
narrative_ontology:cs_axiom('12994871-c5df-4cf1-8d43-fba15d25ec2e', foundational, charter_binds_only_contracting_feudal_parties).
narrative_ontology:cs_axiom_status(charter_binds_only_contracting_feudal_parties, holdable).
narrative_ontology:cs_axiom_grounding('12994871-c5df-4cf1-8d43-fba15d25ec2e', charter_binds_only_contracting_feudal_parties, conventional).
narrative_ontology:cs_reference_frame('12994871-c5df-4cf1-8d43-fba15d25ec2e', feudal_reciprocal_contract_1215).
narrative_ontology:cs_drift_state('12994871-c5df-4cf1-8d43-fba15d25ec2e', modern_constitutional_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('12994871-c5df-4cf1-8d43-fba15d25ec2e', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, commoners).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, women).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, non_landowners).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, feudal_reciprocity_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, baronial_consent_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sealed the charter at Runnymede under baronial duress; retains sovereignty but surrenders arbitrary prerogative over wardship, marriage, and taxation vis-Ã -vis the contracting barons. Attempted to annul the charter via Pope Innocent III, indicating structural resistance to the constraint's limits.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, crown, agenda_setter,
    institutional, generational, constrained, national).

% Are the sole beneficiaries of the charter's liberty clauses; obtain guarantees against arbitrary arrest and seizure, and gain the right to withhold consent for scutage. Exercise enforcement through feudal mechanisms including the security clause (Clause 61) authorizing rebellion against the crown.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landowning_barons, beneficiary,
    powerful, generational, constrained, national).

% Comprise the majority of the population but are excluded from the document's protections; remain subject to arbitrary manorial and royal jurisdiction. The charter's operative term 'free men' does not extend to them, leaving their feudal obligations and legal vulnerability unchanged.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, commoners, excluded,
    powerless, immediate, trapped, local).

% Are explicitly sidelined by clauses regulating wardship and marriage of heirs; lack standing as 'free men' under the charter. Their legal subordination through coverture-like norms is preserved and unaddressed by the feudal contract.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, women, excluded,
    powerless, immediate, trapped, local).

% Unfree peasants, villeins, and landless laborers fall entirely outside the protection set. The charter explicitly preserves certain obligations owed by lords to them, reinforcing their exclusion from the liberty framework.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, non_landowners, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:fixing_cost_class(magna_carta_1215__baronial_privilege_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes reciprocal feudal obligations between the English crown and the landholding baronage, creating a written settlement of 1215 that limits royal prerogative over taxation, wardship, and justice in exchange for baronial allegiance and counsel.
% TRANSFER_FUNCTION: Transfers legal protection and procedural safeguards from the crown to the landholding barons; transfers feudal service and counsel from the barons to the crown. Commoners, women, and unfree persons receive no transfer and remain subject to existing feudal obligations.
% ABSENT_VOICES: Commoners, villeins, women, free tenants without land, and urban burgesses were excluded from Runnymede. They would contest the narrow definition of 'free men' and the preservation of feudal hierarchy had they been admitted.
% DISAPPEARANCE_RATIONALE: If the baronial privilege constraint vanished in 1215, the baronial rebellion would resume; royal prerogative would reassert arbitrarily over the barons, and the specific feudal settlement would collapse. The arrangements of 1215 depended on this constraint.
% FOUNDING_PROBLEM: King John's arbitrary taxation, abuse of feudal wardship and marriage rights, and disregard for customary counsel had driven the baronage to armed rebellion, threatening civil war and a collapse of reciprocal feudal order.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary chroniclers including Roger of Wendover and the Barnwell annalist attest the crisis from outside the baronial beneficiary party. Modern constitutional historians corroborate that the immediate feudal grievances were resolved or transformed by the mid-13th century.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__baronial_privilege_reading_tests).
:- end_tests(magna_carta_1215__baronial_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects a genuine coordination function between crown and barons coupled with the asymmetric exclusion of the majority. Suppression (0.48) captures the active enforcement required: baronial rebellion (Clause 61), papal annulment attempts, and reissue under duress. Theater is low (0.20) because the charter's feudal function is operative, not performative, during this interval. Accessibility collapse (0.35) is moderate: alternatives such as absolute royal prerogative or universal customary rights are partially displaced but remain conceivable. Resistance (0.55) is substantial because the crown actively resists the constraint and the excluded population lacks capacity to resist their exclusion.
 *
 * PERSPECTIVAL GAP:
 *   The baronial seat perceives the constraint as necessary protection against arbitrary royal tyranny. The crown seat perceives it as a coerced surrender of legitimate prerogative. The commoner, women, and non-landowner seats perceive the same document as an extractive pact that secures privilege for the few while preserving their subjugation. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Landowning barons are declared beneficiaries with constrained exit (feudal obligations bind them to the kingdom), yielding a directionality near the beneficiary end. The crown is agenda-setter but structurally targeted by the constraint's limitations; a directionality override raises its d toward the target end to reflect surrendered prerogative. Commoners, women, and non-landowners are declared victims with trapped exit, placing them near the full-target end (d â 1.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The genuine coordination between crown and barons prevents classifying the constraint as a pure snare, while the explicit exclusion of the majority prevents classifying it as a pure rope. The baronial privilege reading captures this hybrid structure: the coordination is real but the beneficiary set is artificially narrow, generating asymmetric extraction through exclusion rather than direct transfer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_completeness,
    'Does the baronial privilege reading exhaust the Magna Carta kernel''s historical reality, or does it underdetermine the text by excluding the adaptive and universalist readings that later developed?',
    'Comparative legal-historical analysis of the document''s reception across centuries; determination of whether the constraint''s identity is fixed at enactment or co-constituted by later interpretive practice.',
    'If the text is fixed at enactment, this reading is complete and siblings are anachronistic projections; if underdetermined, the kernel supports multiple structurally distinct constraints and this reading is one valid instantiation among several.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_completeness, conceptual, 'Whether the baronial privilege reading fully accounts for the Magna Carta kernel.').

omega_variable(
    free_men_semantic_scope,
    'Is ''free men'' in Clauses 1 and 39 coextensive with the landholding baronial class, or did the term include knights, free tenants, and burgesses in 1215?',
    'Philological and social-historical analysis of 13th-century legal terminology; review of contemporary charters, writs, and plea rolls using the term ''liber homo''.',
    'A broader ''free men'' class would expand the beneficiary set and reduce extraction from non-barons, shifting the constraint toward a wider coordination function; a narrow reading confirms the tangled rope structure with concentrated baronial benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_men_semantic_scope, empirical, 'Semantic scope of ''free men'' in 1215 legal usage.').

omega_variable(
    enforcement_mechanism_nature,
    'Was the 1215 charter''s enforcement structurally a legal-judicial process or a feudal right of baronial rebellion under Clause 61?',
    'Analysis of Clause 61''s security clause and its invocation during the First Barons'' War; comparison with enforcement mechanisms in the 1216, 1217, and 1225 reissues.',
    'If enforcement was primarily rebellion, the constraint is a private arms-backed contract; if judicial, it is a nascent legal institution. This affects classification of active enforcement and the directionality of the crown seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_nature, empirical, 'Nature of the charter''s 1215 enforcement machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_1215_bar_tr_t0, magna_carta_1215__baronial_privilege_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(magna_carta_1215_bar_tr_t2, magna_carta_1215__baronial_privilege_reading, theater_ratio, 2, 0.12).
narrative_ontology:measurement(magna_carta_1215_bar_tr_t4, magna_carta_1215__baronial_privilege_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(magna_carta_1215_bar_tr_t6, magna_carta_1215__baronial_privilege_reading, theater_ratio, 6, 0.16).
narrative_ontology:measurement(magna_carta_1215_bar_tr_t8, magna_carta_1215__baronial_privilege_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(magna_carta_1215_bar_tr_t10, magna_carta_1215__baronial_privilege_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(magna_carta_1215_bar_be_t0, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(magna_carta_1215_bar_be_t2, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(magna_carta_1215_bar_be_t4, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(magna_carta_1215_bar_be_t6, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(magna_carta_1215_bar_be_t8, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(magna_carta_1215_bar_be_t10, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_1215_bar_su_t0, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(magna_carta_1215_bar_su_t2, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 2, 0.7).
narrative_ontology:measurement(magna_carta_1215_bar_su_t4, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(magna_carta_1215_bar_su_t6, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(magna_carta_1215_bar_su_t8, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(magna_carta_1215_bar_su_t10, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, living_document_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Magna Carta 1215 kernel. The baronial privilege reading is decomposed from the universal rights and living document readings per the Îµ-invariance principle: each reading produces a structurally distinct beneficiary/victim set and Îµ value, and therefore instantiates a different constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_1215__baronial_privilege_reading, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
