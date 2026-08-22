% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__feudal_prerogative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__feudal_prerogative_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Magna Carta Clause 39: Feudal Prerogative Reading
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   This constraint models the feudal prerogative reading of Magna Carta
 *   Clause 39: a narrow procedural safeguard embedded within the hierarchical
 *   feudal order, preserving crown and baronial authority while formalizing
 *   procedural rights for the elite. The reading interprets 'no free man
 *   shall be proceeded against save by lawful judgement of his peers or the
 *   law of the land' as a constraint on arbitrary action within feudal
 *   hierarchy, not as a universal principle against state power. The
 *   constraint's extractiveness is low because the arrangement redistributes
 *   security within hierarchy rather than extracting from a subordinate
 *   class; suppression is minimal because the hierarchy itself is the assumed
 *   background. The claim is rope (genuine coordination within feudal
 *   structure) and the metrics reflect that framing: low extraction, low
 *   suppression, modest theater (procedure is real but circumscribed). This
 *   reading coexists with liberal and originalist readings of the same kernel
 *   text; the three readings are separate constraints with different ε
 *   values, beneficiary sets, and axioms.
 *
 * KEY AGENTS:
 *   - crown_authority: Agenda-setter (preserves ultimate authority while accepting procedural constraint)
 *   - baronial_peers: Beneficiary (gains procedural security within feudal hierarchy)
 *   - free_men_burgesses: Secondary beneficiary (narrow procedural rights for elite commons)
 *   - unfree_peasantry: Excluded (outside the constraint's referent; no procedural rights)
 *   - feudal_common_law: Vindicated proposition (codification of customary procedural expectation within hierarchy)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.28).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.15).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Magna Carta Clause 39: Feudal Prerogative Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional/legal/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, 'afa9e7fa-020a-4c06-8544-9ee93ef858d0').
narrative_ontology:cs_kernel_codification('afa9e7fa-020a-4c06-8544-9ee93ef858d0', fixed_text).
narrative_ontology:cs_authority_grounding('afa9e7fa-020a-4c06-8544-9ee93ef858d0', lineage).
narrative_ontology:cs_interpretation_layer_present('afa9e7fa-020a-4c06-8544-9ee93ef858d0').
narrative_ontology:cs_reading_relation('afa9e7fa-020a-4c06-8544-9ee93ef858d0', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('afa9e7fa-020a-4c06-8544-9ee93ef858d0', magna_carta_clause_39__originalist_limitation_reading, influences).
narrative_ontology:cs_axiom('afa9e7fa-020a-4c06-8544-9ee93ef858d0', foundational, feudal_hierarchy_natural_and_enduring).
narrative_ontology:cs_axiom_status(feudal_hierarchy_natural_and_enduring, overridden).
narrative_ontology:cs_axiom_grounding('afa9e7fa-020a-4c06-8544-9ee93ef858d0', feudal_hierarchy_natural_and_enduring, conventional).
narrative_ontology:cs_axiom('afa9e7fa-020a-4c06-8544-9ee93ef858d0', foundational, procedure_within_hierarchy_preserves_crown_authority).
narrative_ontology:cs_axiom_status(procedure_within_hierarchy_preserves_crown_authority, holdable).
narrative_ontology:cs_axiom_grounding('afa9e7fa-020a-4c06-8544-9ee93ef858d0', procedure_within_hierarchy_preserves_crown_authority, instrumental).
narrative_ontology:cs_reference_frame('afa9e7fa-020a-4c06-8544-9ee93ef858d0', feudal_hierarchy_with_baronial_procedure).
narrative_ontology:cs_drift_state('afa9e7fa-020a-4c06-8544-9ee93ef858d0', modern_constitutional_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('afa9e7fa-020a-4c06-8544-9ee93ef858d0', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, crown_authority).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, baronial_peers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, free_men_burgesses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ultimate prerogative to govern and administer justice, but accepts procedural constraint: may not proceed against baronial peers or free men without legal process (per legem terrae). The crown's power is not diminished but formalized within feudal hierarchy — the constraint preserves hierarchical authority while adding procedural guardrails against arbitrary assault on peer status and property.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, crown_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive procedural protection against arbitrary crown action that would strip their status, lands, or titles. The constraint secures their position within the feudal order — they remain subject to lawful crown authority but cannot be dispossessed by whim. Their beneficiary status is tied to rank; the constraint does not extend to commoners or peasants.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, baronial_peers, beneficiary,
    powerful, generational, mobile, national).

% Gain narrowly-framed procedural rights: the crown may not seize their chattels or demesne without due process (judgement by peers or lawful judgement). The protection is procedural, not substantive — the rights are bounded by existing feudal hierarchy and apply only within the framework of established law, not universal principle.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, free_men_burgesses, beneficiary,
    moderate, biographical, constrained, local).

% Are expressly outside the constraint's referent: 'free men' (liberi homines) does not include bondsmen or villeins. The constraint's procedural rights do not apply to the majority of the realm's population, who remain subject to baronial and crown authority without process. Their exclusion is the unquestioned premise of the charter.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, unfree_peasantry, excluded,
    powerless, immediate, trapped, local).

% The doctrine of lawful governance by established process (rather than arbitrary will) is vindicated by the constraint's operation — not as a new principle but as a formalization of existing feudal custom. The charter's preservation of the feudal order while formalizing procedure represents no radical shift, only codification of baronial privilege within hierarchy.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, feudal_common_law, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(magna_carta_clause_39__feudal_prerogative_reading, feudal_common_law).

% Read the clause in contested ways: feudal interpreters see a procedural guardrail within hierarchy; liberal interpreters see seeds of universal rights; originalists see a specific remedy for documented 1215 abuses. The constraint's meaning remains unstable across readings.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, interpretive_communities, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Formalizes the feudal bargain within established hierarchy: the crown retains sovereign authority and justice power, but commits to exercising that authority through lawful process (per legem terrae) rather than arbitrary will. This stabilizes the relationship between crown and nobles by making procedural rights explicit, reducing the crisis risk that motivates rebellion.
% TRANSFER_FUNCTION: Redistributes nothing materially; rather, it redistributes security of status and property rights within the feudal hierarchy. Baronial peers and free men gain procedural certainty that they cannot be dispossessed by royal whim; the crown gains legitimacy for its authority by accepting formal procedural constraints. The transfer is symbolic and relational, not material.
% ABSENT_VOICES: The unfree peasantry (the vast majority) has no voice in the charter's negotiation. Church authorities, while present at Runnymede, are a separate estate. Urban merchants in nascent towns are marginal to the feudal bargain. The constraint is forged between crown and baronial elite; the voiceless are those who will bear the consequences of whatever order is established.
% DISAPPEARANCE_RATIONALE: From the feudal prerogative reading, if Clause 39 vanished, the crown would retain its authority and the baronial peers would lose their procedural security, but the feudal order itself would persist — conquest and renewed negotiation would follow. From rival readings (liberal, originalist), the disappearance would signal abandonment of due process principle, with radically different consequences. The verdict depends on which reading's framing one accepts.
% FOUNDING_PROBLEM: Baronial rebellion driven by crown seizure of lands, titles, and persons without lawful process — the crown's exercise of prerogative power had eroded the customary feudal compact. Clause 39 was designed to formalize that barons possessed enforceable procedural rights against arbitrary crown assault on their status and property.
% FOUNDING_PROBLEM_CORROBORATION: Medieval chroniclers attest that Clause 39 responded to documented crown abuses (arbitrary disseisin, dispossession without process). Feudal interpreters and historians of the feudal period corroborate that the baronial revolt stemmed from breach of customary procedural expectations. However, interpretation diverges sharply: liberal and originalist readers dispute whether this problem was specific to 1215 or indicative of a broader principle against arbitrary state action.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).
:- end_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the constraint operates within an accepted hierarchical framework where the crown's authority is legitimate by feudal premises; the constraint redistributes procedural certainty rather than extracting from a lower order. Suppression is minimal (0.15) because the hierarchy itself is the assumed background — there is no alternative order being suppressed, only procedural formalization within the accepted feudal structure. Theater is modest (0.22) because the procedural function is genuine: crowns do respect customary process within feudal compact, and the constraint codifies that real practice. The measurement series models slow growth in extractiveness and theater over the interval as interpretive communities begin applying the clause beyond its feudal scope, reframing it as universal principle — a drift from feudal prerogative toward liberal meaning. Theater grows as the constraint becomes increasingly invoked as cover for actions that diverge from feudal practice.
 *
 * PERSPECTIVAL GAP:
 *   From the crown's seat (agenda-setter): the constraint is a rational coordination device that formalizes the feudal bargain and legitimates crown authority by accepting procedural guardrails. From the baronial seat (beneficiary): the constraint is a hard-won security guarantee against arbitrary prerogative. From the unfree peasant seat (excluded): the constraint is irrelevant — they remain subject to arbitrary authority without process. The engine should compute significant divergence in how these seats experience the constraint: the crown and barons perceive it as coordination within hierarchy, while excluded and powerless populations perceive it as none of their concern.
 *
 * DIRECTIONALITY LOGIC:
 *   The crown's directionality is low (near 0.2–0.3): it is a structural beneficiary of proceduralization (legitimacy), though not extracting rents; it is subject to procedural constraint but retains ultimate authority. Baronial peers are near symmetric (0.4–0.5): they gain procedural security but remain within the feudal hierarchy and subject to lawful crown action; the constraint does not free them from feudal obligation. Free men and burgesses approach target positioning (0.6–0.7) because the procedural rights are narrow and conditional, and their exit options are severely constrained by the feudal order itself. The unfree peasantry are analytically excluded from directionality calculation on this reading because the constraint's referent is 'free men' — they do not sit within the constraint's scope.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by keeping the founding problem live within its own frame: the constraint was built to solve the problem of arbitrary crown prerogative within feudal hierarchy, and that problem remains live so long as feudal hierarchy persists. However, the reading is vulnerable to mandatrophy once interpretive communities begin treating Clause 39 as a universal principle rather than a feudal procedural guardrail. If the constraint is expanded to apply to all subjects (peasants included), the founding problem (baronial procedural security within hierarchy) is no longer the constraint's justification — it becomes a constraint on arbitrary state power generally. At that point, the feudal reading's mandate has outlived its function, and the constraint becomes a zombie: maintained in the charter's text but interpreted in a radically different way. The measurement series models this incipient drift, with theater rising as the constraint is increasingly cited for purposes beyond feudal proceduralization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feudal_hierarchy_natural_or_constructed,
    'Is the feudal hierarchy a natural or inevitable order, or is it a constructed arrangement that Clause 39 helps sustain?',
    'Genealogical and institutional analysis: if feudal hierarchy requires explicit proceduralization and baronial enforcement to persist, it is constructed; if it emerges naturally from property and military arrangements, it is structural to the context. Comparative analysis across societies with and without formal proceduralization of feudal prerogative provides cross-validation.',
    'If feudal hierarchy is constructed, the constraint is helping to maintain a specific political order (higher extraction from subordinate classes, even if not from the elite); if it is natural, the constraint is genuine coordination within an inevitable structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(feudal_hierarchy_natural_or_constructed, conceptual, 'Whether feudal hierarchy is a natural backdrop or a constructed arrangement the constraint preserves.').

omega_variable(
    free_men_category_scope,
    'What is the exact referent of ''free men'' (liberi homines) in the clause''s 1215 context, and does that referent shift over time?',
    'Textual analysis of feudal charters before and after 1215; investigation of how courts applied the clause to different social strata; tracing of the term''s expansion from baronial elite to urban merchants to broader commons.',
    'If the referent is strictly baronial in 1215, the constraint is an elite coordination device; if it includes broader free commons (merchants, urban dwellers, free tenants), the constraint is already universal in scope even under feudal framing. Scope drift would signal the constraint''s meaning is shifting toward liberal interpretation without formal re-negotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_men_category_scope, empirical, 'Whether ''free men'' is a narrow elite category or a broader designation that shifts over time.').

omega_variable(
    reading_decomposition_instability,
    'Is this feudal prerogative reading genuinely stable under modern legal analysis, or is it already a reconstruction by historians seeking an alternative framing?',
    'Meta-analysis: examine the genealogy of the feudal reading itself. Is it attested in medieval sources or is it a 20th-century historiographical construction? If the latter, it may not be a live reading in its own era but rather an artifact of modern interpretive work.',
    'If the reading is a modern historiographical construction, the engine''s classification of this constraint as a rope within feudal hierarchy may not map to any actual historical constraint; instead, it would be modeling a plausible-but-unrealized past. This would signal the need for a separate constraint modeling the reading''s claim as a normative position (should be interpreted this way) rather than historical fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_decomposition_instability, conceptual, 'Whether the feudal prerogative reading is a live historical interpretation or a modern historiographical construction.').

omega_variable(
    sibling_reading_coexistence,
    'In what institutional and temporal contexts do the feudal, liberal, and originalist readings of Clause 39 genuinely coexist, and in what contexts does one reading foreclose the others?',
    'Institutional history: trace which reading was dominant in medieval courts, which in early modern period, which in modern constitutionalism. Identify moments of explicit switching (when one reading was rejected in favor of another) versus moments where multiple readings are held simultaneously by different institutional actors.',
    'If readings coexist across institutional seats (feudal in baronial practice, liberal in nascent parliament, originalist in later constitutional scholarship), the three constraints are genuinely distinct and in simultaneous operation. If one reading forecloses the others over time (e.g., liberal reading displaces feudal over the course of constitutional evolution), the relationship is foreclosure rather than coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, empirical, 'Whether sibling readings coexist institutionally or whether one forecloses others over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(magn_tr_t0, observed).
narrative_ontology:measurement(magn_tr_t20, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(magn_tr_t20, observed).
narrative_ontology:measurement(magn_tr_t40, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(magn_tr_t40, observed).
narrative_ontology:measurement(magn_tr_t60, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement_basis(magn_tr_t60, observed).
narrative_ontology:measurement(magn_tr_t80, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement_basis(magn_tr_t80, observed).
narrative_ontology:measurement(magn_tr_t100, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement_basis(magn_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(magn_be_t0, observed).
narrative_ontology:measurement(magn_be_t20, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement_basis(magn_be_t20, observed).
narrative_ontology:measurement(magn_be_t40, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 40, 0.24).
narrative_ontology:measurement_basis(magn_be_t40, observed).
narrative_ontology:measurement(magn_be_t60, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 60, 0.26).
narrative_ontology:measurement_basis(magn_be_t60, observed).
narrative_ontology:measurement(magn_be_t80, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 80, 0.27).
narrative_ontology:measurement_basis(magn_be_t80, observed).
narrative_ontology:measurement(magn_be_t100, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement_basis(magn_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_clause_39__feudal_prerogative_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__feudal_prerogative_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% Magna Carta Clause 39 is a contested kernel with three structural readings: feudal prerogative (this constraint), liberal due process, and originalist limitation. Each reading yields a different constraint with different ε, beneficiary set, victim set, and axioms. The three constraints form a family linked by network.affects_constraints. The feudal reading provides the historical context for the liberal reading (which reinterprets feudal procedure as universal principle) and influences the originalist reading (which anchors the clause to the 1215 context as a limit on its scope). Decomposition follows ε-invariance principle: a single text is not a single constraint when different readings of it produce substantively different structural claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
