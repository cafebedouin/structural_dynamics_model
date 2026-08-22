% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__feudal_prerogative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Magna Carta Clause 39 — Feudal Prerogative Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   Clause 39 of Magna Carta (1215) states: 'No free man shall be seized or
 *   imprisoned, or stripped of his rights or possessions, or outlawed or
 *   exiled, or deprived of his standing in any other way, nor will we proceed
 *   with force against him, or send others to do so, except by the lawful
 *   judgment of his equals or by the law of the land.' This reading — the
 *   feudal prerogative reading — treats Clause 39 as a narrow procedural
 *   guarantee operating entirely within the feudal hierarchy. It protects
 *   tenants-in-chief and lesser nobility from arbitrary royal action by
 *   requiring judgment by peers (i.e., other nobles) or established customary
 *   law. The crown is constrained but not substantively limited; the
 *   constraint coordinates noble-crown relations by codifying existing
 *   customary protections, extracting minimal compliance from royal
 *   authority. The victim set is effectively empty — no class is harmed by
 *   this constraint; the crown's arbitrary power is checked, but its
 *   structural authority remains intact. The liberal_due_process_reading and
 *   originalist_limitation_reading are distinct constraints (other files)
 *   with different ε, different stakeholders, different classifications.
 *
 * KEY AGENTS:
 *   - crown: agenda_setter (institutional/generational/arbitrage/universal) — the king whose arbitrary power is procedurally checked but whose structural authority is preserved
 *   - tenants_in_chief: beneficiary (powerful/biographical/constrained/national) — great barons who gain codified protection against royal seizure without peer judgment
 *   - lesser_nobility: beneficiary (organized/biographical/constrained/national) — knights and minor landholders who gain the same procedural shield
 *   - free_men_excluded: excluded (powerless/biographical/trapped/national) — the majority of 'free men' in 1215 England who lacked the standing to invoke peer judgment; structurally excluded from the constraint's operation
 *   - royal_officials: payer (moderate/biographical/constrained/national) — sheriffs and judges whose discretionary enforcement is now procedurally bounded
 *   - legal_historians: observer (analytical/civilizational/analytical/universal) — analytical seat observing the constraint's historical operation and doctrinal fate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.12).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.18).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Magna Carta Clause 39 — Feudal Prerogative Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, '1ab18ac8-e2a2-4fff-b408-fc9682cde7d7').
narrative_ontology:cs_kernel_codification('1ab18ac8-e2a2-4fff-b408-fc9682cde7d7', fixed_text).
narrative_ontology:cs_authority_grounding('1ab18ac8-e2a2-4fff-b408-fc9682cde7d7', lineage).
narrative_ontology:cs_interpretation_layer_present('1ab18ac8-e2a2-4fff-b408-fc9682cde7d7').
narrative_ontology:cs_reading_relation('1ab18ac8-e2a2-4fff-b408-fc9682cde7d7', magna_carta_clause_39__liberal_due_process_reading, influences).
narrative_ontology:cs_reading_relation('1ab18ac8-e2a2-4fff-b408-fc9682cde7d7', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('1ab18ac8-e2a2-4fff-b408-fc9682cde7d7', foundational, clause_39_protects_feudal_peers_only).
narrative_ontology:cs_axiom_status(clause_39_protects_feudal_peers_only, holdable).
narrative_ontology:cs_axiom_grounding('1ab18ac8-e2a2-4fff-b408-fc9682cde7d7', clause_39_protects_feudal_peers_only, conventional).
narrative_ontology:cs_axiom('1ab18ac8-e2a2-4fff-b408-fc9682cde7d7', secondary, law_of_the_land_means_customary_feudal_law).
narrative_ontology:cs_axiom_status(law_of_the_land_means_customary_feudal_law, holdable).
narrative_ontology:cs_axiom_grounding('1ab18ac8-e2a2-4fff-b408-fc9682cde7d7', law_of_the_land_means_customary_feudal_law, conventional).
narrative_ontology:cs_reference_frame('1ab18ac8-e2a2-4fff-b408-fc9682cde7d7', feudal_constitutional_order_1215).
narrative_ontology:cs_drift_state('1ab18ac8-e2a2-4fff-b408-fc9682cde7d7', confirmatio_cartarum_1297, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('1ab18ac8-e2a2-4fff-b408-fc9682cde7d7', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, crown).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, tenants_in_chief).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, lesser_nobility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, royal_officials).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__feudal_prerogative_reading, feudal_contractualism).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__feudal_prerogative_reading, noble_privilege_against_royal_arbitrariness).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__feudal_prerogative_reading, customary_law_as_binding_on_king).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The king whose arbitrary power over noble persons and property is procedurally checked by Clause 39. The constraint requires lawful judgment by peers or law of the land before seizure or imprisonment of free men — but 'free men' in operational practice means the nobility. The crown retains structural authority over the realm, taxation, war-making, and justice administration; the constraint coordinates noble-crown relations rather than limiting royal sovereignty. Exit is arbitrage-grade: the crown can seek papal annulment (as John did in 1215), reissue with modifications, or ignore the constraint when power permits.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, crown, agenda_setter,
    institutional, generational, arbitrage, universal).

% The great barons who hold land directly from the crown. They gain a codified procedural right: the king cannot seize their persons or lands without judgment by their peers (other barons) or established law. This converts customary expectation into enforceable claim. Their exit is constrained by feudal obligation — they cannot leave the realm without forfeiting their tenure — but the constraint improves their bargaining position vis-à-vis the crown.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, tenants_in_chief, beneficiary,
    powerful, biographical, constrained, national).

% Knights and minor landholders who hold from tenants-in-chief or the crown. They gain the same procedural protection as the great barons — judgment by peers (local knights) or law of the land. Their exit is similarly constrained by land tenure and feudal service, but the constraint gives them a procedural shield against both royal and baronial arbitrariness.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, lesser_nobility, beneficiary,
    organized, biographical, constrained, national).

% The majority of legally 'free men' in 1215 England — merchants, artisans, yeomen, villeins with some freedoms — who lack the social standing to invoke 'judgment by peers' in the feudal sense. The constraint's text says 'no free man' but its operational peer-judgment mechanism only functions for those with peers in the feudal hierarchy. They are structurally excluded from the constraint's protection; their disputes remain in manorial or royal courts without the procedural guarantee. They would object to this exclusion if the liberal reading's universal claim were instantiated.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, free_men_excluded, excluded,
    powerless, biographical, trapped, national).

% Sheriffs, coroners, escheators, and royal judges whose discretionary enforcement of royal will is now procedurally bounded. They must observe the peer-judgment or law-of-the-land requirement before seizing noble persons or property. This is a modest cost — a procedural hurdle, not a substantive limitation. Their exit is constrained by royal appointment and patronage.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, royal_officials, payer,
    moderate, biographical, constrained, national).

% Analytical observers who trace Clause 39's textual history, reissues (1216, 1217, 1225, 1297), and doctrinal evolution from feudal privilege to universal due process. They see the full structure: the feudal reading's narrow operation, the liberal reading's expansive appropriation, and the originalist reading's restrictive recovery. They neither collect from nor pay into the constraint.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, legal_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates noble-crown relations by codifying customary procedural protections against arbitrary royal seizure of noble persons and property. Solves the collective-action problem among barons: individually they could not constrain the king; collectively, a written charter with peer-judgment enforcement creates a focal point for coordinated resistance to royal overreach.
% TRANSFER_FUNCTION: Moves a narrow procedural compliance burden from the nobility (who previously bore the risk of arbitrary seizure) to the crown (which must now observe judgment-by-peers or law-of-the-land before acting against nobles). The transfer is minimal — a procedural hurdle, not a substantive transfer of wealth or power.
% ABSENT_VOICES: The mass of 'free men' excluded from peer-judgment protection — merchants, yeomen, urban citizens, and those on the margins of feudal status. They are absent because the feudal hierarchy does not recognize them as having peers in the relevant sense. Also absent: the church (which secured its own separate guarantees in Clause 1) and the villein majority (unfree, outside 'free man' entirely). The liberal_due_process_reading would make these voices central; this reading structures them out.
% DISAPPEARANCE_RATIONALE: If Clause 39's feudal procedural guarantee vanished in 1215–1297, the crown's arbitrary power over noble persons and property would be unchecked by even the minimal procedural hurdle. Barons would revert to private warfare and papal appeal as primary checks; the coordination focal point for noble resistance would collapse. The feudal order would rearrange toward either royal absolutism or baronial fragmentation — the constraint's disappearance changes the strategic landscape.
% FOUNDING_PROBLEM: Arbitrary royal seizure of noble lands and persons without legal process — King John's use of disseisin, outlawry, and hostage-taking to coerce barons and extract revenue. The barons needed a collective focal point to constrain this pattern without destroying the feudal order itself.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary chronicles (Roger of Wendover, Matthew Paris) and the charter's own preamble attest to the founding problem from the baronial side. Royal records (Pipe Rolls, Close Rolls) corroborate the pattern of arbitrary disseisin and amercement from the crown's side. The problem is attested from both sides of the conflict — not only by the beneficiaries. The problem remained live through the 13th century (Henry III's reign saw repeated confirmations demanded by barons).
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.12 at interval end) because the constraint primarily codifies existing customary limits on royal power rather than imposing new substantive limits; the crown's extraction from the nobility (arbitrary seizure, dispossession) is modestly reduced. Suppression is low (0.18) because the constraint does not suppress alternatives — it operates within the feudal order, and the crown retains substantial discretionary power outside the narrow procedural channel. Theater ratio is moderate (0.25) because the constraint's high symbolic profile (the 'great charter') exceeds its narrow operational scope in 1215–1297. Accessibility collapse is very low (0.15) because the feudal order's alternatives (royal favor, private warfare, papal appeal) remain fully available. Resistance is moderate (0.35) because the crown actively resisted and sought papal annulment, but the constraint persisted through reissues and confirmations.
 *
 * DIRECTIONALITY LOGIC:
 *   The crown is the agenda-setter (sets the feudal order, administers justice) but also the primary target of the procedural check — directionality derivation places it near symmetric (d ≈ 0.5) because the constraint both preserves the crown's structural authority and extracts compliance on a narrow procedural point. Tenants-in-chief and lesser nobility are beneficiaries — they gain codified procedural protection without bearing costs; their exit options are constrained (feudal bonds, land tenure) but the constraint improves their position. Free men excluded from peer judgment are structurally excluded — they would be victims of the constraint's narrow scope if the liberal reading's universal claim were instantiated, but under this reading they are simply outside the constraint's operation. Royal officials are payers — their enforcement discretion is bounded, a modest cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The feudal prerogative reading does not resolve mandatrophy — the constraint's founding problem (arbitrary royal seizure of noble lands) was live in 1215 and remained live through the 13th century. The constraint was not a solution to a dead problem; it was a coordination mechanism for a live noble-crown conflict. The mandatrophy question applies to later doctrinal expansions (liberal reading) that may have outlived their founding context.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_feudal_prerogative,
    'Does Clause 39 instantiate a narrow feudal privilege constraint (this reading) or a universal due process guarantee (liberal_due_process_reading) or a specific historical limitation on documented royal abuses (originalist_limitation_reading)?',
    'Interpretive history: if subsequent legal practice and constitutional doctrine treat Clause 39 as the seed of universal due process, the feudal prerogative reading is a historical artifact; if it remains confined to noble-versus-crown disputes, this reading captures its operational reality.',
    'If the liberal reading is structurally instantiated, this constraint''s low extractiveness and narrow victim set misrepresent the constraint that actually operated — the true constraint would have broader scope, higher extraction against state power, and a universal victim set. The engine classifies the reading actually instantiated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_feudal_prerogative, conceptual, 'Which reading of the contested kernel Clause 39 is the structurally operative one — feudal prerogative, liberal due process, or originalist limitation.').

omega_variable(
    extraction_displacement_by_later_doctrine,
    'Did the feudal prerogative reading''s low extractiveness persist, or was it displaced by later doctrinal expansion that increased the constraint''s effective extraction against state authority?',
    'Longitudinal doctrinal analysis: trace whether Clause 39''s operational scope expanded from noble privileges to universal rights, and whether that expansion increased the constraint''s extraction from the crown/state (i.e., the constraint began extracting compliance from a wider range of state actions).',
    'If extractiveness increased over time, the feudal reading is a t0 snapshot of a constraint that drifted — the story''s base properties capture only the initial state, not the constraint''s full trajectory. Temporal measurements would be needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_displacement_by_later_doctrine, empirical, 'Whether the feudal prerogative reading''s low extractiveness is stable or a phase in a constraint that accumulated extraction through doctrinal expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 1215, 1297).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_tr_t1215, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1215, 0.3).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_tr_t1225, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1225, 0.28).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_tr_t1297, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1297, 0.25).

% Extraction over time
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_be_t1215, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1215, 0.08).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_be_t1225, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1225, 0.1).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_be_t1297, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1297, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_su_t1215, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1215, 0.15).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_su_t1225, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1225, 0.18).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_su_t1297, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1297, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__feudal_prerogative_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: three readings of the Magna Carta Clause 39 kernel. This reading (feudal_prerogative) is the historically earliest operational instantiation — low extractiveness, narrow scope, coordination function within feudal hierarchy. The liberal_due_process_reading is the downstream doctrinal expansion — higher extractiveness against state power, universal scope, rights-guarantee function. The originalist_limitation_reading is the restrictive historical interpretation — minimal extractiveness, narrowest scope, remedial function. All three share the same kernel_id; each is a separate constraint story linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_clause_39__feudal_prerogative_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
