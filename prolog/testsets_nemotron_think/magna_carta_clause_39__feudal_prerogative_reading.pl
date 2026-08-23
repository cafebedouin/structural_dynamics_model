% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__feudal_prerogative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Magna Carta Clause 39 — Feudal Prerogative Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   Magna Carta Clause 39 ('No free man shall be seized... except by the
 *   lawful judgment of his peers or by the law of the land') is the kernel.
 *   This reading — the feudal prerogative reading — treats the clause as a
 *   narrow procedural guarantee negotiated by the baronial elite to constrain
 *   King John's arbitrary justice while preserving the feudal hierarchy
 *   intact. 'Free men' means the warrior aristocracy (tenants-in-chief,
 *   knights); 'peers' means social equals within that hierarchy. The clause
 *   coordinates dispute resolution among peers, preventing blood feuds and
 *   royal caprice that threaten the feudal contract. The crown concedes it to
 *   secure loyalty; the elite peers gain predictable procedure. The unfree
 *   majority (villeins, serfs) are structurally excluded. Extraction is low
 *   (0.18) because the constraint operates as mutual obligation within the
 *   feudal order, not as a check on hierarchical authority per se. Theater
 *   rises over centuries as the clause becomes a symbolic totem rather than
 *   an operational procedural rule.
 *
 * KEY AGENTS:
 *   - crown: Primary agenda_setter (conceded charter, enforces through royal courts) — institutional/generational/arbitrage
 *   - elite_peers: Primary beneficiaries (barons, knights gain peer-judgment protection) — powerful/biographical/constrained
 *   - serfs_villeins: Excluded (no standing under Clause 39) — powerless/immediate/trapped
 *   - royal_courts: Administer the procedure — institutional/generational/analytical
 *   - church: Witness and guarantor of the charter — institutional/generational/analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.18).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.15).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Magna Carta Clause 39 — Feudal Prerogative Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, '40d1b37b-e643-46b6-b901-fd2c06a52a53').
narrative_ontology:cs_kernel_codification('40d1b37b-e643-46b6-b901-fd2c06a52a53', fixed_text).
narrative_ontology:cs_authority_grounding('40d1b37b-e643-46b6-b901-fd2c06a52a53', lineage).
narrative_ontology:cs_interpretation_layer_present('40d1b37b-e643-46b6-b901-fd2c06a52a53').
narrative_ontology:cs_reading_relation('40d1b37b-e643-46b6-b901-fd2c06a52a53', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('40d1b37b-e643-46b6-b901-fd2c06a52a53', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('40d1b37b-e643-46b6-b901-fd2c06a52a53', foundational, feudal_hierarchy_preserved).
narrative_ontology:cs_axiom_status(feudal_hierarchy_preserved, holdable).
narrative_ontology:cs_axiom_grounding('40d1b37b-e643-46b6-b901-fd2c06a52a53', feudal_hierarchy_preserved, conventional).
narrative_ontology:cs_axiom('40d1b37b-e643-46b6-b901-fd2c06a52a53', foundational, peer_judgment_narrow_scope).
narrative_ontology:cs_axiom_status(peer_judgment_narrow_scope, holdable).
narrative_ontology:cs_axiom_grounding('40d1b37b-e643-46b6-b901-fd2c06a52a53', peer_judgment_narrow_scope, conventional).
narrative_ontology:cs_reference_frame('40d1b37b-e643-46b6-b901-fd2c06a52a53', feudal_contract_1215).
narrative_ontology:cs_drift_state('40d1b37b-e643-46b6-b901-fd2c06a52a53', contemporary_constitutional_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('40d1b37b-e643-46b6-b901-fd2c06a52a53', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, crown).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, elite_peers).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__feudal_prerogative_reading, feudal_contract_mutual_obligation).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__feudal_prerogative_reading, peer_judgment_as_aristocratic_privilege).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conceded Clause 39 under baronial pressure in 1215; reissued it to secure loyalty. Enforces it through royal courts but interprets 'liber homo' and 'peers' narrowly to preserve royal prerogative over the unfree. Gains legitimacy and stabilized feudal order; loses arbitrary power over the baronage. Exit means ignoring the charter — but that risks renewed rebellion and loss of legitimacy.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, crown, agenda_setter,
    institutional, generational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, crown, beneficiary).

% Barons, tenants-in-chief, and knights who forced the charter. They invoke 'judgment by peers' to avoid royal courts where the king controls outcomes. Their protection is real but class-bound: they do not extend it to their own tenants. Exit from the feudal system is constrained — their identity, land tenure, and military obligation are fused with the hierarchy the clause preserves.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, elite_peers, beneficiary,
    powerful, biographical, constrained, national).

% The unfree majority (villeins, serfs, cottars) who comprise 70-80% of the population. They have no standing to invoke Clause 39; their disputes are heard in seigneurial courts controlled by their lords. They are structurally excluded from the clause's protections — not victimized by this constraint specifically, but by the feudal order it helps preserve. Their exit is trapped: bound to land, identity, and labor obligations.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, serfs_villeins, excluded,
    powerless, immediate, trapped, local).

% Administer justice under the charter's terms. Develop procedural forms (writs, assizes, juries) that partially satisfy 'lawful judgment' while retaining royal control. Over centuries, they absorb the clause's language into common law procedure, transforming peer judgment into jury trial. They are both enforcers and interpreters of the constraint.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, royal_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, royal_courts, observer).

% Witnessed and guaranteed the 1215 charter (Archbishop Langton). Provided the moral-theological framework for 'law of the land' as divine/natural law. Later, ecclesiastical courts operate parallel to royal courts, offering alternative forums. The church's role is supervisory and legitimating, not directly subject to the clause's procedural rule.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, church, observer,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides predictable dispute resolution among the warrior aristocracy, preventing blood feuds and royal caprice that destabilize the feudal contract. The peer-judgment procedure replaces private violence and arbitrary royal judgment with a structured, reciprocal process among social equals.
% TRANSFER_FUNCTION: Moves adjudicative authority from sole royal discretion to a peer-judgment procedure for a narrow class of free men. The transfer is not of resources but of decision-rights: the crown surrenders the power to judge barons arbitrarily; barons accept the crown's courts for lesser matters but claim peer judgment for matters of life, limb, and land.
% ABSENT_VOICES: The unfree majority (villeins, serfs, women of all classes) who comprise most of the population but have no standing under Clause 39. They are structurally excluded from its protections; their 'judgment' remains seigneurial and arbitrary. They would object to the clause's celebration as a liberty charter, but they are not in the conversation — then or now.
% DISAPPEARANCE_RATIONALE: If Clause 39 vanished overnight in 1215, the baronial rebellion would resume; royal justice would revert to pure discretion; blood feuds among the aristocracy would increase. The feudal contract would lose its key procedural stabilizer. In later centuries, its disappearance would unravel centuries of common law procedural development built on its language.
% FOUNDING_PROBLEM: Barons' rebellion against King John's arbitrary justice and financial exactions (arbitrary disseisin, excessive reliefs, wardship abuses, hostility to baronial courts). The barons needed a binding limit on royal power to secure their tenure and status; the crown needed to end the rebellion and retain the throne.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary chronicles (Roger of Wendover, Matthew Paris, Barnwell Chronicler) and the charter's own preamble attest to the baronial rebellion as the founding crisis. Modern historians (J.C. Holt, David Carpenter, Nicholas Vincent) corroborate the specific grievances from royal records, pipe rolls, and baronial charters. No serious historian disputes the 1215 crisis as the founding problem; the dispute is over the clause's subsequent meaning.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Low extractiveness (0.18) reflects that the constraint primarily coordinates intra-aristocratic relations; the crown loses arbitrary power over nobles but gains systemic stability. Suppression is minimal (0.15) — the clause creates a procedural path, it doesn't coerce compliance through force. Theater ratio starts near zero (genuine operational procedure in 1215) and rises as the clause becomes a constitutional symbol detached from feudal practice. Accessibility collapse is moderate (0.35) — alternative dispute mechanisms (royal courts, seigneurial courts) persist alongside peer judgment. Resistance is low (0.22) — the barons wanted this constraint; the crown conceded it under pressure but the arrangement serves mutual feudal interests.
 *
 * PERSPECTIVAL GAP:
 *   From the crown's seat (agenda_setter), the constraint is a costly but necessary concession to maintain feudal legitimacy — directionality moderate (d ~0.35). From elite_peers' seat (beneficiary), it is a hard-won protection against royal arbitrariness — directionality low (d ~0.15). From serfs_villeins' seat (excluded), the constraint is irrelevant to their condition — directionality analytical (d ~0.5). The engine computes these divergences from the structural data; the feudal reading's claim that the clause preserves hierarchy means the crown is not a pure target.
 *
 * DIRECTIONALITY LOGIC:
 *   The crown appears in beneficiaries because the feudal reading treats the charter as a mutual settlement: the crown trades arbitrary power for stabilized rule. Elite_peers are beneficiaries because they gain procedural security. No victims are declared because the constraint does not extract from a subordinated class — it coordinates among the ruling stratum. Serfs_villeins are excluded stakeholders, not victims of this constraint (their subordination rests on other constraints). Royal_courts and church are observers/guarantors. The override for crown (d=0.32) corrects the derivation which would otherwise treat any constrained party as a target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (baronial rebellion against John's arbitrary justice) is dead — the specific 1215 crisis is long resolved. Yet the clause persists because it was reissued, confirmed by statute, and mythologized. The feudal prerogative reading does not claim the clause solved a live problem today; it describes the clause's original structural function. The mandatrophy is not resolved — the constraint's form outlived its feudal function, but this reading treats that as a historical fact, not a defect. The liberal_due_process_reading is the one that claims living mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the feudal prerogative reading reflect genuine 13th-century understanding, or is it a retrospective narrowing constructed to oppose later liberal due process readings?',
    'Comparative analysis of 1215-1300 legal records, baronial charters, and royal court rolls to determine whether ''liber homo'' and ''per legem terre'' were understood narrowly (aristocratic peers) or broadly (all free men).',
    'If retrospective, the reading''s low extractiveness claim against traditional authority is a modern analytical artifact; if contemporary, the clause genuinely functioned as intra-aristocratic coordination with minimal extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the feudal prerogative reading is historically authentic or polemically constructed.').

omega_variable(
    scope_of_liber_homo,
    'What population did ''liber homo'' (free man) actually cover in 1215 practice — only tenants-in-chief and knights, or a broader class including free tenants and burgesses?',
    'Domesday Book cross-referencing, Hundred Rolls analysis, and plea roll evidence for who actually invoked Clause 39 protections in 13th-century courts.',
    'A narrow scope (barons/knights only) confirms the feudal prerogative reading''s restricted beneficiary set; a broader scope pushes toward the originalist_limitation_reading''s intermediate position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_liber_homo, empirical, 'Historical scope of the clause''s protected class.').

omega_variable(
    enforcement_reality_13th_century,
    'Was Clause 39 actually enforced against the crown in the 13th century, or did it remain aspirational until later reissues and statutory confirmations?',
    'Case law survey of 1215-1300: instances where royal courts honored peer-judgment demands versus cases where the crown overrode them.',
    'If unenforced, the clause''s coordination function was theoretical and its extractiveness near zero (pure theater); if enforced, it imposed real procedural costs on royal authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_reality_13th_century, empirical, 'Whether the constraint had operational teeth in its founding era.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_tr_t0, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_tr_t50, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_tr_t200, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_tr_t400, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 400, 0.25).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_tr_t600, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 600, 0.35).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_tr_t800, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 800, 0.42).

% Extraction over time
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_be_t0, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_be_t50, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_be_t200, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 200, 0.22).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_be_t400, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 400, 0.25).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_be_t600, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 600, 0.28).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_be_t800, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 800, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_su_t0, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_su_t50, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_su_t200, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 200, 0.18).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_su_t400, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 400, 0.2).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_su_t600, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 600, 0.15).
narrative_ontology:measurement(magna_carta_clause_39__feudal_prerogative_reading_su_t800, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 800, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% This constraint is the feudal_prerogative_reading of kernel magna_carta_clause_39. The liberal_due_process_reading treats the same text as establishing universal procedural rights; the originalist_limitation_reading treats it as addressing only documented 1215 abuses. All three share the fixed text kernel but instantiate different constraints with different beneficiary/victim structures, extractiveness, and claimed types. This reading has the lowest extractiveness against traditional authority and the narrowest beneficiary set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_clause_39__feudal_prerogative_reading, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
