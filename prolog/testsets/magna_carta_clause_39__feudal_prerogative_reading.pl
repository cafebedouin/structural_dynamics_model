% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__feudal_prerogative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Magna Carta Clause 39 – Feudal Prerogative Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   Clause 39 of Magna Carta (1215, restated 1217 onwards) states: 'No free
 *   man shall be seized or imprisoned, or stripped of his rights or
 *   possessions, except by the lawful judgment of his equals or by the law of
 *   the land.' This constraint — the feudal prerogative reading — interprets
 *   Clause 39 within the framework of feudal legitimacy: the clause
 *   coordinates the procedures by which crown and barons exercise their
 *   respective hierarchical prerogatives. The clause does not challenge the
 *   feudal order itself; it presupposes it. 'Free men' means feudal tenants
 *   and landholders; 'judgment of his equals' means judgment by peers of
 *   similar feudal rank; 'law of the land' means the customary feudal law
 *   governing property and status. The clause protects feudal prerogative —
 *   the crown's right to govern, the barons' right to property and
 *   jurisdiction — by establishing procedures rather than challenging the
 *   order. This reading coexists with two siblings: the liberal due process
 *   reading (which abstracts the principle of procedural protection from
 *   feudal status and applies it universally to all persons) and the
 *   originalist limitation reading (which says the clause applies to the
 *   named classes — free men, those with property — but does not imply
 *   universal extension). The feudal prerogative reading is the historically
 *   conservative one: it treats Clause 39 as an internal refinement of feudal
 *   governance, not as a stepping stone to liberal constitutionalism.
 *
 * KEY AGENTS:
 *   - Crown: Primary beneficiary (institutional/arbitrage) — gains certainty about procedures nobles will accept; retains ultimate prerogative
 *   - Baronial Nobility: Primary beneficiary (institutional/arbitrage) — gains assurance against arbitrary crown seizure; able to renegotiate terms
 *   - Feudal Landholders: Secondary beneficiary (powerful/constrained) — gain procedural protection within feudal property order; constrained by feudal hierarchy itself
 *   - Unfree Peasantry and Serfs: Structurally excluded (powerless/trapped) — Clause 39 does not apply; exist outside the legal framework of 'free men'
 *   - Church and Urban Guilds: Marginal beneficiaries (organized/constrained) — benefit from procedural predictability but subordinated to feudal frame
 *   - Feudal Prerogative Analyst: Reconstructs the reading (analytical/analytical) — observer of the feudal frame's internal coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.18).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.35).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Magna Carta Clause 39 – Feudal Prerogative Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional_law/legal_history/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, '55191d70-0eba-4b1d-b2ef-eb271982227f').
narrative_ontology:cs_kernel_codification('55191d70-0eba-4b1d-b2ef-eb271982227f', fixed_text).
narrative_ontology:cs_authority_grounding('55191d70-0eba-4b1d-b2ef-eb271982227f', lineage).
narrative_ontology:cs_interpretation_layer_present('55191d70-0eba-4b1d-b2ef-eb271982227f').
narrative_ontology:cs_reading_relation('55191d70-0eba-4b1d-b2ef-eb271982227f', magna_carta_clause_39__liberal_due_process_reading, forecloses).
narrative_ontology:cs_reading_relation('55191d70-0eba-4b1d-b2ef-eb271982227f', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('55191d70-0eba-4b1d-b2ef-eb271982227f', foundational, feudal_hierarchy_is_legitimate_ordering).
narrative_ontology:cs_axiom_status(feudal_hierarchy_is_legitimate_ordering, holdable).
narrative_ontology:cs_axiom_grounding('55191d70-0eba-4b1d-b2ef-eb271982227f', feudal_hierarchy_is_legitimate_ordering, conventional).
narrative_ontology:cs_axiom('55191d70-0eba-4b1d-b2ef-eb271982227f', foundational, procedural_protection_calibrated_to_feudal_rank).
narrative_ontology:cs_axiom_status(procedural_protection_calibrated_to_feudal_rank, holdable).
narrative_ontology:cs_axiom_grounding('55191d70-0eba-4b1d-b2ef-eb271982227f', procedural_protection_calibrated_to_feudal_rank, conventional).
narrative_ontology:cs_reference_frame('55191d70-0eba-4b1d-b2ef-eb271982227f', feudal_prerogative_authority).
narrative_ontology:cs_drift_state('55191d70-0eba-4b1d-b2ef-eb271982227f', contemporary_post_liberal_constitutionalism, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('55191d70-0eba-4b1d-b2ef-eb271982227f', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, crown_prerogative).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, noble_peers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CROWN AND BARONIAL NOBILITY (ROPE) — This reading sees Clause 39 as coordination among peers of roughly equal institutional power. The crown and barons are negotiating the terms of their mutual accountability within an established feudal hierarchy. The clause coordinates their respective prerogatives: the crown gains certainty about the procedures the barons will accept; the barons gain assurance about the crown's commitment to those procedures. Both benefit from clarity. No victim set — this is pure coordination among consenting hierarchical elites. Exit options reflect institutional actors with significant room for renegotiation (arbitrage). Experienced extraction is minimal because the clause structures mutual benefit.
constraint_indexing:constraint_classification(magna_carta_clause_39__feudal_prerogative_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: FEUDAL LANDHOLDING CLASS (ROPE) — Landholders with significant power (though less than crown/barons) see Clause 39 as establishing procedural rules that protect their status against arbitrary crown seizure. The constraint coordinates expectations about due process within the feudal property order. Extraction is minimal — the clause actually benefits these actors by constraining crown arbitrariness. Suppression is low because the clause gives explicit procedural guarantees.
constraint_indexing:constraint_classification(magna_carta_clause_39__feudal_prerogative_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNFREE PEASANTRY AND COMMONS (MOUNTAIN) — Serfs and unfree persons are structurally excluded from Clause 39. The clause does not apply to them; they have no procedural rights to 'due process' in any meaningful sense. From this perspective, the constraint appears as a natural law of their social world: the feudal order is immutable, procedural protections are for the free and propertied, and the unfree exist outside the legal framework entirely. High suppression and accessibility collapse — escape from serfdom is nearly impossible, and the legal world where Clause 39 applies is fundamentally inaccessible. This is a mountain, but it is a false summit: the naturalization of feudal exclusion as legal necessity masks the active institutional choice to exclude the unfree from procedural protection.
constraint_indexing:constraint_classification(magna_carta_clause_39__feudal_prerogative_reading, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: FEUDAL PREROGATIVE ANALYST (ROPE) — The analyst reconstructing this reading in its original feudal context sees Clause 39 as a genuine coordination mechanism among hierarchical peers. Within the feudal frame — accepting hierarchy as legitimate — the clause solves a real collective action problem: how can the crown and barons establish mutually acceptable procedures without ceding or surrendering legitimate prerogative? The rope classification reflects that this is, within its frame, a pure coordination device with minimal extraction. The analyst's exit options are analytical because they are not participants in the feudal order but observers of its internal logic.
constraint_indexing:constraint_classification(magna_carta_clause_39__feudal_prerogative_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: CHURCH AND URBAN MERCHANT GUILDS (TANGLED ROPE) — Organized institutional actors (Church, emerging merchant guilds, urban communes) occupy an ambiguous position in the feudal prerogative reading. They are not primary feudal lords but have significant institutional autonomy. Clause 39 creates procedural predictability that benefits these actors' institutional planning, but they also experience some suppression: the clause's procedures are still designed around feudal hierarchies and property rights, not commercial transactions or ecclesiastical authority. This is tangled rope — genuine coordination benefit coupled with subordination to a hierarchy not designed around their interests. Suppression reflects the constraint of operating within a feudal framework.
constraint_indexing:constraint_classification(magna_carta_clause_39__feudal_prerogative_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magna_carta_clause_39__feudal_prerogative_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magna_carta_clause_39__feudal_prerogative_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. Within the feudal frame, Clause 39 is a genuine coordination mechanism among hierarchical peers. The crown and barons both benefit from procedural clarity. The modest extractiveness reflects that even within this frame, there is some subordination of baronial interest to crown prerogative (the crown retains ultimate authority to interpret 'law of the land'), but this is not experienced as extraction per se — it is the normal operation of feudal hierarchy. From outside the feudal frame (the peasant or modern perspective), the constraint appears differently. Suppression (0.35): Moderate. Within the feudal framework, procedural protections exist for free men — a significant class that includes landholders and merchants. But suppression is real: the procedures still operate within feudal hierarchy; exit from that hierarchy is structurally impossible; and the unfree — a large portion of the population — are entirely excluded. The suppression reflects both the feudal hierarchy itself and the limited scope of protection. Theater ratio (0.52): Moderate. Procedural protections in feudal contexts involve ritual and ceremony (ordeals, oaths, formal assemblies) but these have genuine functional role: they coordinate behavior and signal commitment. The theater is neither negligible (pure performance) nor minimal (purely functional) — it is typical of medieval legal procedure where legitimacy derives partly from proper form.
 *
 * PERSPECTIVAL GAP:
 *   The feudal prerogative reading produces a dramatic perspectival gap. From the perspective of crown and barons (institutional/arbitrage, immediate or biographical time), the constraint is pure coordination (Rope) — it solves a genuine collective action problem about procedure. From the perspective of the analytical observer reconstructing the feudal frame (analytical/analytical, civilizational time), the constraint is also Rope — internally coherent coordination. But from the perspective of the unfree peasantry (powerless/trapped, civilizational time), the constraint appears as Mountain — a fixed, immutable, natural-law ordering where procedural rights are structurally inaccessible. The mountain perspective is a false summit: the feudal order is naturalized as inevitable, but the structural data shows it as contingent institutional choice. The perspective of Church and guilds (organized/constrained) is Tangled Rope — they benefit from some coordination but are subordinated to a feudal frame not designed around their interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this reading varies dramatically by agent. The crown and barons occupy positions where d ≈ 0.50 (symmetric coordination — roughly equal costs and benefits from procedural clarity). Feudal landholders experience d ≈ 0.40 (moderate extraction — they benefit from protection but are subordinated to hierarchy). The unfree peasantry experience d ≈ 0.95 (maximum extraction — entirely excluded from legal protection). The analytical observer experiences d ≈ 0.72 (canonical analytical value). The Church and guilds experience d ≈ 0.55 (slightly toward victim, as they are partially subordinated). The low overall extractiveness (0.18) reflects the weighted average across a beneficiary-heavy population within the feudal frame (crown, barons, landholders) and the analytical frame (which reconstructs the feudal frame's coherence). From outside the feudal frame — from a modern legal perspective that rejects feudal hierarchy — the extractiveness would be much higher because the entire structure would appear as suppression of human dignity and freedom.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by accepting the feudal frame as legitimate and coherent. Within that frame, Clause 39 is pure coordination (Rope) — no ambiguity. The mandatrophy arises only when we attempt to apply this reading to modern constitutional contexts or to evaluate it from perspectives outside the feudal frame. A modern lawyer or citizen reading Clause 39 through the feudal prerogative lens would find it inadequate as due process — it protects only 'free men' and only from arbitrary seizure, not from substantive injustice. The liberal reading resolves the mandatrophy by discarding the feudal frame and reconstructing the principle as universally applicable. The originalist reading resolves it by focusing on textual constraints without endorsing or rejecting the feudal frame. No single reading captures all dimensions; the presheaf of readings over the observation site (feudal frame, liberal frame, originalist frame, modern frame) is the full answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feudal_hierarchy_legitimacy,
    'Is the feudal hierarchy itself a legitimate ordering principle grounding Clause 39, or is hierarchical organization itself a contingent institutional choice that should be subject to procedural question?',
    'Comparative historical analysis: do non-feudal or post-feudal societies develop alternative procedural frameworks that DO apply to all persons? If yes, then hierarchy is shown to be contingent, not necessary. If feudal procedures are the only possible form of due process, then hierarchy is foundational.',
    'If hierarchy is contingent: the feudal prerogative reading is exposed as one historical instantiation, and the liberal due process reading (which abstracts procedural protection from feudal status) becomes structurally coherent. If hierarchy is necessary: the feudal reading stands as the true articulation of due process within legitimate order.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feudal_hierarchy_legitimacy, empirical, 'Whether feudal hierarchy is a necessary or contingent ordering principle').

omega_variable(
    clause_39_intent_scope,
    'Did the drafters of Magna Carta Clause 39 intend procedural protection to apply only to the feudal elite, or was there latent intent to extend beyond the initial beneficiary class?',
    'Textual analysis of Clause 39 and related charter clauses; historical record of charter reissues and revisions; evidence from contemporary commentary by legal theorists or scribes.',
    'If intent was elite-only: feudal prerogative reading is the accurate historical reading. If latent universalizing intent: the originalist limitation reading (which says the charter applies to its named classes) versus the liberal reading (which argues principle of protection generalizes) both have historical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clause_39_intent_scope, empirical, 'Scope of intended application of Clause 39 protections').

omega_variable(
    prerogative_vs_arbitrariness_boundary,
    'Is a procedure that requires ''judgment of peers'' but explicitly exempts the crown from those same procedures meaningfully different from crown arbitrariness?',
    'Structural analysis: does the requirement of peer judgment actually constrain crown action, or does the crown''s exemption allow it to dismiss peer judgment? Historical evidence of enforcement: were crowns actually constrained by Clause 39 procedures, or did they routinely evade them?',
    'If peer judgment is effective constraint: feudal prerogative reading is coherent (mutual accountability among elites). If crown exemption is loophole: the clause is nominally protective but structurally extractive (tangled rope or snare from peasant perspective; rope disguising suppression from elite perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prerogative_vs_arbitrariness_boundary, empirical, 'Whether peer judgment requirement constrains crown prerogative').

omega_variable(
    reading_committer_identity,
    'Which historical and contemporary actors hold the feudal prerogative reading as their authoritative interpretation, and what institutional interests does that reading serve?',
    'Historical survey of jurisprudence: which legal schools, judges, political theorists explicitly defend the feudal prerogative frame? Contemporary institutional analysis: what modern regimes or constitutional traditions inherit or defend restricted-scope due process?',
    'If feudal prerogative reading is actively held by identifiable contemporary actors with institutional power: the reading remains live and contested. If the reading is historically superseded (held only in historical reconstruction, not in live constitutional frameworks): the reading''s status shifts from active principle to historical artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_identity, empirical, 'Contemporary institutional holders of the feudal prerogative reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc39fp_tr_t0, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mc39fp_tr_t100, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 100, 0.52).
narrative_ontology:measurement(mc39fp_tr_t200, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 200, 0.55).

% Extraction over time
narrative_ontology:measurement(mc39fp_be_t0, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(mc39fp_be_t100, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 100, 0.18).
narrative_ontology:measurement(mc39fp_be_t200, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 200, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% The three Magna Carta Clause 39 readings are not different measurements of one constraint — they are structurally distinct constraints grounded in different axioms about the legitimacy of hierarchy and the scope of procedural protection. The feudal prerogative reading (this file, ε≈0.18) accepts feudal hierarchy and protects elite prerogative. The liberal reading (sibling, ε≈0.45) rejects feudal hierarchy and protects universal rights. The originalist reading (sibling, ε varies by application) focuses on textual boundaries. Each has distinct beneficiary/victim sets and distinct mechanisms. They are linked via network.affects_constraints to show that historical change in how Clause 39 is read reflects structural changes in which reading's underlying axioms are accepted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_clause_39__feudal_prerogative_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
