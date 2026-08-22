% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__originalist_limitation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__originalist_limitation_reading, []).

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
 *   constraint_id: magna_carta_clause_39__originalist_limitation_reading
 *   human_readable: Clause 39 Originalist Reading: Limited Royal Abuse Constraint
 *   domain: constitutional_law/legal_history
 *
 * SUMMARY:
 *   This constraint story instantiates the ORIGINALIST READING of Magna Carta
 *   Clause 39 (1215): 'No man shall be arrested or imprisoned except by the
 *   lawful judgment of his peers or by the law of the land.' Under this
 *   reading, Clause 39 limits ONLY the specific royal abuses King John
 *   committed and the barons documented—arbitrary feudal reliefs, wardship
 *   exactions, and detention without judgment. It does not establish
 *   universal individual rights against state power, nor does it challenge
 *   the feudal hierarchy itself. The barons seek to constrain the king's
 *   arbitrary exercise of incidents they accepted as his prerogative, within
 *   the framework of feudal law. The originalist reading bounds the
 *   constraint to 13th-century grievances and 13th-century beneficiaries (the
 *   baronial consortium), making it a coordination mechanism within feudalism
 *   rather than a universal charter. The claim/metric gap reflects the
 *   deliberate independence of authored type and authored metrics: this
 *   reading CLAIMS the constraint as rope (genuine coordination solving a
 *   documented problem), while the authored metrics describe moderate
 *   extractiveness and declining suppression requirement—the engine computes
 *   whether the metrics support the claim; divergence is the signal.
 *
 * KEY AGENTS:
 *   - Baronial consortium (1215): organized, powerful negotiators who extracted Clause 39 from superior military position; beneficiary of procedural constraint on royal feudal incidents
 *   - King John (agent of royal prerogative): institutional power, constrained at specific documented abuse points but retaining authority to rule, tax, and command military service within customary bounds
 *   - Peasant population: powerless, excluded from the constraint's protections; gains no benefit; remains subject to feudal lords' authority
 *   - Later reinterpreters (17th-century Coke, 19th-century radicals): analytical observers of this reading; they instantiate different constraints by claiming universal rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.38).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.25).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Clause 39 Originalist Reading: Limited Royal Abuse Constraint").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional_law/legal_history").

domain_priors:requires_active_enforcement(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, '6c579f79-e5e3-416f-858d-b47da480129d').
narrative_ontology:cs_kernel_codification('6c579f79-e5e3-416f-858d-b47da480129d', fixed_text).
narrative_ontology:cs_authority_grounding('6c579f79-e5e3-416f-858d-b47da480129d', lineage).
narrative_ontology:cs_interpretation_layer_present('6c579f79-e5e3-416f-858d-b47da480129d').
narrative_ontology:cs_reading_relation('6c579f79-e5e3-416f-858d-b47da480129d', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c579f79-e5e3-416f-858d-b47da480129d', magna_carta_clause_39__liberal_due_process_reading, influences).
narrative_ontology:cs_axiom('6c579f79-e5e3-416f-858d-b47da480129d', foundational, clause_39_bounded_to_documented_grievances).
narrative_ontology:cs_axiom_status(clause_39_bounded_to_documented_grievances, holdable).
narrative_ontology:cs_axiom_grounding('6c579f79-e5e3-416f-858d-b47da480129d', clause_39_bounded_to_documented_grievances, conventional).
narrative_ontology:cs_axiom('6c579f79-e5e3-416f-858d-b47da480129d', foundational, feudal_hierarchy_natural_and_legitimate).
narrative_ontology:cs_axiom_status(feudal_hierarchy_natural_and_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('6c579f79-e5e3-416f-858d-b47da480129d', feudal_hierarchy_natural_and_legitimate, deontological).
narrative_ontology:cs_reference_frame('6c579f79-e5e3-416f-858d-b47da480129d', baronial_procedural_constraint_on_feudal_incidents).
narrative_ontology:cs_drift_state('6c579f79-e5e3-416f-858d-b47da480129d', early_modern_liberal_reinterpretation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6c579f79-e5e3-416f-858d-b47da480129d', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, baronial_consortium_1215).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, king_john_unconstrained_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A coalition of Norman and Anglo-Saxon magnates who negotiated Magna Carta with King John in 1215 to secure specific procedural rights against documented royal abuses: arbitrary imprisonment, excessive feudal dues, control of wardships, and extortionate relief fees. They bargained from superior military position (civil war; London in their hands). The constraint secures their property rights and procedural standing in a feudal hierarchy they accept as natural.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, baronial_consortium_1215, beneficiary,
    organized, generational, constrained, national).

% King John's prerogative to rule without procedure is constrained at specific documented abuse points: he can no longer impose arbitrary reliefs, deny wardships to favored barons, or imprison without judgment. The constraint does not challenge his authority to rule, to tax, or to command military service — only the arbitrary exercise of specific feudal incidents and detentions that the barons documented as grievances.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, king_john_unconstrained_authority, payer,
    institutional, biographical, trapped, national).

% The framework of feudal tenure, royal prerogative, and customary law within which Clause 39 operates. It is not an actor but the institutional substrate that defines what 'abuse' means — violation of established precedent, not violation of universal principle.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, royal_feudal_system, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(magna_carta_clause_39__originalist_limitation_reading, royal_feudal_system).

% The majority of England's population holds no standing in the constraint and gains no benefit from it. Magna Carta protects baronial property and feudal incident; it does not establish rights for the unfree or constrain the lord's authority over villein tenants. Their exclusion is structural, not incidental.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, peasant_population, excluded,
    powerless, biographical, trapped, national).

% Readers from the 17th century onward (Coke, Foxe, radical reformers) who reinterpret Clause 39 as establishing universal individual rights against arbitrary power, not merely baronial procedural protections. They are analytical observers of the originalist constraint, not parties to it; their reinterpretations instantiate different constraints (the liberal_due_process_reading).
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, later_commonist_readers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a narrow procedural boundary: the king commits to judgment-based imprisonment and customary-fee-based feudal incidents, abandoning arbitrary detentions and arbitrary exactions. This solves the baronial coordination problem (how to enforce the king's observance of precedent without permanent civil war) by creating documented, repeatable commitments.
% TRANSFER_FUNCTION: The barons transfer military submission and renewed homage in exchange for the king's transfer of procedural constraint on feudal incidents. Loyalty flows from barons to king; security of property and procedural right flow from king to barons. The transfer is within the feudal hierarchy, not a democratization of rights.
% ABSENT_VOICES: Peasants, villeins, merchants, and the unfree population have no seat at the table. They would have no standing to object—the constraint explicitly excludes them from its protections. The Church initially negotiates separately (ecclesiastical freedom) but is largely addressed in Clause 1, a parallel provision. Later readers (17th-century radicals, 19th-century legal reformers) would object to the originalist reading itself, claiming Clause 39 establishes universal rights; they are absent from the 1215 context.
% DISAPPEARANCE_RATIONALE: If Clause 39 and its originalist limitation vanished in 1215—if the barons extracted the constraint but could not enforce it and John repudiated it—the baronial coalition would dissolve, civil war would resume, and the king would revert to arbitrary feudal exaction. The constraint is the settlement that ends the immediate conflict; without it, the military standoff persists and England fragments into renewed baronial rebellion and royal counter-assertion.
% FOUNDING_PROBLEM: King John's arbitrary exercise of feudal incidents: reliefs of excessive amounts, wardships sold to the highest bidder, relief fees extracted as extortion rather than customary right, arbitrary imprisonment without judgment. The barons documented these practices as violations of established precedent and demanded a written charter reaffirming the limits of royal prerogative.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by baronial representatives who documented John's abuses before the negotiation (monastic chroniclers like Roger of Wendover record the complaints; royal records confirm the reliefs and wardships transactions). Later observers (Roger Bacon, judicial records from Henry III's reign) attest that the problem of arbitrary feudal exaction persisted even after Magna Carta's promulgation, requiring repeated reissues and enforcement. The original baronial framing is corroborated by external witnesses (clergy, chroniclers) and by the constraint's own re-negotiation history.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__originalist_limitation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__originalist_limitation_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__originalist_limitation_reading_tests).
:- end_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the constraint imposes a genuine procedural cost on the king—he must follow judgment-based procedures and customary feudal fees rather than arbitrary exaction—but the cost is bounded to documented abuses and the barons accept the feudal hierarchy as legitimate. Suppression requirement declines over the interval (from 0.70 to 0.25) because enforcement mechanisms shift: initially, the constraint must be backed by military threat (baronial coalition in arms), but as it becomes customary and is reissued and reaffirmed (Henry III era), it becomes embedded in royal practice and requires less active enforcement. Theater ratio remains low (0.08–0.16) because the constraint's function—limiting arbitrary feudal exaction—remains stable; performative reaffirmation is modest. The measurement series track suppression_requirement decline as the constraint normalizes into the feudal order, not as a sign of coordinate strength but as evidence that enforcement capacity (military threat, renewal necessity) decays as custom sediments.
 *
 * PERSPECTIVAL GAP:
 *   From the baronial seat, the constraint is a negotiated coordination mechanism: they used military force to compel the king to document and accept procedural limits on feudal incidents, solving the problem of arbitrary exaction within the feudal hierarchy. From the king's seat, it is a constraint imposed by rebel magnates, reducing his prerogative at the documented points but leaving his authority to rule intact. From the peasant seat (excluded), it is entirely irrelevant—their subordination is not limited by Clause 39. From the later liberal reader's seat, the same text means something entirely different: universal individual rights against arbitrary power, a reading that instantiates a different constraint (the liberal_due_process_reading). The originalist reading anchors its meaning to the 1215 context and the documented grievances; the liberal reading universalizes and abstracts. The engine computes these divergences from the structural data; the reading frame (originalist vs. liberal) determines the ε referent.
 *
 * DIRECTIONALITY LOGIC:
 *   The baronial consortium sits as beneficiary (d near 0.0): they extract procedural constraint on the king's arbitrary power without bearing the cost of enforcement—the king must constrain himself. The king's prerogative sits as the target (d near 1.0): he is constrained at the specific documented points and cannot exercise arbitrary feudal incidents. The peasant population sits at d=1.0 (full target of subordination to feudal authority) but is excluded from the constraint's protections—this is structural asymmetry the engine captures via the excluded role. The royal feudal system (not an actor, agent=false) is the institutional substrate that defines what 'abuse' means—violation of established precedent, not violation of abstract right.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does NOT apply to this reading. The founding problem (King John's documented arbitrary exactions) remains live throughout the interval: later reissues of Magna Carta (1217, 1225) and Henry III's reign show barons repeatedly re-negotiating the same constraint because the problem persists. The constraint is not zombified—it continues to solve the original coordination problem (how to bind the king to procedural limits on feudal incidents) even as enforcement mode shifts from military threat to customary practice. The originalist reading avoids the mandatrophy trap that ensnares the liberal reading: the liberal reading claims Clause 39 established universal rights against arbitrary power, a goal that becomes obsolete as democratic constitutions and legal processes emerge (mandate dies), making reaffirmations of Clause 39 theatric. The originalist reading claims only limited procedural constraint on documented feudal abuses, a narrower goal that remains live and embedded in the feudal order.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_vs_living_charter_boundary,
    'Is Clause 39 a fixed instrument limited to documented 1215 grievances, or a living text that evolves to address each era''s arbitrary-power problem?',
    'Hermeneutic analysis of actual reissues and reinterpretations: if barons and kings in 1217, 1225, Henry III''s reign applied Clause 39 only to feudal incidents, originalism holds; if later interpreters (Coke, 17th-century radicals) stretched it to new contexts and it became accepted, the text became living.',
    'If living, the constraint''s victim set and extractiveness expand over time (later readings instantiate more extractive constraints); if originalist, the constraint remains bounded and extractiveness tracks only feudal-incident disputes. This omega locates the committer contest between originalist and liberal readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalist_vs_living_charter_boundary, conceptual, 'Whether Clause 39 is bounded to 1215 context or evolves with interpretation.').

omega_variable(
    procedural_vs_substantive_right,
    'Does Clause 39 guarantee a substantive right (not to be imprisoned arbitrarily) or merely a procedural right (to judgment before imprisonment)?',
    'Textual analysis and practice history: if ''judgment'' means trial by peers with evidentiary standards, it is substantive; if ''judgment'' means any pronouncement by the king claiming legal basis, it is merely formal procedure.',
    'A substantive reading makes the constraint more protective and more extractive of the king''s prerogative; a procedural reading allows the king to declare arbitrary exaction ''lawful'' and maintain substantial authority. The originalist reading emphasizes procedure (judgment according to feudal custom) rather than substantive limitation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_vs_substantive_right, conceptual, 'Scope of protection: procedural formality or substantive limitation.').

omega_variable(
    feudal_hierarchy_legitimacy,
    'Is the feudal hierarchy natural and legitimate (as the originalist reading assumes), or is it itself an arbitrary power structure whose victims deserve protection?',
    'Meta-ethical analysis: if feudalism is accepted as natural order, Clause 39 legitimately protects only within it; if feudalism is constructed extraction, Clause 39''s confinement to feudal disputes is itself an injustice.',
    'If feudalism is legitimate, the originalist reading is defensible and the constraint is genuinely cooperative coordination within the natural order. If feudalism is extractive, the originalist reading is complicit in excluding peasants and legitimizing hierarchy; the liberal reading (which could be extended to peasant rights) is more historically just.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_hierarchy_legitimacy, preference, 'Whether the feudal substrate is legitimate or itself the extraction target.').

omega_variable(
    committer_contest_originalist_vs_liberal,
    'Which kernel reading—originalist limitation or liberal universalization—faithfully instantiates the 1215 text and intent?',
    'Historical exegesis and record: evidence from baronial intent, contemporary chroniclers, and the constraint''s own reissue language; comparison with how participants in 1217 and 1225 reissues understood Clause 39.',
    'If originalist reading is correct, Clause 39 is a narrow feudal coordination mechanism and has been reinterpreted beyond its intent by later radicals; the engine classifies it as rope within feudalism. If liberal reading is closer to original intent (a claim few historians make), or if intent becomes less binding than reinterpretation history, the constraint shifts toward snare-like extractiveness as it is weaponized to exclude whole populations from its protections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_contest_originalist_vs_liberal, empirical, 'The core kernel contest: what did Clause 39 mean to its framers and what has it become?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(magn_tr_t0, observed).
narrative_ontology:measurement(magn_tr_t10, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(magn_tr_t10, observed).
narrative_ontology:measurement(magn_tr_t20, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(magn_tr_t20, observed).
narrative_ontology:measurement(magn_tr_t30, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(magn_tr_t30, observed).
narrative_ontology:measurement(magn_tr_t50, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement_basis(magn_tr_t50, observed).
narrative_ontology:measurement(magn_tr_t75, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 75, 0.16).
narrative_ontology:measurement_basis(magn_tr_t75, observed).
narrative_ontology:measurement(magn_tr_t100, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement_basis(magn_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(magn_be_t0, observed).
narrative_ontology:measurement(magn_be_t10, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(magn_be_t10, observed).
narrative_ontology:measurement(magn_be_t20, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(magn_be_t20, observed).
narrative_ontology:measurement(magn_be_t30, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement_basis(magn_be_t30, observed).
narrative_ontology:measurement(magn_be_t50, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement_basis(magn_be_t50, observed).
narrative_ontology:measurement(magn_be_t75, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 75, 0.45).
narrative_ontology:measurement_basis(magn_be_t75, observed).
narrative_ontology:measurement(magn_be_t100, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement_basis(magn_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(magn_su_t0, observed).
narrative_ontology:measurement(magn_su_t10, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(magn_su_t10, observed).
narrative_ontology:measurement(magn_su_t20, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(magn_su_t20, observed).
narrative_ontology:measurement(magn_su_t30, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(magn_su_t30, observed).
narrative_ontology:measurement(magn_su_t50, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement_basis(magn_su_t50, observed).
narrative_ontology:measurement(magn_su_t75, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 75, 0.32).
narrative_ontology:measurement_basis(magn_su_t75, observed).
narrative_ontology:measurement(magn_su_t100, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 100, 0.25).
narrative_ontology:measurement_basis(magn_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__originalist_limitation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__originalist_limitation_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__feudal_prerogative_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__liberal_due_process_reading).

% DUAL FORMULATION NOTE:
% Magna Carta Clause 39 kernel has three structurally distinct instantiations: (1) feudal_prerogative_reading — Clause 39 as narrower reaffirmation of pre-existing feudal custom, focusing on continuity and hierarchy preservation; (2) originalist_limitation_reading (this story) — Clause 39 as limited response to documented 1215 grievances, procedural constraint on feudal incidents, moderate extractiveness; (3) liberal_due_process_reading — Clause 39 as foundational universal rights against arbitrary state power, substantially extractive of prerogative when universalized. The three readings share the same kernel text but instantiate different constraints with different ε values, victim sets, and types. Network edges link them: originalist reading influences both siblings by providing the textual anchor they reinterpret; feudal reading coexists with originalist (both accept feudal hierarchy); originalist coexists with liberal (both cite Clause 39 but with opposite universalizing intent). Each story carries omega variables documenting the interpretive contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
