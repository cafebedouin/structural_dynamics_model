% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   Magna Carta 1215 under the baronial privilege reading: a feudal contract
 *   between King John and his rebellious barons that limits royal prerogative
 *   over a narrow set of landholding elites. 'Free men' denotes
 *   tenant-in-chief barons, not commoners or women. Protection is reciprocal
 *   but strictly limited to the contracting parties, leaving the majority of
 *   the English population outside its coverage. This reading treats the
 *   charter as a historical settlement of a specific feudal grievance, not as
 *   a transhistorical rights instrument or as an evolving constitutional
 *   text. The divergence between this reading and its siblings is the core of
 *   the Magna Carta kernel contest.
 *
 * KEY AGENTS:
 *   - landowning_barons: Primary beneficiary and enforcing party (powerful/constrained) â they receive protection from arbitrary arrest and taxation and collectively enforce the charter.
 *   - english_crown: Primary payer (institutional/constrained) â loses arbitrary feudal extraction power and is bound by the charter's clauses under duress.
 *   - commoners: Excluded population (powerless/trapped) â subject to the same royal and baronial jurisdiction but omitted from the charter's protections.
 *   - women: Excluded population (powerless/trapped) â legally mediated through male household heads, not independently covered by due process clauses.
 *   - non_landowners: Excluded population (powerless/trapped) â villeins and laborers for whom 'free men' has no application.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.42).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.58).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta 1215 â Baronial Privilege Reading").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, '34ea42a8-4cf0-4960-bb4b-590d04833732').
narrative_ontology:cs_kernel_codification('34ea42a8-4cf0-4960-bb4b-590d04833732', fixed_text).
narrative_ontology:cs_authority_grounding('34ea42a8-4cf0-4960-bb4b-590d04833732', lineage).
narrative_ontology:cs_reading_relation('34ea42a8-4cf0-4960-bb4b-590d04833732', magna_carta_1215__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('34ea42a8-4cf0-4960-bb4b-590d04833732', magna_carta_1215__living_document_reading, influences).
narrative_ontology:cs_axiom('34ea42a8-4cf0-4960-bb4b-590d04833732', foundational, feudal_reciprocity_limited_to_contracting_parties).
narrative_ontology:cs_axiom_status(feudal_reciprocity_limited_to_contracting_parties, holdable).
narrative_ontology:cs_axiom_grounding('34ea42a8-4cf0-4960-bb4b-590d04833732', feudal_reciprocity_limited_to_contracting_parties, conventional).
narrative_ontology:cs_axiom('34ea42a8-4cf0-4960-bb4b-590d04833732', foundational, free_men_denotes_tenant_in_chief).
narrative_ontology:cs_axiom_status(free_men_denotes_tenant_in_chief, holdable).
narrative_ontology:cs_axiom_grounding('34ea42a8-4cf0-4960-bb4b-590d04833732', free_men_denotes_tenant_in_chief, conventional).
narrative_ontology:cs_reference_frame('34ea42a8-4cf0-4960-bb4b-590d04833732', feudal_runymede_compact).
narrative_ontology:cs_drift_state('34ea42a8-4cf0-4960-bb4b-590d04833732', late_medieval_constitutional_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('34ea42a8-4cf0-4960-bb4b-590d04833732', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, english_crown).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold land directly from the crown and are the contracting parties to whom the charter's protections apply. They receive limits on arbitrary arrest, scutage without consent, and interference with inheritance. They collectively enforced the charter by arms and reasserted it during Henry III's minority. Exit from the feudal bond meant forfeiture or outlawry.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landowning_barons, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, landowning_barons, agenda_setter).

% Bound by the charter to obtain consent for extraordinary feudal levies and to observe defined process before penalizing the baronial party. The constraint extracts from royal prerogative and was accepted under duress at Runnymede, then annulled by Innocent III. The crown's attempts to evade the constraint provoked renewed conflict.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, english_crown, payer,
    institutional, generational, constrained, national).

% Comprise the bulk of the English population but are not party to the feudal contract. They remain subject to arbitrary royal and baronial jurisdiction without the charter's due-process or taxation-consent protections. Their situation is unchanged by the constraint's presence or absence.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, commoners, excluded,
    powerless, biographical, trapped, national).

% Including widows and daughters addressed only in clauses protecting baronial family property. They are not recognized as independent legal persons under the charter; their standing is mediated through male heads of household or direct royal grant.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, women, excluded,
    powerless, biographical, trapped, national).

% Villeins, cottagers, and urban laborers who do not hold freehold status. The term 'free men' in the charter does not extend to them, and they remain outside the contracting parties entirely.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, non_landowners, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__baronial_privilege_reading, landowning_barons).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents immediate civil war between the crown and the major feudal landholders by codifying customary limits on royal prerogative â specifically, requiring consent for scutage and due process for the contracting baronial party.
% TRANSFER_FUNCTION: Moves protection against arbitrary arrest, excessive feudal taxation, and interference with inheritance from the crown's unrestricted discretion to the defined set of landowning barons party to the compact.
% ABSENT_VOICES: Commoners, non-landowners, women, and the unfree are structurally absent from the negotiation and the text; they would claim protection from arbitrary power but are omitted from the charter's operative clauses.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, the crown would resume arbitrary seizure of baronial lands and widows, the barons would rebel, and the specific feudal reciprocity that ended the First Barons' War would collapse.
% FOUNDING_PROBLEM: The crown's arbitrary use of scutage, feudal reliefs, and royal justice to extract wealth from the barony without consent, provoking armed rebellion and near-civil war in 1215.
% FOUNDING_PROBLEM_CORROBORATION: Chroniclers including Roger of Wendover and the baronial party attested the grievances. Modern constitutional historians outside the beneficiary set corroborate that the immediate crisis was baronial and feudal, not universal; the papal curia also documented the coercive circumstances, albeit from a legitimacy standpoint hostile to the barons.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.42, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.42 at interval end) because the charter transfers substantial protections from crown to barons but within a recognized feudal reciprocity. Suppression is moderate-high (0.58) because the constraint's persistence required baronial military enforcement and papal opposition had to be overcome. Theater ratio rises from 0.10 to 0.30 as the charter's symbolic performance outpaced its original feudal function. Accessibility collapse is moderate (0.45): within the feudal framework, alternatives to written charter limitation were weak, but the framework itself was contestable. Resistance is high (0.62) from the crown and papal curia. The claimed type is tangled_rope: genuine coordination of feudal peace combined with asymmetric extraction of privileges by the baronial class from the crown and the explicit exclusion of the majority.
 *
 * PERSPECTIVAL GAP:
 *   From the baronial seat, the constraint is a hard-won restoration of customary feudal limits; from the crown's seat, it is a coerced diminution of royal prerogative; from the commoner seat, it is a visible legal order from which they are structurally omitted. The engine will compute these seats differently: the baronial seat likely approaches rope/coordination, while the commoner seat sees a mountain-like exclusion (no access) and the crown seat sees extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Barons are declared beneficiaries (low d): they receive the protective transfer and are structurally positioned to enforce it. The crown is declared payer (high d): the constraint extracts from royal discretion. Excluded groups (commoners, women, non-landowners) are not named in beneficiaries or victims because under this reading they are outside the constraint's scope rather than its targets; however, their structural omission is recorded in the absent_voices and excluded stakeholder roles. No directionality override is needed; the structural derivation from beneficiary/victim declarations correctly assigns d.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the charter as pure coordination (rope) â which would ignore the duress, the narrow beneficiary capture, and the exclusion of non-parties â and prevents mislabeling it as snare â which would ignore the genuine coordination function of ending the First Barons' War and stabilizing feudal custom. The temporal measurements show declining suppression and extractiveness as the charter aged into customary law, consistent with a constraint whose active enforcement phase was front-loaded. The founding problem (baronial grievance over arbitrary royal extraction) is dead by the end of the interval, though the text persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duress_vs_consent,
    'To what extent was the charter extracted under duress versus agreed by genuine feudal consensus?',
    'Archival study of baronial-crown negotiations in 1215 and papal annulment records assessing the balance of military coercion and customary bargaining.',
    'If pure duress, extraction is higher and the coordination story is largely cover; if genuine consensus, the constraint moves toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_vs_consent, empirical, 'Whether the charter was a coerced extraction or a negotiated coordination.').

omega_variable(
    excluded_groups_status,
    'Does the charter''s narrow scope actively reinforce the subordination of excluded groups, or merely leave them unregulated?',
    'Comparative legal history of villein status, women''s property rights, and non-landowner standing before and after 1215.',
    'If active reinforcement, excluded groups are structural victims; if mere omission, the victim set remains limited to the crown.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_groups_status, conceptual, 'Whether exclusion from the charter constitutes extraction from excluded groups.').

omega_variable(
    kernel_reading_location,
    'How does the baronial privilege reading''s narrow scope structurally constrain the development of the universal rights and living document readings?',
    'Historical tracing of interpretive practice: does the baronial reading function as a historical floor that later readings must overcome, or is it simply bypassed?',
    'If it functions as a floor, it influences later readings; if bypassed, the kernel fragments into disconnected constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural relationship of this kernel reading to its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 1297).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1225, 0.12).
narrative_ontology:measurement(magn_tr_t1235, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1235, 0.15).
narrative_ontology:measurement(magn_tr_t1255, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1255, 0.2).
narrative_ontology:measurement(magn_tr_t1275, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1275, 0.25).
narrative_ontology:measurement(magn_tr_t1297, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1297, 0.3).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1215, 0.52).
narrative_ontology:measurement(magn_be_t1225, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1225, 0.5).
narrative_ontology:measurement(magn_be_t1235, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1235, 0.47).
narrative_ontology:measurement(magn_be_t1255, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1255, 0.43).
narrative_ontology:measurement(magn_be_t1275, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1275, 0.39).
narrative_ontology:measurement(magn_be_t1297, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1297, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1215, 0.75).
narrative_ontology:measurement(magn_su_t1225, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1225, 0.7).
narrative_ontology:measurement(magn_su_t1235, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1235, 0.65).
narrative_ontology:measurement(magn_su_t1255, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1255, 0.55).
narrative_ontology:measurement(magn_su_t1275, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1275, 0.48).
narrative_ontology:measurement(magn_su_t1297, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1297, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, living_document_reading).

% DUAL FORMULATION NOTE:
% The kernel 'magna_carta_1215' decomposes into three structurally distinct readings: baronial_privilege_reading (feudal contract, narrow scope), universal_rights_reading (universal due process), and living_document_reading (adaptive constitutional substrate). Each reading has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
