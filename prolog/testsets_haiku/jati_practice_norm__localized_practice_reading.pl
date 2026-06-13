% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati Practice Norm (Localized Renegotiation Reading)
 *   domain: social/anthropological/religious
 *
 * SUMMARY:
 *   This constraint instantiates the LOCALIZED PRACTICE READING of the
 *   contested jati kernel. Under this reading, jati boundaries are negotiated
 *   coordination norms maintained through local ritual authority and
 *   continuous renegotiation, not fixed scriptural categories or
 *   administrative constructs. The empirical proliferation to 3000+ jati
 *   categories across South Asia is evidence that boundaries remain fluid and
 *   locally produced rather than centrally enforced. This reading generates
 *   low extractiveness (rope) because enforcement depends on local consensus
 *   and social recognition rather than coercive apparatus. The reading
 *   explicitly contrasts with two sibling readings: the orthodox textual
 *   reading (which grounds jati in varna scripture and treats deviation as
 *   pollution) and the colonial census reading (which reified jati into
 *   stable administrative categories). The three readings are not compatible
 *   framings of a single constraint — they instantiate three structurally
 *   distinct constraints with different ε values, different
 *   beneficiary/victim configurations, and different persistence mechanisms.
 *
 * KEY AGENTS:
 *   - local_jati_communities: primary beneficiary and agenda-setter; maintain jati boundaries through genealogical interpretation and ritual practice
 *   - ritual_specialists: organized agenda-setters; interpret purity rules and adjudicate boundary disputes; authority flows from local knowledge, not institutional position
 *   - merchant_guilds: beneficiaries; use jati-like coordination for trade network standards
 *   - untouchable_communities: primary victims; bear occupational restriction and ritual exclusion costs
 *   - brahmanical_textual_tradition: observer (non-agent); site of ongoing interpretive contest about what jati 'really' means
 *   - colonial_administrative_apparatus: excluded; attempted to freeze jati categories into fixed units; represents a different reading entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.35).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.28).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Practice Norm (Localized Renegotiation Reading)").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social/anthropological/religious").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, '58572d9c-7f43-4ade-ba66-8f9bbb0142b8').
narrative_ontology:cs_kernel_codification('58572d9c-7f43-4ade-ba66-8f9bbb0142b8', distributed).
narrative_ontology:cs_authority_grounding('58572d9c-7f43-4ade-ba66-8f9bbb0142b8', practice).
narrative_ontology:cs_interpretation_layer_present('58572d9c-7f43-4ade-ba66-8f9bbb0142b8').
narrative_ontology:cs_reading_relation('58572d9c-7f43-4ade-ba66-8f9bbb0142b8', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('58572d9c-7f43-4ade-ba66-8f9bbb0142b8', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('58572d9c-7f43-4ade-ba66-8f9bbb0142b8', foundational, jati_boundaries_locally_negotiated).
narrative_ontology:cs_axiom_status(jati_boundaries_locally_negotiated, holdable).
narrative_ontology:cs_axiom_grounding('58572d9c-7f43-4ade-ba66-8f9bbb0142b8', jati_boundaries_locally_negotiated, empirically_contingent).
narrative_ontology:cs_axiom('58572d9c-7f43-4ade-ba66-8f9bbb0142b8', foundational, ritual_authority_non_coercive).
narrative_ontology:cs_axiom_status(ritual_authority_non_coercive, holdable).
narrative_ontology:cs_axiom_grounding('58572d9c-7f43-4ade-ba66-8f9bbb0142b8', ritual_authority_non_coercive, empirically_contingent).
narrative_ontology:cs_reference_frame('58572d9c-7f43-4ade-ba66-8f9bbb0142b8', localized_ritual_authority_jati_negotiation).
narrative_ontology:cs_drift_state('58572d9c-7f43-4ade-ba66-8f9bbb0142b8', contemporary_reform_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('58572d9c-7f43-4ade-ba66-8f9bbb0142b8', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, local_jati_communities).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, ritual_specialists).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, merchant_guilds).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, untouchable_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize occupational and social practice around inherited jati identity. Communities continuously negotiate boundary membership, occupational specialization, and ritual status with neighboring jati groups. They benefit from coordinated occupational standards, mutual aid networks, and marriage alliance structures. Jati identity is deeply fused with self-concept, kinship, and livelihood — exit means severing community ties and occupational access.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, local_jati_communities, beneficiary,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__localized_practice_reading, local_jati_communities, agenda_setter).

% Interpret and adjudicate jati boundary disputes, purity rules, and ritual status claims. They maintain knowledge of genealogical legitimacy, occupational traditions, and ritual protocols. They enforce norms through social acknowledgment and ritual authority rather than coercive power. Their authority depends on local consensus and demonstrated knowledge — they cannot unilaterally dictate jati membership.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, ritual_specialists, agenda_setter,
    organized, generational, constrained, local).

% Use jati-like association structures to coordinate trade routes, apprenticeship standards, and quality enforcement. Merchants collectively maintain standards and reputation without centralized enforcement; jati-adjacent association provides coordination infrastructure. They maintain looser identity fusion than occupational jati — mobility is possible but carries cost to reputation and trade networks.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, merchant_guilds, beneficiary,
    organized, biographical, mobile, regional).

% The scriptural varna framework codifies ideal occupation and status categories. Local practice continuously deviates from and reinterprets textual prescriptions. The tradition itself is a site of interpretation — practitioners debate whether text-deviation constitutes pollution or legitimate adaptation.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, brahmanical_textual_tradition, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(jati_practice_norm__localized_practice_reading, brahmanical_textual_tradition).

% Under the colonial census project (1870s onward), attempted to freeze jati categories into fixed administrative units for taxation and governance. This external apparatus contradicts the localized practice reading by imposing stability on categories the reading characterizes as fluid. Colonial administrators were structurally excluded from the local negotiation processes that generated jati boundary pluralism.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, colonial_administrative_apparatus, excluded,
    institutional, biographical, trapped, national).

% Subject to ritual prohibition and occupational restriction under jati hierarchies. In the localized practice reading, these restrictions operate as persistent local coordination norms enforced through social exclusion rather than institutional decree. They bear the cost of the jati system's status hierarchy — their jati categories define them as bearers of ritual impurity and restrict occupational mobility. Exit from jati identity is culturally impossible and practically catastrophic.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, untouchable_communities, payer,
    powerless, generational, trapped, local).

% Interpret jati as a system of oppression requiring dissolution (ambedkarite reading) or as a site of legitimate cultural identity requiring democratic reform (caste-based affirmative action reading). They contest whether the coordination benefits genuinely accrue to all members or are captured by dominant jati within the category.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, contemporary_reform_movements, observer,
    organized, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Local jati communities solve an enduring occupational coordination problem: how to maintain apprenticeship standards, mutual aid, marriage alliance stability, and ritual knowledge transmission across generations without centralized enforcement. Jati boundaries organize kinship, occupation, and status in mutually reinforcing networks — the boundary itself is the coordination mechanism.
% TRANSFER_FUNCTION: Status, ritual authority, and occupational access flow to higher-ranked jati communities and ritual specialists who interpret boundary disputes. Subordinate jati and untouchable castes bear the cost of occupational restriction and ritual exclusion. The transfer is not monetized extraction but rather status distribution and occupational monopoly maintenance.
% ABSENT_VOICES: Untouchable communities are structurally excluded from setting jati boundaries despite being the primary bearers of the system's costs. Colonial administrative voices were excluded from the localized negotiation processes — their later attempt to reify jati through census reflects their enforced absence from local decision-making. Contemporary voices from jati communities resisting the localized framing (those advocating jati abolition) are outside this particular reading's frame.
% DISAPPEARANCE_RATIONALE: If the localized jati practice coordination norm disappeared overnight, occupational knowledge transmission, marriage alliance networks, and mutual aid structures embedded in jati identity would need to reorganize around alternative kinship or guild-based frames. Regional economies dependent on jati-coordinated craft specialization would experience disruption. Communities whose self-identity and livelihood are entirely structured by jati membership would face cultural disorientation and occupational displacement.
% FOUNDING_PROBLEM: Pre-state and early-state occupational specialization in agricultural and urban settings requires coordination mechanisms to transmit skills, enforce standards, and manage status hierarchy without centralized bureaucratic apparatus. Jati practice norms solve this by making occupational identity hereditary and regulating boundary membership through local ritual authority.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary anthropologists and historians studying pre-colonial South Asian economies document that jati practice provided effective occupational coordination without centralized state apparatus (Bayly, Dirks, Stein). Brahmanical ritual specialists within the tradition attest that jati boundary negotiation has always been a local matter of genealogy and ritual interpretation. However, reformist scholars and untouchable-movement voices attest that the founding problem framing erases the system's coercive origins and its status-hierarchical function — what appears as coordination to dominant jati appears as oppression to subordinated communities. The corroboration is split along beneficiary/victim lines.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).
:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.35) because the constraint operates primarily as occupational and kinship coordination. Status hierarchy and occupational monopoly do extract from lower-ranked jati and untouchables, but the mechanism is social exclusion and identity-fusion, not institutional compulsion. Suppression is moderate-low (0.28) because enforcement relies on local social recognition and ritual authority — if a community rejects a boundary judgment, the ritual specialist cannot compel obedience except by collective refusal to interact. The lack of centralized enforcement apparatus (what distinguishes this reading from the colonial reading) means high accessibility collapse (0.42) — once the boundary norm is understood, alternatives seem to collapse into cultural impossibility, but this is internalized suppression, not structural barriers. Resistance is substantial (0.58) because boundary renegotiations are constant, new jati categories continuously emerge, and untouchable movements and reform voices actively contest the reading itself. Theater is low (0.15) because the coordination function is genuinely performed — the norm is not maintained primarily through performative display. The flat measurement trajectory across the interval reflects the claim that this is a steady-state coordination mechanism, not one accumulating extraction or theatrical performance.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a local jati community (beneficiary/agenda-setter), this reading is descriptively accurate — boundaries ARE negotiated continuously, ritual authority IS distributed and local, and the system DOES coordinate occupational and kinship practice effectively. From the seat of an untouchable community (payer), the same constraint appears extractive and coercive — the 'negotiation' is asymmetrically weighted against them, 'local' means local dominants control interpretation, and the coordination works FOR dominant jati at their expense. The engine computes this divergence from directionality atoms (moderate power + identity_locked exit for jati beneficiaries yields low d; powerless + trapped exit for untouchables yields high d) without requiring the story to reconcile the perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Local jati communities hold moderate power (organized but not institutional), generational time horizons, identity-locked exit (jati identity is fused with kinship, livelihood, and self-concept), and local spatial scope. This yields low directionality (d ~ 0.25–0.35) — they are structural beneficiaries. Ritual specialists are organized but constrained by their dependence on local consensus, yielding d ~ 0.30–0.40. Untouchable communities hold powerless power, generational time horizon, trapped exit (culturally impossible to leave jati; occupational paths are blocked), local scope — yielding high directionality (d ~ 0.75–0.90). The asymmetry emerges from exit options: jati identity-lock is deliberate social identity fusion, not coerced external barrier; untouchable trap is social exclusion backed by collective refusal to interact. The measurement series is flat because the localized practice reading characterizes jati as a stable coordination mechanism — extraction and suppression are not accumulating, and theatrical performance is not rising. If suppression_requirement were rising, it would indicate colonial-era institutional encroachment; if it were falling, it would indicate spontaneous dissolution. Neither is claimed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (occupational coordination without centralized state) is contested but not dead — jati communities continue to use jati-like structures for occupational transmission and apprenticeship even in contemporary urban contexts. The disappearance verdict is world_rearranges because occupational knowledge and marriage alliance networks would need to reorganize. The reading does NOT assert that the founding problem has outlived its function (which would trigger mandatrophy resolution). Instead, the mandatrophy uncertainty lies in whether the coordination function genuinely serves all members or primarily captures benefits for dominant jati — this is the beneficiary/victim split documented in six_questions.founding_problem_corroboration. The reading claims coordination; the victims claim this is cover for status extraction. The mandatrophy gate is satisfied by acknowledging this split.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Do jati practice norms genuinely coordinate occupational practice, or is occupational coordination the cover story for status hierarchy extraction?',
    'Ethnographic analysis of jati communities where the coordination function has been disrupted or replaced: if occupational transmission continues through alternative frames with reduced status hierarchy, the coordination benefit was real and separable from extraction. If occupational transmission collapses, coordination was the primary function.',
    'If coordination is separable and primary, the rope classification holds. If status hierarchy extraction is primary and coordination is incidental, the constraint reclassifies as tangled_rope or snare. If both are inseparable, the classification depends on whether payers consent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether jati occupational coordination is genuine or cover for status extraction.').

omega_variable(
    identity_lock_mechanism,
    'Is jati identity-locking structural (external barriers making exit culturally impossible) or internalized (community members internalize jati as essential self)?',
    'Post-exit analysis of communities that have broken with jati categories: if jati-identity suppression persists after exit, it is internalized; if individuals adapt occupational and kinship practice to new frames without crisis, the lock was more structural than internalized.',
    'If internalized, the effective suppression is higher than the structural measure (0.28) suggests, and the constraint is more extractive than authored. If structural, the authored suppression is accurate. Identity-locked exit is a higher-cost exit than constrained or trapped for bounded populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether jati identity-locking is structural external barrier or internalized identity fusion.').

omega_variable(
    localized_practice_vs_textual_reading_contest,
    'Do jati boundaries instantiate the localized practice reading (continuously renegotiated, empirically fluid) or the orthodox textual reading (fixed by varna scripture, deviation as pollution)?',
    'Genealogical and ritual-knowledge analysis documenting the degree of continuity vs. innovation in jati boundary interpretation across generations within communities. High innovation and boundary proliferation (3000+ categories) supports localized-practice. High continuity with scriptural alignment supports textual reading. Debate within brahmanical tradition between reformists and ortho dox scholars on this question.',
    'If localized-practice dominates, ε stays at 0.35 (rope). If textual reading dominates, ε rises to ~0.55–0.65 (tangled rope, with textual authority as the enforcement mechanism). If colonial reading dominates, ε rises further to ~0.75–0.85 (snare, with administrative enforcement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(localized_practice_vs_textual_reading_contest, conceptual, 'Which reading of the jati kernel is structurally accurate: localized practice, textual prescription, or colonial reification.').

omega_variable(
    untouchable_agency_in_localized_practice,
    'In the localized practice reading, do untouchable communities participate in jati boundary renegotiation, or are they excluded from negotiation and only subject to the resulting boundaries?',
    'Ethnographic and historical documentation of untouchable participation in jati council disputes, marriage alliance negotiation, and ritual interpretation. Presence of untouchable ritual specialists or councils; degree of voice in boundary adjudication.',
    'If untouchables participate in negotiation and consensus-setting, exit_options for untouchable communities should be ''constrained'' rather than ''trapped,'' and directionality drops from 0.80+ to 0.60–0.75, making the constraint more purely rope. If excluded from negotiation, directionality stays high and the constraint is more tangled_rope (coordination for some, extraction for excluded). The current authoring assumes exclusion (trapped exit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(untouchable_agency_in_localized_practice, empirical, 'Whether untouchable communities have voice in jati boundary negotiation or are purely subject to boundaries set without their participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__localized_practice_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(jati_tr_t0, projected).
narrative_ontology:measurement(jati_tr_t5, jati_practice_norm__localized_practice_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement_basis(jati_tr_t5, projected).
narrative_ontology:measurement(jati_tr_t10, jati_practice_norm__localized_practice_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(jati_tr_t10, projected).
narrative_ontology:measurement(jati_tr_t15, jati_practice_norm__localized_practice_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement_basis(jati_tr_t15, projected).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__localized_practice_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(jati_tr_t20, projected).
narrative_ontology:measurement(jati_tr_t25, jati_practice_norm__localized_practice_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement_basis(jati_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__localized_practice_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(jati_be_t0, projected).
narrative_ontology:measurement(jati_be_t5, jati_practice_norm__localized_practice_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement_basis(jati_be_t5, projected).
narrative_ontology:measurement(jati_be_t10, jati_practice_norm__localized_practice_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement_basis(jati_be_t10, projected).
narrative_ontology:measurement(jati_be_t15, jati_practice_norm__localized_practice_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement_basis(jati_be_t15, projected).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__localized_practice_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(jati_be_t20, projected).
narrative_ontology:measurement(jati_be_t25, jati_practice_norm__localized_practice_reading, base_extractiveness, 25, 0.35).
narrative_ontology:measurement_basis(jati_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__localized_practice_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(jati_su_t0, projected).
narrative_ontology:measurement(jati_su_t5, jati_practice_norm__localized_practice_reading, suppression_requirement, 5, 0.26).
narrative_ontology:measurement_basis(jati_su_t5, projected).
narrative_ontology:measurement(jati_su_t10, jati_practice_norm__localized_practice_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement_basis(jati_su_t10, projected).
narrative_ontology:measurement(jati_su_t15, jati_practice_norm__localized_practice_reading, suppression_requirement, 15, 0.28).
narrative_ontology:measurement_basis(jati_su_t15, projected).
narrative_ontology:measurement(jati_su_t20, jati_practice_norm__localized_practice_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement_basis(jati_su_t20, projected).
narrative_ontology:measurement(jati_su_t25, jati_practice_norm__localized_practice_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement_basis(jati_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__localized_practice_reading, 0.12).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__colonial_census_reading).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__orthodox_textual_reading).

% DUAL FORMULATION NOTE:
% The JATI_PRACTICE_NORM is a contested kernel with three readings. This constraint instantiates the LOCALIZED_PRACTICE_READING (low extractiveness, empirically fluid boundaries, local authority). Sibling readings: COLONIAL_CENSUS_READING (administrative reification, higher suppression) and ORTHODOX_TEXTUAL_READING (scriptural grounding, different victim structure). The three readings are not compatible framings of one constraint — they have distinct ε values, beneficiary/victim structures, and persistence mechanisms. The localized-practice reading produces the lowest ε (0.35, rope) because enforcement depends on local consensus rather than centralized apparatus. The textual reading would produce higher ε (~0.55–0.65, tangled rope) if textual authority is the enforcement mechanism. The colonial reading would produce highest ε (~0.75–0.85, snare) if administrative apparatus stabilized categories. Each reading is authored as a separate constraint with its own stakeholder configuration and omega variables documenting the reading contest. All three are linked via network.affects_constraints to document the family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
