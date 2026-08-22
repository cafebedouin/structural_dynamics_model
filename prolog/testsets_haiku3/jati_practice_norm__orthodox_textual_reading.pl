% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Orthodox Textual Jati Boundary Enforcement
 *   domain: social/religious/political_economy
 *
 * SUMMARY:
 *   The orthodox textual reading instantiates jati categories as derived from
 *   and perpetually validated by scriptural varna doctrine. Within this
 *   reading, jati boundaries are not negotiable, local, or administrative
 *   artifacts — they are cosmologically mandated assignments of occupational
 *   duty and ritual status. Deviation from one's assigned jati role and
 *   occupation produces ritual pollution that contaminates not only the
 *   individual but their family and community. This reading underpins
 *   brahminical authority and justifies hierarchical labor extraction. The
 *   claim (snare) and metrics are aligned: extractiveness is high because the
 *   reading offers no exit path for occupational reassignment outside the
 *   scriptural frame; suppression is high because the constraint's
 *   persistence depends on actively delegitimizing localized negotiation and
 *   reform critique as spiritual corruption. Theater is moderate because the
 *   reading does embed a genuine coordination function (occupational
 *   predictability, ritual role clarity) alongside its extractive operation.
 *
 * KEY AGENTS:
 *   - Brahminical authority (agenda-setter; locks own identity to the role of textual gatekeeper)
 *   - Polluting jatis (victims; trapped in occupational assignment + pollution status)
 *   - Occupationally bound but non-polluting jatis (victims; identity-locked to occupational inheritance)
 *   - High jatis (beneficiaries; monopolize resources, education, ritual privilege)
 *   - Varna scriptural corpus (vindicated proposition; treated as cosmologically authoritative)
 *   - Localized jati practitioners (excluded; would testify to negotiation and fluidity)
 *   - Reform movements (excluded; would testify to contingency and power asymmetry)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.82).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.79).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Orthodox Textual Jati Boundary Enforcement").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social/religious/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, '62f17015-c82d-4c36-af75-ab8f5733ae2d').
narrative_ontology:cs_kernel_codification('62f17015-c82d-4c36-af75-ab8f5733ae2d', fixed_text).
narrative_ontology:cs_authority_grounding('62f17015-c82d-4c36-af75-ab8f5733ae2d', lineage).
narrative_ontology:cs_interpretation_layer_present('62f17015-c82d-4c36-af75-ab8f5733ae2d').
narrative_ontology:cs_reading_relation('62f17015-c82d-4c36-af75-ab8f5733ae2d', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('62f17015-c82d-4c36-af75-ab8f5733ae2d', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('62f17015-c82d-4c36-af75-ab8f5733ae2d', foundational, varna_doctrine_cosmically_binding).
narrative_ontology:cs_axiom_status(varna_doctrine_cosmically_binding, holdable).
narrative_ontology:cs_axiom_grounding('62f17015-c82d-4c36-af75-ab8f5733ae2d', varna_doctrine_cosmically_binding, theological).
narrative_ontology:cs_axiom('62f17015-c82d-4c36-af75-ab8f5733ae2d', foundational, occupational_inheritance_dharmic_duty).
narrative_ontology:cs_axiom_status(occupational_inheritance_dharmic_duty, holdable).
narrative_ontology:cs_axiom_grounding('62f17015-c82d-4c36-af75-ab8f5733ae2d', occupational_inheritance_dharmic_duty, deontological).
narrative_ontology:cs_reference_frame('62f17015-c82d-4c36-af75-ab8f5733ae2d', vedic_cosmic_order_framework).
narrative_ontology:cs_drift_state('62f17015-c82d-4c36-af75-ab8f5733ae2d', contemporary_post_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('62f17015-c82d-4c36-af75-ab8f5733ae2d', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahminical_authority).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, polluting_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, occupational_lockdown_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, orthodox_religious_authority).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, high_jatis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Brahminical gatekeepers maintain and adjudicate the scriptural varna framework. They validate jati status through ritual classification, control access to purification rites, and determine which occupations carry pollution. They monopolize the legitimacy of boundary enforcement; their authority persists because they control what counts as correct practice within the religious tradition. To exit would mean abandoning brahminical professional identity itself — the occupation is constituted by this role.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahminical_authority, agenda_setter,
    institutional, civilizational, identity_locked, regional).

% Jatis assigned to 'polluting' occupations are barred from shared wells, temples, and domestic spaces. Their children inherit the occupational assignment and the pollution status. Exit from the occupation means leaving the community entirely — occupational mobility is blocked by the pollution framework itself. Geographic mobility encounters the same classification system. Their resistance is structured as spiritual deficiency: the framework teaches that their low status is karmic consequence.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, polluting_jatis, payer,
    powerless, civilizational, trapped, regional).

% Non-polluting but occupationally bound jatis are locked to their occupational role by scriptural assignment and ritual obligation. Children cannot become merchants or warriors without abandoning their jati identity entirely — which means losing family, ritual status, and community belonging. The constraint locks occupation to birth through identity fusion.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, occupational_lockdown_communities, payer,
    powerless, civilizational, identity_locked, regional).

% The scriptural varna doctrine is vindicated and treated as cosmologically authoritative by the constraint's operation. The texts are cited as the source of jati legitimacy; their internal logic becomes operational reality. The doctrine collects no rents but its authority is strengthened by the constraint's enforcement.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, varna_scriptural_corpus, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jati_practice_norm__orthodox_textual_reading, varna_scriptural_corpus).

% Hindu temples and orthodox intellectual traditions derive their legitimacy partly from maintaining varna-jati orthodoxy. Deviation is framed as spiritual corruption. They benefit from categorical rigidity because it makes their teaching role seem natural and unchosen — 'we are merely transmitting timeless truth' rather than 'we are enforcing a power structure.'
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, orthodox_religious_authority, beneficiary,
    institutional, civilizational, identity_locked, regional).

% Upper-ranked jatis benefit from occupational monopolies, access to education, temple rights, and the service of lower jatis. The scriptural framework legitimizes their resource control. They carry the constraint forward because it protects their advantages.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, high_jatis, beneficiary,
    organized, civilizational, constrained, regional).

% Village-level jati councils and local ritual specialists practice jati negotiation and boundary adjustment based on community economics. They are excluded from the orthodox reading: the framework treats local practice as corruption of true varna principles. Were they present, they would testify to jati categories shifting and subdividing based on social factors.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, localized_practitioners, excluded,
    moderate, biographical, constrained, local).

% Hindu reform movements explicitly rejected scriptural jati doctrine and called for occupational mobility and inter-jati marriage. Their counter-readings of the same scriptural tradition are excluded from the orthodox framework. They would testify that the varna doctrine is a historical contingency, not a timeless truth.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, reform_movements, excluded,
    organized, generational, constrained, regional).

% British officials documented and attempted to codify jati categories for census and administrative purposes. They created a different constraint (colonial_census_reading) that froze fluid categories into rigid lists and made jati legible as a census variable.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, colonial_administrators, observer,
    institutional, biographical, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__orthodox_textual_reading, brahminical_authority).
narrative_ontology:fixing_cost_class(jati_practice_norm__orthodox_textual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The orthodox textual reading offers a cosmological framework for occupational assignment and social hierarchy: it solves the coordination problem of 'who does what work' and 'how are ritual relations ordered' by mapping these questions to birth and scriptural duty. This provides predictability and role clarity.
% TRANSFER_FUNCTION: Moves labor service, ritual deference, and surplus agricultural production from lower jatis to higher jatis and brahminical authority. A leather worker provides essential services but receives restricted access to shared resources, temples, and marriage networks. Upper jatis extract unpaid ritual obligation from lower jatis. Brahminical authority collects the power to define what counts as polluting.
% ABSENT_VOICES: Localized practitioners and jati councils who renegotiate boundaries pragmatically are excluded — they would testify to fluidity. Reform movements and abolitionists are excluded — they would argue the scriptural reading is selective and self-serving. Colonial administrators are observers only.
% DISAPPEARANCE_RATIONALE: If the orthodox textual constraint vanished, occupational inheritance would become subject to negotiation, pollution-based exclusion would have no cosmological warrant, and inter-jati mobility would expand. Labor organization would shift, upper-jati monopolies would be exposed as historical arrangements, and the brahminical teaching role would require justification outside scriptural transmission.
% FOUNDING_PROBLEM: The constraint claims to solve the problem of cosmic order and duty: how should occupational roles be assigned? The scriptural varna framework asserts that birth-determined occupational duty is cosmically correct — deviation creates entropy and ritual pollution.
% FOUNDING_PROBLEM_CORROBORATION: Brahminical authorities attest the founding problem is live: maintaining varna-jati order is essential to social harmony. Reform movements and Ambedkarite scholars attest it is dead: the constraint was built not to solve cosmic order but to lock occupational monopolies. Historians note the varna system differs substantially across textual strata, suggesting consolidation rather than timelessness. No neutral observer endorses the brahminical narrative without qualification.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__orthodox_textual_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much the constraint transfers labor, deference, and resources from lower to higher positions while blocking occupational exit. At 0.82, this reflects: (1) rigid occupational lockdown across generations (no mobility mechanism except abandoning family/community); (2) pollution-based exclusion that denies basic resources and dignity to assigned-polluting jatis; (3) unpaid ritual obligation extracted from lower jatis. The trajectory is stable (~0.78–0.82 across the interval) because the constraint's enforcement machinery (brahminical authority, ritual gatekeeping, scriptural teaching) persists unchanged — it is not eroding. Theater ratio is moderate (0.28) because the constraint does perform real coordination (role clarity, predictability) even as its primary function is extraction; the theater is not theatrical performance masquerading as function, but genuine coordination carrying extractive overlay. Suppression at 0.79 reflects: (1) structural barriers (occupational inheritance, geographic classification portability); (2) internalized barriers (karma doctrine teaching that low status is deserved, that deviation brings pollution); (3) active enforcement (brahminical gatekeeping of ritual status, temple exclusion). The internalized component is substantial — the constraint persists not only through external barriers but through making its victims believe their status is cosmically ordained.
 *
 * PERSPECTIVAL GAP:
 *   Brahminical authority seats the constraint as a beneficiary — they experience it as coordination they maintain and defend. Their exit would require abandoning professional identity and community authority. Lower-jati seats experience it as extraction with no exit — occupational reassignment is blocked both externally (ritual gatekeeping) and internally (identity fusion: 'I am a leather worker' becomes 'I am polluted'). The engine should compute radically different type classifications: from the brahminical seat, this might compute as rope or tangled rope (coordination with enforcement); from the victim seats, it computes as snare (no exit, structured extraction, coercion justified by the victim's supposed spiritual deficiency). The directionality derivation should produce d~0.1–0.2 for brahminical authority (beneficiary, institutional power, escape via role redefinition) and d~0.85–0.95 for polluting jatis (trapped, powerless, identity-locked, cosmological justification of constraint).
 *
 * DIRECTIONALITY LOGIC:
 *   Brahminical authority: beneficiary (collects ritual authority, monopolizes legitimacy), institutional power (controls teaching and gatekeeping), identity_locked exit (the role IS constituted by brahminical function within the varna system — to exit is to cease being brahmin). Directionality should be low (~0.15–0.25), closer to beneficiary end. Polluting jatis: victims (trapped in assigned occupation, denied resources, subjected to pollution doctrine), powerless (no leverage in negotiation, outnumbered, economically dependent on high-jati demand for their services), trapped exit (occupational inheritance is total; geographic mobility carries the classification forward; exit from the jati means leaving family and community). Directionality should be high (~0.85–0.95), at the full target end. Occupational-lockdown jatis: victims in occupational inheritance (bound to role by birth), identity_locked (their social personhood is constituted by jati membership; leaving jati = leaving self), powerless-to-moderate power (some leverage as organized occupational groups, but constrained by ritual ranking). Directionality ~0.65–0.75. High jatis: beneficiaries (monopolize education, landholding, ritual access), organized-to-institutional power, constrained exit (could leave but would lose status, wealth, social position). Directionality ~0.20–0.35.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cosmic order through birth-determined duty) is attested as LIVE by brahminical authority. Reform movements and contemporary scholars attest it is DEAD — occupational duty is not cosmologically mandated, and the constraint persists as institutional extraction defended by inertia and authority capture. The constraint should be examined for mandatrophy resolution: if the founding problem is truly dead (occupational assignment no longer solves cosmic order; it solves only labor extraction), and the constraint persists anyway, then the arrangement has outlived its function. The high theater ratio (though still moderate) combined with the refusal to acknowledge functional obsolescence in orthodox teaching suggests low-level mandatrophy. The measurement trajectory showing slight theater growth (0.20 → 0.28) indicates the coordination function (occupational predictability) is becoming a smaller fraction of why the constraint persists — pure extraction defense is growing relative. This is a soft mandatrophy signal: the constraint is not theoretically bankrupt, but its founding justification is increasingly disputed and its actual operation increasingly exposes as extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_vs_constructed,
    'Is the orthodox varna-jati framework a timeless scriptural doctrine, or a historical consolidation project that claimed timelessness as legitimacy?',
    'Historical-linguistic analysis of the varna system across textual corpus (Rigveda varna vs. Manusmriti jati); comparison of early-text flexibility with classical-text rigidity; examination of when jati became synonymous with varna in the textual tradition.',
    'If the framework is a consolidation (not timeless), the constraint is a manufactured hierarchy defending itself by claiming natural law — this would reclassify it from a potentially coordinating arrangement to a pure snare, and would feed a false-summit diagnosis if any reading claimed it as mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_vs_constructed, empirical, 'Whether varna doctrine is timeless revelation or historical consolidation claiming timelessness.').

omega_variable(
    internalized_vs_structural_suppression,
    'How much of the observed suppression (0.79) is structural (external barriers to exit) vs. internalized (the victim believes the constraint is justified)?',
    'Historical-comparative: examine occupational mobility outcomes in post-colonial India as legal barriers were removed; measure persistence of behavioral suppression (occupational inheritance, inter-jati avoidance) after structural barriers fell; interview second-generation recipients of educational access regarding residual belief in jati-based occupational duty.',
    'If internalized suppression dominates, the constraint is more entrenched than structural metrics suggest — removing legal barriers will not suffice for exit. The constraint''s persistence would depend on ideology and identity fusion, not just coercive machinery. Remedies would require identity decontamination and teaching, not merely resource reallocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'The decomposition of suppression into structural and internalized components.').

omega_variable(
    reading_foreclosure_via_cosmology,
    'Does the orthodox textual reading logically foreclose the localized practice reading, or do they coexist as different frames applied by different communities?',
    'Examine whether an orthodox brahminical authority can coherently hold both readings simultaneously (e.g., ''the varna doctrine is true AND jati boundaries are locally negotiable'') or whether the readings occupy genuinely incompatible premises. Test by posing the question to brahminical scholars: can varna-jati be both fixed and fluid?',
    'If the readings foreclose each other, they cannot coexist within a single authority structure — one must win and the other be suppressed. If they coexist, the framework is more flexible than the orthodox reading appears, and the constraint''s rigidity is performative choice, not logical necessity. This affects the reading_relations in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_via_cosmology, conceptual, 'Whether the orthodox and localized readings are logically incompatible (forecloses) or occupiable by different communities simultaneously (coexists).').

omega_variable(
    brahminical_identity_lock_depth,
    'Is brahminical professional identity fused with the varna-jati enforcement role, or can brahminical scholarship and community function be disaggregated from jati-boundary gatekeeping?',
    'Examine historical instances where brahminical communities reformed or abandoned jati enforcement (Brahmo Samaj, Arya Samaj reformers). Analyze contemporary brahminical scholarship that critiques varna doctrine while maintaining brahminical learning and ritual roles.',
    'If the identity fusion is absolute, brahminical authority cannot exit the constraint without ceasing to exist — they are trapped by identity as much as lower jatis. If it is separable, brahminical exit is possible (abandoning gatekeeping while maintaining scholarship/ritual), making their benefit extraction a choice rather than necessity. This affects directionality for brahminical stakeholders and the classification of their role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahminical_identity_lock_depth, empirical, 'The degree of identity fusion between brahminical professional identity and jati-enforcement function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__orthodox_textual_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jati_tr_t5, jati_practice_norm__orthodox_textual_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(jati_tr_t10, jati_practice_norm__orthodox_textual_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(jati_tr_t15, jati_practice_norm__orthodox_textual_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__orthodox_textual_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(jati_tr_t25, jati_practice_norm__orthodox_textual_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(jati_be_t5, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 5, 0.79).
narrative_ontology:measurement(jati_be_t10, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(jati_be_t15, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 15, 0.81).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(jati_be_t25, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 25, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0, 0.76).
narrative_ontology:measurement(jati_su_t5, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 5, 0.77).
narrative_ontology:measurement(jati_su_t10, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(jati_su_t15, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(jati_su_t20, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(jati_su_t25, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 25, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__orthodox_textual_reading, 0.12).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% The jati_practice_norm kernel admits three structurally distinct constraint readings instantiated in separate JSON files. The orthodox_textual_reading (this file) models jati as fixed scriptural doctrine with high extraction. The localized_practice_reading models jati as fluid local-negotiation coordination with lower extraction. The colonial_census_reading models jati as administrative stabilization with extraction captured by colonial apparatus. Each reading carries its own ε, beneficiary/victim structure, and cs_structure framing. The three readings coexist in the historical reality — different communities hold different readings — but each reading is internally consistent as a single constraint. See commentary.kernel_context for the reading divergence. All three readings affect each other: the colonial reading froze categories that the localized reading treats as fluid, and both pressure the textual reading's claim to timelessness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jati_practice_norm__orthodox_textual_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
