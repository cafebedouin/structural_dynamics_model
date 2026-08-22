% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__folk_syncretistic_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Folk Syncretistic Divine Legitimacy (Household Ritual Reading)
 *   domain: religious/political_economy/ancient_history
 *
 * SUMMARY:
 *   In the folk syncretistic reading, divine legitimacy is not centralized in
 *   state priesthood or pharaonic claims, but distributed across household
 *   and village ritual practice. Households and villages select from a
 *   pragmatic menu of deities—Amun for grain, Taweret for childbirth, Ptah
 *   for craft, Bes for household protection, gods borrowed from neighboring
 *   lands when local needs suggest it—without seeking permission from state
 *   priesthood or conforming to doctrinal orthodoxy. Authority over religious
 *   legitimacy flows downward from elites to communities; the pharaoh and
 *   state priesthood are recognized but held at arm's length. This reading
 *   instantiates one constraint from the divine_legitimacy_substrate
 *   kernel—the same underlying commitment (that divine power grounds the
 *   social order) is read through the folk lens rather than through priestly
 *   or monotheistic orthodoxy. The measurement series shows modest upward
 *   drift in extractiveness as state pressure to enforce doctrinal conformity
 *   gradually increases, but suppression remains low because the folk
 *   practice is resilient and difficult to suppress from the center.
 *
 * KEY AGENTS:
 *   - household_heads — direct ritual authority, local decision-making power, mobile exit options
 *   - village_elders — coordinators of collective ritual calendar, memory-keepers of efficacy knowledge, moderate power
 *   - state_priesthood — claim central authority over divine interpretation, structurally excluded from folk legitimacy
 *   - pharaonic_authority — recognized but distant; folk reading treats pharaonic divinity as separate from household petitions
 *   - ritual_specialists — consulted experts without monopoly authority; benefit from belief in efficacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.38).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.22).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Folk Syncretistic Divine Legitimacy (Household Ritual Reading)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "religious/political_economy/ancient_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, '19b64e33-4497-4542-9989-037ab13045aa').
narrative_ontology:cs_kernel_codification('19b64e33-4497-4542-9989-037ab13045aa', distributed).
narrative_ontology:cs_authority_grounding('19b64e33-4497-4542-9989-037ab13045aa', practice).
narrative_ontology:cs_interpretation_layer_present('19b64e33-4497-4542-9989-037ab13045aa').
narrative_ontology:cs_reading_relation('19b64e33-4497-4542-9989-037ab13045aa', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('19b64e33-4497-4542-9989-037ab13045aa', divine_legitimacy_substrate__atenist_monotheistic_reading, coexists_with).
narrative_ontology:cs_axiom('19b64e33-4497-4542-9989-037ab13045aa', foundational, divine_legitimacy_decentralized_to_community).
narrative_ontology:cs_axiom_status(divine_legitimacy_decentralized_to_community, holdable).
narrative_ontology:cs_axiom_grounding('19b64e33-4497-4542-9989-037ab13045aa', divine_legitimacy_decentralized_to_community, deontological).
narrative_ontology:cs_axiom('19b64e33-4497-4542-9989-037ab13045aa', foundational, ritual_efficacy_validates_practice).
narrative_ontology:cs_axiom_status(ritual_efficacy_validates_practice, holdable).
narrative_ontology:cs_axiom_grounding('19b64e33-4497-4542-9989-037ab13045aa', ritual_efficacy_validates_practice, instrumental).
narrative_ontology:cs_reference_frame('19b64e33-4497-4542-9989-037ab13045aa', household_centered_pragmatic_polytheism).
narrative_ontology:cs_drift_state('19b64e33-4497-4542-9989-037ab13045aa', late_dynastic_period_increasing_centralization, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('19b64e33-4497-4542-9989-037ab13045aa', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_elders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, ritual_specialists).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__folk_syncretistic_reading, pragmatic_polytheism_is_stable).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__folk_syncretistic_reading, ritual_efficacy_trumps_doctrinal_purity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise practical authority over household ritual: selecting which deities to propitiate, timing ceremonies, making offerings. Benefit from the flexibility to adapt worship to local conditions, family circumstance, and immediate needs without reference to priestly doctrine or pharaonic decree. Their authority is immediate and uncontested within the household sphere.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads, beneficiary,
    moderate, biographical, mobile, local).

% Coordinate the village's collective ritual calendar, mediate disputes over proper propitiation, preserve the accumulated knowledge of which deities respond to which petitions in local context. They are the memory-keepers and arbiters but do not centralize doctrine; they defend the flexibility that lets households and villages respond to famine, disease, or local calamity by mobilizing whichever deities tradition suggests might help.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, village_elders, agenda_setter,
    powerful, generational, mobile, regional).

% Maintain official temple theology and royal cult, claiming authority over the 'correct' interpretation of the gods. They would prefer a unified, top-down doctrine where local practice answers to priestly interpretation. The folk reading deliberately bypasses their authority—households do not petition the priesthood for permission to worship; they worship what works. Priesthood is excluded from the conversation about legitimacy, though they attempt repeatedly to impose doctrine.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, state_priesthood, excluded,
    institutional, civilizational, trapped, national).

% Claims divine status and role as intercessor between gods and people. In the folk reading, the pharaoh is recognized but held at arm's length: people know the pharaoh claims special relationship to the divine but do not depend on the pharaoh's ritual for their own access to protective or productive divine power. The pharaoh's divinity is distant and formal; household and village gods are immediate.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaonic_authority, excluded,
    institutional, civilizational, trapped, national).

% Move through Egyptian territory and encounter village worship. They see Egyptians invoking gods from multiple traditions—sometimes Amun, sometimes Ptah, sometimes gods borrowed from Nubia or the Levant—and pragmatically adapted to circumstance. They document this flexibility as a remarkable feature of Egyptian religion, distinct from more rigid doctrinal systems in other lands.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, foreign_merchants_and_travelers, observer,
    moderate, immediate, arbitrage, global).

% Provide expertise in propitiation methods, efficacy diagnostics, and ritual remedies for household misfortune. They are consulted but not controlling; they advise, households decide. They benefit from the belief that ritual works and from the work households commission, but they do not monopolize religious authority—any household head can perform basic propitiation, and the specialist is a respected practitioner, not a licensed gate-keeper.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, ritual_specialists, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, ritual_specialists, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__folk_syncretistic_reading, diffuse).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__folk_syncretistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of religious uncertainty in a precarious agricultural society: when drought, disease, or flood strikes, the village needs a shared framework for mobilizing divine assistance. The folk reading coordinates responses by preserving a flexible menu of deities known to help with different types of misfortune, allowing households and villages to adapt without waiting for top-down priestly guidance. Ritual efficacy—whether a petition worked—is the feedback mechanism that keeps the system adaptive.
% TRANSFER_FUNCTION: Moves authority and legitimacy downward from state priesthood and pharaonic claims to households and village elders. In exchange for accepting the pharaoh's formal divinity and paying state temple taxes, ordinary people retain control over their own spiritual petitions and the choice of which gods to invoke for their own circumstances. This is not an extraction mechanism; it is a boundary-setting arrangement.
% ABSENT_VOICES: State priests and theological reformers (the Atenist faction, the Amunist orthodox priesthood) are structurally excluded. They would object that folk practice is doctrinally incoherent and spiritually dangerous, that the gods should be approached only through proper priestly mediation, and that the pharaoh's divinity should flow through unified state religion. They are kept out by the same mechanism that preserves folk authority: local communities simply do not implement priestly doctrine and do not defer to state theological judgments about which gods are legitimate.
% DISAPPEARANCE_RATIONALE: If folk syncretistic practice vanished and were replaced by enforced doctrinal uniformity (either priestly Amunism or pharaonic Atenism), the legitimacy basis for household and village autonomy would collapse. People would lose the practical authority to worship as local circumstance demands, and would depend on state or priestly permission for every petition. Social organization would shift from decentralized households and villages coordinating their own spiritual safety to a hierarchy where religious legitimacy flows downward from the state. Resistance and heterodox practice would intensify.
% FOUNDING_PROBLEM: In a pre-industrial agricultural society without reliable written communication across regions, central authority cannot credibly claim to know what will protect each household and village from the particular dangers they face—locust plagues, inundation patterns, animal diseases, epidemics specific to local ecology. The founding problem is: how can people access divine protection when they cannot rely on central doctrine to address their immediate, place-specific crises?
% FOUNDING_PROBLEM_CORROBORATION: Household and village participants attest the problem is live: local ecological variation is real, central authorities are distant and slow, and the gods must be petitioned urgently. State priesthood disputes this: they claim priestly expertise and pharaonic divinity provide all necessary guidance. Foreign observers (Nubian, Levantine, Mediterranean traders and settlers) who interact with Egyptian households corroborate the household reading—they document the pragmatic selection of deities and the localization of practice. No single outside authority endorses the state priesthood's claim to monopoly on divine access.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__folk_syncretistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__folk_syncretistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).
:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and grows slowly (0.25 → 0.38) because the constraint operates without a centralized beneficiary: households benefit from flexibility, village elders benefit from coordinating authority, ritual specialists benefit from work, but no single party extracts systematic rents. The constraint persists because it solves a real problem (place-specific divine access in a precarious society) and because enforcement against it is weak—households simply do not comply with priestly doctrine if local practice works better for them. Suppression is low (0.22 at end) because the folk practice is diffuse, decentralized, and difficult to suppress from above; there is no single point where the state can choke off the constraint. Theater ratio is very low (0.15) because the ritual activity is functionally oriented toward actual outcomes (crops, children, health) rather than performative loyalty to doctrine. The measurement series share a single time grid; all metrics are authored at all six time points to enable temporal analysis without misalignment.
 *
 * PERSPECTIVAL GAP:
 *   State priesthood and pharaonic authority would compute this constraint as illegitimate heresy and threat to order; from their seat, folk syncretism is extraction of religious authority that rightfully belongs to the state. Household and village seats compute it as essential autonomy and practical wisdom. The engine computes these divergences from power and exit options: institutional seats (priesthood, pharaoh) face trapped exit and generational time horizon, which amplifies their perception of threat; moderate and powerful household/elder seats have mobile exit and can walk away from state doctrine, which lowers their experience of suppression. This gap is structural, not negotiable.
 *
 * DIRECTIONALITY LOGIC:
 *   Household heads and village elders are the structural beneficiaries—they collects authority and local control. State priesthood and pharaonic authority are the parties whose claims are foreclosed or weakened. The constraint does not extract from households in the material sense (no tax, no labor transfer); it redistributes legitimacy downward. Ritual specialists benefit modestly from the belief that ritual works. The key asymmetry: households have exit (they can practice folk ritual whether priesthood approves or not), so effective extraction on them is near zero; priesthood and pharaoh are trapped in the position of having their authority bypassed, so they bear a cost (loss of monopoly) with limited exit. This reversal—beneficiaries and payers are not who central authority would predict—is precisely why this reading is contested.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (precarious agriculture, place-specific crises, slow central communication) and the constraint solves it well (flexible decentralized access to divine protection). There is no mandatrophy signal: the constraint's function remains aligned with its origin, and resistance remains high because people experience it as protective. The state priesthood would diagnose mandatrophy (the founding problem is 'solved' by priestly doctrine and should be obsolete), but that diagnosis is a contestation of the reading itself, not evidence of mandate atrophy. Under the folk reading, the constraint will persist as long as place-specific ecological vulnerability persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    folk_practice_vs_doctrine_boundary,
    'Is the observed folk syncretism a genuine alternative reading of divine legitimacy, or is it understood by participants themselves as heretical deviation from the ''true'' doctrine that only educated priests know?',
    'Ethnographic reconstruction from household participants'' own statements, insofar as available from sources; examination of whether folk practitioners frame their practice as parallel-legitimate vs. inferior to priesthood.',
    'If folk practice is understood as genuinely legitimate (parallel authority), the reading is autonomy-grounded. If understood as deviant (participants know it violates doctrine but do it anyway), the constraint shifts toward snare (suppressed heterodoxy). The classification depends on participant self-understanding, not observer judgment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(folk_practice_vs_doctrine_boundary, empirical, 'Whether folk practice is framed as legitimate alternative or transgressive deviation by practitioners themselves.').

omega_variable(
    priesthood_enforcement_capacity,
    'How actively does the state priesthood attempt to enforce doctrinal conformity on folk practice, and how successful are those efforts?',
    'Evidence from temple records, priestly writings, and historical documentation of reform attempts (e.g., Amunist consolidation, Atenist suppression campaigns) to measure enforcement intensity over time.',
    'High active enforcement and some success would reclassify toward tangled_rope or snare (suppressed folk practice); low enforcement or complete failure would confirm rope (coordination without suppression). Current measurement of suppression (0.22 rising slowly to 0.22) suggests low success, but future intensification could sharply alter the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priesthood_enforcement_capacity, empirical, 'Measurement of actual priesthood enforcement effort against folk practice.').

omega_variable(
    pharaonic_divinity_functional_role,
    'Does ordinary participation in the constraint depend on accepting pharaonic divinity claims, or is pharaonic divinity orthogonal to folk household worship?',
    'Evidence from household religious practice: do households invoke pharaonic divinity in their petitions, or is the pharaoh recognized only as a formal/political claim separate from efficacy-oriented worship?',
    'If pharaonic divinity is functionally integrated into folk worship, pharaonic authority is not excluded—it is a distant but recognized component of the legitimacy system. If orthogonal, pharaonic divinity is ceremonial theater while real legitimacy lies elsewhere. This affects whether the pharaoh is best classified as agenda_setter or excluded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharaonic_divinity_functional_role, empirical, 'Whether pharaonic divinity is functionally required for folk ritual or orthogonal to it.').

omega_variable(
    committer_frame_reading_authenticity,
    'Is the folk syncretistic reading an artifact of later scholarly interpretation of ancient Egypt, or is it an authentic reading that ancient participants themselves would recognize and endorse?',
    'Source analysis: examination of whether the folk perspective emerges from ancient Egyptian sources (household-level texts, magical papyri, private devotional records) or is imposed by modern reconstructive scholarship.',
    'If the reading is an artifact of modern interpretation, the constraint story is describing a scholarly classification, not an ancient lived constraint. If authentic to ancient practice, it is a genuine alternative reading of the kernel. This does not change the constraint''s formal properties but affects its historical grounding and the reliability of the stakeholder situation descriptions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_reading_authenticity, conceptual, 'Whether the folk syncretistic reading is an authentic ancient participant reading or a modern scholarly reconstruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(divi_tr_t0, observed).
narrative_ontology:measurement(divi_tr_t8, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement_basis(divi_tr_t8, observed).
narrative_ontology:measurement(divi_tr_t16, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement_basis(divi_tr_t16, observed).
narrative_ontology:measurement(divi_tr_t24, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement_basis(divi_tr_t24, observed).
narrative_ontology:measurement(divi_tr_t32, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 32, 0.15).
narrative_ontology:measurement_basis(divi_tr_t32, observed).
narrative_ontology:measurement(divi_tr_t40, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(divi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(divi_be_t0, observed).
narrative_ontology:measurement(divi_be_t8, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 8, 0.3).
narrative_ontology:measurement_basis(divi_be_t8, observed).
narrative_ontology:measurement(divi_be_t16, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 16, 0.35).
narrative_ontology:measurement_basis(divi_be_t16, observed).
narrative_ontology:measurement(divi_be_t24, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement_basis(divi_be_t24, observed).
narrative_ontology:measurement(divi_be_t32, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 32, 0.38).
narrative_ontology:measurement_basis(divi_be_t32, observed).
narrative_ontology:measurement(divi_be_t40, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(divi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(divi_su_t0, observed).
narrative_ontology:measurement(divi_su_t8, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 8, 0.17).
narrative_ontology:measurement_basis(divi_su_t8, observed).
narrative_ontology:measurement(divi_su_t16, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 16, 0.19).
narrative_ontology:measurement_basis(divi_su_t16, observed).
narrative_ontology:measurement(divi_su_t24, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 24, 0.21).
narrative_ontology:measurement_basis(divi_su_t24, observed).
narrative_ontology:measurement(divi_su_t32, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 32, 0.22).
narrative_ontology:measurement_basis(divi_su_t32, observed).
narrative_ontology:measurement(divi_su_t40, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(divi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__folk_syncretistic_reading, 0.12).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).

% DUAL FORMULATION NOTE:
% The divine_legitimacy_substrate kernel decomposes into three constraints: folk_syncretistic_reading (this story), amun_polytheistic_reading (priestly orthodoxy), and atenist_monotheistic_reading (pharaonic monotheism). All three instantiate different readings of the same kernel—the commitment that divine power grounds social legitimacy. Each reading locates authority differently (village vs. temple vs. throne), produces different stakeholder positions, and exhibits different suppression and beneficiary profiles. The three constraints are linked via network.affects_constraints because changes to one reading's institutional position (e.g., state priesthood successfully enforcing Amunism, or pharaoh successfully imposing Atenism) directly affect the viability of the others. Folk practice persists as long as neither priestly nor pharaonic enforcement succeeds; if enforcement succeeds, folk practice is driven underground and the constraint classification shifts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
