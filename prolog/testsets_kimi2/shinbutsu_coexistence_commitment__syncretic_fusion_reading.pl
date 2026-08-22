% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__syncretic_fusion_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__syncretic_fusion_reading
 *   human_readable: Honji Suijaku Syncretic Fusion (Kami as Buddhist Manifestations)
 *   domain: religious/philosophical/historical
 *
 * SUMMARY:
 *   This constraint instantiates the syncretic_fusion_reading of the
 *   shinbutsu_coexistence_commitment kernel, which governed religious life in
 *   Japan from the Heian period until the Meiji separation of 1868. The
 *   reading asserts that kami and Buddhist deities form a single coherent
 *   ontology through honji suijaku â local kami are manifestations
 *   (suijaku) of universal Buddhist truth (honji). Sibling readings include
 *   domain_partition_reading (separate domains without ontological
 *   unification) and incoherent_bundle_reading (a politically maintained
 *   incoherence). The constraint operated through jinguji institutions and a
 *   theological elite that interpreted every local deity through Buddhist
 *   metaphysics, extracting ritual authority from shrine priests while
 *   delivering a unified sacred hierarchy to the imperial court.
 *
 * KEY AGENTS:
 *   - buddhist_theological_elite: Primary agenda_setter (institutional/constrained) â composes and enforces the doctrinal framework, captures interpretive authority and patronage.
 *   - jinguji_institutions: Structural beneficiary (institutional/constrained) â embodies the fusion materially, receives land and ritual labor.
 *   - imperial_court: Secondary beneficiary (powerful/mobile) â sanctions the hierarchy to consolidate legitimacy.
 *   - local_shrine_priests: Primary target (moderate/identity_locked) â loses independent theological authority, must accept Buddhist reinterpretation of their own deities.
 *   - local_kami_communities: Secondary target (powerless/identity_locked) â devotional practice redescribed in Buddhist terms, autonomous cosmology extracted.
 *   - autonomous_shrine_lineages: Excluded voice (organized/constrained) â maintains independent kami theology but is marginalized from doctrinal councils and patronage.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.62).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.68).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Honji Suijaku Syncretic Fusion (Kami as Buddhist Manifestations)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious/philosophical/historical").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'b38371f2-181e-47c0-9e3c-421a060ed418').
narrative_ontology:cs_kernel_codification('b38371f2-181e-47c0-9e3c-421a060ed418', fixed_text).
narrative_ontology:cs_authority_grounding('b38371f2-181e-47c0-9e3c-421a060ed418', lineage).
narrative_ontology:cs_interpretation_layer_present('b38371f2-181e-47c0-9e3c-421a060ed418').
narrative_ontology:cs_reading_relation('b38371f2-181e-47c0-9e3c-421a060ed418', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('b38371f2-181e-47c0-9e3c-421a060ed418', shinbutsu_coexistence_commitment__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('b38371f2-181e-47c0-9e3c-421a060ed418', foundational, kami_as_buddhist_manifestations).
narrative_ontology:cs_axiom_status(kami_as_buddhist_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('b38371f2-181e-47c0-9e3c-421a060ed418', kami_as_buddhist_manifestations, theological).
narrative_ontology:cs_axiom('b38371f2-181e-47c0-9e3c-421a060ed418', foundational, theological_elite_interpretive_supremacy).
narrative_ontology:cs_axiom_status(theological_elite_interpretive_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('b38371f2-181e-47c0-9e3c-421a060ed418', theological_elite_interpretive_supremacy, conventional).
narrative_ontology:cs_reference_frame('b38371f2-181e-47c0-9e3c-421a060ed418', universal_buddhist_truth_framework).
narrative_ontology:cs_drift_state('b38371f2-181e-47c0-9e3c-421a060ed418', meiji_restoration, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('b38371f2-181e-47c0-9e3c-421a060ed418', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_theological_elite).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_shrine_priests).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_communities).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, honji_suijaku_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, ontological_unification_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compose, transmit, and enforce the honji suijaku doctrinal framework. Assert interpretive authority over which kami correspond to which Buddhist figures, subordinating local cults to universal Buddhist ontology. Receive institutional patronage, land grants, and ritual supremacy from the imperial court and provincial governors in exchange for providing a unified sacred hierarchy.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_theological_elite, agenda_setter,
    institutional, generational, constrained, national).

% Combined temple-shrine institutions whose physical and economic existence depends on maintaining the syncretic ritual economy. Receive patronage streams, labor obligations, and territorial rights by embodying the doctrinal fusion; their architecture and ritual calendars enact the ontological subordination of kami to Buddhas.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institutions, beneficiary,
    institutional, generational, constrained, regional).

% Patronizes Buddhist institutions and sanctions honji suijaku as official religious ideology. Benefits from a unified sacred hierarchy that reduces competing claims to divine authority and channels provincial religious loyalty through a single, court-legitimated institutional structure.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court, beneficiary,
    powerful, generational, mobile, national).

% Maintain local kami cults and festivals but must accept Buddhist sutra recitation, Buddhist imagery, and doctrinal reinterpretation of their deities as traces of universal Buddhist truth. Lose independent theological authority; their ritual primacy is progressively absorbed into temple-administered jinguji complexes.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_shrine_priests, payer,
    moderate, biographical, identity_locked, local).

% Worship local protective and agricultural deities. Their devotional objects, festival calendars, and oral traditions are gradually redescribed in Buddhist terms; independent kami-centric theology becomes inaccessible or classified as heterodox. Experience the constraint as religious maintenance rather than overt coercion, but their autonomous cosmology is extracted.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_communities, payer,
    powerless, biographical, identity_locked, local).

% Shrine traditions that resist full Buddhist incorporation and maintain independent kami theology and ritual lineages. Excluded from doctrinal councils, imperial patronage networks, and scriptural education; their objections to ontological subordination are categorized as ignorance rather than legitimate theological dissent.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, autonomous_shrine_lineages, excluded,
    organized, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves competition and potential violent or fiscal conflict between Buddhist and kami worship institutions by subsuming both under a single ontological hierarchy, creating a unified ritual, patronage, and legitimacy economy across the archipelago.
% TRANSFER_FUNCTION: Moves theological authority, interpretive primacy, and ritual jurisdiction from local shrine priests and kami communities to Buddhist temples and doctrinal elites; moves material patronage, land, and labor toward jinguji institutions and the imperial court.
% ABSENT_VOICES: Autonomous shrine lineages that maintain independent kami worship without Buddhist overlay are structurally excluded from doctrinal councils and patronage networks; their theological objections are treated as rustic ignorance rather than legitimate dissent.
% DISAPPEARANCE_RATIONALE: If the ontological unification vanished overnight, the jinguji institutional economy would collapse, local shrine priests would reclaim independent ritual authority and theological dignity, Buddhist elites would lose their interpretive monopoly, and the imperial court would face unmediated competing sacred claims from revived local kami cults.
% FOUNDING_PROBLEM: Competing religious authorities in the Nara-Heian transition creating parallel patronage demands, territorial conflict between temples and shrines, and ontological anxiety about how universal Buddhist truth related to local protective deities already deeply embedded in agricultural and communal life.
% FOUNDING_PROBLEM_CORROBORATION: Court chronicles and provincial gazetteers from the Nara-Heian transition attest to shrine-temple competition over land, ritual jurisdiction, and patronage from outside the mature Buddhist benefiting establishment. Modern religious historians and archaeologists outside the jinguji lineage corroborate that the competitive landscape existed, though they dispute whether the syncretic solution was the only structurally possible resolution.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the doctrine systematically transfers theological authority from shrine priests to Buddhist elites; suppression (0.68) is higher because the arrangement required active enforcement through jinguji institutionalization and imperial sanction to maintain ontological subordination against shrine autonomy. Theater ratio (0.30) is moderate: the theological synthesis is genuinely elaborate, but a growing share of late-period maintenance defended Buddhist institutional supremacy rather than organic spiritual integration. Accessibility collapse (0.75) is high because once inside the jinguji system, independent kami-centric theology became nearly unthinkable; resistance (0.40) reflects persistent but institutionally weak pushback from autonomous shrine lineages. The measurement series share one time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The Buddhist theological elite experiences the constraint as necessary coordination â solving the problem of competing cults through sophisticated metaphysics â while local shrine priests experience the same structure as extraction of their autonomous theological identity. The engine computes this divergence from the structural data: agenda_setter plus beneficiary status with constrained exit yields low directionality (subsidy), while payer status with identity_locked exit yields high directionality (target). The imperial court sits between, extracting political legitimacy without bearing ritual costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (buddhist_theological_elite, jinguji_institutions, imperial_court) receive low directionality because the constraint subsidizes their authority and resource claims. Victims (local_shrine_priests, local_kami_communities) receive high directionality because they bear the costs of ontological subordination with identity_locked exit â their religious identity is fused with shrine roles that have no doctrinal autonomy within the honji suijaku framework. The excluded autonomous_shrine_lineages would register even higher directionality if they were inside the constraint rather than marginalized outside it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â competition between Buddhist and kami authorities â was substantially transformed by the Edo period into a stable patronage hierarchy where the jinguji system delivered real coordination benefits (reduced inter-cult conflict, unified ritual calendar). However, the arrangement persisted beyond its original problem configuration and showed rising theater_ratio and suppression_requirement in the late Edo period as autonomous shrine traditions and state rationalization pressures mounted. The mandatrophy was forcibly resolved by the Meiji state's shinbutsu bunri decrees rather than internal atrophy, preventing a full piton transition but producing late-interval signatures of degraded performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_divergence,
    'Does the syncretic fusion reading describe a genuine theological discovery or a constructed political compromise, and how would a sibling domain_partition reading change the epsilon referent?',
    'Comparative historical analysis of doctrinal development versus patronage politics in the Nara-Heian transition; a sibling domain_partition reading would author epsilon for parallel ritual jurisdiction without ontological subordination, collapsing the authority-transfer mechanism.',
    'If the fusion was constructed for political coordination, extraction is higher (authority transfer to Buddhist elite is revealed as contingent power, not discovered truth). If discovered, extraction is lower (constraint approaches coordination with minimal overhead).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Whether the constraint is a discovered truth or constructed fusion').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s persistence due to internalized belief in honji suijaku by shrine priests, or structural enforcement by jinguji institutions and imperial patronage networks?',
    'Analysis of post-Meiji separation behavior: if shrine priests immediately reclaimed independent kami theology, suppression was primarily structural; if they persisted in Buddhist reinterpretation long after institutional removal, suppression was significantly internalized.',
    'Internalized suppression implies higher effective extraction than structural measures suggest; the target carries the constraint after exit. Would shift late-interval metrics toward higher extraction and support reclassification toward snare-like identity capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in doctrinal subordination').

omega_variable(
    coordination_extraction_boundary,
    'How much of the syncretic fusion''s persistence represents genuine coordination benefit (reduced inter-cult conflict, unified ritual calendar, stable patronage) versus asymmetric extraction (Buddhist institutional dominance, ontological erasure of kami autonomy)?',
    'Counterfactual analysis of religious conflict indicators in periods and regions where syncretic fusion was weak versus strong; measurement of patronage flow concentration toward Buddhist institutions relative to shrine networks.',
    'A high coordination component supports the authored tangled_rope classification; a low coordination component with high extraction would shift the computed type toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Coordination benefit versus extraction in syncretic system').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 200, 0.22).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 400, 0.3).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 600, 0.38).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 800, 0.48).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1000, 0.58).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(shin_be_t200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 200, 0.52).
narrative_ontology:measurement(shin_be_t400, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 400, 0.61).
narrative_ontology:measurement(shin_be_t600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 600, 0.68).
narrative_ontology:measurement(shin_be_t800, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 800, 0.72).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1000, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(shin_su_t200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 200, 0.48).
narrative_ontology:measurement(shin_su_t400, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 400, 0.58).
narrative_ontology:measurement(shin_su_t600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 600, 0.68).
narrative_ontology:measurement(shin_su_t800, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 800, 0.78).
narrative_ontology:measurement(shin_su_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1000, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% The shinbutsu_coexistence_commitment kernel decomposes into three structurally distinct constraints because the natural-language label 'shinbutsu-shugo' conflates multiple claims: domain_partition (separate ontologies), syncretic_fusion (unified ontology), and incoherent_bundle (no coherent ontology). Each reading has a different epsilon, beneficiary/victim structure, and classification. This story (syncretic_fusion) links to its siblings as the historically dominant reading that structurally influenced both the reactive partition and the modern critical bundle reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
