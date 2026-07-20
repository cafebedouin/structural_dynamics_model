% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__incoherent_bundle_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: Shinbutsu Syncretism as State-Enforced Incoherent Bundle
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the incoherent_bundle_reading of the
 *   shinbutsu_ontological_substrate kernel. It models the historical Japanese
 *   shinbutsu (Shinto-Buddhist syncretism) arrangement not as a coherent
 *   theological commitment but as an accumulated bundle of institutional
 *   practices fused and maintained by state enforcement. The state apparatus
 *   and the institutional syncretic hierarchy extract political legitimacy
 *   and material patronage from the enforced performance of unity, while
 *   practitioners bear the cognitive cost of contradictory ontological
 *   commitments (e.g., honji suijaku as both metaphor and metaphysical claim)
 *   without doctrinal resolution.
 *
 * KEY AGENTS:
 *   - state_apparatus: Primary beneficiary/agenda_setter (institutional/mobile) â enforces fusion and captures legitimacy
 *   - institutional_syncretic_hierarchy: Secondary beneficiary (organized/constrained) â performs syncretic rites for state patronage
 *   - practitioners: Primary target (powerless/identity_locked) â bear contradictory ritual and ontological obligations
 *   - doctrinal_purists: Excluded victims (moderate/trapped) â marginalized for insisting on ontological coherence
 *   - modern_historians: Analytical observer (analytical/analytical) â reads the structure as political drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.8).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.76).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "Shinbutsu Syncretism as State-Enforced Incoherent Bundle").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'f1de3ef1-2775-4a68-bcc5-7fb2b54ef513').
narrative_ontology:cs_kernel_codification('f1de3ef1-2775-4a68-bcc5-7fb2b54ef513', implicit).
narrative_ontology:cs_authority_grounding('f1de3ef1-2775-4a68-bcc5-7fb2b54ef513', extraction).
narrative_ontology:cs_interpretation_layer_present('f1de3ef1-2775-4a68-bcc5-7fb2b54ef513').
narrative_ontology:cs_reading_relation('f1de3ef1-2775-4a68-bcc5-7fb2b54ef513', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('f1de3ef1-2775-4a68-bcc5-7fb2b54ef513', shinbutsu_ontological_substrate__domain_partition_reading, influences).
narrative_ontology:cs_axiom('f1de3ef1-2775-4a68-bcc5-7fb2b54ef513', foundational, no_coherent_ontological_kernel).
narrative_ontology:cs_axiom_status(no_coherent_ontological_kernel, holdable).
narrative_ontology:cs_axiom_grounding('f1de3ef1-2775-4a68-bcc5-7fb2b54ef513', no_coherent_ontological_kernel, empirically_contingent).
narrative_ontology:cs_axiom('f1de3ef1-2775-4a68-bcc5-7fb2b54ef513', foundational, state_enforcement_sustains_apparent_unity).
narrative_ontology:cs_axiom_status(state_enforcement_sustains_apparent_unity, holdable).
narrative_ontology:cs_axiom_grounding('f1de3ef1-2775-4a68-bcc5-7fb2b54ef513', state_enforcement_sustains_apparent_unity, empirically_contingent).
narrative_ontology:cs_reference_frame('f1de3ef1-2775-4a68-bcc5-7fb2b54ef513', state_syncretic_equilibrium).
narrative_ontology:cs_drift_state('f1de3ef1-2775-4a68-bcc5-7fb2b54ef513', meiji_restoration, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('f1de3ef1-2775-4a68-bcc5-7fb2b54ef513', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, institutional_syncretic_hierarchy).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, doctrinal_purists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Imposes and regulates the integration of Shinto and Buddhist institutions through law, patronage, and ritual mandate. Derives political legitimacy and social control from presenting a unified religious front. Can alter or abolish the arrangement by fiat, as demonstrated by the Meiji Restoration's shinbutsu bunri.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Shrine priests and temple monks whose institutional survival and income depend on performing state-sanctioned syncretic rites. They receive land, patronage, and ritual role from the fused system. Their capacity to exit is constrained by dependence on state recognition and hereditary position.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, institutional_syncretic_hierarchy, beneficiary,
    organized, biographical, constrained, national).

% Lay populace and local clergy required to participate in syncretic rituals without doctrinal clarification of whether kami are local manifestations of buddhas or independent beings. Bear the cognitive and ritual costs of contradictory ontological commitments enforced by custom and state sanction.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, practitioners, payer,
    powerless, biographical, identity_locked, local).

% Buddhist exclusivists and Shinto nativists who reject syncretism on doctrinal grounds. They are marginalized from state patronage, excluded from official ritual roles, and their voices are suppressed from the dominant discourse. They would object to the arrangement if present in the institutional conversation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, doctrinal_purists, excluded,
    moderate, biographical, trapped, regional).

% Modern scholars of Japanese religion who analyze the historical arrangement as politically motivated institutional drift rather than coherent theology. They observe the divergence between state rhetoric and practitioner experience.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, modern_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_apparatus).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__incoherent_bundle_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Managing the coexistence of Shinto and Buddhist institutional authority under a single political order to consolidate state legitimacy and prevent sectarian competition for power.
% TRANSFER_FUNCTION: Moves doctrinal coherence and ontological clarity from practitioners to the state apparatus and institutional hierarchy in exchange for political stability and ritual patronage.
% ABSENT_VOICES: Doctrinal purists in both Shinto and Buddhist traditions who would insist on ontological exclusivity, and lay practitioners seeking coherent theological instruction rather than ritual obligation. They are excluded from the institutional discourse by the syncretic hierarchy's monopoly on state-recognized religious practice.
% DISAPPEARANCE_RATIONALE: If the state-enforced syncretic framework vanished, shrine-temple complexes would decouple into autonomous traditions, practitioners would sort into clarified doctrinal affiliations or secular practice, and the unified religious authority underwriting imperial legitimacy would fragment into competing sectarian claims.
% FOUNDING_PROBLEM: How to consolidate political authority over a population with dual ritual obligations to both native kami cults and imported Buddhist institutions without permitting either to challenge state supremacy.
% FOUNDING_PROBLEM_CORROBORATION: The Meiji government's shinbutsu bunri policy (1868) and modern historians of Japanese religion acting from outside the Edo-period beneficiary set attest that the original political problem of imperial consolidation no longer required syncretic fusion, and that the arrangement persisted as institutional inertia rather than live coordination.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.8, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.80) because practitioners pay in unresolved doctrinal contradiction and ritual compulsion while the state captures unified religious authority. Suppression (0.76) reflects active state enforcement of temple-shrine integration and marginalization of exclusivist movements. Theater_ratio (0.68) captures the performative maintenance of ontological unity that masks underlying incoherence. Accessibility_collapse (0.72) indicates that alternatives (pure Shinto, pure Buddhism) were structurally marginalized. Resistance (0.48) acknowledges periodic exclusivist movements (e.g., Yoshida Shinto, Buddhist reformers) that were ultimately subdued. Temporal measurements trace the intensification from organic medieval blending to early modern state codification.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus, the constraint is political coordination consolidating legitimacy; from the practitioner seat, it is cognitive extraction without doctrinal resolution. The institutional hierarchy occupies a dual position, extracting patronage while performing the theater of unity. The engine computes these divergences from structural data â beneficiary/victim declarations, exit options, and power levels.
 *
 * DIRECTIONALITY LOGIC:
 *   State_apparatus and institutional_syncretic_hierarchy are structural beneficiaries (low d): they collect legitimacy and patronage and have mobile/constrained exit respectively. Practitioners are structural targets (high d): identity_locked to a ritual system that demands contradictory ontological commitments. Doctrinal_purists are excluded targets (high d, trapped exit). Modern_historians are analytical with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â consolidating state authority over dual religious institutions â was live during the Heian and Edo periods. By the late Tokugawa period, the arrangement had become a self-sustaining extraction structure: the state continued to enforce fusion despite diminishing sectarian threat. The Meiji state's dissolution of the arrangement (shinbutsu bunri) confirms that the coordination function had atrophied and the constraint persisted by inertia and enforcement until an external actor dismantled it. This prevents mislabeling the historical snare as a rope or scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_drift_vs_intentional_design,
    'Was shinbutsu syncretism the result of intentional state design or organic institutional drift that the state later captured and enforced?',
    'Archaeological and textual analysis of pre-state syncretic practices versus state edicts codifying the arrangement.',
    'If primarily intentional design, the constraint reads as a snare from inception; if organic drift later captured, it may have begun as rope before being weaponized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_drift_vs_intentional_design, empirical, 'Origin ambiguity of syncretism as state design versus drift').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the enforcement of syncretism primarily structural (state edicts, institutional fusion) or internalized (practitioners naturalizing contradictory beliefs)?',
    'Analysis of post-Meiji practitioner behavior: if contradictory belief structures dissolved quickly after state enforcement ended, suppression was structural; if they persisted, internalized.',
    'Internalized suppression implies higher effective extraction than structural measures alone suggest, as practitioners carry the constraint after formal enforcement ceases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in religious syncretism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(shin_tr_t5, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(shin_tr_t10, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(shin_tr_t15, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 15, 0.6).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 20, 0.65).
narrative_ontology:measurement(shin_tr_t25, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 25, 0.68).
narrative_ontology:measurement(shin_tr_t30, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 30, 0.7).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(shin_be_t5, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(shin_be_t10, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(shin_be_t15, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(shin_be_t20, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(shin_be_t25, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement(shin_be_t30, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 30, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(shin_su_t5, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(shin_su_t10, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(shin_su_t15, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(shin_su_t20, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(shin_su_t25, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(shin_su_t30, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 30, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial label 'shinbutsu ontological substrate' into three structurally distinct readings: ontological fusion (syncretic_fusion), functional separation (domain_partition), and incoherent state-enforced bundle (this reading). Each reading has a different epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
