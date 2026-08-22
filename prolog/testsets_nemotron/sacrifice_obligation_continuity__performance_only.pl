% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation — Performance-Only Reading
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint story captures the 'performance_only' reading of the
 *   sacrifice_obligation_continuity kernel: the obligation to offer qorbanot
 *   (sacrifices) in the Jerusalem Temple is structurally binding on every
 *   generation of Jews, yet the physical means of fulfillment (Temple,
 *   priesthood, altar, Sanhedrin) have been absent since 70 CE. The reading
 *   holds that study of sacrifice law (qodashim tractates, Seder Kodashim) is
 *   meritorious preparation for eventual restoration but does not discharge
 *   the obligation. The current generation therefore stands in a state of
 *   unfulfillable obligation — guilty of non-performance through no action of
 *   their own. The constraint extracts high psychological and communal cost
 *   (guilt, exclusion from full covenantal standing, pressure toward
 *   messianic activism) while suppressing alternatives (the
 *   study_as_performance reading, messianic_suspension,
 *   archival_preservation) through institutional authority and social
 *   enforcement. Beneficiaries include rabbinic authorities who maintain
 *   interpretive monopoly and Temple Institute organizations that fundraise
 *   on restoration narratives. Victims are the observant laity who bear the
 *   guilt without remedy, converts without priestly lineage who can never
 *   serve even in a restored Temple, and women excluded from priestly service
 *   by the same textual tradition.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: agenda_setter (institutional/biographical/constrained/regional) — defines the obligation's parameters and polices interpretive boundaries
 *   - temple_institute_organizations: beneficiary (organized/biographical/mobile/national) — materially benefits from restoration fundraising and political advocacy
 *   - observant_laity_current_generation: payer (moderate/generational/identity_locked/regional) — bears the guilt and social cost of unfulfillable obligation
 *   - converts_without_lineage: payer (powerless/generational/trapped/universal) — permanently excluded from priestly service even in restoration scenario
 *   - women_excluded_from_priestly_service: payer (moderate/generational/identity_locked/universal) — structurally barred from fulfillment by the same tradition that imposes the obligation
 *   - historical_critical_scholars: observer (analytical/civilizational/analytical/universal) — analyzes the textual development without normative commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.82).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.75).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.82).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, snare).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation — Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, '0408ac80-5182-4d42-9f7f-a8504f2779f3').
narrative_ontology:cs_kernel_codification('0408ac80-5182-4d42-9f7f-a8504f2779f3', fixed_text).
narrative_ontology:cs_authority_grounding('0408ac80-5182-4d42-9f7f-a8504f2779f3', lineage).
narrative_ontology:cs_interpretation_layer_present('0408ac80-5182-4d42-9f7f-a8504f2779f3').
narrative_ontology:cs_reading_relation('0408ac80-5182-4d42-9f7f-a8504f2779f3', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('0408ac80-5182-4d42-9f7f-a8504f2779f3', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('0408ac80-5182-4d42-9f7f-a8504f2779f3', sacrifice_obligation_continuity__archival_preservation, coexists_with).
narrative_ontology:cs_axiom('0408ac80-5182-4d42-9f7f-a8504f2779f3', foundational, physical_performance_sole_fulfillment_mode).
narrative_ontology:cs_axiom_status(physical_performance_sole_fulfillment_mode, holdable).
narrative_ontology:cs_axiom_grounding('0408ac80-5182-4d42-9f7f-a8504f2779f3', physical_performance_sole_fulfillment_mode, deontological).
narrative_ontology:cs_axiom('0408ac80-5182-4d42-9f7f-a8504f2779f3', foundational, study_is_preparation_not_satisfaction).
narrative_ontology:cs_axiom_status(study_is_preparation_not_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('0408ac80-5182-4d42-9f7f-a8504f2779f3', study_is_preparation_not_satisfaction, deontological).
narrative_ontology:cs_reference_frame('0408ac80-5182-4d42-9f7f-a8504f2779f3', sinaitic_temple_service_mandate).
narrative_ontology:cs_drift_state('0408ac80-5182-4d42-9f7f-a8504f2779f3', post_churban_longue_duree, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0408ac80-5182-4d42-9f7f-a8504f2779f3', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, temple_institute_organizations).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, observant_laity_current_generation).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, converts_without_lineage).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, women_excluded_from_priestly_service).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, torah_obligations_are_eternal_and_unchangeable).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, physical_performance_is_sole_mode_of_fulfillment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the obligation's parameters through halakhic rulings, control yeshiva curricula that center qodashim study as preparation-only, determine who may serve in rabbinic positions (excluding advocates of alternative readings). Their authority derives from being the recognized interpreters of the 'authentic' tradition. Exit is constrained — a rabbi who adopts study_as_performance loses institutional standing.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, rabbinic_authorities, agenda_setter,
    institutional, biographical, constrained, regional).

% Run Temple Mount visits, manufacture ritual vessels, train kohanim, fundraise on restoration narratives. They materially benefit from the performance_only reading because it makes restoration the ONLY path to obligation fulfillment — study cannot substitute. Their exit is mobile (they could pivot to heritage tourism), but the reading's logic makes their current model lucrative.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, temple_institute_organizations, beneficiary,
    organized, biographical, mobile, national).

% Born into a covenantal framework that demands Temple sacrifice as a core obligation. They study qodashim intensively (daf yomi, yeshiva) but are explicitly taught this study does NOT fulfill the obligation — it only prepares for a Temple that has not existed for 1,950 years. They bear the guilt of non-performance with no remedy. Exit is identity_locked: leaving means abandoning the covenantal identity that structures their entire life, family, and community.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, observant_laity_current_generation, payer,
    moderate, generational, identity_locked, regional).

% Converted to Judaism without priestly (kohen/levi) lineage. Even if the Temple were rebuilt tomorrow, they could never perform the sacrificial service — the performance_only reading restricts priestly service to patrilineal descendants of Aaron. They bear the full obligation with structurally zero possibility of fulfillment even in the restoration scenario. Exit is trapped: they chose this framework and cannot undo the conversion, but the framework permanently excludes them from its central rite.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, converts_without_lineage, payer,
    powerless, generational, trapped, universal).

% Bound by the same obligation (qorbanot are communal and individual duties) but structurally barred from the priestly service that is the only recognized mode of performance. The performance_only reading inherits the textual tradition's gender restriction on the priesthood. They study the laws, support the institutions, bear the communal guilt — but can never be the ones who actually perform the commanded act. Exit is identity_locked like the observant laity generally.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, women_excluded_from_priestly_service, payer,
    moderate, generational, identity_locked, universal).

% Analyze the textual development of sacrifice law from biblical through rabbinic periods, documenting how the performance_only reading emerged post-70 CE as a response to catastrophe. They have no stake in the obligation's fulfillment or non-fulfillment. Their exit is analytical — they can change frameworks without identity cost.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, historical_critical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__performance_only, rabbinic_authorities).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Jewish covenantal identity and continuity across exile by anchoring the community to the Temple service as its orienting center — even in the Temple's absence, the obligation structures communal life, education, and eschatological hope.
% TRANSFER_FUNCTION: Moves psychological burden (guilt, inadequacy, longing), communal resources (yeshiva funding, Temple Institute donations, political advocacy), and interpretive authority from the laity to the rabbinic gatekeepers and restoration organizations. The obligation's impossibility is the extraction mechanism: the unfulfillable demand generates the transfer.
% ABSENT_VOICES: The study_as_performance reading (represented by some Modern Orthodox and Conservative thinkers), the messianic_suspension reading (some Hasidic and Kabbalistic traditions), and the archival_preservation reading (secular scholars, Reform Judaism) are structurally excluded from halakhic authority. They would argue that study fulfills, or that obligation is suspended, or that the law is cultural heritage — but the performance_only reading controls the institutions that define orthodoxy.
% DISAPPEARANCE_RATIONALE: If the performance_only reading vanished overnight, the observant laity would experience immediate existential relief (guilt lifted), yeshiva curricula would radically shift (qodashim study would become optional/historical), Temple Institute fundraising would collapse, and rabbinic authority would lose its primary anchor to 'eternal unchangeable law.' The Jewish world would reorganize around study_as_performance, messianic_suspension, or archival_preservation — each already existent as live alternatives.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the rabbinic movement needed to maintain covenantal continuity and Jewish identity without the central institution (Temple, priesthood, sacrifice) that the Torah presents as indispensable. The performance_only reading solved this by declaring the obligation eternal and its current impossibility a temporary divine decree — preserving the Temple as the orienting center of Jewish life.
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical scholarship (Neusner, Schwartz, Boyarin) documents the rabbinic reconfiguration post-70 CE as a response to catastrophe, not as the revelation of an eternal truth. Archaeological evidence shows no Temple, no priesthood, no altar for 1,950 years. The very existence of the sibling readings (study_as_performance in Mishnah Menachot 110a, messianic_suspension in Rambam Hilkhot Melakhim, archival_preservation in modern Reform responsa) corroborates that the founding problem (how to live without the Temple) generated MULTIPLE solutions, not one eternal obligation. The performance_only reading's beneficiaries (rabbinic authorities) are the ONLY ones who attest the problem is still live in its original form.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.82) is high because the obligation demands physical performance that is structurally impossible for the current generation, yet the system assigns full guilt for non-performance. The extraction is not monetary but existential: covenantal standing, communal belonging, and eschatological hope are leveraged. Suppression (0.75) reflects active enforcement through social pressure, educational curricula that present the performance_only reading as the only authentic position, and institutional exclusion of dissenting voices from positions of authority. Theater ratio (0.45) is significant: study of qodashim is performed with great intensity (daf yomi cycles, yeshiva curricula, Temple Institute simulations) but the reading explicitly denies it fulfills the obligation — the performance is theater in the precise sense of maintained appearance without functional payoff. Accessibility collapse (0.88) is near-total: the physical preconditions (Temple, priesthood, altar) are historically gone and the reading denies any substitute. Resistance (0.15) is low because the identity_locked exit option makes departure from the framework nearly unthinkable for the observant laity; the primary resistance comes from external scholars and internal doubters who lack institutional voice.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic_authorities seat (agenda_setter, institutional power, constrained exit), the constraint appears as faithful guardianship of an eternal covenant — the obligation's impossibility is a feature, not a bug, proving the temporariness of exile. From the observant_laity seat (payer, moderate power, identity_locked exit), the same structure operates as a snare: they are born into guilt they cannot remedy, with study explicitly denied as satisfaction. The converts_without_lineage and women_excluded_from_priestly_service seats experience even sharper extraction — they are permanently structurally excluded from the only mode of fulfillment the reading recognizes. The engine will compute these seat divergences from the structural data; the claim (snare) reflects the payer seats' reality, not the agenda_setter's framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities are beneficiaries (d ~ 0.15): they hold interpretive monopoly, control educational pipelines, and derive authority from being the gatekeepers of the 'authentic' reading. Temple Institute organizations are beneficiaries (d ~ 0.20): they receive material resources (donations, political support) from the restoration narrative. Observant laity are payers (d ~ 0.85): they bear the full psychological weight of unfulfillable obligation with identity_locked exit (leaving the framework means abandoning covenantal identity). Converts without lineage are payers (d ~ 0.95): even in a hypothetical restoration, they lack the tribal lineage for priestly service — their exclusion is structural and permanent. Women excluded from priestly service are payers (d ~ 0.90): the same textual tradition that imposes the obligation bars them from its fulfillment. Historical-critical scholars are analytical observers (d = 0.5): they analyze without stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining covenantal continuity through centralized sacrificial worship) is dead — the Temple is destroyed, the priesthood scattered, the altar gone. The arrangement persists not because the problem lives but because the performance_only reading extracts authority and resources from maintaining the obligation's structural impossibility. The mandatrophy is unresolved: the constraint has outlived its function by nearly two millennia but the reading denies this, framing the impossibility as divine test rather than institutional obsolescence. The founding_problem_status = 'contested' captures that beneficiaries claim the problem is live (exile is temporary) while payers experience it as dead (no Temple in sight). Corroboration from outside the beneficiary set comes from historical-critical scholarship, archaeological evidence, and the very existence of the sibling readings (study_as_performance, messianic_suspension, archival_preservation) which emerged precisely because the performance_only reading became unlivable for significant communities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commitment_frame_vs_historical_fact,
    'Is the obligation''s binding force a genuine covenantal reality (from within the commitment frame) or a historically constructed claim that serves institutional interests (from outside)?',
    'The question is irresolvable within the system — it is the kernel contest itself. From within the performance_only frame, the obligation''s eternality is axiomatic. From outside (historical-critical, sociological), the reading''s emergence coincides with the need to maintain authority after 70 CE. No empirical test can adjudicate between frames.',
    'If the commitment frame is taken as authoritative, the constraint is a mountain (eternal divine law) experienced as snare only from the limited human perspective. If the historical frame is taken as authoritative, the constraint is a snare constructed to maintain rabbinic authority and Temple restoration narratives. The classification divergence IS the measurement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commitment_frame_vs_historical_fact, conceptual, 'Whether the obligation''s eternality is a covenantal axiom or a post-70 CE institutional construction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative readings (study_as_performance, messianic_suspension) structural (institutional exclusion, educational monopoly) or internalized (the observant laity genuinely believe alternatives are heretical)?',
    'Post-exit trajectory analysis: if individuals who leave the observant framework continue to experience guilt about unfulfilled sacrifice obligation, the suppression has been internalized. If guilt dissolves upon exit, suppression was primarily structural.',
    'If internalized, effective suppression is higher than institutional measures suggest — the target carries the constraint''s enforcement internally. This would increase the snare classification strength for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the performance_only reading''s interpretive monopoly').

omega_variable(
    restoration_probability_as_extraction_lever,
    'Does the restoration narrative function as an extraction lever — maintaining obligation intensity by keeping fulfillment ''imminent'' across two millennia?',
    'Historical analysis of restoration rhetoric intensity correlated with institutional fundraising, political mobilization, and interpretive rigidity. If restoration imminence rhetoric peaks during institutional stress periods, it functions as extraction lever.',
    'If confirmed, the messianic horizon is not a theological claim but a structural feature that prevents the obligation from lapsing into obsolescence — the ''always about to be fulfilled'' dynamic maintains extraction indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_probability_as_extraction_lever, empirical, 'Whether the restoration narrative structurally functions to prevent obligation lapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.12).
narrative_ontology:measurement(sacr_tr_t390, sacrifice_obligation_continuity__performance_only, theater_ratio, 390, 0.18).
narrative_ontology:measurement(sacr_tr_t780, sacrifice_obligation_continuity__performance_only, theater_ratio, 780, 0.28).
narrative_ontology:measurement(sacr_tr_t1170, sacrifice_obligation_continuity__performance_only, theater_ratio, 1170, 0.36).
narrative_ontology:measurement(sacr_tr_t1560, sacrifice_obligation_continuity__performance_only, theater_ratio, 1560, 0.41).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_obligation_continuity__performance_only, theater_ratio, 1950, 0.45).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sacr_be_t390, sacrifice_obligation_continuity__performance_only, base_extractiveness, 390, 0.42).
narrative_ontology:measurement(sacr_be_t780, sacrifice_obligation_continuity__performance_only, base_extractiveness, 780, 0.55).
narrative_ontology:measurement(sacr_be_t1170, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1170, 0.68).
narrative_ontology:measurement(sacr_be_t1560, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1560, 0.76).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1950, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(sacr_su_t390, sacrifice_obligation_continuity__performance_only, suppression_requirement, 390, 0.35).
narrative_ontology:measurement(sacr_su_t780, sacrifice_obligation_continuity__performance_only, suppression_requirement, 780, 0.52).
narrative_ontology:measurement(sacr_su_t1170, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1170, 0.64).
narrative_ontology:measurement(sacr_su_t1560, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1560, 0.71).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1950, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__performance_only, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sacrifice_obligation_continuity kernel. The performance_only reading and its siblings (study_as_performance, messianic_suspension, archival_preservation) constitute a constraint family linked by network.affects_constraints. Each reading instantiates a different constraint with different ε, different beneficiary/victim structures, and different classifications from the same textual kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_continuity__performance_only, institutional, 0.15).
constraint_indexing:directionality_override(sacrifice_obligation_continuity__performance_only, organized, 0.2).
constraint_indexing:directionality_override(sacrifice_obligation_continuity__performance_only, moderate, 0.85).
constraint_indexing:directionality_override(sacrifice_obligation_continuity__performance_only, powerless, 0.95).
constraint_indexing:directionality_override(sacrifice_obligation_continuity__performance_only, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
