% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Temple Sacrifice Obligation â Study as Archiving
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This constraint story instantiates the study_as_archiving reading of the
 *   temple_sacrifice_obligation kernel. The standing arrangement under
 *   contest is the post-Temple rabbinic regime in which the biblical
 *   commandment of animal sacrifice remains halakhically binding despite the
 *   absence of the Temple, and intensive study of sacrificial law is
 *   explicitly ruled not to fulfill the obligation. The arrangement
 *   coordinates diasporic Jewish identity and preserves ritual knowledge
 *   across millennia, while simultaneously concentrating interpretive
 *   authority in the rabbinic establishment and locking the community into a
 *   state of perpetual non-compliance. The sibling readings â
 *   study_as_occupation (study fulfills) and messianic_suspension (obligation
 *   paused) â are structurally distinct constraints with different epsilon
 *   values and victim profiles; they are modeled as separate stories linked
 *   in the constraint family.
 *
 * KEY AGENTS:
 *   - rabbinic_establishment: Primary agenda-setter (institutional/identity_locked/global) â administers the binding-but-unperformable ruling, derives legitimacy from guardianship of the kernel.
 *   - jewish_community: Primary payer (organized/identity_locked/global) â bears the burden of the unfulfilled commandment and the labor of non-remedial study.
 *   - torah_study_institutions: Secondary beneficiary (organized/constrained/global) â receives resources and prestige from the curricular mandate.
 *   - kohanim: Excluded voice (moderate/constrained/global) â ritual specialists marginalized by the study-substitution logic.
 *   - academic_religion_scholars: Analytical observer (analytical/analytical/global) â external observer of the commitment-system dynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.58).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.62).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.58).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Temple Sacrifice Obligation â Study as Archiving").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious/halakhic").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, 'f623b739-d45a-4352-8d06-1c11a015768a').
narrative_ontology:cs_kernel_codification('f623b739-d45a-4352-8d06-1c11a015768a', fixed_text).
narrative_ontology:cs_authority_grounding('f623b739-d45a-4352-8d06-1c11a015768a', lineage).
narrative_ontology:cs_interpretation_layer_present('f623b739-d45a-4352-8d06-1c11a015768a').
narrative_ontology:cs_reading_relation('f623b739-d45a-4352-8d06-1c11a015768a', temple_sacrifice_obligation__study_as_occupation, forecloses).
narrative_ontology:cs_reading_relation('f623b739-d45a-4352-8d06-1c11a015768a', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('f623b739-d45a-4352-8d06-1c11a015768a', foundational, study_non_fulfilling_substitute).
narrative_ontology:cs_axiom_status(study_non_fulfilling_substitute, holdable).
narrative_ontology:cs_axiom_grounding('f623b739-d45a-4352-8d06-1c11a015768a', study_non_fulfilling_substitute, deontological).
narrative_ontology:cs_axiom('f623b739-d45a-4352-8d06-1c11a015768a', foundational, obligation_binding_despite_unperformability).
narrative_ontology:cs_axiom_status(obligation_binding_despite_unperformability, holdable).
narrative_ontology:cs_axiom_grounding('f623b739-d45a-4352-8d06-1c11a015768a', obligation_binding_despite_unperformability, deontological).
narrative_ontology:cs_reference_frame('f623b739-d45a-4352-8d06-1c11a015768a', temple_cult_active).
narrative_ontology:cs_drift_state('f623b739-d45a-4352-8d06-1c11a015768a', post_temple_contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('f623b739-d45a-4352-8d06-1c11a015768a', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, rabbinic_establishment).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, torah_study_institutions).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, jewish_community).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, oral_torah_authority).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, bindingness_despite_exile).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the halakhic ruling that Torah study of sacrificial law preserves knowledge for future Temple restoration but does not discharge the personal or communal obligation. Derives legitimacy from being the sole authority capable of administering an unperformable divine commandment. Exit from this role would mean relinquishing the interpretive monopoly on the kernel.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, rabbinic_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Bound by a divine commandment they cannot physically perform. Directed to intensive study of the sacrificial laws as the only permissible activity, which provides no halakhic fulfillment. Bears the burden of continuous non-compliance and the identity fusion that makes abandoning the framework costly.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, jewish_community, payer,
    organized, generational, identity_locked, global).

% Receive sustained institutional resources, curricular priority, and communal prestige from the mandate to study sacrificial law in lieu of performance. They do not set the halakhic agenda but benefit from the labor and attention the constraint directs toward their domain.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, torah_study_institutions, beneficiary,
    organized, generational, constrained, global).

% Hereditary priests who would perform the sacrificial service if the Temple stood. Their concrete ritual role is supplanted by study; they are not the authority determining the substitution logic and their voice in halakhic adjudication is marginalized on this question.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, kohanim, excluded,
    moderate, generational, constrained, global).

% Observe from outside the halakhic commitment system, analyzing how the maintenance of an unfulfillable commandment structures authority, preserves communal boundaries, and coordinates diasporic identity across millennia.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, academic_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_archiving, rabbinic_establishment).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_archiving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves detailed textual and procedural knowledge of Temple sacrifice across generations of exile, ensuring that if the Temple is restored, the practice can be reinstated without loss of textual or procedural memory.
% TRANSFER_FUNCTION: Moves communal religious labor from physical performance to textual study, and transfers interpretive authority and communal deference to the rabbinic establishment that administers the substitution.
% ABSENT_VOICES: The kohanim, who would emphasize actual priestly performance, and advocates of messianic suspension or study-as-fulfillment are structurally excluded from halakhic agenda-setting; their absence is what allows the binding-but-unfulfilled reading to remain hegemonic.
% DISAPPEARANCE_RATIONALE: If the obligation were declared fulfilled, suspended, or abolished, the central justification for intensive study of sacrificial law would weaken, rabbinic authority would lose a key sphere of unperformable jurisdiction, and communal energy would redistribute toward performable commandments or alternative frameworks.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE removed the only legitimate site for biblical sacrifices, creating an immediate crisis of halakhic continuity and ritual practice.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians and archaeologists corroborate the Temple's destruction and continued absence; the rabbinic establishment itself treats the loss as a historical fact. The corroboration that the founding problem is dead comes from outside the benefiting parties.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_archiving, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate: the constraint does not extract material rents, but it extracts continuous religious labor (study) and defers halakhic fulfillment indefinitely, consolidating authority. Suppression (0.62) is moderate-high because the authority must actively rule out competing readings (study-as-occupation, messianic suspension) to maintain the binding-but-unfulfilled status. Theater ratio (0.45) reflects that much study is genuine preservation, but a substantial share has become performative maintenance of rabbinic jurisdiction over an unperformable domain. Accessibility collapse (0.70) is high because, within the halakhic frame, the only permissible responses to the destroyed Temple are mourning, study, or passive waiting; active sacrificial performance or halakhic suspension are institutionally barred. Resistance (0.35) is low-moderate: historical breakaway movements contested the rabbinic substitution logic, but within contemporary rabbinic Judaism the reading is largely hegemonic.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic seat experiences the constraint as a heavy responsibility of guardianship and legitimate continuity; the community seat experiences it as a burden of unfulfillable obligation and deferred redemption. The engine computes this divergence from the structural data: identical spatial scope and similar time horizons, but opposite beneficiary/payer roles and identity-locked exit that amplifies extraction for the community while subsidizing authority for the rabbis.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic establishment is declared a beneficiary and agenda-setter: it collects interpretive monopoly and communal deference (low directionality, subsidized effective extraction). The Jewish community is declared a victim/payer: it bears the cost of unfulfilled commandment and non-remedial study labor (high directionality, amplified effective extraction). Torah study institutions collect resources but do not set the agenda, placing them at moderate-low directionality. The kohanim are excluded, receiving no flow from the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preserving halakhic continuity immediately after 70 CE â is dead. The Temple has been absent for nearly two millennia. The arrangement persists because it now serves identity coordination and authority maintenance. This is mandatrophy: the constraint has outlived its original justification but remains structurally entrenched. The classification as tangled_rope captures that genuine coordination (preservation of knowledge, communal continuity) still occurs through the same structure that extracts authority and defers fulfillment. It prevents mislabeling the arrangement as a pure snare (there is real preservation value) or as a pure rope (the authority asymmetry and perpetual non-compliance are real extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archiving_vs_occupation_boundary,
    'Does the study of sacrificial law constitute a halakhic fulfillment or merely a non-remedial preservation of knowledge?',
    'Comparative analysis of Talmudic sources and medieval responsa; detection of whether study generates halakhic credit or only mnemonic continuity.',
    'If study fulfills, the constraint demotes to lower extraction (possibly Rope); if study does not fulfill, the community remains in perpetual halakhic default.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archiving_vs_occupation_boundary, conceptual, 'Boundary between archival study and fulfilling study').

omega_variable(
    messianic_suspension_status,
    'Is the sacrificial obligation actively binding in the post-Temple period, or is it in a state of messianic suspension?',
    'Historical theology survey of halakhic and non-halakhic movements; detection of whether non-performance constitutes violation or permitted waiting.',
    'If suspended, the victim set empties and extraction collapses; if binding, the extraction is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_suspension_status, conceptual, 'Whether the obligation is suspended or binding').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative readings structural or internalized?',
    'Post-exit trajectory: do communities that leave rabbinic authority continue to treat the obligation as binding-unfulfilled, or do they adopt alternative readings?',
    'If internalized, suppression is higher than structural measure suggests and persists outside institutional enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temple_sacrifice_study_archiving_tr_t70, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 70, 0.1).
narrative_ontology:measurement(temple_sacrifice_study_archiving_tr_t250, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 250, 0.25).
narrative_ontology:measurement(temple_sacrifice_study_archiving_tr_t600, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 600, 0.35).
narrative_ontology:measurement(temple_sacrifice_study_archiving_tr_t1200, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1200, 0.42).
narrative_ontology:measurement(temple_sacrifice_study_archiving_tr_t1800, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1800, 0.45).
narrative_ontology:measurement(temple_sacrifice_study_archiving_tr_t2026, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 2026, 0.45).

% Extraction over time
narrative_ontology:measurement(temple_sacrifice_study_archiving_be_t70, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 70, 0.3).
narrative_ontology:measurement(temple_sacrifice_study_archiving_be_t250, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 250, 0.45).
narrative_ontology:measurement(temple_sacrifice_study_archiving_be_t600, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 600, 0.55).
narrative_ontology:measurement(temple_sacrifice_study_archiving_be_t1200, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1200, 0.6).
narrative_ontology:measurement(temple_sacrifice_study_archiving_be_t1800, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1800, 0.58).
narrative_ontology:measurement(temple_sacrifice_study_archiving_be_t2026, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(temple_sacrifice_study_archiving_su_t70, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 70, 0.35).
narrative_ontology:measurement(temple_sacrifice_study_archiving_su_t250, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 250, 0.6).
narrative_ontology:measurement(temple_sacrifice_study_archiving_su_t600, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 600, 0.65).
narrative_ontology:measurement(temple_sacrifice_study_archiving_su_t1200, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1200, 0.62).
narrative_ontology:measurement(temple_sacrifice_study_archiving_su_t1800, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1800, 0.6).
narrative_ontology:measurement(temple_sacrifice_study_archiving_su_t2026, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the temple_sacrifice_obligation kernel. The kernel conflates three structurally distinct claims: study as occupation (study fulfills), study as archiving (study preserves only), and messianic suspension (obligation paused). Each has distinct epsilon, beneficiary structure, and classification. They form a constraint family linked by shared kernel origin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
