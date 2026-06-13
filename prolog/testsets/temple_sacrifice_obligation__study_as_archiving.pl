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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Temple Sacrifice Obligation — Study-as-Archiving Reading
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   After the Roman destruction of the Temple in 70 CE, Jewish law faced an
 *   unprecedented crisis: the obligation to offer animal sacrifices remained
 *   binding under divine command, but the physical location and authority to
 *   perform the sacrifices were destroyed and remained absent for 2,000
 *   years. The rabbinic tradition responded by creating a legal framework in
 *   which study of sacrifice law substitutes for performance. Under the
 *   'study-as-archiving' reading, this study preserves knowledge for possible
 *   future Temple restoration but explicitly does NOT fulfill the original
 *   obligation — the law remains binding, unperformed, and unresolved. This
 *   reading stands in structural contrast to two sibling readings:
 *   messianic-suspension (the obligation is suspended until the Temple
 *   returns) and study-as-occupation (study itself counts as occupying the
 *   obligation). The study-as-archiving reading is the most extractive: it
 *   maintains binding status while denying that any substitute satisfies the
 *   law, leaving practitioners in permanent non-compliance. Authority
 *   maintains control over the definition of compliance itself.
 *
 * KEY AGENTS:
 *   - rabbinic_interpretive_authority: maintains the binding status and authorizes the specific reading (agenda-setter)
 *   - talmudic_study_community: performs substitute obligation knowing it does not fulfill (beneficiary + payer, identity-locked)
 *   - jewish_legal_practitioners: navigate the impossibility under this reading's framework (payer)
 *   - competing_halakhic_readings: excluded alternatives that would dissolve the non-compliance (excluded)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.58).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.41).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.58).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Temple Sacrifice Obligation — Study-as-Archiving Reading").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious/halakhic").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, 'e63b3208-78d3-463a-8a1e-5dc8e8b07c86').
narrative_ontology:cs_kernel_codification('e63b3208-78d3-463a-8a1e-5dc8e8b07c86', fixed_text).
narrative_ontology:cs_authority_grounding('e63b3208-78d3-463a-8a1e-5dc8e8b07c86', lineage).
narrative_ontology:cs_interpretation_layer_present('e63b3208-78d3-463a-8a1e-5dc8e8b07c86').
narrative_ontology:cs_reading_relation('e63b3208-78d3-463a-8a1e-5dc8e8b07c86', temple_sacrifice_obligation__messianic_suspension, forecloses).
narrative_ontology:cs_reading_relation('e63b3208-78d3-463a-8a1e-5dc8e8b07c86', temple_sacrifice_obligation__study_as_occupation, forecloses).
narrative_ontology:cs_axiom('e63b3208-78d3-463a-8a1e-5dc8e8b07c86', foundational, study_does_not_fulfill_obligation).
narrative_ontology:cs_axiom_status(study_does_not_fulfill_obligation, holdable).
narrative_ontology:cs_axiom_grounding('e63b3208-78d3-463a-8a1e-5dc8e8b07c86', study_does_not_fulfill_obligation, deontological).
narrative_ontology:cs_axiom('e63b3208-78d3-463a-8a1e-5dc8e8b07c86', foundational, binding_status_persists_despite_impossibility).
narrative_ontology:cs_axiom_status(binding_status_persists_despite_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('e63b3208-78d3-463a-8a1e-5dc8e8b07c86', binding_status_persists_despite_impossibility, conventional).
narrative_ontology:cs_reference_frame('e63b3208-78d3-463a-8a1e-5dc8e8b07c86', perpetual_binding_obligation_despite_temple_destruction).
narrative_ontology:cs_drift_state('e63b3208-78d3-463a-8a1e-5dc8e8b07c86', contemporary_post_restoration_improbability, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e63b3208-78d3-463a-8a1e-5dc8e8b07c86', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, interpretive_authority_structure).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, legal_preservation_community).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, halakhic_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, talmudic_study_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, temple_restoration_movement).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, talmudic_study_community).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, jewish_legal_practitioners).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, perpetual_binding_status_of_law).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, interpretive_authority_over_suspension).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the authoritative interpretation of Jewish law. Declares and maintains that the Temple sacrifice obligation remains binding despite the Temple's destruction (70 CE), but can be fulfilled through study and textual preservation rather than actual sacrifice. Administers the interpretive rules that distinguish study-as-archiving from study-as-occupation. Collects no material benefit but maintains institutional authority over the definition of compliance and the meaning of the law itself.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, rabbinic_interpretive_authority, agenda_setter,
    institutional, generational, analytical, global).

% Practices intensive study of sacrifice law, preserving detailed knowledge of Temple procedures, offerings, and regulations for 2,000 years. Gains institutional status, community identity, and religious legitimacy through this specialized knowledge work. Simultaneously bears the cost of performing a substitute obligation that, under this reading, does not actually fulfill the original command — they study knowing they do not complete the act the law demands.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, talmudic_study_community, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_archiving, talmudic_study_community, payer).

% Seeks literal Temple reconstruction and animal sacrifice resumption. Under this reading, preserved knowledge is the prerequisite infrastructure for future actual performance — the study corpus is the archive that makes restoration possible when political conditions permit. Gains the intellectual and legal scaffolding required to execute the law, but only in a future state.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, temple_restoration_movement, beneficiary,
    moderate, civilizational, constrained, regional).

% All practitioners of Jewish law must navigate the binding status of the sacrifice obligation while acknowledging its impossibility. They bear the cognitive and institutional load of maintaining an unperformable law as binding. The interpretive authority enforces the distinction: study-as-archiving preserves knowledge but does not satisfy; other readings (study-as-occupation, messianic-suspension) offer different escape routes that this reading explicitly forecloses.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, jewish_legal_practitioners, payer,
    powerful, biographical, constrained, global).

% Alternative interpretations (messianic-suspension, study-as-occupation) that would dissolve the non-compliance. This reading's authority structure actively maintains the binding status despite impossibility, which forecloses the competing readings' core premises. They would argue the obligation is suspended or that study constitutes legitimate occupation; this reading denies both and insists study is archiving only.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, competing_halakhic_readings, excluded,
    institutional, generational, trapped, global).

% Observe and sometimes contest the authority structure's maintenance of binding status. Different movements (Orthodox, Conservative, Reform, Reconstructionist) diverge in how they read the obligation, but all operate within or against the interpretive authority's declared framework. They testify to the burden of maintaining unperformable law as binding.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, contemporary_jewish_movement_leaders, observer,
    powerful, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_archiving, rabbinic_interpretive_authority).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_archiving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves precise knowledge of Temple sacrifice law, regulations, and procedures through intensive textual study and transmission — ensuring that if the Temple is ever rebuilt, practitioners would have detailed knowledge of how to execute the law correctly. The coordination problem is: how do you maintain expertise in an activity that is permanently prevented?
% TRANSFER_FUNCTION: Moves the burden of non-compliance from a diffuse legal crisis (the entire post-Temple period is violation) onto the shoulders of the study community and legal practitioners, who accept the burden of performing a substitute that explicitly does NOT fulfill the obligation. The transfer is from abstract law to concrete human practice: the law remains binding, but its execution is transferred to study rather than performance.
% ABSENT_VOICES: Practitioners who have abandoned the obligation entirely; movements that deny the binding status (some Reform and Reconstructionist voices); skeptics outside the Jewish tradition who question the coherence of binding law that is permanently unexecutable. These voices are structurally excluded from the interpretive authority's framework — they would argue for dissolution or reframing, but the framework does not admit their standing.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and one of the competing readings became authoritative, the entire post-Temple Jewish legal landscape would reorganize: messianic-suspension reading would relieve practitioners of the burden of maintaining binding status; study-as-occupation reading would convert study into direct fulfillment rather than archiving. The legal and institutional structure of Jewish practice would shift fundamentally.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), Jewish law remained binding but its central ritual obligation became impossible to perform. The question became: how does Jewish law persist as binding law when its signature act cannot be executed? Study-as-archiving answers: by declaring that study preserves knowledge for restoration while explicitly not claiming study fulfills the obligation.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic sources (Talmud, Mishnah, medieval Rishonim) attest the founding problem directly — the entire post-Temple halakhic corpus addresses the impossibility of sacrifice. Contemporary legal theorists outside the Jewish tradition (legal philosophers, historians) confirm that the problem remains a defining feature of Jewish law: a 2,000-year period of maintaining binding law that is unexecutable. The status is corroborated by external observers including historians of religion and legal theorists who document the unique structural position of Jewish law in maintaining unperformable obligations.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).

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
 *   Extractiveness is moderate (0.58) because the constraint operates by maintaining binding status of unperformable law — the interpretive authority retains control over the definition of compliance, and the study community bears the burden of permanent non-fulfillment. The constraint is not pure extraction (there is genuine coordination in preserving knowledge) but extraction is substantial because the authority enforces a reading that explicitly denies that study satisfies the obligation, preventing practitioners from resolving the legal burden. Theater ratio is high (0.62) because much of the performed study activity is performative maintenance of binding status rather than functional preservation — the question 'for whom is knowledge being preserved?' makes clear that the immediate beneficiary is the authority structure that maintains the binding claim, not future Temple-builders who may never exist. The measurement series shows slow accumulation: extractiveness and theater increase modestly over 1,900 years as institutional elaboration deepens the framework. Suppression requirement is lower (0.41) because the mechanism is interpretive rather than coercive — the suppression is cognitive and institutional (accepting the binding status as binding despite impossibility) rather than physical. This reading is characterized by the authority's ability to foreclose exit routes: practitioners cannot declare the obligation satisfied, suspended, or dissolved — only study-as-archiving is authorized.
 *
 * PERSPECTIVAL GAP:
 *   The authority's view: 'This reading preserves the law binding while keeping knowledge alive for restoration.' The study community's view: 'We study knowing we do not fulfill, which is the structure of our religious non-compliance.' Competing readings' view: 'This reading forecloses our resolution, leaving the legal crisis unresolved.' These three views are incommensurable within a single framework — one reading cannot accommodate all three positions simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic interpretive authority is the structural beneficiary (d near 0.0–0.2): it controls the definition of the law, decides what study means, maintains institutional authority over interpretation, and collects no cost for the arrangement. The study community is dual-positioned: they are partly beneficiary (they gain religious status, community identity, institutional standing from specialized knowledge work) but they are also partly victim (they are bound by the authority's explicit declaration that study does not fulfill, leaving them in permanent non-compliance). Their directionality is moderate-to-high (d around 0.4–0.6) because identity-lock and the binding status create asymmetric burden. Jewish legal practitioners generally are victims (d high, around 0.65–0.85) because they must navigate the impossible law under this reading's framework. The unfulfilled divine command and the alternative readings are conceptual victims — they cannot be satisfied or substituted under this reading. Competing readings are excluded, which is a form of targeting (d high for their voices, trapped exit from the framework).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as tangled rope rather than snare because it genuinely coordinates knowledge preservation (a real collective problem solved through organized study) AND enforces extraction through maintained binding status. It is not a pure snare because the coordination function is authentic — the knowledge DOES need to be preserved, and the study community DOES solve that problem. But it is tangled rope rather than pure rope because the authority enforces binding status precisely to prevent practitioners from declaring the obligation satisfied, which maintains the authority's control over interpretation. The mandate (preserve Temple sacrifice knowledge) has not outlived its function — Temple restoration remains theologically live in Jewish tradition. However, the reading enforces a subsidiary mandate: 'maintain binding status despite impossibility,' which persists primarily to preserve the authority structure's role. This subsidiary mandate exhibits mandatrophy — it persists to keep the authority functional, not because the primary problem requires it. A piton reading would deny that knowledge preservation serves any real function; this reading acknowledges the preservation function but adds a burden layer (non-fulfillment doctrine) that serves primarily institutional continuity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_status_persistence,
    'What is the functional purpose of maintaining binding status of a law that is universally acknowledged as unexecutable for 2,000 years? Is it genuine legal preservation (knowledge for restoration) or institutional maintenance of authority?',
    'Comparative study of other unexecutable religious laws across traditions and their authority structures; analysis of whether binding status claims change if Temple restoration becomes practically impossible (shifts from future-restoration to permanent-theological-claim).',
    'If binding status serves primarily authority maintenance, the constraint reclassifies from tangled rope toward snare. If it serves genuine knowledge preservation with real restoration possibility, the coordination function strengthens and extractiveness drops.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_status_persistence, conceptual, 'Whether binding status is functional preservation or institutional performance.').

omega_variable(
    knowledge_preservation_audience,
    'For whom exactly is Temple sacrifice knowledge being preserved? If Temple restoration is treated as impossible (rather than merely future), does the knowledge preservation function collapse?',
    'Historical analysis of how different Jewish movements have framed Temple restoration (as lived theological expectation vs. abstract symbolic commitment vs. impossible fantasy); examination of whether knowledge is actually maintained or primarily theatrically invoked.',
    'If knowledge is preserved for an actual restoration movement (even minority), coordination function is real and extractiveness is lower. If restoration is purely symbolic, knowledge preservation is theater and extractiveness is higher (snare-adjacent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_preservation_audience, empirical, 'Whether preserved knowledge serves a real functional need or primarily institutional performance.').

omega_variable(
    competing_reading_foreclosure,
    'Does this reading''s insistence that study does NOT fulfill the obligation foreclose the other readings through logical contradiction, or through institutional suppression of alternative interpretations?',
    'Halakhic analysis of whether messianic-suspension and study-as-occupation are logically incompatible with study-as-archiving, or whether they are merely institutionally excluded by the authority structure.',
    'If logically foreclosed, the relation is genuine conceptual contradiction (forecloses). If institutionally suppressed, they coexist or influence rather than foreclose, and the interpretive authority is maintaining control through suppression rather than through argument.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_reading_foreclosure, conceptual, 'Whether alternative readings are logically ruled out or institutionally suppressed.').

omega_variable(
    study_community_identity_lock,
    'Is the study community''s identity-lock authentic (they genuinely identify as preservationists of sacred law) or is it induced by the authority structure''s enforcement of binding status?',
    'Ethnographic and historical study of study community recruitment, attrition, and stated motivations; examination of whether practitioners who leave the reading report identity-shift or escape from coercive framework.',
    'If authentic identity-fusion, the study community''s directionality is lower (they consent to non-fulfillment as part of sacred identity). If induced, the identity-lock is a suppression mechanism and directionality is higher (they are trapped by the framework).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_community_identity_lock, empirical, 'Whether study community identity-lock is self-constituted or enforced.').

omega_variable(
    reading_family_decomposition,
    'Are the three readings of temple_sacrifice_obligation structurally different constraints (different epsilon values, different victim sets) or are they one constraint read differently?',
    'Prolog comparison of the three readings'' epsilon values, beneficiary/victim structures, and authority frames. If the readings have substantially different eps and different governance structures, they are separate constraints in a family. If they are merely interpretive alternatives on a single epsilon, they are one constraint with multiple readings.',
    'If separate constraints: each has its own classification, network edges link them as constraint family. If one constraint: the choice of reading is observer-relative and the constraint type is genuinely contested. This determines whether the corpus treats them as three stories or as one story with contested classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_family_decomposition, conceptual, 'Whether the three readings are separate constraints or one constraint with multiple interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t70, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 70, 0.45).
narrative_ontology:measurement_basis(temp_tr_t70, observed).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 500, 0.5).
narrative_ontology:measurement_basis(temp_tr_t500, observed).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1000, 0.56).
narrative_ontology:measurement_basis(temp_tr_t1000, observed).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1500, 0.6).
narrative_ontology:measurement_basis(temp_tr_t1500, observed).
narrative_ontology:measurement(temp_tr_t1900, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1900, 0.61).
narrative_ontology:measurement_basis(temp_tr_t1900, observed).
narrative_ontology:measurement(temp_tr_t2026, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 2026, 0.62).
narrative_ontology:measurement_basis(temp_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t70, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 70, 0.35).
narrative_ontology:measurement_basis(temp_be_t70, observed).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 500, 0.42).
narrative_ontology:measurement_basis(temp_be_t500, observed).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1000, 0.48).
narrative_ontology:measurement_basis(temp_be_t1000, observed).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1500, 0.54).
narrative_ontology:measurement_basis(temp_be_t1500, observed).
narrative_ontology:measurement(temp_be_t1900, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1900, 0.56).
narrative_ontology:measurement_basis(temp_be_t1900, observed).
narrative_ontology:measurement(temp_be_t2026, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 2026, 0.58).
narrative_ontology:measurement_basis(temp_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t70, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 70, 0.25).
narrative_ontology:measurement_basis(temp_su_t70, observed).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 500, 0.3).
narrative_ontology:measurement_basis(temp_su_t500, observed).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1000, 0.36).
narrative_ontology:measurement_basis(temp_su_t1000, observed).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1500, 0.39).
narrative_ontology:measurement_basis(temp_su_t1500, observed).
narrative_ontology:measurement(temp_su_t1900, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1900, 0.4).
narrative_ontology:measurement_basis(temp_su_t1900, observed).
narrative_ontology:measurement(temp_su_t2026, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 2026, 0.41).
narrative_ontology:measurement_basis(temp_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_archiving, 0.12).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__messianic_suspension).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__study_as_occupation).

% DUAL FORMULATION NOTE:
% This constraint is ONE reading of the kernel 'temple_sacrifice_obligation.' The kernel is the ancient Jewish law commanding Temple sacrifice. The three readings instantiate different constraints: messianic-suspension (obligation suspended pending restoration), study-as-archiving (study preserves knowledge but does not fulfill — THIS constraint), and study-as-occupation (study constitutes legitimate occupation). All three readings share the same kernel but have different epsilon values and different authority frames. They form a constraint family linked by network.affects_constraints. The decomposition respects epsilon-invariance: each reading has a different structural relationship to the binding status, different victim sets, and different extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_obligation__study_as_archiving, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
