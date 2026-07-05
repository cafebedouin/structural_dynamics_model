% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__adaptive_fiction_reading, []).

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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Immutability as Adaptive Fiction (Constitutional Cover for Covert Reform)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   Spartan political rhetoric held the laws of Lycurgus to be divinely
 *   sanctioned, unchangeable, and identical across centuries. In practice,
 *   the ephorate and gerousia issued a continuous stream of interpretive
 *   rulings — on citizenship qualification, land tenure, military manpower,
 *   and helot status — that adjusted the substantive content of 'ancestral
 *   custom' to fit contemporary elite interests, all while the public
 *   ideology insisted nothing had changed. The tangled-rope reading holds
 *   that this arrangement genuinely coordinated a small ruling class against
 *   internal factionalism (a real function) while extracting compliance from
 *   helots, perioikoi, and declining citizens who had no comparable
 *   interpretive access.
 *
 * KEY AGENTS:
 *   - spartan_ephorate: administers covert reinterpretation, benefits from unchallengeable authority
 *   - dual_kingship: trades sacral legitimacy for negotiated flexibility
 *   - hypomeiones_declining_citizens: bears the property threshold's selectively rigid application
 *   - helot_population: bears the weight of an ideology of permanence that in practice bent whenever elite military need required
 *   - later_greek_historians: external corroborating witnesses to the adaptation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.58).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.45).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, tangled_rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Immutability as Adaptive Fiction (Constitutional Cover for Covert Reform)").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, 'f5905139-e70f-4756-81cd-7e5c0e7e16d1').
narrative_ontology:cs_kernel_codification('f5905139-e70f-4756-81cd-7e5c0e7e16d1', fixed_text).
narrative_ontology:cs_authority_grounding('f5905139-e70f-4756-81cd-7e5c0e7e16d1', lineage).
narrative_ontology:cs_interpretation_layer_present('f5905139-e70f-4756-81cd-7e5c0e7e16d1').
narrative_ontology:cs_reading_relation('f5905139-e70f-4756-81cd-7e5c0e7e16d1', lycurgan_laws__sacral_fidelity_reading, forecloses).
narrative_ontology:cs_reading_relation('f5905139-e70f-4756-81cd-7e5c0e7e16d1', lycurgan_laws__demographic_trap_reading, influences).
narrative_ontology:cs_axiom('f5905139-e70f-4756-81cd-7e5c0e7e16d1', foundational, immutability_rhetoric_is_functional_cover).
narrative_ontology:cs_axiom_status(immutability_rhetoric_is_functional_cover, holdable).
narrative_ontology:cs_axiom_grounding('f5905139-e70f-4756-81cd-7e5c0e7e16d1', immutability_rhetoric_is_functional_cover, empirically_contingent).
narrative_ontology:cs_axiom('f5905139-e70f-4756-81cd-7e5c0e7e16d1', foundational, interpretive_monopoly_constitutes_the_extraction).
narrative_ontology:cs_axiom_status(interpretive_monopoly_constitutes_the_extraction, holdable).
narrative_ontology:cs_axiom_grounding('f5905139-e70f-4756-81cd-7e5c0e7e16d1', interpretive_monopoly_constitutes_the_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('f5905139-e70f-4756-81cd-7e5c0e7e16d1', lycurgan_founding_ordinance).
narrative_ontology:cs_drift_state('f5905139-e70f-4756-81cd-7e5c0e7e16d1', classical_period_manpower_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f5905139-e70f-4756-81cd-7e5c0e7e16d1', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_ephorate).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, dual_kingship).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, gerousia_elders).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, hypomeiones_declining_citizens).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, perioikoi_subject_populations).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, helot_population).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, lycurgan_founding_myth).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, eunomia_good_order_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five annually elected ephors administer the rhetra, adjudicate disputes, and issue rulings that in practice reinterpret Lycurgan custom to fit present circumstance (land redistribution pressures, military manpower shortfalls, treaty obligations) while publicly insisting the ancestral laws are untouched. They hold the interpretive monopoly and benefit from the immutability myth precisely because it means their adaptive rulings cannot be challenged as innovation.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_ephorate, agenda_setter,
    institutional, generational, arbitrage, national).

% The two kings command armies and preside over religious sacrifice, deriving legitimacy from claimed unbroken descent and adherence to Lycurgus's ordinance. They quietly negotiate exceptions (marriage law relaxations, citizenship grants to shore up hoplite numbers) through ephor cooperation, trading real flexibility for continued sacral prestige.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, dual_kingship, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, dual_kingship, beneficiary).

% The council of elders ratifies or blocks proposals to the assembly and functions as the interpretive gatekeeper for what counts as consistent with ancestral custom. Their prestige and veto power depend entirely on the fiction that they are guardians of something fixed, not architects of ongoing revision.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, gerousia_elders, beneficiary,
    organized, generational, arbitrage, national).

% Spartiates who fall below the property threshold for full citizenship (through inheritance division, war losses, or debt) lose citizen status under rules nominally fixed by Lycurgus but administratively enforced with inconsistent severity depending on ephor discretion and manpower needs. They bear the cost of a supposedly immutable property qualification that was in fact adjustable when convenient for the elite and rigid when it protected elite land concentration.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, hypomeiones_declining_citizens, payer,
    moderate, biographical, trapped, national).

% Dependent Laconian communities supply auxiliary troops and tribute under a constitutional order they had no part in framing and cannot petition to revise; when Sparta needed manpower it adjusted their obligations unilaterally, citing ancestral necessity rather than negotiated change.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, perioikoi_subject_populations, payer,
    powerless, generational, trapped, regional).

% The enslaved agricultural workforce bears the material weight of a system whose supposed fixity justified permanent subjugation; periodic krypteia violence and selective helot manumission (when military need required it) show the rules bending in practice while the ideology of eternal, unchangeable order justified their continued bondage.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, helot_population, payer,
    powerless, generational, trapped, regional).

% Writers like Aristotle, Plutarch, and Thucydides document specific instances of Spartan law adapting (mothakes admission, brasidas-era helot grants, currency and land-tenure workarounds) while noting the persistent Spartan self-presentation as changeless — providing the outside evidence that the immutability claim was rhetorical rather than descriptive.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, later_greek_historians, observer,
    analytical, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The immutability claim solves a genuine legitimation problem: a small warrior elite governing a much larger subject and dependent population needs its rules to appear beyond dispute, since visible negotiability would invite constant renegotiation demands from helots, perioikoi, and declining citizens alike. Claimed fixity lowers the cost of rule enforcement by removing 'why not for us too' as a live question.
% TRANSFER_FUNCTION: Interpretive discretion over what counts as 'ancestral custom' flows to the ephors, kings, and gerousia, who use it to preserve elite land concentration and military manpower on their own terms; the cost of the resulting rigidity in areas that protect elite privilege (property-based citizenship) and the cost of covert flexibility in areas that serve elite interests (manpower levies, helot grants) both fall on the powerless — hypomeiones, perioikoi, and helots absorb whichever mode of the law currently disadvantages them.
% ABSENT_VOICES: Helots and perioikoi never sat in any body that could rule on what Lycurgan custom actually required; their objections, where recorded at all, survive only through hostile Spartan or later Athenian sources, not their own testimony. Declining citizens (hypomeiones) had some voice through the assembly but no formal channel to contest the property threshold's application.
% DISAPPEARANCE_RATIONALE: If the fiction of unchangeable ancestral law were publicly abandoned, the ephorate's authority to issue binding 'interpretations' would collapse into visible ad hoc rule-making, inviting immediate challenge from hypomeiones over citizenship criteria and from helots over the terms of servitude; the elite's capacity to adjust the rules without appearing to change them would be gone, and every past adaptation would become contestable precedent rather than settled custom.
% FOUNDING_PROBLEM: A small Spartiate elite needed to govern a much larger, resentful subject and dependent population (helots outnumbering citizens many times over) while preventing internal elite factionalism from fracturing military cohesion — the rhetra of Lycurgus was built to fix both problems at once by claiming divine, unrevisable sanction for the resulting arrangement.
% FOUNDING_PROBLEM_CORROBORATION: Aristotle's Politics and Plutarch's Lycurgus (writing centuries later, from outside Sparta's own self-legitimating tradition) document specific adaptive rulings and note the gap between Spartan rhetoric of changelessness and observed practice; modern epigraphic and archaeological work on Spartan land tenure similarly shows property arrangements shifting across the classical period despite the constitutional claim of fixity. No source internal to the Spartan elite itself concedes the adaptation; the corroboration is entirely external.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__adaptive_fiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__adaptive_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and theater ratio (0.72, rising across the interval) reflect that the immutability claim functioned increasingly as legitimating performance for a system whose actual operation drifted from its founding rhetoric — the widening gap between claimed fixity and observed adaptive practice is itself the theater. Suppression (0.45) is moderate rather than high because much of the system's stability rested on ideological acceptance (the myth was widely internalized, not merely coerced) alongside real coercive machinery (krypteia, military discipline) applied unevenly. Accessibility collapse (0.35) and resistance (0.4) are moderate: alternatives to the Lycurgan framework were not fully closed off — other Greek poleis offered visible contrasting models, and internal resistance (hypomeiones agitation, occasional helot revolt) was real, though rarely successful.
 *
 * PERSPECTIVAL GAP:
 *   From the ephors' seat, the arrangement looks like faithful stewardship of ancestral wisdom, continuously and carefully applied to changing circumstance — genuine, necessary coordination. From a helot's or hypomeion's seat, the same set of rulings looks like an unaccountable elite changing the rules whenever convenient while forbidding anyone else from proposing the word 'change.' The engine should register this divergence in the computed per-seat types rather than have it asserted here.
 *
 * DIRECTIONALITY LOGIC:
 *   The ephorate, gerousia, and (with more constrained latitude) the dual kingship are structural beneficiaries: they hold the interpretive monopoly that lets them adjust substantive rules while collecting the legitimacy premium of claimed permanence. Hypomeiones, perioikoi, and helots are targets: they bear the costs of both the rigid applications (property threshold enforcement against them) and the flexible applications (unilateral obligation increases) without any comparable interpretive access of their own. The asymmetry is structural, not incidental — only those who administer the fiction can safely deviate from its literal content.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (elite cohesion + subject-population control) remained partially live throughout Spartan classical history, but the specific mechanism — claimed legal changelessness — increasingly served the narrower function of insulating elite land concentration from citizenship-threshold pressure, rather than the broader coordination it originally solved. The founding_problem_status of 'contested' reflects that Sparta's own elite never conceded any mandatrophy (the myth persisted to the end), while external observers (Aristotle, Plutarch) documented the drift explicitly — the classic asymmetry between self-report and outside corroboration that this framework is built to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extent_of_covert_adaptation,
    'How extensive was actual Spartan legal-institutional adaptation relative to the immutability claim — occasional pragmatic exceptions, or a continuous shadow process of reinterpretation comparable in scope to ordinary lawmaking elsewhere?',
    'Systematic collation of attested rulings (citizenship grants, land-tenure adjustments, helot manumissions) against the chronology of the immutability rhetoric''s public assertions, cross-checked against epigraphic and archaeological evidence for property distribution changes.',
    'If adaptation was extensive and continuous, the tangled_rope classification here is well-supported (real coordination function plus real extraction via unequal interpretive access). If adaptation was rare and marginal, this reading collapses toward the sacral_fidelity_reading''s premise and the extraction charge weakens substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_covert_adaptation, empirical, 'Whether covert adaptation was systemic or occasional — the core empirical claim this reading rests on.').

omega_variable(
    kernel_reading_choice_signal,
    'What in the source material justifies choosing the adaptive_fiction_reading over the sacral_fidelity_reading or demographic_trap_reading as the operative frame for THIS story, given all three are defensible readings of the same kernel?',
    'This is a conceptual, not empirical, question: the choice tracks which sources one privileges (Spartan self-presentation vs. external historians) and which failure mode one is trying to make legible (legitimation-cover extraction vs. genuine rigidity-driven collapse vs. sacred literalism). No dataset resolves it; it is a framing decision made explicit here rather than left implicit.',
    'Adopting sacral_fidelity_reading instead would classify this constraint as a mountain (or near-mountain) claim taken at face value, eliminating the tangled_rope''s beneficiary/victim asymmetry entirely — the ephorate would appear as faithful administrators, not interpretive rent-collectors. Adopting demographic_trap_reading would keep extraction present but relocate its source from interpretive discretion to enforcement failure against genuine rigidity, changing which stakeholders count as agenda_setters versus mere administrators of a broken system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_choice_signal, conceptual, 'Documents which reading was selected and what the alternative framings would change — the committer-axis disclosure required by kernel authoring.').

omega_variable(
    helot_testimony_absence,
    'Given that no helot or perioikoi source survives independently of hostile Spartan or foreign framing, how much of the ''victim'' narrative here is itself a later (often Athenian or Roman-era) reconstruction rather than contemporaneous testimony?',
    'Careful source-critical work distinguishing classical-period attestation from later, ideologically motivated retrospective accounts (e.g., Hellenistic and Roman moralizing about Sparta).',
    'If the victim narrative is substantially a later construction, the extractiveness and suppression metrics here may be somewhat anachronistic impositions rather than contemporaneous structural facts — though the underlying institution of helotry is independently well-attested regardless of interpretive framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(helot_testimony_absence, empirical, 'Source-critical uncertainty about the provenance of victim-perspective evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(lycu_tr_t0, projected).
narrative_ontology:measurement(lycu_tr_t80, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 80, 0.45).
narrative_ontology:measurement_basis(lycu_tr_t80, projected).
narrative_ontology:measurement(lycu_tr_t160, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 160, 0.55).
narrative_ontology:measurement_basis(lycu_tr_t160, projected).
narrative_ontology:measurement(lycu_tr_t240, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 240, 0.63).
narrative_ontology:measurement_basis(lycu_tr_t240, projected).
narrative_ontology:measurement(lycu_tr_t320, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 320, 0.68).
narrative_ontology:measurement_basis(lycu_tr_t320, projected).
narrative_ontology:measurement(lycu_tr_t400, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 400, 0.72).
narrative_ontology:measurement_basis(lycu_tr_t400, projected).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(lycu_be_t0, projected).
narrative_ontology:measurement(lycu_be_t80, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 80, 0.47).
narrative_ontology:measurement_basis(lycu_be_t80, projected).
narrative_ontology:measurement(lycu_be_t160, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 160, 0.5).
narrative_ontology:measurement_basis(lycu_be_t160, projected).
narrative_ontology:measurement(lycu_be_t240, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 240, 0.54).
narrative_ontology:measurement_basis(lycu_be_t240, projected).
narrative_ontology:measurement(lycu_be_t320, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 320, 0.56).
narrative_ontology:measurement_basis(lycu_be_t320, projected).
narrative_ontology:measurement(lycu_be_t400, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 400, 0.58).
narrative_ontology:measurement_basis(lycu_be_t400, projected).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(lycu_su_t0, projected).
narrative_ontology:measurement(lycu_su_t80, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 80, 0.42).
narrative_ontology:measurement_basis(lycu_su_t80, projected).
narrative_ontology:measurement(lycu_su_t160, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 160, 0.44).
narrative_ontology:measurement_basis(lycu_su_t160, projected).
narrative_ontology:measurement(lycu_su_t240, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 240, 0.45).
narrative_ontology:measurement_basis(lycu_su_t240, projected).
narrative_ontology:measurement(lycu_su_t320, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 320, 0.46).
narrative_ontology:measurement_basis(lycu_su_t320, projected).
narrative_ontology:measurement(lycu_su_t400, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 400, 0.45).
narrative_ontology:measurement_basis(lycu_su_t400, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__adaptive_fiction_reading, 0.12).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, demographic_trap_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the lycurgan_laws kernel. sacral_fidelity_reading treats the immutability claim as genuine sacred law (mountain-flavored: near-zero extraction, high accessibility_collapse, near-zero resistance). demographic_trap_reading treats the system as genuinely rigid, locating extraction/harm in the failure to adapt citizenship rules fast enough as Spartiate numbers declined (a different causal story producing possibly a snare or tangled_rope with a different victim/beneficiary map centered on enforcement failure rather than interpretive capture). This adaptive_fiction_reading is the one that locates both the coordination function and the extraction in the SAME mechanism — the interpretive monopoly itself — which is why it computes as tangled_rope rather than mountain or pure snare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
