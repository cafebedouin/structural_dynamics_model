% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__discontinuity_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: correct_latin__discontinuity_reading
 *   human_readable: Classical Latin as Authoritative Standard (Discontinuity Reading)
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   The discontinuity reading of the correct_latin kernel posits that Correct
 *   Latin is uniquely the Classical form preserved in ancient texts (Cicero,
 *   Virgil, inscriptions, legal documents) and that medieval Latin is a
 *   corruption requiring external reconstruction from textual sources rather
 *   than legitimate evolution. This reading was championed by Renaissance
 *   humanists and became institutionalized in classical philology curricula.
 *   It declares a rupture: medieval forms exit the legitimate usage set; the
 *   authority to define correctness transfers from medieval practitioners and
 *   ecclesiastical tradition to textual scholars who control manuscript
 *   interpretation. The claim/metric divergence is intentional: the
 *   constraint is CLAIMED as tangled_rope (genuine coordination problem of
 *   standardization + asymmetric benefit distribution) while the authored
 *   metrics show substantial extraction (0.68) and moderately high
 *   suppression (0.71), reflecting the power shift and the erasure of
 *   medieval practitioners from the legitimacy conversation. The engine
 *   computes this gap as structural mismatch.
 *
 * KEY AGENTS:
 *   - Classical philologists: institutional agenda-setters who control the standard and curriculum; benefit from the authority
 *   - Renaissance humanists: powerful beneficiaries who gain prestige and institutional power by positioning textual recovery as the only valid path
 *   - Medieval scribes: powerless targets, their practice retroactively declared corrupt and erased from legitimate usage
 *   - Living Latin practitioners: moderate-power targets, trapped between abandoning the language or constantly studying ancient texts to avoid error
 *   - Ecclesiastical authorities: organized targets, displaced from their traditional authority to teach and transmit the language
 *   - Continuity tradition (excluded): the sibling reading, systematically excluded from the legitimate hypothesis set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.68).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.71).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Classical Latin as Authoritative Standard (Discontinuity Reading)").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "intellectual_history/philology").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, '2bf773fe-668f-4faa-a1d3-c183716c6b77').
narrative_ontology:cs_kernel_codification('2bf773fe-668f-4faa-a1d3-c183716c6b77', fixed_text).
narrative_ontology:cs_authority_grounding('2bf773fe-668f-4faa-a1d3-c183716c6b77', extraction).
narrative_ontology:cs_interpretation_layer_present('2bf773fe-668f-4faa-a1d3-c183716c6b77').
narrative_ontology:cs_reading_relation('2bf773fe-668f-4faa-a1d3-c183716c6b77', correct_latin__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('2bf773fe-668f-4faa-a1d3-c183716c6b77', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('2bf773fe-668f-4faa-a1d3-c183716c6b77', foundational, classical_form_uniquely_authoritative).
narrative_ontology:cs_axiom_status(classical_form_uniquely_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('2bf773fe-668f-4faa-a1d3-c183716c6b77', classical_form_uniquely_authoritative, deontological).
narrative_ontology:cs_axiom('2bf773fe-668f-4faa-a1d3-c183716c6b77', secondary, medieval_practice_invalid_corruption).
narrative_ontology:cs_axiom_status(medieval_practice_invalid_corruption, holdable).
narrative_ontology:cs_axiom_grounding('2bf773fe-668f-4faa-a1d3-c183716c6b77', medieval_practice_invalid_corruption, empirically_contingent).
narrative_ontology:cs_reference_frame('2bf773fe-668f-4faa-a1d3-c183716c6b77', classical_author_standard).
narrative_ontology:cs_drift_state('2bf773fe-668f-4faa-a1d3-c183716c6b77', institutional_consolidation_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2bf773fe-668f-4faa-a1d3-c183716c6b77', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, renaissance_humanists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, institutional_grammarians).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_scribes).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, living_latin_practitioners).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, ecclesiastical_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, ecclesiastical_authorities).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, manuscript_editors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish and enforce the standard that Correct Latin is the form preserved in ancient texts (Cicero, Virgil, Livy). They set the curriculum, control manuscript editing, referee correctness in published works. They benefit from the authority this grants them and from the scarcity it creates — mastery of the authentic form requires years of training under their guidance. They actively suppress medieval forms as corruptions and dismiss living practitioners as inaccurate.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, classical_philologists, agenda_setter,
    institutional, generational, analytical, universal).

% Use the discontinuity reading to displace medieval scholastic authority and establish their own intellectual legitimacy. By claiming to restore Classical Latin from textual sources, they gain prestige and the authority to rewrite curricula. They benefit from the constraint because it positions their textual-recovery work as the only valid path to correct Latin. They have exit options (could embrace continuity) but choose discontinuity because it amplifies their power relative to institutional competitors.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, renaissance_humanists, beneficiary,
    powerful, generational, mobile, continental).

% Their practice of adapting Latin to contemporary use and written needs is retroactively declared corrupt and illegitimate. They are not consulted in the definition of correctness, cannot defend their usage choices, and their textual output is systematically edited and corrected by later scholars. Their decades of transmitted practice are erased from the legitimate usage set.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medieval_scribes, payer,
    powerless, biographical, trapped, local).

% Communities practicing Latin as a living language — monastic orders, academic disputants, clerical communicators — find their usage patterns classified as corruption. They cannot innovate or adapt the language to new purposes without their work being labeled deviant. The discontinuity reading denies them the legitimacy to extend Latin naturally; they are trapped between abandoning the language or constantly studying ancient texts to avoid error.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, living_latin_practitioners, payer,
    moderate, biographical, constrained, regional).

% Control the transmission of Latin through religious education and liturgy. Under the discontinuity reading they are subordinated to secular philologists who define correctness through ancient texts rather than through the Church's own tradition of transmission. They maintain the language in practice but are told their practice is corrupt; their legitimacy to teach and transmit erodes. Some Church actors benefit if they align with humanist authority; others suffer loss of control over linguistic standards.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, ecclesiastical_authorities, payer,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, ecclesiastical_authorities, beneficiary).

% Control the reproduction and correction of texts. Under the discontinuity reading they gain authority to alter medieval manuscript readings to bring them into conformity with reconstructed Classical norms. They present this as recovering the true text; they benefit from the intellectual authority this grants them and from making themselves indispensable gatekeepers between ancient authors and modern readers.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, manuscript_editors, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, manuscript_editors, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__discontinuity_reading, classical_philologists).
narrative_ontology:fixing_cost_class(correct_latin__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, teachable, and universally comparable standard for Latin: anchors the language to a fixed textual corpus (ancient authors) so learners everywhere study the same norms, and communication across regions and generations can be held to one reference point.
% TRANSFER_FUNCTION: Moves authority from medieval practitioners and ecclesiastical tradition-keepers to Renaissance philologists and classicists. Medieval scribal choices are reassigned from legitimate variation to error; the power to define correctness shifts from living communities to textual scholars who control manuscript interpretation and emendation.
% ABSENT_VOICES: Medieval practitioners themselves — scribes, ecclesiastical copyists, living-Latin speakers — are structurally excluded from the conversation about what constitutes correct Latin. They are not asked whether their forms were rule-governed, whether they understood themselves as corrupting or adapting, or whether continuous practice constitutes legitimacy. The continuity_reading would argue for them but is itself excluded from the authoritative discussion.
% DISAPPEARANCE_RATIONALE: If the discontinuity reading disappeared and medieval forms were readmitted to the legitimate usage set, the curriculum would reorganize: students would learn from medieval texts as well as ancient ones; scribal variation would be studied as adaptation rather than error; ecclesiastical practice would regain authority as a transmission path. The power differential between philologists and practitioners would flatten. Institutional gatekeeping on textual authority would collapse.
% FOUNDING_PROBLEM: After the fall of the Western Roman Empire, Latin ceased to be a native language; medieval users adapted it to new purposes and no longer had direct access to native-speaker intuition. How should the language be standardized when no living community speaks it authentically anymore?
% FOUNDING_PROBLEM_CORROBORATION: Classical philologists attest the problem is live: without a standard anchor in ancient texts, any form becomes acceptable and communication degrades. Medieval scholars and continuity-reading advocates attest the problem is differently solved: medieval practice itself becomes the standard, and innovation within that tradition is legitimate. Historians of linguistics document that the founding problem existed but that the discontinuity response was not inevitable — Byzantine scholars maintained a different answer (Greek standards); the choice was ideological, not empirically forced.
narrative_ontology:disappearance_verdict(correct_latin__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__discontinuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the interval because as the discontinuity reading becomes institutionalized, the cost to practitioners of non-compliance grows: medieval forms become not merely incorrect but evidence of ignorance or corruption. The temporal trajectory models the institutionalization of the reading through printing, university curricula, and manuscript-editing conventions (roughly 1450–1550 in historical time). Theater_ratio stays relatively low (0.28–0.42) throughout because the coordination function — standardization around a fixed textual corpus — is real and produces genuine benefits for cross-regional communication and preservation. The theater that grows (the performative maintenance of the boundary between Classical and corrupt) remains subordinate to the functional coordination. Suppression mirrors the extraction curve: as the reading institutionalizes, the enforcement machinery required to keep medieval forms out of legitimate usage grows. The accessibility_collapse is high (0.79) because once the discontinuity frame is accepted, alternatives are genuinely hard to see — the texts are there, the corruption claim seems self-evident, the competitors are silent. Resistance is moderate (0.58) because ecclesiastical and continuity-tradition actors do resist, but the resistance is systematically marginalized and loses institutional power over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits extreme perspectival divergence. From the philologist's analytical seat the discontinuity reading is a genuine solution to a real coordination problem: Latin ceased to be a native language, so anchoring it to a fixed textual standard is the only way to maintain stability and cross-regional communication. From the medieval scribe's seat the same constraint is pure extraction and erasure: their accumulated practice, which followed rule-governed adaptation, is retroactively declared corrupt, their work is edited by strangers according to standards they were never part of creating, and their legitimacy is destroyed. From the ecclesiastical seat there is a hybrid: the constraint solves the standardization problem (genuine benefit) but transfers the authority to solve it from the Church to secular scholars (extraction of institutional power). The engine computes these divergences per-seat from the structural data: high-power agents with exit options will perceive the constraint differently than powerless agents with trapped exit. The claim of a unified tangled_rope masks the fact that from some seats the type approaches snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional agenda-setters (philologists, manuscript editors) sit at full-beneficiary directionality (d near 0.0): they benefit directly from the constraint's operation, control its enforcement, and have high exit options (could endorse continuity but don't). Renaissance humanists are moderate beneficiaries (d ~0.25–0.35): they benefit substantially from the constraint and have mobile exit options but choose to enforce it. Medieval scribes are full targets (d near 1.0): they benefit not at all and have trapped exit (their historical practice is already fixed). Living Latin practitioners are high targets (d ~0.75–0.85): their contemporary practice is constrained but they retain some ability to teach and communicate, hence not quite full trappage. Ecclesiastical authorities are asymmetric: as tradition-keepers they are targets (displaced from authority, d ~0.65), but those who align with humanist authority become partial beneficiaries (d ~0.35). This dual positioning is captured by the secondary_role mechanism. Textual sources are analytical observers (d = 0.5): they are neither beneficiaries nor targets but are the objective referent that anchors the entire constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The discontinuity reading creates a mandatrophy risk: the founding problem was how to standardize a language that lost native speakers. By 1600, this founding problem is substantially solved — the classical standard is established, printed texts are stable, curricula are uniform across regions. Yet the constraint persists and even intensifies: the theater_ratio grows slightly (from 0.28 to 0.42) as manuscript editors spend increasing effort on emendation and correctness enforcement even though the original coordination crisis has passed. The threat level of medieval forms decreases as ecclesiastical Latin becomes less dominant, yet suppression continues and even grows. This pattern is consistent with the constraint shifting from coordination toward pure extraction once the original problem is solved. The mandatrophy verdict: founding_problem_status is contested (authorities disagree on whether standardization is still the active goal or whether the constraint now persists for institutional rent-seeking), and disappearance_verdict is world_rearranges (if the discontinuity constraint disappeared, medieval forms would be readmitted, institutional authority would redistribute, curricula would reorganize). The mismatch (dead problem + world_rearranges) signals mandatrophy in the making. The constraint is not yet a piton because the theater_ratio is still below 0.5 and the coordination function remains partially live; it is a tangled_rope in transition toward snare as the coordination need declines and extraction motivation persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_vs_ideology,
    'Was the discontinuity rupture between Classical and medieval Latin a structural necessity (the language genuinely cannot be standardized without fixed ancient anchors) or an ideological choice (the discontinuity frame was one of several viable answers to the standardization problem)?',
    'Historical comparison: did Byzantine Greek scholars, medieval Iberian scholars, or other language communities facing the same crisis of written transmission adopt different standards? If yes, the necessity claim fails. If no, the necessity argument gains strength.',
    'If ideological, the constraint becomes snare rather than tangled_rope: the coordination benefit is real but minimal, and the extraction (authority transfer, practitioner erasure) is the primary function. If necessary, the constraint remains tangled_rope: extraction is a genuine byproduct of solving an unavoidable coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_vs_ideology, empirical, 'Whether the discontinuity frame was structurally necessary or ideologically chosen.').

omega_variable(
    medieval_practice_rule_governed,
    'Were medieval scribal forms rule-governed adaptive developments (legitimate evolution) or genuinely random corruption (true error)?',
    'Systematic linguistic analysis of medieval Latin syntax, morphology, and usage patterns: do they follow consistent, learnable rules or are they arbitrary deviations from Classical norms?',
    'If rule-governed, medieval forms claim legitimacy as evolved Latin under continuity_reading, and the discontinuity suppression of medieval practice is erasing a valid linguistic system. If arbitrary, the discontinuity reading''s characterization of medieval Latin as corruption gains empirical support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medieval_practice_rule_governed, empirical, 'Whether medieval Latin forms followed rule-governed patterns or were genuinely random errors.').

omega_variable(
    textual_accident_vs_historical_fact,
    'Do the absences of particular forms in surviving ancient texts prove those forms were not part of Classical Latin, or do they merely reflect accidents of manuscript preservation?',
    'Examine documented cases where a Classical form appears in only one or two surviving texts, yet is used across multiple literary genres; compare manuscript survival rates by genre (legal documents, private letters, etc.); assess the representativeness of extant texts.',
    'If absences are often accidental, the discontinuity reading''s reliance on textual sources as a complete inventory becomes suspect, and medieval forms absent from surviving texts may have been part of living Classical Latin. If absences reliably correlate with non-existence, textual sources gain authority as a sufficient standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_accident_vs_historical_fact, empirical, 'Whether textual absence proves historical absence or merely reflects manuscript survival accidents.').

omega_variable(
    reading_kernel_committer_structure,
    'Is the discontinuity_reading a substantive commitment to the claim that Classical form is authoritative, or is it a reading that different institutional actors adopt strategically when it serves their interests (Renaissance humanists for authority, philologists for gatekeeping)?',
    'Examine the genealogy of discontinuity endorsement: do different actors across different times all endorse it for the same structural reason, or do they adopt and adapt it opportunistically? When institutional interests shift, does commitment to the reading shift?',
    'If the reading is substantive, it has internal logical structure independent of actor interests. If it is strategic, the constraint becomes more extractive: the reading is a tool adopted to serve institutional capture, not a response to a genuine coordination problem. This affects the classification boundary between tangled_rope and snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_committer_structure, conceptual, 'Whether the discontinuity reading is a substantive commitment or a strategic institutional tool.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__discontinuity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(corr_tr_t0, observed).
narrative_ontology:measurement(corr_tr_t8, correct_latin__discontinuity_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(corr_tr_t8, observed).
narrative_ontology:measurement(corr_tr_t16, correct_latin__discontinuity_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement_basis(corr_tr_t16, observed).
narrative_ontology:measurement(corr_tr_t24, correct_latin__discontinuity_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(corr_tr_t24, observed).
narrative_ontology:measurement(corr_tr_t32, correct_latin__discontinuity_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(corr_tr_t32, observed).
narrative_ontology:measurement(corr_tr_t40, correct_latin__discontinuity_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(corr_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__discontinuity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(corr_be_t0, observed).
narrative_ontology:measurement(corr_be_t8, correct_latin__discontinuity_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(corr_be_t8, observed).
narrative_ontology:measurement(corr_be_t16, correct_latin__discontinuity_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(corr_be_t16, observed).
narrative_ontology:measurement(corr_be_t24, correct_latin__discontinuity_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(corr_be_t24, observed).
narrative_ontology:measurement(corr_be_t32, correct_latin__discontinuity_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(corr_be_t32, observed).
narrative_ontology:measurement(corr_be_t40, correct_latin__discontinuity_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(corr_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__discontinuity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(corr_su_t0, observed).
narrative_ontology:measurement(corr_su_t8, correct_latin__discontinuity_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(corr_su_t8, observed).
narrative_ontology:measurement(corr_su_t16, correct_latin__discontinuity_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement_basis(corr_su_t16, observed).
narrative_ontology:measurement(corr_su_t24, correct_latin__discontinuity_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(corr_su_t24, observed).
narrative_ontology:measurement(corr_su_t32, correct_latin__discontinuity_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(corr_su_t32, observed).
narrative_ontology:measurement(corr_su_t40, correct_latin__discontinuity_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(corr_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__discontinuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin__discontinuity_reading, 0.12).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% The correct_latin kernel decomposes into three constraint stories, each instantiating a different reading. The discontinuity_reading declares a rupture between Classical and medieval forms; the continuity_reading embraces evolved transmission; the hybrid_reading attempts to maintain Classical authority while incorporating medieval practice. Each story has its own ε (extractiveness), its own beneficiary/victim structure, and its own type classification. They share a kernel (the question of what counts as correct Latin) and are linked via network edges. The three readings coexist in historical time — different institutional actors endorse different readings — and are logically distinct (each makes incompatible claims about the legitimacy of medieval forms). This is not observable-dependent variation of one constraint; it is structural decomposition per DP-001 (ε-invariance principle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin__discontinuity_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
