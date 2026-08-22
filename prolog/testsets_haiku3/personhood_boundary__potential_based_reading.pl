% ============================================================================
% CONSTRAINT STORY: personhood_boundary__potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__potential_based_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Personhood via Potential Rational Agency (Potential-Based Reading)
 *   domain: moral_philosophy/historical_ethics
 *
 * SUMMARY:
 *   This constraint embodies one reading of the personhood boundary kernel:
 *   the claim that personhood derives from potential for rational agency, and
 *   that severely disabled infants who lack or cannot develop this capacity
 *   therefore lack full moral standing. This reading has been influential in
 *   bioethics and neonatal medicine since the 1970s, grounding decisions
 *   about life support, treatment intensity, and resource allocation. The
 *   constraint is CLAIMED as a snare (the reading itself is extractive—it
 *   denies standing to specific categories of humans based on medical
 *   judgments of capacity) while employing the theoretical apparatus of
 *   personhood boundaries to make that denial appear principled rather than
 *   discriminatory. The measurement series tracks extraction rising from 0.48
 *   to 0.68 across the interval, with theater ratio rising from 0.28 to 0.42,
 *   indicating that over time the constraint's justificatory performance has
 *   grown while its functional extraction has stabilized.
 *
 * KEY AGENTS:
 *   - Severely disabled infants and individuals with profound cognitive impairment — the named victims whose personhood status the reading contests
 *   - Parental-medical authority (neonatologists, pediatricians, parents) — the agenda-setters who determine which infants possess or lack potential for rational agency
 *   - Bioethicists advocating the potential criterion — beneficiaries who see their theoretical framework operationalized and given institutional force
 *   - Disability advocates and alternative-reading proponents — excluded or marginalized voices who dispute the equation of personhood with rational-agency capacity
 *   - The medical profession as institutional actor — gains authority and liability protection by treating moral questions as technical medical ones
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.68).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.71).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, snare).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Personhood via Potential Rational Agency (Potential-Based Reading)").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/historical_ethics").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, '7db2c267-be83-4743-bc43-1754ffb040fd').
narrative_ontology:cs_kernel_codification('7db2c267-be83-4743-bc43-1754ffb040fd', distributed).
narrative_ontology:cs_authority_grounding('7db2c267-be83-4743-bc43-1754ffb040fd', extraction).
narrative_ontology:cs_interpretation_layer_present('7db2c267-be83-4743-bc43-1754ffb040fd').
narrative_ontology:cs_reading_relation('7db2c267-be83-4743-bc43-1754ffb040fd', personhood_boundary__birth_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('7db2c267-be83-4743-bc43-1754ffb040fd', personhood_boundary__fitness_contingent_reading, influences).
narrative_ontology:cs_axiom('7db2c267-be83-4743-bc43-1754ffb040fd', foundational, rational_agency_grounds_personhood).
narrative_ontology:cs_axiom_status(rational_agency_grounds_personhood, holdable).
narrative_ontology:cs_axiom_grounding('7db2c267-be83-4743-bc43-1754ffb040fd', rational_agency_grounds_personhood, deontological).
narrative_ontology:cs_axiom('7db2c267-be83-4743-bc43-1754ffb040fd', foundational, potential_assessment_is_objective_medical_determination).
narrative_ontology:cs_axiom_status(potential_assessment_is_objective_medical_determination, overridden).
narrative_ontology:cs_axiom_grounding('7db2c267-be83-4743-bc43-1754ffb040fd', potential_assessment_is_objective_medical_determination, empirically_contingent).
narrative_ontology:cs_reference_frame('7db2c267-be83-4743-bc43-1754ffb040fd', capacity_neutral_moral_status_framework).
narrative_ontology:cs_drift_state('7db2c267-be83-4743-bc43-1754ffb040fd', contemporary_disability_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7db2c267-be83-4743-bc43-1754ffb040fd', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_infants).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, profoundly_cognitively_disabled_individuals).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, anencephalic_infants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, bioethicists_advocating_potential_criterion).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, medical_profession_institutional).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, severely_disabled_children_capacity_positive).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_children_capacity_positive).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, rational_agency_as_personhood_criterion).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, medical_authority_determination_of_potential).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Newborns and infants with severe cognitive impairments, anencephaly, or conditions that medical assessment deems incompatible with rational-agency development. Under this reading's framework, their exclusion from personhood standing turns on judgments of potential that are made by medical and parental authority, without their participation or consent. Their situation is entirely determined by others' assessments; they cannot develop alternative capacities that would change the judgment, cannot advocate for themselves, and cannot exit the exclusion even if development exceeds initial medical predictions.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_infants, payer,
    powerless, immediate, trapped, local).

% Parents and medical professionals (neonatologists, pediatric neurologists, hospital ethics committees) exercise authority to determine which infants possess potential for rational agency and therefore qualify for personhood-based protections. This reading vests them with the power to make exclusion determinations and to act on those determinations—initiating or withdrawing life support, directing resource allocation, making decisions that would violate bodily integrity or personal interests if the excluded entity possessed full standing. Their authority is framed as technical expertise (assessing medical potential) but carries profound moral weight. They benefit from the constraint insofar as it legitimates their decisions as following objective principle rather than subjective judgment or value-based triage.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parental_medical_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Scholars and institutional bioethicists who ground personhood in potential for rational agency. The potential-based reading vindicates their theoretical framework and creates institutional legitimacy for it. Their work shapes neonatal protocols, parental decision-making, and medical education. They collect benefit in the form of theoretical confirmation (the reading is operationalized), professional authority (their criterion is adopted), and resource flows (funding for research on capacity and potential assessment). They have substantial exit options—they could adopt alternative readings, move to different institutional contexts, or change their theoretical positions—but benefit from the constraint's persistence.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, bioethicists_advocating_potential_criterion, beneficiary,
    organized, generational, arbitrage, global).

% Disability rights advocates, scholars, and organizations argue that the potential criterion is a construct that disguises exclusion based on disability status. They dispute the medical expertise claim, arguing that assessing potential is partly speculative and partly value-laden, not purely technical. They advocate for birth-threshold or capacity-neutral personhood frameworks. They are largely absent from neonatal intensive-care ethics committees and from real-time medical decision-making, though their voices are present in legal advocacy, policy analysis, and academic critique. Their exclusion from decision-making authority is structural—they lack the medical credentials typically required to participate in neonatal ethics discussions, and their alternative readings are often treated as advocacy rather than expertise.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disability_advocates_contesting_criterion, excluded,
    moderate, generational, constrained, global).

% The medical profession collectively gains authority to determine personhood status by framing it as a technical assessment of potential. The constraint allows doctors to avoid explicit moral choice by treating personhood exclusions as factual medical conclusions—the appearance of objectivity carries significant benefit. The profession also gains liability protection: decisions framed as following medical criteria and bioethical principle appear less arbitrary and less legally vulnerable than decisions based on value-based triage or resource scarcity. The profession bears some cost in the form of cognitive and emotional burden (making exclusion determinations) and increasing scrutiny from disability advocates and legal accountability frameworks, but the net benefit is substantial.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, medical_profession_institutional, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, medical_profession_institutional, beneficiary).

% Some severely disabled children develop capacities that satisfy the potential criterion (minimal rational agency, communication, relational engagement), and are therefore included in personhood standing. They benefit nominally by crossing the threshold into protected status, but carry the scar of initial exclusion and remain under-resourced and stigmatized because they were categorized as lacking potential. Their situation reveals the constraint's harm: the threat of exclusion and the framework of capacity-contingency persists even when they achieve enough to be nominally included.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_children_capacity_positive, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, severely_disabled_children_capacity_positive, payer).

% Philosophers, disability rights scholars, and legal advocates arguing that personhood begins at birth and is independent of capacity. They observe the potential-based reading's operations and provide alternative theoretical grounding and policy models. They are largely outside the immediate decision-making contexts in neonatal medicine but present in law, policy, and academic discourse. They have analytical standing but lack decision-making authority in the medical contexts where the constraint operates most pressingly. Their role is to articulate alternatives and document the reading's exclusions, but they do not participate in the moment-to-moment enforcement of the constraint.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, birth_threshold_advocates, observer,
    organized, generational, analytical, global).

% Philosophers and bioethicists who argue that personhood depends on demonstrated capacity (not potential). They occupy a middle position: more restrictive than birth-threshold, but more demanding than potential-based (requiring actual evidence of capacity rather than assessment of potential). They observe the potential-based reading and occasionally engage with it, but are mostly present in academic philosophy and marginal in neonatal medicine. Their role is to critique the potential criterion as under-demanding (potential is speculative) while offering an alternative framework, though that alternative is even more restrictive.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, fitness_contingent_advocates, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__potential_based_reading, parental_medical_authority).
narrative_ontology:fixing_cost_class(personhood_boundary__potential_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principle for allocating neonatal medical resources and determining the intensity of life-sustaining intervention. Solves the coordination problem of making decisions about treatment initiation and withdrawal when survival is possible but expected quality of life or capacity is severely limited. Establishes a rule (personhood via rational-agency potential) that is meant to make these decisions appear principled rather than arbitrary.
% TRANSFER_FUNCTION: Transfers authority over life-and-death determinations from law and universal personhood principles to parental-medical judgment. Transfers life-sustaining resources away from infants deemed to lack potential for rational agency and toward those deemed to possess it. Transfers decision-making power upward (to authority figures) and outward (from the excluded individual to others acting in their 'best interest'). Transfers moral standing and legal protection from certain categories of humans to the framework of parental-medical expertise.
% ABSENT_VOICES: Severely disabled infants cannot speak for themselves and have no representation. Disability scholars and disability rights advocates who dispute the potential criterion are largely absent from neonatal intensive-care ethics committees and from real-time medical decision-making. Alternative personhood frameworks (birth-threshold, relational, capacity-neutral dignity) are present in academic and policy discourse but excluded from decision-making authority in the medical contexts where the constraint operates most pressingly. The perspectives of disabled adults who were judged to lack potential as infants but developed meaningful capacities are often absent from the framework's deliberations.
% DISAPPEARANCE_RATIONALE: If this constraint (personhood grounded in potential for rational agency, with medical authority determining who possesses it) disappeared overnight, the entire framework for withdrawing life support from severely disabled newborns would lose legitimacy. The decisions would either require explicit value-based triage (framed as resource allocation rather than personhood exclusion), shift to a birth-threshold or relational framework, or be prohibited entirely. Medical protocols, parental decision-making, and legal frameworks for neonatal treatment decisions would all reorganize. The moral cover provided by the potential criterion would vanish, forcing more transparent acknowledgment of the value judgments embedded in treatment decisions.
% FOUNDING_PROBLEM: In the 1970s–1980s, neonatal medical technology became capable of sustaining infants with severe cognitive impairments and terminal conditions previously incompatible with life. Parents and physicians faced decisions about whether to initiate or continue life support for infants with no capacity for independent functioning and profound expected disability. The founding problem: what principle justifies treating some newborns' lives as not worth the full commitment of life-sustaining medical intervention?
% FOUNDING_PROBLEM_CORROBORATION: Neonatologists and medical ethicists from the 1980s onward attest the problem remains live—difficult treatment decisions for severely disabled newborns continue to require guidance. Disability scholars and advocates attest the founding problem has been reframed rather than solved: the real coordination problem is resource scarcity and parental anxiety, not philosophical disagreement about personhood. They argue the potential criterion obscures rather than solves the founding problem by encoding exclusion into personhood philosophy. Legal scholarship and human rights analysis outside the bioethics community attests that the founding problem is partly an artifact of the medical system's need to ration intensive care and shift decision-making authority to parents and doctors; alternative healthcare systems without the same resource constraints may not experience it. The constraint persists not because it genuinely solves the founding problem but because it provides legitimacy for decisions that would otherwise appear arbitrary or value-based.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__potential_based_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__potential_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__potential_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint denies standing to individuals based on a capacity judgment that is (a) partly speculative (assessing potential is uncertain), (b) applied in an asymmetric context where the excluded party cannot advocate for themselves, and (c) has real consequences—treatment withdrawal, resource non-allocation, exclusion from protective legal frameworks. Suppression is substantial (0.71) because the reading suppresses alternative frameworks (birth-threshold personhood, relational personhood, disability-inclusive personhood) by claiming its criterion is objective and neutral. Theater ratio is moderate and rising (0.42) because the constraint increasingly relies on medical-expertise performance to carry what are partly normative judgments about what capacities matter. The measurement trajectory shows initial rise as the criterion became institutionalized in medical protocols (1970s–1990s), then plateau as it reached stable enforcement (1990s onward), with theater ratio continuing to rise as disability critique has forced more explicit rhetorical justification.
 *
 * PERSPECTIVAL GAP:
 *   The parental-medical authority seats and the bioethicist seats experience this constraint as a legitimate personhood principle solving a genuine problem (resource allocation, end-of-life decision-making). From the perspective of disabled individuals and disability advocates, the same constraint is a framework that reconstructs historical exclusion—it makes systematic denial of standing appear principled by grounding it in a criterion (rational potential) that is difficult to verify, partly contestable, and typically applied only to people with disabilities. The engine should compute different types for these seats: the beneficiary/agenda-setter seats may compute the constraint as genuine coordination (we need a principle to guide hard choices), while the victim and excluded seats should compute it as snare (the principle is a cover for extracting authority and resources based on disability status). The perspectival divergence is structural, not merely evaluative—the constraint's operation differs substantively across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Severely disabled infants occupy the full-target end of directionality (d ≈ 1.0): the constraint extracts from them (denies standing, enables treatment withdrawal, excludes them from resource-allocation priority) and they have zero exit options (trapped: they cannot develop the required potential, cannot leave the situation, cannot advocate their case). Parental-medical authority is the beneficiary/agenda-setter (d ≈ 0.05–0.15): they gain authority, liability protection, and legitimation for decisions that would otherwise require explicit value-based justification. Bioethicists are secondary beneficiaries (d ≈ 0.1): their theoretical framework is vindicated and operationalized. Disability advocates sit in the excluded/observer position but with constrained exit (d ≈ 0.6–0.7): they can articulate alternatives but lack decision-making authority in the medical contexts where the constraint bites hardest. The medical profession as an institutional actor is a complex case: it gains authority but also carries the cognitive/emotional burden of making exclusion judgments, so its directionality may be closer to d ≈ 0.35–0.45 (moderate beneficiary, moderate payer of the suppression cost).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits mandatrophy: the founding problem (how to make life-and-death decisions for severely disabled newborns) remains live, but the constraint's function has shifted from solving it to obscuring it. Early in its adoption (1970s–1980s), the potential-based criterion did provide a framework where difficult decisions could be made. As neonatal medicine advanced and survival improved even for severely impaired infants, the criterion's function increasingly became theatrical—it provides moral cover for decisions that are actually driven by resource scarcity, parental anxiety, and residual prejudice against disability. The constraint persists not because it continues to solve the founding problem but because parental-medical authority has become invested in its maintenance, and the theoretical apparatus of personhood-via-capacity continues to be taught and cited. A genuine mandatrophy resolution would require either: (a) acknowledging that the real problem is resource allocation and explicit value-based triage (replacing the personhood framing), or (b) shifting to a birth-threshold or capacity-neutral framework that solves the same coordination problem (deciding about resource intensity) without denying standing. The measurement series shows extraction plateauing while theater rises, consistent with the atrophied-function/inertial-maintenance profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potential_determination_ambiguity,
    'What counts as ''potential for rational agency,'' and how is it assessed? Can potential be reliably distinguished from current incapacity by medical examination alone?',
    'Longitudinal follow-up studies of severely disabled infants classified as lacking potential, comparing actual development outcomes to initial medical predictions. Transparency in the medical criteria used to determine potential vs. actual-capacity assessments in neonatal records.',
    'If potential determinations are found to be substantially inaccurate, or if the criterion proves impossible to apply consistently, the constraint loses its legitimacy as an objective principle and collapses into pure authority-based exclusion. Alternatively, the criterion becomes revealed as partly speculative judgment dressed in medical language, requiring explicit ethical rather than medical adjudication.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(potential_determination_ambiguity, empirical, 'Whether ''potential for rational agency'' can be assessed with sufficient reliability to ground personhood exclusions.').

omega_variable(
    rational_agency_as_personhood_ground,
    'Is rational agency the correct ground for personhood, or is personhood more fundamental—grounded in humanity, birth, relational participation, or capacity-independent dignity? Is the potential-based reading internally consistent with its own premises?',
    'Philosophical analysis and cross-cultural comparison of personhood concepts. Empirical investigation of whether parental-medical authority actually treats potential-based exclusion as neutral or whether disability status functions as a proxy. Review of historical record to assess whether the criterion was adopted to solve a genuine coordination problem or to legitimate pre-existing exclusion practices.',
    'If rational agency is not the ground of personhood, the entire constraint rests on a disputed philosophical foundation; if the criterion is a proxy for disability exclusion rather than a genuine principle, the constraint''s claim to objectivity collapses and it becomes frankly extractive. Either finding would require reclassification toward snare and possible replacement with alternative framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_agency_as_personhood_ground, conceptual, 'Whether the potential-based reading''s foundational axiom about rational agency and personhood can withstand philosophical scrutiny and cross-paradigm comparison.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of alternative personhood readings (birth-threshold, relational, capacity-neutral) structural (the potential criterion is genuinely more persuasive in neonatal medicine) or internalized (disability advocates and alternative frameworks are excluded from decision-making authority and have internalized their non-status)?',
    'Audit of neonatal ethics committees and decision-making structures: what reading frameworks are represented, what credentials are required to participate, whose voices carry weight. Comparison of suppression before and after institutional inclusion of disability advocates in these spaces. Measurement of resistance in jurisdictions where alternative readings have been institutionalized.',
    'If suppression is mostly structural (genuine persuasiveness), the constraint''s dominance may reflect legitimate philosophical merit. If suppression is partly internalized (exclusion of dissenting voices), the suppression metric understates the actual coercive force by not accounting for cognitive suppression of alternatives within the system. This would argue for a higher suppression value and reconsideration of whether the constraint''s persistence depends on keeping alternatives literally absent from decision-making authority rather than on genuine philosophical persuasion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether the suppression of alternative personhood readings operates through structural barriers or through internalized exclusion of dissenting voices.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the potential-based reading logically foreclose the birth-threshold reading? Can both readings coexist within a single ethical framework, or does adoption of one require rejection of the other?',
    'Philosophical analysis of the logical relationships between the axioms: if potential-based personhood is correct, must birth-threshold personhood be false? Or are they addressing different aspects (e.g., legal personhood vs. moral status, different institutional contexts)?',
    'Classification of the reading relation as ''forecloses'' vs. ''coexists_with'' hinges on this question. If the readings are logically contradictory, one is correct and the other impossible within a single framework. If they address different framings or institutional contexts, they coexist—this matters for understanding whether the constraint represents a genuine philosophical discovery or a contingent institutional choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between the potential-based reading and the birth-threshold reading of the personhood kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__potential_based_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(pers_tr_t0, observed).
narrative_ontology:measurement(pers_tr_t8, personhood_boundary__potential_based_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(pers_tr_t8, observed).
narrative_ontology:measurement(pers_tr_t16, personhood_boundary__potential_based_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement_basis(pers_tr_t16, observed).
narrative_ontology:measurement(pers_tr_t25, personhood_boundary__potential_based_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(pers_tr_t25, observed).
narrative_ontology:measurement(pers_tr_t37, personhood_boundary__potential_based_reading, theater_ratio, 37, 0.42).
narrative_ontology:measurement_basis(pers_tr_t37, observed).
narrative_ontology:measurement(pers_tr_t50, personhood_boundary__potential_based_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(pers_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__potential_based_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(pers_be_t0, observed).
narrative_ontology:measurement(pers_be_t8, personhood_boundary__potential_based_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(pers_be_t8, observed).
narrative_ontology:measurement(pers_be_t16, personhood_boundary__potential_based_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(pers_be_t16, observed).
narrative_ontology:measurement(pers_be_t25, personhood_boundary__potential_based_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(pers_be_t25, observed).
narrative_ontology:measurement(pers_be_t37, personhood_boundary__potential_based_reading, base_extractiveness, 37, 0.68).
narrative_ontology:measurement_basis(pers_be_t37, observed).
narrative_ontology:measurement(pers_be_t50, personhood_boundary__potential_based_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(pers_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__potential_based_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(pers_su_t0, observed).
narrative_ontology:measurement(pers_su_t8, personhood_boundary__potential_based_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(pers_su_t8, observed).
narrative_ontology:measurement(pers_su_t16, personhood_boundary__potential_based_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement_basis(pers_su_t16, observed).
narrative_ontology:measurement(pers_su_t25, personhood_boundary__potential_based_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(pers_su_t25, observed).
narrative_ontology:measurement(pers_su_t37, personhood_boundary__potential_based_reading, suppression_requirement, 37, 0.71).
narrative_ontology:measurement_basis(pers_su_t37, observed).
narrative_ontology:measurement(pers_su_t50, personhood_boundary__potential_based_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(pers_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(personhood_boundary__potential_based_reading, 0.12).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__fitness_contingent_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (potential-based) of the personhood_boundary kernel. Two sibling readings are authored as separate constraints: birth_threshold_reading (personhood from birth, all born humans included) and fitness_contingent_reading (personhood from demonstrated capacity, potential irrelevant). The three readings compete in neonatal medical ethics, legal frameworks, and disability policy. Each has distinct victim sets, enforcement mechanisms, and measurement profiles. All three are linked through the kernel; the potential-based reading influences the others by establishing 'potential' as a contestable criterion, creating pressure for fitness_contingent to emphasize demonstrated capacity as more objective, and pressure for birth_threshold to emphasize birth as a simpler boundary. The three readings coexist across different institutional contexts: potential-based dominates neonatal medicine; birth-threshold dominates human rights and disability law; fitness-contingent is marginal but resurfaces in discussions of cognitive thresholds and personhood of non-human animals.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__potential_based_reading, powerless, 0.98).
constraint_indexing:directionality_override(personhood_boundary__potential_based_reading, organized, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
