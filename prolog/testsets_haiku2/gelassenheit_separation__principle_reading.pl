% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__principle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__principle_reading, []).

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
 *   constraint_id: gelassenheit_separation__principle_reading
 *   human_readable: Gelassenheit Separation Principle (Functional Isolation Reading)
 *   domain: religious_studies/technology_governance
 *
 * SUMMARY:
 *   Gelashenheit (yielding to God) is a principle of spiritual separation
 *   from worldly entanglement. The principle_reading interprets 'separation'
 *   as functional disengagement from structural systems: technology is
 *   acceptable only if it operates independently of the wider institutional
 *   order. This reading permits solar power and pneumatic tools (off-grid, no
 *   institutional dependency) while forbidding internet and insurance
 *   (networked, creating structural entanglement). The reading is contested
 *   within the tradition: the artifact_reading emphasizes visual distinction
 *   from English society; the consequence_reading evaluates technology by its
 *   effects on community practice. Leadership authority adjudicates which
 *   interpretation is correct, extracting interpretive power and collecting
 *   deference from adherents who accept the principle as authoritative. The
 *   measured extraction (0.48) is moderate because the principle provides
 *   genuine coordination benefit (a stable criterion for technology
 *   adoption), but it also permits leadership to shift boundaries and
 *   suppress alternative interpretations. Suppression (0.52) is substantial
 *   because the identity-locking of adherents (spiritual identity fused with
 *   acceptance of the principle) makes exit costly, and active boundary
 *   maintenance (distinguishing 'functionally isolated' from 'networked'
 *   systems) is necessary to police the distinction.
 *
 * KEY AGENTS:
 *   - Spiritual leadership authority (institutional agenda-setter): articulates the principle, adjudicates edge cases, collects interpretive authority
 *   - Adherents adopting principle (moderate, identity-locked payers): accept the criterion, forgo networked infrastructure, bear the cost of living under a technologically restricted rule
 *   - Community boundary maintainers (moderate, constrained payers): enforce the distinction, respond to member questions, police adoption
 *   - Alternative reading holders (moderate, excluded): would argue for artifact or consequence criteria but are silenced by the principle's institutional adoption
 *   - Wider technological society (powerful, analytical observer): documents how the principle operates in practice and compares it to other interpretations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, 0.48).
domain_priors:suppression_score(gelassenheit_separation__principle_reading, 0.52).
domain_priors:theater_ratio(gelassenheit_separation__principle_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__principle_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__principle_reading, "Gelassenheit Separation Principle (Functional Isolation Reading)").
narrative_ontology:topic_domain(gelassenheit_separation__principle_reading, "religious_studies/technology_governance").

domain_priors:requires_active_enforcement(gelassenheit_separation__principle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__principle_reading, 'fcf49f87-6039-4faf-919b-755321dbcb67').
narrative_ontology:cs_kernel_codification('fcf49f87-6039-4faf-919b-755321dbcb67', fixed_text).
narrative_ontology:cs_authority_grounding('fcf49f87-6039-4faf-919b-755321dbcb67', lineage).
narrative_ontology:cs_interpretation_layer_present('fcf49f87-6039-4faf-919b-755321dbcb67').
narrative_ontology:cs_reading_relation('fcf49f87-6039-4faf-919b-755321dbcb67', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('fcf49f87-6039-4faf-919b-755321dbcb67', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('fcf49f87-6039-4faf-919b-755321dbcb67', foundational, functional_isolation_moral_sufficiency).
narrative_ontology:cs_axiom_status(functional_isolation_moral_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('fcf49f87-6039-4faf-919b-755321dbcb67', functional_isolation_moral_sufficiency, deontological).
narrative_ontology:cs_axiom('fcf49f87-6039-4faf-919b-755321dbcb67', secondary, systemic_entanglement_spiritual_danger).
narrative_ontology:cs_axiom_status(systemic_entanglement_spiritual_danger, holdable).
narrative_ontology:cs_axiom_grounding('fcf49f87-6039-4faf-919b-755321dbcb67', systemic_entanglement_spiritual_danger, deontological).
narrative_ontology:cs_reference_frame('fcf49f87-6039-4faf-919b-755321dbcb67', theological_separatism_via_functional_autonomy).
narrative_ontology:cs_drift_state('fcf49f87-6039-4faf-919b-755321dbcb67', contemporary_digital_ubiquity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fcf49f87-6039-4faf-919b-755321dbcb67', '2026-08-03T14:32:15Z').
narrative_ontology:cs_kernel_id(gelassenheit_separation__principle_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, spiritual_leadership_authority).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, adherents_adopting_principle).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, community_boundary_maintainers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, adherents_adopting_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulates and adjudicates the principle that separation means functional (rather than visual) disengagement from worldly systems. Determines which technologies violate structural entanglement criteria. Maintains interpretive authority over what 'functionally isolated' means in new technical contexts. Collects authority legitimacy and community deference from the principle's maintenance.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, spiritual_leadership_authority, agenda_setter,
    institutional, generational, analytical, regional).

% Adopt the principle as the frame for technology acceptance. Pay the cost of foregoing networked infrastructure (no internet, no insurance) even when functionally isolated alternatives exist, because the principle forbids structural entanglement. Benefit from a coherent separatist framework that permits solar power and pneumatic tools while excluding digital systems. Their identity as members depends on accepting the principle's authority; exit means leaving the community's spiritual understanding.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, adherents_adopting_principle, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__principle_reading, adherents_adopting_principle, beneficiary).

% Enforce the principle's distinctions in community practice: deciding which new tools comply, preventing adoption of forbidden technologies, responding to member questions about edge cases. Bear the administrative and social cost of policing boundaries as they shift with technology. Their exit is to stop policing, but doing so would dissolve the constraint.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, community_boundary_maintainers, payer,
    moderate, biographical, constrained, regional).

% Hold the artifact_reading or consequence_reading within the same theological tradition. Would argue that visual distinction or community-practice effects, not abstract functional entanglement, are the true separatist criterion. Excluded from adjudicating this reading's truth; their interpretations are treated as heterodox. Their presence as an excluded voice signals the reading is contested within the tradition, not universal doctrine.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, alternative_reading_holders, excluded,
    moderate, biographical, constrained, regional).

% Observes Gelashenheit communities from outside, documenting technology adoption patterns, comparing this principle-reading interpretation to others, and noting how adherents navigate the boundary between isolated tools and networked infrastructure. Takes no enforcement role but provides external measurement of how the constraint operates.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, wider_technological_society, observer,
    powerful, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__principle_reading, spiritual_leadership_authority).
narrative_ontology:fixing_cost_class(gelassenheit_separation__principle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the hermeneutical problem: how can a separatist community engage modernity's technological proliferation without losing spiritual coherence? The principle provides a stable, technically defensible criterion (functional isolation vs. structural entanglement) that allows some technologies while forbidding others, and centralizes interpretive authority so the boundary does not fragment across the community.
% TRANSFER_FUNCTION: Transfers interpretive authority and boundary-maintenance labor from individual conscience to the spiritual leadership. Adherents surrender the freedom to reason privately about technology morality; leadership collects the authority to adjudicate cases and shift criteria. Community boundary-maintainers transfer social compliance cost (policing enforcement) upward to leadership legitimacy.
% ABSENT_VOICES: Adherents who favor the artifact_reading or consequence_reading (alternative theologians within the tradition) are excluded from the kernel's adjudication — the principle_reading presupposes its own criterion as correct. Technologists from outside the tradition, who might argue that no distinction between 'functionally isolated' and 'networked' makes moral sense, are also structurally absent. Their exclusion is the constraint's feature: permitting their voice would collapse the boundary.
% DISAPPEARANCE_RATIONALE: If this principle disappeared and the community lost the functional-isolation criterion, the boundary between permitted and forbidden technology would destabilize. Adherents would face incoherent rules (solar permitted but internet forbidden, both isolated) or would drift toward the artifact_reading or consequence_reading. Leadership's interpretive authority would weaken. The technology governance structure the community has built (exemptions for off-grid systems, blanket prohibition on networked ones) would require re-articulation.
% FOUNDING_PROBLEM: How to maintain spiritual separation from worldly systems in an era of technological ubiquity, when technology is not monolithic but manifold, and when some technologies (solar power, pneumatic tools) serve separatist ends (reducing commercial entanglement) while others (internet, insurance) violate them (creating structural dependency on worldly institutions)? The principle-reading proposes that separatism hinges not on artifacts' appearance but on their structural role: tools that operate independently of the wider system are compatible; tools that bind the user into institutional networks are not.
% FOUNDING_PROBLEM_CORROBORATION: Historical Mennonite and Amish theological scholarship documents the emergence of this criterion in the mid-twentieth century as communities faced industrial technology. Leadership figures from Gelashenheit communities testify that functional isolation is the operative criterion in practice. However, dissenting theological voices within the tradition (documented in community periodicals and private correspondence) argue that the artifact_reading (visual distinction) or consequence_reading (community-practice effects) better represent the spirit of separation. No consensus corroborates the principle_reading as the sole legitimate interpretation; its authority depends on institutional endorsement by leadership, not on unanimity.
narrative_ontology:disappearance_verdict(gelassenheit_separation__principle_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__principle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__principle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__principle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__principle_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__principle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__principle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction begins low (1950, ~0.32) when the principle is newly articulated and enjoys novelty legitimacy; it rises gradually as leadership's interpretive authority becomes entrenched, plateauing around 2000 (~0.45) when the principle is institutionalized across the community. By 2026, extraction stabilizes at 0.48 — moderate because the principle solves a genuine problem (technology governance) but is increasingly performative as new technologies make the 'functional isolation' criterion harder to maintain. Theater rises from 0.12 to 0.28 over the interval, indicating that enforcement effort increasingly focuses on defending the boundary against internal and external pressure rather than on clear application of the principle. Suppression rises from 0.35 to 0.52, driven by tightening boundary maintenance as internet and cellular technology become ubiquitous: leadership must actively forbid what was unthinkable in 1950 (smartphones, social media) and suppress the question of whether functional isolation is achievable in a networked world. The divergence between extraction (0.48) and suppression (0.52) reflects a tangled_rope structure: genuine coordination benefit (solving technology governance) and asymmetric extraction (leadership collecting authority) operate through the same mechanism. Adherents benefit from a clear rule; leadership benefits from the authority to adjudicate it. Seat divergence: from leadership's seat, this is rope — solving a real problem, enabling community coherence. From boundary-maintainers' and adherents' seats, it is increasingly tangled — the principle's clarity is sustained by active suppression of alternative readings and by identity-locking that makes exit costly. The claim/metric gap is intentional: the principle_reading is CLAIMED as tangled_rope by its own theology (coordination + enforcement), and the metrics support that claim.
 *
 * PERSPECTIVAL GAP:
 *   Leadership sits at the beneficiary end of directionality (d ~0.2): the principle centralizes interpretive authority, and they collect the deference and power that come from adjudicating technology boundaries. Adherents sit near the target end (d ~0.75): they must foreclose networked technologies even when functionally isolated alternatives might exist, and their identity-locking makes exit costly. Boundary maintainers sit at moderate target (d ~0.65): they bear the labor of enforcement and respond to member questions about edge cases, but they also benefit from a clear rule structure. Alternative-reading holders sit at excluded (no formal role), representing a structural absent voice that the principle_reading's institutional adoption silences. The engine should compute these divergences from the power/exit/scope data; the authored metrics describe the constraint's operation as experienced from leadership's authority-collecting position, not as neutral measurement. From boundary-maintainers' seats, suppression and extraction should compute higher; from adherents' identity-locked seats, they should compute as the cost of belonging rather than coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership authority (institutional power, analytical exit) benefits from the principle: their interpretive role is irreplaceable so long as the principle requires continuous application. Adherents (moderate power, identity-locked exit) are the targets: they pay the cost of technology restriction, and exit is identity-dissolving, not merely inconvenient. Boundary maintainers (moderate power, constrained exit) sit between: they benefit from a clear rule structure that simplifies their policing role, but they bear the labor of enforcement and face pressure from adherents questioning edge cases. The identity-locking of adherents is the key to understanding directionality: their spiritual identity is constituted through acceptance of the principle's authority, so exit from the principle means exit from the community and from their self-understanding. This makes their directionality near 0.75 (high target) even though they are moderate-power actors — identity-locking substitutes for coercive power by fusing the agent's self-concept with the constraint. Overrides are not needed: the structural data (identity-locked exit, moderate power, moderate time horizon) derives the correct directionality without adjustment.
 *
 * MANDATROPHY ANALYSIS:
 *   The principle_reading avoids mandatrophy at the current measurement point (2026) because the founding problem (technology governance for a separatist community) remains contested and live — leadership continues to adjudicate edge cases (solar panels, wireless sensors, electric vehicles). However, the trajectory is concerning: theater has plateaued at 0.28, and suppression has stabilized at 0.52, suggesting that the coordination function (providing a stable criterion) is becoming subordinate to the extraction function (concentrating interpretive authority). If alternative readings gain traction (adherents begin questioning whether functional isolation is the right criterion), the principle would face mandatrophy: a founding problem (how do we govern technology?) answered by a principle that no longer commands consensus. The absence of revision — the principle has not formally evolved since its mid-twentieth-century articulation despite massive technological change — is itself a sign of performance becoming primary. Leadership must invest increasing suppression to maintain the boundary as technology outpaces the principle's conceptual apparatus. Mandatrophy resolution depends on the omega variables: if leadership acknowledges the instability of 'functional isolation' and permits alternative interpretations to gain standing, the constraint can avoid mandatrophy by evolving into a **constraint family** (three distinct readings with different ε and different extraction profiles). If leadership doubles down on enforcement, mandatrophy becomes likely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_isolation_definition_instability,
    'What constitutes ''functional isolation''? As technology evolves, can the principle maintain a stable criterion, or does it require continuous interpretive renegotiation?',
    'Document leadership adjudications on novel technologies (e.g., does satellite imagery technology fall inside or outside the principle? Does a solar-powered device with offline computation differ from one connected to a cellular network?). Track whether the criterion drifts or remains fixed as new edge cases emerge.',
    'If the criterion requires continuous renegotiation, the principle reading collapses into the artifact_reading (visual judgment) or consequence_reading (case-by-case assessment) — the distinction between readings becomes illusory. If the criterion remains stable, the principle reading''s claim to provide objective grounding is validated. Extracted authority is concentrated when the criterion is contested; it diffuses when it is clear.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_isolation_definition_instability, empirical, 'Whether ''functional isolation'' can remain a stable criterion under technological change.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the measured suppression (0.52) driven by external enforcement (leadership policing technology adoption) or internalized acceptance (adherents believe the principle is correct and choose to follow it)?',
    'Post-exit interviews: if adherents who leave the community maintain the technology restrictions (no internet, no insurance) after moving to permissive environments, suppression is substantially internalized. If they rapidly adopt forbidden technologies, suppression is structural. Survey data on adherents'' belief that the principle is divinely mandated vs. pragmatically useful.',
    'If internalized, the extraction (leadership collecting interpretive authority) is less coercive than the suppression metric suggests — adherents carry the restriction with them. If structural, the suppression score understates the active enforcement burden, and the constraint is more tangled-rope-like (extraction sustained by active boundary maintenance). Identity-locking via belief internalizes suppression; identity-locking via community membership makes suppression more structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Mechanism of suppression: internalized belief vs. structural enforcement.').

omega_variable(
    kernel_contest_resolution_impossibility,
    'Can the principle_reading be evaluated as true or false against the artifact_reading and consequence_reading, or is the contest irresolvable because each reading presupposes different criteria for separatism itself?',
    'Examine whether leadership who endorse the principle_reading would accept the artifact_reading as a valid alternative interpretation if evidence suggested visual distinction better preserved community practice. If leadership dismisses the alternative reading on theological grounds (not empirical), the contest is conceptual; if they would shift readings on evidence, it is empirical.',
    'If irresolvable, the three readings form a **constraint family** (per ε-invariance principle) where each is a distinct constraint with different ε and different stakeholder structures. The present story (principle_reading) should link the others via network.affects_constraints. If resolvable, one reading is objectively more defensible than the others, and the constraint is a single claim with uncertain measurement rather than three distinct claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_resolution_impossibility, conceptual, 'Whether the kernel contest between readings is empirically resolvable or conceptually under-determined.').

omega_variable(
    false_summit_candidate_vindicated_propositions,
    'Do the vindicated propositions (''spiritual autonomy requires systemic disengagement,'' ''functional isolation equals moral safety'') describe natural facts or constructed commitments that benefit leadership authority?',
    'Historical analysis: did these propositions emerge from theological reflection on separatism, or were they articulated to defend a particular technology boundary against internal dissent? Do communities with the artifact_reading or consequence_reading endorse these propositions with the same confidence, or are they reading-specific doctrines?',
    'If the propositions are reading-specific doctrines, they are vindicated by the principle_reading''s operation (circular: the reading vindicates its own premises). If they are shared across readings, they represent genuine theological claims the different interpretations disagree on applying. If reading-specific, the principle_reading may be a **false summit**: presented as natural doctrine, actually sustained by extracted authority that benefits leadership interpretive power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_candidate_vindicated_propositions, conceptual, 'Whether vindicated propositions are shared theological claims or reading-specific doctrines benefiting leadership authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__principle_reading, 1950, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t1950, gelassenheit_separation__principle_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(gela_tr_t1975, gelassenheit_separation__principle_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(gela_tr_t2000, gelassenheit_separation__principle_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(gela_tr_t2013, gelassenheit_separation__principle_reading, theater_ratio, 2013, 0.27).
narrative_ontology:measurement(gela_tr_t2020, gelassenheit_separation__principle_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(gela_tr_t2026, gelassenheit_separation__principle_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(gela_be_t1950, gelassenheit_separation__principle_reading, base_extractiveness, 1950, 0.32).
narrative_ontology:measurement(gela_be_t1975, gelassenheit_separation__principle_reading, base_extractiveness, 1975, 0.38).
narrative_ontology:measurement(gela_be_t2000, gelassenheit_separation__principle_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(gela_be_t2013, gelassenheit_separation__principle_reading, base_extractiveness, 2013, 0.47).
narrative_ontology:measurement(gela_be_t2020, gelassenheit_separation__principle_reading, base_extractiveness, 2020, 0.48).
narrative_ontology:measurement(gela_be_t2026, gelassenheit_separation__principle_reading, base_extractiveness, 2026, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t1950, gelassenheit_separation__principle_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(gela_su_t1975, gelassenheit_separation__principle_reading, suppression_requirement, 1975, 0.42).
narrative_ontology:measurement(gela_su_t2000, gelassenheit_separation__principle_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(gela_su_t2013, gelassenheit_separation__principle_reading, suppression_requirement, 2013, 0.52).
narrative_ontology:measurement(gela_su_t2020, gelassenheit_separation__principle_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement(gela_su_t2026, gelassenheit_separation__principle_reading, suppression_requirement, 2026, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__principle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__principle_reading, 0.12).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% The gelassenheit_separation kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading. The principle_reading (this story) interprets separation as functional disengagement from institutional networks (ε=0.48, tangled_rope). The artifact_reading interprets separation as visual distinction (separate story, different ε). The consequence_reading interprets separation as preservation of community practices (separate story, different ε). The three readings coexist within the tradition as live theological positions. They are not the same constraint viewed from different angles — their ε-values differ because 'separation' means something structurally different in each reading, and the stakes of technology adoption differ accordingly. All three should be authored as separate stories and linked via affects_constraints to establish the family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
