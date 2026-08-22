% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: D&C 132 Immutable Commandment Reading — Polygamy as Eternal Exaltation Requirement
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   D&C 132 (1843) establishes plural marriage as an eternal, immutable
 *   divine law required for the highest degree of exaltation. The
 *   immutable_commandment_reading holds that the revelation cannot be
 *   altered, superseded, or suspended — it is a fixed kernel binding all who
 *   accept it. Federal pressure (Morrill Act 1862, Poland Act 1874, Edmunds
 *   Act 1882, Edmunds-Tucker Act 1887) creates a martyrdom constraint:
 *   compliance with civil law means apostasy from the covenant; resistance
 *   means property seizure, imprisonment, and dislocation. The 1890 Manifesto
 *   (Official Declaration 1) suspends practice but, under this reading, does
 *   not renounce doctrine — the eternal principle remains valid while
 *   obedience to civil law is a temporary accommodation. No legitimate
 *   revision path exists within this reading's framework; the prophetic
 *   override and temporal accommodation readings are treated as heretical
 *   departures. The constraint extracts labor, reproductive capacity, and
 *   resources from plural wives and dissenting members, concentrating status
 *   and authority in male heads of household and fundamentalist leadership.
 *
 * KEY AGENTS:
 *   - fundamentalist_leadership: agenda_setter (institutional/identity_locked/global) — administers the covenant, defines orthodoxy, collects compliance
 *   - male_heads_of_household: beneficiary (organized/identity_locked/local) — receive spiritual authority, domestic labor, reproductive access through the covenant
 *   - plural_wives: payer (powerless/identity_locked/local) — bear reproductive burden, economic dependency, status subordination; exit means loss of salvation framework
 *   - dissenting_members: payer (powerless/trapped/local) — face excommunication, shunning, property loss for questioning the covenant
 *   - children_in_plural_families: payer (powerless/identity_locked/local) — socialized into the covenant with no meaningful exit before adulthood
 *   - federal_authorities: observer (institutional/analytical/national) — enforce anti-polygamy laws, creating the martyrdom constraint
 *   - mainstream_latter_day_saints: excluded (organized/mobile/national) — hold competing readings (prophetic_override, temporal_accommodation) but are structurally excluded from this reading's authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.82).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.91).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, snare).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "D&C 132 Immutable Commandment Reading — Polygamy as Eternal Exaltation Requirement").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, '63cb01ee-0d6f-4f28-905a-81bc46805130').
narrative_ontology:cs_kernel_codification('63cb01ee-0d6f-4f28-905a-81bc46805130', fixed_text).
narrative_ontology:cs_authority_grounding('63cb01ee-0d6f-4f28-905a-81bc46805130', lineage).
narrative_ontology:cs_interpretation_layer_present('63cb01ee-0d6f-4f28-905a-81bc46805130').
narrative_ontology:cs_reading_relation('63cb01ee-0d6f-4f28-905a-81bc46805130', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('63cb01ee-0d6f-4f28-905a-81bc46805130', eternal_marriage_covenant__temporal_accommodation_reading, influences).
narrative_ontology:cs_axiom('63cb01ee-0d6f-4f28-905a-81bc46805130', foundational, d_and_c_132_immutable_eternal_law).
narrative_ontology:cs_axiom_status(d_and_c_132_immutable_eternal_law, holdable).
narrative_ontology:cs_axiom_grounding('63cb01ee-0d6f-4f28-905a-81bc46805130', d_and_c_132_immutable_eternal_law, theological).
narrative_ontology:cs_axiom('63cb01ee-0d6f-4f28-905a-81bc46805130', foundational, polygamy_required_for_exaltation).
narrative_ontology:cs_axiom_status(polygamy_required_for_exaltation, holdable).
narrative_ontology:cs_axiom_grounding('63cb01ee-0d6f-4f28-905a-81bc46805130', polygamy_required_for_exaltation, theological).
narrative_ontology:cs_axiom('63cb01ee-0d6f-4f28-905a-81bc46805130', foundational, no_legitimate_revision_path_exists).
narrative_ontology:cs_axiom_status(no_legitimate_revision_path_exists, holdable).
narrative_ontology:cs_axiom_grounding('63cb01ee-0d6f-4f28-905a-81bc46805130', no_legitimate_revision_path_exists, deontological).
narrative_ontology:cs_reference_frame('63cb01ee-0d6f-4f28-905a-81bc46805130', joseph_smith_restoration_completeness).
narrative_ontology:cs_drift_state('63cb01ee-0d6f-4f28-905a-81bc46805130', post_1890_manifesto, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('63cb01ee-0d6f-4f28-905a-81bc46805130', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, fundamentalist_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, male_heads_of_household).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, plural_wives).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, dissenting_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, children_in_plural_families).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, eternal_progression_through_polygamy).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, divine_law_above_civil_law).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, martyrdom_as_faithfulness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces the immutable covenant; controls ordinations, sealings, and community boundaries; collects tithes, labor, and loyalty. Their authority derives entirely from the covenant's immutability — revision would dissolve their position. Exit is identity_locked: their self-concept and institutional role are fused with the covenant.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, fundamentalist_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Hold priesthood authority over plural households; receive domestic labor, reproductive access, and spiritual status through the covenant. Their exaltation depends on covenant fidelity. Exit is identity_locked: the covenant constitutes their identity as patriarchs; leaving means forfeiting believed eternal progression.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, male_heads_of_household, beneficiary,
    organized, biographical, identity_locked, local).

% Enter marriage covenant with limited prior consent (often adolescent); bear children, manage households, and submit to husband's authority. Economic dependency is structural (no independent property, community ostracism if they leave). Theological conviction fuses identity with the covenant: exit means believed loss of salvation. They are the primary extraction targets.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, plural_wives, payer,
    powerless, biographical, identity_locked, local).

% Question the covenant's immutability or their place in it; face excommunication, shunning, property seizure, and loss of family. Some are plural wives seeking exit; some are men refusing additional marriages. No community support for dissent; federal authorities offer physical escape but not theological resolution. Exit is trapped: structural barriers (property, family, community) and internalized conviction both operate.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, dissenting_members, payer,
    powerless, immediate, trapped, local).

% Socialized from birth into covenant theology; no meaningful exit before adulthood. Girls face early marriage assignment; boys face surplus male problem (expulsion or subordination). Their identity is formed within the covenant; leaving means total social and theological rupture. They bear costs without ever consenting.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, children_in_plural_families, payer,
    powerless, biographical, identity_locked, local).

% Enforce anti-polygamy legislation (Morrill, Poland, Edmunds, Edmunds-Tucker Acts). Their prosecution creates the martyrdom constraint that this reading treats as proof of divine origin. They neither benefit from nor pay into the covenant; they are the external pressure that reveals the constraint's suppression mechanism.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, federal_authorities, observer,
    institutional, generational, analytical, national).

% Hold the prophetic_override_reading and temporal_accommodation_reading of the same kernel. They are structurally excluded from this reading's authority — their prophet declared the Manifesto, their church accepted statehood, their theology evolved. They would object to this reading's claim to represent the kernel, but they have no seat in its operation. Their exit is mobile: they already exited the fundamentalist schism.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, mainstream_latter_day_saints, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__immutable_commandment_reading, fundamentalist_leadership).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__immutable_commandment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates eternal family formation across generations under a single immutable divine law, providing a fixed framework for exaltation that does not shift with civil law or prophetic succession.
% TRANSFER_FUNCTION: Moves reproductive labor, domestic labor, property, tithes, and loyalty from plural wives, dissenting members, and children to male heads of household and fundamentalist leadership, as the price of covenant fidelity and believed exaltation.
% ABSENT_VOICES: Plural wives who would refuse additional marriages, dissenting members who would leave but cannot, children who would choose differently — all are structurally silenced by the identity_locked exit and the theological foreclosure of alternatives. Mainstream Latter-day Saints (holding sibling readings) are excluded from this reading's authority structure entirely.
% DISAPPEARANCE_RATIONALE: If the immutable commandment reading vanished overnight, fundamentalist communities would lose their doctrinal foundation; plural marriages would lose their exaltation rationale; leadership authority would collapse; federal pressure would lose its martyrdom object; the entire community structure would reorganize around either the prophetic_override_reading, the temporal_accommodation_reading, or complete dissolution.
% FOUNDING_PROBLEM: Early Latter-day Saint theology required restoration of all biblical practices for the restitution of all things; plural marriage was revealed as essential to the highest exaltation and to solving the demographic/spiritual problem of eternal increase.
% FOUNDING_PROBLEM_CORROBORATION: This reading's leadership attests the problem is live (exaltation still requires the covenant). Mainstream Latter-day Saint leadership (prophetic_override_reading) attests the problem was solved by continuing revelation — the 1890 Manifesto and subsequent doctrinal shifts. Federal authorities (observer) attest the problem was never a genuine coordination need but a power structure. Independent historians (analytical) attest the founding problem was contested from the start (Emma Smith's opposition, early dissent).
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the covenant diverts substantial resources (labor, offspring, property, loyalty) to a narrow beneficiary set while imposing severe costs on plural wives and dissenters. Suppression is extreme (0.91) because the constraint's persistence depends on both federal coercion (external) and theological identity fusion (internal) — exit means not just social loss but believed eternal damnation. Theater ratio is low (0.15) because the enforcement machinery (internal discipline, external resistance) is functionally real, not performative. Accessibility collapse is near-total (0.92): once the covenant is accepted as immutable divine law, alternatives (monogamy, dissent, exit) are theologically foreclosed. Resistance is high (0.78) because both federal authorities and internal dissenters actively oppose the arrangement, requiring continuous enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (leadership, male heads) experience the constraint as genuine coordination solving the problem of eternal family formation — a rope or mountain from their position. The payer seats (plural wives, dissenters, children) experience it as enforced extraction with no exit — a snare. The engine computes this divergence from the structural data; the claimed_type (snare) reflects the structural reality from the target seats, which bear the constraint's weight.
 *
 * DIRECTIONALITY LOGIC:
 *   Fundamentalist leadership and male heads of household are beneficiaries (d ≈ 0.15–0.25): they receive authority, labor, and spiritual capital. Plural wives, dissenting members, and children are targets (d ≈ 0.85–0.95): they bear the costs with identity_locked exit (theological conviction makes exit unthinkable). Federal authorities are analytical observers (d ≈ 0.5): they impose costs but do not benefit from the covenant's operation. Mainstream Latter-day Saints are excluded from this reading's framework entirely — they hold sibling readings but have no seat in this constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (eternal family formation for exaltation) is claimed as live by this reading, but the arrangement's persistence after 1890 depends on suppressing the church's own revision mechanisms (continuing revelation, prophetic authority). The mandatrophy is unresolved: the constraint has outlived the institutional context that could revise it (the mainstream church abandoned it), yet the reading treats revision as impossible. This is a snare masquerading as a mountain — the false summit of 'immutable divine law' covers ongoing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the eternal_marriage_covenant kernel, or does it describe the kernel itself?',
    'Compare the structural delta across declared sibling readings (prophetic_override_reading, temporal_accommodation_reading). If the constraint''s beneficiary/victim structure and epsilon differ from siblings, it is a reading, not the kernel.',
    'Confirms the committer frame: this file instantiates the immutable_commandment_reading only; the kernel is the contested commitment that generates multiple readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commits this story to the one-reading-one-constraint discipline (Rule 1).').

omega_variable(
    martyrdom_as_extraction_mechanism,
    'Does the federal pressure creating a martyrdom constraint function as an extraction amplifier (raising effective extraction on dissenters) or as a coordination signal (proving the arrangement''s divine origin)?',
    'Trace historical trajectory: if martyrdom correlates with increased resource flow to leadership and decreased exit for plural wives, it amplifies extraction; if it correlates with community cohesion without resource concentration, it may be genuine coordination.',
    'If extraction amplifier, the snare classification is reinforced; if coordination signal, the constraint may be a tangled_rope at the community level despite federal suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martyrdom_as_extraction_mechanism, empirical, 'Whether martyrdom pressure is structurally extractive or coordinative.').

omega_variable(
    revision_path_closure,
    'Is the ''no legitimate revision path'' claim structurally true of the commitment system, or does the continuing revelation doctrine (prophetic_override_reading) constitute a live internal revision mechanism that this reading refuses to recognize?',
    'Analyze whether the authority structure of this reading has formally foreclosed the prophetic override axiom, or merely treats it as heretical. Formal foreclosure = overridden axiom status; mere heresy declaration = coexistence with active suppression.',
    'If revision path is genuinely closed, the snare classification holds; if a live but suppressed mechanism exists, the constraint is a tangled_rope with active suppression of an internal alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revision_path_closure, conceptual, 'Whether the commitment system''s own doctrine provides a revision path that this reading suppresses.').

omega_variable(
    suppression_internalized_vs_structural,
    'Is the measured suppression (0.91) primarily structural (federal prosecution, property seizure, disfellowshipment) or internalized (theological conviction that dissent equals damnation, identity fused with the covenant)?',
    'Post-exit suppression trajectory: track former members who left fundamentalist communities — if suppression experience persists after physical exit, internalized component is significant.',
    'If substantially internalized, effective suppression exceeds the structural measure; the constraint operates as a snare even when formal enforcement is absent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalized_vs_structural, empirical, 'Structural vs. internalized suppression mechanism in an interpersonal/religious constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 1843, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_tr_t1843, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1843, 0.05).
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_tr_t1852, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1852, 0.08).
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_tr_t1870, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1870, 0.12).
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_tr_t1887, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1887, 0.18).
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_tr_t1890, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1890, 0.22).
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_tr_t1904, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1904, 0.15).

% Extraction over time
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_be_t1843, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1843, 0.35).
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_be_t1852, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1852, 0.62).
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_be_t1870, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1870, 0.71).
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_be_t1887, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1887, 0.85).
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_be_t1890, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1890, 0.88).
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_be_t1904, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1904, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_su_t1843, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1843, 0.45).
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_su_t1852, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1852, 0.65).
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_su_t1870, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1870, 0.78).
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_su_t1887, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1887, 0.93).
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_su_t1890, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1890, 0.95).
narrative_ontology:measurement(eternal_marriage_covenant__immutable_commandment_reading_su_t1904, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1904, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, federal_anti_polygamy_enforcement).

% DUAL FORMULATION NOTE:
% Part of the eternal_marriage_covenant constraint family (3 readings). This reading (immutable_commandment) has the highest extractiveness and suppression because it forecloses internal revision. The prophetic_override_reading has lower extractiveness (prophetic authority can adjust) but higher suppression of dissent within its own frame. The temporal_accommodation_reading has the lowest extractiveness (practice suspended) but creates a dual-obedience tension. All three link to federal_anti_polygamy_enforcement as the external pressure that reveals their structural differences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__immutable_commandment_reading, organized, 0.2).
constraint_indexing:directionality_override(eternal_marriage_covenant__immutable_commandment_reading, powerless, 0.93).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
