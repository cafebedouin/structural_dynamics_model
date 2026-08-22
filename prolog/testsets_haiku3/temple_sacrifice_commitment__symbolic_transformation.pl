% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Temple Sacrifice Commitment via Symbolic Transformation Reading
 *   domain: religious/legal/commitment-system
 *
 * SUMMARY:
 *   This constraint models the rabbinic symbolic transformation reading of
 *   the sacrifice commitment after the Second Temple's destruction (70 CE).
 *   In this reading, the divine command for temple sacrifice does not remain
 *   suspended awaiting restoration; rather, it has been
 *   authorized-transformed into an equivalent commitment occupied through
 *   prayer and textual study. The transformation is pronounced by rabbinic
 *   authority as a legitimate reinterpretation, not a substitute or
 *   workaround. This reading coexists with three sibling readings: (1)
 *   performance_only — material instantiation is non-negotiable and the
 *   commitment remains suspended; (2) hybrid_preparatory — study maintains
 *   the commitment in a suspended state pending temple restoration; (3)
 *   study_as_exercise — intellectual engagement itself performs the command.
 *   The extraction measured here represents the cost borne by those who hold
 *   alternative readings and by the institutional pressure required to
 *   maintain transformation authority. The theater_ratio tracks the extent to
 *   which the transformation's legitimation is performatively maintained
 *   versus functionally occupying the underlying commitment problem.
 *
 * KEY AGENTS:
 *   - rabbinic_authority_structure: pronounces and maintains the transformation; collects institutional authority from this position (institutional/mobile)
 *   - material_performance_obligators: hold original-performance reading; subordinated interpretive standing; exit requires theological capitulation (moderate/identity_locked)
 *   - practitioners_of_rabbinic_judaism: benefit from normalized prayer/study occupation; absorb diffuse enforcement cost (organized/constrained)
 *   - performance_only_communities: hold competing reading; structurally marginalized; cannot reverse transformation (powerless/trapped)
 *   - messianic_expectation_holders: accept interim reading; coexist in uneasy tension; subordinated but not forcefully excluded (moderate/identity_locked)
 *   - halakhic_scholarship_community: produces scholarly consensus; maintains interpretive legitimacy; analytical distance (powerful/arbitrage)
 *   - theological_dissent_holders: record dissent against unauthorized transformation; lack institutional standing (powerless/identity_locked)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.68).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.71).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.68).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Temple Sacrifice Commitment via Symbolic Transformation Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious/legal/commitment-system").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, 'be41eecc-3ad4-4dcd-b0e3-fde74d9edf32').
narrative_ontology:cs_kernel_codification('be41eecc-3ad4-4dcd-b0e3-fde74d9edf32', fixed_text).
narrative_ontology:cs_authority_grounding('be41eecc-3ad4-4dcd-b0e3-fde74d9edf32', lineage).
narrative_ontology:cs_interpretation_layer_present('be41eecc-3ad4-4dcd-b0e3-fde74d9edf32').
narrative_ontology:cs_reading_relation('be41eecc-3ad4-4dcd-b0e3-fde74d9edf32', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('be41eecc-3ad4-4dcd-b0e3-fde74d9edf32', temple_sacrifice_commitment__hybrid_preparatory, influences).
narrative_ontology:cs_reading_relation('be41eecc-3ad4-4dcd-b0e3-fde74d9edf32', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_axiom('be41eecc-3ad4-4dcd-b0e3-fde74d9edf32', foundational, transformation_authorizes_new_instantiation).
narrative_ontology:cs_axiom_status(transformation_authorizes_new_instantiation, holdable).
narrative_ontology:cs_axiom_grounding('be41eecc-3ad4-4dcd-b0e3-fde74d9edf32', transformation_authorizes_new_instantiation, conventional).
narrative_ontology:cs_axiom('be41eecc-3ad4-4dcd-b0e3-fde74d9edf32', foundational, prayer_study_symbolically_equivalent_to_sacrifice).
narrative_ontology:cs_axiom_status(prayer_study_symbolically_equivalent_to_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('be41eecc-3ad4-4dcd-b0e3-fde74d9edf32', prayer_study_symbolically_equivalent_to_sacrifice, empirically_contingent).
narrative_ontology:cs_reference_frame('be41eecc-3ad4-4dcd-b0e3-fde74d9edf32', rabbinic_hermeneutic_authority_post_destruction).
narrative_ontology:cs_drift_state('be41eecc-3ad4-4dcd-b0e3-fde74d9edf32', contemporary_institutional_entrenchment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('be41eecc-3ad4-4dcd-b0e3-fde74d9edf32', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, material_performance_obligators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, practitioners_of_rabbinic_judaism).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, practitioners_of_rabbinic_judaism).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, rabbinic_hermeneutic_authority).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, symbolic_equivalent_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms of commitment occupation by pronouncing the transformation. Maintains interpretive authority over textual basis of transformation validity. Enforces through institutional gatekeeping: canonical status for transformation reading, marginalizing alternative readings in mainstream Jewish institutions. Collects the authority to define what constitutes fidelity to divine command in post-temple context.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure, agenda_setter,
    institutional, civilizational, mobile, global).

% Adherents to the performance-only reading, holding that the sacrifice commitment requires material instantiation in the temple and remains suspended without it. They bear the cost of their dissent: interpretive subordination within mainstream Judaism, institutional pressure to conform, structural impossibility of activating their reading without reversing the rabbinic transformation. Exiting their reading requires theological capitulation and abandoning their tradition-internal understanding of divine command.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, material_performance_obligators, payer,
    moderate, civilizational, identity_locked, global).

% Benefit from the transformation's legitimacy: prayer and textual study provide an executable form of commitment occupation that does not require temple restoration or political sovereignty to restore sacrifice infrastructure. They bear diffuse cost through institutional enforcement of the transformation's authority: suppression of alternative readings, ritual performance maintaining transformation legitimacy, and the structural subordination of dissenting interpretive communities.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, practitioners_of_rabbinic_judaism, beneficiary,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, practitioners_of_rabbinic_judaism, payer).

% Karaite Jews, Samaritan communities, and contemporary restorationists holding the performance-only reading. Structurally excluded from mainstream institutional authority that pronounces the transformation. Their reading is actively delegitimized as archaic or theologically deficient. They cannot reverse the transformation through institutional channels (lack power and institutional standing). Exit from their reading requires accepting rabbinic hermeneutic authority, which their theological framework rejects.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, performance_only_communities, excluded,
    powerless, civilizational, trapped, regional).

% Holders of the hybrid_preparatory reading: the commitment remains substantially suspended pending messianic temple restoration; prayer/study are legitimate interim occupation but not final transformation. Their position coexists uncomfortably with the symbolic transformation reading within mainstream Judaism. Less forcefully excluded than performance-only communities (accepted as legitimate within orthodox framework) but subordinated: their reading is treated as preliminary rather than terminal, their expectation for temple restoration as eschatological rather than binding present obligation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, messianic_expectation_holders, excluded,
    moderate, civilizational, identity_locked, global).

% Produces the scholarly consensus interpreting the textual basis of transformation validity. Maintains analytic distance from institutional enforcement while producing the intellectual legitimacy that justifies the transformation. Capable of revising consensus (arbitrage option) but structurally incentivized toward transformation-support because mainstream institutional authority backs transformation reading.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, halakhic_scholarship_community, observer,
    powerful, civilizational, arbitrage, global).

% Individual scholars, rabbis, and practitioners who hold that the transformation is unauthorized drift — that the divine command for material sacrifice has not been rescinded or transformed, only suspended. Their dissent is recorded in halakhic literature but administratively subordinated. They cannot exit their reading without abandoning their understanding of divine command fidelity; they cannot institutionally reverse the transformation due to powerlessness.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, theological_dissent_holders, excluded,
    powerless, civilizational, identity_locked, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__symbolic_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the structural problem of how a commitment predicated on material temple performance persists after the temple's destruction: the transformation reading coordinates all practitioners around a unified, executable form of the commitment (prayer and study) that does not require temple restoration.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional power from material-performance obligators and dissenting communities to the rabbinic authority structure. The structure that pronounces the transformation collects legitimacy to define what occupies divine command in the absence of temple conditions. Practitioners transfer their time and attention from seeking material temple restoration to normative engagement with prayer and textual study.
% ABSENT_VOICES: Communities holding the performance-only reading (Karaites, Samaritans, contemporary restorationists) are structurally excluded from the institutional conversation that legitimizes the transformation. They would attest that the commitment remains suspended and that the transformation is unauthorized drift, not authorized reinterpretation. Their absence from the authorizing discussion is not accidental but structurally maintained.
% DISAPPEARANCE_RATIONALE: If the transformation disappeared and material-performance obligation resumed, rabbinic Judaism would reorganize toward either temple preparation (accepting hybrid_preparatory reading) or acknowledge suspension (accepting performance-only reading). If the transformation authority were reasserted against competing readings, those readings would persist as marginalized alternatives. The verdict is contested because the transformation's metaphysical status — whether it genuinely accomplishes commitment metamorphosis or merely authorizes institutional drift — is the core dispute.
% FOUNDING_PROBLEM: After the Second Temple's destruction in 70 CE, a commitment predicated on material sacrifice in the temple became impossible to execute. The founding problem: how to maintain fidelity to a binding divine command when its material instantiation is no longer available.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authority attests the founding problem (temple destruction made material performance impossible) and claims the transformation solves it (prayer/study now occupy the commitment). Scholarly halakhic analysis outside the benefiting party attests the problem existed and that the transformation was pronounced. However, dissenting communities (performance-only holders, some messianic expectation holders) attest the problem is NOT solved by transformation — they hold the commitment remains suspended. Outside observers from academic theology and history of religions confirm the transformation was authorized but do NOT confirm it resolved the underlying commitment problem — that remains contested across reading communities.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects the cost borne by dissenting communities and performance-only holders. The transformation is high-extractiveness because its authority claim — that prayer/study are the NEW instantiation, not a substitute — depends on suppressing the performance-only reading as incoherent. If the transformation is authorized, the beneficiary (rabbinic authority) collects the interpretive power to define what occupies the commitment. If the transformation is unauthorized drift, it is a snare disguised as rope, and victims include those whose reading is delegitimized. Suppression (0.71) is high because alternative readings require active institutional suppression to remain marginal; performance-only communities cannot simply exist as a coequal reading within mainstream Jewish authority. Theater (0.52) indicates that roughly half the transformation maintenance is genuinely substantive (prayer/study networks, textual study institutions) and half is performative legitimation (liturgical framing, scholarly consensus-building, institutional gatekeeping). The measurement series spans 1900 years to capture the transformation's initial imposition (low extractiveness at t=0, high suppression cost in the first century to stabilize it) and its later entrenchment (extractiveness plateaus at 0.68 because the institutional victory is complete; suppression requirement stabilizes because the transformation is now background practice, not active contestation).
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic authority seat experiences this as genuine coordination accomplished through interpretive authority: prayer/study is a successful commitment occupation that resolves the suspension problem. The material-performance-obligator seat experiences this as unauthorized extraction: the commitment remains suspended, and the transformation is an illegitimate authority claim that prevents material restoration. The halakhic scholarship community (observer seat) experiences this as a technical question about interpretive validity, not extraction. The engine should compute radically different type classifications per seat: the agenda-setter seat sees rope (successful coordination), the payer seat sees snare (suppressed alternative reading), the observer seat computes based on the structural metrics alone. This divergence is NOT a failure to clarify the constraint; it is the measurement of how authority claims can appear coordinative from one seat and extractive from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority sits at d ≈ 0.15 (beneficiary position: collects interpretive power, mobile exit options, powerful institutional position). Material-performance-obligators sit at d ≈ 0.85 (target position: subordinated reading, identity-locked exit, moderate power constrains their ability to challenge). Practitioners of rabbinic Judaism sit at d ≈ 0.50 (symmetric: genuine benefit from having an executable commitment form, diffuse cost from institutional suppression of alternatives). Performance-only communities sit at d ≈ 0.90 (nearly full targets: their reading is suppressed, they have no exit without capitulating, they are powerless). The directionality is NOT overridden; it derives from the beneficiary/victim declarations and the power/exit atoms. The rabbinic structure benefits from the transformation's authority; dissenting communities bear the cost through institutional subordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to maintain a commitment whose material instantiation became impossible) is contested in resolution. Rabbinic authority claims the transformation solves it; performance-only holders claim the commitment remains suspended and unsolved. The transformation reading avoids mandatrophy (commitment death) by redefining what occupies the commitment. But this redefinition is the source of the extraction: the authority structure that pronounces the transformation collects the power to define legitimate occupation. If the founding problem were uncontestedly solved, extractiveness would be lower and theater would be lower; the high theater ratio (0.52) indicates that maintenance of the transformation's legitimacy requires performative work — the constraint is not self-evidently true but requires institutional reinforcement. Mandatrophy has NOT formally resolved (no base_properties.mandatrophy_resolved flag) because the dispute remains live: some communities deny the transformation accomplishes the necessary re-commitment, holding instead that suspension is the honest state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_authorization_source,
    'What grants the rabbinic authority the power to authorize transformation of a divine command? Is this power itself divinely mandated, institutionally constructed, or a fusion of both?',
    'Textual analysis of Talmudic passages claiming hermeneutic authority over divine commands; comparative analysis of other divine-command transformations in Jewish law; examination of whether authorization-power itself requires higher authorization.',
    'If authorization is divine, extractiveness is legitimately lower (genuine coordination of a new commitment form). If authorization is institutionally constructed, extractiveness is higher and the transformation becomes authorized drift rather than transformation of the commitment itself. This resolves whether the constraint is tangled_rope (genuine coordination with asymmetric benefit) or snare (extracted authority disguised as coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transformation_authorization_source, conceptual, 'Whether transformation authority is divinely mandated or institutionally constructed.').

omega_variable(
    commitment_identity_across_transformation,
    'Does the commitment remain the same entity after symbolic transformation, or does transformation create a new commitment that shares ancestry with but is distinct from the original material-sacrifice commitment?',
    'Conceptual analysis of commitment identity criteria: does identity require material form, intentional continuity, or structural role? Comparison with other divine-command transformations (e.g., Levitical service transformed by temple loss; priestly service transformed by diaspora). Survey of how practitioners experience the identity question.',
    'If the commitment is identical across transformation, rabbinic authority successfully re-instantiated it and extractiveness is lower. If it is a new commitment claiming ancestry, the original commitment remains suspended and the transformation is relabeling, making extractiveness higher for those who hold original-commitment obligation. This determines whether dissenting communities are suppressed alternative readings or holders of a genuinely distinct (suspended) commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_identity_across_transformation, conceptual, 'Whether the commitment identity persists through transformation or is constitutively altered.').

omega_variable(
    performance_only_marginalization_mechanism,
    'Is performance-only reading marginalized because the transformation is legitimate and the reading is structurally incoherent, or because rabbinic institutional power suppresses it despite its conceptual validity?',
    'Historical analysis of minority-reading communities: do they persist because they hold coherent alternatives (suggesting suppression), or do they fade because the reading is internally unstable (suggesting legitimacy)? Comparative analysis with other Talmudic disputes where minority readings were preserved vs. marginalized.',
    'If performance-only reading is legitimately incoherent, rabbinic suppression is justified as coordination around the correct reading; extractiveness is lower. If it is coherently alternative, suppression is institutional power protecting the transformation; extractiveness is higher and theater_ratio rises (the transformation requires performative maintenance, not just substantive commitment occupation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_only_marginalization_mechanism, empirical, 'Whether performance-only reading is marginalized by legitimacy or by institutional power.').

omega_variable(
    symbolic_equivalence_validity,
    'Does prayer and textual study actually occupy the same divine-command obligation as material sacrifice? Are they substitutes (same obligation, different form), new obligations, or a hybrid?',
    'Theological analysis of prayer/study function in halakhic literature; phenomenological study of practitioner experience; comparison of obligation structure (frequency, specificity, consequences of non-performance) across material and symbolic forms.',
    'If symbolic equivalence is valid, the transformation genuinely resolves the founding problem and extractiveness reflects legitimate institutional authority to redefine. If equivalence fails (symbolic forms are new obligations distinct from original material commitment), the original commitment remains suspended and the transformation is relabeling, supporting higher extractiveness and the performance-only reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_equivalence_validity, empirical, 'Whether prayer/study are validly equivalent to material sacrifice for divine-command purposes.').

omega_variable(
    kernel_reading_commissioning,
    'Is this reading one of an authorized set of interpretations of the same kernel, or does this reading itself instantiate a NEW kernel (commitment redefinition) that displaces the original?',
    'Genealogical analysis of the kernel''s historical instantiations: does the Talmud treat transformation as one reading among alternatives, or as THE reinterpretation that becomes binding? Is the original performance-demand recorded as still-valid alternative or as superseded?',
    'If this is one authorized reading, the sibling readings remain structurally live (even if institutionally subordinated); the constraint should carry high resistance and low accessibility_collapse (alternatives persist in principle). If this reading creates a new kernel displacing the original, alternatives become archival rather than live; accessibility_collapse is higher (the original commitment is closed off) and the transformation is more extractive for those refusing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commissioning, conceptual, 'Whether the reading is one interpretation of a persistent kernel or the kernel''s authoritative redefinition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(m_theater_70ce, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(m_theater_70ce, projected).
narrative_ontology:measurement(m_theater_170ce, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 100, 0.38).
narrative_ontology:measurement_basis(m_theater_170ce, observed).
narrative_ontology:measurement(m_theater_470ce, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 400, 0.46).
narrative_ontology:measurement_basis(m_theater_470ce, observed).
narrative_ontology:measurement(m_theater_870ce, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 800, 0.51).
narrative_ontology:measurement_basis(m_theater_870ce, observed).
narrative_ontology:measurement(m_theater_1270ce, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1200, 0.52).
narrative_ontology:measurement_basis(m_theater_1270ce, observed).
narrative_ontology:measurement(m_theater_1900ce, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1900, 0.52).
narrative_ontology:measurement_basis(m_theater_1900ce, observed).

% Extraction over time
narrative_ontology:measurement(m_extract_70ce, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(m_extract_70ce, projected).
narrative_ontology:measurement(m_extract_170ce, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 100, 0.35).
narrative_ontology:measurement_basis(m_extract_170ce, observed).
narrative_ontology:measurement(m_extract_470ce, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 400, 0.52).
narrative_ontology:measurement_basis(m_extract_470ce, observed).
narrative_ontology:measurement(m_extract_870ce, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 800, 0.64).
narrative_ontology:measurement_basis(m_extract_870ce, observed).
narrative_ontology:measurement(m_extract_1270ce, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1200, 0.68).
narrative_ontology:measurement_basis(m_extract_1270ce, observed).
narrative_ontology:measurement(m_extract_1900ce, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1900, 0.68).
narrative_ontology:measurement_basis(m_extract_1900ce, observed).

% Suppression requirement over time
narrative_ontology:measurement(m_supp_70ce, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(m_supp_70ce, projected).
narrative_ontology:measurement(m_supp_170ce, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 100, 0.48).
narrative_ontology:measurement_basis(m_supp_170ce, observed).
narrative_ontology:measurement(m_supp_470ce, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 400, 0.62).
narrative_ontology:measurement_basis(m_supp_470ce, observed).
narrative_ontology:measurement(m_supp_870ce, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 800, 0.68).
narrative_ontology:measurement_basis(m_supp_870ce, observed).
narrative_ontology:measurement(m_supp_1270ce, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1200, 0.71).
narrative_ontology:measurement_basis(m_supp_1270ce, observed).
narrative_ontology:measurement(m_supp_1900ce, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1900, 0.71).
narrative_ontology:measurement_basis(m_supp_1900ce, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__symbolic_transformation, 0.12).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the temple_sacrifice_commitment kernel. Four structurally distinct constraints instantiate four competing readings: (1) symbolic_transformation (THIS FILE) — prayer/study are authorized new instantiation; (2) performance_only — commitment remains suspended without material performance; (3) hybrid_preparatory — study maintains suspended commitment pending restoration; (4) study_as_exercise — intellectual engagement itself performs the command. These readings share the same referent (temple sacrifice commitment after temple destruction) but differ radically in their claims about what occupies the commitment and whether transformation is authorized. ε-invariance principle: each reading produces its own ε (extractiveness is different for each because the victim set and authority claims differ). The four stories form a constraint family linked by network.affects_constraints. The symbolic_transformation reading (this file) influences the others: if this reading is authoritative, it constrains the structural space available to performance_only (which must now explain why an authorized transformation is invalid) and coexists with hybrid_preparatory and study_as_exercise (which accept transformation but differ on finality/adequacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
