% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__moderate_iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__moderate_iconoclast_reading, []).

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
 *   constraint_id: decalogue_image_prohibition__moderate_iconoclast_reading
 *   human_readable: Moderate Iconoclast Image Prohibition: Three-Dimensional Statuary Forbidden, Two-Dimensional Images Regulated
 *   domain: theology/religious_authority
 *
 * SUMMARY:
 *   The moderate iconoclast reading of the second commandment (Exodus 20:4–5)
 *   permits two-dimensional religious imagery under strict regulatory control
 *   while categorically forbidding three-dimensional statuary. The
 *   justification is material-phenomenological: sculpture creates elevated
 *   idolatry risk through its spatial presence and tactile accessibility,
 *   while flat images permit devotional mediation with lower idolatry risk.
 *   This reading instantiates a specific extraction mechanism: it appears to
 *   permit a limited visual practice (two-dimensional images) while actually
 *   installing a detailed regulatory gate through which practitioners and
 *   communities must pass. The snare structure becomes visible: the
 *   'permission' is the cover story; the extraction is the bureaucratic
 *   overhead, approval uncertainty, and gatekeeping power that flows to the
 *   regulatory authority.
 *
 * KEY AGENTS:
 *   - regulatory_authority (institutional agenda-setter): administers the prohibition, determines permissible imagery, controls the approval apparatus
 *   - image_practitioners (moderate power, identity-locked): artisans and monks whose professional identity depends on regulatory permission
 *   - devotional_communities (powerless, constrained): seek visual aids to worship, subject to regulatory approval
 *   - radical_iconoclasts (excluded, powerful): argue for total prohibition; treat the two-dimensional exception as inadequate
 *   - iconodule_theologians (excluded, powerful): argue both dimensions are permitted under proper devotional framing
 *   - theological_interpreters (observer, institutional): adjudicate the commandment's meaning and ground the authority's legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.68).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.71).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Moderate Iconoclast Image Prohibition: Three-Dimensional Statuary Forbidden, Two-Dimensional Images Regulated").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theology/religious_authority").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, '3f223a24-42c4-4c99-ba0a-cd63688553c8').
narrative_ontology:cs_kernel_codification('3f223a24-42c4-4c99-ba0a-cd63688553c8', fixed_text).
narrative_ontology:cs_authority_grounding('3f223a24-42c4-4c99-ba0a-cd63688553c8', lineage).
narrative_ontology:cs_interpretation_layer_present('3f223a24-42c4-4c99-ba0a-cd63688553c8').
narrative_ontology:cs_reading_relation('3f223a24-42c4-4c99-ba0a-cd63688553c8', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f223a24-42c4-4c99-ba0a-cd63688553c8', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('3f223a24-42c4-4c99-ba0a-cd63688553c8', foundational, material_presence_correlates_idolatry_risk).
narrative_ontology:cs_axiom_status(material_presence_correlates_idolatry_risk, holdable).
narrative_ontology:cs_axiom_grounding('3f223a24-42c4-4c99-ba0a-cd63688553c8', material_presence_correlates_idolatry_risk, empirically_contingent).
narrative_ontology:cs_axiom('3f223a24-42c4-4c99-ba0a-cd63688553c8', secondary, three_dimensionality_heightens_material_presence).
narrative_ontology:cs_axiom_status(three_dimensionality_heightens_material_presence, holdable).
narrative_ontology:cs_axiom_grounding('3f223a24-42c4-4c99-ba0a-cd63688553c8', three_dimensionality_heightens_material_presence, empirically_contingent).
narrative_ontology:cs_reference_frame('3f223a24-42c4-4c99-ba0a-cd63688553c8', two_dimensional_regulated_mediation_framework).
narrative_ontology:cs_drift_state('3f223a24-42c4-4c99-ba0a-cd63688553c8', contemporary_reformed_iconoclasm_challenge, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3f223a24-42c4-4c99-ba0a-cd63688553c8', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, regulatory_authority).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, image_practitioners).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, devotional_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, devotional_communities).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_second_commandment).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, idolatry_material_mediation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the prohibition: determines which two-dimensional images are permissible, reviews proposals for new artwork, monitors compliance, and enforces the ban on statuary. Maintains detailed regulatory criteria and inspection apparatus. Controls access to religious visual culture through the gate of approval.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, regulatory_authority, agenda_setter,
    institutional, civilizational, analytical, regional).

% Artisans, monks, and artists who create religious imagery under the prohibition. Must submit work for approval, rework rejected pieces, navigate detailed regulatory criteria that shift with interpretive drift. Cannot practice three-dimensional work in their domain at all; two-dimensional work remains subject to arbitrary approval withdrawal. Professional identity and income depend on regulatory permission.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, image_practitioners, payer,
    moderate, biographical, identity_locked, regional).

% Parishes, monasteries, and lay communities that want visual aids to devotion. Under the prohibition they cannot commission statuary at all; permitted two-dimensional images are subject to regulatory approval, limiting their visual options and creating uncertainty about whether new devotional practices will be sanctioned. They benefit from reduced idolatry risk (as framed by the authority) but pay compliance and limitation costs.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, devotional_communities, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__moderate_iconoclast_reading, devotional_communities, beneficiary).

% Hold that ALL material imagery is prohibited; they reject the two-dimensional exception as merely a halfway measure that perpetuates idolatry. They argue for total prohibition but are excluded from the regulatory framing because the moderate reading treats their position as categorical rather than pragmatic.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, radical_iconoclasts, excluded,
    powerful, civilizational, trapped, regional).

% Argue that both two-dimensional and three-dimensional imagery are theologically legitimate under proper devotional framing (honor to prototype, not idolatry of the image). They reject the prohibition's material distinction as theologically confused. Excluded because the regulatory frame does not admit their core claim (the Incarnation sanctifies all matter).
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_theologians, excluded,
    powerful, civilizational, trapped, regional).

% Scholars and doctrinal bodies who interpret the second commandment and adjudicate whether the two-dimensional/three-dimensional distinction is scripturally justified. Their interpretations feed regulatory authority's legitimacy, but they are separated from the enforcement machinery.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, theological_interpreters, observer,
    institutional, civilizational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__moderate_iconoclast_reading, regulatory_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__moderate_iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents runaway material mediation in worship by establishing a boundary: three-dimensional statuary is categorically prohibited on the grounds that sculptural presence creates elevated idolatry risk; two-dimensional images are permitted but regulated to prevent their abuse as functional equivalents to banned statuary.
% TRANSFER_FUNCTION: Moves regulatory authority over visual religious culture from practitioners and communities (who could choose freely) to the institutional gate-keeper. Every image proposal must pass approval; rejected pieces cannot be realized; the definition of permissible imagery is held as administrative discretion. Communities and artists surrender choice in exchange for the authority's idolatry-prevention framing.
% ABSENT_VOICES: Radical iconoclasts and iconodule theologians are both structurally excluded: iconoclasts want total prohibition and see the two-dimensional exception as incoherent halfway measure; iconodules want both dimensions permitted and see the boundary as theologically confused. Neither gets a seat at the regulatory table because the moderate reading treats the issue as settled (the material boundary is justified) rather than contested.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished, practitioners would immediately resume statuary production, communities would commission the full range of sculptural and painted imagery without prior approval, and the visual culture of religious practice would reorganize around unrestricted material mediation. The regulatory apparatus would dissolve or be reoriented to other domains. A substantial portion of the constraint's enforcement machinery would become unnecessary.
% FOUNDING_PROBLEM: Worship communities were producing three-dimensional religious statuary in ways that practitioners and observers reported as functional idolatry—veneration of the object rather than direction through the object to the divine prototype. The problem framed as: material presence in sculpture creates psychological conditions for idolatrous attachment; flat images create less intimate material presence and therefore lower idolatry risk. The constraint was built to preserve devotional mediation (two-dimensional) while eliminating the highest-risk material form (three-dimensional).
% FOUNDING_PROBLEM_CORROBORATION: The regulatory authority attests the problem remains live and the prohibition effective. Radical iconoclasts attest they see the problem as inadequately addressed (the two-dimensional exception permits ongoing idolatry). Iconodule theologians attest the problem is misframed (the Incarnation doctrine resolves the idolatry concern and renders the distinction theologically incoherent). Independent historical and phenomenological studies from outside the benefiting parties document both actual instances of idolatrous attachment to statuary AND instances of equal attachment to two-dimensional images, undermining the claimed material distinction.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__moderate_iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint's real function is regulatory gatekeeping: the authority collects compliance costs (approval processes, rework, uncertainty, limitation of practice) without proportional service delivery. The material boundary (three-dimensional forbidden, two-dimensional permitted) is the Trojan horse—it appears empirically justified but functions as cover for extraction. Suppression is high (0.71) because practitioners face identity lock (professional identity fused with the practice) and communities face constrained exit (cannot practice their devotion freely; can only petition for permission). Theater rises gradually (0.38 to 0.52) as the regulatory apparatus develops more elaborate criteria and justificatory rhetoric to defend what was initially a simpler boundary, suggesting performative maintenance growing over time. The measurement grid is shared: every metric is authored at each examined time point. The plateau from t=25 onward shows the constraint reaching stable extraction and theatrical intensity—no further intensification, but hardened at the snare floor.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory authority's seat, the constraint is a well-justified compromise: it permits some visual practice (two-dimensional) while eliminating the highest-risk form (three-dimensional), solving the idolatry problem through material boundaries. From the practitioner's seat, the constraint is a gatekeeping apparatus: the permission is worthless without approval, and approval is discretionary. From the radical iconoclast seat (excluded from the table), the constraint is an inadequate halfway measure that perpetuates the core problem. From the iconodule seat (also excluded), the constraint is theologically confused—it treats matter as inherently risky rather than sanctified by the Incarnation. These divergent readings flow from structural relationships, not preference. The engine computes per-seat classification from the power atoms and exit options; the authored claim (snare) reflects the payer seats' assessment of what the constraint actually does.
 *
 * DIRECTIONALITY LOGIC:
 *   The regulatory authority sits at the beneficiary end of the directionality spectrum (d near 0.0): it controls the definition of permissible practice, collects compliance costs without bearing them, and has analytical exit (it can change the rules at will). Image practitioners sit at the target end (d near 1.0): they cannot exit the religious visual culture domain (identity-locked) and cannot practice their craft without permission. Devotional communities also approach the target end (constrained exit, no unilateral ability to change the rules). The excluded radical iconoclasts and iconodules are structurally prevented from contesting the distinction at the decision table—they have powerful institutional positions but are trapped outside the regulatory frame, so their exit options are listed as 'trapped' (they cannot reframe the question from within the system). The moderate reading deliberately cuts them out by treating the material distinction as settled fact rather than live theological question.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested: was the problem genuine idolatry (attachment to three-dimensional objects) or was the problem the regulatory authority seeking to consolidate power over visual culture? The moderate reading claims to address genuine idolatry through a material distinction; radical iconoclasts claim the problem was inadequately addressed; iconodules claim the problem is misframed (matter is not inherently problematic). The classification as snare resolves this: if the constraint were a pure coordination solution to a genuine coordination problem, it would be a rope (beneficiaries and victims would both be better off without it, but it solves a real problem they cannot solve individually). Instead, the constraint extracts compliance costs through detailed regulation while appearing to permit practice. The beneficiary (regulatory authority) is the only seat that gains unambiguously—practitioners and communities gain uncertainty and limitation. This asymmetry, combined with active enforcement machinery designed to police the two-dimensional boundary, indicates snare-type extraction using a theological justification as cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_idolatry_correlation,
    'Does three-dimensional material presence genuinely correlate with elevated idolatry risk relative to two-dimensional images, or is the distinction a post-hoc rationalization for regulatory gatekeeping?',
    'Comparative phenomenological and historical study of devotional communities under three-dimensional restrictions vs. unrestricted regimes, measuring actual idolatrous attachment across both material forms. Independent theological analysis of whether the material distinction is scripturally justified or is an interpretive add-on.',
    'If the correlation is real and significant, the constraint functions as genuine coordination (solving a real idolatry problem). If the correlation is absent or weak, the constraint is pure extraction using theological cover—the snare classification is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(material_idolatry_correlation, empirical, 'Whether the three-dimensional/two-dimensional boundary reflects genuine risk differentiation or regulatory gatekeeping.').

omega_variable(
    regulatory_authority_gatekeeping_mechanism,
    'Is the regulatory authority''s approval process for two-dimensional images structured to minimize idolatry risk, or is it structured to maximize administrative control and compliance extraction?',
    'Examination of approved vs. rejected image proposals; analysis of approval criteria stability over time; measurement of rework and resubmission rates; comparison to the stated idolatry-prevention logic.',
    'Approval criteria that correlate with resource control (favoring authority-aligned imagery, penalizing independent interpretation) would indicate gatekeeping extraction rather than safety review. Criteria that track idolatry-risk metrics would support the coordination framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_authority_gatekeeping_mechanism, empirical, 'The true function of the approval apparatus: safety review or regulatory gatekeeping.').

omega_variable(
    theological_legitimacy_of_material_distinction,
    'Is the three-dimensional/two-dimensional distinction coherent within Incarnation doctrine, or does Incarnation theology render the distinction theologically indefensible?',
    'Systematic analysis of how major theological traditions (early church fathers, medieval scholasticism, Reformation debates, modern theology) treat the material boundary. Examination of whether the moderate reading''s material distinction is consistent with or contradictory to the tradition''s own Incarnation commitments.',
    'If the distinction is theologically incoherent within the reading''s own tradition, the authority is defending an unjustifiable position through enforcement machinery—increasing confidence in snare classification. If the distinction is defended within major streams of the tradition, the classification remains ambiguous on theological grounds (empirical idolatry evidence becomes the tie-breaker).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_legitimacy_of_material_distinction, conceptual, 'The internal theological coherence of the moderate reading''s core distinction.').

omega_variable(
    excluded_reading_suppression_mechanism,
    'Are radical iconoclasts and iconodules excluded from the regulatory table through procedural rules or through substantive foreclosure of their positions?',
    'Examination of past attempts by excluded readings to challenge the prohibition; analysis of whether procedures exist for theological reinterpretation or whether the moderate reading is treated as fixed. Study of institutional mechanisms that prevent iconoclasts and iconodules from reframing the question.',
    'Procedural exclusion would indicate the moderate reading is one live option among contested others; substantive foreclosure would indicate the authority has already decided the theological question and uses the gate to enforce it—confirming snare extraction of theological legitimacy, not just compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_reading_suppression_mechanism, empirical, 'Whether excluded readings are blocked by procedure or by substantive authority refusal to reframe.').

omega_variable(
    identity_lock_mechanism_for_practitioners,
    'Can image practitioners exit the religious visual culture domain, or is their professional and spiritual identity so fused with the practice that exit is cognitively and socially unthinkable?',
    'Post-exit trajectory study: when practitioners leave the constrained domain, do they report suppression relief (indicating structural constraint) or ongoing psychological identification with the practice (indicating internalized suppression)? Do displaced practitioners create alternative communities or reframe their identity?',
    'If suppression is internalized (identity-fused), the constraint''s effective suppression is higher than measured—practitioners carry the restriction with them after exit. If suppression is purely structural, the classification holds as snare-type extraction of compliance. Internalized identity lock would indicate deeper capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_practitioners, empirical, 'Structural vs. internalized suppression mechanism for identity-locked practitioners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(deca_tr_t5, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement(deca_tr_t10, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(deca_tr_t15, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 15, 0.47).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(deca_tr_t25, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 25, 0.51).
narrative_ontology:measurement(deca_tr_t30, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement(deca_be_t5, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(deca_be_t10, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(deca_be_t15, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(deca_be_t25, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(deca_be_t30, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(deca_su_t5, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(deca_su_t10, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(deca_su_t15, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(deca_su_t25, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(deca_su_t30, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__moderate_iconoclast_reading, 0.12).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconodule_reading).

% DUAL FORMULATION NOTE:
% The kernel decalogue_image_prohibition decomposes into three constraint stories representing three live readings of the second commandment. The moderate_iconoclast_reading (this story) occupies the middle position: it forbids three-dimensional statuary while permitting two-dimensional images under regulation. The iconoclast_reading forbids all material imagery; the iconodule_reading permits both under proper devotional framing. Each reading has a distinct epsilon (extractiveness), distinct beneficiary/victim structure, and distinct type. The ε values differ because the readings assess the standing arrangement (the current regulatory state) by different lights: from the radical iconoclast perspective, the moderate reading permits ongoing idolatry (moderate ε value misconstrues the problem); from the iconodule perspective, the moderate reading imposes unjustified restriction (moderate ε value reflects constraint the reading would not accept as legitimate). All three stories link via network.affects_constraints to enable constraint-family analysis and reading-contention detection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__moderate_iconoclast_reading, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
