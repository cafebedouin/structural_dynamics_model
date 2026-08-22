% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__moderate_iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Moderate Iconoclast Prohibition: Three-Dimensional Statuary Forbidden, Two-Dimensional Images Regulated
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   The moderate iconoclast reading of the Decalogue prohibition instantiates
 *   a middle-ground rule: three-dimensional religious statuary is absolutely
 *   forbidden on grounds of heightened idolatry risk, but two-dimensional
 *   images are permitted under strict regulatory oversight. This reading
 *   emerged historically as a compromise between strict iconoclasts (who
 *   forbid all religious imagery) and iconodules (who permit full
 *   three-dimensional representation as spiritually valid post-Incarnation).
 *   The moderate reading claims to honor both the received prohibition and
 *   devotional access to material representation by distinguishing
 *   dimensionality as a proxy for idolatry risk. Structurally, it operates as
 *   a snare: the regulatory authority maintains gatekeeping power by
 *   controlling which two-dimensional images comply with regulation,
 *   extracting compliance costs from artists and iconodule communities while
 *   preserving the appearance of honoring the prohibition. The boundary
 *   between forbidden three-dimensional and regulated two-dimensional is
 *   itself the site of extraction — the authority that interprets it collects
 *   decision-making power.
 *
 * KEY AGENTS:
 *   - Regulatory Authority: institutional agenda-setter, controls interpretation and enforcement of the boundary
 *   - Religious Artists: moderate-power payers, constrained to approved two-dimensional forms; income dependent on regulatory approval
 *   - Iconodule Communities: organized payers with identity-lock (their theological commitment to three-dimensional mediation is identity-constitutive), face suppression of their full devotional expression
 *   - Sculptors and Artisans: powerless victims, trapped by prohibition from practicing their primary skill; dependent on now-criminalized religious patronage
 *   - Strict Iconoclast Factions: excluded, believe the moderate reading itself violates the prohibition
 *   - Theological Observers: analytical seats documenting how readings instantiate different constraint types
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.68).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.72).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Moderate Iconoclast Prohibition: Three-Dimensional Statuary Forbidden, Two-Dimensional Images Regulated").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, 'dd1ee877-0008-4c85-948f-ebf239df3ce1').
narrative_ontology:cs_kernel_codification('dd1ee877-0008-4c85-948f-ebf239df3ce1', fixed_text).
narrative_ontology:cs_authority_grounding('dd1ee877-0008-4c85-948f-ebf239df3ce1', lineage).
narrative_ontology:cs_interpretation_layer_present('dd1ee877-0008-4c85-948f-ebf239df3ce1').
narrative_ontology:cs_reading_relation('dd1ee877-0008-4c85-948f-ebf239df3ce1', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('dd1ee877-0008-4c85-948f-ebf239df3ce1', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('dd1ee877-0008-4c85-948f-ebf239df3ce1', foundational, dimensionality_correlates_idolatry_risk).
narrative_ontology:cs_axiom_status(dimensionality_correlates_idolatry_risk, holdable).
narrative_ontology:cs_axiom_grounding('dd1ee877-0008-4c85-948f-ebf239df3ce1', dimensionality_correlates_idolatry_risk, empirically_contingent).
narrative_ontology:cs_axiom('dd1ee877-0008-4c85-948f-ebf239df3ce1', foundational, regulatory_oversight_enables_safe_mediation).
narrative_ontology:cs_axiom_status(regulatory_oversight_enables_safe_mediation, holdable).
narrative_ontology:cs_axiom_grounding('dd1ee877-0008-4c85-948f-ebf239df3ce1', regulatory_oversight_enables_safe_mediation, instrumental).
narrative_ontology:cs_reference_frame('dd1ee877-0008-4c85-948f-ebf239df3ce1', theodox_image_compromise).
narrative_ontology:cs_drift_state('dd1ee877-0008-4c85-948f-ebf239df3ce1', contemporary_enforcement_intensification, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dd1ee877-0008-4c85-948f-ebf239df3ce1', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, regulatory_authority).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, religious_artists).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, sculptors_and_artisans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The church hierarchy or theological council that interprets and enforces the prohibition. Sets the boundary between permitted two-dimensional images and forbidden three-dimensional statuary. Reviews proposed artworks, decides what falls within strict regulation, and maintains the gatekeeping apparatus. Justifies the distinction on grounds of idolatry risk while claiming to preserve legitimate devotional access through flatter media.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, regulatory_authority, agenda_setter,
    institutional, generational, analytical, regional).

% Create religious imagery under the prohibition's constraints. Cannot produce three-dimensional religious statuary without violating the rule; two-dimensional work requires regulatory approval, limiting artistic freedom. Face career income loss if their work is deemed non-compliant. Exit would mean either abandoning religious art entirely or relocating to jurisdictions with different rules — both costly.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, religious_artists, payer,
    moderate, biographical, constrained, regional).

% Worship communities that believe full three-dimensional representation is spiritually legitimate and that matter itself can mediate the divine. Constrained by the prohibition from using the material forms they consider most effective for devotion. Retain access to regulated two-dimensional imagery but at the cost of compliance monitoring and the knowledge that fuller expression is forbidden. Their theological identity is bound to the belief that three-dimensional form is not inherently idolatrous, making exit from the constraint conceptually incompatible with staying in the tradition.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_communities, payer,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_communities, beneficiary).

% Craftspeople whose livelihood depends on three-dimensional work but who live in jurisdictions under the prohibition. Cannot legally practice their primary skill in the religious market; must either retrain for secular sculpture, migrate, or operate clandestinely. Have no institutional power to challenge the rule and depend on continued patronage from religious communities — but that patronage is criminalized.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, sculptors_and_artisans, payer,
    powerless, biographical, trapped, regional).

% Religious communities that believe even two-dimensional images constitute idolatry and that the prohibition should be absolute. The moderate reading forecloses their position by legitimizing image use itself; they are excluded from the compromise because accepting it would mean endorsing what they believe violates the commandment. Their dissent is suppressed by the regulatory authority's monopoly on interpretation.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, strict_iconoclast_factions, excluded,
    organized, generational, constrained, regional).

% Scholars and theologians analyzing the interpretation of the prohibition across traditions. Witness how different readings instantiate different constraints (full prohibition, full permission with safeguards, moderate compromise) and document how the regulatory authority justifies the boundary between what is forbidden and what is regulated as acceptable.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, theological_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__moderate_iconoclast_reading, regulatory_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__moderate_iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared practice of image-based devotion by establishing a boundary condition: distinguishes material forms that carry inherent idolatry risk (three-dimensional statuary, which invites physical veneration) from those that can be regulated for safety (two-dimensional images, which maintain interpretive distance). Solves the problem of whether and how religious communities can use material representation without violating the received prohibition.
% TRANSFER_FUNCTION: Transfers regulatory authority from the faithful themselves to the institutional interpreter. Community members surrender the right to decide what forms of material representation are legitimate for their devotion; the regulatory authority collects that decision-making power in exchange for permitting limited two-dimensional use. Also extracts compliance costs: time, resources, and artistic constraint from those producing or using permitted images.
% ABSENT_VOICES: Strict iconoclasts who believe even two-dimensional images are idolatrous; iconodules who believe full three-dimensional representation is theologically necessary; secular artisans and patrons whose material culture is entirely removed from the religious economy by the prohibition. None of these seats participate in setting the boundary.
% DISAPPEARANCE_RATIONALE: If the prohibition and its regulatory apparatus disappeared, three-dimensional religious statuary would be produced and venerated openly by communities currently constrained; strict iconoclasts would mount more forceful opposition to image use; iconodules would produce and use fuller material forms; artistic patronage in the religious market would reorganize around three-dimensional work. The constraint does not prevent idolatry desire or iconoclast objection; it channels both through the regulatory gate, preventing them from settling without institutional mediation.
% FOUNDING_PROBLEM: How can a religious community sustain devotional imagery while honoring the received prohibition against idolatry? The tradition teaches that material representation can seduce the faithful into worshiping the object rather than what it represents. The founding problem is simultaneously theological (what does the commandment forbid?) and pastoral (how do communities access the spiritual benefits of material mediation while staying within orthodoxy?).
% FOUNDING_PROBLEM_CORROBORATION: The regulatory authority attests the founding problem is live and argues that the moderate reading solves it by distinguishing high-idolatry-risk three-dimensional form from regulated low-risk two-dimensional use. Strict iconoclasts attest the founding problem is only solved by total prohibition. Iconodules attest the founding problem is based on a false premise — matter is not inherently idolatrous post-Incarnation. Independent theological historians note that the 'founding problem' framing itself varies by reading and that each reading selects different aspects of the received tradition as controlling.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68 at interval end) because the regulatory authority extracts decision-making power under the guise of distinguishing safe from unsafe forms, and the boundary itself is contestable — what constitutes 'strict regulation' is decided by the authority, not by principle. Suppression is correspondingly high (0.72) because strict iconoclasts are foreclosed by the reading's core premise (three-dimensional is permitted to be regulated, not categorically forbidden), and iconodule communities face identity-lock suppression (their theological conviction that full dimensionality is necessary is treated as a failure of orthodoxy, not a legitimate reading). Theater is elevated (0.58) because enforcement increasingly focuses on the approval bureaucracy rather than the theological distinction itself — the apparatus becomes performative: is this image sufficiently 'two-dimensional' to be approved? Does the regulatory review genuinely prevent idolatry or merely maintain gatekeeper authority? The measurements show extraction accumulating over the interval (base_extractiveness rising from 0.52 to 0.68) as the regulatory authority develops more elaborate approval procedures, and theater also rising as those procedures become increasingly formalized and detached from the theological rationale. Suppression stabilizes at the interval end as opposition either becomes identity-locked (iconodules accept the constraint as part of their subordination within the tradition) or is foreclosed (strict iconoclasts exit or join underground movements).
 *
 * PERSPECTIVAL GAP:
 *   From the authority's institutional seat, dimensionality is an objective proxy for idolatry risk: three-dimensional form invites physical veneration (kissing, bowing, anointing), while two-dimensional imagery maintains cognitive distance. From the iconodule seat, the distinction is a false boundary: if the Incarnation sanctifies matter, then the dimensionality of the representation is theologically irrelevant — the authority is using a pseudo-technical distinction to extract power under the guise of safety. From the sculptor seat, both readings are academic luxury; the constraint is straightforwardly calamitous: their livelihood is erased.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from power, exit, and beneficiary/victim roles. The regulatory authority is powerful, has analytical exit (it can revise its interpretation without leaving the tradition), and is the declared beneficiary — d near 0.0. Religious artists are moderate-power with constrained exit (they can relocate or retrain, but at high cost); they are victims — d ~0.6. Iconodule communities are organized with identity-locked exit (their theological conviction fuses them to the tradition even as they are constrained by it); they are victims — d ~0.75. Sculptors are powerless with trapped exit (they cannot escape except by abandoning the tradition or their craft entirely); they are victims — d ~0.95. Strict iconoclasts are organized with constrained exit (they can relocate to communities with stricter prohibitions or go underground; some remain and practice passive resistance) — d ~0.70.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to honor both the prohibition and material mediation — was live when the moderate reading emerged. However, the structural analysis reveals that the compromise was designed by the regulatory authority and benefits it primarily. The founding problem has partly dead-ended: strict iconoclasts deny the moderate reading solves anything (it violates the prohibition by legitimizing images at all); iconodules deny it solves the spiritual problem (regulated two-dimensional access is not equivalent to full three-dimensional mediation). What remains live is the regulatory authority's interest in maintaining the boundary, which now extracts power and compliance costs independent of whether the compromise actually solved the founding problem. The constraint approaches mandatrophy (the original mandate has outlived its function as a genuine compromise, but the apparatus persists to maintain authority) — indexed by the rising theater_ratio and the stabilizing suppression: the constraint is increasingly maintained through gatekeeping apparatus rather than through the theological distinction it claims to implement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dimensionality_as_proxy_legitimacy,
    'Is dimensionality an objective theological marker of idolatry risk, or a proxy the regulatory authority uses to maintain interpretive gatekeeping?',
    'Cross-traditional comparison: examine how other traditions that accept three-dimensional representation handle the idolatry risk (through practice norms, theological education, liturgical context rather than formal prohibition). If those traditions achieve lower idolatry behavioral incidence than the moderate iconoclast jurisdiction, dimensionality is not an effective risk proxy; if incidence is similar or higher, the proxy works but the regulatory authority could not be the only solution.',
    'If dimensionality is a proxy rather than a principle, the constraint is purely extractive (regulatory authority collects power via a pseudo-technical distinction). If it is a principle, part of the measured extraction is the cost of implementing a real theological safeguard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dimensionality_as_proxy_legitimacy, empirical, 'Whether dimensionality functions as an objective theological category or as a tool for regulatory authority.').

omega_variable(
    identity_lock_vs_exit_stability,
    'For iconodule communities, is the constraint maintained by internalized acceptance (they have become convinced three-dimensional representation violates orthodoxy) or by structural trapping (they stay in the tradition despite rejecting the constraint)?',
    'Post-exit suppression trajectory: if communities leave the jurisdiction and continue three-dimensional practice without diminished theological confidence, the suppression was structural; if communities that leave gradually adopt less iconodule devotional forms, suppression was partly internalized and mobility does not fully exit the constraint.',
    'If suppression is partly internalized, the constraint''s effective reach extends beyond formal enforcement and the payer seats'' effective exit improves on relocation. If structural, fixing the constraint requires only changing enforcement, not re-educating adherents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vs_exit_stability, empirical, 'Whether iconodule identity-lock is internalized conviction or structural enforcement that would dissolve on exit.').

omega_variable(
    foreclosure_vs_coexistence_iconoclast,
    'Does the moderate reading genuinely foreclose the strict iconoclast reading, or do the two coexist as live positions within the broader tradition?',
    'Textual and institutional history: trace whether strict iconoclasts are formally declared heretical (foreclosed) or merely minority factions (coexisting). If strict iconoclasts retain institutional representation or textual authority despite disagreement, the relation is coexistence; if they are formally expelled or declared beyond the tradition, the relation is foreclosure.',
    'If coexistence, strict iconoclasts are excluded parties whose suppression is ongoing but non-terminal; if foreclosure, the moderate reading has already won the theological battle and any remaining strict iconoclasts are post-defeat resistance. The suppression requirement would be lower in the foreclosure case (the battle is settled) and higher in the coexistence case (the authority must continuously maintain the boundary against live opposition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence_iconoclast, empirical, 'Whether strict iconoclasm is foreclosed by the moderate reading or coexists as a live tradition.').

omega_variable(
    reading_as_committer_frame_artifact,
    'Is the moderate iconoclast reading a genuine theological position held by real communities, or a scholarly interpolation that has become institutional consensus without deliberate adoption?',
    'Institutional and genealogical history: identify when and by whom the moderate reading became canonical, whether it was explicitly adopted by councils or communities, and whether it was ever put to deliberative choice or was installed incrementally by institutional practice.',
    'If genuinely adopted, the reading has committer legitimacy within the tradition. If installed as institutional consensus without deliberate choice, it is a Piton constraint (maintained by inertia) rather than a Snare (maintained by active enforcement for the authority''s benefit) — the distinction affects whether mandatrophy is resolvable through policy change or requires cultural shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_committer_frame_artifact, conceptual, 'Whether the moderate reading is an authentic theological position or an institutional artifact installed by governance rather than doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(deca_tr_t8, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement(deca_tr_t16, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 16, 0.54).
narrative_ontology:measurement(deca_tr_t24, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 24, 0.58).
narrative_ontology:measurement(deca_tr_t32, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 32, 0.59).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(deca_be_t8, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(deca_be_t16, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(deca_be_t24, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(deca_be_t32, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(deca_su_t8, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(deca_su_t16, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(deca_su_t24, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(deca_su_t32, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__moderate_iconoclast_reading, 0.12).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconodule_reading).

% DUAL FORMULATION NOTE:
% The decalogue_image_prohibition kernel instantiates three structurally distinct constraints corresponding to three live theological readings. The iconoclast_reading treats all material religious representation as inherently idolatrous (Mountain type: natural theological fact). The iconodule_reading treats three-dimensional representation as theologically legitimate post-Incarnation (Rope type: genuine coordination of material mediation with orthodoxy). The moderate_iconoclast_reading (this file) splits the difference by forbidding three-dimensional statuary while permitting regulated two-dimensional images (Snare type: regulatory gatekeeping under cover of theological compromise). Each instantiates a different epsilon and type; the three are linked via network.affects_constraints to document that they are readings of a single kernel rather than independent constraints. The moderate reading forecloses strict iconoclasm by legitimizing image use and is foreclosed by iconodule theology by failing to distinguish dimensionality as theologically significant. Sibling files carry the other readings; consumers use the network edges to track the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__moderate_iconoclast_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
