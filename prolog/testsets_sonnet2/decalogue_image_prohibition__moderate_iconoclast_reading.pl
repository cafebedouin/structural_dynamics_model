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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: decalogue_image_prohibition__moderate_iconoclast_reading
 *   human_readable: Moderate Iconoclast Reading: Two-Dimensional Permission Under Regulatory Gatekeeping
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This story instantiates the moderate iconoclast reading of the decalogue
 *   image prohibition: the commandment is read as forbidding
 *   three-dimensional statuary specifically (on grounds of elevated idolatry
 *   risk) while permitting two-dimensional images under a detailed regulatory
 *   regime. Unlike the total-prohibition (iconoclast) reading, which
 *   forecloses all material mediation, and the iconodule reading, which
 *   sanctifies material representation broadly through the Incarnation, this
 *   reading splits the difference administratively. That split does not
 *   resolve the underlying theological question — it institutionalizes a
 *   compromise line and then builds licensing, inspection, and sanction
 *   machinery around policing that line. The coordination function (some
 *   sanctioned devotional mediation) is real but thin; the extraction
 *   (licensing fees, compliance costs, exclusion of an entire craft
 *   tradition, sanctions on unlicensed practice) is substantial and grows as
 *   the regulatory apparatus matures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.58).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.62).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Moderate Iconoclast Reading: Two-Dimensional Permission Under Regulatory Gatekeeping").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, '8be9052b-384d-41b0-b05b-2625b2422e6a').
narrative_ontology:cs_kernel_codification('8be9052b-384d-41b0-b05b-2625b2422e6a', fixed_text).
narrative_ontology:cs_authority_grounding('8be9052b-384d-41b0-b05b-2625b2422e6a', lineage).
narrative_ontology:cs_interpretation_layer_present('8be9052b-384d-41b0-b05b-2625b2422e6a').
narrative_ontology:cs_reading_relation('8be9052b-384d-41b0-b05b-2625b2422e6a', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('8be9052b-384d-41b0-b05b-2625b2422e6a', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('8be9052b-384d-41b0-b05b-2625b2422e6a', foundational, dimensionality_tracks_idolatry_risk).
narrative_ontology:cs_axiom_status(dimensionality_tracks_idolatry_risk, holdable).
narrative_ontology:cs_axiom_grounding('8be9052b-384d-41b0-b05b-2625b2422e6a', dimensionality_tracks_idolatry_risk, conventional).
narrative_ontology:cs_axiom('8be9052b-384d-41b0-b05b-2625b2422e6a', secondary, regulated_permission_prevents_abuse_without_total_ban).
narrative_ontology:cs_axiom_status(regulated_permission_prevents_abuse_without_total_ban, holdable).
narrative_ontology:cs_axiom_grounding('8be9052b-384d-41b0-b05b-2625b2422e6a', regulated_permission_prevents_abuse_without_total_ban, instrumental).
narrative_ontology:cs_reference_frame('8be9052b-384d-41b0-b05b-2625b2422e6a', administrable_compromise_doctrine).
narrative_ontology:cs_drift_state('8be9052b-384d-41b0-b05b-2625b2422e6a', contemporary_devotional_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8be9052b-384d-41b0-b05b-2625b2422e6a', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulatory_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_icon_workshops).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, sculptors_and_carvers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_practitioners).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, unlicensed_image_makers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_practitioners).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_icon_workshops).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draws the line between permitted two-dimensional images and forbidden three-dimensional statuary, issues detailed regulations governing composition, materials, and display of permitted images, and licenses which workshops may produce them. Collects fees, exercises approval power over content, and maintains a standing inspection apparatus. Frames this as protecting the faithful from idolatry risk while retaining discretionary control over what counts as safely flat versus dangerously sculptural.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulatory_authority, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulatory_authority, beneficiary).

% Operate within the permitted two-dimensional category and hold licenses granted by the regulatory authority. Benefit from exclusive legal standing to produce devotional images, but must submit designs for approval, pay licensing costs, and accept ongoing inspection. Their advantage over unlicensed makers depends entirely on the gatekeeping regime persisting.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_icon_workshops, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_icon_workshops, payer).

% Their entire craft tradition — three-dimensional devotional statuary — is categorically forbidden regardless of intent, skill, or theological content, on the grounds that dimensionality alone elevates idolatry risk. They cannot practice their trade within the sanctioned religious economy at all; exit means abandoning the craft or working underground at legal and social risk.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, sculptors_and_carvers, payer,
    powerless, biographical, trapped, regional).

% May possess and venerate permitted two-dimensional images, but only those produced through the licensed, regulated channel — increasing cost and reducing local availability. They benefit from having some sanctioned material mediation available (unlike under a total prohibition) but bear the compliance overhead built into the permitted category and risk sanction for possessing unlicensed or three-dimensional images.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_practitioners, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_practitioners, beneficiary).

% Produce devotional images outside the licensing structure — often more affordable or locally rooted than licensed workshop output — and are subject to confiscation, fines, or exclusion from parish life if discovered. Their exit options are minimal: comply with licensing costs they often cannot afford, or operate in violation and absorb the risk.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, unlicensed_image_makers, payer,
    powerless, biographical, trapped, local).

% Hold that the Incarnation sanctifies matter broadly and that the dimensionality distinction (two versus three) has no principled theological basis — dulia through any material form, sculptural or flat, is licit. Their position is not represented in the regulatory framework, which treats the 2D/3D line as settled rather than as one contested resolution among several.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_theologians, excluded,
    moderate, generational, constrained, regional).

% Hold that any material representation of the divine or the sanctified constitutes a violation regardless of dimensionality — the 2D/3D distinction is, in their view, an accommodation to devotional demand rather than a genuine theological boundary. They are excluded from the compromise position that structures this constraint, which treats their total-prohibition reading as too severe to administer.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, strict_iconoclasts, excluded,
    moderate, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulatory_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__moderate_iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the faithful with some sanctioned channel for material devotional mediation while drawing an administrable line (dimensionality) that the regulatory authority can inspect and enforce, avoiding both the total image-ban's suppression of devotional practice and the iconodule position's unregulated material veneration.
% TRANSFER_FUNCTION: Moves licensing fees and compliance costs from would-be image producers and lay practitioners to the regulatory authority and the workshops it licenses; moves craft livelihood away from sculptors entirely; moves risk of sanction onto unlicensed producers and possessors.
% ABSENT_VOICES: Iconodule theologians, who see the 2D/3D line as theologically arbitrary rather than principled, and strict iconoclasts, who see the entire permitted category as an unprincipled concession, are both excluded from the negotiated compromise this reading enforces — the line is administratively convenient, not doctrinally derived from either sibling's own premises.
% DISAPPEARANCE_RATIONALE: If this specific licensing-and-dimensionality regime vanished, sculptors could resume producing devotional statuary without categorical prohibition, unlicensed image makers would lose their exposure to sanction, licensed workshops would lose their exclusive market position, and the regulatory authority would lose a standing gatekeeping and fee-collection function — the devotional image economy would reorganize substantially.
% FOUNDING_PROBLEM: Communities disagreed sharply over how much material mediation of the sacred was permissible without tipping into idolatry; the moderate position sought an administrable compromise that would neither suppress devotional practice entirely nor permit unregulated material veneration, using dimensionality as a proxy line for idolatry risk.
% FOUNDING_PROBLEM_CORROBORATION: The regulatory authority attests the dimensionality line remains a live and necessary safeguard against idolatry. Iconodule theologians attest, from outside the benefiting administrative structure, that the line has no independent theological grounding and that the compromise persists because it preserves institutional gatekeeping power rather than because it resolves the underlying doctrinal question; sculptors and unlicensed makers, who bear the compliance and exclusion costs, corroborate that the practical effect is livelihood suppression rather than idolatry prevention.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__moderate_iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.38 to 0.58 over the interval as the regulatory apparatus around the permitted category thickens — more licensing requirements, more inspection points, more grounds for sanction. Suppression (0.62) reflects that the constraint depends on active enforcement of the dimensionality line and of licensing status, not on voluntary participant preference; sculptors face categorical exclusion regardless of consent. Theater ratio (0.44) is moderate-high: a meaningful share of the regulatory activity around 'preventing abuse' of the permitted 2D category functions as gatekeeping performance rather than genuine idolatry prevention, since the dimensionality proxy itself has no settled theological derivation. Accessibility collapse (0.35) is comparatively low — the moderate reading, unlike a total prohibition, deliberately leaves a permitted channel open, so alternatives to full suppression persist even as that channel is heavily regulated. Resistance (0.48) reflects real, organized pushback from excluded craft communities and from both sibling theological traditions.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesiastical regulatory authority is the structural beneficiary: it sets and enforces the dimensionality line, collects licensing revenue, and retains discretionary gatekeeping power over what counts as safely flat. Licensed workshops benefit from exclusive market access but also pay compliance costs, placing them near-symmetric with a beneficiary tilt. Sculptors and carvers sit at the full-target end — trapped, powerless, categorically excluded regardless of individual conduct. Lay practitioners and unlicensed makers bear diffuse and acute costs respectively, with lay practitioners retaining partial benefit (some sanctioned mediation exists) while unlicensed makers face the sharpest exposure to sanction with no institutional standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fear of idolatry through material representation — has not disappeared, but the specific mechanism this reading uses to address it (the dimensionality line) is contested by both sibling readings as an unprincipled compromise rather than a genuine solution. The persistence of licensing and inspection infrastructure well past any settled theological consensus on why 2D differs categorically from 3D is the mandatrophy signature here: the regulatory apparatus has an independent institutional life (fee collection, gatekeeping authority) that would survive even if the underlying idolatry-risk theory were abandoned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dimensionality_as_theological_proxy,
    'Is the two-dimensional/three-dimensional distinction a genuine theological boundary reducing idolatry risk, or an administratively convenient proxy that neither forecloses nor grounds the underlying dispute between total prohibition and full iconodule veneration?',
    'Examine whether the moderate reading''s own foundational texts derive the dimensionality line from doctrine independent of enforcement convenience, or whether the line appears primarily in administrative/canonical regulation rather than in first-order theological argument.',
    'If the line is doctrinally ungrounded, the constraint''s coordination story (protecting against idolatry) is largely cover for a gatekeeping structure that benefits the regulatory authority and licensed producers; if doctrinally grounded, the regulatory overhead is closer to a genuine, if costly, coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dimensionality_as_theological_proxy, conceptual, 'Whether the 2D/3D line is a principled theological boundary or an administrative proxy.').

omega_variable(
    kernel_reading_contest_location,
    'Where exactly do the three kernel readings (iconoclast, iconodule, moderate_iconoclast) disagree at the structural level — is it about the referent of ''graven image'' (any representation vs. worshipped representation), about the sanctity of matter after the Incarnation, or about the correct proxy for idolatry risk?',
    'Compare the foundational axioms of each reading: the iconoclast reading forecloses on material representation categorically; the iconodule reading forecloses on the latria/dulia distinction being illegitimate; this reading locates the disagreement in a risk-stratification claim (dimensionality) that neither sibling accepts as the relevant variable.',
    'If the disagreement is really about the Incarnation''s sanctifying effect on matter (as the iconodule reading holds), the dimensionality distinction this reading relies on is answering the wrong question entirely, and the regulatory apparatus built on it is extracting compliance costs to police a boundary that doesn''t track any party''s actual theological premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Documents where the three sibling readings actually diverge and what each would change structurally.').

omega_variable(
    regulatory_capture_of_moderate_position,
    'Does the ecclesiastical regulatory authority maintain the moderate position because it best resolves the theological dispute, or because the licensing/inspection apparatus it requires is a source of institutional revenue and control that neither the total-prohibition nor the full-permission position would generate?',
    'Track whether licensing fee revenue and inspection authority have grown independently of measurable idolatry outcomes, and whether the regulatory authority has resisted proposals to simplify or abolish the licensing apparatus even when doctrinal consensus shifts.',
    'Evidence of independent institutional growth in the regulatory apparatus, decoupled from doctrinal outcomes, would support classifying this reading as substantially extractive (snare) rather than a good-faith doctrinal compromise (tangled_rope at most).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_of_moderate_position, empirical, 'Whether the regulatory authority''s investment in the moderate position is doctrinal or institutional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(deca_tr_t8, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(deca_tr_t16, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(deca_tr_t24, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(deca_tr_t32, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(deca_be_t8, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(deca_be_t16, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(deca_be_t24, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(deca_be_t32, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(deca_su_t8, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(deca_su_t16, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(deca_su_t24, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(deca_su_t32, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 32, 0.61).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'decalogue image prohibition' per the ε-invariance principle: iconoclast_reading (total prohibition of material representation), iconodule_reading (Incarnation sanctifies matter; latria/dulia distinction permits veneration through images), and this moderate_iconoclast_reading (dimensionality-based compromise with regulatory gatekeeping). Each reading has its own stable ε, beneficiary/victim structure, and classification — the iconoclast reading would show near-total accessibility collapse with no permitted material channel; the iconodule reading would show low extraction and broad beneficiary structure (no categorical exclusion of any craft); this reading uniquely generates a snare-shaped structure because it creates a regulated permission with active enforcement machinery and a gatekeeping beneficiary, which the other two readings do not require.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
