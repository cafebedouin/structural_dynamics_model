% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__moderate_iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: decalogue_image_prohibition__moderate_iconoclast_reading
 *   human_readable: Moderate Iconoclast Reading: Statuary Prohibition with Regulated Two-Dimensional Permission
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This story instantiates the moderate iconoclast reading of the Decalogue
 *   image prohibition: the commandment is read as forbidding
 *   three-dimensional statuary specifically (on the theory that free-standing
 *   figures pose a heightened idolatry risk) while permitting two-dimensional
 *   images under a regulatory regime designed to prevent that permission from
 *   sliding into abuse. This is a distinct constraint from the strict
 *   iconoclast reading (which forbids all material religious imagery) and the
 *   iconodule reading (which permits material mediation broadly, grounding
 *   permission in the Incarnation rather than dimensionality). The three
 *   readings are linked as siblings in the same kernel contest; each is
 *   authored as its own ε-invariant constraint per Rule 1, and none is a
 *   measurement of the others.
 *
 * KEY AGENTS:
 *   - ecclesiastical_regulatory_authority: agenda_setter (institutional/arbitrage) — draws and enforces the dimensionality line, collects licensing structure benefit
 *   - licensed_icon_producers: beneficiary (organized/mobile) — protected market position behind compliance barrier
 *   - lay_devotional_practitioners: payer (powerless/constrained) — bears compliance markup and scrutiny
 *   - unlicensed_artisans: payer (powerless/trapped) — trade criminalized by the line
 *   - statuary_traditions_communities: payer (moderate/constrained) — loses devotional heritage entirely
 *   - iconodule_theologians and strict_iconoclasts: excluded — both reject the dimensionality axis from opposite directions
 *   - comparative_theology_historians: observer (analytical) — traces the line's historical rather than doctrinal origin
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.61).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.57).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.57).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Moderate Iconoclast Reading: Statuary Prohibition with Regulated Two-Dimensional Permission").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, '5d8a6475-b9a1-4acd-a510-c63724c28eb9').
narrative_ontology:cs_kernel_codification('5d8a6475-b9a1-4acd-a510-c63724c28eb9', fixed_text).
narrative_ontology:cs_authority_grounding('5d8a6475-b9a1-4acd-a510-c63724c28eb9', extraction).
narrative_ontology:cs_interpretation_layer_present('5d8a6475-b9a1-4acd-a510-c63724c28eb9').
narrative_ontology:cs_reading_relation('5d8a6475-b9a1-4acd-a510-c63724c28eb9', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d8a6475-b9a1-4acd-a510-c63724c28eb9', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('5d8a6475-b9a1-4acd-a510-c63724c28eb9', foundational, dimensionality_determines_idolatry_risk).
narrative_ontology:cs_axiom_status(dimensionality_determines_idolatry_risk, holdable).
narrative_ontology:cs_axiom_grounding('5d8a6475-b9a1-4acd-a510-c63724c28eb9', dimensionality_determines_idolatry_risk, conventional).
narrative_ontology:cs_axiom('5d8a6475-b9a1-4acd-a510-c63724c28eb9', secondary, regulated_permission_prevents_devotional_abuse).
narrative_ontology:cs_axiom_status(regulated_permission_prevents_devotional_abuse, holdable).
narrative_ontology:cs_axiom_grounding('5d8a6475-b9a1-4acd-a510-c63724c28eb9', regulated_permission_prevents_devotional_abuse, instrumental).
narrative_ontology:cs_reference_frame('5d8a6475-b9a1-4acd-a510-c63724c28eb9', graduated_risk_compromise_framework).
narrative_ontology:cs_drift_state('5d8a6475-b9a1-4acd-a510-c63724c28eb9', post_regulatory_hardening_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5d8a6475-b9a1-4acd-a510-c63724c28eb9', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulatory_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_icon_producers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_practitioners).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, unlicensed_artisans).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, statuary_traditions_communities).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, graduated_idolatry_risk_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draws and enforces the line between forbidden three-dimensional statuary and permitted two-dimensional imagery. Issues licenses, inspects workshops, adjudicates borderline cases (bas-relief, deep engraving, votive plaques), and can withdraw permission at will. Its gatekeeping authority over what counts as acceptably 'flat enough' is the seat's primary asset — every ambiguous case that requires adjudication reinforces the necessity of the seat itself.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulatory_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Guild workshops that have obtained certification to produce compliant two-dimensional images. Compliance costs (inspection fees, approved-pigment sourcing, submission of designs for pre-clearance) are real but function as a barrier that keeps out unlicensed competitors, so the regulatory burden doubles as market protection for those already inside it.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_icon_producers, beneficiary,
    organized, biographical, mobile, regional).

% Ordinary worshippers whose devotional practice is now mediated by a permission structure: acceptable icons must come from licensed sources, at licensed prices, meeting licensed specifications. They bear the compliance markup and the anxiety of scrutiny (is this plaque too deep, too rounded, too close to statuary?) without any voice in where the line is drawn.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_practitioners, payer,
    powerless, biographical, constrained, local).

% Craftspeople who worked in relief carving, small statuary, or devotional figurines before the regulation hardened. Their trade is now criminalized or requires licensing they cannot afford or qualify for; their tools and skills, previously legitimate, are stranded on the wrong side of a line administered by an authority they cannot appeal to on equal footing.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, unlicensed_artisans, payer,
    powerless, biographical, trapped, local).

% Communities whose devotional heritage centered on three-dimensional figures (processional statues, household figurines) lose access to their own tradition entirely under this reading, while communities that favored flat imagery lose comparatively little. The distribution of loss tracks pre-existing material culture, not theological principle.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, statuary_traditions_communities, payer,
    moderate, generational, constrained, regional).

% Hold that the Incarnation sanctifies matter as such and that dimensionality is theologically irrelevant to the latria/dulia distinction. They would argue the three-dimensional/two-dimensional line has no scriptural basis and is an administrative convenience dressed as doctrine, but their reading is not the one this constraint enforces and their objections are treated as heterodox rather than adjudicated on the merits.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_theologians, excluded,
    moderate, generational, constrained, regional).

% Hold that any material representation used in worship is idolatrous regardless of dimensionality. They would argue the permitted two-dimensional category is itself already a violation, and that the moderate reading's entire regulatory apparatus exists to protect a compromise they consider indefensible.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, strict_iconoclasts, excluded,
    moderate, generational, constrained, regional).

% Study how the three-dimensional/two-dimensional distinction emerged historically, tracing it to specific councils, controversies, and periods of image-related conflict rather than to a stable exegetical consensus. Note the correlation between the line's location and the material culture the regulating authority already favored.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, comparative_theology_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulatory_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__moderate_iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable, administrable compromise between total prohibition and unrestricted image use — offering communities continued visual/devotional practice while claiming to guard against the specific idolatry risk statuary is held to pose (a free-standing, potentially venerated object versus a flat representation).
% TRANSFER_FUNCTION: Moves gatekeeping authority and compliance revenue toward the regulatory apparatus and licensed producers; moves cost, uncertainty, and lost practice toward lay practitioners, unlicensed artisans, and communities whose traditions were three-dimensional.
% ABSENT_VOICES: Iconodule theologians (who would deny dimensionality is the relevant axis at all) and strict iconoclasts (who would deny the two-dimensional exemption is legitimate) are both excluded from the adjudicating body; the line is set and policed by an authority whose institutional interest is served by there being a line to administer.
% DISAPPEARANCE_RATIONALE: If the dimensionality-based licensing regime vanished, unlicensed artisans would resume statuary production, lay practitioners would lose the compliance markup and scrutiny, licensed producers would lose their protected market position, and the regulatory authority would lose a domain of jurisdiction — devotional material culture would re-diversify along regional and traditional lines rather than the administratively drawn one.
% FOUNDING_PROBLEM: A genuine theological concern that free-standing three-dimensional images invite a qualitatively different (or more intense) form of veneration-that-becomes-worship than flat images do, requiring some principled boundary to prevent devotional practice from sliding into idolatry.
% FOUNDING_PROBLEM_CORROBORATION: The regulatory authority and licensed producers attest the dimensionality line remains theologically necessary and administratively workable. Comparative theology historians, writing from outside the regulatory apparatus, document that the specific two-dimensional/three-dimensional boundary tracks historical controversies and existing material culture more closely than any stable scriptural or patristic consensus, and that both iconodule and strict iconoclast traditions reject the boundary's theological grounding from opposite directions — suggesting the line's persistence is substantially administrative rather than doctrinal.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__moderate_iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored moderate-to-substantial (0.61 at interval end) and rising: the compliance and licensing apparatus around the two-dimensional exemption has hardened over time from an initially loose distinction into a detailed regulatory regime (theater_ratio rising from 0.20 to 0.42 reflects growing performative adjudication of borderline cases — how deep is too deep a relief carving — that serves the authority's jurisdiction more than it serves any stable theological principle). Suppression is authored moderate (0.57) rather than severe, because the regime permits substantial devotional practice (two-dimensional imagery) rather than closing off material mediation altogether — this is what differentiates a snare from a mountain: real coordination happens for those who accept the line, but the line itself is enforced with real coercive apparatus against those on the wrong side of it (unlicensed artisans, statuary communities). Accessibility collapse is authored lower (0.38) because the reading does not claim alternatives are impossible in principle — iconodule and strict iconoclast readings remain live, contested positions, which is precisely why this is a snare riding a genuine theological question rather than a mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory authority's seat, this reading is principled doctrine responsibly enforced — a needed line against a real risk. From the unlicensed artisan's or statuary community's seat, the same arrangement is a coercively enforced boundary whose theological grounding is contested by serious parties on both sides. The engine computes these divergent seat classifications from the structural power/exit data; the claimed_type (snare) reflects the authoring seat's judgment that regulatory gatekeeping dominates the coordination function here.
 *
 * DIRECTIONALITY LOGIC:
 *   The regulatory authority sits at the beneficiary end: it administers the boundary, collects compliance revenue and jurisdiction, and has no structural incentive to resolve the ambiguity it profits from adjudicating. Licensed producers are secondary beneficiaries — their compliance costs double as a market-protecting barrier to entry. Lay practitioners, unlicensed artisans, and statuary-tradition communities sit at the target end: they bear cost, uncertainty, or total loss of practice without proportionate voice in where the line is drawn. The two excluded theological camps (iconodule and strict iconoclast) are structurally interesting: both would object to the dimensionality axis itself, from opposite premises, which suggests the axis is administratively convenient rather than doctrinally compelled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing acceptable devotional mediation from idolatry) may remain theologically live in the abstract, but the specific dimensionality-based solution shows signs of having outlived close theological scrutiny: the regulatory apparatus around it (licensing, inspection, borderline adjudication) has grown even as its doctrinal grounding is contested by both iconodule and strict-iconoclast traditions from opposite directions. This is the mismatch the R5 fields are designed to surface — status is authored as 'contested' precisely because the authority's own account (the line is theologically necessary) is disputed by outside corroboration (historians tracing the line to controversy and existing material culture rather than consensus exegesis).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dimensionality_axis_theological_warrant,
    'Is the three-dimensional/two-dimensional distinction a genuine scriptural or patristic principle, or a historically contingent administrative convenience later dressed in doctrinal language?',
    'Close textual and historical analysis of the councils and controversies where the distinction first hardened into policy, cross-referenced against earlier periods where the distinction was absent or differently drawn.',
    'If the distinction is shown to track available material culture and political control rather than stable exegesis, the constraint''s claimed coordination function (preventing idolatry via a principled boundary) collapses into pure gatekeeping, strengthening the snare classification. If a genuine and stable theological warrant for the dimensionality line is found, the coordination function is stronger than the authored metrics currently assume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dimensionality_axis_theological_warrant, conceptual, 'Whether the dimensionality boundary is doctrinally principled or administratively convenient.').

omega_variable(
    borderline_case_adjudication_capture,
    'Does the regulatory authority''s discretion over borderline cases (deep relief, bas-relief, near-statuary) function as necessary doctrinal judgment or as a mechanism for expanding the authority''s own jurisdiction over time?',
    'Track the direction of borderline rulings over the interval: if ambiguous cases are increasingly resolved toward requiring licensing/inspection rather than toward clarity that reduces future adjudication need, the mechanism is self-expanding.',
    'A self-expanding adjudication pattern would corroborate the theater_ratio trajectory and support the extraction reading; a converging, clarity-producing pattern would suggest genuine, diminishing-need coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(borderline_case_adjudication_capture, empirical, 'Whether discretionary borderline-case rulings expand or diminish the regulatory footprint over time.').

omega_variable(
    cs_framing_kernel_vs_administrative_layer,
    'Is the correct commitment-system framing the Decalogue text itself as kernel with this reading as one interpretation, or is the true operative kernel actually the specific council/synodal rulings that established the dimensionality test — with the Decalogue text serving only as distal legitimating reference?',
    'Compare which document practitioners and regulators actually cite when adjudicating borderline cases: the commandment text itself, or the derivative rulings/canons that operationalize it.',
    'If the operative kernel is the derivative ruling rather than the commandment text, authority_grounding shifts from lineage-to-scripture toward lineage-to-institutional-precedent, and the extraction is more clearly located in the administrative layer rather than in scriptural interpretation itself — strengthening rather than weakening the snare reading, since the derivative layer has no independent scriptural warrant of its own.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_administrative_layer, conceptual, 'Whether the kernel proper is the commandment text or the derivative institutional rulings that operationalize the dimensionality test.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(deca_tr_t8, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(deca_tr_t16, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(deca_tr_t24, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(deca_tr_t32, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(deca_be_t8, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(deca_be_t16, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(deca_be_t24, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(deca_be_t32, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(deca_su_t8, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(deca_su_t16, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(deca_su_t24, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(deca_su_t32, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 32, 0.54).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 40, 0.57).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconodule_reading).

% DUAL FORMULATION NOTE:
% This constraint, decalogue_image_prohibition__iconoclast_reading, and decalogue_image_prohibition__iconodule_reading are three sibling readings of one contested kernel (decalogue_image_prohibition), each authored as its own ε-invariant constraint per the ε-invariance principle. The strict iconoclast reading (ε low-to-negligible extraction, near-total accessibility collapse of material imagery as a category) sits at one pole; the iconodule reading (ε low, broad legitimation of material mediation grounded in the Incarnation, minimal enforcement apparatus) sits at the other; this moderate reading sits between them but is NOT their average — it introduces a distinct extraction mechanism (boundary-policing bureaucracy) that neither pole requires. Each story's beneficiary/victim structure, enforcement requirement, and claimed_type differ accordingly and must not be reconciled to one another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
