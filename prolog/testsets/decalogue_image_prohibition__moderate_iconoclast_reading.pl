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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: decalogue_image_prohibition__moderate_iconoclast_reading
 *   human_readable: Moderate Iconoclast Image Prohibition with Dimensional Gatekeeping
 *   domain: theological/religious_authority
 *
 * SUMMARY:
 *   The moderate iconoclast reading draws a line through the material world:
 *   three-dimensional sculptural forms are categorically forbidden as
 *   carrying inherent idolatry risk; two-dimensional images (icons, frescoes,
 *   reliefs) are permitted under strict regulatory supervision. This reading
 *   emerged as a middle ground between the absolute iconoclast position (all
 *   images are forbidden) and the iconodule position (the Incarnation
 *   sanctifies all matter). The moderate reading claims to split the
 *   difference but operationally functions as a snare: it permits limited
 *   material mediation while imposing detailed regulatory gatekeeping,
 *   extracting compliance costs and authority power from the payers (artists,
 *   congregations, lay practitioners) to the beneficiary (the ecclesiastical
 *   authority maintaining the boundary). The constraint is CLAIMED as this
 *   theological reading; the metrics describe substantially extractive,
 *   actively enforced operation — theater_ratio rising to 0.58 by interval
 *   end indicates performative maintenance of the boundary increasingly
 *   outweighs functional idolatry prevention.
 *
 * KEY AGENTS:
 *   - regulatory_ecclesiastical_authority: Institutional agenda-setter; sets and enforces the dimensional boundary; collects gatekeeping power and approval revenue
 *   - visual_artists_sacred_context: Moderate-power payers; barred from sculpture entirely; icon painters constrained by approval processes; exit means loss of patron base and professional identity
 *   - local_congregations: Organized payers with secondary beneficiary role; want visual mediation but bounded by dimensionality; bear costs of restricted architectural options and unpredictable enforcement
 *   - lay_devotional_practitioners: Powerless identity-locked payers; embodied devotional practice fused with religious identity; frustrated by image restrictions; internalize prohibition as spiritual discipline
 *   - iconoclast_faction: Excluded powerful actor; argues the boundary is insufficiently restrictive; outside the regulatory conversation but exerting pressure
 *   - iconodule_faction: Excluded powerful actor; argues the boundary is unnecessarily restrictive; outside the regulatory conversation but exerting pressure
 *   - theological_interpretive_authority: Institutional observer; validates or delegitimizes the moderate reading through scholarship
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
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Moderate Iconoclast Image Prohibition with Dimensional Gatekeeping").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theological/religious_authority").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, '26becc2f-b0b8-486d-867c-fa8289a8c130').
narrative_ontology:cs_kernel_codification('26becc2f-b0b8-486d-867c-fa8289a8c130', fixed_text).
narrative_ontology:cs_authority_grounding('26becc2f-b0b8-486d-867c-fa8289a8c130', lineage).
narrative_ontology:cs_interpretation_layer_present('26becc2f-b0b8-486d-867c-fa8289a8c130').
narrative_ontology:cs_reading_relation('26becc2f-b0b8-486d-867c-fa8289a8c130', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('26becc2f-b0b8-486d-867c-fa8289a8c130', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('26becc2f-b0b8-486d-867c-fa8289a8c130', foundational, dimensionality_correlates_with_idolatry_risk).
narrative_ontology:cs_axiom_status(dimensionality_correlates_with_idolatry_risk, holdable).
narrative_ontology:cs_axiom_grounding('26becc2f-b0b8-486d-867c-fa8289a8c130', dimensionality_correlates_with_idolatry_risk, empirically_contingent).
narrative_ontology:cs_axiom('26becc2f-b0b8-486d-867c-fa8289a8c130', secondary, regulated_two_dimensional_images_safe_under_oversight).
narrative_ontology:cs_axiom_status(regulated_two_dimensional_images_safe_under_oversight, holdable).
narrative_ontology:cs_axiom_grounding('26becc2f-b0b8-486d-867c-fa8289a8c130', regulated_two_dimensional_images_safe_under_oversight, instrumental).
narrative_ontology:cs_reference_frame('26becc2f-b0b8-486d-867c-fa8289a8c130', dimensional_media_categorization).
narrative_ontology:cs_drift_state('26becc2f-b0b8-486d-867c-fa8289a8c130', contemporary_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('26becc2f-b0b8-486d-867c-fa8289a8c130', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, regulatory_ecclesiastical_authority).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, visual_artists_sacred_context).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, local_congregations).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, local_congregations).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, idolatry_risk_scalar_to_dimensionality).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, matter_inherently_corrupting_in_worship).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets, enforces, and adjudicates the dimensional boundary. Determines which two-dimensional images are theologically safe; punishes unauthorized three-dimensional work through confiscation and excommunication; conducts approval reviews for icon painters and relief carvers. Maintains gatekeeping power by keeping the boundary contested and under continuous review. Justifies enforcement as protecting the faithful from idolatry while exercising discretionary authority over artists and congregations.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, regulatory_ecclesiastical_authority, agenda_setter,
    institutional, civilizational, analytical, global).

% Create religious imagery across all media. Sculptors are structurally barred; icon painters and relief carvers must seek approval for each work. The approval process is opaque, reversal is possible, and punishment for violation ranges from work destruction to professional and spiritual censure. Exit from sacred art means loss of their primary patron base (the church) and their professional identity. Many artists develop alternative markets (secular commissions, clandestine religious work, private devotional pieces), but at reduced income and social standing.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, visual_artists_sacred_context, payer,
    moderate, biographical, constrained, regional).

% Seek visual mediation for corporate worship and community identity. The prohibition restricts their architectural and devotional choices: permitted two-dimensional images (icon screens, frescoes, relief panels) lack the embodied presence many seek; forbidden three-dimensional forms (statuary, monumental sculptural works) are forbidden. Some congregations secretly maintain hidden statuary or commission work in violation, risking enforcement. They benefit from the coordination of idolatry-prevention orthodoxy but bear costs of limited material expression and enforcement unpredictability.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, local_congregations, payer,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__moderate_iconoclast_reading, local_congregations, beneficiary).

% Seek embodied, tactile engagement with the sacred through three-dimensional icons, reliquaries, and statuary in personal prayer and domestic devotion. The prohibition isolates them: permitted two-dimensional images lack the physical presence they experience as necessary for devotional intensity; forbidden forms carry severe punishment risk (spiritual and material). Their devotional identity is deeply fused with material practice (touching icons, circumambulating statues, kissing relics); deviation feels like apostasy. Many internalize the prohibition as a spiritual discipline (mortification through image denial) while privately grieving the loss of embodied practice. Exit from the faith community due to image prohibition alone is rare; instead, practitioners endure suppression as a cost of belonging.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Holds the reading that ALL religious imagery (two-dimensional and three-dimensional alike) violates the commandment and constitutes idolatry. They argue that the moderate reading is a compromise that preserves the idolatrous practice under a thinner theological disguise: two-dimensional matter remains matter, remains an object of devotion, remains forbidden. They are structurally excluded from the regulatory machinery; their arguments are labeled heretical. They pressure the authority from outside theological communities and clandestine networks but have no official hearing in the rule-making process. Their exclusion is engineered by the constraint itself—only the moderate reading has a seat at the enforcement table.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconoclast_faction, excluded,
    powerful, generational, trapped, global).

% Holds the reading that the Incarnation (God becoming matter) sanctifies all material form; images are not idolatrous but legitimate channels of veneration toward their prototypes (dulia—honor to the image conducts honor to its prototype). They argue that the moderate reading unnecessarily restricts matter by dimensional category; all images used with proper theological intent are safe. They are structurally excluded from regulatory authority; their calls for full image permission are treated as theologically deviant. They contest the constraint from outside (theological writing, alternative liturgical communities, lay advocacy) but have no voice in the rule-making process. Their exclusion is engineered by the constraint itself—only the moderate reading has a seat at the enforcement table.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_faction, excluded,
    powerful, generational, trapped, global).

% Studies the kernel (the commandment text and its theological context) and evaluates the three competing readings. Produces scholarly commentary on which reading most faithfully interprets the source text, most coherently integrates with other doctrinal commitments, and most effectively prevents idolatry. Positioned as neutral scholarship; de facto, their interpretive authority can legitimize or delegitimize the moderate reading and its dimensional distinction. They influence but do not set regulatory policy.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, theological_interpretive_authority, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__moderate_iconoclast_reading, regulatory_ecclesiastical_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__moderate_iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified categorical boundary against idolatry risk by sorting material media: three-dimensional sculptural forms are categorically forbidden (assumed to carry inherent idolatry risk due to embodied presence); two-dimensional images (painted icons, frescoes, relief carving) are conditionally permitted under regulatory supervision (assumed to carry lower risk if monitored). This sorting provides consistent rule-based guidance that the faithful can internalize and apply; it prevents individual devotional choices from devolving into uncontrolled material practice and gives the authority a principled rationale for enforcement.
% TRANSFER_FUNCTION: Moves regulatory authority and gatekeeping power from the faithful (who would choose material forms freely) to the ecclesiastical authority (who adjudicates which images are theologically safe). Artists transfer autonomy over their creative work; they must seek approval and accept constraints on sculptural expression. Congregations transfer architectural freedom; they cannot commission statuary or large sculptural installations. Lay practitioners transfer embodied devotional options; they internalize image restriction as a spiritual discipline. All these transfers flow to the regulatory seat in the form of enhanced authority, discretionary gatekeeping power, and demonstrated theological leadership.
% ABSENT_VOICES: The iconoclast faction (who would argue the boundary should expand leftward—all images forbidden) and the iconodule faction (who would argue it should expand rightward—all images permitted virtuously) are structurally excluded from the regulatory conversation. Artists whose traditional practice centers on three-dimensional sculpture have no official hearing in the rule-making process. Lay practitioners with embodied devotional traditions are not consulted on whether the prohibition serves their spiritual needs. Congregations seeking architectural expressiveness through monumental form are absent from the enforcement table. Their exclusion is not accidental; it is engineered by the rule itself—only the moderate reading has institutional access to the decision-making machinery.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, sculptural statuary would return to sacred spaces within weeks; artists would resume their full craft portfolio; congregations would commission monumental works; lay practitioners would restore hidden three-dimensional pieces to altars and domestic shrines. The regulatory authority's gatekeeping power would collapse. The material culture of worship would reorganize around embodied presence and the visual aesthetics of dimension, rather than around abstract dimensional categories. The theological landscape would shift: the iconoclast and iconodule factions would no longer be excluded; their readings would become live options in the theological conversation. The constraint's disappearance would rearrange both the material and intellectual world of worship.
% FOUNDING_PROBLEM: Early worship communities experienced a persistent cognitive risk: the devotional intensity directed toward a physical image could collapse into worship of the image itself (latria of the image rather than dulia toward its prototype). Sculptural forms, in particular, were observed to intensify emotional attachment and to blur the boundary between honoring what the image represents and worshiping the physical object. A boundary-setting rule was needed to prevent this cognitive and spiritual collapse and to maintain the distinction between image and prototype in the minds and practices of the faithful.
% FOUNDING_PROBLEM_CORROBORATION: The regulatory ecclesiastical authority and moderate theologians attest the founding problem remains live and that the dimensional boundary has proven effective at preventing cognitive collapse. Iconoclasts attest the founding problem persists precisely because two-dimensional images are insufficiently restricted—the distinction by dimension is arbitrary and leaves idolatry risk unaddressed. Iconodules attest the founding problem is misdiagnosed: the actual risk is not inherent to matter but arises from misdirected intention, and properly catechized communities show robust devotional integrity across all media. Independent observers (cognitive scientists studying embodied devotion, art historians documenting worship practices, religious studies scholars) note that the founding problem's persistence is unproven; ethnographic evidence shows many congregations maintain consistent devotional theology across material media without the dimensional restriction, while others demonstrate idolatry risk with two-dimensional images alone. No sources outside the regulatory authority and its theological beneficiaries affirm the dimensional boundary as the crucial mechanism preventing idolatry.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__moderate_iconoclast_reading, 'none', 1).

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
 *   Extraction is moderate-high (0.68 at interval end) because the regulatory authority's gatekeeping power is substantial and the approval process extracts compliance labor from artists and congregations. Suppression is also high (0.72) because the rule persists through active enforcement (punishing unauthorized three-dimensional work, auditing two-dimensional images, confiscating violations) rather than voluntary adoption. Theater is the most revealing metric: it rises from 0.42 to 0.58, indicating that enforcement increasingly involves performative demonstration of orthodoxy (public destruction of forbidden images, elaborate approval ceremonies) rather than functional idolatry prevention. The measurement grid is shared across all three metrics at every time point. Accessibility collapse is low (0.45) because alternatives remain available outside the regulatory frame (hidden statuary, rival readings, unmonitored communities); resistance is high (0.71) because multiple factions actively contest the boundary. The constraint's classification as snare (not tangled_rope or rope) is supported by the victim set, the gatekeeping extraction, and the lack of genuine coordination benefit—lay practitioners do not spontaneously prefer the dimensional boundary; it is imposed and enforced.
 *
 * PERSPECTIVAL GAP:
 *   The regulatory authority experiences this constraint as legitimate theological leadership, maintaining orthodox boundaries against idolatry. Artists and congregations experience it as bureaucratic gatekeeping that restricts their material autonomy without commensurate spiritual benefit. The lay practitioners experience it as identity mortification, a spiritual cost extracted in the name of salvation. The engine computes these divergent d values from the structural data (the authority has near-zero exit cost and directionality toward benefit; the payers have high exit costs and directionality toward extraction). The author claims snare type because the gatekeeping structure is not genuinely coordinative—it does not solve a collective-action problem the payers face; it solves a problem the authority faces (maintaining theological authority) by extracting compliance from those who would otherwise practice differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory authority sits at full beneficiary (d ≈ 0.0): institutional power, no exit cost, gatekeeping revenue, discretionary authority. Visual artists sit at high target (d ≈ 0.8): moderate power, constrained exit (patron base loss), direct extraction of autonomy. Local congregations sit at asymmetric (d ≈ 0.65): organized power but constrained by the authority's superior institutional position, dual benefits (idolatry prevention coordination) and costs (restricted architecture, unpredictable enforcement). Lay practitioners sit at high target (d ≈ 0.88): powerless, identity-locked (apostasy cost of exit), embodied devotional practice directly forbidden—the internalizado of suppression is documented in an omega below. Iconoclast and iconodule factions sit at trapped excluded (d ≈ 1.0 in principle but they are not within the system so effective d is undefined): their exit is non-participation, which they already exercise; the constraint operates despite their objections because they are excluded from the regulatory machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early worship risk of idolatry, material confusion) was live at the constraint's origin. At interval end, the founding problem status is contested: the authority attests the problem remains (hidden statuary discovered, enforcement necessary), but independent observers note that many congregations show robust devotional integrity across media types without the restriction. Theater rising from 0.42 to 0.58 is the key signal: the constraint is increasingly maintained through performative enforcement (public condemnation, destruction ceremonies, elaborate approval protocols) rather than functional prevention. This trajectory is consistent with mandatrophy (founding function atrophied, constraint persists by theatrical enforcement). The constraint has not been formally abandoned, but it is sustained by the authority's interest in gatekeeping and theatrical orthodoxy-demonstration rather than by live idolatry prevention. A genuinely live coordination function would show stable or declining theater ratio; rising theater indicates the function is dying and the form is being kept animated by performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dimensional_idolatry_risk_asymmetry,
    'Is three-dimensional sculptural form inherently more conducive to idolatrous devotion than two-dimensional imagery, or is this distinction a constructed theological preference?',
    'Cognitive science studies of embodied devotional practice comparing idolatry risk across material media (sculpture vs. icon vs. relic vs. written word); ethnographic evidence from communities practicing with both media; theological historiography tracing how the dimensional boundary became canonical.',
    'If three-dimensional form does carry inherent idolatry risk, the constraint''s categorical boundary is justified and extraction is the cost of genuine coordination. If the distinction is constructed (embodied presence vs. visual representation are both equally subject to devotional intensity), the dimensional gatekeeping is pure extraction dressed in theological language.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dimensional_idolatry_risk_asymmetry, empirical, 'Whether dimensionality correlates with idolatry risk or is a theological distinction without experiential basis.').

omega_variable(
    suppression_mechanism_internalized_structural,
    'Do lay practitioners constrain their devotional material practice because they accept the theological argument against three-dimensional forms, or because they fear punishment and have internalized the authority''s claim that violation is sinful?',
    'Post-prohibition ethnography: when restrictions are lifted or enforcement lapses, do practitioners spontaneously resume three-dimensional practice (indicating internalization) or return cautiously and with discomfort (indicating structural suppression that persists in internalized form)? Do practitioners separated from the authority context report changes in felt idolatry risk?',
    'If suppression is primarily internalized (devotional identity fused with prohibition), exit after enforcement removal would be difficult; the constraint carries its enforcement mechanism within the target''s self-concept. If primarily structural (fear of authority), post-removal behavior would shift rapidly. This distinction affects whether reclassification to piton (inertial constraint) applies—internalized suppression keeps pitons animated longer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_structural, empirical, 'Structural vs. internalized suppression in identity-locked lay practitioners.').

omega_variable(
    regulatory_authority_gateway_or_genuine_coordination,
    'Does the regulatory approval process for two-dimensional images serve a genuine coordination function (collectively establishing safe boundaries against idolatry), or does it exist primarily to maintain the authority''s gatekeeping power?',
    'Administrative data: approval rates by category, average review time, rejection rationale; evidence of actual idolatry incidents in approved vs. unapproved contexts; comparison to regions where approval processes are less burdensome; testimony from approved artists on whether the review changed their practice or merely delayed it.',
    'If approval genuinely prevents idolatry (approval-process correlates with lower idolatry risk), the constraint is closer to tangled_rope (coordination + extraction). If approval rates are high and rejection rare (or rejection rationales are opaque), the process functions as gatekeeping theater, supporting snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_authority_gateway_or_genuine_coordination, empirical, 'Whether the approval gatekeeping serves functional idolatry prevention or performative authority-maintenance.').

omega_variable(
    iconoclast_iconodule_foreclosure_structure,
    'Does the moderate reading logically foreclose the iconoclast and iconodule readings, or do all three coexist as live theological options held by different factions?',
    'Theological historiography: can a coherent theological framework hold all three readings simultaneously, or does adopting the moderate reading commit one to rejecting key premises of the others? Are the three readings held by different communities (coexist) or does the moderate reading claim to be the only correct reading (forecloses)?',
    'If the moderate reading forecloses the siblings, the three readings are not truly distinct constraints but are a single contested constraint with one winning reading. If they coexist, they are three genuinely independent constraints in a contested landscape. This affects the network.affects_constraints structure and the kernel decomposition strategy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(iconoclast_iconodule_foreclosure_structure, conceptual, 'Whether the moderate reading forecloses or coexists with its sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(deca_tr_t0, observed).
narrative_ontology:measurement(deca_tr_t5, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 5, 0.46).
narrative_ontology:measurement_basis(deca_tr_t5, observed).
narrative_ontology:measurement(deca_tr_t10, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement_basis(deca_tr_t10, observed).
narrative_ontology:measurement(deca_tr_t15, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 15, 0.53).
narrative_ontology:measurement_basis(deca_tr_t15, observed).
narrative_ontology:measurement(deca_tr_t25, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 25, 0.57).
narrative_ontology:measurement_basis(deca_tr_t25, observed).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(deca_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(deca_be_t0, observed).
narrative_ontology:measurement(deca_be_t5, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(deca_be_t5, observed).
narrative_ontology:measurement(deca_be_t10, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(deca_be_t10, observed).
narrative_ontology:measurement(deca_be_t15, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(deca_be_t15, observed).
narrative_ontology:measurement(deca_be_t25, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(deca_be_t25, observed).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(deca_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(deca_su_t0, observed).
narrative_ontology:measurement(deca_su_t5, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(deca_su_t5, observed).
narrative_ontology:measurement(deca_su_t10, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(deca_su_t10, observed).
narrative_ontology:measurement(deca_su_t15, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(deca_su_t15, observed).
narrative_ontology:measurement(deca_su_t25, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(deca_su_t25, observed).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(deca_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__moderate_iconoclast_reading, 0.12).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconodule_reading).

% DUAL FORMULATION NOTE:
% The moderate_iconoclast_reading is one of three structurally distinct instantiations of the contested kernel decalogue_image_prohibition. The three readings (iconoclast, iconodule, moderate_iconoclast) decompose because their ε values differ substantially: the strict iconoclast reading emits low extraction (simpler boundary, less gatekeeping); the iconodule reading emits no extraction (the constraint dissolves); the moderate reading emits moderate-high extraction via dimensional gatekeeping. The moderate reading uniquely creates a bifurcated material world (permitted/forbidden by dimension) that grants the regulatory authority gatekeeping power over the permissible category. Each reading is compiled to a separate constraint story with its own beneficiaries, victims, and classification. The three stories are linked by network.affects_constraints to indicate they are siblings within a constraint family, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__moderate_iconoclast_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
