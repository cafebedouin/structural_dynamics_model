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
 *   human_readable: Moderate Iconoclast Reading of the Image Prohibition (2D Permitted Under Regulation, 3D Forbidden)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This story instantiates the moderate iconoclast reading of the
 *   Decalogue's image prohibition: three-dimensional statuary is
 *   categorically forbidden as the highest-risk form of idolatry, while
 *   two-dimensional images are permitted but only under a detailed licensing
 *   and inspection regime. This reading is structurally distinct from the
 *   strict iconoclast reading (which forbids all religious imagery, full
 *   stop) and the iconodule reading (which permits both dimensions on the
 *   theological ground that the Incarnation sanctifies matter). The
 *   dimensional line this reading draws is not present in either sibling
 *   reading — it is this reading's own invention, and it is precisely the
 *   invention that generates the administrative apparatus (licensing
 *   authority, inspection regime, certification fees) that the other two
 *   readings do not require. That apparatus is what makes this reading, and
 *   only this reading, the snare-shaped one among the three.
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
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Moderate Iconoclast Reading of the Image Prohibition (2D Permitted Under Regulation, 3D Forbidden)").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, '828a11a0-9251-401d-a004-2e88faa1e8dc').
narrative_ontology:cs_kernel_codification('828a11a0-9251-401d-a004-2e88faa1e8dc', fixed_text).
narrative_ontology:cs_authority_grounding('828a11a0-9251-401d-a004-2e88faa1e8dc', extraction).
narrative_ontology:cs_interpretation_layer_present('828a11a0-9251-401d-a004-2e88faa1e8dc').
narrative_ontology:cs_reading_relation('828a11a0-9251-401d-a004-2e88faa1e8dc', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('828a11a0-9251-401d-a004-2e88faa1e8dc', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('828a11a0-9251-401d-a004-2e88faa1e8dc', foundational, dimensionality_tracks_idolatry_risk).
narrative_ontology:cs_axiom_status(dimensionality_tracks_idolatry_risk, holdable).
narrative_ontology:cs_axiom_grounding('828a11a0-9251-401d-a004-2e88faa1e8dc', dimensionality_tracks_idolatry_risk, empirically_contingent).
narrative_ontology:cs_axiom('828a11a0-9251-401d-a004-2e88faa1e8dc', secondary, regulated_mediation_permissible_below_risk_threshold).
narrative_ontology:cs_axiom_status(regulated_mediation_permissible_below_risk_threshold, holdable).
narrative_ontology:cs_axiom_grounding('828a11a0-9251-401d-a004-2e88faa1e8dc', regulated_mediation_permissible_below_risk_threshold, instrumental).
narrative_ontology:cs_reference_frame('828a11a0-9251-401d-a004-2e88faa1e8dc', graduated_risk_licensing_framework).
narrative_ontology:cs_drift_state('828a11a0-9251-401d-a004-2e88faa1e8dc', post_licensing_bureaucratization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('828a11a0-9251-401d-a004-2e88faa1e8dc', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulatory_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_iconographers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, sculptors_and_carvers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_practitioners).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, unlicensed_image_makers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_iconographers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draws and polices the line between permissible two-dimensional representation and forbidden three-dimensional statuary, issues licenses for approved iconography, inspects workshops and churches for compliance, and adjudicates disputed cases. Justifies the line as protecting against idolatry while it accrues the power to decide, case by case, what counts as devotion versus abuse. Collects fees, tithes, and deference for administering the distinction it alone is positioned to interpret.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulatory_authority, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulatory_authority, beneficiary).

% Trained and certified to produce two-dimensional devotional images within the permitted regulatory frame. Gain guild-like protection and steady commissions from the exclusivity of licensure, but must submit each new work for approval, pay inspection and certification costs, and risk sanction if a commissioned piece is judged to cross into forbidden dimensionality or style.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_iconographers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_iconographers, payer).

% Practice a craft categorically forbidden under this reading regardless of subject matter or intent. Cannot obtain licensure for statuary at all; their trade is foreclosed within the religious market and they must either abandon three-dimensional devotional work entirely or relocate their craft to secular or export markets where enforcement does not reach.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, sculptors_and_carvers, payer,
    moderate, biographical, constrained, regional).

% Want durable, tangible devotional objects for household or communal worship. Must rely on regulator-approved two-dimensional images, submit to inspection of home shrines in stricter jurisdictions, and pay licensing-inflated prices for compliant images; any homemade or informally carved figure risks confiscation or accusations of idolatrous practice.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_practitioners, payer,
    powerless, biographical, trapped, local).

% Produce devotional images or objects outside the licensing system, often out of poverty, remoteness, or disagreement with the regulatory framework. Have no standing to contest classification of their work and face confiscation, fines, or social sanction; their view that the two-dimensional/three-dimensional line is arbitrary bureaucratic overreach is not solicited by the authority that draws it.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, unlicensed_image_makers, excluded,
    powerless, biographical, trapped, local).

% Holds that ANY material representation used in worship is idolatrous, full stop, and regards the two-dimensional carve-out as a compromise that legitimizes exactly the abuse the commandment forbids. Petitions for stricter enforcement or full abolition of the permitted category but operates from outside the regulatory apparatus this reading empowers.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconoclast_faction, excluded,
    organized, generational, constrained, regional).

% Holds that the Incarnation sanctifies matter broadly and that both two- and three-dimensional images can licitly convey honor to their prototypes without constituting idolatry. Regards the dimensional distinction as theologically arbitrary — a bureaucratic line with no doctrinal grounding — and is excluded from setting the regulatory terms this reading enforces.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_faction, excluded,
    organized, generational, constrained, regional).

% Study how the dimensional line was drawn, when licensing regimes emerged, and whose interests the distinction has served across periods and regions. Document the drift between the doctrinal rationale and administrative practice without participating in either.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulatory_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__moderate_iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable middle path that lets devotional visual culture continue (satisfying the pastoral need for imagery in worship and instruction) while maintaining a bright, administratively enforceable line meant to prevent the highest-risk form of idolatry (freestanding cult statuary) — solving the genuine coordination problem of how a community sustains devotional practice without descending into full image-worship.
% TRANSFER_FUNCTION: Moves control over which devotional objects are licit from individual conscience and local practice to a centralized licensing and inspection apparatus; moves fees, compliance costs, and market access from independent image-makers and lay households to the regulatory authority and its licensed producers.
% ABSENT_VOICES: The iconoclast faction (who would forbid all images and see the two-dimensional carve-out as itself idolatrous) and the iconodule faction (who would permit both dimensions and see the line as theologically arbitrary) are both excluded from setting the regulatory terms; unlicensed image makers who cannot afford or access licensure have no forum to contest the classification of their work.
% DISAPPEARANCE_RATIONALE: If the licensing and inspection regime vanished overnight, sculptors and carvers currently foreclosed from religious commissions would re-enter the devotional market, lay households would resume producing or acquiring images without regard to dimensionality, and the regulatory authority would lose both its gatekeeping function and the fees and deference that function generates — the devotional economy would reorganize around whichever doctrinal or communal norms filled the vacuum.
% FOUNDING_PROBLEM: Communities needed devotional imagery for worship, instruction, and memory, but freestanding statuary was judged to carry acute risk of slipping into cult-object status and outright idolatry; a workable line was needed to permit some material mediation while blocking the form judged most dangerous.
% FOUNDING_PROBLEM_CORROBORATION: The regulatory authority attests the dimensional risk distinction remains theologically and pastorally necessary. Religious historians attest that the line has, in documented periods, tracked licensing revenue and guild protection at least as much as demonstrated idolatry risk, and that comparable communities sustain devotional practice under both stricter (iconoclast) and looser (iconodule) regimes without the administrative apparatus this reading requires — corroboration from outside the beneficiary set exists but is contested by the authority itself.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate-high and rising over the interval (0.34 to 0.58) because the compliance costs of the licensing and inspection regime accumulate as the regulatory apparatus matures — what begins as doctrinal line-drawing hardens into fee structures, guild protections for licensed iconographers, and confiscation practices against unlicensed work. Suppression tracks similarly (0.40 to 0.62) as enforcement capacity (inspectors, sanctions, confiscation authority) builds out. Theater ratio rises more slowly (0.22 to 0.44) reflecting that a genuine doctrinal concern (statuary's idolatry risk) underlies the regime even as an increasing share of enforcement activity defends licensing revenue and guild exclusivity rather than adjudicating actual idolatrous practice. Accessibility collapse is moderate (0.40) — the two-dimensional path remains genuinely open, unlike a pure snare where all alternatives close — while resistance is substantial (0.55) reflecting active contestation from both excluded factions and from unlicensed practitioners.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory authority's seat, this is principled doctrinal risk management: statuary really is more dangerous, and the compliance regime is the necessary cost of allowing any material mediation at all. From the sculptor's seat, the same structure is a categorical trade prohibition with no available redress. From the lay practitioner's seat it is an inflated price for devotional objects and a risk of confiscation for informal ones. The engine computes these as different per-seat classifications from the same structural data — the claim (snare) and the metrics are authored independently and the divergence across seats is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The regulatory authority is the clearest structural beneficiary: it both sets and enforces the dimensional line and collects the deference, fees, and gatekeeping power that flow from being the sole interpreter of a distinction with no self-evident boundary. Licensed iconographers are secondary beneficiaries who also pay compliance costs — a genuinely dual-positioned seat. Sculptors and carvers are full targets: their trade is foreclosed outright, not merely regulated, so their directionality sits at the target extreme regardless of exit options, because there is no compliant path available to them at all. Lay practitioners and unlicensed image makers are targets with the least power and the least exit — trapped by locality and poverty respectively.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (freestanding cult statuary posing acute idolatry risk) may well have been live at the regime's founding, but the six-questions interview surfaces genuine contest over whether it remains live today or whether the apparatus has outrun its founding justification. The mismatch signal here is real: founding_problem_status is authored as contested, and disappearance_verdict is world_rearranges — the combination that the corpus's mismatch consumer is built to flag, since an authority that both claims ongoing necessity and would visibly lose fee income and gatekeeping power if the regime vanished is exactly the profile a zombie-mandate detector should surface for review, not a profile this story pre-adjudicates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dimensional_line_theological_grounding,
    'Is the two-dimensional/three-dimensional distinction a genuine theological discovery about differential idolatry risk, or an administratively convenient line with no doctrinal necessity — i.e., could the same pastoral goal (permitting devotional imagery while preventing idol-worship) be achieved without any dimensional criterion at all?',
    'Comparative doctrinal history: examine whether communities operating under the iconodule reading (which permits both dimensions) show measurably higher rates of idolatrous practice than communities under this reading, controlling for other factors. Absence of a measurable difference would suggest the dimensional line is administratively rather than theologically load-bearing.',
    'If the line is administratively convenient rather than theologically necessary, the classification shifts further toward pure snare (the coordination story is cover for the licensing authority''s gatekeeping); if genuinely theologically grounded, more of the measured extraction should be read as the legitimate cost of a real coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dimensional_line_theological_grounding, conceptual, 'Whether the dimensional criterion tracks genuine risk or serves administrative convenience.').

omega_variable(
    licensing_regime_capture_trajectory,
    'Has the licensing and inspection apparatus, over time, been captured by the interests of licensed iconographers and the regulatory authority itself, independent of whatever idolatry-prevention function it originally served?',
    'Track fee structures, licensing approval rates, and sanction patterns over the interval against independent measures of actual idolatrous practice (household shrine surveys, confiscation case records) to see whether enforcement intensity correlates with revenue capture rather than risk indicators.',
    'Evidence of capture would corroborate the rising extraction and theater_ratio trajectories as genuine rent-seeking rather than proportionate doctrinal enforcement, strengthening the snare classification at the regulator seat specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_regime_capture_trajectory, empirical, 'Whether the regulatory apparatus has drifted from risk-prevention toward rent capture.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does this reading''s dimensional carve-out logically foreclose the strict iconoclast reading within a single ecclesiastical framework, or can a single tradition hold both readings as live positions across different eras or factions?',
    'Examine whether any single ecclesiastical body has historically held both ''all images forbidden'' and ''two-dimensional images permitted under regulation'' as simultaneously operative rules, versus treating them as successive or competing positions.',
    'If genuinely irreconcilable within one framework, the reading_relations edge to iconoclast_reading should be forecloses rather than coexists_with; the current authoring treats them as coexisting factional positions because both are documented as live within overlapping religious communities historically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether the moderate and strict iconoclast positions can coexist within one tradition or logically exclude each other.').


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
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(deca_be_t8, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(deca_be_t16, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(deca_be_t24, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(deca_be_t32, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(deca_su_t8, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(deca_su_t16, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(deca_su_t24, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(deca_su_t32, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconodule_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the decalogue_image_prohibition kernel. The iconoclast_reading forbids all religious imagery (no dimensional line, no licensing apparatus — likely a Mountain-adjacent or Tangled Rope structure with minimal administrative overhead). The iconodule_reading permits both dimensions via the latria/dulia distinction (a Rope-shaped coordination mechanism with low compliance cost, since no gatekeeping line is drawn). This moderate_iconoclast_reading uniquely generates a snare-shaped structure because it alone requires an administrative apparatus (licensing, inspection, certification) to police a dimensional line that has no counterpart in either sibling. The three readings share the same underlying kernel text but produce three structurally distinct constraints with three different epsilon values, three different beneficiary/victim structures, and three different classifications — exactly the decomposition the eps-invariance principle requires rather than one story averaged across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
