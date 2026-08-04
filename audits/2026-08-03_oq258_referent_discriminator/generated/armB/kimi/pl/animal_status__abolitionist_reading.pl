% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__abolitionist_reading, []).

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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Abolitionist Animal Rights: Inherent Value Precluding All Instrumental Use
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the abolitionist reading of the
 *   animal_status kernel: the claim that nonhuman animals are rights-holders
 *   with inherent value that categorically precludes all instrumental human
 *   use. It is a contested normative legal-philosophical constraint that
 *   functions as a tangled rope: it coordinates society toward ending animal
 *   exploitation but asymmetrically extracts from animal-use industries and
 *   research sectors. The story authors high extractiveness and suppression
 *   because the constraint's realization requires prohibiting economically
 *   central practices and actively suppressing the property and welfare
 *   alternatives; the beneficiaries are nonhuman animals and the payers are
 *   the livestock and research sectors. This is one of three readings of the
 *   kernel; siblings are authored as separate constraints.
 *
 * KEY AGENTS:
 *   - abolitionist_coalition: Agenda-setter (organized/global/generational) — establishes the rights framework and pushes for legal prohibition.
 *   - nonhuman_animals: Beneficiary (powerless/trapped/universal) — the protected class removed from instrumental use.
 *   - livestock_industry: Primary payer (powerful/constrained/global) — bears the economic cost of prohibition and stranded capital.
 *   - animal_research_sector: Secondary payer (institutional/constrained/national) — faces shutdown of animal-dependent research protocols.
 *   - welfare_reform_advocates: Excluded voice (moderate/mobile/global) — argues for incremental reform, rejected as legitimation of exploitation.
 *   - jurisprudential_observer: Analytical observer (analytical/analytical/universal) — tracks the logical structure of rights expansion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.82).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.85).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Abolitionist Animal Rights: Inherent Value Precluding All Instrumental Use").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, 'cf85de74-3698-4315-afd0-04fb5e49343f').
narrative_ontology:cs_kernel_codification('cf85de74-3698-4315-afd0-04fb5e49343f', formalized).
narrative_ontology:cs_authority_grounding('cf85de74-3698-4315-afd0-04fb5e49343f', lineage).
narrative_ontology:cs_interpretation_layer_present('cf85de74-3698-4315-afd0-04fb5e49343f').
narrative_ontology:cs_reading_relation('cf85de74-3698-4315-afd0-04fb5e49343f', animal_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('cf85de74-3698-4315-afd0-04fb5e49343f', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('cf85de74-3698-4315-afd0-04fb5e49343f', foundational, inherent_value_independent_of_use).
narrative_ontology:cs_axiom_status(inherent_value_independent_of_use, holdable).
narrative_ontology:cs_axiom_grounding('cf85de74-3698-4315-afd0-04fb5e49343f', inherent_value_independent_of_use, deontological).
narrative_ontology:cs_axiom('cf85de74-3698-4315-afd0-04fb5e49343f', foundational, instrumental_use_categorically_impermissible).
narrative_ontology:cs_axiom_status(instrumental_use_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('cf85de74-3698-4315-afd0-04fb5e49343f', instrumental_use_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('cf85de74-3698-4315-afd0-04fb5e49343f', rights_bearing_moral_community).
narrative_ontology:cs_drift_state('cf85de74-3698-4315-afd0-04fb5e49343f', contemporary_legal_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('cf85de74-3698-4315-afd0-04fb5e49343f', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, nonhuman_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, livestock_industry).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animal_research_sector).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, inherent_value_theory).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, speciesism_as_prejudice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and drafts the legal-philosophical framework establishing animals as rights-holders; works to dismantle property and welfare frameworks. Sets the agenda but does not capture economic extraction; the gain is normative realization and legal transformation.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_coalition, agenda_setter,
    organized, generational, mobile, global).

% The class of beings reclassified from property to rights-holders; they receive the protective benefit of the constraint and are removed from legal commodification and instrumental use categories.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, nonhuman_animals, beneficiary,
    powerless, civilizational, trapped, universal).

% Bears the primary economic cost of prohibition; production facilities, genetic stock, and supply chains lose legal legitimacy and market viability. Transition to plant-based or cellular agriculture is capital-intensive and slow.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, livestock_industry, payer,
    powerful, biographical, constrained, global).

% Academic and commercial laboratories that rely on animal models face legal prohibition of their core methodology; must transition to alternative methods or cease operations. Regulatory approval pathways historically depend on animal data, creating institutional lock-in.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_research_sector, payer,
    institutional, biographical, constrained, national).

% Argue for incremental welfare improvements within existing use paradigms; the abolitionist reading structurally excludes this position from legitimate policy space, treating welfare reforms as legitimation of continued exploitation.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_reform_advocates, excluded,
    moderate, biographical, mobile, global).

% Analyzes the logical and practical consequences of extending rights-based moral status to nonhuman animals; observes the tension between the reference frame of a rights-bearing moral community and existing legal categories of property.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, jurisprudential_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective failure to recognize nonhuman animals as moral patients by establishing a categorical prohibition on instrumental use, eliminating the coordination problem of unchecked exploitation across jurisdictions and industries.
% TRANSFER_FUNCTION: Transfers the legal and economic possibility of using animals from industries and consumers to the protected status of rights-holders; compliance and transition costs fall on animal-use sectors while protective standing accrues to animals.
% ABSENT_VOICES: Welfare reform advocates who would argue for incremental improvement within use, and indigenous or subsistence communities whose practices rely on limited animal use, are structurally excluded from the abolitionist framework's legitimate policy space.
% DISAPPEARANCE_RATIONALE: The global food system, biomedical research pipeline, and legal property regime would revert to treating animals as resources; the absence of the rights-holder status would remove the legal floor preventing commodification and reinstate the property framework.
% FOUNDING_PROBLEM: The systematic instrumentalization of sentient beings as property and resources without recognition of their inherent value or interests, resulting in industrial-scale exploitation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by animal advocacy organizations and a segment of moral philosophy; contested by agricultural economics and legal positivist traditions that treat animals as chattel. No fully neutral corroborator exists outside the normative dispute, though independent ecologists and public health researchers attest to the scale of industrial animal use as a structural problem.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.82) because the constraint categorically prohibits major global industries, creating massive stranded capital and forced transition costs. Suppression is similarly high (0.85) because persistence depends on actively suppressing black markets, property-based legal frameworks, and welfare alternatives. Theater ratio is moderate-low (0.30): the normative commitment is substantively held, though some enforcement activity becomes performative under political pressure. Accessibility collapse is very high (0.90) because once the rights framework is operative, legal instrumental use collapses entirely for compliant agents. Resistance is very high (0.88) due to entrenched economic interests and cultural practice. The measurement series track the historical intensification of the constraint from marginal philosophical position to enforced legal norm.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist coalition experiences the constraint as genuine coordination solving a collective moral failure; the livestock and research sectors experience it as extraction destroying their operating legitimacy. The engine computes this divergence from the same structural data: the coalition sets the agenda with mobile exit, while industries pay with constrained exit. The nonhuman animal beneficiary seat is structurally powerless and trapped, receiving protective subsidy rather than extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Nonhuman animals are declared beneficiaries (powerless, trapped, universal scope), placing them at the full-beneficiary end of the directionality spectrum. The livestock industry and animal research sector are declared victims (powerful/institutional, constrained exit), placing them at the full-target end. The abolitionist coalition is the agenda setter with mobile exit, sitting near the beneficiary side without being a direct economic capturer. Welfare reform advocates are excluded, sitting near the middle. The engine will compute high effective extraction for industries and negative effective extraction (subsidy) for animals.
 *
 * MANDATROPHY ANALYSIS:
 *   Not a mandatrophy case. The constraint's mandate — establishing animals as rights-holders — is not outlived or atrophied. It is aspirational and contested, with active enforcement intensifying over time rather than decaying into performance. Were the mandate to be achieved and enforcement to decay while the legal form persisted, it would risk piton status; current trajectories show rising theater and extraction consistent with active contention, not inertial maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_black_market_risk,
    'Will categorical prohibition of instrumental use generate black markets that increase total animal suffering relative to regulated use?',
    'Comparative analysis of jurisdictions with partial or full prohibitions versus regulated welfare regimes, measuring illegal trade volumes and enforcement costs.',
    'If prohibition reliably generates high-black-market regimes, the constraint''s effective extraction may exceed its protective benefit, potentially shifting classification emphasis toward snare-like harm; if enforcement succeeds, the tangled rope framing is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_black_market_risk, empirical, 'Whether abolitionist enforcement produces counterproductive underground markets.').

omega_variable(
    rights_lineage_validity,
    'Does the rights lineage derived from Enlightenment human-rights frameworks validly extend to nonhuman animals, or does it impose an anthropocentric legal category?',
    'Cross-cultural and jurisprudential analysis of whether rights frameworks function coherently when applied across species boundaries, or whether alternative framings (capabilities, relational ethics) would avoid category error.',
    'If the rights framework is anthropocentric misprojection, the constraint''s authority grounding may shift from lineage to extraction or practice, altering the engine''s assessment of interpretive stability and drift direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_lineage_validity, conceptual, 'Whether rights-based framing is a valid or imposed category for animals.').

omega_variable(
    transition_cost_distribution,
    'Are the economic and social costs of abolition borne justly by animal-use industries, or do they fall disproportionately on marginalized workers and communities?',
    'Socioeconomic impact modeling of sectoral transition, including employment displacement, regional economic dependence, and access to transition capital.',
    'If costs fall unjustly on powerless workers rather than on capital-holding firms, the victim set may need expansion to include economically trapped human communities, changing the directionality profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_cost_distribution, preference, 'Justice of cost distribution during enforced sectoral transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anim_tr_t10, animal_status__abolitionist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(anim_tr_t20, animal_status__abolitionist_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(anim_tr_t30, animal_status__abolitionist_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(anim_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(anim_tr_t50, animal_status__abolitionist_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(anim_be_t10, animal_status__abolitionist_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(anim_be_t20, animal_status__abolitionist_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(anim_be_t30, animal_status__abolitionist_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(anim_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(anim_be_t50, animal_status__abolitionist_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(anim_su_t10, animal_status__abolitionist_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(anim_su_t20, animal_status__abolitionist_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(anim_su_t30, animal_status__abolitionist_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(anim_su_t40, animal_status__abolitionist_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(anim_su_t50, animal_status__abolitionist_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, identity_coordination).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three structurally distinct constraints: abolitionist (rights precluding use), welfare (interests constraining use), and property (legal objects without moral standing). This story instantiates the abolitionist reading only; siblings are separate constraints with their own epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
