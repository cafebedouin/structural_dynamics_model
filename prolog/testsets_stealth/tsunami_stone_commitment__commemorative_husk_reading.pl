% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__commemorative_husk_reading, []).

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
 *   constraint_id: tsunami_stone_commitment__commemorative_husk_reading
 *   human_readable: Tsunami Warning Stone Commitment — Commemorative Husk Reading
 *   domain: disaster_anthropology/commitment_system_analysis/institutional_memory
 *
 * SUMMARY:
 *   Along the Sanriku coast, communities that survived the 1896 and 1933
 *   tsunamis carved warnings into stone: do not build below this point. By
 *   2011 the stones still stood, cleaned and ceremonially attended, while
 *   most settlements had long since filled the lowlands beneath them. This
 *   story instantiates the commemorative_husk_reading of the kernel
 *   tsunami_stone_commitment: the inscription regime decayed into a symbolic
 *   artifact whose protective compliance was coincidental or weakly enforced,
 *   and whose ceremonial persistence supplied the reassuring sense that
 *   disaster memory was being honored while siting decisions transferred
 *   exposure to those not yet born. The epsilon referent is the standing
 *   arrangement as it actually operated — stone regime plus the land-use
 *   pattern it ceased to govern — assessed by this reading's own lights; the
 *   endorsed alternative (revived enforcement) is not the referent. Claim and
 *   metrics are independent authored facts: the claim states the structure I
 *   believe true; the metrics state the operation I believe describable.
 *   Family decomposition follows the epsilon-invariance principle — the
 *   behavioral_competence_reading and the catastrophe_validation_axis are
 *   separate files linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - coastal_development_actors: primary beneficiary (powerful/arbitrage) — captures lowland development value, externalizes exposure beyond its horizons
 *   - municipal_heritage_authorities: agenda setter (organized/constrained) — administers the husk, funds the ceremonies, cannot abandon the lowland tax base
 *   - future_coastal_residents: primary target (powerless/trapped) — inherits fixed exposure with no seat in any decision
 *   - low_lying_settlement_households: realized targets (moderate/constrained) — occupy the marked zone, bound by livelihood and kin
 *   - seismic_safety_engineers: excluded voice (moderate/constrained) — prescient recommendations never reached the siting conversation
 *   - disaster_anthropology_researchers: analytical observer (institutional/analytical) — maps stones against inundation lines, sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, 0.78).
domain_priors:suppression_score(tsunami_stone_commitment__commemorative_husk_reading, 0.3).
domain_priors:theater_ratio(tsunami_stone_commitment__commemorative_husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__commemorative_husk_reading, tangled_rope).
narrative_ontology:human_readable(tsunami_stone_commitment__commemorative_husk_reading, "Tsunami Warning Stone Commitment — Commemorative Husk Reading").
narrative_ontology:topic_domain(tsunami_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_system_analysis/institutional_memory").

domain_priors:requires_active_enforcement(tsunami_stone_commitment__commemorative_husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__commemorative_husk_reading, '4152caf2-91e4-4acc-a3db-f0e5c24a90d8').
narrative_ontology:cs_kernel_codification('4152caf2-91e4-4acc-a3db-f0e5c24a90d8', fixed_text).
narrative_ontology:cs_authority_grounding('4152caf2-91e4-4acc-a3db-f0e5c24a90d8', lineage).
narrative_ontology:cs_interpretation_layer_present('4152caf2-91e4-4acc-a3db-f0e5c24a90d8').
narrative_ontology:cs_reading_relation('4152caf2-91e4-4acc-a3db-f0e5c24a90d8', tsunami_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('4152caf2-91e4-4acc-a3db-f0e5c24a90d8', tsunami_stone_commitment__catastrophe_validation_axis, influences).
narrative_ontology:cs_axiom('4152caf2-91e4-4acc-a3db-f0e5c24a90d8', foundational, commemoration_substituted_for_protection).
narrative_ontology:cs_axiom_status(commemoration_substituted_for_protection, holdable).
narrative_ontology:cs_axiom_grounding('4152caf2-91e4-4acc-a3db-f0e5c24a90d8', commemoration_substituted_for_protection, empirically_contingent).
narrative_ontology:cs_axiom('4152caf2-91e4-4acc-a3db-f0e5c24a90d8', foundational, unrepresented_future_bearers_make_lapsed_protection_costly_to_them).
narrative_ontology:cs_axiom_status(unrepresented_future_bearers_make_lapsed_protection_costly_to_them, holdable).
narrative_ontology:cs_axiom_grounding('4152caf2-91e4-4acc-a3db-f0e5c24a90d8', unrepresented_future_bearers_make_lapsed_protection_costly_to_them, deontological).
narrative_ontology:cs_reference_frame('4152caf2-91e4-4acc-a3db-f0e5c24a90d8', commemorative_monument_baseline).
narrative_ontology:cs_drift_state('4152caf2-91e4-4acc-a3db-f0e5c24a90d8', post_2011_inundation_survey, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('4152caf2-91e4-4acc-a3db-f0e5c24a90d8', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_actors).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, municipal_heritage_authorities).
narrative_ontology:constraint_victim(tsunami_stone_commitment__commemorative_husk_reading, future_coastal_residents).
narrative_ontology:constraint_victim(tsunami_stone_commitment__commemorative_husk_reading, low_lying_settlement_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hotel operators, fish-processing firms, landowners, and construction interests that acquired and built on flat, port-adjacent land inside the historical inundation zone — the cheapest and only industrially usable terrain on a steep coast. Returns on siting decisions matured within their investment horizons; tsunami exposure matured on longer timelines. When losses arrived, insurance, disaster relief, and reconstruction subsidies absorbed much of the remainder. Capital is mobile: a firm booking a loss in one coastal town reallocates to another region or sector.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_actors, beneficiary,
    powerful, biographical, arbitrage, regional).

% Town governments and preservation societies that own, clean, and re-ink the stones, organize annual ceremonies, and fold them into school curricula and tourism materials. Maintenance draws budget lines; stewardship attracts grants and cultural prestige. They cannot abandon the lowland tax base that ports and processing plants generate, and after 2011 they administered reconstruction in the same footprint. Their discretion over siting narrowed as national technical standards displaced local custom.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, municipal_heritage_authorities, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__commemorative_husk_reading, municipal_heritage_authorities, beneficiary).

% People not yet born, or not yet resident, during the siting and investment decisions that fixed their exposure. No seat existed for them in town assemblies or land-use hearings; the risk they inherited was set by other parties' shorter horizons. Exit is unavailable in advance — it exists only afterward, as survivors relocating from destroyed neighborhoods.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, future_coastal_residents, payer,
    powerless, generational, trapped, regional).

% Families living below the inscribed high-water marks — some descendants of the communities that raised the stones. Homes, boats, nets, and workplaces tie them to the shore; moving uphill means leaving kin networks, schools, and livelihoods. Some kept the stones' counsel personally while neighbors built seaward; the marks governed no one uniformly. Commemorative identity binds many to place as much as economics does.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, low_lying_settlement_households, payer,
    moderate, biographical, constrained, local).

% Engineers and seismologists who argued before 2011 for rezoning, taller seawalls, or relocation on the strength of the same geological record the stones preserved. Findings circulated in journals and ministry committees but carried little weight against port economics and reconstruction momentum; several sat on advisory boards whose recommendations went unimplemented.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, seismic_safety_engineers, excluded,
    moderate, biographical, constrained, national).

% Scholars who, after 2011, mapped stone locations against surveyed inundation lines, interviewed memory-keepers, and published competing accounts of whether the stones ever guided behavior. They bear none of the costs and collect none of the gains; their analyses now anchor both the husk and behavioral accounts of the record.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, disaster_anthropology_researchers, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_actors).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a durable, publicly legible record of historical flood heights and surrounds it with a recurring ritual calendar — cleaning, ceremonies, school visits — that synchronizes communal memory across generations; originally intended to synchronize settlement-siting decisions with that record.
% TRANSFER_FUNCTION: Moves tsunami exposure from the actors making siting and investment decisions (biographical or shorter horizons) onto present and future residents of the marked zones; moves municipal budgets, volunteer labor, and civic attention into ceremonial upkeep; moves development value on cheap, port-adjacent hazard-zone land to builders and landowners.
% ABSENT_VOICES: Future residents had no seat — the exposure was fixed before they existed. Pre-2011 safety engineers and relocation advocates were present in journals and ministry committees but marginal in town assemblies. The dead of 1896 and 1933 spoke only through stone text increasingly read as folklore rather than instruction.
% DISAPPEARANCE_RATIONALE: Heritage calendars, school curricula, tourism materials, and municipal identity all reference the stones, and reconstruction planning cites them as historical benchmarks; removing them overnight would orphan the commemorative economy and erase the only physical datum linking the 1896, 1933, and 2011 flood heights. Physical protection, under this reading, would continue to rest on seawalls and zoning — not on the stones — so the protective world would not rearrange, but the arranged world around the stones would.
% FOUNDING_PROBLEM: After the 1896 Meiji Sanriku tsunami, and again after 1933, surviving communities needed a way to carry flood-height knowledge past the limit of living memory, so that future generations would site homes and workplaces above the demonstrated kill zone without relearning it by catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: post-2011 university geospatial surveys and cabinet-office records show most inundated settlements stood below nearby inscribed marks, and municipal reconstruction plans justify their footprints by engineered seawalls rather than stone guidance. Heritage boards themselves describe the stones as monuments, not regulations. No beneficiary-seat source alone sustains the claim that the protective mandate remained operative.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__commemorative_husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tsunami_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__commemorative_husk_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the arrangement's operative effect was to fix exposure on parties absent from the decisions that created it, while its development-value gains accrued to seated actors on short horizons. Suppression is modest (0.30): the husk coerces no one physically; its remaining force is discursive — the ceremonial frame presents risk as remembered and handled, damping the demand for siting restraint — plus the structural lock of shore-dependent livelihoods. Note the scalar distinction: base_properties.suppression is the raw structural property (unscaled by power or scope), whereas the suppression_requirement series tracks enforcement-capacity change, which decayed from 0.72 to 0.14 as the survivor generation aged out and transmission became performance. Theater_ratio is very high (0.82): cleaning, re-inking, ceremonies, and curricula dominate activity around the stones, while their governing function approaches zero. Accessibility_collapse is low (0.30): alternatives — seawalls, zoning, relocation — remained available and partly built; the husk coexists with modern defenses rather than foreclosing them. Resistance is moderate (0.38): engineers and some survivors pressed for relocation before 2011, against port economics. The measurement series run on one shared nine-point grid (t=0..78, mapped to 1933..2011) so every tracked metric is authored at every examined time point; trajectories are deliberately monotonic — rising extraction accumulation, rising theater, decaying enforcement — because the husk decay is a secular trend, not a cycle, and no oscillation-as-mechanism claim is made.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes per-seat classifications from the structural data, and the seats diverge sharply. From the development-actor seat the arrangement is compatible heritage: stones are tended, business grows, nothing forbids anything. From the low-lying household seat the same arrangement is a covenant quietly withdrawn — ancestors' marks overhead, floor below the flood line. From the researcher seat it is a selection effect mistaken for protection. Same-level lateral dynamics matter: low_lying_settlement_households and coastal_development_actors are both private actors, but constrained versus arbitrage exit splits them — the household's capital is a house; the firm's capital is a portfolio. Identity-lock appears on the stewardship side: descendant memory-keepers fuse with the role ('the town that remembers'), an institutional identity that sustains the husk; if that frame broke, maintenance would stop and the stones would revert to weathering rock, collapsing the arrangement's residue entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. coastal_development_actors: listed beneficiary with arbitrage exit — derivation places them near the beneficiary pole; gains demonstrably accrue here, hence gain_flow names this seat. future_coastal_residents: listed victim, trapped, powerless — near the full-target pole; their extraction is amplified by having no exit at all. low_lying_settlement_households: victims with constrained exit — high but not maximal. municipal_heritage_authorities: the derivation would read the organized seat as near-beneficiary from its beneficiary listing and cultural-capital gains; the override sets d=0.35 because their actual position is administrator-with-exposure — they spend their own budgets on upkeep and absorb post-2011 reputational liability, a modest net benefit rather than capture. disaster_anthropology_researchers sit analytically outside the flow. Suppression, again, enters the computation unscaled; only extractiveness is scaled by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is preventing two symmetrical mislabels. Reading the husk as a rope (pure memory-coordination) ignores that the same structure transfers risk to unrepresented parties — commemoration and exposure-transfer ride one apparatus. Reading it as a snare ignores that the commemorative function is genuinely valued and independently maintained; the coordination story is not mere cover. Tangled_rope holds both halves. Mandatrophy is resolved: the founding protective mandate is dead (corroborated externally, per the R5 interview), the arrangement persists on inertia and identity, and the status-dead x world-rearranges mismatch flags the zombie condition directly. On coalition: the primary victims cannot coalition — future residents do not yet exist — while low-lying households are fragmented by village, elevation, and livelihood, which is precisely why the transfer persisted for eight decades.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint instantiates the commemorative_husk_reading of kernel tsunami_stone_commitment; would instantiating the behavioral_competence_reading instead produce a different structural classification?',
    'Adjudicate the causal-efficacy record (documented enforcement acts, siting decisions attributable to stone guidance, transmission practices). If live intergenerational enforcement is established, regenerate the story under the sibling reading.',
    'The sibling instantiation yields low epsilon with whole-community beneficiaries and no victim class — the inverse beneficiary/victim structure. The disagreement is located entirely in the causal-efficacy premise, not in the metric arithmetic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification: one kernel, rival readings, divergent epsilon.').

omega_variable(
    survivorship_selection_confound,
    'Were the survivals later attributed to heeding the stones caused by the stones'' guidance, or by confounds (terrain elevation, harbor geometry, early seawalls correlated with where stones stood)?',
    'Geospatial regression of 2011 inundation depth against stone proximity, controlling for elevation and defense infrastructure.',
    'If selection explains the survivals, the husk reading hardens and epsilon stands; a residual protective effect would shift evidentiary weight toward the behavioral reading and lower epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_selection_confound, empirical, 'Whether stone-correlated survival reflects guidance or selection.').

omega_variable(
    counterfactual_enforcement_effect,
    'Would active enforcement of the inscribed high-water marks have materially reduced 2011 mortality, or did wave physics overwhelm any feasible norm?',
    'Matched comparison of settlements with documented siting enforcement against comparable lowland settlements, adjusting for run-up height and topography.',
    'If enforcement would have mattered, the husk''s displacement of protection carries the mortality attribution and the transfer-to-future-residents reading is confirmed; if not, the burden shifts from the arrangement to geography.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_enforcement_effect, empirical, 'Counterfactual efficacy of the lapsed enforcement the husk replaced.').

omega_variable(
    discursive_vs_structural_suppression,
    'Is the weak opposition to lowland rebuilding structural (port-dependent livelihoods, sunk infrastructure) or internalized (fatalistic acceptance, deference to ancestral memory)?',
    'Post-relocation cohort studies: if opposition to hazard-zone return remains weak where livelihoods have been re-established inland, internalized components dominate.',
    'If internalized, effective suppression runs above the structural measure and persists after physical exit becomes available; the omega separates the two mechanisms the scalar cannot.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discursive_vs_structural_suppression, empirical, 'Structural versus internalized share of the remaining suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__commemorative_husk_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(tsun_tr_t10, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(tsun_tr_t20, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(tsun_tr_t30, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(tsun_tr_t40, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(tsun_tr_t50, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 50, 0.61).
narrative_ontology:measurement(tsun_tr_t60, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 60, 0.7).
narrative_ontology:measurement(tsun_tr_t70, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 70, 0.76).
narrative_ontology:measurement(tsun_tr_t78, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 78, 0.82).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(tsun_be_t10, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(tsun_be_t20, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(tsun_be_t30, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(tsun_be_t40, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(tsun_be_t50, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(tsun_be_t60, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(tsun_be_t70, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 70, 0.73).
narrative_ontology:measurement(tsun_be_t78, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 78, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tsun_su_t0, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(tsun_su_t10, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(tsun_su_t20, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(tsun_su_t30, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(tsun_su_t40, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(tsun_su_t50, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 50, 0.31).
narrative_ontology:measurement(tsun_su_t60, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 60, 0.24).
narrative_ontology:measurement(tsun_su_t70, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 70, 0.18).
narrative_ontology:measurement(tsun_su_t78, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 78, 0.14).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment__behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment__catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the tsunami stones protected the villages' decomposes, per the epsilon-invariance principle, into rival readings of one kernel (tsunami_stone_commitment) with different epsilon values and different beneficiary/victim structures. The behavioral_competence_reading authors low epsilon with whole-community beneficiaries; the catastrophe_validation_axis authors 2011 as a clean binary test; this commemorative_husk_reading authors high epsilon with development-actor beneficiaries and future-resident victims. Each is a separate file; this story links both siblings via affects_constraints. Upstream/downstream: the validation axis is cited as evidence for the behavioral reading, and this reading's confound analysis attacks the axis's decisiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tsunami_stone_commitment__commemorative_husk_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
