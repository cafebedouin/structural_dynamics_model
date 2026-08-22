% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Legitimacy of Practice Change via Voluntary Adoption (Endogenous Displacement Reading)
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This story instantiates the endogenous-displacement reading of a
 *   contested kernel about when practice change (calendar reform, dress
 *   codes, customary shifts) is legitimate. Under this reading, the standing
 *   arrangement being assessed is the historically-observed pattern of
 *   gradual, voluntary, utility-driven adoption: elites adopt first for
 *   status and network access, merchants follow for transaction efficiency,
 *   and the practice diffuses regionally over one to several generations
 *   while holdouts maintain a transitional 'double life.' The reading treats
 *   this diffusion pattern itself — not state decree, not domain-partitioned
 *   coexistence — as the legitimating mechanism. Sibling readings
 *   (exogenous_override_reading: state decree legitimates change;
 *   dual_practice_equilibrium_reading: legitimacy is domain-partitioned
 *   between state and traditional authority) are NOT part of this story; they
 *   are separate constraints with their own ε and stakeholder structures,
 *   linked here only via cs_structure relations and network edges.
 *
 * KEY AGENTS:
 *   - early_adopter_elites: powerful/arbitrage — first movers who profit from prestige and network access
 *   - urban_merchant_classes: moderate/mobile — adopt for genuine transaction-cost reasons
 *   - practice_innovation_entrepreneurs: moderate/mobile — commercial beneficiaries of the new practice's material culture
 *   - traditionalist_rural_communities: powerless/constrained — bear rising friction costs as holdouts
 *   - practitioners_of_displaced_custom: powerless/trapped — lose livelihood as demand for old-practice skills erodes
 *   - cultural_evolution_theorists: analytical/analytical — document and interpret the diffusion pattern
 *   - holdout_regions: powerless/constrained — excluded from the narrative of successful voluntary change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.28).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.22).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Legitimacy of Practice Change via Voluntary Adoption (Endogenous Displacement Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/modernization_studies/institutional_change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '20773d6f-9256-414b-a1bb-125901a92d38').
narrative_ontology:cs_kernel_codification('20773d6f-9256-414b-a1bb-125901a92d38', distributed).
narrative_ontology:cs_authority_grounding('20773d6f-9256-414b-a1bb-125901a92d38', practice).
narrative_ontology:cs_interpretation_layer_present('20773d6f-9256-414b-a1bb-125901a92d38').
narrative_ontology:cs_reading_relation('20773d6f-9256-414b-a1bb-125901a92d38', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('20773d6f-9256-414b-a1bb-125901a92d38', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('20773d6f-9256-414b-a1bb-125901a92d38', foundational, voluntary_uptake_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(voluntary_uptake_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('20773d6f-9256-414b-a1bb-125901a92d38', voluntary_uptake_constitutes_legitimacy, conventional).
narrative_ontology:cs_axiom('20773d6f-9256-414b-a1bb-125901a92d38', secondary, perceived_utility_is_sufficient_warrant_for_displacement).
narrative_ontology:cs_axiom_status(perceived_utility_is_sufficient_warrant_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('20773d6f-9256-414b-a1bb-125901a92d38', perceived_utility_is_sufficient_warrant_for_displacement, instrumental).
narrative_ontology:cs_reference_frame('20773d6f-9256-414b-a1bb-125901a92d38', organic_diffusion_baseline).
narrative_ontology:cs_drift_state('20773d6f-9256-414b-a1bb-125901a92d38', post_mass_adoption_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('20773d6f-9256-414b-a1bb-125901a92d38', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopter_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_merchant_classes).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, practice_innovation_entrepreneurs).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditionalist_rural_communities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, practitioners_of_displaced_custom).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_evolutionary_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, voluntary_diffusion_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Urban notables, merchants, and professionals who adopt the new calendar, dress, or custom first because it signals cosmopolitan status and eases dealings with foreign trade partners or administrators. They set the visible example that others copy, and they profit from being early — access to new networks, prestige, and often preferential treatment in commerce that uses the new standard.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopter_elites, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopter_elites, agenda_setter).

% Traders and shopkeepers who find the new practice (a common calendar, standardized dress code, a new weekly rest day) genuinely useful for coordinating with distant partners and foreign markets. They adopt it because it lowers transaction friction, not because anyone compels them, and they can revert or hybridize as convenient.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_merchant_classes, beneficiary,
    moderate, biographical, mobile, regional).

% Tailors, printers, calendar-makers, and cultural brokers who profit from selling the material and symbolic goods of the new practice — new garments, new almanacs, new manners manuals. Their livelihood depends on the practice spreading, but no one forces the spread; they compete on persuasion and utility.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, practice_innovation_entrepreneurs, beneficiary,
    moderate, generational, mobile, regional).

% Villages and agrarian communities whose seasonal calendar, dress, and ritual life were built around the old practice. As urban and elite adoption spreads, they experience mounting friction — market days shift, administrative deadlines are recalculated in the new terms, marriageable and creditworthy status increasingly depend on visible modernization. Formally no one compels them, but the network effects of holdouts shrinking make maintaining the old practice steadily more costly.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditionalist_rural_communities, payer,
    powerless, generational, constrained, regional).

% Ritual specialists, seamstresses, and calendar-keepers whose expertise is tied to the old practice. As voluntary adoption erodes demand for their skills, their social standing and livelihood decline even though no law targeted them. They cannot easily retrain into the new practice's equivalent roles because those roles are already occupied by adapters from the elite-diffusion pathway.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, practitioners_of_displaced_custom, payer,
    powerless, biographical, trapped, local).

% Historians and social scientists who document diffusion curves, regional variation, and elite-to-mass adoption patterns. They read the change as an emergent, bottom-up process legitimated by demonstrated utility rather than coercion, and they compare this case to other diffusion episodes to test the endogenous-displacement thesis.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_evolution_theorists, observer,
    analytical, civilizational, analytical, global).

% Peripheral regions that resist longest, sometimes for a generation or more, maintaining a 'double life' — official new-practice compliance in dealings with outsiders alongside continued old-practice life internally. Their voice rarely enters the official record of how the change happened, since the story is told from the perspective of the diffusion's successful centers.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, holdout_regions, excluded,
    powerless, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__endogenous_displacement_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__endogenous_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Adopting a common calendar, dress code, or custom lowers transaction and communication costs among people who increasingly interact across regional and cultural lines — a shared standard makes trade, travel, and administration easier for anyone who opts in.
% TRANSFER_FUNCTION: Status, market access, and administrative convenience shift toward early adopters and those positioned to profit from the new practice's material culture (tailors, printers, brokers); social standing and economic footing shift away from those whose skills and routines were built around the practice being displaced.
% ABSENT_VOICES: Holdout regions and displaced-custom practitioners rarely feature in the diffusion narrative except as a residual 'lag' to be overcome; their perspective — that gradual erosion of practical viability is itself a form of pressure, not pure voluntary choice — is largely absent from accounts that celebrate the change as organic cultural evolution.
% DISAPPEARANCE_RATIONALE: If voluntary-adoption legitimacy were withdrawn as the operative story, the material fact of the diffusion (calendars printed, garments sold, market days already shifted) would likely persist — the practice has already taken root. What would change is the political cover: without the endogenous-displacement narrative, resistance to further practice change could no longer be framed as mere 'lag' to be waited out, and holdout communities could contest new pressure to conform on the grounds that the earlier 'voluntary' change was not as voluntary as claimed.
% FOUNDING_PROBLEM: Communities engaging in wider trade, travel, and administration needed common reference points — dates, dress signals, customary practices — that worked across formerly separate cultural zones; the old localized practices increasingly failed to serve interactions beyond the local community.
% FOUNDING_PROBLEM_CORROBORATION: Cultural evolution theorists and comparative historians (outside the beneficiary set) attest that genuine coordination gains from standardization existed and drove real uptake among cross-regional traders. However, oral histories and ethnographic accounts collected from holdout and displaced-custom communities — also outside the beneficiary set — attest that the 'voluntary' framing understates the practical coercion of shrinking markets, administrative penalties for continued old-practice use, and social stigma; both attestations are independently sourced and in tension.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28 at interval end) because the endogenous-displacement reading's own account genuinely centers voluntary utility-driven uptake rather than coercive extraction — the ε referent is the standing diffusion arrangement AS THIS READING SEES IT, and this reading sees relatively little coercion, though it acknowledges some structural squeeze on holdouts as network effects shrink their options. Suppression is low (0.22): there is no legal or administrative machinery forcing adoption under this reading's own account; whatever pressure holdouts feel is market-and-status pressure, not coercion. Theater ratio stays low throughout (0.05 to 0.15) because the diffusion process is not primarily performative — it reflects real, if uneven, uptake. Accessibility collapse (0.35) and resistance (0.3) are both moderate: alternatives to adoption persist longer under this reading than under an exogenous-override reading, and resistance is real but framed as temporary friction that resolves as utility perception spreads, consistent with the expected structural delta (gradual adoption curves, regional variation, elite-to-mass diffusion, temporary resistance, transitional double-life).
 *
 * PERSPECTIVAL GAP:
 *   From the early-adopter and merchant seats, the process reads cleanly as rope: pure coordination gain from voluntary standardization, low coercion, exit always available. From the traditionalist and displaced-custom seats, the same diffusion computes closer to a tangled arrangement: what is voluntary from the center looks like structural squeeze from the periphery, as shrinking markets and administrative convenience quietly foreclose the old practice's viability without any single decree ever being issued against it. The engine should register this divergence from the differing power/exit declarations, not from any claim override.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters, merchants, and innovation entrepreneurs are declared beneficiaries with mobile-to-arbitrage exit — the practice change subsidizes their status, efficiency, or livelihood, so directionality sits near the beneficiary end. Traditionalist communities and displaced-custom practitioners are declared victims with constrained-to-trapped exit — even though this reading holds the change to be voluntary at the level of individual choice, the aggregate effect of others' voluntary adoption steadily narrows their practical options, which is exactly the structural asymmetry the victim declaration captures without contradicting the reading's own 'voluntary' framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for cross-regional coordination standards) is contested as live vs. dead: cultural evolution theorists affirm the coordination need was real and remains partially live wherever cross-regional interaction continues to grow; but the practice, once diffused, persists well past the point where holdout communities have any realistic alternative, which risks mislabeling completed extraction (foreclosed old-practice viability) as ongoing organic coordination. Classifying this as rope (not snare) prevents overcorrection — the coordination function is genuine and the mechanism is not centrally enforced — while the victim declarations and moderate extractiveness prevent undercorrection that would erase the real cost borne by holdouts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_structurally_induced_adoption,
    'Is the diffusion this reading describes genuinely voluntary at the level where legitimacy is claimed, or does aggregate elite/merchant adoption structurally foreclose the old practice''s viability for holdouts in a way that functions as coercion without any coercive act?',
    'Comparative case analysis: track whether holdout communities that received subsidized support to maintain the old practice indefinitely) maintained genuine long-run viability, versus communities where market/administrative network effects alone determined the outcome. If subsidized holdouts persist stably while unsubsidized ones collapse, the ''voluntary'' framing understates structural pressure.',
    'If adoption is substantially structurally induced rather than utility-perceived, this reading''s own legitimacy claim weakens, and the constraint would be read closer to tangled_rope than rope even within this reading''s terms — though it would still not become the exogenous_override_reading, since no central decree exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_structurally_induced_adoption, empirical, 'Whether the diffusion this reading calls voluntary contains disguised structural coercion via network effects.').

omega_variable(
    elite_diffusion_as_natural_or_engineered,
    'Is elite-first adoption a natural byproduct of elites having more cross-regional contact and status incentive, or is it partly engineered by early adopters and entrepreneurs who benefit from being first and who actively promote the practice to consolidate that advantage?',
    'Archival evidence of coordinated promotion efforts (guild campaigns, merchant association lobbying, printer/tailor marketing) versus evidence of purely emergent, uncoordinated individual choices.',
    'Evidence of coordinated promotion would push the beneficiary declarations toward a more active agenda-setting role, potentially warranting reclassification toward tangled_rope even within this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_diffusion_as_natural_or_engineered, conceptual, 'Whether elite-led diffusion is spontaneous or actively engineered by beneficiaries.').

omega_variable(
    kernel_framing_underdetermination,
    'Given that the same historical diffusion episode (e.g. a calendar or dress reform) could be authored under any of the three sibling readings depending on which observable is foregrounded (adoption curves vs. decree dates vs. domain partition), is the choice of the endogenous_displacement_reading for THIS story justified by the specific evidence available, or could the same underlying event equally well support the exogenous_override_reading if state administrative deadlines are foregrounded instead?',
    'Cross-reference the specific historical record cited for this story: if state decrees exist but postdate widespread voluntary uptake, endogenous_displacement is the better fit; if decrees precede and drive uptake, exogenous_override would be the better fit for that specific episode.',
    'Misassigning a state-driven episode to this reading would inflate the apparent legitimacy of what was actually imposed change; the classification of ANY specific historical case under this reading should be checked against decree-timing evidence before being cited as an instance of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the choice of this reading over its siblings is well-grounded for any given specific historical episode, given observable underdetermination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 50, 0.14).
narrative_ontology:measurement(legi_tr_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 60, 0.15).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 10, 0.13).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 50, 0.27).
narrative_ontology:measurement(legi_be_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 60, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(legitimacy_of_practice_standardization__endogenous_displacement_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.1).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legitimacy_of_practice_standardization kernel. The exogenous_override_reading locates legitimacy in state decree rather than voluntary uptake — a structurally opposed origin claim. The dual_practice_equilibrium_reading rejects the premise that either origin story fully displaces the alternative practice, instead modeling indefinite domain-partitioned coexistence. All three stories describe the same kernel (contested claims about legitimate practice change) but instantiate structurally distinct constraints with different beneficiary/victim structures, different ε, and different persistence dynamics. They are linked here for contamination-propagation analysis, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
