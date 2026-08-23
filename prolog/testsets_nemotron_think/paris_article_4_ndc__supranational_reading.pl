% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__supranational_reading, []).

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
 *   constraint_id: paris_article_4_ndc__supranational_reading
 *   human_readable: Paris Article 4 NDCs as Binding Supranational Commitments with Ratcheting Accountability
 *   domain: international_law/climate_governance/political_economy
 *
 * SUMMARY:
 *   The supranational reading of Paris Article 4 treats NDCs as legally
 *   binding commitments under international law, subject to a ratcheting
 *   mechanism (global stocktake every 5 years) that legally requires
 *   progressively higher ambition, with a compliance committee empowered to
 *   impose reputational and financial sanctions for non-compliance. This
 *   reading institutionalizes wealth transfers from developed to developing
 *   states as legal obligations, not voluntary aid. It creates a
 *   high-extraction constraint system: carbon-intensive industries face
 *   regulatory extinction timelines, fossil fuel exporters face stranded
 *   asset risks enforced by trade measures, and developed states bear binding
 *   finance obligations. The coordination function is genuine — the regime
 *   solves the collective-action credibility problem — but the extraction is
 *   asymmetric and actively enforced, making this a tangled rope from the
 *   supranational reading's own structural perspective.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.78).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.72).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris Article 4 NDCs as Binding Supranational Commitments with Ratcheting Accountability").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_law/climate_governance/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, '51c9fac7-fcae-4981-8511-3191ad869935').
narrative_ontology:cs_kernel_codification('51c9fac7-fcae-4981-8511-3191ad869935', formalized).
narrative_ontology:cs_authority_grounding('51c9fac7-fcae-4981-8511-3191ad869935', extraction).
narrative_ontology:cs_interpretation_layer_present('51c9fac7-fcae-4981-8511-3191ad869935').
narrative_ontology:cs_reading_relation('51c9fac7-fcae-4981-8511-3191ad869935', paris_article_4_ndc__equity_reading, forecloses).
narrative_ontology:cs_reading_relation('51c9fac7-fcae-4981-8511-3191ad869935', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_axiom('51c9fac7-fcae-4981-8511-3191ad869935', foundational, binding_international_climate_law).
narrative_ontology:cs_axiom_status(binding_international_climate_law, holdable).
narrative_ontology:cs_axiom_grounding('51c9fac7-fcae-4981-8511-3191ad869935', binding_international_climate_law, conventional).
narrative_ontology:cs_axiom('51c9fac7-fcae-4981-8511-3191ad869935', foundational, ratcheting_mechanism_legally_enforceable).
narrative_ontology:cs_axiom_status(ratcheting_mechanism_legally_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('51c9fac7-fcae-4981-8511-3191ad869935', ratcheting_mechanism_legally_enforceable, conventional).
narrative_ontology:cs_axiom('51c9fac7-fcae-4981-8511-3191ad869935', secondary, climate_finance_as_legal_obligation).
narrative_ontology:cs_axiom_status(climate_finance_as_legal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('51c9fac7-fcae-4981-8511-3191ad869935', climate_finance_as_legal_obligation, conventional).
narrative_ontology:cs_reference_frame('51c9fac7-fcae-4981-8511-3191ad869935', paris_treaty_legal_framework).
narrative_ontology:cs_drift_state('51c9fac7-fcae-4981-8511-3191ad869935', contemporary_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('51c9fac7-fcae-4981-8511-3191ad869935', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, climate_vulnerable_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, developing_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, international_climate_institutions).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, developed_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, developing_states).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, common_but_differentiated_responsibilities_as_dynamic_obligation).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, international_environmental_law_as_binding_not_aspirational).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, climate_finance_as_legal_obligation_not_charity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Champions the binding supranational reading in negotiations; commits to legally binding NDCs with ratcheting ambition and institutionalized wealth transfers to developing states. Bears financial costs of climate finance obligations and domestic decarbonization mandates. Exit is constrained by treaty law, reputational costs, and the geopolitical necessity of climate leadership.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, developed_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, developed_states, payer).

% Receives institutionalized wealth transfers, technology transfer, and capacity building under the binding regime. Simultaneously bound by ratcheting NDC commitments that constrain fossil-fueled development pathways. Exit is identity-locked: the climate justice narrative and development-as-survival framing make withdrawal from the regime politically unimaginable domestically and internationally.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, developing_states, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, developing_states, payer).

% Existential dependence on the regime's binding character — without legally enforceable ratcheting and finance, they face territorial loss and state failure. No meaningful exit: they cannot leave the climate system, and the regime is their only structural lever. Trapped by geography and physics, not just politics.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, climate_vulnerable_states, beneficiary,
    moderate, generational, trapped, global).

% Faces regulatory extinction timelines imposed by binding NDC trajectories: coal phase-out, internal combustion bans, carbon pricing at levels that render current assets stranded. Exit options constrained by sunk capital, workforce dependencies, and the global scope of the regime — relocation merely shifts jurisdiction to another bound party.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    powerful, biographical, constrained, global).

% State revenues, geopolitical influence, and social contracts depend on hydrocarbon rents. Binding ratcheting NDCs with international accountability directly threaten the resource base. Exit is constrained: they can obstruct, delay, or defect, but the regime's financial sanctions and trade measures (carbon border adjustments) follow them; diversification is a multi-decade project they have not started.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_states, payer,
    powerful, generational, constrained, global).

% UNFCCC secretariat, Paris Agreement mechanisms, and associated financial institutions (GCF, etc.) administer the binding regime: transparency framework, global stocktake, compliance committee, finance mobilization. Their institutional survival and mandate expansion depend on the supranational reading prevailing. They observe the system analytically but also constitute its enforcement architecture.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, international_climate_institutions, agenda_setter,
    institutional, civilizational, analytical, global).

% Civil society networks that mobilize the supranational reading as a normative benchmark — they demand binding accountability, ratcheting, and finance as justice. They do not administer the constraint but shape its legitimacy; their exit is mobile (they can shift framing, targets, tactics) but they are invested in the regime's survival as a lever.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, climate_justice_movements, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the global collective-action problem of climate mitigation by converting voluntary pledges into legally binding, ratcheting commitments with a transparency framework, global stocktake, and compliance mechanism — creating a credible pathway to net-zero that no state could credibly commit to unilaterally.
% TRANSFER_FUNCTION: Moves financial resources, technology, and carbon budget space from developed states and carbon-intensive industries to developing states and climate-vulnerable states, institutionalized through the $100B/year floor, Article 6 markets, and loss-and-damage arrangements — all legally enforceable under the binding reading.
% ABSENT_VOICES: Future generations (who bear the consequences of insufficient ratcheting), Indigenous peoples whose territories host both extraction and 'green transition' mining, and states that have not ratified or have withdrawn (US under Trump, Iran, Libya, Yemen, Eritrea) — they are structurally excluded from the compliance mechanism but subject to its extraterritorial effects (CBAM, finance conditionalities).
% DISAPPEARANCE_RATIONALE: If the binding supranational reading vanished overnight, the Paris Agreement would revert to a purely voluntary pledge-review system (the sovereigntist reading), the $100B finance obligation would become aspirational, carbon border adjustments would lose legal basis, and the global stocktake would lose its compliance teeth — the entire architecture of ratcheting accountability would collapse into a coordination vacuum.
% FOUNDING_PROBLEM: The Copenhagen Accord (2009) and pre-Paris regime failed because voluntary pledges without legal force or ratcheting mechanism could not close the emissions gap; developing states refused binding commitments without differentiated responsibility and finance; developed states refused binding finance without universal participation. The Paris Agreement was built to solve this trilemma through a legally binding framework with self-differentiated NDCs, ratcheting, and institutionalized finance.
% FOUNDING_PROBLEM_CORROBORATION: The UNFCCC secretariat and EU attest the trilemma persists (emissions gap widening, finance shortfall, ratcheting insufficient). Developing state negotiators (G77, AOSIS, LDCs) attest the differentiated responsibility limb is being eroded by the supranational reading's uniform accountability. Independent legal scholars (Bodansky, Rajamani, Voigt) attest the treaty's hybrid legal character — binding process, non-binding targets — remains unresolved in practice.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__supranational_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__supranational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__supranational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint moves substantial resources (finance, carbon budget, regulatory freedom) from powerful actors (developed states, carbon industries, fossil exporters) to vulnerable actors, with legal enforceability. Suppression (0.72) is high because the regime's persistence depends on active enforcement: transparency framework, compliance committee, carbon border adjustments, finance conditionalities — not merely participant preference. Theater ratio (0.38) is moderate: the global stocktake and transparency framework perform accountability, but the gap between pledged and implemented NDCs grows, and the compliance committee has never imposed material sanctions. Accessibility collapse (0.75) is high: once a state ratifies under this reading, the legal architecture makes withdrawal costly and re-entry conditional on higher ambition. Resistance (0.68) is high: developed states resist finance legalization, fossil exporters obstruct, carbon industries litigate — but the regime's institutional momentum continues.
 *
 * PERSPECTIVAL GAP:
 *   The supranational reading presents itself as rope (pure coordination solving collective action) from the agenda-setter seats (developed states, institutions). From the payer seats (carbon industries, fossil exporters, developing states' development aspirations), the same structure computes as snare — binding extraction without their consent. The engine computes this divergence from the structural data; the claimed_type 'tangled_rope' acknowledges both functions coexist in one constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed states are agenda-setters who designed the binding architecture but also payers who bear finance and decarbonization costs — their directionality is pulled toward beneficiary by institutional control but toward payer by resource extraction. Developing states are beneficiaries of finance but payers of constrained development — their identity-locked exit (climate justice narrative fused with development survival) traps them in the payer seat despite beneficiary status. Climate-vulnerable states are pure beneficiaries but trapped by physics, not choice. Carbon-intensive industries and fossil exporters are pure payers with constrained exit (global scope, sunk assets). International institutions are agenda-setters whose survival depends on the regime's binding character — analytical exit but institutional capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding trilemma (credibility, differentiation, universality) remains contested — the supranational reading claims to have solved it through binding process with self-differentiated targets, but the equity reading contests that differentiation has been hollowed out, and the sovereigntist reading contests that bindingness was ever agreed. The mandate has not atrophied (ratcheting continues, finance architecture expands) but its legitimacy is contested from within the regime. This is not piton — the coordination function is live and the extraction is active, not inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'Is the supranational reading a distinct constraint from the equity_reading and sovereigntist_reading, or a measurement basis of a single constraint?',
    'Apply the ε-invariance test: if evaluating NDC bindingness under the supranational reading yields ε=0.78 but under the sovereigntist reading yields ε≈0.15 (voluntary pledges), they are different constraints with different ε referents. The kernel_id paris_article_4_ndc indexes the contested commitment; each reading instantiates a separate constraint story with its own ε, stakeholders, and classification.',
    'If they are one constraint, the framework must model observable-dependent classification (rejected by DP-001). If they are three constraints, each gets its own ε-invariant story linked by network.affects_constraints. The latter is the correct modeling choice per ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Whether the kernel''s multiple readings constitute one constraint or a constraint family.').

omega_variable(
    binding_character_enforceability,
    'Are the ''binding commitments'' and ''international accountability'' of this reading legally enforceable in practice, or politically aspirational?',
    'Track compliance committee outcomes, ICJ/ITLOS advisory opinions, carbon border adjustment litigation, and finance mobilization records. If non-compliance triggers material sanctions (not just naming/shaming), the binding character is enforceable. If the regime operates through peer pressure and transparency alone, the reading''s structural premises are aspirational.',
    'If enforceable, the high extractiveness and suppression metrics are structurally justified. If aspirational, the constraint''s actual operation is lower-extraction (rope or scaffold) and the supranational reading overstates its own coercive architecture — a false summit of legal form over political substance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_character_enforceability, empirical, 'Whether the supranational reading''s legal bindingness translates to material enforcement.').

omega_variable(
    north_south_transfer_realization,
    'Will the institutionalized wealth transfers from North to South materialize at the scale the reading requires, or remain a legal shell?',
    'Track GCF replenishments, Article 6.4 market launches, loss-and-damage fund operationalization, and MDB climate finance alignment against the $100B/year floor and post-2025 NCQG. Compare legal obligations (Decision 1/CP.21, Paris Article 9) to actual disbursements.',
    'If transfers materialize, the beneficiary structure for developing/vulnerable states is real and the tangled rope''s coordination function is substantiated. If transfers fail, the developing state beneficiary seat is illusory — the constraint extracts from their development space without delivering the promised coordination benefit, shifting classification toward snare for that seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(north_south_transfer_realization, empirical, 'Whether the reading''s promised North-South wealth transfers will actually be delivered.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__supranational_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(pari_tr_t6, paris_article_4_ndc__supranational_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(pari_tr_t12, paris_article_4_ndc__supranational_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(pari_tr_t18, paris_article_4_ndc__supranational_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement(pari_tr_t24, paris_article_4_ndc__supranational_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(pari_tr_t30, paris_article_4_ndc__supranational_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__supranational_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pari_be_t6, paris_article_4_ndc__supranational_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(pari_be_t12, paris_article_4_ndc__supranational_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(pari_be_t18, paris_article_4_ndc__supranational_reading, base_extractiveness, 18, 0.7).
narrative_ontology:measurement(pari_be_t24, paris_article_4_ndc__supranational_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(pari_be_t30, paris_article_4_ndc__supranational_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__supranational_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(pari_su_t6, paris_article_4_ndc__supranational_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(pari_su_t12, paris_article_4_ndc__supranational_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(pari_su_t18, paris_article_4_ndc__supranational_reading, suppression_requirement, 18, 0.66).
narrative_ontology:measurement(pari_su_t24, paris_article_4_ndc__supranational_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(pari_su_t30, paris_article_4_ndc__supranational_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__supranational_reading, 0.12).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__equity_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, carbon_border_adjustment_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, loss_and_damage_fund).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, article_6_4_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the supranational_reading of the paris_article_4_ndc kernel. It forecloses both sibling readings (equity_reading and sovereigntist_reading) within any single legal framework because its core premises — binding uniform accountability, legally enforceable ratcheting, institutionalized North-South transfers — directly contradict the equity reading's structural differentiation and the sovereigntist reading's voluntary sovereignty. The three stories form a constraint family linked by mutual foreclosure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__supranational_reading, organized, 0.65).
constraint_indexing:directionality_override(paris_article_4_ndc__supranational_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
