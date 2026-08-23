% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__sovereigntist_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: paris_article_4_ndc__sovereigntist_reading
 *   human_readable: Paris Agreement Article 4 NDCs — Sovereigntist Reading
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This constraint story represents the sovereigntist reading of the Paris
 *   Agreement Article 4 NDC framework. Under this reading, NDCs are
 *   voluntary, self-determined national pledges that preserve each state's
 *   energy sovereignty and development pathway autonomy. The global stocktake
 *   and ratchet mechanism (Article 4.3) are procedural — they create
 *   transparency and iterative updating but no binding enforcement.
 *   Fossil-dependent economies (Gulf states, Russia, Australia historically,
 *   coal-dependent emerging economies) and major emerging emitters (India,
 *   Indonesia, South Africa) are the primary beneficiaries: they retain exit
 *   options, revision freedom, and protection against externally imposed
 *   decarbonization timelines. The constraint coordinates through
 *   information-sharing and peer pressure rather than coercion. Theater ratio
 *   rises over time as procedural compliance (submitting NDCs, attending
 *   COPs) substitutes for emissions outcomes. Suppression remains low because
 *   no state is coerced into a target it did not set — though soft pressure
 *   from the stocktake process increases modestly. This reading coexists with
 *   the equity_reading (which demands structural differentiation of
 *   obligations) and the supranational_reading (which treats NDCs as binding
 *   ratchet commitments); none logically forecloses the others within a
 *   single party's framework, but they create competing legitimacy
 *   conditions.
 *
 * KEY AGENTS:
 *   - fossil_dependent_economies: Primary beneficiaries (institutional/biographical/trapped) — preserve fossil revenue streams and development pathways
 *   - emerging_major_emitters: Primary beneficiaries (organized/biographical/constrained) — protect industrialization trajectory and energy access
 *   - state_sovereignty_advocates: Beneficiaries (institutional/generational/arbitrage) — advance sovereignty-as-shield doctrine in international law
 *   - climate_vulnerable_states: Excluded/payer (powerless/biographical/trapped) — bear climate impacts without enforcement leverage
 *   - eu_climate_leadership_bloc: Observer/agenda_setter (institutional/generational/analytical) — pushes for stronger interpretation
 *   - unfccc_secretariat: Observer (institutional/generational/analytical) — administers process without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__sovereigntist_reading, 0.08).
domain_priors:suppression_score(paris_article_4_ndc__sovereigntist_reading, 0.12).
domain_priors:theater_ratio(paris_article_4_ndc__sovereigntist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__sovereigntist_reading, rope).
narrative_ontology:human_readable(paris_article_4_ndc__sovereigntist_reading, "Paris Agreement Article 4 NDCs — Sovereigntist Reading").
narrative_ontology:topic_domain(paris_article_4_ndc__sovereigntist_reading, "international_climate_governance/treaty_law/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__sovereigntist_reading, '220a70ce-d829-41fa-84b5-f7d2cabeed72').
narrative_ontology:cs_kernel_codification('220a70ce-d829-41fa-84b5-f7d2cabeed72', formalized).
narrative_ontology:cs_authority_grounding('220a70ce-d829-41fa-84b5-f7d2cabeed72', distributed).
narrative_ontology:cs_reading_relation('220a70ce-d829-41fa-84b5-f7d2cabeed72', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_reading_relation('220a70ce-d829-41fa-84b5-f7d2cabeed72', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_axiom('220a70ce-d829-41fa-84b5-f7d2cabeed72', foundational, national_energy_sovereignty_inviolable).
narrative_ontology:cs_axiom_status(national_energy_sovereignty_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('220a70ce-d829-41fa-84b5-f7d2cabeed72', national_energy_sovereignty_inviolable, deontological).
narrative_ontology:cs_axiom('220a70ce-d829-41fa-84b5-f7d2cabeed72', foundational, voluntary_pledge_as_only_legitimate_basis).
narrative_ontology:cs_axiom_status(voluntary_pledge_as_only_legitimate_basis, holdable).
narrative_ontology:cs_axiom_grounding('220a70ce-d829-41fa-84b5-f7d2cabeed72', voluntary_pledge_as_only_legitimate_basis, conventional).
narrative_ontology:cs_axiom('220a70ce-d829-41fa-84b5-f7d2cabeed72', secondary, ratchet_is_procedural_not_substantive).
narrative_ontology:cs_axiom_status(ratchet_is_procedural_not_substantive, holdable).
narrative_ontology:cs_axiom_grounding('220a70ce-d829-41fa-84b5-f7d2cabeed72', ratchet_is_procedural_not_substantive, conventional).
narrative_ontology:cs_reference_frame('220a70ce-d829-41fa-84b5-f7d2cabeed72', copenhagen_postmortem_sovereignty_salvage).
narrative_ontology:cs_drift_state('220a70ce-d829-41fa-84b5-f7d2cabeed72', post_glasgow_stocktake_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('220a70ce-d829-41fa-84b5-f7d2cabeed72', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_economies).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, emerging_major_emitters).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, state_sovereignty_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_states).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, national_energy_sovereignty).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, voluntary_pledge_flexibility).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, common_but_differentiated_responsibilities_procedural).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, no_external_compliance_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States whose fiscal revenues and employment depend on fossil extraction (Saudi Arabia, Russia, UAE, Qatar, Kuwait, Iraq, Nigeria, Angola, Australia historically). They use NDC sovereignty to protect revenue streams, delay transition, and resist external timelines. Exit from the constraint means withdrawing from Paris — diplomatically costly but legally trivial. They are 'trapped' in the sense that their economic structure makes genuine transition costly, not that the constraint traps them.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_economies, beneficiary,
    institutional, biographical, trapped, national).

% Rapidly industrializing states with large populations and coal-dependent energy systems (India, Indonesia, South Africa, Vietnam, Philippines). They benefit from the sovereignty shield to prioritize energy access and industrialization over early peaking. Exit is constrained: leaving Paris isolates them from climate finance and technology cooperation, but staying lets them set their own pace. They are organized as negotiating blocs (LMDC, BASIC, G77+China).
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, emerging_major_emitters, beneficiary,
    organized, biographical, constrained, national).

% States and legal scholars who advance the doctrinal position that climate cooperation must not compromise national sovereignty over energy, resources, and development choices. Includes US (variable by administration), Brazil (variable), China (on sovereignty grounds), and many Global South states. They have arbitrage-grade exit: they can invoke sovereignty in any forum, shift between readings, and are not bound by a single interpretation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, state_sovereignty_advocates, beneficiary,
    institutional, generational, arbitrage, global).

% Small island states (AOSIS), least developed countries, and climate-exposed nations (Bangladesh, Philippines, Sahel states). They bear the physical impacts of inadequate global mitigation while having no leverage to compel stronger action under the sovereigntist reading. They are structurally excluded from shaping the constraint's operation. Their 'payment' is measured in existential risk, not financial transfer. They have no exit from climate impacts.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_states, payer,
    powerless, biographical, trapped, regional).

% EU and aligned states (Nordics, UK, Canada, NZ, climate-ambitious Latin American states) that push for stronger interpretation of the ratchet mechanism, transparency framework, and global stocktake. They set the procedural agenda within UNFCCC but cannot enforce outcomes. They experience the constraint analytically — as a framework they try to steer toward the supranational_reading.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, eu_climate_leadership_bloc, agenda_setter,
    institutional, generational, analytical, continental).

% The UNFCCC secretariat administers the NDC registry, global stocktake, and transparency reporting. It has no enforcement authority, no power to compel ambition, and no stake in outcomes — it services the process. Its situation is purely analytical: it observes whether the procedural machinery functions as designed.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, unfccc_secretariat, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal procedural framework for iterative national climate pledges — a common timeline, reporting format, and stocktake cycle that enables transparency, peer learning, and incremental ambition updating without surrendering national decision-making to an external authority.
% TRANSFER_FUNCTION: Moves diplomatic recognition and procedural legitimacy from the international community to states that submit and update NDCs. No financial or resource transfer is mandated by this reading; the transfer is symbolic (participation rights, good standing) rather than material. The equity_reading and supranational_reading each posit material transfers (finance, technology, binding targets) that this reading rejects.
% ABSENT_VOICES: Future generations and non-human nature are structurally excluded — they bear the cumulative consequences of the sovereignty-preserving structure but have no seat in the UNFCCC. Fossil fuel workers and communities in transition are also absent: the sovereignty shield protects incumbent industries but does not fund just transition. These voices would object to the lock-in of fossil trajectories but are not in the room.
% DISAPPEARANCE_RATIONALE: If the NDC framework vanished overnight, the global climate regime would lose its only universal participation mechanism. States would revert to fragmented bilateral/regional deals or no coordination. The transparency infrastructure (reporting, review, stocktake) would collapse. Fossil-dependent economies would lose the legitimacy cover the framework provides. Climate-vulnerable states would lose even the weak procedural leverage they have. The world would rearrange into a less transparent, less universal, possibly more conflictual climate politics.
% FOUNDING_PROBLEM: How to achieve universal participation in climate action without requiring states to surrender sovereignty over energy systems, development pathways, and natural resources — the Copenhagen 2009 failure proved that top-down binding targets could not secure universal buy-in.
% FOUNDING_PROBLEM_CORROBORATION: The sovereigntist coalition (fossil-dependent economies, emerging emitters, sovereignty advocates) attests the problem is live: sovereignty remains the price of participation. The supranational_reading coalition (EU, AOSIS, climate NGOs) attests the problem is solved in principle — the framework exists, now it must deliver outcomes — and that sovereignty is being weaponized as delay. The equity_reading coalition (G77+China, LMDC, LDCs) attests the problem is mischaracterized: it is not sovereignty vs. cooperation, but differentiated responsibility vs. false universalism. No single corroboration exists outside the benefiting parties; the founding problem is structurally contested.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(paris_article_4_ndc__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__sovereigntist_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__sovereigntist_reading_tests).
:- end_tests(paris_article_4_ndc__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is rope: a genuine coordination mechanism (iterative pledge-and-review) with minimal coercion, where participants are net beneficiaries relative to a no-agreement baseline. Extraction is low (0.08) because the constraint does not transfer resources from targets to beneficiaries — it leaves each state's energy choices intact. Suppression is low (0.12) because no enforcement machinery compels compliance; the stocktake creates transparency and reputational pressure only. Theater ratio (0.22) reflects growing performative compliance: states submit NDCs and participate in reviews while emissions trajectories diverge from pledges. Accessibility collapse is low (0.15) because alternatives (withdrawal, weaker pledges, non-participation) remain fully open. Resistance is moderate (0.35) because the constraint faces opposition from both the supranational_reading bloc (demanding binding targets) and the equity_reading bloc (demanding differentiated obligations) — but this resistance is political contestation, not resistance to extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the fossil-dependent economy seat, the constraint is a sovereignty shield — pure coordination benefit. From the climate-vulnerable state seat, the same constraint is a snare: it legitimizes inadequate action while blocking stronger frameworks. The engine computes this divergence from structural data (power, exit_options, spatial_scope). The equity_reading and supranational_reading are separate constraint stories with their own ε values and stakeholder structures, linked via network.affects_constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (fossil_dependent_economies, emerging_major_emitters, state_sovereignty_advocates) have institutional/organized power, biographical/generational horizons, and constrained-to-arbitrage exit — they can leave the agreement or weaken pledges with modest cost. Climate_vulnerable_states are powerless, trapped, with no exit from climate impacts — they are the implicit payers of the sovereignty preservation, though not formal victims under this reading (the constraint does not actively extract from them; it fails to protect them). The UNFCCC secretariat and EU bloc are analytical/institutional observers. Directionality derives from this structure: beneficiaries have low d (subsidized by the constraint's non-interference), vulnerable states have high d (bear costs without voice).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating global climate action without sacrificing national sovereignty) remains live and contested. The arrangement has not resolved its mandatrophy because the coordination function (limiting warming to 1.5–2°C) is failing while the sovereignty-preserving structure persists. The theater ratio rise signals drift toward piton: procedural compliance replaces substantive coordination. But the constraint is not yet a piton because the pledge-and-review cycle still functions as designed — states do update NDCs, the stocktake operates, and the process continues. The mandate has not atrophied; it is being stress-tested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commitment_kernel_reading_frame,
    'Is the Paris Article 4 NDC framework a single kernel with multiple readings, or are the ''readings'' distinct constraint systems?',
    'Analyze whether the three readings (sovereigntist, equity, supranational) instantiate structurally different constraints with different ε values, beneficiary/victim structures, and enforcement logics — or whether they are merely interpretive emphases on one constraint.',
    'If distinct constraints: each gets its own ε, stakeholders, and classification linked by network.affects_constraints. If one constraint: ε varies by measurement basis, violating ε-invariance and requiring decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_kernel_reading_frame, conceptual, 'Whether the kernel frame correctly identifies distinct constraints or a single constraint with measurement ambiguity.').

omega_variable(
    sovereigntist_extraction_underestimation,
    'Does the low ε (0.08) for this reading mask extraction that materializes through the equity_reading''s structural demands on the same states?',
    'Track whether fossil-dependent economies that invoke sovereignty to resist supranational targets simultaneously extract climate finance and technology transfer under the equity_reading — making the combined extraction higher than either reading alone.',
    'If the readings function as a coupled extraction system, the sovereigntist reading''s low ε is an artifact of analyzing one strand of a multi-stranded constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereigntist_extraction_underestimation, empirical, 'Whether the low extractiveness of this reading is an analytic artifact of decomposing a coupled constraint family.').

omega_variable(
    ndc_voluntariness_vs_ratchet_mechanism,
    'Is the NDC revision freedom (Article 4.3 ''ratchet'') structurally voluntary or does the global stocktake create de facto compliance pressure?',
    'Observe whether states that submit weaker successive NDCs face diplomatic, financial, or market consequences that function as enforcement without formal sanctions.',
    'If ratchet pressure is real, the constraint has higher suppression and extraction than this reading claims — moving it toward tangled_rope. If purely voluntary, rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ndc_voluntariness_vs_ratchet_mechanism, empirical, 'Whether the ''voluntary'' revision freedom is structurally maintained or eroded by soft enforcement.').

omega_variable(
    fossil_economy_beneficiary_capture,
    'Do fossil-dependent economies genuinely benefit from sovereignty preservation, or are they locked into declining asset trajectories that the sovereignty claim obscures?',
    'Compare long-term economic outcomes for states that maximized sovereignty flexibility vs. those that committed to early transition — measuring stranded asset exposure, technology lock-in, and competitiveness in decarbonizing markets.',
    'If sovereignty preservation accelerates economic decline for fossil economies, the beneficiary claim is false — the constraint extracts from its purported beneficiaries via stranded assets and missed transition rents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fossil_economy_beneficiary_capture, empirical, 'Whether the declared beneficiaries actually benefit or are harmed by the sovereignty-preserving structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__sovereigntist_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paris_ndc_sovereigntist_tr_t2015, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(paris_ndc_sovereigntist_tr_t2018, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2018, 0.14).
narrative_ontology:measurement(paris_ndc_sovereigntist_tr_t2021, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2021, 0.18).
narrative_ontology:measurement(paris_ndc_sovereigntist_tr_t2024, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement(paris_ndc_sovereigntist_tr_t2027, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2027, 0.25).
narrative_ontology:measurement(paris_ndc_sovereigntist_tr_t2030, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2030, 0.28).

% Extraction over time
narrative_ontology:measurement(paris_ndc_sovereigntist_be_t2015, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2015, 0.05).
narrative_ontology:measurement(paris_ndc_sovereigntist_be_t2018, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2018, 0.06).
narrative_ontology:measurement(paris_ndc_sovereigntist_be_t2021, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2021, 0.07).
narrative_ontology:measurement(paris_ndc_sovereigntist_be_t2024, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2024, 0.08).
narrative_ontology:measurement(paris_ndc_sovereigntist_be_t2027, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2027, 0.08).
narrative_ontology:measurement(paris_ndc_sovereigntist_be_t2030, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2030, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(paris_ndc_sovereigntist_su_t2015, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2015, 0.05).
narrative_ontology:measurement(paris_ndc_sovereigntist_su_t2018, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2018, 0.07).
narrative_ontology:measurement(paris_ndc_sovereigntist_su_t2021, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2021, 0.09).
narrative_ontology:measurement(paris_ndc_sovereigntist_su_t2024, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2024, 0.12).
narrative_ontology:measurement(paris_ndc_sovereigntist_su_t2027, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2027, 0.14).
narrative_ontology:measurement(paris_ndc_sovereigntist_su_t2030, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2030, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__sovereigntist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__sovereigntist_reading, 0.02).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__equity_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_6_cooperative_approaches).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_13_transparency_framework).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the paris_article_4_ndc kernel. The sovereigntist reading (this file) treats NDCs as voluntary sovereignty-preserving pledges (rope, ε≈0.08). The equity_reading treats NDCs as differentiated obligations requiring structural transfer (tangled_rope, ε≈0.35). The supranational_reading treats NDCs as binding ratchet commitments (tangled_rope or snare, ε≈0.55). The three stories form a constraint family linked by affects_constraints. The upstream constraint (this reading's voluntary framework) enables the downstream readings' claims by providing the procedural substrate they interpret.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__sovereigntist_reading, institutional, 0.15).
constraint_indexing:directionality_override(paris_article_4_ndc__sovereigntist_reading, organized, 0.2).
constraint_indexing:directionality_override(paris_article_4_ndc__sovereigntist_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
