% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Commerce Clause — Originalist Narrow Reading (Border-Crossing Trade Only)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This story instantiates the originalist narrow reading of the Commerce
 *   Clause kernel: federal power under Article I, Section 8 extends only to
 *   trade that literally crosses state lines or to the instrumentalities and
 *   channels of interstate movement (railroads, navigable waterways,
 *   interstate carriers), not to intrastate economic activity regardless of
 *   its aggregate national effect. This reading was ascendant pre-1937
 *   (Lochner-era, Hammer v. Dagenhart, Carter Coal), suffered near-total
 *   eclipse during the New Deal settlement (1937-1995), and has been
 *   partially revived since United States v. Lopez (1995) and United States
 *   v. Morrison (2000) as a live doctrinal position, though it has never
 *   fully displaced the substantial-effects framework in practice. The
 *   claimed type (tangled_rope) reflects that this reading genuinely serves a
 *   coordination function — preserving federalism's check on centralized
 *   power — while simultaneously producing asymmetric extraction: state
 *   governments and locally dominant industry capture the benefit of
 *   regulatory arbitrage, while cross-border externality bearers and less
 *   mobile workers absorb the cost, with no textual mechanism internal to the
 *   reading itself for those externalities to be addressed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.42).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.38).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Commerce Clause — Originalist Narrow Reading (Border-Crossing Trade Only)").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, '257332d1-a683-4a13-a64a-961996c576a9').
narrative_ontology:cs_kernel_codification('257332d1-a683-4a13-a64a-961996c576a9', fixed_text).
narrative_ontology:cs_authority_grounding('257332d1-a683-4a13-a64a-961996c576a9', lineage).
narrative_ontology:cs_interpretation_layer_present('257332d1-a683-4a13-a64a-961996c576a9').
narrative_ontology:cs_reading_relation('257332d1-a683-4a13-a64a-961996c576a9', commerce_clause_text__expansive_federal_reading, forecloses).
narrative_ontology:cs_reading_relation('257332d1-a683-4a13-a64a-961996c576a9', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('257332d1-a683-4a13-a64a-961996c576a9', foundational, commerce_power_confined_to_border_crossing_trade).
narrative_ontology:cs_axiom_status(commerce_power_confined_to_border_crossing_trade, holdable).
narrative_ontology:cs_axiom_grounding('257332d1-a683-4a13-a64a-961996c576a9', commerce_power_confined_to_border_crossing_trade, conventional).
narrative_ontology:cs_axiom('257332d1-a683-4a13-a64a-961996c576a9', foundational, residual_police_power_belongs_exclusively_to_states).
narrative_ontology:cs_axiom_status(residual_police_power_belongs_exclusively_to_states, holdable).
narrative_ontology:cs_axiom_grounding('257332d1-a683-4a13-a64a-961996c576a9', residual_police_power_belongs_exclusively_to_states, deontological).
narrative_ontology:cs_reference_frame('257332d1-a683-4a13-a64a-961996c576a9', dual_sovereignty_enumerated_powers_framework).
narrative_ontology:cs_drift_state('257332d1-a683-4a13-a64a-961996c576a9', post_new_deal_integrated_national_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('257332d1-a683-4a13-a64a-961996c576a9', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, local_industry_shielded_from_federal_regulation).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, interstate_externality_bearers).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, national_standard_seeking_industries).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, workers_in_states_with_weak_labor_protections).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, consumers_facing_fragmented_regulatory_protection).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, dual_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, enumerated_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain regulatory authority over intrastate labor, manufacturing, agriculture, and commerce under this reading. Can set lower environmental, wage, or safety standards than neighboring states to attract business, and litigate to strike down federal statutes that reach beyond literal border-crossing transactions. Benefits directly from every judicial narrowing of federal reach.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, state_governments, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__originalist_narrow_reading, state_governments, agenda_setter).

% Ideological and litigation-funding coalition that treats federal regulatory expansion as illegitimate consolidation of power. Files amicus briefs, funds test cases, and uses the narrow reading as a wedge to roll back New Deal-era and later federal statutes. Bears no direct cost from the reading's operation; collects legitimacy and precedent.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates, beneficiary,
    organized, civilizational, mobile, national).

% Manufacturers, agricultural operations, and employers whose production and labor practices are formally intrastate escape federal wage, safety, and environmental floors. Can relocate operations to jurisdictions with the most permissive intrastate regime, using the fragmented regulatory map as a competitive tool against firms in stricter states.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, local_industry_shielded_from_federal_regulation, beneficiary,
    powerful, biographical, mobile, regional).

% Residents of downstream or downwind states who absorb pollution, wage suppression spillover, or unsafe-product risk generated by intrastate activity in a neighboring state that federal regulation cannot reach under this reading. Have no vote in the originating state's legislature and no federal remedy because the underlying activity is classified as beyond the commerce power's border-crossing scope.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, interstate_externality_bearers, payer,
    powerless, biographical, trapped, regional).

% Firms that want uniform national rules (single compliance regime, predictable competition) instead of fifty divergent intrastate regimes. Must comply with a patchwork of state law for functionally identical activity, raising compliance cost and creating arbitrage opportunities for competitors who relocate to lax jurisdictions.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, national_standard_seeking_industries, payer,
    powerful, biographical, constrained, national).

% Employed in intrastate manufacturing or agriculture activity federal wage-and-hour, safety, or organizing protections cannot reach under this reading because the activity is not itself border-crossing trade. Exit means relocating households, which is costly and often not viable; remain subject to whatever floor the state legislature sets, which is shaped by the same firms benefiting from the narrow reading.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, workers_in_states_with_weak_labor_protections, payer,
    powerless, biographical, trapped, regional).

% Purchase goods and services produced under whatever intrastate standard applied at the point of manufacture, without a federal floor for safety or disclosure where production is formally intrastate. Cannot easily verify or select against weak-regulation-origin goods in a national marketplace.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, consumers_facing_fragmented_regulatory_protection, payer,
    powerless, biographical, constrained, national).

% Determines, case by case, whether a given economic activity counts as 'crossing state borders' or as a mere 'instrumentality' of interstate movement versus purely intrastate activity beyond federal reach. This line-drawing is the actual mechanism through which the reading is enforced or relaxed.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Study the historical record of founding-era trade practices, debate whether 'commerce' was originally understood narrowly or broadly, and assess whether the narrow reading reflects genuine originalist recovery or motivated construction serving contemporary deregulatory goals.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a genuine zone of state police-power autonomy — allowing states to legislate on local matters (zoning, most criminal law, intrastate labor and business conditions) without federal preemption, consistent with a system of dual sovereignty designed to prevent concentration of regulatory power in one national government.
% TRANSFER_FUNCTION: Moves regulatory authority and the costs of externalities generated by intrastate economic activity away from federal oversight and onto state legislatures (often captured by local industry) and onto residents of other states and less powerful in-state parties who cannot reach the activity through federal channels.
% ABSENT_VOICES: Interstate externality bearers — residents of downwind or downstream states harmed by another state's intrastate activity — have no vote in the state generating the harm and, under this reading, no federal forum either. Workers in weak-protection states are structurally underrepresented in the state legislative process that sets their floor.
% DISAPPEARANCE_RATIONALE: If this reading's confinement of federal commerce power to literal border-crossing transactions vanished (i.e., a maximally expansive reading took over entirely), state legislatures would lose leverage over intrastate industry, federal wage/safety/environmental floors would displace the current patchwork, and the competitive advantage certain states currently extend to industry via looser regulation would collapse — a substantial rearrangement of federal-state power distribution and industrial location incentives.
% FOUNDING_PROBLEM: At the framing era, the states feared that an unconfined federal commerce power would collapse state sovereignty entirely, replicating the centralization the Revolution had rejected; the narrow reading was built to keep national power confined to genuinely cross-border trade friction (tariffs, currency chaos, interstate trade wars) that state governments could not solve individually.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and state attorneys general attest the founding problem (checking centralized federal overreach) remains live and structurally identical today. Constitutional historians outside the states'-rights litigation apparatus and federal regulatory agencies attest that the modern integrated national economy has made the originating border/non-border distinction largely artificial — economic activity once local now routinely generates the exact cross-border spillovers the Commerce Clause was written to address, meaning the doctrine's textual anchor has drifted from its functional purpose even as its formal justification persists unchanged.
narrative_ontology:disappearance_verdict(commerce_clause_text__originalist_narrow_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__originalist_narrow_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__originalist_narrow_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).
:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at moderate (0.42) rather than high because the reading's core coordination function (preventing one level of government from swallowing the other) is real and not merely pretextual — this is not a pure snare. But it is not a clean rope either: the same doctrinal line that protects state autonomy over genuinely local matters also insulates powerful in-state industry from federal floors it would otherwise be subject to, and it does so at a cost to parties (downstream states, mobile workers, national-standard-seeking firms) who have no voice in the state legislative processes that set the floor. Suppression (0.38) reflects that the reading is enforced through judicial doctrine (stare decisis, precedent, the political question of appointments) rather than raw coercion, but it is actively defended in courts and confirmation battles. Resistance is high (0.72) because this reading has never gone unchallenged; it was the losing position for six decades and remains a minority current within the judiciary even after Lopez/Morrison. Theater ratio (0.28, rising slightly since 1995) reflects the increasing use of 'commercial activity' / 'non-commercial activity' line-drawing as a doctrinal proxy that does less analytical work than it purports to as courts extend the reasoning into contested territory.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and locally entrenched industry are structural beneficiaries: the narrower federal reach is, the more regulatory discretion (and the competitive advantage that discretion enables) accrues to them, with essentially no offsetting cost imposed on them by the reading itself. Interstate externality bearers, workers in weak-protection states, and national-standard-seeking industries are structural targets: costs generated by activity classified as purely intrastate land on them without a federal channel for redress, and their exit options are trapped or constrained (a resident cannot easily relocate away from cross-border pollution; a national firm cannot escape a fifty-state patchwork by choosing a different market). The federal judiciary is the actual mechanism (agenda_setter) whose case-by-case line-drawing between 'crossing borders' and 'purely local' determines where the reading's boundary falls in any given dispute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — checking runaway federal centralization — was live and urgent at the framing and remains a defensible ongoing concern in the abstract. But the specific textual test this reading uses (does the activity literally cross a border) has drifted from tracking that concern as the national economy has become genuinely integrated: activity that is formally intrastate (e.g., wheat grown and consumed on one farm, per Wickard) now routinely has real cross-border effects the original test was not built to see. Classifying this as tangled_rope rather than snare acknowledges the coordination function is not a pretext, while classifying it as tangled_rope rather than rope acknowledges that the reading's operation, as currently practiced, distributes the founding problem's benefits and its avoided costs asymmetrically across state and non-state actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_recovery_or_motivated_construction,
    'Does the narrow border-crossing reading accurately recover the founding-era understanding of ''commerce among the several states,'' or is it a modern doctrinal construction retrofitted onto originalist rhetoric to serve contemporary deregulatory goals?',
    'Historical linguistic and legal-practice analysis of how ''commerce'' was used in ratification-era debates, state trade practices, and early Congressional and judicial commerce power exercises (e.g., Gibbons v. Ogden) compared against the modern narrow doctrine''s actual boundary lines.',
    'If the narrow reading is a genuine historical recovery, its coordination-function claim (checking federal overreach as originally intended) is strengthened and the tangled_rope classification''s coordination half is more solidly grounded. If it is substantially a modern construction, the reading functions closer to a snare wearing originalist historical cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalist_recovery_or_motivated_construction, conceptual, 'Whether the narrow reading is authentic originalism or motivated doctrinal construction.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly does the disagreement between the three commerce-clause readings live: is it a disagreement about what ''commerce'' meant at ratification (historical-semantic), about what test best serves federalism values today (normative-functional), or about how much judicial deference courts owe Congress''s own commerce findings (institutional-procedural)?',
    'Structural analysis of the case law reasoning across Lopez, Morrison, Raich, and NFIB v. Sebelius to isolate whether courts adopting different readings are disputing historical fact, contemporary values, or institutional competence.',
    'If the disagreement is primarily institutional-procedural (how much deference Congress gets), the narrow reading and substantial-effects-limited reading may be closer to convergent than the naming suggests, with the true fault line being expansive_federal_reading versus the other two on deference alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Locating the actual axis of disagreement among the three sibling readings of the commerce clause kernel.').

omega_variable(
    state_capture_of_police_power_benefit,
    'Is the coordination benefit of preserved state police power actually captured broadly by state residents, or is it substantially captured by industry actors who lobby state legislatures more effectively than federal ones?',
    'Comparative regulatory-capture analysis: compare industry influence over state legislative rulemaking on labor/environmental standards versus influence over equivalent federal rulemaking (e.g., OSHA, EPA) in the same sectors.',
    'If state-level capture is systematically higher, the reading''s coordination benefit (state autonomy) is substantially diverted to the local_industry beneficiary group rather than broadly distributed to state residents, sharpening the tangled_rope classification toward the extraction pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capture_of_police_power_benefit, empirical, 'Whether the federalism coordination benefit accrues broadly to state residents or narrowly to locally dominant industry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 1937, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1937, 0.2).
narrative_ontology:measurement_basis(comm_tr_t1937, observed).
narrative_ontology:measurement(comm_tr_t1955, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1955, 0.22).
narrative_ontology:measurement_basis(comm_tr_t1955, observed).
narrative_ontology:measurement(comm_tr_t1975, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement_basis(comm_tr_t1975, observed).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement_basis(comm_tr_t1995, observed).
narrative_ontology:measurement(comm_tr_t2010, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2010, 0.26).
narrative_ontology:measurement_basis(comm_tr_t2010, observed).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(comm_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1937, 0.55).
narrative_ontology:measurement_basis(comm_be_t1937, observed).
narrative_ontology:measurement(comm_be_t1955, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1955, 0.5).
narrative_ontology:measurement_basis(comm_be_t1955, observed).
narrative_ontology:measurement(comm_be_t1975, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1975, 0.35).
narrative_ontology:measurement_basis(comm_be_t1975, observed).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement_basis(comm_be_t1995, observed).
narrative_ontology:measurement(comm_be_t2010, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement_basis(comm_be_t2010, observed).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(comm_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(commerce_clause_text__originalist_narrow_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__originalist_narrow_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% Part of the commerce_clause_text kernel family (3 stories). This story (originalist_narrow_reading) authors ε=0.42 for the narrow border-crossing standing arrangement as this reading's own tradition sees it — genuine but asymmetrically distributed federalism coordination. The sibling expansive_federal_reading authors a different ε for the aggregate-effects arrangement it defends; substantial_effects_limited_reading authors a third ε for the nexus-constrained middle position. Each story's beneficiary/victim sets differ structurally (this reading benefits states and local industry; expansive_federal_reading would benefit national-standard beneficiaries and externality bearers instead). Link direction: this reading's judicial revival (post-1995) exerts downstream pressure on how courts apply the substantial_effects_limited_reading's nexus test, and directly forecloses application of expansive_federal_reading's aggregate test within any single court adopting this reading's core premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
