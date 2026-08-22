% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__endogenous_climb_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: imposition_pathway_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Climb: Fringe-Adoption-to-Decree Commitment Displacement
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The Meiji state's calendar and dress decrees (1872–1873) appear as
 *   top-down impositions of new commitment standards on a unified territory.
 *   The endogenous-climb reading reframes them as state ratifications of an
 *   already-underway transformation. Foreign contact initiated fringe
 *   adoption (treaty ports, military units, diplomatic households, merchant
 *   elites) decades before formal decree. The state decree accelerates the
 *   climb from fringe to mass, but the climb was already underway with
 *   invisible early stages. The constraint is not state imposition but the
 *   compression-and-enforcement of a pre-existing fringe adoption pathway.
 *
 * KEY AGENTS:
 *   - Modernizing coalition: Military modernizers, merchant elites in treaty ports, reformers pre-adopting new calendar and dress
 *   - Treaty-port merchant networks: Foreign-contact communities actively adopting and lobbying for state ratification
 *   - State administrative apparatus: Issues decree as ratification and enforcement of ongoing climb
 *   - Traditionalist rural population: Experiences decree as sudden imposition; bears compliance costs
 *   - Bureaucratic incumbents: Mid-rank officials whose authority rested on old temporal/sartorial orders
 *   - Foreign diplomatic corps: Initiates fringe desirability through presence; observes ratification
 *   - Historical observers: Adjudicates whether fringe stages were hidden or absent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__endogenous_climb_reading, 0.42).
domain_priors:suppression_score(imposition_pathway_kernel__endogenous_climb_reading, 0.31).
domain_priors:theater_ratio(imposition_pathway_kernel__endogenous_climb_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__endogenous_climb_reading, "Endogenous Climb: Fringe-Adoption-to-Decree Commitment Displacement").
narrative_ontology:topic_domain(imposition_pathway_kernel__endogenous_climb_reading, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__endogenous_climb_reading, '38a189a1-4900-4c4a-b853-2f4c037cda91').
narrative_ontology:cs_kernel_codification('38a189a1-4900-4c4a-b853-2f4c037cda91', distributed).
narrative_ontology:cs_authority_grounding('38a189a1-4900-4c4a-b853-2f4c037cda91', lineage).
narrative_ontology:cs_interpretation_layer_present('38a189a1-4900-4c4a-b853-2f4c037cda91').
narrative_ontology:cs_reading_relation('38a189a1-4900-4c4a-b853-2f4c037cda91', imposition_pathway_kernel__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('38a189a1-4900-4c4a-b853-2f4c037cda91', imposition_pathway_kernel__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('38a189a1-4900-4c4a-b853-2f4c037cda91', foundational, fringe_adoption_precedes_state_mandate).
narrative_ontology:cs_axiom_status(fringe_adoption_precedes_state_mandate, holdable).
narrative_ontology:cs_axiom_grounding('38a189a1-4900-4c4a-b853-2f4c037cda91', fringe_adoption_precedes_state_mandate, empirically_contingent).
narrative_ontology:cs_axiom('38a189a1-4900-4c4a-b853-2f4c037cda91', foundational, state_enforcement_amplifies_not_initiates).
narrative_ontology:cs_axiom_status(state_enforcement_amplifies_not_initiates, holdable).
narrative_ontology:cs_axiom_grounding('38a189a1-4900-4c4a-b853-2f4c037cda91', state_enforcement_amplifies_not_initiates, deontological).
narrative_ontology:cs_reference_frame('38a189a1-4900-4c4a-b853-2f4c037cda91', organic_fringe_adoption_baseline).
narrative_ontology:cs_drift_state('38a189a1-4900-4c4a-b853-2f4c037cda91', post_decree_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('38a189a1-4900-4c4a-b853-2f4c037cda91', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, modernizing_coalition).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, treaty_port_merchant_networks).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, traditionalist_rural_population).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, bureaucratic_incumbents).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__endogenous_climb_reading, state_capacity_amplifies_not_initiates).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__endogenous_climb_reading, cultural_norms_descend_from_fringe).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Military modernizers, merchant elites in treaty ports, bureaucratic reformers who pre-adopted the new calendar and dress styles before state decree. They benefit from coordination around a shared temporal and sartorial standard that aligns domestic practice with international commerce and military interoperability. The decree retrospectively legitimizes what they were already doing.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, modernizing_coalition, beneficiary,
    organized, generational, mobile, national).

% Foreign-contact merchant communities in treaty ports (Yokohama, Shanghai, etc.) who adopted Gregorian calendar and Western dress from contact with foreign traders and diplomats. They maintain pressure on the state to standardize around their practices and actively lobby for decree-backed enforcement. They set the agenda for what counts as 'modern' within merchant and military circles.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, treaty_port_merchant_networks, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__endogenous_climb_reading, treaty_port_merchant_networks, agenda_setter).

% Issues the formal decree mandating calendar and dress change (e.g., 1873 calendar reform, 1872 hair ordinance, Meiji-era sumptuary edicts). The state's authority derives from ratifying an already-climbing commitment, not from generating it de novo. Enforcement machinery exists but primarily suppresses non-compliance among the reluctant rural majority who did not participate in pre-decree adoption.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Rural farmers, craftspeople, and village administrators who maintained lunar calendar, traditional dress, and local temporal markers for generations. They experience the decree as a sudden top-down imposition from an invisible fringe they never joined. Compliance costs are real: reckoning harvests in unfamiliar calendar, purchasing new clothing, adjusting work rhythms. Their resistance is high but uncoordinated; they cannot exit the administrative jurisdiction.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, traditionalist_rural_population, payer,
    powerless, biographical, trapped, local).

% Mid-rank officials and local governors whose authority and expertise rested on mastery of the old temporal and sartorial orders. The decree devalues their accumulated knowledge and forces rapid re-training. They must enforce compliance they did not author, creating resentment. Their exit options are limited: they cannot easily leave office without career consequences.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, bureaucratic_incumbents, payer,
    moderate, biographical, constrained, regional).

% Foreign diplomats and merchants whose practices and temporal standards the fringe coalition adopted. They do not directly enforce or author the decree, but their existence created the fringe desirability in the first place. They observe the state's ratification of a climb they initiated through presence.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, foreign_diplomatic_corps, observer,
    powerful, biographical, arbitrage, global).

% Historians and sociologists who study whether commitment changes arise from top-down state imposition or bottom-up fringe adoption, and whether the Meiji case shows hidden fringe stages before apparent decrees. They gather testimony, archival evidence of pre-decree adoption, and analyze enforcement patterns to adjudicate the mechanism.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__endogenous_climb_reading, treaty_port_merchant_networks).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solve the commitment problem of having a unified temporal and sartorial reference system across a heterogeneous territory undergoing rapid international contact. The fringe coalition needs state ratification to make the climb binding; the state needs the fringe's prior adoption to make enforcement credible and minimize cost.
% TRANSFER_FUNCTION: Moves temporal and sartorial authority from local, lunar, and traditional systems to a centralized Gregorian and Western-dress standard. The transfer flows upward from communities that never adopted the fringe practice to the state apparatus that enforces it, and sideways from the fringe-adopting modernizers (who maintain status as the exemplars of correct practice) to the rural and traditionalist payers who bear compliance costs.
% ABSENT_VOICES: Rural populations and traditionalist bureaucrats experience this as top-down imposition and were largely absent from the fringe adoption phase. Their objections to the decree — that it came too fast, that compliance is costly, that it erodes local authority and knowledge — would be louder if they had been present in the merchant circles and military reformer networks where the climb began. The reading's own mechanism renders them structurally absent.
% DISAPPEARANCE_RATIONALE: If the state had not issued the decree, the fringe adoption would have continued to climb gradually. Major cities, military units, merchant houses, and government offices would have shifted calendar and dress over decades. Rural areas would have resisted longer or maintained parallel systems. Without the decree, the climb would take longer, remain more contested, and lack a single binding moment. The constraint's disappearance removes the moment of enforcement-backed standardization; the world rearranges into a slower, more geographically stratified adoption curve.
% FOUNDING_PROBLEM: Japan experienced rapid international contact (1853 onward) without a unified internal commitment to temporal and sartorial standards. Foreign diplomacy and military modernization required interoperability with Gregorian calendar and Western dress. Fringe populations (treaty ports, military, modernizers) adopted these practices first. The state faced a coordination choice: leave adoption gradual and geographically fragmented, or ratify the climb and enforce uniformity.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Meiji modernization (e.g., Marius Jansen, Tessa Morris-Suzuki) attest that fringe adoption preceded state decree and that enforcement was concentrated on regions that had NOT already adopted — Japan's consulates and military units report pre-decree calendar and dress use. Foreign traders' letters document calendar confusion until standardization. Rural tax records show resistance concentrated in areas that had not been exposed to treaty-port networks. These sources lie outside the benefiting coalition (the state and modernizers claim the decree was necessary to create uniformity; external historical witnesses document the pre-decree climb).
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__endogenous_climb_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply at 1873 (decree date: 0.28 → 0.52) because enforcement machinery mobilizes to suppress non-compliance in the uncoordinated traditionalist majority. It then declines (0.52 → 0.38 by 1900) as compliance becomes normalized and the need for active suppression falls — this is the characteristic post-climb arc. Theater peaks at decree (0.35) because state propaganda emphasizes the decree's novelty and necessity; the peak reflects the compressed moment when enforcement apparatus must convince the unwilling masses. Theater declines as the practice becomes naturalized and stops requiring justification. Suppression follows the same arc: high at decree (0.48) to force compliance among the resistant, declining as voluntary adoption spreads. The measurements are authored at the interval start/end and at historical inflection points (fringe adoption phases before decree, the decree event itself, post-decree normalization).
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, the decree appears as a necessary top-down imposition: Japan required unified standards for military and diplomatic interoperability, and the rural majority would never have adopted them voluntarily. From the fringe-adopter seats (merchant, military), the decree is a welcome ratification of a climb already underway — they benefit from the binding enforcement. From the rural/traditionalist seats, the decree is a sudden imposition from an invisible coalition that had already decided the outcome. The engine computes these divergences from the directionality data (state: d ≈ 0.3, fringe beneficiary: d ≈ 0.2, rural payer: d ≈ 0.85) without reconciling them; the classification divergence IS the measurement the reading provides.
 *
 * DIRECTIONALITY LOGIC:
 *   Modernizing coalition and treaty-port merchants benefit from the climb's acceleration and the enforcement machinery that binds the reluctant majority — low directionality (d ≈ 0.2–0.3), beneficiary seats. The state apparatus is moderately extractive (d ≈ 0.4) because it collects administrative authority from the standardization (ability to enforce uniform practices, tax records, mobilization) but also bears costs of enforcement and coordination. Traditionalist rural populations and displaced bureaucrats are high-d targets (d ≈ 0.75–0.85): they bear compliance costs, lose local autonomy, and have trapped exit (cannot leave jurisdiction or revert to old standards without state punishment). Directionality overrides are not needed; the beneficiary/victim declarations and exit options produce the right gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Japan needs unified commitment standards for military and diplomatic interoperability) is LIVE, not dead. The decree solved a real coordination problem: without unified calendar and dress, military units could not coordinate, diplomatic protocol was confused, and international commerce required repeated conversions. BUT the reading asserts that the problem was already being solved by fringe adoption — the state did not CREATE the solution, it ACCELERATED and ENFORCED it. This prevents misclassifying the decree as pure coordination (Rope) when it also extracts from the resistant majority. The Tangled Rope type captures the duality: genuine coordination function (unified standards) plus asymmetric extraction (fringe beneficiaries, rural payers, enforcement costs borne by the unwilling).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_adoption_observability,
    'How much fringe adoption must be archaeologically visible before the reading''s mechanism (pre-decree climb) is confirmed versus a competing reading''s mechanism (state-initiated imposition with retroactive fringe rationalization)?',
    'Archival evidence of calendar/dress use in treaty-port documents, military records, and merchant correspondence dated before decree. Cross-referencing with rural resistance timelines: if resistance spike coincides with decree (not pre-decree), fringe adoption was prior; if resistance predates decree, fringe adoption was recognized earlier.',
    'High visibility of pre-decree fringe adoption strengthens the endogenous-climb reading and marginalizes exogenous-override. Low visibility shifts weight toward hybrid-cascade (state artificially creates fringe through military/bureaucratic mandate, then organic climb proceeds from the artificial base).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_adoption_observability, empirical, 'Whether pre-decree fringe adoption is materially present in the historical record or retroactively constructed.').

omega_variable(
    state_ratification_necessity,
    'If the state had NOT issued the decree, would fringe adoption have climbed to near-universal compliance on its own timeline? Or would it have plateaued at a regional/class-based equilibrium?',
    'Comparative historical analysis: regions that resisted decree most strongly (timing/intensity of suppression needed) vs. regions that adopted willingly. If high-resistance regions show long-term adoption after decree withdrawal or weakening (suggesting endogenous climb was always operating), fringe-only climb is plausible. If adoption stalls where enforcement stops, the decree was necessary — favoring hybrid-cascade or exogenous-override readings.',
    'If fringe climb was self-sustaining, the reading is strengthened: state ratified an inevitable climb. If enforcement stopped climb, the reading is weakened; hybrid or exogenous mechanisms better explain the observed outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_ratification_necessity, conceptual, 'Whether state enforcement was ratification of inevitable climb or acceleration of otherwise-stalling adoption.').

omega_variable(
    definition_of_fringe,
    'Does the fringe category include ONLY organic pre-decree adoption (merchants, military reformers, foreign-contact populations), or does it include the state''s own early administrative units and their enforced adoption as pseudo-organic?',
    'Genealogy of state units issuing calendar/dress orders before the formal decree (if any): did central government issue local decrees that cascaded? If so, the state was already using enforcement on subordinate units before ratifying it empire-wide — the cascade reading''s mechanism. If decrees emerged only after fringe adoption was visible, the reading holds.',
    'If state used enforcement pre-decree on its own apparatus, the distinction between endogenous-climb and hybrid-cascade collapses; both readings'' core premises become incoherent (state cannot simultaneously initiate and ratify). This omega forces precise definition of ''fringe'' vs. ''state apparatus'' as distinct adoption pathways.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_fringe, conceptual, 'Whether the fringe is defined tightly (organic, non-state populations) or broadly (any early adopter, including state instrumentalities).').

omega_variable(
    alternative_reading_presence,
    'Are the sibling readings (exogenous_override, hybrid_cascade) live positions held by competing historiographical schools, or are they strawmen constructed to test the coherence of the endogenous-climb reading?',
    'Bibliography audit: do mainstream historians of Meiji explicitly argue for top-down state imposition WITHOUT fringe precondition (exogenous), or for state-initiated artificial fringe with cascading climb (hybrid)? Or does the consensus acknowledge fringe adoption but debate its causal necessity for the decree?',
    'If sibling readings are strawmen, the kernel is not genuinely contested — it''s a reading exercise without real historiographical stakes. If siblings are live positions with named defenders, the kernel represents a genuine scholarly dispute and the reading system is adjudicating a real problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_presence, empirical, 'Whether the sibling readings represent live historiographical alternatives or are artifacts of the decomposition exercise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__endogenous_climb_reading, 1850, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t1850, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1850, 0.08).
narrative_ontology:measurement_basis(impo_tr_t1850, observed).
narrative_ontology:measurement(impo_tr_t1860, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1860, 0.14).
narrative_ontology:measurement_basis(impo_tr_t1860, observed).
narrative_ontology:measurement(impo_tr_t1870, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1870, 0.18).
narrative_ontology:measurement_basis(impo_tr_t1870, observed).
narrative_ontology:measurement(impo_tr_t1873, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1873, 0.35).
narrative_ontology:measurement_basis(impo_tr_t1873, observed).
narrative_ontology:measurement(impo_tr_t1880, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1880, 0.28).
narrative_ontology:measurement_basis(impo_tr_t1880, observed).
narrative_ontology:measurement(impo_tr_t1890, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1890, 0.22).
narrative_ontology:measurement_basis(impo_tr_t1890, observed).
narrative_ontology:measurement(impo_tr_t1900, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement_basis(impo_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t1850, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1850, 0.05).
narrative_ontology:measurement_basis(impo_be_t1850, observed).
narrative_ontology:measurement(impo_be_t1860, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1860, 0.12).
narrative_ontology:measurement_basis(impo_be_t1860, observed).
narrative_ontology:measurement(impo_be_t1870, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1870, 0.28).
narrative_ontology:measurement_basis(impo_be_t1870, observed).
narrative_ontology:measurement(impo_be_t1873, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1873, 0.52).
narrative_ontology:measurement_basis(impo_be_t1873, observed).
narrative_ontology:measurement(impo_be_t1880, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1880, 0.48).
narrative_ontology:measurement_basis(impo_be_t1880, observed).
narrative_ontology:measurement(impo_be_t1890, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1890, 0.42).
narrative_ontology:measurement_basis(impo_be_t1890, observed).
narrative_ontology:measurement(impo_be_t1900, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement_basis(impo_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t1850, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1850, 0.05).
narrative_ontology:measurement_basis(impo_su_t1850, observed).
narrative_ontology:measurement(impo_su_t1860, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1860, 0.12).
narrative_ontology:measurement_basis(impo_su_t1860, observed).
narrative_ontology:measurement(impo_su_t1870, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1870, 0.22).
narrative_ontology:measurement_basis(impo_su_t1870, observed).
narrative_ontology:measurement(impo_su_t1873, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1873, 0.48).
narrative_ontology:measurement_basis(impo_su_t1873, observed).
narrative_ontology:measurement(impo_su_t1880, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1880, 0.38).
narrative_ontology:measurement_basis(impo_su_t1880, observed).
narrative_ontology:measurement(impo_su_t1890, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1890, 0.32).
narrative_ontology:measurement_basis(impo_su_t1890, observed).
narrative_ontology:measurement(impo_su_t1900, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1900, 0.28).
narrative_ontology:measurement_basis(impo_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__endogenous_climb_reading, 0.12).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested imposition_pathway_kernel. All three readings (endogenous_climb, exogenous_override, hybrid_cascade) share the same referent — Meiji calendar/dress changes — but differ structurally in what mechanism produced them and when state authority became necessary. The readings do not coexist within a single framework; they are held by competing historiographical schools. Each reading emits a different ε, different beneficiary/victim structure, and different type. They are linked as a family via network.affects_constraints; see the sibling files for the other readings' omegas and structural data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_pathway_kernel__endogenous_climb_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
