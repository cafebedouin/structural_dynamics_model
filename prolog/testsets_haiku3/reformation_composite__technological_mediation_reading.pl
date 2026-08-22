% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Printing Press as Technological Constraint on Reformation Propagation
 *   domain: historical_epistemology/technological_mediation
 *
 * SUMMARY:
 *   This constraint instantiates the TECHNOLOGICAL_MEDIATION_READING of the
 *   contested Reformation kernel. The core claim: the printing press is a
 *   physical/natural constraint (mountain) that transformed local theological
 *   dissent into a continental mass movement by enabling the reproduction and
 *   distribution of reformed doctrine at scale previously impossible. The
 *   referent for epsilon is the standing arrangement of information
 *   distribution in 15th-century Europe assessed by this reading's epistemic
 *   lights — not the reading's endorsed alternative (unlimited print access),
 *   but the existing constraint that the press operates within. Publication
 *   rates (print runs, titles per year, geographic dispersion) and literacy
 *   distribution (regional variation, class stratification) are the primary
 *   observables. The reading brackets the theological questions
 *   (fragmentation) and political questions (nation-state sovereignty claims)
 *   as secondary or contingent — the technological constraint is presented as
 *   the enabling structure. This claim is independent of whether theology or
 *   politics *also* mattered; the reading asserts technological mediation as
 *   structurally prior to mass propagation. The other two sibling readings
 *   (theological_fragmentation, political_realignment) instantiate competing
 *   framings of the same historical event; none forecloses the others when
 *   held by different scholarly communities, but they produce different ε and
 *   different classification profiles when instantiated as separate
 *   constraints.
 *
 * KEY AGENTS:
 *   - printing_press_operators: The craftspeople, merchants, and investors who operated the press technology, determined publication runs and titles, priced access, and thus shaped what could be distributed at scale.
 *   - literate_urban_populations: Merchants, clergy, educated urban dwellers (1.5–3% of early-16th-century European population) who could read printed texts directly.
 *   - reformed_doctrine_advocates: Theologians, preachers, and activists whose dissent was amplified by print access — local critique became continental movement only via reproduction at scale.
 *   - oral_transmission_vectors: Preachers, visual artists, theater troupes, hymn-singers — they carried reformed ideas to populations outside the literate stratum; this reading brackets their role as secondary.
 *   - information_system_as_actor: The printing press itself, treated as a structural constraint, not an intentional agent.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.15).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.08).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, mountain).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Printing Press as Technological Constraint on Reformation Propagation").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical_epistemology/technological_mediation").

domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, '6336e402-9931-4982-98a6-900ba72eef8d').
narrative_ontology:cs_kernel_codification('6336e402-9931-4982-98a6-900ba72eef8d', distributed).
narrative_ontology:cs_authority_grounding('6336e402-9931-4982-98a6-900ba72eef8d', diffuse_epistemic).
narrative_ontology:cs_reading_relation('6336e402-9931-4982-98a6-900ba72eef8d', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('6336e402-9931-4982-98a6-900ba72eef8d', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('6336e402-9931-4982-98a6-900ba72eef8d', foundational, information_technology_causally_prior_to_scale).
narrative_ontology:cs_axiom_status(information_technology_causally_prior_to_scale, holdable).
narrative_ontology:cs_axiom_grounding('6336e402-9931-4982-98a6-900ba72eef8d', information_technology_causally_prior_to_scale, empirically_contingent).
narrative_ontology:cs_axiom('6336e402-9931-4982-98a6-900ba72eef8d', secondary, literacy_distribution_enables_print_access).
narrative_ontology:cs_axiom_status(literacy_distribution_enables_print_access, holdable).
narrative_ontology:cs_axiom_grounding('6336e402-9931-4982-98a6-900ba72eef8d', literacy_distribution_enables_print_access, empirically_contingent).
narrative_ontology:cs_reference_frame('6336e402-9931-4982-98a6-900ba72eef8d', manuscript_information_distribution_epoch).
narrative_ontology:cs_drift_state('6336e402-9931-4982-98a6-900ba72eef8d', print_scale_establishment, gap(stable, severe, true)).
narrative_ontology:cs_created_at('6336e402-9931-4982-98a6-900ba72eef8d', '2026-06-12T14:37:22Z').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_urban_populations).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, reformed_doctrine_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, church_authority_hierarchy).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, media_determines_movement_scale).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, literacy_distribution_constrains_ideological_reach).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owners and craftspeople who operated printing presses in 16th-century European cities. They determined publication runs, titles, prices, and geographic distribution. They faced supply constraints (ink, paper, trained labor) and demand constraints (literacy, purchasing power, religious/political risk). They could choose WHICH texts to print (commercial calculus favoring profitable titles) but could not change the physical efficiency of the press itself.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, printing_press_operators, agenda_setter,
    institutional, biographical, constrained, regional).

% Educated merchants, clergy, minor nobility, urban professionals (approximately 1.5–3% of 16th-century European population) who could read Latin and vernacular texts. They gained direct access to reformed theology in print form rather than relying on pulpit or manuscript circulation. They bore the cost of purchasing printed texts but gained information access at unprecedented speed and scale.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literate_urban_populations, beneficiary,
    moderate, biographical, mobile, regional).

% Theologians, preachers, and activists whose critiques of church doctrine and practice were amplified by print reproduction. Luther's Ninety-Five Theses (1517) would have remained a local academic controversy without rapid print distribution; print enabled their dissemination across Europe within weeks. They did not control the printing press but benefited from its capacity to reach scale.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, reformed_doctrine_advocates, beneficiary,
    organized, biographical, constrained, continental).

% The approximately 97% of 16th-century European populations who could not read. They were excluded from direct access to printed reformed texts. They encountered reformed theology through oral transmission (preachers, public debates, hymns, visual art) and could not verify claims against printed sources. The technological constraint (printing press + literacy) was structurally inaccessible to them.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, illiterate_populations, excluded,
    powerless, biographical, trapped, local).

% Catholic ecclesiastical hierarchy (papacy, bishops, monastic orders) whose doctrine and authority were subjected to mass-reproduced critique for the first time in European history. They bore the cost of responding to rapid, continent-wide dissemination of alternative theological positions. Their traditional mechanisms (pulpit, manuscript circulation, local councils) operated at slower scale than print.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, church_authority_hierarchy, payer,
    institutional, civilizational, constrained, continental).

% Preachers, visual artists, theater troupes, hymn-singers, street performers who carried reformed ideas to illiterate populations. This reading brackets their role as secondary — the technological mediation reading focuses on print and literacy, not on the oral/performative transmission mechanisms that reached the majority. They remain analytically present but not focal to the constraint's logic.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, oral_transmission_vectors, observer,
    moderate, biographical, mobile, local).

% The material supply chains (paper mills, ink production, labor) that enabled printing. These are treated as non-agent infrastructure for the purposes of this reading — they constrain the press's capacity but are not actors making choices about reform doctrine. Listed for completeness as they are part of the constraint's material instantiation.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, paper_and_ink_supply_networks, observer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_non_agent(reformation_composite__technological_mediation_reading, paper_and_ink_supply_networks).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__technological_mediation_reading, diffuse).
narrative_ontology:fixing_cost_class(reformation_composite__technological_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the functional sense: the printing press does not solve a collective-action problem through coordination. It provides a technical capability (information reproduction and distribution at scale) that enables coordination among other actors (reformed advocates, literate audiences) but is not itself a coordination mechanism.
% TRANSFER_FUNCTION: The printing press transfers information bandwidth from pre-print limits (manuscript circulation at ~hundreds of copies per year per location) to print-era capacity (thousands to tens of thousands of copies per year per location). The transfer goes FROM manuscript-based information distribution (slow, local, labor-intensive) TO print-based information distribution (rapid, continental, capital-intensive). Financially, it transfers income FROM purchasers TO press operators and paper/ink suppliers.
% ABSENT_VOICES: Illiterate and oral-transmission-dependent populations are excluded from direct voice in print-based theological debate. They encounter reformed ideas through preachers and visual media but cannot verify claims against texts or participate in written controversy. Rival print operators (if any existed beyond the dominant presses) would argue for different publication priorities but are largely absent from the historical record as contending voices.
% DISAPPEARANCE_RATIONALE: If the printing press disappeared overnight (reverted to pre-press information distribution limits), the scale at which reformed theology propagated across Europe would be radically constrained. Local theological dissent would remain (monasteries, universities, lay piety movements would continue generating critique), but continental-scale movement would not form at the speed or reach historically observed. The Reformation as a 16th-century continental event depends on print capacity — without it, reformation becomes a series of local/regional movements spread over centuries.
% FOUNDING_PROBLEM: Before the printing press, information replication was labor-intensive and slow: a manuscript theology text required months of hand-copying per copy. Local theological innovation (monastery debates, university disputations, lay movements) remained geographically contained because scaling required reproducing texts at unsustainable labor cost. The founding problem: how to distribute ideas across space and populations at sufficient speed and cost to create continental intellectual movements?
% FOUNDING_PROBLEM_CORROBORATION: Print historians and book historians (Eisenstein, Pettegree, Chartier, Febvre) outside the benefiting theological parties attest that the founding problem (information distribution at scale) was the central innovation the press addressed and that continental-scale movement became possible only with print capacity. Quantitative evidence from early modern book history (publication rate data, literacy statistics, book circulation records) supports the founding problem claim. This corroboration comes from historians who do not have stakes in the theological or political questions but analyze the technological/information dimension.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_composite__technological_mediation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.15): a natural/physical constraint (the printing press, its operating limits, ink/paper/labor costs) does not extract from participants the way social arrangements do. It enables action at a new scale but does not redistribute goods or sustain itself through coercion. Suppression is VERY LOW (0.08): alternatives to the printing press persist (oral transmission, manuscript circulation, church authority's traditional mechanisms); the press constrains but does not foreclose. Accessibility collapse is VERY HIGH (0.89): once the printing press exists and literacy is distributed, the constraint's logic becomes nearly irreversible — returning to pre-print information dynamics requires active suppression (burning books, banning literacy, destroying presses), which the constraint itself makes materially harder. Resistance is NEAR-ZERO (0.04): the constraint is not defended by social actors; it persists because it is a physical/natural fact. Theater is very low (0.12): little performative maintenance is required because the constraint does not depend on social belief in its legitimacy — the press works whether participants believe it should or not. The measurement series are flat across time (t=0 to t=40) because the constraint's structural properties do NOT CHANGE as the Reformation unfolds; the press was a constant environmental feature of 1500–1550, not an agent adapting or intensifying. Mountains do not drift.
 *
 * PERSPECTIVAL GAP:
 *   From the LITERATE ADVOCATE seat: the printing press is liberatory technology, a pure beneficiary frame. From the ILLITERATE or ORAL-TRANSMISSION seat: the press is irrelevant to their experience; the movement reaches them via sermon and song, not print. The technological reading does not compute a different type from each seat because it is a mountain — every seat computes it the same way (mountain from all seats) — but the reading itself is more salient and meaningful to the literate stratum. From the SIBLING READING seats (political or theological): the technological reading is incomplete or misleading — political decisions about sovereignty and theological choices about doctrine are primary, technology is instrumental. The technological reading does NOT adjudicate these disputes; it simply asserts a different causal priority.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has NO VICTIMS in the structural sense: no agent is targeted for extraction. It has BENEFICIARIES (literate urban populations, reformed advocates) in the sense that the press's existence enables them to reach scale they could not otherwise achieve. But beneficiary status does NOT make directionality high-d (extraction-ward) — a beneficiary of a natural constraint sits at low d (near 0.0, subsidy end). The press operators collect fees, but the constraint being authored is ABOUT INFORMATION DISTRIBUTION, not about the economics of the press-manufacturing business (that is a separate constraint, press_operator_profit_extraction, with different ε and beneficiaries). The technological reading brackets the question: who profits from the press? It asserts the constraint is the information distribution CAPACITY ENABLED BY the press, which is available to all who have literacy and access. Directionality derivation: beneficiaries = literate_urban_populations + reformed_advocates; victims = none (no structural extraction). d for beneficiaries ≈ 0.15 (low, they benefit from the natural constraint's existence). d for the press operators would be different and is authored in a separate constraint about economic extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (decay of original mandate) does not apply to a mountain: the founding problem of the printing press WAS information distribution efficiency and reach, and that mandate has not decayed — the press still solves the problem it was designed for. The constraint persists because it works, not because maintenance theater substitutes for function. No false summit here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_reading,
    'Is the printing press''s role in the Reformation a physical/natural constraint (mountain: independent of human choice, would persist regardless of politics), or a constructed constraint (reading-specific framing that beneficiaries of this lens have motives to promote)?',
    'Comparative historical analysis: does the same printing-press availability with DIFFERENT political/theological contexts produce similar movement scales, or is technological mediation intelligible only when allied with political sovereignty claims and theological fragmentation?',
    'If the constraint is genuinely natural (mountain), the technological reading isolates a necessary but not sufficient condition — useful but incomplete. If constructed (false summit), the ''naturalness'' of the printing press framing masks political and theological choices that the other readings foreground. Classification would shift to tangled_rope or snare depending on who benefits from the technological framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_constructed_reading, conceptual, 'Whether printing-press mediation is a natural constraint or a reading-specific constructed framing').

omega_variable(
    kernel_contest_interdependence,
    'Do the three sibling readings (technological_mediation, political_realignment, theological_fragmentation) describe independent dimensions of the Reformation, or are they three framings of a single complex event where no dimension causally precedes the others?',
    'Counterfactual historical analysis: (1) Reformation WITHOUT printing press but WITH political sovereignty claims and theological differences — would local dissent scale into continental movement? (2) WITH printing press but WITHOUT political/theological differentiation — would mass publication of existing orthodoxy produce reformation? (3) Comparative: did other print-enabled societies without political/theological conditions experience reformation-scale movements?',
    'If readings describe independent causal chains, the technological reading stands alone as a sufficient condition (false — none is sufficient). If they describe interdependent dimensions, none of the readings is causally primary; all three are aspects of one event. The classification of each sibling would shift from mountain toward tangled_rope or snare (interdependence means each is partly constructed, partly natural).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_interdependence, conceptual, 'Whether the three readings are independent or aspects of an interdependent whole').

omega_variable(
    literacy_distribution_ambiguity,
    'Does the technological mediation reading refer to literate urban merchants and clergy (the 1.5–3% who could read in 1500–1550 Europe) or to the broader population eventually mobilized? If the latter, is literacy distribution a constraint on the Reformation, or is oral transmission (preachers, popular theater, visual art, hymn-singing) the actual mediation mechanism?',
    'Quantitative analysis of literacy rates by region and time point; comparison of literate-vs-oral transmission mechanisms'' documented reach and speed in Reformation propagation.',
    'If oral transmission is primary, the printing press is a constraint on elite theological discourse, not on mass movement scale — the technological reading applies only to a narrow stratum (clergy, city merchants) and misses the popular movement. If oral transmission is secondary, the literacy-constrained reading applies more broadly. This affects whether the constraint''s accessibility_collapse (how alternatives close when understood) should be 0.89 (high for elite theology) or lower (0.65–0.75) for the broader population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_distribution_ambiguity, empirical, 'Whether literacy distribution or oral transmission is the primary mediation mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_composite__technological_mediation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(refo_tr_t0, observed).
narrative_ontology:measurement(refo_tr_t8, reformation_composite__technological_mediation_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement_basis(refo_tr_t8, observed).
narrative_ontology:measurement(refo_tr_t16, reformation_composite__technological_mediation_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement_basis(refo_tr_t16, observed).
narrative_ontology:measurement(refo_tr_t24, reformation_composite__technological_mediation_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement_basis(refo_tr_t24, observed).
narrative_ontology:measurement(refo_tr_t32, reformation_composite__technological_mediation_reading, theater_ratio, 32, 0.12).
narrative_ontology:measurement_basis(refo_tr_t32, observed).
narrative_ontology:measurement(refo_tr_t40, reformation_composite__technological_mediation_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(refo_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_composite__technological_mediation_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(refo_be_t0, observed).
narrative_ontology:measurement(refo_be_t8, reformation_composite__technological_mediation_reading, base_extractiveness, 8, 0.13).
narrative_ontology:measurement_basis(refo_be_t8, observed).
narrative_ontology:measurement(refo_be_t16, reformation_composite__technological_mediation_reading, base_extractiveness, 16, 0.14).
narrative_ontology:measurement_basis(refo_be_t16, observed).
narrative_ontology:measurement(refo_be_t24, reformation_composite__technological_mediation_reading, base_extractiveness, 24, 0.15).
narrative_ontology:measurement_basis(refo_be_t24, observed).
narrative_ontology:measurement(refo_be_t32, reformation_composite__technological_mediation_reading, base_extractiveness, 32, 0.15).
narrative_ontology:measurement_basis(refo_be_t32, observed).
narrative_ontology:measurement(refo_be_t40, reformation_composite__technological_mediation_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement_basis(refo_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_composite__technological_mediation_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(refo_su_t0, observed).
narrative_ontology:measurement(refo_su_t8, reformation_composite__technological_mediation_reading, suppression_requirement, 8, 0.07).
narrative_ontology:measurement_basis(refo_su_t8, observed).
narrative_ontology:measurement(refo_su_t16, reformation_composite__technological_mediation_reading, suppression_requirement, 16, 0.08).
narrative_ontology:measurement_basis(refo_su_t16, observed).
narrative_ontology:measurement(refo_su_t24, reformation_composite__technological_mediation_reading, suppression_requirement, 24, 0.08).
narrative_ontology:measurement_basis(refo_su_t24, observed).
narrative_ontology:measurement(refo_su_t32, reformation_composite__technological_mediation_reading, suppression_requirement, 32, 0.08).
narrative_ontology:measurement_basis(refo_su_t32, observed).
narrative_ontology:measurement(refo_su_t40, reformation_composite__technological_mediation_reading, suppression_requirement, 40, 0.08).
narrative_ontology:measurement_basis(refo_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(reformation_composite__technological_mediation_reading, 0.08).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).

% DUAL FORMULATION NOTE:
% The Reformation is a contested historical kernel decomposed into three structurally distinct constraints: (1) technological_mediation_reading — printing press as mountain enabling scale shift in information distribution; (2) political_realignment_reading — nation-state sovereignty differentiation via religious identity; (3) theological_fragmentation_reading — incompatible soteriological commitments. Each reading generates different ε, different beneficiary/victim structure, different classification. This story (technological_mediation) influences both siblings via network.affects_constraints because the printing-press infrastructure enabled the scale at which political realignment and theological differentiation became historically visible — but neither sibling is logically foreclosed by this reading's core premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
