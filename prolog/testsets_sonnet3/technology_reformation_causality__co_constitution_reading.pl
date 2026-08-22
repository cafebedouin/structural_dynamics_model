% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__co_constitution_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Co-Constitution of Print Technology and Reform Movements
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the co-constitution reading of the
 *   printing-press/Reformation causal kernel: technology and reform movements
 *   are treated as mutually shaping rather than one causing the other. The
 *   press enabled vernacular mass distribution but did not by itself produce
 *   Reformation theology; reformers' doctrinal needs and rhetorical choices
 *   in turn shaped what got printed, in what formats, and how fast. The
 *   extraction measured here is low and rises only modestly across the
 *   interval — it derives from the interaction term of technology-adoption
 *   and doctrinal-content choices, concentrated on parties with no seat at
 *   either negotiating table (scribal labor, minority dialects), not from a
 *   single dominant extractor. This is a different structural claim from the
 *   technological_determinism_reading (which would treat the press as
 *   sufficient cause, driving extraction largely from below-threshold
 *   coordination costs) and from the beneficiary_agency_reading (which would
 *   treat reformers and printers as strategic agents deploying a tool,
 *   concentrating any extraction in intentional gatekeeping choices by those
 *   agents). The three readings are siblings of one kernel, not competing
 *   measurements of one constraint; each authors its own ε from its own
 *   causal architecture.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.28).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.22).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Co-Constitution of Print Technology and Reform Movements").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, '1ffc3786-101c-4b4f-bbaf-2b2f20251143').
narrative_ontology:cs_kernel_codification('1ffc3786-101c-4b4f-bbaf-2b2f20251143', distributed).
narrative_ontology:cs_authority_grounding('1ffc3786-101c-4b4f-bbaf-2b2f20251143', distributed).
narrative_ontology:cs_reading_relation('1ffc3786-101c-4b4f-bbaf-2b2f20251143', technology_reformation_causality__technological_determinism_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ffc3786-101c-4b4f-bbaf-2b2f20251143', technology_reformation_causality__beneficiary_agency_reading, coexists_with).
narrative_ontology:cs_axiom('1ffc3786-101c-4b4f-bbaf-2b2f20251143', foundational, causality_is_bidirectional_and_mutually_constituting).
narrative_ontology:cs_axiom_status(causality_is_bidirectional_and_mutually_constituting, holdable).
narrative_ontology:cs_axiom_grounding('1ffc3786-101c-4b4f-bbaf-2b2f20251143', causality_is_bidirectional_and_mutually_constituting, empirically_contingent).
narrative_ontology:cs_axiom('1ffc3786-101c-4b4f-bbaf-2b2f20251143', secondary, neither_technology_nor_agency_alone_is_causally_sufficient).
narrative_ontology:cs_axiom_status(neither_technology_nor_agency_alone_is_causally_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('1ffc3786-101c-4b4f-bbaf-2b2f20251143', neither_technology_nor_agency_alone_is_causally_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('1ffc3786-101c-4b4f-bbaf-2b2f20251143', mutual_adaptation_historiography).
narrative_ontology:cs_drift_state('1ffc3786-101c-4b4f-bbaf-2b2f20251143', post_actor_network_theory_reception, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('1ffc3786-101c-4b4f-bbaf-2b2f20251143', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, printers_and_publishers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, reform_movement_leadership).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, vernacular_literate_laity).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, displaced_scribal_workers).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, non_vernacular_regional_dialects).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, technology_and_agency_are_mutually_shaping).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, media_infrastructure_constrains_but_does_not_determine_ideological_outcomes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Print workshops in cities like Wittenberg, Basel, and Strasbourg adapted their output to what sold: pamphlets, broadsides, and vernacular tracts. They did not simply serve a predetermined reform agenda; their commercial choices (typeface, pamphlet length, pricing) shaped what reform ideas could circulate cheaply and fast, and reform leaders adapted their writing to printable formats. Exit for printers meant switching patrons or genres, not leaving the trade.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, printers_and_publishers, beneficiary,
    organized, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, printers_and_publishers, agenda_setter).

% Figures like Luther wrote in formats calibrated to the press's capacities and the market's appetite — short, vernacular, argumentative pamphlets rather than long Latin treatises. Their theological content shaped press output; the press's economics and reach shaped which arguments got made and how. They could not have produced the same movement through pulpit and manuscript alone, but the press alone would not have produced this content absent their doctrinal innovation.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, reform_movement_leadership, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, reform_movement_leadership, agenda_setter).

% Newly literate or semi-literate laypeople gained access to scripture and pamphlets in their own tongue, participating in religious debate previously mediated entirely by clergy. Their appetite for vernacular material fed back into what printers commissioned, making them co-producers of demand, not merely passive recipients.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, vernacular_literate_laity, beneficiary,
    moderate, biographical, constrained, regional).

% Monastic and guild scribes who had copied manuscripts by hand lost commissions as print output scaled. Their labor and institutional position had no equivalent role in the new configuration; retraining into print trades was possible for some but not systemic or guaranteed.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, displaced_scribal_workers, payer,
    powerless, biographical, trapped, regional).

% Print standardized a small number of vernacular forms (High German, printers' Parisian French) as the commercially viable written registers, accelerating the marginalization of dialects and minority vernaculars that lacked print market share. This is a structural byproduct of the co-evolution, not a chosen policy by any single actor.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, non_vernacular_regional_dialects, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(technology_reformation_causality__co_constitution_reading, non_vernacular_regional_dialects).

% Faced a media environment whose rules were being rewritten by the interaction of press economics and reform content, arriving without an established playbook for either. Their attempts to respond — indices of prohibited books, counter-pamphleteering — were themselves shaped by the format constraints of the medium they were reacting to. They are not centered in this reading, which is itself a methodological choice about where to locate causal weight.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, catholic_ecclesiastical_authorities, excluded,
    institutional, generational, constrained, continental).

% Assess the causal architecture of the print-Reformation relationship retrospectively, weighing determinist, instrumentalist, and co-constitutive accounts against the documentary record of print runs, patronage networks, and doctrinal content evolution.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, media_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__co_constitution_reading, diffuse).
narrative_ontology:fixing_cost_class(technology_reformation_causality__co_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Printers needed reliable, sellable content; reformers needed cheap, fast, wide-reaching distribution outside clerical control. The press and the reform movement solved each other's resource problem: print technology supplied scale and speed, reform content supplied a mass vernacular market that justified capital investment in new presses and typefaces.
% TRANSFER_FUNCTION: Moves religious authority and interpretive control from centralized clerical hierarchy toward printers, reform leadership, and a newly literate laity; moves economic activity from scribal copying toward mechanized printing; moves linguistic prestige toward the small set of vernaculars that achieved print-market viability.
% ABSENT_VOICES: Scribal guilds and minority-dialect communities had no organized voice in either the print market or the reform debate; their displacement is a byproduct nobody negotiated and few contemporaries framed as a cost worth weighing. Catholic ecclesiastical authorities are present but structurally reactive in this reading rather than co-authors of the causal story.
% DISAPPEARANCE_RATIONALE: If the co-constitutive relationship (as opposed to either technology or reform theology alone) had not obtained — if either side had not adapted to the other's constraints and opportunities — historians disagree on whether an equivalent reform movement or an equivalent print-driven vernacular expansion would have emerged through different channels (oral preaching networks, manuscript circulation, later print adoption). The determinist and agency readings would each answer differently; this reading holds the counterfactual open.
% FOUNDING_PROBLEM: Neither the printing press's economic viability nor the Reformation's mass reach was, on its own, solved: printers needed profitable high-volume content, and reformers needed distribution that bypassed clerical gatekeeping and reached beyond Latin-literate elites.
% FOUNDING_PROBLEM_CORROBORATION: Book-history economic records (print-run counts, workshop account books) and reform-movement correspondence, both assembled by later historians outside either the printing trade or any confessional tradition, corroborate that the mutual-dependency problem was real in the 1510s-1540s and has since been resolved by the maturation of both print markets and confessional institutions — neither now depends on the other's scaffolding function.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, contested).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__co_constitution_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).
:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness stays low throughout (0.12 to 0.28) because the core relationship is genuinely coordinative — press and reform each solved the other's resource problem — and the costs that do exist (scribal displacement, dialect marginalization) are diffuse byproducts of the interaction rather than a captured rent stream. Theater ratio rises across the interval (0.20 to 0.42) tracking the increasing institutionalization of reform confessions after the initial dynamic period — by the mid-1600s, printed catechisms and confessional literature had become partly performative maintenance of settled doctrine rather than the live coordination of the 1517-1550 window, consistent with the sibling delta describing reformers as trending toward piton (atrophied dynamism, institutional maintenance) once the movement matured. Suppression and accessibility_collapse are kept modest because the co-constitution reading holds that alternative media/theological configurations remained conceivable and were not foreclosed by either technology or doctrine alone.
 *
 * PERSPECTIVAL GAP:
 *   From the printer/reformer seats, the relationship looks like functioning coordination — each got what it needed from the other. From the scribal-labor and dialect-community seats, the same interaction looks like an unaccountable structural displacement with no addressable agent to petition, since neither printers nor reformers set out to marginalize them; this is exactly the seat divergence the co-constitution reading is built to explain, in contrast to readings that would locate a clearer villain (determinism: the technology; agency: the reformers).
 *
 * DIRECTIONALITY LOGIC:
 *   Printers, reform leadership, and the newly literate laity are declared beneficiaries because the co-constitution reading locates the causal engine in mutual adaptation that served all three; none of them individually bears the interaction's costs. Displaced scribal workers and marginalized dialects are victims because they are structural casualties of the co-evolving system with no compensating benefit and no seat in either negotiation — their d sits near the full-target end despite being outside any deliberate extraction, because the interaction pattern reallocated resources and prestige without their participation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mutual resource dependency between fragile early print economics and a distribution-starved reform movement) is dead by the mid-1600s: print markets and confessional institutions both achieved independent stability. The rising theater_ratio traces this — what remains institutionalized (catechetical printing, confessional publishing houses) increasingly performs continuity with the founding dynamic rather than reproducing it, consistent with the reformers-as-piton delta: the coordination function atrophied into inertial institutional maintenance once both sides' original resource problems were solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_reform_without_print,
    'Would an equivalent-scale reform movement have emerged through oral preaching networks and manuscript circulation absent the printing press, or was print''s speed and volume a necessary (not merely accelerating) condition?',
    'Comparative case study against pre-print heterodox movements (Lollardy, Hussite movement) that achieved regional but not continental scale without print, controlling for other variables (political fragmentation, urbanization).',
    'If a comparable movement was achievable without print, this supports the co-constitution reading''s claim that press enabled-but-did-not-determine; if the pre-print cases show a scale ceiling print alone broke through, this strengthens the sibling determinism reading at this reading''s expense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_reform_without_print, empirical, 'Whether print was a necessary or merely accelerating condition for Reformation scale.').

omega_variable(
    interaction_term_measurement,
    'Can the extraction attributed to the print-reform interaction (as opposed to either factor alone) be isolated empirically, or is the co-constitution framing itself an artifact of historiographical convenience that cannot be operationalized?',
    'Quantitative book-history analysis correlating print-run economics with doctrinal content shifts across multiple printing centers, seeking evidence of bidirectional adaptation versus one-directional influence.',
    'If bidirectional adaptation is empirically demonstrable at fine grain, the co-constitution reading''s ε (derived from the interaction term) is well-grounded; if the data resolve cleanly into one-directional causal chains, the co-constitution frame may be conceptually elegant but empirically underdetermined relative to its sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interaction_term_measurement, conceptual, 'Whether the interaction term this reading''s ε depends on is empirically isolable or a historiographical framing choice.').

omega_variable(
    scribal_displacement_attribution,
    'Is the scribal labor displacement properly attributed to the print-reform interaction specifically, or to print technology''s economics independent of any reform content (i.e., would scribes have been displaced by a purely secular print expansion)?',
    'Examine print markets and scribal employment trajectories in regions with strong print growth but limited or delayed reform penetration (parts of Italy, France) to isolate the reform-specific displacement effect.',
    'If displacement tracks print volume regardless of reform content, the victim declaration here over-attributes cost to the co-constitution relationship specifically rather than to print technology generally, which would argue for narrowing this reading''s victim set or moving that cost to a technology-only sibling constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scribal_displacement_attribution, empirical, 'Whether scribal displacement is attributable to the reform-print interaction or to print economics alone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1450, technology_reformation_causality__co_constitution_reading, theater_ratio, 1450, 0.2).
narrative_ontology:measurement(tech_tr_t1490, technology_reformation_causality__co_constitution_reading, theater_ratio, 1490, 0.25).
narrative_ontology:measurement(tech_tr_t1517, technology_reformation_causality__co_constitution_reading, theater_ratio, 1517, 0.3).
narrative_ontology:measurement(tech_tr_t1550, technology_reformation_causality__co_constitution_reading, theater_ratio, 1550, 0.38).
narrative_ontology:measurement(tech_tr_t1600, technology_reformation_causality__co_constitution_reading, theater_ratio, 1600, 0.4).
narrative_ontology:measurement(tech_tr_t1650, technology_reformation_causality__co_constitution_reading, theater_ratio, 1650, 0.42).

% Extraction over time
narrative_ontology:measurement(tech_be_t1450, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1450, 0.12).
narrative_ontology:measurement(tech_be_t1490, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1490, 0.15).
narrative_ontology:measurement(tech_be_t1517, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1517, 0.19).
narrative_ontology:measurement(tech_be_t1550, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1550, 0.25).
narrative_ontology:measurement(tech_be_t1600, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1600, 0.27).
narrative_ontology:measurement(tech_be_t1650, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1650, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(technology_reformation_causality__co_constitution_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, information_standard).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__co_constitution_reading, 0.05).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, beneficiary_agency_reading).

% DUAL FORMULATION NOTE:
% These three stories decompose the natural-language claim 'the printing press caused the Reformation' per the ε-invariance principle: the label conflates a determinist claim (press as sufficient cause), an instrumentalist claim (reformers as strategic tool-users), and a co-constitutive claim (mutual shaping) which have different ε values, different beneficiary/victim structures, and different classifications (rope-trending-piton here; likely mountain-adjacent inevitability framing for determinism; likely rope-with-concentrated-agenda-setters for beneficiary_agency). Linked via affects_constraints as sibling readings of one kernel, not as one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
