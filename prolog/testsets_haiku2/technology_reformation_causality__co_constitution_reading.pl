% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Co-Constituted Technology-Social Actor Interaction in Reformation
 *   domain: historical/theological/technological
 *
 * SUMMARY:
 *   This reading instantiates the co-constitution thesis: technology
 *   (printing) and social actors (reformers, Church, printers, vernacular
 *   communities) co-evolved through a bidirectional causality. The printing
 *   press did not determine the Reformation; reformers did not simply use a
 *   neutral tool. Instead, reformist networks shaped what the press produced
 *   (by commissioning vernacular Bibles, writing for print format,
 *   coordinating editions), while the press simultaneously enabled and
 *   constrained what reformist theology could become (standardized, scalable,
 *   citable, but also simplified and polished for typographic clarity). The
 *   constraint is claimed as tangled_rope: genuine coordination function
 *   (standardized theological debate across geography) bundled with
 *   asymmetric extraction (Church authority monopoly disrupted, manuscript
 *   authorities displaced). The founding problem is live because the
 *   coordination function persists; the bifurcation between
 *   technological-determinist and beneficiary-agency readings is preserved by
 *   modeling this reading as one instantiation of a contested kernel.
 *
 * KEY AGENTS:
 *   - reformation_printers_publishers — coordinate standardization, enforce textual discipline, extract from Church gatekeeping monopoly
 *   - reformation_theologians_scholars — author the ideas, shape what gets printed, benefit from coordination but constrained by printable format
 *   - church_institutional_hierarchy — loses monopoly on textual authority, forced to adapt printing strategy, payer in the extractive structure
 *   - latin_reading_clergy — identity-locked payers; exclusive access to texts was professional capital now eroded by vernacular competition
 *   - vernacular_literacy_communities — beneficiaries; access to texts impossible in manuscript era, enabled by standardized print production
 *   - historians_interpreters — analytical seat assessing the causality: neither technology nor agency alone explains Reformation trajectory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.62).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.45).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Co-Constituted Technology-Social Actor Interaction in Reformation").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "historical/theological/technological").

domain_priors:requires_active_enforcement(technology_reformation_causality__co_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, '9286d82e-afe9-4615-b4b5-d2bba87f9b99').
narrative_ontology:cs_kernel_codification('9286d82e-afe9-4615-b4b5-d2bba87f9b99', distributed).
narrative_ontology:cs_authority_grounding('9286d82e-afe9-4615-b4b5-d2bba87f9b99', distributed).
narrative_ontology:cs_reading_relation('9286d82e-afe9-4615-b4b5-d2bba87f9b99', technology_reformation_causality__technological_determinism_reading, coexists_with).
narrative_ontology:cs_reading_relation('9286d82e-afe9-4615-b4b5-d2bba87f9b99', technology_reformation_causality__beneficiary_agency_reading, coexists_with).
narrative_ontology:cs_axiom('9286d82e-afe9-4615-b4b5-d2bba87f9b99', foundational, bidirectional_causality_in_technical_social_change).
narrative_ontology:cs_axiom_status(bidirectional_causality_in_technical_social_change, holdable).
narrative_ontology:cs_axiom_grounding('9286d82e-afe9-4615-b4b5-d2bba87f9b99', bidirectional_causality_in_technical_social_change, empirically_contingent).
narrative_ontology:cs_axiom('9286d82e-afe9-4615-b4b5-d2bba87f9b99', foundational, technology_and_agency_are_mutually_constitutive).
narrative_ontology:cs_axiom_status(technology_and_agency_are_mutually_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('9286d82e-afe9-4615-b4b5-d2bba87f9b99', technology_and_agency_are_mutually_constitutive, deontological).
narrative_ontology:cs_reference_frame('9286d82e-afe9-4615-b4b5-d2bba87f9b99', printing_and_reformation_causally_intertwined).
narrative_ontology:cs_drift_state('9286d82e-afe9-4615-b4b5-d2bba87f9b99', contemporary_historical_scholarship, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('9286d82e-afe9-4615-b4b5-d2bba87f9b99', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, reformation_intellectual_movement).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, vernacular_literacy_communities).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, church_monopoly_on_textual_authority).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, latin_reading_gatekeepers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, reformation_theologians_scholars).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, reformist_intellectual_networks).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, reformation_theologians_scholars).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, church_manuscript_authorities).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, latin_reading_clergy).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, manuscript_copyist_guilds).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, church_institutional_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate printing shops and coordinate manuscript acquisition, typeface decisions, and distribution networks. They enforce standardization of text (fixed page layout, concordances, marginalia placement) and decide which reformers' texts get amplified vs. suppressed. They benefit from reformer patronage and from the constraint that gives their standardization authority; their exit would require reversion to manuscript culture or abandoning reformist networks.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, reformation_printers_publishers, agenda_setter,
    organized, generational, constrained, regional).

% Author the theological arguments that the press amplifies. They benefit from the constraint because the printing press standardizes and stabilizes their arguments across geographies in ways manuscript copying cannot. They also pay: they must conform their arguments to printable format (brevity, typographic clarity, reduction of ambiguity that oral transmission could handle). Their exit is possible (they can publish handwritten broadsheets or return to pulpit orality) but expensive in reach.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, reformation_theologians_scholars, beneficiary,
    powerful, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, reformation_theologians_scholars, payer).

% Controlled textual transmission through scribal networks and institutional libraries. The printing constraint extracts their monopoly on textual authority: they can no longer regulate which texts circulate through gatekeeping-on-copying. They are forced to respond reactively to printed texts they did not authorize. Their only exit is technological regression (which they cannot enforce across Christendom) or institutional capture of printing itself (which requires resources and speed they do not have).
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, church_manuscript_authorities, payer,
    institutional, generational, trapped, continental).

% Their professional identity is constituted by exclusive access to Latin sacred texts and the interpretive authority that flows from that literacy. The printing constraint that enables vernacular Bibles and reformist glosses directly threatens their professional monopoly. Exit would mean abandoning the clerical identity entirely. The constraint forces them to either learn vernacular languages (identity dilution) or lose authority (identity loss).
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, latin_reading_clergy, payer,
    organized, biographical, identity_locked, continental).

% Urban artisans, merchants, and educated women who gain access to religious texts in their own language through printed works. They benefit from the constraint because the printing press makes vernacular Bibles and reformist tracts economically viable at scale. Their exit is bounded by the geographic extent of printing networks and their purchasing power; they cannot revert to manuscript culture even if they wanted.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, vernacular_literacy_communities, beneficiary,
    moderate, generational, constrained, regional).

% Professional scribes whose livelihoods depend on hand-copying texts. The printing press extracts their economic role entirely. Some transition to printed-book production, but this requires new skills and subjects them to printer authority rather than independent guild autonomy. Their identity as craftsmen is partly fungible (some become printers), but the guild structure and apprenticeship model are disrupted by the constraint.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, manuscript_copyist_guilds, payer,
    moderate, biographical, identity_locked, regional).

% Erasmus, Luther, and networks of scholars who shape what gets printed and how. They benefit from the press as a coordination mechanism: standardized texts allow their arguments to reach far and be debated coherently across regions. They co-constitute the constraint by deciding what reformist ideas get printed, which translations are authorized, which marginalia appear. They have exit options (underground manuscripts, oral networks) but these are vastly less effective.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, reformist_intellectual_networks, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, reformist_intellectual_networks, agenda_setter).

% The Catholic hierarchy is forced to respond to printed reformist arguments it does not author and cannot fully suppress. The constraint extracts institutional authority: the Church must now publish its own defenses, standardize doctrine in print, and compete on a playing field it did not design. Exit would require either re-establishing textual monopoly (logistically impossible) or adapting the Church's own publication apparatus (which happens but at enormous cost and delay).
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, church_institutional_hierarchy, payer,
    institutional, generational, constrained, universal).

% Assess the causal relationship between technology and social change. This reading — co-constitution — disputes both strict technological determinism and pure beneficiary agency. The historian observes that printing and reformism were mutually shaping, not unilateral. They document the decisions that could have gone otherwise: printers could have refused reformist manuscripts; reformers could have remained oral; the Church could have adopted printing first.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, historians_and_interpreters, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__co_constitution_reading, reformation_printers_publishers).
narrative_ontology:fixing_cost_class(technology_reformation_causality__co_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press, as deployed by reformist networks, solves the problem of consistent theological argument across geography: standardized texts allow coherent debate, mutual citation, and the building of intellectual lineages across regions and time. This solves a genuine coordination problem that oral networks and manuscript culture could not: how to keep an idea stable enough to critique, develop, and defend across distances. Simultaneously, it enables the problem it solves to exist at all — the need for consistency across geography is partly constituted by printing's existence.
% TRANSFER_FUNCTION: Moves textual authority from the Church-authorized manuscript elite (clergy, monastery scribes) to printer-mediated networks (reformers, educated laypeople, vernacular readers). Also moves economic value from copyist guilds to printer-publisher enterprises. The constraint transfers interpretive authority from institutional gatekeepers to networks that can author, annotate, and distribute print.
% ABSENT_VOICES: Oral culture practitioners, non-literate populations, and alternative dissemination methods (stone carving, theatrical performance, broadsheet song) are structurally excluded by a reading that privileges the printed word as the site of causality. Their perspectives would argue that printing amplified some arguments while suppressing others and that the 'inevitability' of the Reformation obscures the roads not taken through non-print media.
% DISAPPEARANCE_RATIONALE: If the printing constraint disappeared — if mass printing had never been adopted for reformist texts — the Reformation would have followed a radically different trajectory: either remaining a localized intellectual movement without continental coherence, or requiring entirely different technological and social structures (more intensive use of oral networks, manuscript networks, theatrical dissemination, pilgrimage networks). The theologians, printers, and Church all would have reorganized differently; the constraint's absence redraws the entire historical landscape.
% FOUNDING_PROBLEM: The problem that printing and reformism co-constitutively solved was the lack of a mechanism for standardized theological debate across geography: how to ensure that an argument made in Wittenberg reaches Zurich in a form identical enough to be debated coherently, how to build a body of commentary that doesn't degrade through successive manuscript copying, how to establish priority and attribution across regions.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Reformation document that reformers actively leveraged printing technology to solve exactly this problem: Erasmus's correspondence networks coordinating editions, Luther's use of print to establish textual priority, and Calvin's printed Institutes as a standardized reference point all evidence that the coordination problem was real and acute. Non-beneficiary sources (manuscript scholars documenting the fragility of manuscript transmission, historians of oral culture noting its spatial limits) corroborate that the problem existed independently of print's adoption.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__co_constitution_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 to 0.62 over the interval because the constraint's operation increasingly disrupts the Church's monopoly on textual authority. Theater ratio peaks at 0.42 around 1530 because performative aspects of the constraint emerge: the Church publishes apologetics not as genuine doctrinal development but as reactive positioning in a print market it did not create. Theater then dips to 0.38 by 1550 as the Church's print strategy becomes functional (the Tridentine reforms incorporate printing into official doctrine production). Suppression requirement peaks at 0.52 around 1530 because the constraint at that moment requires active enforcement: printing presses must be protected from Church raids, reformist networks must evade censorship lists, the constraint's persistence depends on agents actively defending it against countervailing institutional power. By 1550, suppression requirement declines to 0.45 because the Church has adapted: it now participates in print culture rather than fighting it, so the enforcement overhead decreases even though the constraint's extractive power remains high. This is the marker of tangled_rope stabilization: the extraction persists, but the active defense shifts into institutional accommodation. Accessibility collapse (0.48) reflects that alternatives — return to manuscript culture, exclusive Latin gatekeeping, or pre-printing authority structures — remained theoretically available but became practically inaccessible once printing was established (the constraint is deeply embedded but not quite at natural-law inevitability).
 *
 * PERSPECTIVAL GAP:
 *   The reformist-printer seat experiences this constraint very differently from the Church seat. For reformers and printers, the constraint is genuine coordination: it solves the problem of scaling their ideas across geography. They perceive themselves as agents who chose to use printing, not as passive beneficiaries of technology. From the Church's seat, the same constraint is pure extraction: the printing press was adopted by outside actors without the Church's authorization, and the Church is forced to respond defensively. The constraint computes as tangled_rope from the engine's seat because it exhibits genuine coordination (interdependence of theology and print standardization) plus asymmetric extraction (Church monopoly disrupted, manuscript authorities displaced, clergy identity threatened). The payer seats (Church, manuscripts authorities, Latin clergy) compute this as snare-like from their own perspective; the beneficiary seats compute it as rope-like. The engine's per-seat computation should show this divergence: institutional power and organized reformer seats experience coordination value; institutionally displaced seats experience pure extraction. No single sitting understands the constraint as genuinely co-constitutive — that's the analyst's insight, not any stakeholder's lived experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist networks and printers hold beneficiary roles: they benefit from the constraint and actively shape it. Their directionality is low (d ~ 0.15–0.25): they are partly organized, have mobile exit options (can shift networks, languages, audiences), and the constraint subsidizes their work (enables their ideas at scale). Church authorities and Latin clergy are payers: they lose institutional monopoly and professional identity to the constraint. Their directionality is high (d ~ 0.75–0.85): they are trapped or identity-locked, have limited exit (cannot revert Christendom to manuscript culture), and the constraint extracts their exclusive authority. Vernacular communities are beneficiaries with constrained exit: they gain access to texts but cannot operate outside print networks once established. Their directionality is mixed (d ~ 0.35–0.45): they benefit, but the constraint also binds them to the reformist intellectual agenda (they cannot freely choose non-reformist texts if print supplies only reformist ones in their language). Printer-publishers occupy an agenda-setter role: they set the constraint's operation (decide what gets printed, typeface, marginal notes) while also being shaped by reformer demands and Church pressure. Their directionality is low-to-moderate (d ~ 0.25–0.40): they benefit economically but are constrained by patron dependence and religious faction loyalty.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (standardized theological debate across geography) is live: the Reformation requires this coordination mechanism, and the constraint solves it. Disappearance verdict is world_rearranges: if printing had not been adopted for reformist texts, the Reformation follows a radically different path (more localized, less coherent, possibly captured by other dissemination structures). The constraint is not mandatrophic: the founding problem persists, and the constraint remains functional. However, there is a secondary mandatrophy question embedded in the sibling readings: does the constraint's justification rest on genuine coordination, or does it rest on technological determinism (a false natural-law claim) or beneficiary agency alone (a reducibility claim)? This reading asserts that the full truth includes all three: technology enables, social actors direct, and the interaction is irreducible. Neither technological determinism alone nor beneficiary-agency-alone captures the constraint's structure, so those readings would be partly mandatrophic if adopted exclusively (they would misclassify the constraint by omitting one causal axis).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bidirectional_causality_measurement,
    'How do we empirically distinguish bidirectional causality from unidirectional causality running in either direction? If reformers strategically chose printing (beneficiary agency), is that evidence against technological causation, or evidence of co-constitution?',
    'Counterfactual historical analysis: document decisions that could have gone otherwise at key junctures (1470–1520). If printers could have refused reformist manuscripts without cost, or if reformers could have succeeded through oral networks alone, then agency is genuinely independent. If either decision would have been structurally irrational given the constraints of the time, then the causality is more tightly coupled.',
    'If bidirectional causality is empirically confirmed, the constraint is tangled_rope at minimum; if unidirectional causality is confirmed in either direction, the constraint should reclassify to rope (beneficiary case) or mountain/determined (technological case). If no empirical distinction is possible between the narratives, the reading remains in omega rather than resolving to a fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bidirectional_causality_measurement, empirical, 'Whether causality between printing and Reformation is genuinely bidirectional or reducible to one direction').

omega_variable(
    co_constitution_vs_beneficiary_agency_foreclosure,
    'Does the claim that reformers shaped what printing produced logically foreclose the claim that printing made Reformation inevitable?',
    'Logical analysis of the axioms: if printing made Reformation inevitable (tech determinism axiom), then reformers'' choices were not free — but reformers clearly made deliberate choices about what to print. If reformers'' choices were free (beneficiary agency axiom), then printing did not make Reformation inevitable. Can both axioms be true in one framework, or does one exclude the other?',
    'If the axioms foreclose each other, then the readings are in logical competition (one reading''s truth would falsify another). If both axioms can coexist (e.g., printing made Reformation inevitable GIVEN reformer agency, not inevitable independent of it), then all three readings coexist without foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_constitution_vs_beneficiary_agency_foreclosure, conceptual, 'Whether the co-constitution axiom forecloses technological determinism or beneficiary-agency readings').

omega_variable(
    extraction_referent_stability,
    'Is the measured extractiveness (0.62) a property of the co-constitutional relationship itself, or a property of a reading that privileges printed texts as the site of causality? Would a reading that centered oral networks or patronage networks measure different extraction?',
    'Author constraint stories for alternative causality framings (technology as amplifier of pre-existing oral networks; patronage networks as primary and printing as secondary). Compare the ε values across readings. If ε varies substantially across readings of the same kernel, then extraction is reading-indexed (per OQ-26); if ε remains stable, then extraction is a property of the standing arrangement, not the reading.',
    'If extraction is reading-indexed, then this story''s ε (0.62) is the abductive reading''s own assessment of extraction, not a neutral fact. The engine should flag this as a committed reading rather than a neutral observation. If ε is stable across readings, then extraction is objective and the reading disagreement is elsewhere (causality direction, not impact magnitude).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_referent_stability, conceptual, 'Whether measured extractiveness is a property of the arrangement or of the reading''s framing').

omega_variable(
    reformer_agency_vs_printer_dependence,
    'Is reformer agency (the causal power of theologians to shape print) genuine independent agency, or is it constrained agency that depends on printers'' economic incentives?',
    'Historical case analysis: document instances where reformers wanted texts printed but printers refused (or vice versa). If such instances are rare and marginal, agency is tightly coupled to printer incentives. If they are common and decisive, agency is more independent. Also measure: how much of reformist theological innovation was driven by theological argument vs. driven by what was economically printable?',
    'If reformer agency is coupled to printer incentives, then the ''co-constitution'' is actually an illusion: the constraint runs from economics (printers'' profit motive) through technology (printing press) to outcomes (which texts circulate), with reformers as nodes in that chain rather than independent agents. If reformer agency is independent, then co-constitution is real: reformers'' ideas constrain what printers do, not just vice versa.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformer_agency_vs_printer_dependence, empirical, 'Whether reformer theological agency is independent of printer economic incentives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 1440, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1440, technology_reformation_causality__co_constitution_reading, theater_ratio, 1440, 0.05).
narrative_ontology:measurement(tech_tr_t1465, technology_reformation_causality__co_constitution_reading, theater_ratio, 1465, 0.12).
narrative_ontology:measurement(tech_tr_t1485, technology_reformation_causality__co_constitution_reading, theater_ratio, 1485, 0.25).
narrative_ontology:measurement(tech_tr_t1510, technology_reformation_causality__co_constitution_reading, theater_ratio, 1510, 0.38).
narrative_ontology:measurement(tech_tr_t1530, technology_reformation_causality__co_constitution_reading, theater_ratio, 1530, 0.42).
narrative_ontology:measurement(tech_tr_t1550, technology_reformation_causality__co_constitution_reading, theater_ratio, 1550, 0.38).

% Extraction over time
narrative_ontology:measurement(tech_be_t1440, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1440, 0.15).
narrative_ontology:measurement(tech_be_t1465, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1465, 0.28).
narrative_ontology:measurement(tech_be_t1485, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1485, 0.42).
narrative_ontology:measurement(tech_be_t1510, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1510, 0.58).
narrative_ontology:measurement(tech_be_t1530, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1530, 0.65).
narrative_ontology:measurement(tech_be_t1550, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1550, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1440, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1440, 0.2).
narrative_ontology:measurement(tech_su_t1465, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1465, 0.28).
narrative_ontology:measurement(tech_su_t1485, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1485, 0.35).
narrative_ontology:measurement(tech_su_t1510, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1510, 0.48).
narrative_ontology:measurement(tech_su_t1530, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1530, 0.52).
narrative_ontology:measurement(tech_su_t1550, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1550, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__co_constitution_reading, 0.18).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__beneficiary_agency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'technology_reformation_causality'. The sibling constraints (technological_determinism_reading and beneficiary_agency_reading) instantiate the same historical event under different causal framings. All three readings share the referent (the Reformation, 1440–1550) but instantiate different constraints because their ε values and structural dependencies differ. The co_constitution reading models extractiveness as an interaction term between technology adoption and reformist network power; technological determinism would model extractiveness as technology-determined (reducing reformer agency); beneficiary agency would model extractiveness as reformer-determined (reducing technology necessity). Network edges link all three; the engine's contamination propagation will model how empirical findings in one reading affect the others' empirical plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_reformation_causality__co_constitution_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
