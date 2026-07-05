% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_determinism, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: Printing Press as Autonomous Enabling Technology (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the technological-determinism reading of the
 *   press-Reformation causality kernel: the claim that the printing press
 *   functioned as an autonomous, physics-like enabling constraint whose
 *   mechanical properties (rapid reproduction, cost reduction, geographic
 *   reach) made vernacular scripture distribution and Reformation success
 *   effectively inevitable, independent of which specific humans operated the
 *   presses or why. Under this reading, the technology is classified as a
 *   mountain — a fixed structural feature of the information environment that
 *   emerged and operated regardless of any individual actor's intentions.
 *   This is a deliberately narrow reading: the sibling readings
 *   (strategic_deployment, co_constitution) are NOT described here except to
 *   note their existence in the kernel contest; each is its own constraint
 *   story with its own epsilon.
 *
 * KEY AGENTS:
 *   - print_shop_proprietors: primary beneficiary (moderate/mobile) — profits obscured by determinism framing
 *   - protestant_territorial_princes: primary beneficiary (institutional/arbitrage) — political consolidation obscured
 *   - vernacular_bible_publishers: beneficiary (moderate/mobile) — commercial choice recast as technological necessity
 *   - catholic_church_authorities: payer (institutional/constrained) — lost authority framed as inevitable rather than contingent
 *   - illiterate_and_semiliterate_populations: excluded (powerless/trapped) — absent from a print-centric causal account
 *   - historians_of_technology: analytical observer — evaluates competing causal claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.18).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.12).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.18).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Printing Press as Autonomous Enabling Technology (Technological Determinism Reading)").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, '549ba023-4b94-4c87-8aa6-9a0a8015c2fd').
narrative_ontology:cs_kernel_codification('549ba023-4b94-4c87-8aa6-9a0a8015c2fd', distributed).
narrative_ontology:cs_authority_grounding('549ba023-4b94-4c87-8aa6-9a0a8015c2fd', distributed).
narrative_ontology:cs_reading_relation('549ba023-4b94-4c87-8aa6-9a0a8015c2fd', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('549ba023-4b94-4c87-8aa6-9a0a8015c2fd', press_reformation_causality__co_constitution, influences).
narrative_ontology:cs_axiom('549ba023-4b94-4c87-8aa6-9a0a8015c2fd', foundational, technology_as_autonomous_causal_agent).
narrative_ontology:cs_axiom_status(technology_as_autonomous_causal_agent, holdable).
narrative_ontology:cs_axiom_grounding('549ba023-4b94-4c87-8aa6-9a0a8015c2fd', technology_as_autonomous_causal_agent, empirically_contingent).
narrative_ontology:cs_axiom('549ba023-4b94-4c87-8aa6-9a0a8015c2fd', secondary, human_intent_causally_subordinate_to_mechanical_capacity).
narrative_ontology:cs_axiom_status(human_intent_causally_subordinate_to_mechanical_capacity, holdable).
narrative_ontology:cs_axiom_grounding('549ba023-4b94-4c87-8aa6-9a0a8015c2fd', human_intent_causally_subordinate_to_mechanical_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('549ba023-4b94-4c87-8aa6-9a0a8015c2fd', print_capacity_as_fixed_physical_parameter).
narrative_ontology:cs_drift_state('549ba023-4b94-4c87-8aa6-9a0a8015c2fd', post_print_culture_historiography_revision, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('549ba023-4b94-4c87-8aa6-9a0a8015c2fd', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, print_shop_proprietors).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, protestant_territorial_princes).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, vernacular_bible_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, catholic_church_authorities).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, technological_autonomy_thesis).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, print_capitalism_inevitability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own presses and type in cities like Wittenberg, Basel, and Strasbourg; under the determinism reading they are cast as neutral conduits merely operating machinery whose outputs (pamphlet volume, vernacular Bibles) were dictated by the technology's inherent capacities rather than by their own commercial choices about what to print and for whom. This framing obscures that they profited handsomely from choosing to print Luther's tracts over competing material.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, print_shop_proprietors, beneficiary,
    moderate, biographical, mobile, regional).

% Rulers such as Frederick the Wise who protected reform-minded printers and preachers; the determinism reading attributes the spread of reform ideas to press mechanics rather than to princely patronage, tax exemptions, and armed protection that specific rulers extended to specific presses for specific political reasons, obscuring their agency and their consolidation of church lands and authority.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, protestant_territorial_princes, beneficiary,
    institutional, generational, arbitrage, continental).

% Produced and sold translated scripture at scale; framed under determinism as passive instruments of an unstoppable technological logic, when in fact publication decisions, translation commissioning, and distribution networks were deliberate commercial and theological choices that generated substantial and durable revenue streams.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, vernacular_bible_publishers, beneficiary,
    moderate, biographical, mobile, continental).

% Lost doctrinal monopoly, tithe revenue, and political authority across large territories as reform literature spread; the determinism reading frames this loss as the inevitable working of an autonomous technology, foreclosing questions about whether different institutional responses, earlier print regulation, or different alliance choices could have altered outcomes.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, catholic_church_authorities, payer,
    institutional, generational, constrained, continental).

% The majority of the European population who could not read the vernacular texts the press produced; their reception of Reformation ideas came through preaching, images, and oral transmission, not the printed page itself. Their absence from the causal story is total under a determinism reading that treats 'the press' and 'the Reformation's spread' as coextensive.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, illiterate_and_semiliterate_populations, excluded,
    powerless, biographical, trapped, regional).

% Scholars evaluating competing causal accounts of the Reformation's spread; can examine printing output data, literacy rates, and distribution networks to assess whether press capacity alone explains outcomes or whether strategic and co-constitutive factors better fit the evidence.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, historians_of_technology, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the coordination sense — this is a causal-attribution claim, not a coordination mechanism. Insofar as any coordination function is asserted, it is the claim that print technology itself standardized and distributed religious content without requiring negotiated human coordination.
% TRANSFER_FUNCTION: The determinism framing transfers explanatory credit away from specific human decision-makers (printers choosing what to print, princes choosing whom to protect, reformers choosing translation strategies) and onto an impersonal technological process — this is a transfer of causal and moral responsibility, not of material resources per se, though it also obscures the material profits captured by proprietors and publishers.
% ABSENT_VOICES: Illiterate and semiliterate populations who received Reformation ideas through non-print channels are absent from a narrative that equates 'the press' with 'the spread'; also absent are the specific commercial and political actors whose deliberate choices the determinism frame flattens into mechanical inevitability.
% DISAPPEARANCE_RATIONALE: If the technological-determinism reading of this history were abandoned, the world would not physically rearrange (the press still existed, the Reformation still happened) — but the historiographical field, curricula, and popular understanding of media's role in social change would materially shift toward crediting strategic and institutional actors, which affects how present-day claims about internet/social-media 'inevitability' are evaluated by analogy.
% FOUNDING_PROBLEM: Historians and media theorists sought to explain why the Reformation succeeded so rapidly and broadly compared to earlier heresies (Lollards, Hussites) that were suppressed; the printing press offered a clean, parsimonious causal mechanism: this time the technology was different, so the outcome was different.
% FOUNDING_PROBLEM_CORROBORATION: Media theorists in the tradition of Elizabeth Eisenstein and Marshall McLuhan corroborate the determinism framing from outside any beneficiary group. However, social historians of print culture (Adrian Johns, Andrew Pettegree) and historians of literacy argue from outside the same beneficiary set that the mechanism requires human strategic mediation and that literacy/distribution constraints make pure technological inevitability empirically unsupportable — corroboration is genuinely split among independent scholarly authorities, not merely among interested parties.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, contested).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__technological_determinism, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causality__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causality__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causality__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) and suppression low (0.12) because, taken as a claim about physical/mechanical capacity, the press genuinely did lower reproduction costs and increase distribution speed in ways not contingent on any single actor's will — this is the mountain-like kernel of truth in the reading. Accessibility collapse is high (0.78) because once movable type existed, hand-copying manuscripts became commercially uncompetitive almost everywhere it was introduced — a genuine technological ratchet. Resistance is moderate (0.35): the Church did resist (indices, licensing, occasional press seizures) but could not resist the underlying mechanical fact of reduced reproduction cost, only its specific applications. Theater ratio rises modestly over the interval (0.10 to 0.28) as post-hoc determinism narratives accumulate retrospective certainty not present in contemporaries' own accounts of contested, contingent choices.
 *
 * PERSPECTIVAL GAP:
 *   From the analytical observer's seat, the determinism claim and the strategic-deployment claim describe different structural objects even though they discuss the 'same' historical events — this is exactly the epsilon-invariance case: change the observable (mechanical capacity vs. deliberate strategic choice) and you get a different constraint, not a different view of one constraint. The payer seat (Catholic authorities) experiences the outcome as loss regardless of causal attribution, but WHICH reading is authoritative determines whether that loss is narrated as unavoidable natural process or contestable political defeat — with consequences for how later media-technology-inevitability arguments are evaluated by analogy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (proprietors, princes, publishers) are declared because they capture real material and political gains, but the determinism reading's own logic minimizes their agency — they appear as conduits of an autonomous process rather than strategic actors, which is precisely the obscuring effect the kernel context flags. This creates an internal tension the story surfaces rather than resolves: metrics describe a low-extraction mountain-like technological fact, while the declared beneficiary list gestures at the FSM-adjacent concern that 'inevitability' framing conveniently launders windfall gains as unchosen consequences of physics.
 *
 * MANDATROPHY ANALYSIS:
 *   The determinism reading's classification as mountain is precisely the kind of claim the false-summit-mountain signature exists to catch: a mountain claim that carries declared beneficiaries is a candidate for reclassification once the coordination/extraction structure underneath the naturalized story is examined. Declaring beneficiaries here is intentional FSM-candidate authoring, not an oversight — the omega variables below carry the full ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_or_constructed_inevitability_narrative,
    'Is the press''s enabling effect a genuine mountain (a mechanical/economic fact independent of who used it) or a constructed inevitability narrative that retroactively naturalizes choices made by identifiable beneficiaries (proprietors, princes, publishers) who profited from specific deployment decisions?',
    'Comparative case analysis: examine regions/periods where presses existed but reform literature was suppressed or failed to spread (e.g., France, Spain, parts of Italy) versus regions where it succeeded (Germany, Low Countries, England). If press presence alone predicts reform spread regardless of political protection, determinism gains support; if political/strategic variables better predict outcomes than press density, the mountain framing is a constructed cover for strategic and beneficiary-driven choices.',
    'If resolved toward constructed narrative, this constraint should be reclassified from mountain toward tangled_rope or snare, with the declared beneficiaries recognized as active strategic agents rather than passive conduits — this is exactly the FSM reclassification path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_or_constructed_inevitability_narrative, conceptual, 'Core ambiguity: genuine technological mountain vs. constructed inevitability narrative serving identifiable beneficiaries.').

omega_variable(
    literacy_ceiling_on_determinism,
    'Given that the majority of the 16th-century European population was illiterate or semiliterate, how can a purely print-driven determinism account for the Reformation''s spread among populations who could not read the vernacular texts being printed?',
    'Literacy-rate data cross-referenced with regional reform adoption timing; oral transmission and preaching-network historical records.',
    'If reform spread substantially through oral/preaching channels independent of print literacy, the determinism reading''s causal sufficiency claim weakens significantly, supporting the co_constitution reading instead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literacy_ceiling_on_determinism, empirical, 'Whether print-based determinism can account for reform''s reach into illiterate populations.').

omega_variable(
    committer_framing_selection,
    'Why was the technological_determinism framing selected as the reading being generated here rather than treating the underlying historical events as a single under-determined claim?',
    'This is answered structurally by the kernel-context assignment: the manifest explicitly tasked this generation with the determinism reading, with strategic_deployment and co_constitution assigned as separate sibling stories. No further empirical resolution is needed for the framing-selection question itself, though the substantive question of which reading best fits the evidence (see the two omegas above) remains open.',
    'Clarifies that this story''s mountain classification is a property of the reading, not an all-things-considered verdict on Reformation historiography — the sibling stories carry different classifications for the same underlying events viewed through different causal-attribution lenses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_selection, conceptual, 'Documents the committer-frame selection and its relationship to sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causality__technological_determinism, theater_ratio, 1450, 0.1).
narrative_ontology:measurement(pres_tr_t1480, press_reformation_causality__technological_determinism, theater_ratio, 1480, 0.14).
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causality__technological_determinism, theater_ratio, 1517, 0.2).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causality__technological_determinism, theater_ratio, 1540, 0.24).
narrative_ontology:measurement(pres_tr_t1570, press_reformation_causality__technological_determinism, theater_ratio, 1570, 0.26).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causality__technological_determinism, theater_ratio, 1600, 0.28).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causality__technological_determinism, base_extractiveness, 1450, 0.08).
narrative_ontology:measurement(pres_be_t1480, press_reformation_causality__technological_determinism, base_extractiveness, 1480, 0.1).
narrative_ontology:measurement(pres_be_t1517, press_reformation_causality__technological_determinism, base_extractiveness, 1517, 0.14).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causality__technological_determinism, base_extractiveness, 1540, 0.16).
narrative_ontology:measurement(pres_be_t1570, press_reformation_causality__technological_determinism, base_extractiveness, 1570, 0.17).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__technological_determinism, base_extractiveness, 1600, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causality__technological_determinism, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the press_reformation_causality kernel. technological_determinism (this file) treats print capacity as a fixed, autonomous, mountain-like parameter and human actors as downstream responders, which obscures the beneficiary structure of proprietors, princes, and publishers. strategic_deployment treats the same historical events as the product of deliberate strategic choices by reformers and printers, which would likely classify closer to tangled_rope or rope given active coordination and identifiable strategic beneficiaries. co_constitution treats technology and agency as mutually formative through feedback loops, which resists a single-index classification and may require its own hybrid treatment. All three share the underlying historical record but assign different causal weight to technology versus human agency, producing genuinely different epsilon values — this is the epsilon-invariance principle in action: same label ('the printing press caused the Reformation'), three structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
