% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__commons_conservation, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ost_article_ii_non_appropriation__commons_conservation
 *   human_readable: Outer Space Treaty Article II Non-Appropriation Principle (Commons-Conservation Reading)
 *   domain: international_law/space_governance/commons
 *
 * SUMMARY:
 *   This story instantiates the commons-conservation reading of Article II of
 *   the 1967 Outer Space Treaty: that the 'use or occupation' prohibition on
 *   appropriation extends beyond formal territorial claims to cover de facto
 *   appropriation achieved through resource extraction, and that this
 *   non-appropriation principle binds private actors as well as states
 *   (closing the loophole that a state cannot claim sovereignty but its
 *   nationals could still extract and own resources freely). Under this
 *   reading, large-scale extractive activity without multilateral
 *   authorization is itself a prohibited act of appropriation, not merely a
 *   permitted private-property transaction occurring beneath a
 *   still-unclaimed territory. This is a Tangled Rope: it genuinely
 *   coordinates against a first-mover enclosure race (real coordination
 *   function protecting non-spacefaring states' future access) while imposing
 *   asymmetric extraction on states and firms that have already committed
 *   capital to extraction under the competing (extraction-permissive)
 *   reading. The enforcement is diplomatic and reputational rather than
 *   juridical in the traditional sense — there is no space court — but it
 *   operates through denial of legal certainty, financing, and insurance
 *   markets, and through the slow pace of the COPUOS multilateral
 *   authorization process functioning as a de facto moratorium.
 *
 * KEY AGENTS:
 *   - non_spacefaring_states: Primary beneficiary (organized/analytical) — retains veto leverage over enclosure under this reading
 *   - private_asteroid_mining_ventures: Primary target (moderate/constrained) — capital committed under a competing legal theory, now stranded
 *   - first_mover_spacefaring_states: Secondary target (powerful/constrained) — domestic legislation asserting extraction rights undermined by this reading's authority
 *   - multilateral_treaty_framework: Institutional agenda-setter (institutional/analytical) — administers and elaborates the reading through COPUOS practice
 *   - commercial_lunar_resource_developers: Secondary target (moderate/constrained) — in-situ resource utilization plans face legal uncertainty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.42).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.58).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.42).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "Outer Space Treaty Article II Non-Appropriation Principle (Commons-Conservation Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international_law/space_governance/commons").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, '6376d86e-0f5c-458e-b189-f3b49aa64340').
narrative_ontology:cs_kernel_codification('6376d86e-0f5c-458e-b189-f3b49aa64340', fixed_text).
narrative_ontology:cs_authority_grounding('6376d86e-0f5c-458e-b189-f3b49aa64340', distributed).
narrative_ontology:cs_reading_relation('6376d86e-0f5c-458e-b189-f3b49aa64340', ost_article_ii_non_appropriation__extraction_permissive, forecloses).
narrative_ontology:cs_reading_relation('6376d86e-0f5c-458e-b189-f3b49aa64340', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('6376d86e-0f5c-458e-b189-f3b49aa64340', foundational, de_facto_extraction_equals_appropriation).
narrative_ontology:cs_axiom_status(de_facto_extraction_equals_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('6376d86e-0f5c-458e-b189-f3b49aa64340', de_facto_extraction_equals_appropriation, conventional).
narrative_ontology:cs_axiom('6376d86e-0f5c-458e-b189-f3b49aa64340', foundational, non_appropriation_binds_private_actors_directly).
narrative_ontology:cs_axiom_status(non_appropriation_binds_private_actors_directly, holdable).
narrative_ontology:cs_axiom_grounding('6376d86e-0f5c-458e-b189-f3b49aa64340', non_appropriation_binds_private_actors_directly, conventional).
narrative_ontology:cs_reference_frame('6376d86e-0f5c-458e-b189-f3b49aa64340', common_heritage_commons_framework).
narrative_ontology:cs_drift_state('6376d86e-0f5c-458e-b189-f3b49aa64340', post_artemis_accords_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6376d86e-0f5c-458e-b189-f3b49aa64340', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, future_generations_of_spacefaring_capacity).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, multilateral_treaty_framework).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, private_asteroid_mining_ventures).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, first_mover_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, commercial_lunar_resource_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold no near-term capacity to extract space resources themselves but retain a treaty-conferred veto over unilateral enclosure by capable states or firms. Their leverage exists entirely inside this reading of Article II — if extraction is read as legally clear absent their consent, their bargaining position collapses. They actively press this interpretation at UN COPUOS and in General Assembly resolutions.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, beneficiary,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, agenda_setter).

% Have raised capital and developed extraction technology premised on eventually monetizing off-world resources. Under this reading, their claims to extracted material lack legal protection until a multilateral authorization regime exists, which stalls financing, insurance, and offtake agreements. They cannot exit the treaty system (no state will shield extraction activity from the non-appropriation norm) and cannot proceed without accepting stranded-asset risk.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, private_asteroid_mining_ventures, payer,
    moderate, biographical, constrained, global).

% Have passed domestic legislation (e.g., asserting rights of their nationals to retain extracted resources) betting on the extraction-permissive reading prevailing in practice. Under the commons-conservation reading their domestic statutes are treated as legally insufficient to license extraction, and they face reputational and diplomatic costs for proceeding unilaterally. They cannot simply exit the treaty regime without abandoning the broader cooperative benefits of OST membership (orbital coordination, liability regime, non-weaponization norms).
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, first_mover_spacefaring_states, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__commons_conservation, first_mover_spacefaring_states, excluded).

% Plan in-situ resource utilization (water ice, regolith processing) for lunar bases and propellant depots. This reading treats large-scale, exclusive-use extraction as functionally equivalent to appropriation, creating legal uncertainty over whether their planned operations require prior multilateral sign-off they have no clear path to obtaining.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, commercial_lunar_resource_developers, payer,
    moderate, biographical, constrained, global).

% The UN COPUOS process and the treaty's interpretive community administer and elaborate this reading through resolutions, working groups, and diplomatic practice. It has authority to slow-walk or block emergence of an authorization regime, and its slow deliberative pace is itself a mechanism by which the conservation reading is operationalized as a de facto moratorium.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, multilateral_treaty_framework, agenda_setter,
    institutional, civilizational, analytical, universal).

% Not yet capable of asserting interests directly; the reading's rationale is that preserving an un-appropriated commons keeps resource access open to states and peoples who currently lack spacefaring capability but may develop it later. Listed for completeness as the temporally displaced beneficiary class the reading is meant to protect.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, future_generations_of_spacefaring_capacity, beneficiary,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__commons_conservation, future_generations_of_spacefaring_capacity).

% Argue that unregulated extraction risks environmental harm to celestial bodies and orbital congestion, but have no formal seat in the appropriation debate, which is dominated by states and commercial actors; their concerns are absorbed rhetorically into the commons framing without being given independent standing.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, orbital_debris_and_environmental_stewardship_advocates, excluded,
    powerless, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a first-mover race to unilaterally enclose lunar and asteroidal resources by requiring that large-scale, exclusive extraction be authorized through a multilateral process, preserving equal legal access for states that currently lack extraction capability.
% TRANSFER_FUNCTION: Moves decision authority over resource access from capable extracting actors (states and firms with the technology to mine) to the collective body of treaty parties, and defers realized economic value from present first-movers to a negotiated future distribution.
% ABSENT_VOICES: Commercial insurers, financiers, and future space-resource consumers who bear the cost of legal uncertainty are not party to COPUOS negotiations; environmental stewardship advocates for celestial bodies have no formal standing in the appropriation debate.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned, private ventures and first-mover states would treat extracted resources as legally securable without multilateral sign-off, unlocking financing and insurance markets for space mining and ending non-spacefaring states' veto leverage over enclosure — a substantial reallocation of both capital flows and diplomatic power.
% FOUNDING_PROBLEM: Cold War-era concern that superpowers would extend terrestrial colonial land-grab dynamics into space, converting technological and military capability into permanent territorial or resource control over celestial bodies to the exclusion of the rest of humanity.
% FOUNDING_PROBLEM_CORROBORATION: Non-spacefaring states and UN legal scholars outside the commercial space sector attest the founding concern remains live as extraction capability becomes real for the first time since 1967. Industry associations and first-mover states' space agencies contest this, arguing the founding problem was about territorial sovereignty claims specifically, not resource extraction, and that the conservation reading retrofits a broader prohibition the treaty drafters did not intend — this dispute is itself the kernel contest this story is one reading of.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__commons_conservation, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__commons_conservation_tests).
:- end_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 — moderate rather than extreme, because this reading genuinely coordinates a real collective-action problem (preventing enclosure races) even as it imposes real costs on committed extraction ventures; it is not pure extraction dressed as coordination. Suppression is authored higher (0.58) because the mechanism by which the reading operates — denying legal certainty, blocking insurance and financing markets, and using deliberative slowness as a de facto moratorium — is a genuine active-suppression mechanism against a specific class of activity (unilateral extraction), not merely passive non-endorsement. Theater ratio sits at a moderate 0.4: much of the COPUOS process (working groups, non-binding resolutions, repeated restatement of principles without binding authorization framework) increasingly resembles procedural theater relative to producing an actual authorization regime, and this has risen over the measured interval as extraction technology has matured faster than the diplomatic process.
 *
 * PERSPECTIVAL GAP:
 *   From the non-spacefaring state seat, this reading is coordination succeeding exactly as designed — protecting a commons against enclosure by the capable. From the first-mover state and private venture seat, the identical structure computes as extraction: capital and technological investment made in good faith under a plausible alternative legal reading is stranded by an interpretive move that arrived after the investment decision. The engine should show this seat divergence directly from the beneficiary/victim declarations and exit constraints, not from any authored type label.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-spacefaring states and the future-capacity beneficiary class sit near the beneficiary end of directionality: the reading's entire function is to preserve their optionality against loss to capable first-movers, and they bear essentially no cost from the current deadlock. First-mover states, private ventures, and lunar developers sit toward the target end: they have made capital commitments premised on extraction being legally securable, and this reading directly frustrates that expectancy, with 'constrained' rather than 'trapped' exit because they retain other business lines and could theoretically defect from OST membership at severe diplomatic cost (hence not fully trapped, but not mobile either).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Cold War colonial land-grab anxiety) is contested as either live or dead depending on which reading of the kernel one holds. This reading treats the founding problem as live and directly applicable to modern resource extraction — the fear of asymmetric capability converting into permanent exclusive control is structurally identical whether the mechanism is a flag-planting claim or an extraction operation that functionally excludes others from a resource. The classification as tangled_rope (rather than snare) turns on this being a genuine coordination story that a specific class of actor is asked to bear the cost of, not a naked extraction dressed as principle — the coordination function (preserving universal future access) is real and would be lost if the reading were abandoned, which is what distinguishes it from pure rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    de_facto_appropriation_threshold_ambiguity,
    'At what scale or exclusivity of resource extraction does an operation cross from permissible ''use'' into prohibited ''de facto appropriation'' under this reading?',
    'A binding interpretive instrument (ICJ advisory opinion, or consensus COPUOS resolution with state practice backing) establishing thresholds — e.g., distinguishing sample-return science missions from continuous industrial-scale extraction claiming exclusive operational zones.',
    'A narrow threshold (only large-scale industrial claims count) would substantially lower this reading''s effective extraction on smaller-scale actors; a broad threshold (any exclusive-use extraction counts) would raise it further and more clearly stall the entire commercial space-resource sector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_facto_appropriation_threshold_ambiguity, conceptual, 'Where the extraction/appropriation line sits under the conservation reading is itself undetermined.').

omega_variable(
    committer_framing_alternative_readings,
    'This story instantiates the commons_conservation reading of the ost_article_ii_non_appropriation kernel. Two sibling readings — extraction_permissive and international_regime — instantiate structurally different constraints from the same treaty text. Which reading actually governs state practice going forward?',
    'State practice and opinio juris accumulation: if states and firms increasingly proceed with extraction without multilateral authorization and face no meaningful diplomatic or legal consequence, the extraction_permissive reading is prevailing in practice regardless of legal commentary favoring conservation; if a binding Article XI-analogue authorization regime is negotiated and adopted, the international_regime reading has displaced both unilateral readings.',
    'If extraction_permissive prevails in practice, this story''s high suppression score becomes a description of a norm losing its grip rather than a norm successfully enforced — the classification would drift from tangled_rope toward a contested/weakening rope. If international_regime prevails, this story''s premise (that Article II itself settles the extraction question) is superseded by the emergence of the very multilateral framework this reading calls for, potentially resolving the tangled_rope into a genuine rope once the authorization regime exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_alternative_readings, conceptual, 'The kernel contest between three readings of Article II''s appropriation language remains open; this story''s ε and classification are stable only relative to the commons-conservation reading, not across the kernel.').

omega_variable(
    private_actor_coverage_ambiguity,
    'Does Article II''s non-appropriation principle bind private actors directly, or only states (with private conduct attributable to states under Article VI''s authorization-and-supervision requirement)?',
    'Domestic court rulings testing the validity of national space resource statutes (e.g., US Commercial Space Launch Competitiveness Act, Luxembourg''s space resources law) against Article II, or an authoritative multilateral interpretive statement.',
    'If private actors are not directly bound (only states, via Article VI), the practical suppression this reading exerts on private ventures weakens considerably, since national authorization could substitute for multilateral authorization — pushing this reading''s effective classification toward the extraction_permissive sibling''s territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_actor_coverage_ambiguity, empirical, 'Whether the non-appropriation principle''s private-actor coverage (the second half of this reading''s title) is itself a settled or contested legal question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(ost__tr_t1979, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1979, 0.28).
narrative_ontology:measurement(ost__tr_t1998, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1998, 0.32).
narrative_ontology:measurement(ost__tr_t2015, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(ost__tr_t2025, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1967, 0.15).
narrative_ontology:measurement(ost__be_t1979, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1979, 0.22).
narrative_ontology:measurement(ost__be_t1998, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1998, 0.26).
narrative_ontology:measurement(ost__be_t2015, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2020, 0.39).
narrative_ontology:measurement(ost__be_t2025, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1967, 0.3).
narrative_ontology:measurement(ost__su_t1979, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1979, 0.4).
narrative_ontology:measurement(ost__su_t1998, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1998, 0.45).
narrative_ontology:measurement(ost__su_t2015, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(ost__su_t2025, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, resource_allocation).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__commons_conservation, 0.15).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__international_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the ost_article_ii_non_appropriation kernel (the contested meaning of Article II's 'use or occupation' language). commons_conservation (this story) authors extraction as prohibited absent multilateral authorization, covering private actors, ε=0.42, tangled_rope. extraction_permissive authors a narrower prohibition covering only sovereign territorial claims, permitting private resource ownership, with a substantially lower ε and a rope-leaning classification. international_regime authors the question as unresolved pending a future Article XI-analogue framework, with ε reflecting genuine legal uncertainty rather than either substantive position. All three share the same treaty text and interval but produce different beneficiary/victim structures and different ε values — per the ε-invariance principle, this is three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
