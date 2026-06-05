% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__beneficiary_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__beneficiary_agency_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technology_reformation_causality__beneficiary_agency_reading
 *   human_readable: Reformer-Printer Coalition Authority Bypass (Beneficiary Agency Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint models the Reformation-printing relationship from the
 *   beneficiary agency reading: reformers and printers deployed printing
 *   strategically to bypass Church doctrinal authority. The constraint is a
 *   tangled_rope because the reformer-printer coalition exhibits genuine
 *   coordination (both solve a mutual problem — reformers need mass
 *   distribution, printers need sustainable markets) paired with asymmetric
 *   extraction from the Church's monopoly on scriptural interpretation and
 *   manuscript production. Technology functions as a scaffold — a temporary
 *   infrastructure enabling the coalition's strategic goal — not as an
 *   autonomous cause. The core analytical claim: causality flows from the
 *   coalition's deliberate choice to use printing as a strategic tool, not
 *   from the printing press determining the coalition's formation or success.
 *   This reading contests two alternatives: the technological determinism
 *   reading (printing made Reformation inevitable) and the co-constitution
 *   reading (technology and actors mutually shaped each other). All three
 *   readings are historiographically live; they instantiate different causal
 *   narratives about the same historical process and are formalized as
 *   separate constraint stories with different structural signatures.
 *
 * KEY AGENTS:
 *   - Reformer Networks (institutional/arbitrage): Primary strategic beneficiary — deliberately organized printing adoption to bypass Church authority. Experienced printing as enabling their core function (vernacular scripture distribution at scale). Net beneficiary with substantial agency.
 *   - Printer Consortia (institutional/arbitrage): Co-beneficiary — shared reformer demand and profit motive. Printing religious texts solved scale and market problems for printers; reformers solved demand problem for printers. Mutual coordination benefit.
 *   - Church Doctrinal Authority (powerless/trapped): Primary victim — extraction target. Experiences the reformer-printer coalition as deliberately circumventing its scriptural monopoly and interpretive gatekeeping. No exit options available; must escalate suppression (censorship, book banning).
 *   - Secondary Reform Communities (moderate/constrained): Secondary victims and beneficiaries. Benefit from expanded scripture access but constrained by reliance on printer networks and exposure to competing theological interpretations. Mixed experience.
 *   - Printing Technology as Scaffold (organized/mobile): Mediating infrastructure with no autonomous agency. Functions as a tool deployed strategically by the coalition. Low intrinsic extraction; high functional value to coalition. Sunset clause implicit once coalition achieves strategic goals.
 *   - Analytical Observer (analytical/analytical): Sees the beneficiary agency structure — reformers and printers as agents choosing printing strategically, not as passive beneficiaries of technological determinism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, 0.52).
domain_priors:suppression_score(technology_reformation_causality__beneficiary_agency_reading, 0.62).
domain_priors:theater_ratio(technology_reformation_causality__beneficiary_agency_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformer-Printer Coalition Authority Bypass (Beneficiary Agency Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, '67075889-2758-40d6-bab4-f987b255fd68').
narrative_ontology:cs_kernel_codification('67075889-2758-40d6-bab4-f987b255fd68', fixed_text).
narrative_ontology:cs_authority_grounding('67075889-2758-40d6-bab4-f987b255fd68', lineage).
narrative_ontology:cs_interpretation_layer_present('67075889-2758-40d6-bab4-f987b255fd68').
narrative_ontology:cs_reading_relation('67075889-2758-40d6-bab4-f987b255fd68', technology_reformation_causality__technological_determinism_reading, coexists_with).
narrative_ontology:cs_reading_relation('67075889-2758-40d6-bab4-f987b255fd68', technology_reformation_causality__co_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('67075889-2758-40d6-bab4-f987b255fd68', foundational, reformer_printer_strategic_intent).
narrative_ontology:cs_axiom_status(reformer_printer_strategic_intent, holdable).
narrative_ontology:cs_axiom_grounding('67075889-2758-40d6-bab4-f987b255fd68', reformer_printer_strategic_intent, empirically_contingent).
narrative_ontology:cs_axiom('67075889-2758-40d6-bab4-f987b255fd68', foundational, technology_as_tool_not_cause).
narrative_ontology:cs_axiom_status(technology_as_tool_not_cause, holdable).
narrative_ontology:cs_axiom_grounding('67075889-2758-40d6-bab4-f987b255fd68', technology_as_tool_not_cause, deontological).
narrative_ontology:cs_reference_frame('67075889-2758-40d6-bab4-f987b255fd68', church_doctrinal_monopoly).
narrative_ontology:cs_drift_state('67075889-2758-40d6-bab4-f987b255fd68', post_mass_printing_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('67075889-2758-40d6-bab4-f987b255fd68', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, reformer_networks).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, printer_consortia).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, church_doctrinal_monopoly).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, manuscript_production_hierarchy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHURCH DOCTRINAL AUTHORITY (SNARE) — Trapped by the coalition's deliberate strategy to bypass its monopoly on scriptural interpretation. The printing-reformer nexus extracts authority from the Church without providing coordination benefit. The Church perceives this as pure extraction: vernacular scripture distribution circumvents its authentication and interpretive gatekeeping. No exit option; suppression mechanisms (censorship, book banning, excommunication) must escalate.
constraint_indexing:constraint_classification(technology_reformation_causality__beneficiary_agency_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SECONDARY REFORM COMMUNITIES (TANGLED ROPE) — Constrained by reliance on printer networks for dissemination while also benefiting from expanded access to scriptural texts. Experience both extraction (printers extract rents; reformers may enforce orthodoxy within communities) and genuine coordination (scripture access, community formation, theological debate). Exit options are constrained — suppression is high but not total; some communities can establish alternative manuscript networks.
constraint_indexing:constraint_classification(technology_reformation_causality__beneficiary_agency_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REFORMER LEADERSHIP (ROPE) — Primary strategic beneficiary. Experiences printing as pure coordination: the technology enables the coalition's core function (bypassing Church authority through mass text distribution). Reformers deliberately orchestrated printer engagement; the constraint is their innovation. Exit options include arbitrage — could revert to manuscript networks if printing became suppressed, but retain theological authority. Net beneficiary with agency.
constraint_indexing:constraint_classification(technology_reformation_causality__beneficiary_agency_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PRINTER CONSORTIA (ROPE) — Co-beneficiary with reformers. Experiences the constraint as profitable coordination: mass production of religious texts (vernacular scripture, reform tracts) at scale they could not achieve with commission-based manuscript copying. Reformers provide steady demand; printers provide logistics. Genuine mutual benefit. Exit options: could return to traditional manuscript work or shift to secular texts if reform market contracted.
constraint_indexing:constraint_classification(technology_reformation_causality__beneficiary_agency_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: PRINTING TECHNOLOGY AS SCAFFOLD (ORGANIZED/MOBILE) — Technology functions as temporary infrastructure enabling the coalition strategy. The press is a tool deployed deliberately, not an autonomous cause. Effective extraction is low (χ ≤ 0.30) because the technology has no agency or beneficiary of its own — it mediates the reformer-printer coordination. Theater is low (0.48) — the printing mechanism is functional, not performative. The sunset clause is implicit: once the reformer-printer coalition achieves its strategic goal (vernacular scripture distribution at scale), the technology's special role degrades into routine commodity production. Technology per se does not persist as an extractive mechanism post-Reformation.
constraint_indexing:constraint_classification(technology_reformation_causality__beneficiary_agency_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — BENEFICIARY AGENCY READING (TANGLED ROPE) — From the civilizational view, this reading emphasizes that reformers and printers were not merely passive beneficiaries of technology but active agents deploying printing as a strategic tool to bypass Church authority. The constraint exhibits genuine coordination (reformers and printers solving a mutual problem) paired with asymmetric extraction (both extract authority and profit from the Church's doctrinal monopoly). Technology is the scaffold, not the cause. The reading's core claim: causality flows from the coalition's agency through the technology, not from technology determining the coalition's agency.
constraint_indexing:constraint_classification(technology_reformation_causality__beneficiary_agency_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(technology_reformation_causality__beneficiary_agency_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technology_reformation_causality__beneficiary_agency_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε = 0.52): Moderate-high. The reformer-printer coalition extracts substantial value from the Church's doctrinal monopoly — they bypass its authority, reduce its gatekeeping power, and establish vernacular scripture as legitimate outside Church-controlled interpretation. However, ε is not higher (like a snare) because genuine coordination benefits exist for both reformers and printers. The extraction is from the Church, not from secondary reform communities (though some extraction from the interpretive commons may occur). The value 0.52 reflects the tangled_rope threshold: extraction ≥ 0.30, coordination function present, asymmetry evident. Suppression (σ = 0.62): High. The Church escalates suppression mechanisms — book banning, censorship, excommunication, direct control of printing — because the coalition's strategy is explicitly designed to evade its authority. Suppression is necessary to maintain the Church's monopoly; the coalition's threat justifies escalation. Theater ratio (τ = 0.48): Moderate-low. The printing mechanism itself is functional rather than performative — books are actually produced and distributed, not merely shown. The coalition's strategy is transparent (they are deliberately bypassing Church authority, not hiding behind elaborate justifications). Theater rises slightly over the interval (0.38 → 0.48) as printing becomes routinized and some performative infrastructure (legitimacy narratives, printer networks) develops, but remains below the piton threshold (0.70).
 *
 * PERSPECTIVAL GAP:
 *   The reformer-printer coalition perceives printing as pure coordination (Rope) — a technology enabling their shared goal. The Church perceives it as pure extraction (Snare) — a threat to its authority with no coordination benefit. Secondary reform communities experience both coordination (scripture access) and extraction (dependence on printer networks, interpretive instability). The printing technology itself, viewed as an agent, functions as a Scaffold — temporary infrastructure enabling coalition strategy with no agency of its own. The civilizational analytical observer (this reading) sees the tangled_rope structure: mutual benefit between reformers and printers paired with authority extraction from the Church. The perspectival gap reveals how the same structural phenomenon — the strategic deployment of printing to bypass Church authority — is experienced differently depending on the observer's structural position relative to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status and exit options. Reformers (beneficiary, arbitrage exit) experience low d (0.15–0.20) because they can exit to manuscript networks if printing were suppressed, and they benefit from the printing strategy. Printers (beneficiary, arbitrage exit) experience similarly low d for the same reasons. The Church (victim, trapped exit) experiences high d (0.85–0.95) because it cannot exit the threat to its authority and bears the cost of doctrinal monopoly erosion. Secondary reform communities (mixed victim/beneficiary, constrained exit) experience moderate d (0.50–0.60) because they benefit from scripture access but are constrained by reliance on printer networks and cannot easily revert to Church-controlled interpretations. The analytical observer (analytical exit) derives d from the structural topology (0.72–0.73 canonical, adjusted for explicit analysis of the coalition's agency). The formula χ = ε × f(d) × σ(S) produces effective extraction values that reflect these structural asymmetries: high chi for the Church (high d, high f(d)), moderate chi for secondary communities (moderate d), near-zero or negative chi for reformers and printers (low d, negative f(d)).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the tangled_rope classification: the constraint exhibits both genuine coordination (reformers and printers solving mutual problems) and asymmetric extraction (authority bypass from the Church). The reading avoids the false dichotomy of 'Is this pure technology or pure agency?' by modeling the technological relationship as a scaffold — temporary infrastructure enabling coalition strategy. The beneficiary agency axiom (reformers chose printing strategically) is foundational; the reading stands or falls on evidence of strategic intent vs opportunistic adoption. If strategic intent is established, tangled_rope is confirmed and the reading's core claim holds. If opportunistic adoption is shown, the co-constitution or technological determinism readings gain plausibility. The mandatrophy is not resolved by choosing one type; it is resolved by mapping the reading's structural claim (agency-driven coalition strategy) to a specific constraint type (tangled_rope) with specific beneficiary/victim asymmetries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_intent_evidence,
    'Did reformers and printers deliberately coordinate to use printing strategically, or did they exploit printing''s capabilities after discovering them opportunistically?',
    'Historiographic evidence: dates of printing adoption relative to reform theology development; correspondence between reformers and printers showing explicit coordination vs opportunistic adoption; analysis of which texts were prioritized first (strategic vs random).',
    'If strategic: confirms tangled_rope / scaffold reading (agency-driven). If opportunistic: suggests technological affordance discovery (co-constitution or technological determinism reading more plausible). If mixed: agency and discovery both operative (touches all readings).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strategic_intent_evidence, empirical, 'Degree of strategic intent vs opportunistic adaptation in reformer-printer coordination').

omega_variable(
    determinism_counterfactual,
    'Could the Reformation have occurred without printing press technology, and to what extent?',
    'Comparative historical analysis: earlier reform movements that lacked printing (Lollards, Hussites) and their scale/persistence vs post-printing movements. Analysis of whether reformers'' theological innovations were contingent on print distribution or preceded it. Examination of manuscript-based dissemination networks'' capacity.',
    'If Reformation could have occurred without printing: technological determinism refuted; beneficiary agency reading strengthened (printing was tool, not cause). If impossible: determinism supported; technology reading gains plausibility. If partial: co-constitution reading gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(determinism_counterfactual, empirical, 'Whether Reformation was possible without printing technology').

omega_variable(
    reading_contest_kernel,
    'Which reading of the reformation-printing relationship is instantiated by this constraint — beneficiary agency, co-constitution, or technological determinism?',
    'By design: this constraint instantiates ONLY the beneficiary agency reading. Sibling readings are separate constraint_ids with their own ε values, perspectives, and structural claims. This omega documents the contest itself (rule 2: route committer structure to omegas). The three readings coexist as live historiographic positions; none forecloses the others within contemporary academic discourse.',
    'The three readings produce different ε values and different constraint types. Beneficiary agency (this reading): ε=0.52, tangled_rope. Technological determinism (sibling): ε would be lower (printing as exogenous cause, less extraction, more coordination). Co-constitution (sibling): ε would be moderate (mutual shaping, mixed extraction). The contest is not empirical ambiguity but different causal narratives instantiated as structurally distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_kernel, conceptual, 'This constraint is one reading of a contested kernel with three sibling readings instantiated as separate constraint_ids').

omega_variable(
    extraction_flow_direction,
    'Does the reformer-printer coalition extract value from the Church, or does it extract value FROM THE COMMONS (scripture, knowledge) to benefit both reformers and printers?',
    'Analysis of who bears costs: the Church (authority erosion, competitive threat) vs the reading populace (disruption of traditional literacy practices, theological confusion, exposure to heretical interpretations). Examination of benefit distribution: do secondary reform communities gain access to scripture (benefit) or merely substitute for Church-controlled scriptural interpretation (extraction from the interpretive commons).',
    'If extraction flows Church-ward: beneficiary agency reading confirmed (coalition extracts authority). If extraction flows commons-ward: the reading may actually describe a constraint where the coalition extracts from the epistemic commons (meaning: the reading''s type might degrade to snare under this interpretation). If mixed: tangled_rope confirmed (both flows operative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_flow_direction, empirical, 'Direction and target of extractive flows in the reformer-printer coalition').

omega_variable(
    technology_substitutability,
    'Was the printing press uniquely necessary for the reformer strategy, or could alternative technologies (manuscript networks, oral networks, secret copying) have achieved similar bypass effects?',
    'Historical analysis of actual manuscript network speed vs print production speed; evidence of whether reformers sought printing specifically or adopted it when available; counterfactual scenarios with alternative technologies at the same cost/speed profile.',
    'If press was substitutable: strengthens technological determinism reading (technology was contingent, not determining). If press was uniquely superior: strengthens beneficiary agency reading (strategic choice of the best available tool). If neither: emphasizes that technology was a scaffold (medium-level enabler, not determining factor).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_substitutability, empirical, 'Whether printing press was uniquely necessary or substitutable for reformer strategy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reform_agency_tr_t0, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(reform_agency_tr_t15, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(reform_agency_tr_t30, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(reform_agency_be_t0, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(reform_agency_be_t15, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(reform_agency_be_t30, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(reform_agency_su_t0, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(reform_agency_su_t15, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(reform_agency_su_t30, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% The three readings of the reformation-printing relationship (beneficiary agency, technological determinism, co-constitution) are instantiated as structurally distinct constraints because they produce different ε values and different beneficiary/victim structures. Beneficiary agency (this file): ε=0.52, tangled_rope, reformers and printers deliberate agents. The sibling readings have their own ε values and structural claims. Network edges link the three readings as a constraint family instantiating the kernel contest. Not separate observables of the same constraint — separate constraints generated by different causal narratives applied to the same historical process.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_reformation_causality__beneficiary_agency_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
