% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__beneficiary_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   Between roughly 1440 and 1550, reformers and printing entrepreneurs
 *   deployed the printing press strategically to bypass ecclesiastical
 *   authority's monopoly on textual authority. This reading holds that the
 *   constraint's persistence derives not from printing technology's inherent
 *   affordances (the determinism reading) or from mutual shaping between
 *   actors and technology (the co-constitution reading), but from deliberate,
 *   coordinated agency by two groups with aligned short-term interests and
 *   divergent long-term ones: reformers seeking doctrinal authority outside
 *   Rome, printers seeking profitable markets outside guild monopolies. The
 *   extraction is mutual — reformers use printers to broadcast heterodox
 *   theology, printers use reformers' controversial content to build customer
 *   bases and profit margins — and both face suppression from ecclesiastical
 *   and mercantile authorities working to maintain their respective
 *   monopolies. The constraint is claimed as tangled_rope because both groups
 *   benefit from the arrangement and both pay through active enforcement
 *   pressure.
 *
 * KEY AGENTS:
 *   - reform_theologians: Agents (Luther, Zwingli, later Calvin) seeking doctrinal authority independent from papal Rome; coordinate with printers to distribute vernacular scripture and polemic. Extract legitimacy and audience reach. Bear prosecution and excommunication.
 *   - printing_entrepreneurs: Agents (printing-house owners, compositors with market power) seeking profitable markets; coordinate with reformers to build customer base around controversial theology. Extract capital and market share. Bear pressure from scribal guilds and mercantile authorities.
 *   - ecclesiastical_hierarchy: Primary victim of the constraint (papal authority, bishops, inquisitorial apparatus). Authority over textual interpretation erodes as reformers distribute competing texts. Bear extraction as doctrinal authority fragments.
 *   - manuscript_scribes_and_guild_monopolies: Secondary victim. Scribal livelihoods erode as printing competes on speed and cost. Guild protections become unenforceable as print proliferates. Bear extraction as economic disruption.
 *   - lay_readership: Beneficiary but not agenda-setter. Gain access to theology in vernacular and at lower cost. Do not coordinate the constraint; their demand is exploited by the coalition but not orchestrated by it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, 0.68).
domain_priors:suppression_score(technology_reformation_causality__beneficiary_agency_reading, 0.72).
domain_priors:theater_ratio(technology_reformation_causality__beneficiary_agency_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformer-Printer Coalition Authority Bypass (Beneficiary Agency Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, '255689c4-f839-4b3f-8952-f65a76c6005b').
narrative_ontology:cs_kernel_codification('255689c4-f839-4b3f-8952-f65a76c6005b', distributed).
narrative_ontology:cs_authority_grounding('255689c4-f839-4b3f-8952-f65a76c6005b', lineage).
narrative_ontology:cs_interpretation_layer_present('255689c4-f839-4b3f-8952-f65a76c6005b').
narrative_ontology:cs_reading_relation('255689c4-f839-4b3f-8952-f65a76c6005b', technology_reformation_causality__technological_determinism_reading, coexists_with).
narrative_ontology:cs_reading_relation('255689c4-f839-4b3f-8952-f65a76c6005b', technology_reformation_causality__co_constitution_reading, influences).
narrative_ontology:cs_axiom('255689c4-f839-4b3f-8952-f65a76c6005b', foundational, reformer_strategic_agency_over_technology).
narrative_ontology:cs_axiom_status(reformer_strategic_agency_over_technology, holdable).
narrative_ontology:cs_axiom_grounding('255689c4-f839-4b3f-8952-f65a76c6005b', reformer_strategic_agency_over_technology, empirically_contingent).
narrative_ontology:cs_axiom('255689c4-f839-4b3f-8952-f65a76c6005b', foundational, mutual_extraction_coalition_structure).
narrative_ontology:cs_axiom_status(mutual_extraction_coalition_structure, holdable).
narrative_ontology:cs_axiom_grounding('255689c4-f839-4b3f-8952-f65a76c6005b', mutual_extraction_coalition_structure, empirically_contingent).
narrative_ontology:cs_reference_frame('255689c4-f839-4b3f-8952-f65a76c6005b', monopoly_doctrinal_authority_and_scribal_textual_distribution).
narrative_ontology:cs_drift_state('255689c4-f839-4b3f-8952-f65a76c6005b', post_reformation_stabilization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('255689c4-f839-4b3f-8952-f65a76c6005b', '2026-06-12T15:42:00Z').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, reform_theologians).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, printing_entrepreneurs).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, manuscript_scribes).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__beneficiary_agency_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__beneficiary_agency_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the constraint's function is to transfer authority and capital FROM the ecclesiastical and scribal monopolies TO the reformer-printer coalition, and this transfer persists against active suppression. The extraction is not zero-sum market competition (which would be high-extraction snare dynamics) but rather institutional authority capture — the constraint restructures who gets to adjudicate truth and profit from textual distribution. Suppression is correspondingly high (0.72) because both ecclesiastical and mercantile authorities invest in enforcement: heresy prosecution, book burning, printing privilege restriction, guild enforcement against competing technologies. Theater ratio is moderate (0.41) because the constraint's function is genuine (reformers do distribute theology, printers do build capital) but the distribution of benefit becomes increasingly asymmetric over the interval — by the plateau at time 40+, reformed theology becomes institutionalized (new orthodox monopolies replace old ones in some regions), and the extractive edge becomes theatrical (defending privileges now rather than enabling dissent). The shared measurement grid shows all three metrics rising during the ramp (0-30), then stabilizing once the constraint settles into equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   Reformers and printers both experience the constraint as empowering (gaining what was previously monopolized), but from opposite directions: reformers gain doctrinal freedom, printers gain capital. The church experiences it as pure extraction (losing authority without compensation). Lay readers experience it as beneficiary (access to texts, cheaper books) but have zero enforcement power—they are not in the coalition and do not set the constraint. The engine computes this divergence: beneficiary seats compute as rope-flavored (low extraction, coordination benefit), victim seat computes as snare-flavored (high extraction, trapped exit, no coordination function for the church). The per-seat type divergence is the point—the constraint is tangled_rope from the coalition perspective, snare from the victim perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directedness toward extraction derives from the authority-bypass value, not from the printing technology itself. Reformers strategically deploy printing to bypass Rome; Rome loses control. Printers strategically deploy controversial theology to build market share; guild monopolies lose protection. The extraction is the transfer of authority and capital—printing is the instrument, not the cause. This reading's ε (0.68) is driven by the magnitude of authority loss Rome bears and the degree to which that loss is structured into the constraint's operation (Rome cannot maintain authority-over-texts AND permit printing; the constraint forces the choice). The suppression is correspondingly high because maintaining the constraint requires active prevention of book burning, censorship, guild enforcement—the church and guilds invest heavily in suppression, and the coalition invests in counter-suppression (underground presses, smuggling networks, doctrinal justifications for printing as divine tool).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the false dichotomy between 'technology caused the Reformation' and 'the Reformation would have happened anyway.' Instead it grounds the constraint in strategic agency: specific people made deliberate choices to deploy a specific technology toward a specific end (bypass authority), and the constraint persists because both the reformer and printer seats benefit from the arrangement while both face suppression. Mandatrophy is not present at interval end (time 50) because the constraint's founding purpose—enabling doctrinal dissent against monopoly authority—remains live in most Protestant regions and continues to generate doctrinal innovation and publishing competition. However, a secondary mandatrophy signal appears: by time 40–50, reformed theology itself becomes orthodox in some regions (Lutheran princes, Reformed magistrates), and printing becomes their tool for enforcing new orthodoxies. The constraint's original function (enabling dissent) becomes theatrical in those regions while remaining live elsewhere. This is not mandatrophy-of-the-constraint but rather constraint-mutation: the same technology and coalition structure now enforces new orthodoxies instead of opposing old ones. The theater_ratio plateau at 0.41 captures this shift—printing's function is increasingly maintenance of reformed orthodoxy rather than enabling radical dissent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_separation,
    'Is this constraint structurally distinct from the technological_determinism and co_constitution readings, or do all three instantiate the same underlying causality relationship with different interpretive framings?',
    'Structural audit: compare ε values (authority-bypass extraction), beneficiary/victim sets, and suppression mechanisms across readings. If ε diverges significantly (±0.15+), the readings describe genuinely different constraints; if ε is stable across readings, the distinction is interpretive rather than structural.',
    'If distinct constraints: each reading''s classification (tangled_rope vs. scaffold vs. co-constitution pattern) stands independently. If interpretive variance only: the three readings are perspectives on one constraint, and the kernel classification oscillates based on which reading''s framing dominates at a given moment (higher institutional instability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_separation, conceptual, 'Whether readings are distinct constraints or interpretations of one constraint.').

omega_variable(
    strategic_intent_evidence,
    'What constitutes evidence that reformers and printers deployed printing STRATEGICALLY (deliberate, coordinated, targeted to bypass authority) versus merely OPPORTUNISTICALLY (using an available tool without prior plan)?',
    'Historical record audit: correspondence between reformers and printers pre-printing; explicit statements about bypassing Rome; coordination of content selection and distribution timing; contrast with scribal-manuscript distribution patterns. Absence of written strategy is not absence of strategy (oral coordination, tacit understanding).',
    'If strategic intent is established, the coalition''s mutual extraction (each party using the other to achieve goals prohibited by the authority) becomes the defining structure — tangled_rope holds. If only opportunistic use is documented, the reading weakens toward scaffold (technology as transition) or co_constitution (mutual shaping without pre-formulated plan).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_intent_evidence, empirical, 'Whether reformer-printer deployment was coordinated strategic action or emergent opportunism.').

omega_variable(
    extraction_asymmetry_within_coalition,
    'Within the reformer-printer coalition, who extracted more from whom? Did printers use reformers to bypass guild restrictions, or did reformers use printers to bypass ecclesial authority, or was the extraction genuinely mutual?',
    'Economic and institutional analysis: profit flows to printers; doctrinal authority and theological legitimacy accumulates to reformers. Suppression mechanisms differ (economic pressure on printers, heresy prosecution on reformers). Trace whether either party could have achieved their goals without the other.',
    'Mutual extraction supports tangled_rope (both coordinated and both paid). Asymmetric extraction (one party used the other instrumentally) would reframe as snare or scaffold depending on whether the tool party was trapped or transitional. The measured tangled_rope claim depends on this asymmetry holding at moderate rather than extreme levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_asymmetry_within_coalition, empirical, 'Whether reformer-printer extraction was mutual or asymmetric.').

omega_variable(
    printing_as_scaffold_vs_infrastructure,
    'Is printing a temporary scaffold (transitional technology enabling authority bypass, meant to be replaced by something else) or a durable infrastructure that became the constraint''s permanent substrate?',
    'Periodization: does the reformer-printer constraint dissolve or transform after print stabilizes? If printers and reformers separate (printers become conventional commercial publishers, reformers become established churches with different authority), the technology was transitional. If the constraint persists and mutates (modern printing politics, digital platform authority bypass), printing was infrastructure, not scaffold.',
    'If scaffold: the constraint''s primary function was transition, and both tangled_rope and extraction metrics are time-bounded phenomena of the transition. If infrastructure: tangled_rope persists as a permanent institutional pattern, and extraction continues as long as the technology remains the substrate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(printing_as_scaffold_vs_infrastructure, conceptual, 'Whether printing was a transitional scaffold or durable constraint infrastructure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(tech_tr_t10, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(tech_tr_t20, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(tech_tr_t30, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(tech_tr_t40, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(tech_tr_t50, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tech_be_t10, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(tech_be_t20, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(tech_be_t30, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(tech_be_t40, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(tech_be_t50, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(tech_su_t10, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(tech_su_t20, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(tech_su_t30, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(tech_su_t40, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(tech_su_t50, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__beneficiary_agency_reading, 0.09).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint family comprises three readings of the 'technology_reformation_causality' kernel. The beneficiary_agency_reading isolates strategic coalition action (ε ≈ 0.68, tangled_rope). The technological_determinism_reading isolates printing's structural affordances (expected ε lower, mountain or rope). The co_constitution_reading isolates feedback dynamics between technology and actors (expected ε moderate, likely rope or scaffold). All three are linked; none can be understood in isolation from the kernel dispute they instantiate. The 'affects' direction runs FROM beneficiary_agency (the most institutionally active reading, with the highest suppression and enforcement investment) TO the other readings, because defending the beneficiary-agency reading requires suppressing both the technological determinism reading (which would absolve reformers of strategic responsibility) and the co_constitution reading (which would blur the distinction between deliberate coordination and emergent complexity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
