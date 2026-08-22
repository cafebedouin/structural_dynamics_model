% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__technological_determinism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__technological_determinism_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: technology_reformation_causality__technological_determinism_reading
 *   human_readable: Printing Press as Deterministic Cause of the Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the technological_determinism_reading of the
 *   technology_reformation_causality kernel: the claim that the printing
 *   press's collapse of per-unit reproduction cost for vernacular scripture
 *   made a Reformation-scale rupture in religious authority structurally
 *   inevitable, independent of which specific reformers or printers acted.
 *   Under this reading, the press functions as a mountain — a
 *   physical/economic constraint (falling marginal cost of text reproduction)
 *   that reformers, printers, and Church authorities alike could adapt to but
 *   not prevent or redirect. Two sibling readings of the SAME kernel are NOT
 *   part of this constraint: beneficiary_agency_reading treats the press as a
 *   tool strategically deployed by reformers who bypassed Church authority
 *   (technology as instrument, agency as cause), and co_constitution_reading
 *   treats press and social actors as co-evolving, denying either pure
 *   priority. Those are separate files with their own ε and stakeholder
 *   structures, linked via network.affects_constraints. This reading's ε
 *   (0.12) is low because it is authored against the production-cost
 *   mechanism alone, not against distribution control or censorship regimes,
 *   which belong properly to the sibling readings.
 *
 * KEY AGENTS:
 *   - printing_press_technology: the mountain itself — a physical/economic constraint whose cost curve, once movable type existed, could not be un-invented
 *   - vernacular_reading_publics: downstream beneficiaries of falling text-access costs, adapting to a changed information environment they did not create
 *   - reformist_clergy: downstream adapters under this reading — their strategic choices are treated as responses to an already-inevitable diffusion, not as causally prior
 *   - church_hierarchy: downstream adapters attempting (unsuccessfully) to resist a cost-structure change beyond their control
 *   - print_technology_narrative_historians: beneficiaries of a historiographical framework that treats media technology as primary explanatory variable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, 0.12).
domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, 0.08).
domain_priors:theater_ratio(technology_reformation_causality__technological_determinism_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__technological_determinism_reading, mountain).
narrative_ontology:human_readable(technology_reformation_causality__technological_determinism_reading, "Printing Press as Deterministic Cause of the Reformation").
narrative_ontology:topic_domain(technology_reformation_causality__technological_determinism_reading, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__technological_determinism_reading, '80921b92-719e-40e1-813e-10cf8e277fd6').
narrative_ontology:cs_kernel_codification('80921b92-719e-40e1-813e-10cf8e277fd6', distributed).
narrative_ontology:cs_authority_grounding('80921b92-719e-40e1-813e-10cf8e277fd6', distributed).
narrative_ontology:cs_reading_relation('80921b92-719e-40e1-813e-10cf8e277fd6', technology_reformation_causality__beneficiary_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('80921b92-719e-40e1-813e-10cf8e277fd6', technology_reformation_causality__co_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('80921b92-719e-40e1-813e-10cf8e277fd6', foundational, technology_sufficient_cause_of_reformation).
narrative_ontology:cs_axiom_status(technology_sufficient_cause_of_reformation, holdable).
narrative_ontology:cs_axiom_grounding('80921b92-719e-40e1-813e-10cf8e277fd6', technology_sufficient_cause_of_reformation, empirically_contingent).
narrative_ontology:cs_axiom('80921b92-719e-40e1-813e-10cf8e277fd6', secondary, human_strategic_agency_causally_epiphenomenal).
narrative_ontology:cs_axiom_status(human_strategic_agency_causally_epiphenomenal, holdable).
narrative_ontology:cs_axiom_grounding('80921b92-719e-40e1-813e-10cf8e277fd6', human_strategic_agency_causally_epiphenomenal, empirically_contingent).
narrative_ontology:cs_reference_frame('80921b92-719e-40e1-813e-10cf8e277fd6', print_cost_collapse_as_fixed_constraint).
narrative_ontology:cs_drift_state('80921b92-719e-40e1-813e-10cf8e277fd6', contemporary_historiography, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('80921b92-719e-40e1-813e-10cf8e277fd6', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, print_technology_narrative_historians).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, media_determinist_scholarship).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, vernacular_reading_publics).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, church_hierarchy).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, technological_determinism_thesis).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, print_capitalism_inevitability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advance and teach a media-determinist framework in which the printing press is treated as the primary sufficient cause of the Reformation. Gain explanatory parsimony, textbook adoption, and disciplinary prestige from a framework that assigns technology primary causal weight over contingent human strategy. Their professional standing is not threatened by data showing insufficiency in isolated cases, but is threatened by a broad comparative pattern undermining sufficiency.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, print_technology_narrative_historians, beneficiary,
    analytical, civilizational, analytical, global).

% The scholarly tradition itself (McLuhan-descended media theory applied to Reformation history) that gains legitimacy and continued citation when the determinism reading is treated as the default account. Listed for completeness as a non-agent intellectual tradition, not a real-world actor capable of exit or action.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, media_determinist_scholarship, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(technology_reformation_causality__technological_determinism_reading, media_determinist_scholarship).

% Sixteenth-century lay readers who gained access to printed vernacular scripture as costs fell. Under this reading they are pure downstream beneficiaries of a cost mechanism they did not create and could not have altered; their own choices about what to do with newly accessible texts are treated as adaptation, not causation. They have no voice in the historiographical debate about whether they were agents or merely recipients.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, vernacular_reading_publics, beneficiary,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__technological_determinism_reading, vernacular_reading_publics, excluded).

% Historical figures (Luther and successors) whose strategic choices about translation, pamphleteering, and print partnerships are, under THIS reading, recast as adaptations to an already-inevitable cost collapse rather than as causally significant interventions. This reading structurally excludes their agency from the causal account even though they would (and did, in their own writings) claim deliberate strategic authorship of the print campaign.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, reformist_clergy, excluded,
    moderate, biographical, constrained, continental).

% Attempted to resist vernacular scripture diffusion through indices of prohibited books, licensing requirements, and printer prosecution. Under this reading, their resistance is friction against a mountain — a losing battle against a fixed cost-structure change, not a genuinely contestable political struggle. They bear the cost of authority erosion but are not modeled as victims of any actor's extraction, since under determinism no one profits from their loss; the press itself is the cause.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, church_hierarchy, payer,
    institutional, generational, trapped, continental).

% Examine cases where printing technology existed without Reformation-equivalent religious rupture (Ottoman print resistance, East Asian woodblock printing traditions) to test whether the press was actually sufficient or merely necessary. Their comparative findings are the primary mechanism by which this reading's mountain claim could be falsified or vindicated.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, there is no coordination problem being solved by an arrangement between parties — the press is a physical/economic constraint, not a social coordination mechanism. To the extent a coordination-like function exists, it is the standardization of text reproduction cost across print shops, which lowered the price of vernacular scripture uniformly regardless of who operated any given press.
% TRANSFER_FUNCTION: No deliberate transfer is modeled under this reading — the press does not move resources from a payer to a beneficiary through anyone's design. Historical resources (Church authority, control over scriptural interpretation) shifted away from centralized clergy toward diffuse lay readers and vernacular translators as a side effect of falling reproduction costs, not as an engineered transfer.
% ABSENT_VOICES: Reformist clergy and printers themselves are structurally excluded from the causal account this reading gives, even though contemporaneous sources show them describing their print campaigns as deliberate strategy. Their own testimony about intentional deployment is the strongest objection to this reading and is exactly what the sibling beneficiary_agency_reading elevates.
% DISAPPEARANCE_RATIONALE: If the printing press's cost-collapse mechanism 'disappeared' (i.e., were shown historically not to have been sufficient on its own), the determinism reading's status as description of what happened would collapse, but the actual historical events (the Reformation occurred) would not un-happen — what would change is which reading of the kernel is vindicated. Determinist historians would lose an explanatory framework; agency-focused and co-constitution historians would gain ground. The verdict is contested precisely because it depends on which sibling reading the comparative evidence favors.
% FOUNDING_PROBLEM: This reading was constructed to explain why a religious rupture of this scale and speed occurred in the specific historical window it did — the 'why then, why so fast' problem that pure theological or political explanations struggled to answer, since Luther was far from the first reform-minded theologian to challenge Rome.
% FOUNDING_PROBLEM_CORROBORATION: Media theorists and some economic historians (outside the beneficiary set of narrative historians specifically invested in the strong determinism claim) corroborate that print cost-collapse was a necessary enabling condition; however comparative historians studying the Ottoman world and East Asia — genuinely outside the beneficiary set — report cases where equivalent or comparable printing capacity did not produce equivalent religious rupture, which undercuts the sufficiency claim specifically (not the necessity claim) and is the strongest outside corroboration against this reading's strong form.
narrative_ontology:disappearance_verdict(technology_reformation_causality__technological_determinism_reading, contested).
narrative_ontology:founding_problem_status(technology_reformation_causality__technological_determinism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__technological_determinism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__technological_determinism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__technological_determinism_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__technological_determinism_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, ExtMetricName, E),
    domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(technology_reformation_causality__technological_determinism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) because under the determinism reading, the press does not extract from anyone — it is a neutral physical/economic fact (falling reproduction costs) that reshapes the information landscape without anyone controlling or profiting from the shift as its architect. Suppression is low (0.08) because a genuine mountain does not need active enforcement; it simply obtains. Accessibility_collapse is authored moderately high (0.72, not 0.85+) reflecting that while the cost mechanism itself is a hard physical constraint, the SPEED and COMPLETENESS of vernacular diffusion still depended on literacy rates, distribution networks, and political tolerance — leaving this reading's mountain status somewhat less airtight than, say, a law of physics. Resistance is low (0.2): the Church's attempts to resist print diffusion (indices, licensing, burnings) are treated under this reading as friction against an underlying cost-structure change, not as genuine contestation of the mountain's existence.
 *
 * PERSPECTIVAL GAP:
 *   Under this reading, there should be minimal seat divergence for material actors (reformers, printers, church officials) because the reading treats them all as downstream adapters to a fixed constraint — divergence would only appear at the analytical/historiographical seat, where determinist historians experience the framework as explanatorily sufficient while agency-focused historians (the sibling reading's seat) would experience the same historical record as underdetermined by technology alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries under this reading are analytical/historiographical, not material — print_technology_narrative_historians and media_determinist_scholarship benefit from a framework that assigns primary causal weight to the artifact rather than to human strategic action. This is precisely the FSM (false summit mountain) candidate structure: a mountain claim with declared beneficiaries triggers the engine's false-summit detection, which is appropriate here — the determinism narrative is contestable specifically because it has intellectual beneficiaries (scholars, media theorists) who gain explanatory parsimony and prestige from treating technology as sufficient cause, displacing credit from strategic religious and political actors.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not directly at issue here since this is a claim about historical causality rather than an ongoing institutional mandate; however, the founding_problem framing captures an analogous dynamic: the determinism thesis was mobilized to explain a specific historical rupture, and if the underlying comparative evidence (Ottoman print resistance, Chinese woodblock trajectories) undermines sufficiency, the thesis persists in some corners of the field via disciplinary inertia rather than continued explanatory power — a piton-like drift internal to media-determinist historiography, distinct from the constraint itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_or_constructed_narrative,
    'Is the printing press''s causal role in the Reformation a genuine physical/economic constraint (a mountain — production cost collapse that no human agency could have prevented once movable type existed) or a constructed narrative that benefits scholars and disciplines invested in media-determinist explanatory frameworks?',
    'Comparative counterfactual analysis: examine regions/periods where printing technology existed without a Reformation-equivalent rupture (e.g. Ottoman resistance to print, or Chinese woodblock printing''s different religious trajectory) to test whether the technology alone was sufficient or merely necessary.',
    'If sufficiency fails in comparative cases, this reading''s mountain classification is a false summit — the ''inevitability'' claim would be reclassified toward tangled_rope, with media-determinist historiography as an identifiable beneficiary of the naturalized narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_or_constructed_narrative, conceptual, 'Whether technological determinism about print is natural law or beneficiary-serving narrative construction.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'This constraint is one reading (technological_determinism_reading) of the kernel technology_reformation_causality. Where exactly do the three sibling readings disagree — is it about WHETHER the press was necessary, WHETHER it was sufficient, or WHO gets causal credit?',
    'The determinism reading claims sufficiency (press alone made the outcome inevitable, reformers merely adapted downstream); the beneficiary_agency reading claims the press was a necessary tool but insufficient without strategic deployment by Luther, printers, and vernacular translators; the co_constitution reading denies the necessary/sufficient framing altogether, treating cause as an emergent property of press-and-actors interacting over time. The disagreement is located at the sufficiency claim and at whether agency is causally prior, co-equal, or epiphenomenal.',
    'If the sufficiency claim fails, this reading collapses toward co_constitution; if agency is shown causally prior, it collapses toward beneficiary_agency. The determinism reading survives only if press-driven cost collapse can be shown to produce vernacular scripture diffusion independent of any particular reformer''s strategic choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Locating the exact axis of disagreement among the three kernel readings.').

omega_variable(
    production_cost_epsilon_grounding,
    'Is the low extractiveness (ε≈0.12) authored here actually stable, or does it depend on treating ''production cost reduction'' as the sole causal variable while bracketing distribution control, literacy gatekeeping, and censorship regimes that determined who could actually access cheaply-printed vernacular texts?',
    'Trace whether printers, patrons, and civic authorities who controlled press licensing and distribution captured rents from the technology''s diffusion — if so, ε for THIS reading (which treats the press as a neutral physical constraint) may be understating extraction that belongs properly to the sibling readings.',
    'Confirms the ε-invariance discipline: if extraction differs meaningfully once distribution control is considered, that is evidence the beneficiary_agency and co_constitution readings are correctly instantiated as separate constraints rather than alternate measurements of this one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(production_cost_epsilon_grounding, empirical, 'Whether this reading''s low ε is genuinely about the technology alone, not smuggling in distribution-control extraction that belongs to sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__technological_determinism_reading, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1450, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(tech_tr_t1490, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1490, 0.08).
narrative_ontology:measurement(tech_tr_t1517, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(tech_tr_t1550, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1550, 0.13).
narrative_ontology:measurement(tech_tr_t1600, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1600, 0.15).
narrative_ontology:measurement(tech_tr_t1650, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1650, 0.15).

% Extraction over time
narrative_ontology:measurement(tech_be_t1450, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1450, 0.05).
narrative_ontology:measurement(tech_be_t1490, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1490, 0.07).
narrative_ontology:measurement(tech_be_t1517, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1517, 0.1).
narrative_ontology:measurement(tech_be_t1550, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1550, 0.12).
narrative_ontology:measurement(tech_be_t1600, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1600, 0.12).
narrative_ontology:measurement(tech_be_t1650, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1650, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(technology_reformation_causality__technological_determinism_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__beneficiary_agency_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single colloquial claim 'the printing press caused the Reformation,' per the ε-invariance principle. The three readings differ in where they locate causal sufficiency: this reading (technological_determinism_reading) treats the press's cost-collapse as sufficient and independent of agency (ε=0.12, mountain, no material beneficiaries — only historiographical ones); beneficiary_agency_reading treats reformers and printers as strategic deployers of the technology with the press as instrument (expected higher ε, tangled_rope or rope depending on whether Church suppression is modeled as victim-generating); co_constitution_reading treats press and actors as jointly determining outcomes with neither causally prior (expected intermediate ε, likely rope or tangled_rope with distributed beneficiaries). All three share the historical substrate but are structurally distinct constraints, linked here for contamination-propagation analysis: if comparative evidence undermines this reading's sufficiency claim, pressure flows toward validating the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
