% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Secession Legitimacy via Grievance Threshold (Extractive Federalism Reading)
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The grievance-threshold reading claims that federal structural injustice
 *   — extraction, rights violations, or regulatory capture at asymmetric cost
 *   to subordinated provinces — can legitimize otherwise unconstitutional
 *   secession. This reading appears to offer a neutral mechanism: an
 *   objective standard that both protects federal authority from frivolous
 *   exit claims (the threshold must be high and demonstrable) and protects
 *   subordinated provinces from permanent lock-in (exit becomes legitimate
 *   once demonstrable injustice crosses into structural category). The
 *   reading instantiates one claim from a contested kernel about what makes
 *   secession legitimate: the constitutionalist says the constitution forbids
 *   it, the grievance-threshold reader says the constitution becomes
 *   illegitimate when applied unjustly, the popular-sovereignty reader says
 *   the referendum IS the legitimacy, and the treaty reader says
 *   pre-federation rights preempt all. This JSON instantiates only the
 *   grievance-threshold reading — a clean, self-contained constraint story
 *   with its own ε value, its own beneficiary/victim structure, and its own
 *   type classification. Sibling readings are other files.
 *
 * KEY AGENTS:
 *   - federal_authority_structure: Sets and interprets the threshold; benefits from high threshold that keeps exit costly
 *   - structurally_subordinated_provinces: Bear asymmetric costs; can theoretically exit if threshold crossed
 *   - communities_bearing_direct_extraction: Face direct costs but are identity-locked and lack capacity to mount cases
 *   - jurisdictions_opposing_secession: Benefit from high threshold; invest in controlling interpretation
 *   - legal_and_academic_arbiters: Positioned as neutral but favor interpretations maintaining institutional stability
 *   - secession_movement_leadership: Bear burden of proof; must produce evidence threshold was crossed
 *   - international_observers: Structurally excluded but needed for recognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.68).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.71).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Secession Legitimacy via Grievance Threshold (Extractive Federalism Reading)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political/constitutional").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, '33b4def7-0c38-494c-abc7-970311a34637').
narrative_ontology:cs_kernel_codification('33b4def7-0c38-494c-abc7-970311a34637', fixed_text).
narrative_ontology:cs_authority_grounding('33b4def7-0c38-494c-abc7-970311a34637', lineage).
narrative_ontology:cs_interpretation_layer_present('33b4def7-0c38-494c-abc7-970311a34637').
narrative_ontology:cs_reading_relation('33b4def7-0c38-494c-abc7-970311a34637', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('33b4def7-0c38-494c-abc7-970311a34637', secession_legitimacy_boundary__popular_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('33b4def7-0c38-494c-abc7-970311a34637', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('33b4def7-0c38-494c-abc7-970311a34637', foundational, structural_injustice_can_override_constitutional_text).
narrative_ontology:cs_axiom_status(structural_injustice_can_override_constitutional_text, holdable).
narrative_ontology:cs_axiom_grounding('33b4def7-0c38-494c-abc7-970311a34637', structural_injustice_can_override_constitutional_text, deontological).
narrative_ontology:cs_axiom('33b4def7-0c38-494c-abc7-970311a34637', foundational, legitimacy_requires_objective_threshold_determination).
narrative_ontology:cs_axiom_status(legitimacy_requires_objective_threshold_determination, holdable).
narrative_ontology:cs_axiom_grounding('33b4def7-0c38-494c-abc7-970311a34637', legitimacy_requires_objective_threshold_determination, empirically_contingent).
narrative_ontology:cs_reference_frame('33b4def7-0c38-494c-abc7-970311a34637', federal_legitimacy_conditional_on_justice).
narrative_ontology:cs_drift_state('33b4def7-0c38-494c-abc7-970311a34637', contemporary_extraction_intensity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('33b4def7-0c38-494c-abc7-970311a34637', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, federal_authority_structure).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, jurisdictions_opposing_secession).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, structurally_subordinated_provinces).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, communities_bearing_federal_extraction).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, structurally_subordinated_provinces).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, secession_movement_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms of the federation and interprets whether any given provincial grievance meets the threshold for legitimizing secession. Maintains the constitutional text as the binding framework while selectively interpreting what counts as 'structural injustice.' Benefits from a high threshold that keeps exit costly even when extraction is substantial.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_authority_structure, agenda_setter,
    institutional, generational, analytical, universal).

% Bear asymmetric costs of federation (resource extraction, revenue transfers, regulatory capture by federal or majority interests). Can theoretically exit if they can demonstrate their grievances cross an 'objective' threshold of structural injustice. The threshold itself is contested and high; proving it requires institutional and financial capacity to sustain the burden of proof.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, structurally_subordinated_provinces, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, structurally_subordinated_provinces, beneficiary).

% Face direct extraction through resource seizure, labor conscription, tax disproportionality, or cultural suppression at the federal level. Their grievances may meet an objective threshold of injustice, but they lack the organizational and legal capacity to mount a secession case. Identity-locked because exit means abandoning ancestral territory or renouncing citizenship bonds that constitute their political membership.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, communities_bearing_federal_extraction, payer,
    powerless, biographical, identity_locked, local).

% Benefit from the high legitimacy threshold because it protects them from losing territory or losing leverage over remaining federal partners. They operate within the constitutional text and have the resources to argue why a given province's grievances do not meet the threshold. They invest in controlling the threshold interpretation.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, jurisdictions_opposing_secession, beneficiary,
    powerful, generational, arbitrage, regional).

% Attempt to establish what 'structural injustice' objectively means and what empirical evidence meets the threshold. Their pronouncements influence which grievances are treated as legitimizing and which are dismissed as insufficient. They are positioned as neutral but their credentialing system and professional incentives favor interpretations that maintain institutional stability.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, legal_and_academic_arbiters, observer,
    institutional, generational, analytical, global).

% Mobilizes popular grievance into a secession case, bearing the legal, political, and reputational costs of challenging federal legitimacy. Must produce evidence that grievances cross the objective threshold or be delegitimized by academic and legal arbiters. The burden of proof is asymmetrically placed on them; the federal authority need not prove its own justice, only that the threshold was not crossed.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, secession_movement_leadership, payer,
    moderate, biographical, constrained, regional).

% Would recognize or withhold recognition based on whether they judge the federal actions to constitute structural injustice meeting a threshold. Their inclusion in the recognition question is explicitly forbidden by the constraint; international law defers to internal constitutional processes. They are structurally excluded from the legitimacy determination yet their recognition is necessary for successful secession.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, international_observers, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__grievance_threshold_reading, federal_authority_structure).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__grievance_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a decision procedure for determining when federal authority has become so extractive or unjust that subordinated jurisdictions may exit the federation without violating constitutional order. This appears to coordinate the balance between federal stability and provincial sovereignty rights by creating an objective standard for when balance has failed.
% TRANSFER_FUNCTION: Moves the burden of proof onto secession movements: they must demonstrate federal structural injustice meeting an objective threshold, while the federal authority and opposing jurisdictions are positioned as the defenders of a constitutionally sound status quo. The transfer is in legitimacy claims: the federal structure retains presumptive legitimacy unless the threshold is crossed; once crossed, it operates as a liberation argument for the exiting polity.
% ABSENT_VOICES: Powerless communities bearing direct extraction are structurally excluded from threshold-setting — legal and academic arbiters who determine what counts as 'structural injustice' rarely include voices from the communities experiencing it, preferring institutional and professional frameworks. International observers and treaty-holding Indigenous nations are also excluded from the threshold determination, though both have stakes in secession legitimacy and its precedent.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared — if federal actions could not trigger secession legitimacy regardless of threshold — the federation would ossify around whatever federal-authority decisions are made; exit would be unconditionally impossible. Subordinated provinces would be locked in indefinitely. The constraint's disappearance would collapse the apparent bargain of conditional federation and force renegotiation of the entire federal compact.
% FOUNDING_PROBLEM: Federations require a mechanism to prevent tyranny of the center: if federal authority becomes extractive and subordinated provinces have zero legitimate exit, the federation becomes a cage. The threshold reading was built to hold that a federal structure maintains legitimacy only so long as it does not cross into structural injustice. Once crossed, the federal claim to authority dissolves and secession becomes a self-defense claim rather than a rebellion.
% FOUNDING_PROBLEM_CORROBORATION: Subordinated provinces and secession movements attest that federal extraction has crossed structural-injustice thresholds and their bids for legitimacy should be granted. Federal authorities and opposing jurisdictions attest that the threshold has not been met and that the constitutional text remains binding. Academic and legal arbiters offer competing interpretations of what 'structural injustice' means; some scholars (particularly from subordinated regions) argue the threshold has been crossed repeatedly and arbiters are protecting federal interests; others (from federal centers) argue the threshold is appropriately high and movement claims overstate their case. No authoritative corroboration exists from outside the contesting parties — the determination itself is what the dispute is about.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.68 at interval end) reflects the underlying federal arrangement: extraction rises as subordinated provinces accumulate evidence of structural injustice, but the rise plateaus because the mechanism itself (the threshold) prevents extraction from becoming unlimited — once it crosses visibly into injustice, the legitimacy of the federal structure begins to erode. The measurement series shows extraction rising sharply from t=0 to t=25, then stabilizing as the threshold reading becomes increasingly invoked (and contested) in legal and political discourse. Theater ratio (0.42 at interval end) captures the growing performativity: federal authority increasingly frames extraction as justified coordination cost while subordinated provinces frame the same facts as structural injustice; the gap between the institutional narrative and actual operation widens. Suppression requirement (0.71 at interval end) reflects active enforcement: federal authority and opposing jurisdictions must actively suppress secession movements and delegitimize their threshold claims to maintain the federation, because the mechanism itself concedes that legitimate exit is possible if the threshold is met. The constraint is a tangled rope because (1) it coordinates a solution to the eternal federation problem (how to prevent both tyranny and fragmentation), (2) it extracts from subordinated provinces by placing the burden of proof on them, and (3) it requires active enforcement to suppress the threshold claims that would otherwise dissolve federal legitimacy. The measurement series captures one complete oscillation (rise of federal extraction and delegitimization, plateau as threshold-crossing becomes plausible, slight decline as federal authority manages the legitimacy crisis by making minor reforms and reinterpreting the threshold upward). The oscillation is itself an extraction mechanism: periodic tension and reform cycles keep subordinated provinces mobilized enough to feel heard but disorganized enough to rarely succeed at demonstrating threshold-crossing.
 *
 * PERSPECTIVAL GAP:
 *   The federal authority experiences the constraint as coordination (a way to keep the federation viable while respecting provincial autonomy rights). Subordinated provinces experience it as extraction (a mechanism that appears to offer exit while making exit procedurally impossible). Powerless communities experience it as theater (a valve that is labeled but does not actually flow because their identity-lock makes exit infeasible regardless of threshold). The gap is structural: all parties benefit from the appearance of a neutral threshold, but the mechanism's details (burden of proof, arbiter selection, threshold definition) systematically favor federal stability over provincial exit. This is why the engine should compute different types per seat — the constraint genuinely has both rope and snare properties depending on institutional position.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal authority is positioned as the beneficiary (sets the rules, interprets the threshold, benefits from status quo unless subordinates overcome the proof burden). Subordinated provinces and bearing-extraction communities are positioned as the targets (must prove the threshold was crossed, face active suppression of their secession claims, pay the cost of the constraint's maintenance through continued extraction while waiting for legitimacy). Jurisdictions opposing secession are secondary beneficiaries (benefit from federal stability without bearing proof burden). The burden of proof creates asymmetric directionality: the federal authority has d near the beneficiary end (0.15-0.25), subordinated provinces have d near the target end (0.75-0.85), and powerless communities locked by identity have d nearly at full-target (0.90+) because they pay the cost, have no exit, and cannot invoke the mechanism. Legal arbiters have d near 0.5 (they service both sides but structurally favor institutional stability). Secession movements have d near 0.80 because they bear the burden of proof while the federal authority bears none.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not trigger mandatrophy via the usual mechanism (founding problem dead, constraint persisting). Instead, it instantiates what might be called 'legitimacy drift': the founding problem (preventing both tyranny and fragmentation) remains live, but the mechanism for solving it has shifted from genuine coordination to bureaucratic suppression disguised as neutrality. The threshold reading was built to prevent federal tyranny by creating a legitimacy exit valve. But the valve's operation — the burden of proof, the arbiter selection, the threshold definition — has become so controlled by federal interests that the valve is now part of the tyranny-mechanism. This is different from classic mandatrophy because the constraint is STILL BEING USED, STILL being invoked in legal proceedings, STILL being cited as the framework for secession legitimacy. What has happened is that the mechanism has been captured without being abandoned — it persists because all parties gain from its appearance of neutrality, even though its operation is asymmetrically captured. The founding problem is not dead; it is being 'solved' by making the mechanism's failure look like proper procedure. This is closer to what the Piton reading describes (atrophied function, continued performance), except the performance is not harmless — it actively suppresses the legitimate exit claims it pretends to serve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_objectivity_ambiguity,
    'Is there an ''objective'' threshold of structural injustice that can be measured independently of the reading parties'' interests, or does the threshold itself become a contested object that each side interprets in its favor?',
    'Attempt to establish a formal, measurable definition of structural injustice (e.g., resource extraction exceeding per-capita cost of federal services, rights violations meeting specific legal tests) and observe whether subordinated provinces and federal authorities converge on its application, or whether they continue to contest its meaning even after operationalization.',
    'If objectivity is possible, the threshold functions as claimed — a neutral gate that prevents both arbitrary federal tyranny and frivolous secession claims. If the threshold is fundamentally contestable, the constraint becomes a naming game where the powerful determine what counts as injustice, and the mechanism fails to protect subordinated provinces.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_objectivity_ambiguity, conceptual, 'Whether the structural-injustice threshold is objectively measurable or inherently contested.').

omega_variable(
    burden_of_proof_asymmetry,
    'Why does the burden of proof rest on the seceding party to prove the threshold was crossed, rather than on the federal authority to prove it remained just?',
    'Historical and comparative analysis: examine cases where burden-of-proof allocation was reversed (e.g., federal authority required to justify extraction, subordinated provinces presumed to have legitimacy grounds). Observe whether the outcome changed substantially.',
    'If the burden-of-proof asymmetry is structural rather than incidental, the threshold reading operationally favors federal stability over provincial exit regardless of actual extraction levels. The mechanism becomes a tool for suppressing legitimate secession claims by making them procedurally nearly impossible to sustain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_of_proof_asymmetry, empirical, 'Whether the asymmetric burden of proof reflects genuine evidentiary requirements or suppresses exit claims.').

omega_variable(
    arbitral_capture_risk,
    'Do the legal and academic arbiters determining what counts as ''structural injustice'' have structural incentives to maintain the federation, and does this capture the threshold determination?',
    'Examine the demographics, funding sources, and career advancement patterns of arbiters in cases where the threshold was invoked. Observe whether arbiters from federal centers systematically ruled against secession thresholds at higher rates than arbiters from subordinated regions.',
    'If arbiter capture is systematic, the threshold reading is a false neutral mechanism that protects federal extraction from challenge. The constraint becomes a snare masquerading as tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arbitral_capture_risk, empirical, 'Degree to which arbiters of the threshold are structurally captured by federal interests.').

omega_variable(
    identity_lock_and_exit_feasibility,
    'Can powerless communities bearing direct federal extraction actually exit the federation if the threshold is crossed, given that identity-lock prevents their physical departure?',
    'Case studies of successful and failed secession movements: examine whether communities locked by cultural, kinship, or religious identity to their territory could mount viable secession cases even when extraction was severe, or whether identity-lock rendered them unable to exit regardless of threshold crossing.',
    'If identity-lock prevents exit even when the threshold is crossed, the constraint offers false legitimacy to exit while maintaining actual impossibility for the most harmed communities. It becomes a theater mechanism that protects federal structure while offering the appearance of a valve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_and_exit_feasibility, empirical, 'Whether identity-locked communities can exercise secession rights even if threshold is met.').

omega_variable(
    reading_sibling_foreclosure,
    'Can this reading (grievance-threshold) coexist with the constitutional-impossibility reading in the same framework, or does asserting that federal injustice legitimizes secession logically foreclose the assertion that secession is constitutionally impossible?',
    'Examine whether the same polity can hold both ''secession is constitutionally impermissible'' AND ''secession becomes legitimate when structural injustice crosses a threshold'' simultaneously without internal contradiction. Consider whether the readings occupy different temporal or normative domains (e.g., what is constitutionally legal vs. what is morally legitimate) that allow both to hold.',
    'If the readings foreclose each other, then adoption of the threshold reading by the federal authority constitutes a formal rejection of constitutional-impossibility framing. If they coexist, each is a live position that different parties can hold, and the contestation persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_sibling_foreclosure, conceptual, 'Whether grievance-threshold reading logically rules out constitutional-impossibility reading.').

omega_variable(
    international_recognition_decoupling,
    'If a subordinated province meets the threshold as determined internally and secedes, but the international community withholds recognition based on different criteria, has the constraint protected the province''s legitimacy?',
    'Examine cases where internal determination of legitimacy diverged from international recognition outcomes. Assess whether the threshold reading''s exclusion of international voices from the determination created a gap between internal legitimacy and external recognition capacity.',
    'If internal threshold-crossing does not translate to international recognition, the constraint provides legitimacy in theory while leaving the exiting province unable to implement its independence. The mechanism becomes a naming ceremony without enforceability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_recognition_decoupling, empirical, 'Whether internal threshold determination suffices for international recognition of secession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(sece_tr_t0, observed).
narrative_ontology:measurement(sece_tr_t5, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(sece_tr_t5, observed).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(sece_tr_t10, observed).
narrative_ontology:measurement(sece_tr_t15, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(sece_tr_t15, observed).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(sece_tr_t20, observed).
narrative_ontology:measurement(sece_tr_t25, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement_basis(sece_tr_t25, observed).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(sece_tr_t30, observed).
narrative_ontology:measurement(sece_tr_t35, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(sece_tr_t35, observed).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(sece_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(sece_be_t0, observed).
narrative_ontology:measurement(sece_be_t5, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(sece_be_t5, observed).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(sece_be_t10, observed).
narrative_ontology:measurement(sece_be_t15, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(sece_be_t15, observed).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(sece_be_t20, observed).
narrative_ontology:measurement(sece_be_t25, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(sece_be_t25, observed).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(sece_be_t30, observed).
narrative_ontology:measurement(sece_be_t35, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 35, 0.67).
narrative_ontology:measurement_basis(sece_be_t35, observed).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(sece_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(sece_su_t0, observed).
narrative_ontology:measurement(sece_su_t5, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(sece_su_t5, observed).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(sece_su_t10, observed).
narrative_ontology:measurement(sece_su_t15, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(sece_su_t15, observed).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(sece_su_t20, observed).
narrative_ontology:measurement(sece_su_t25, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(sece_su_t25, observed).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(sece_su_t30, observed).
narrative_ontology:measurement(sece_su_t35, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 35, 0.7).
narrative_ontology:measurement_basis(sece_su_t35, observed).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(sece_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__grievance_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__grievance_threshold_reading, 0.12).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__treaty_primacy_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, federal_resource_extraction_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, province_revenue_transfer_asymmetry).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the secession_legitimacy_boundary kernel. All four readings address the same constitutional question (what legitimizes secession) but disagree on the answer. The readings do not compete within a single framework — they are held simultaneously by different legal and political traditions. This grievance-threshold reading proposes that an objective, measurable standard (structural injustice crossing a threshold) can determine legitimacy. The constitutional-impossibility reading denies that any standard external to the constitutional text can override it. The popular-sovereignty reading asserts that the referendum result itself IS the standard, needing no external justification. The treaty-primacy reading asserts that pre-federation Indigenous agreements preempt all federal and provincial authority. Each reading instantiates a different constraint with different ε values, different beneficiary/victim structures, and different classification outcomes. They are linked by network edges because adoption of one reading delegitimizes others (or keeps them coexisting, depending on which readings forecast each other). This file models only the grievance-threshold reading; other files model the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__grievance_threshold_reading, powerless, 0.92).
constraint_indexing:directionality_override(secession_legitimacy_boundary__grievance_threshold_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
