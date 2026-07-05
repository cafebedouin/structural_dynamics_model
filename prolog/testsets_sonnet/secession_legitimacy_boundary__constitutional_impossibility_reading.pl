% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__constitutional_impossibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__constitutional_impossibility_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: secession_legitimacy_boundary__constitutional_impossibility_reading
 *   human_readable: Constitutional Impossibility Reading of the Secession Legitimacy Boundary
 *   domain: political economy / federalism / resource politics
 *
 * SUMMARY:
 *   A federation faces recurring secession pressure from a resource-rich
 *   province. This story instantiates the constitutional-impossibility
 *   reading of the contested secession legitimacy boundary kernel: the
 *   federal courts and government hold that unilateral secession has no
 *   constitutional standing and that the only legitimate exit is through the
 *   formal amendment process, which requires broad interprovincial and
 *   federal consent. This is a distinct constraint from the sibling readings
 *   (popular_sovereignty_reading, grievance_threshold_reading,
 *   treaty_primacy_reading), each of which locates legitimacy differently and
 *   produces a different beneficiary/victim structure. This story's ε is
 *   authored for THIS reading alone — the doctrinal claim that unilateral
 *   exit is void — and does not average across readings.
 *
 * KEY AGENTS:
 *   - federal_government: institutional agenda-setter administering the doctrine
 *   - constitutional_courts: institutional agenda-setter/observer that formalizes the rule via rulings
 *   - non_secessionist_provinces: organized beneficiaries of continued fiscal transfers
 *   - national_bondholders: powerful beneficiaries via reduced sovereign risk
 *   - separatist_movement_leadership: moderate-power payer with trapped exit
 *   - resource_rich_provincial_electorate: moderate-power payer/excluded voice
 *   - comparative_constitutional_scholars: analytical observers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.28).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.52).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Constitutional Impossibility Reading of the Secession Legitimacy Boundary").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political economy / federalism / resource politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, '96f72f4e-51e3-43f9-b7d9-b33b270779d0').
narrative_ontology:cs_kernel_codification('96f72f4e-51e3-43f9-b7d9-b33b270779d0', fixed_text).
narrative_ontology:cs_authority_grounding('96f72f4e-51e3-43f9-b7d9-b33b270779d0', lineage).
narrative_ontology:cs_interpretation_layer_present('96f72f4e-51e3-43f9-b7d9-b33b270779d0').
narrative_ontology:cs_reading_relation('96f72f4e-51e3-43f9-b7d9-b33b270779d0', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('96f72f4e-51e3-43f9-b7d9-b33b270779d0', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('96f72f4e-51e3-43f9-b7d9-b33b270779d0', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('96f72f4e-51e3-43f9-b7d9-b33b270779d0', foundational, amendment_process_is_exclusive_legitimacy_channel).
narrative_ontology:cs_axiom_status(amendment_process_is_exclusive_legitimacy_channel, holdable).
narrative_ontology:cs_axiom_grounding('96f72f4e-51e3-43f9-b7d9-b33b270779d0', amendment_process_is_exclusive_legitimacy_channel, conventional).
narrative_ontology:cs_axiom('96f72f4e-51e3-43f9-b7d9-b33b270779d0', foundational, provincial_referendum_lacks_independent_constitutional_force).
narrative_ontology:cs_axiom_status(provincial_referendum_lacks_independent_constitutional_force, holdable).
narrative_ontology:cs_axiom_grounding('96f72f4e-51e3-43f9-b7d9-b33b270779d0', provincial_referendum_lacks_independent_constitutional_force, conventional).
narrative_ontology:cs_reference_frame('96f72f4e-51e3-43f9-b7d9-b33b270779d0', founding_federal_compact_supremacy).
narrative_ontology:cs_drift_state('96f72f4e-51e3-43f9-b7d9-b33b270779d0', contemporary_secession_referenda_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('96f72f4e-51e3-43f9-b7d9-b33b270779d0', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, non_secessionist_provinces).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, national_bondholders).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, separatist_movement_leadership).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_rich_provincial_electorate).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, amendment_process_exclusivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the constitutional order and the courts that interpret it. Treats the amendment formula as the sole legitimate exit path and deploys judicial rulings, fiscal transfers, and federal police powers to make unilateral secession legally and practically unworkable. Collects continued tax base, resource revenue-sharing, and geopolitical standing by holding the federation together.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government, agenda_setter,
    institutional, civilizational, analytical, national).

% Benefit from the equalization payments and internal market access that depend on the federation remaining intact. Their fiscal position is subsidized in part by resource-rich provinces remaining inside the union; they support the constitutional-impossibility reading because it locks in that subsidy flow.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, non_secessionist_provinces, beneficiary,
    organized, generational, constrained, national).

% Hold sovereign debt priced on the assumption of continued territorial and fiscal integrity. A credible secession path would reprice risk; the constitutional-impossibility doctrine removes that tail risk from their portfolios at no cost to them.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, national_bondholders, beneficiary,
    powerful, biographical, arbitrage, global).

% Organizes referenda and legislative motions toward independence, only to have every path declared constitutionally void absent a federal amendment that federal partners have no incentive to grant. Bears the political and legal costs of pursuing an exit route the doctrine defines as categorically unavailable; cannot appeal to any forum the federal government does not also control.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, separatist_movement_leadership, payer,
    moderate, biographical, trapped, regional).

% Votes repeatedly for greater autonomy or independence and finds the results treated as politically informative but legally inert. Continues remitting resource revenue and taxes into federal structures while lacking any constitutionally cognizable route to translate electoral will into exit, since the amendment formula requires consent from the very parties who benefit from the status quo.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_rich_provincial_electorate, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_rich_provincial_electorate, excluded).

% Issues the reference opinions and rulings that formalize the impossibility doctrine, framing it as a neutral reading of constitutional text and structure rather than a policy choice. Its rulings are treated as dispositive by the federal government and are the primary mechanism by which the doctrine is operationalized.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_courts, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_courts, observer).

% Studies how this doctrine compares to secession jurisprudence elsewhere, noting that the 'no unilateral exit, amendment only' rule is one of several available doctrinal postures and not a logical necessity of federalism as such.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__constitutional_impossibility_reading, diffuse).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__constitutional_impossibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, judicially administrable rule for when territorial exit is legally valid, preventing ad hoc secession attempts from destabilizing currency, debt, defense, and treaty commitments shared across the federation.
% TRANSFER_FUNCTION: Channels the political and fiscal costs of foreclosed exit onto the resource-rich province and its separatist leadership, while the benefit of continued territorial integrity — tax base, debt stability, resource revenue sharing, geopolitical weight — flows to the federal government, non-secessionist provinces, and holders of federal debt.
% ABSENT_VOICES: The resource-rich provincial electorate has repeatedly expressed its preference through referenda; those results are heard as data but are structurally excluded from constitutional force because the amendment formula requires the consent of parties (other provinces, federal Parliament) who benefit from refusal. Indigenous treaty holders within the disputed territory are also absent from this reading's frame — their claims are treated as a separate legal question, not integrated into the secession threshold at all.
% DISAPPEARANCE_RATIONALE: If the constitutional-impossibility doctrine were abandoned tomorrow in favor of, say, a popular-sovereignty or grievance-threshold reading, the resource-rich province's referendum results would become legally actionable, debt markets would reprice federal and provincial bonds, and the amendment-formula veto currently held by non-secessionist provinces would lose its function as the sole gate on exit.
% FOUNDING_PROBLEM: Founding federal agreements needed a rule preventing any single province from unilaterally dissolving shared currency, debt, defense, and treaty obligations whenever a local majority wished it, which could destabilize the whole federation on short notice.
% FOUNDING_PROBLEM_CORROBORATION: The federal government and constitutional courts attest the problem (destabilizing unilateral exit) remains live and cite historical secession crises as evidence. Independent comparative constitutional scholars, writing from outside both the federal government and the separatist movement, corroborate that federations vary widely in how they resolve this tension and that the amendment-only rule is a specific policy choice rather than an inevitable one — supporting a contested rather than settled status.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__constitutional_impossibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__constitutional_impossibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__constitutional_impossibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).
:- end_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate-low (0.28) because the doctrine's primary function is genuinely coordinative — preventing unilateral currency, debt, and treaty disruption — and the direct fiscal transfer it protects (equalization payments funded partly by resource revenue) is a real but bounded flow. Suppression is authored higher (0.52) because the doctrine's persistence depends on courts and federal police power actively foreclosing any non-amendment exit route, not merely on the coordination benefit being self-evidently attractive. Theater is low-moderate (0.22): the doctrine is substantively enforced through real judicial and administrative machinery, not mere performance, though its share of purely symbolic reaffirmation (repeated reference rulings restating the same holding) grows slightly over the interval.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and constitutional courts sit at the agenda-setting end — they administer and can in principle alter the doctrine through further litigation or constitutional negotiation, though civilizational time horizon and analytical exit reflect their structural distance from being personally exited. Non-secessionist provinces and bondholders are beneficiaries with derived low d: they collect stability and fiscal transfer without bearing the political cost. The separatist leadership and the resource-rich electorate are targets: their exit route (unilateral secession) is precisely what the doctrine forecloses, and their only path (constitutional amendment) requires the consent of the parties who benefit from refusal, making their exit_options effectively trapped/constrained rather than mobile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing destabilizing unilateral fragmentation of shared currency, debt, and defense — remains partly live (contested status), which is why this reading resists full snare classification: it is not pure inertial extraction, there is a real coordination function corroborated by scholars outside the federal government. But the status is contested rather than clearly live, because the amendment-formula veto structurally advantages exactly the parties who benefit from the doctrine's persistence, which is the tangled-rope signature: genuine coordination function plus asymmetric extraction requiring active enforcement to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_as_natural_reading_vs_policy_choice,
    'Is the constitutional-impossibility reading the single correct interpretation of the founding constitutional text and structure, or is it one defensible policy choice among several (as comparative federalism suggests)?',
    'Comparative doctrinal analysis across federations with differing secession jurisprudence (e.g. jurisdictions permitting negotiated exit after a qualified referendum) combined with close textual and drafting-history analysis of this federation''s founding documents.',
    'If the doctrine is a contingent policy choice rather than the only defensible reading, its extractive component (locking exit behind a veto held by beneficiaries) is more clearly visible; if it is genuinely compelled by text and structure, the constraint is closer to a mountain-adjacent coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_as_natural_reading_vs_policy_choice, conceptual, 'Whether this reading is textually compelled or a contestable interpretive choice among several defensible framings.').

omega_variable(
    kernel_reading_selection_mechanism,
    'What determines which of the four sibling readings of the secession_legitimacy_boundary kernel becomes the operative constitutional doctrine — is it judicial precedent path-dependence, relative bargaining power of the parties, or genuine legal-textual determinacy?',
    'Process-tracing of the sequence of court rulings and political negotiations that entrenched this reading over the sibling readings; comparison with federations where a different reading (e.g. popular_sovereignty_reading) became dominant instead.',
    'If reading-selection tracks bargaining power of incumbent beneficiaries rather than legal necessity, the constitutional-impossibility reading''s claim to be a neutral mountain-like feature of the constitutional order weakens considerably, supporting a tangled_rope or even snare-leaning reclassification of THIS reading specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, conceptual, 'Whether the dominance of this particular reading among the four kernel readings reflects power asymmetry or textual necessity.').

omega_variable(
    amendment_formula_veto_asymmetry,
    'Does the amendment formula''s requirement of broad interprovincial consent function as neutral coordination machinery, or does it structurally hand a veto to exactly the parties who benefit from the resource-rich province remaining inside the federation?',
    'Formal analysis of the amendment formula''s voting thresholds against the actual coalition of provinces that would need to consent, cross-referenced with fiscal transfer data showing which provinces are net beneficiaries of the status quo.',
    'If the veto structurally concentrates in net-beneficiary provinces, the coordination story is substantially cover for extraction, pushing the classification toward tangled_rope or snare; if the veto distribution is genuinely dispersed and includes provinces with no fiscal stake, the coordination reading is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_formula_veto_asymmetry, empirical, 'Whether the constitutional amendment veto is captured by fiscal beneficiaries of continued union.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(sece_tr_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 16, 0.23).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement(sece_be_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(sece_su_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__constitutional_impossibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language 'secession legitimacy' concept per the ε-invariance principle. Each reading of the secession_legitimacy_boundary kernel — constitutional_impossibility_reading (this story), popular_sovereignty_reading, grievance_threshold_reading, treaty_primacy_reading — has its own ε, its own beneficiary/victim structure, and its own claimed type, because the four readings locate legitimacy in structurally different places (constitutional text/process vs. referendum result vs. injustice threshold vs. treaty consent) and would misleadingly average into a single incoherent ε if merged. This reading is upstream of the others in the sense that it is the currently operative doctrine against which the sibling readings are asserted as challenges; adoption of any sibling reading would require this reading's authority to be displaced.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
