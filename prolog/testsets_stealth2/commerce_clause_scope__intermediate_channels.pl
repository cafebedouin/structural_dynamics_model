% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__intermediate_channels, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause Three-Category Framework with Categorical Limits (Intermediate Channels Reading)
 *   domain: constitutional law/federalism
 *
 * SUMMARY:
 *   This story authors the intermediate_channels reading of the commerce
 *   clause scope kernel as a single ε-invariant constraint: the standing
 *   post-Lopez doctrinal framework under which federal power extends to (1)
 *   channels of interstate commerce, (2) instrumentalities and persons and
 *   things in interstate commerce, and (3) activities substantially affecting
 *   interstate commerce, bounded by three limiting principles: non-economic
 *   activity requires a jurisdictional element, aggregation applies only to
 *   economic activity, and regulation may not proceed through attenuated
 *   causal chains. The framework is the operative settlement of Lopez (1995),
 *   Morrison (2000), and Raich (2005), administered by the Supreme Court and
 *   navigated daily by Congress, federal agencies, state governments, and
 *   litigants. It genuinely coordinates (a workable, administrable
 *   federal-state boundary) and simultaneously extracts (an extensive
 *   transfer of regulatory and criminal jurisdiction to the federal
 *   government within the economic sphere, with the limits biting only at the
 *   margins and growing more manipulable over the interval). The claimed_type
 *   is authored independently from the metrics: I believe the framework is
 *   structurally a tangled rope, and the metrics describe what I believe is
 *   its actual operation; the engine computes per-seat classifications from
 *   the structural data and any divergence from the claim is the measurement
 *   the corpus exists to take. KEY AGENTS (by structural relationship) are
 *   enumerated in key_agents below.
 *
 * KEY AGENTS:
 *   - supreme_court — agenda setter (institutional/constrained): declares and polices the three-category framework and its limiting principles; could redraw or abandon the settlement at substantial legitimacy cost
 *   - congress — primary beneficiary, secondary payer (institutional/constrained): collects the transfer of regulatory and criminal jurisdiction; experiences the limits as drafting constraints to route around
 *   - federal_regulatory_agencies — secondary beneficiary (institutional/constrained): enforce the statutes the framework authorizes; adapt enforcement practice to each boundary shift
 *   - state_governments — beneficiary of the limits (organized/constrained): retain family law, criminal law, and education; litigate in multi-state coalitions; pay erosion at the margins
 *   - national_economic_actors — payer, secondary beneficiary (powerful/mobile): bear compliance burdens under the aggregation logic; gain uniform national commercial rules; restructure across jurisdictions
 *   - noncommercial_local_actors — primary payer (powerless/trapped): individuals whose local non-economic conduct is swept into federal criminal reach; appear only as case-specific defendants
 *   - doctrinal_coherence — payer, non-agent (powerless/trapped): the doctrine's own conceptual structure, which pays for every manipulative application of the economic/non-economic line and the jurisdictional element
 *   - state_courts — excluded (institutional/trapped): absorb displaced jurisdiction with no seat in the doctrinal dialogue
 *   - constitutional_law_scholars — analytical observer (analytical/analytical): map coherence and manipulability; shape long-run legitimacy conditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.5).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.4).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.5).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause Three-Category Framework with Categorical Limits (Intermediate Channels Reading)").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, '30890682-68a3-4f76-b180-6adf8fb9b875').
narrative_ontology:cs_kernel_codification('30890682-68a3-4f76-b180-6adf8fb9b875', fixed_text).
narrative_ontology:cs_authority_grounding('30890682-68a3-4f76-b180-6adf8fb9b875', lineage).
narrative_ontology:cs_interpretation_layer_present('30890682-68a3-4f76-b180-6adf8fb9b875').
narrative_ontology:cs_reading_relation('30890682-68a3-4f76-b180-6adf8fb9b875', commerce_clause_scope__narrow_originalist, coexists_with).
narrative_ontology:cs_reading_relation('30890682-68a3-4f76-b180-6adf8fb9b875', commerce_clause_scope__broad_effects_test, coexists_with).
narrative_ontology:cs_axiom('30890682-68a3-4f76-b180-6adf8fb9b875', foundational, aggregated_economic_activity_regulable).
narrative_ontology:cs_axiom_status(aggregated_economic_activity_regulable, holdable).
narrative_ontology:cs_axiom_grounding('30890682-68a3-4f76-b180-6adf8fb9b875', aggregated_economic_activity_regulable, instrumental).
narrative_ontology:cs_axiom('30890682-68a3-4f76-b180-6adf8fb9b875', foundational, categorical_limits_judicially_enforceable).
narrative_ontology:cs_axiom_status(categorical_limits_judicially_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('30890682-68a3-4f76-b180-6adf8fb9b875', categorical_limits_judicially_enforceable, conventional).
narrative_ontology:cs_reference_frame('30890682-68a3-4f76-b180-6adf8fb9b875', categorically_bounded_national_commerce_power).
narrative_ontology:cs_drift_state('30890682-68a3-4f76-b180-6adf8fb9b875', post_raich_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('30890682-68a3-4f76-b180-6adf8fb9b875', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, congress).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, noncommercial_local_actors).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, doctrinal_coherence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, national_economic_actors).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, congress).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, national_economic_actors).
narrative_ontology:constraint_vindicates(commerce_clause_scope__intermediate_channels, substantial_effects_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__intermediate_channels, categorical_limits_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares and polices the three-category framework and its limiting principles: decides which federal statutes fall inside or outside the commerce power, articulates the jurisdictional-element requirement, the economic/non-economic line, and the prohibition on attenuated causal chains. Could redraw or abandon the settlement by overruling its own precedents at substantial legitimacy cost; in practice maintains it through incremental doctrine.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Enacts regulatory and criminal legislation within the three categories and drafts jurisdictional elements to survive review. Collects extensive authority over the national economy, including purely intrastate economic activity via aggregation, while its power over non-economic local conduct is formally checked. Experiences the limits as drafting constraints to route around rather than hard boundaries; cannot exit the framework short of constitutional amendment.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, congress, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, congress, payer).

% Enforce federal firearms, controlled-substances, environmental, and civil-rights statutes whose reach the framework authorizes. Their enforcement footprint expands or contracts with each doctrinal boundary shift; they have no seat in redrawing the boundary and adapt enforcement practice to whatever the Court announces.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_regulatory_agencies, beneficiary,
    institutional, biographical, constrained, national).

% Retain formal authority over family law, criminal justice, and education under the limiting principles, and litigate collectively through multi-state coalitions to defend that reserve. They bear erosion at the margins as federal criminal jurisdiction and regulatory preemption creep into local matters, cannot exit the federal system, and their remedies are litigation and amendment campaigns.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_governments, beneficiary,
    organized, generational, constrained, national).

% Businesses and market participants subject to comprehensive federal regulation of economic activity. They bear compliance costs under the aggregation logic, since even local operations are regulable if their class affects commerce, while benefiting from uniform national commercial rules. They can restructure transactions and operations across jurisdictions and lobby for favorable boundary-drawing.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, national_economic_actors, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, national_economic_actors, beneficiary).

% Individuals whose local, non-economic conduct is swept into federal reach: firearm possession near a school, home cultivation of a regulated plant, locally confined violence prosecuted federally via jurisdictional elements. They enter the framework only as case-specific defendants, bear criminal liability and litigation costs, and cannot exit federal jurisdiction.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, noncommercial_local_actors, payer,
    powerless, biographical, trapped, local).

% The conceptual structure of the doctrine itself. The economic/non-economic distinction, the jurisdictional-element requirement, and the attenuated-causation prohibition are supposed to form a coherent limiting architecture, but each is applied manipulably: the economic label stretches to cover home-grown marijuana, jurisdictional elements become drafting boilerplate, and causal-chain scrutiny appears and disappears. The framework's coherence pays the cost of every manipulative application. Authored as a non-agent entity per the kernel's expected structural delta; it feeds no directionality arithmetic.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, doctrinal_coherence, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__intermediate_channels, doctrinal_coherence).

% Apply the federal framework and absorb displaced jurisdiction at the margins, as federal criminal statutes and regulatory preemption remove matters from state dockets. They have no seat in the doctrinal dialogue that redraws the boundary and learn each shift from the federal reports that bind them.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_courts, excluded,
    institutional, generational, trapped, national).

% Map the framework's coherence, document the manipulability of its distinctions, and advocate rival allocations of authority from both directions. They shape the long-run legitimacy conditions of the doctrine but decide no cases and collect no rents from it.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__intermediate_channels, congress).
narrative_ontology:fixing_cost_class(commerce_clause_scope__intermediate_channels, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates regulatory authority in a dual-sovereign system: the three categories tell Congress where it may govern (channels, instrumentalities, substantial effects), the limiting principles reserve family law, criminal law, and education to the states, and the framework gives lower courts an administrable test for policing the boundary, solving once and centrally a boundary-drawing problem that would otherwise be relitigated statute by statute.
% TRANSFER_FUNCTION: Moves regulatory and criminal jurisdiction from state governments to the federal government across the economic sphere, including intrastate economic activity via aggregation; moves compliance burdens and enforcement exposure onto economic actors and local individuals; and moves litigation costs and doctrinal uncertainty onto whoever must litigate the manipulable lines.
% ABSENT_VOICES: Individuals subject to federal criminal jurisdiction appear only as case-specific defendants: no institutional seat represents people whose local conduct is swept into federal reach, so the payer-side experience of the framework is assembled retroactively from case records. State courts absorb displaced jurisdiction without participation in the doctrinal dialogue. The framework's apparent stability is partly an artifact of who gets to be a repeat player: Congress, the Court, and organized state coalitions litigate the boundary, while the individuals who pay for it do not.
% DISAPPEARANCE_RATIONALE: If the three-category framework and its limits vanished overnight, every federal statute enacted in reliance on it would face immediate challenge under whichever rival allocation the surviving courts adopted; federal criminal jurisdiction over firearms and controlled substances would contract or expand dramatically; states would regain or lose authority over whole domains; and decades of enforcement practice, agency structure, and compliance arrangements built on the current boundary would have to reorganize.
% FOUNDING_PROBLEM: After Wickard v. Filburn (1942) rendered the commerce power effectively plenary, the problem was how to preserve the national economic regulatory state while restoring a judicially enforceable boundary that protects state authority over local, non-economic life. Lopez (1995), Morrison (2000), and Raich (2005) built this framework to answer it.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court majority attests the problem is live, since each boundary case reasserts it, but that is a beneficiary seat. Outside the beneficiary set: state attorneys general litigating multi-state challenges attest that federal reach into local matters remains a live grievance; constitutional scholars across the spectrum document that the limiting principles rarely bite after Raich and argue the founding problem is substantially dead in practice; dissenting justices in both directions (Thomas in Raich; the Morrison dissents) attest the framework solves neither the limits problem nor the coherence problem. Corroboration exists but is itself split, which is why the status is contested rather than live or dead.
narrative_ontology:disappearance_verdict(commerce_clause_scope__intermediate_channels, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__intermediate_channels, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__intermediate_channels, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__intermediate_channels, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__intermediate_channels_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__intermediate_channels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.5 (medium): within the economic sphere the framework operates as a near-plenary grant, since Raich's aggregation logic reaches purely intrastate economic activity, while the limiting principles formally check only non-economic local conduct, so the net transfer of authority is large but bounded. Suppression is authored at 0.4 as a raw structural property, unscaled by power or scope: the framework binds through judicial supremacy and stare decisis; the rival allocations remain live in scholarship and dissents and constitutional amendment remains available in principle, so alternatives are costly but not foreclosed. Theater_ratio is authored at 0.4 and rising across the interval: Lopez and Morrison did real limiting work, but after Raich the jurisdictional element has become drafting boilerplate, the economic/non-economic label does manipulable work, and Gundy dicta signaled Morrison's disrepair, so a growing share of the limiting machinery is performative maintenance. Accessibility_collapse is 0.4: understanding the framework does not collapse the alternatives, since both rival allocations remain litigable positions. Resistance is 0.55: the framework is contested from both directions simultaneously, originalist scholarship and state-coalition litigation from the narrow side and progressive scholarship and expansive congressional drafting from the broad side. The measurement series run on one shared time grid (1995, 2000, 2005, 2012, 2019, 2025) with all three tracked metrics authored at every point. The base_extractiveness series peaks at Raich (0.55) when aggregation folded local instances into federal reach, dips at NFIB (0.50) when a narrow new limit was announced, and settles at 0.50. The suppression_requirement series models the limiting machinery's enforcement intensity rather than extraction: it ratchets up through Lopez and Morrison (0.60 to 0.65), decays after Raich (0.45), is partially reasserted in NFIB (0.50), then erodes through Gundy dicta to 0.40, an enforcement-decay trajectory for the limits, not for the framework itself.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same doctrine. From the congress seat the framework is an enabling structure it learned to draft within: the limits are manageable constraints, so that seat computes a coordination-heavy classification. From the noncommercial_local_actors seat the same framework is an unpredictable federal criminal reach entered only as a defendant: extraction with no offsetting coordination benefit and trapped exit. From the state_governments seat the framework is a defended boundary that erodes at the margins: partially protective, partially extractive. From the supreme_court seat the framework is its own administered settlement, maintained at acceptable legitimacy cost. The engine computes this divergence from roles, power, and exit options; this commentary does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: congress and federal_regulatory_agencies collect the transfer of regulatory and criminal jurisdiction (d near the beneficiary end); state_governments collect the formal reservation of family law, criminal law, and education (low d, moderated by their secondary exposure to erosion at the margins, which is why congress also carries a payer secondary role). Victim declarations drive high directionality: noncommercial_local_actors bear federal criminal liability without an offsetting benefit and cannot exit federal jurisdiction (d near the full-target end). doctrinal_coherence is declared as a victim but authored with agent: false, so it is excluded from beneficiary/victim derivation and directionality arithmetic; it is authored to record where the manipulability costs land, per the kernel's expected structural delta. national_economic_actors sit near symmetric: compliance burdens under the aggregation logic against uniform-rules benefits, with mobile exit damping their extraction. The supreme_court is agenda_setter rather than a declared beneficiary: its structural relationship is maintenance, and its institutional power is what makes the framework's suppression credible. Scope amplification applies modestly at national scale, where verification of the boundary is institutionally expensive.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Against snare: the framework has a genuine, non-cover coordination function, since it solves the boundary-drawing problem of a dual-sovereign system once and centrally and lower courts can administer it; a snare reading would erase the real work the three categories do. Against rope: the limiting machinery extracts real costs through the same structure that coordinates (litigation uncertainty, manipulable line-drawing, criminal exposure for local actors), and the extraction is asymmetric: the coordination benefit concentrates in congress, the enforcement benefit in federal agencies, while the costs concentrate in local actors and in the doctrine's own coherence. Mandatrophy: the founding problem, a judicially enforceable limit after Wickard's plenary grant, is contested, since the framework persists while its limiting function decays. The theater_ratio series is the early-warning instrument: if the economic/non-economic line collapses into a pure label, the limiting machinery goes piton-like, theatrical maintenance of a spent function, even as the enabling structure remains a live tangled rope. The measurements are built to catch that divergence; the R5 mismatch check (contested status x world_rearranges verdict) cross-checks it from the genealogy side.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is the intermediate_channels reading of the commerce_clause_scope kernel; what would the sibling readings (narrow_originalist, broad_effects_test) change structurally if instantiated instead?',
    'Author the sibling stories as separate ε-invariant files and compare computed classifications: narrow_originalist should shrink the federal beneficiary set, elevate state_governments and local actors to primary beneficiaries, and lower ε; broad_effects_test should expand the federal beneficiary set, convert state_governments and local actors into primary payers, and raise ε.',
    'The victim and beneficiary structure authored here is reading-relative: under narrow_originalist the primary payers become congress and federal agencies; under broad_effects_test the primary payers become states and local actors. Cross-reading comparisons of ε or victim sets without this delta would be invalid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer-frame delta: how sibling readings of the commerce kernel restructure beneficiaries, victims, and extraction.').

omega_variable(
    economic_noneconomic_line_stability,
    'Is the economic/non-economic distinction a real structural limit or a manipulable judicial label?',
    'Track lower-court classifications of challenged activities over time and the success rate of commerce-clause challenges: if classifications track litigant sophistication rather than activity character, the line is a label; if outcomes are predictable from activity type, the line is structural.',
    'If the line is purely manipulable, theater_ratio rises toward 0.6 or above and the limiting machinery drifts toward piton-like theatrical maintenance; if structural, the tangled_rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_noneconomic_line_stability, empirical, 'Stability of the doctrine''s core limiting distinction.').

omega_variable(
    constructed_vs_textual_settlement,
    'Is the three-category framework compelled by the constitutional text, or is it a judicially constructed equilibrium serving identifiable institutional beneficiaries (congressional drafting practice, judicial agenda control)?',
    'Originalist textual and historical analysis set against institutional analysis of who gains from the framework''s persistence: if the text underdetermines the three categories and the beneficiaries defend them predictably, the settlement is constructed.',
    'If constructed, the framework''s limits are better read as jurisdictional theater around a power-sharing settlement between Congress and the Court, and the constraint''s effective suppression is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_textual_settlement, conceptual, 'Textual compulsion versus constructed equilibrium serving institutional beneficiaries.').

omega_variable(
    aggregation_residual_reach,
    'After Raich, does any non-economic local activity remain beyond federal reach in practice, or have aggregation plus the channels and instrumentalities categories swallowed the limiting principles?',
    'Catalog post-2005 commerce-clause challenges and their outcomes: a shrinking set of successful challenges indicates the victim set noncommercial_local_actors is residual rather than protected.',
    'If no practical residual exists, the limiting principles are cover and the framework''s extraction is higher than authored; the victim set would need re-authoring to include whole classes of local conduct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_residual_reach, empirical, 'Whether the limiting principles leave a real protected zone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__intermediate_channels, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_scope__intermediate_channels, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_scope__intermediate_channels, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_scope__intermediate_channels, theater_ratio, 2012, 0.32).
narrative_ontology:measurement(comm_tr_t2019, commerce_clause_scope__intermediate_channels, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_scope__intermediate_channels, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(comm_be_t1995, commerce_clause_scope__intermediate_channels, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_scope__intermediate_channels, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_scope__intermediate_channels, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(comm_be_t2012, commerce_clause_scope__intermediate_channels, base_extractiveness, 2012, 0.5).
narrative_ontology:measurement(comm_be_t2019, commerce_clause_scope__intermediate_channels, base_extractiveness, 2019, 0.52).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_scope__intermediate_channels, base_extractiveness, 2025, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1995, commerce_clause_scope__intermediate_channels, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_scope__intermediate_channels, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_scope__intermediate_channels, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(comm_su_t2012, commerce_clause_scope__intermediate_channels, suppression_requirement, 2012, 0.5).
narrative_ontology:measurement(comm_su_t2019, commerce_clause_scope__intermediate_channels, suppression_requirement, 2019, 0.42).
narrative_ontology:measurement(comm_su_t2025, commerce_clause_scope__intermediate_channels, suppression_requirement, 2025, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, necessary_and_proper_clause_scope).

% DUAL FORMULATION NOTE:
% The colloquial label 'commerce clause scope' covers three structurally distinct constraints, one per reading of the kernel. This story authors ε for the intermediate_channels reading's standing arrangement (the post-Lopez three-category framework with categorical limits) as that reading assesses it: moderate extraction, extensive federal power within the economic sphere with limits that bite only at the margins. The narrow_originalist sibling authors a much smaller federal power with a different beneficiary/victim structure (states and local individuals as primary beneficiaries, congress as primary payer); the broad_effects_test sibling authors near-plenary federal power (states and local actors as primary payers). Each sibling is a separate file with its own ε, stakeholders, and classification; the family is linked via affects_constraints. The necessary_and_proper link records the structural dependency surfaced in Raich, where the power to regulate carried the power to enforce local instances.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
