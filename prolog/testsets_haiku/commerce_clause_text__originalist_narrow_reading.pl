% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Commerce Clause Originalist Reading: Border-Crossing Transaction Limit
 *   domain: constitutional/federalism/economic-regulation
 *
 * SUMMARY:
 *   This constraint embodies the originalist reading of the Commerce Clause:
 *   federal regulatory authority extends only to commerce that crosses state
 *   borders or involves instrumentalities of interstate movement (ships,
 *   railroads, telecommunications lines). All intrastate commerce remains
 *   subject to state police power. This reading repudiates the post-New Deal
 *   expansive construction (Wickard v. Filburn, NLRB v. Jones & Laughlin
 *   Steel, Gonzales v. Raich) under which the substantial-effects test
 *   permitted federal regulation of nearly all economic activity. The
 *   originalist reading is one instantiation of a contested constitutional
 *   kernel (the text 'Congress shall have Power...To regulate
 *   Commerce...among the several States') and produces a structurally
 *   distinctive constraint with identifiable beneficiaries (state
 *   governments, federalism advocates) and victims (federal regulatory
 *   capacity, uniform national standards, externality management). The
 *   claimed type is tangled_rope: the originalist reading coordinates a
 *   division of regulatory authority (coordination function) while extracting
 *   significant federal capacity and imposing fragmentation costs (extraction
 *   function), both achieved through active judicial enforcement of the
 *   narrow reading against competing interpretations.
 *
 * KEY AGENTS:
 *   - State governments: institutional beneficiaries retaining police power under this reading
 *   - Anti-federal-consolidation advocates: organized beneficiaries ideologically committed to limiting federal authority
 *   - Uniform national standards regimes (EPA, FDA, OSHA, NHTSA): institutional payers losing doctrinal authority
 *   - Externality management infrastructure: institutional payers unable to justify federal environmental/regulatory action
 *   - Federal regulatory capacity: institutional payer losing flexibility in jurisdictional argumentation
 *   - Originalist judiciary: agenda-setter enforcing narrow reading through invalidating/narrowing federal statutes
 *   - Expansive-commerce advocates (federal agencies, progressive scholars): excluded from this reading's legitimacy structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.68).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.45).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Commerce Clause Originalist Reading: Border-Crossing Transaction Limit").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional/federalism/economic-regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, 'ef2d8d91-66bc-4d7d-8c28-eb4f125a5935').
narrative_ontology:cs_kernel_codification('ef2d8d91-66bc-4d7d-8c28-eb4f125a5935', fixed_text).
narrative_ontology:cs_authority_grounding('ef2d8d91-66bc-4d7d-8c28-eb4f125a5935', lineage).
narrative_ontology:cs_interpretation_layer_present('ef2d8d91-66bc-4d7d-8c28-eb4f125a5935').
narrative_ontology:cs_reading_relation('ef2d8d91-66bc-4d7d-8c28-eb4f125a5935', commerce_clause_text__expansive_federal_reading, forecloses).
narrative_ontology:cs_reading_relation('ef2d8d91-66bc-4d7d-8c28-eb4f125a5935', commerce_clause_text__substantial_effects_limited_reading, forecloses).
narrative_ontology:cs_axiom('ef2d8d91-66bc-4d7d-8c28-eb4f125a5935', foundational, interstate_commerce_textually_limited_to_border_crossing).
narrative_ontology:cs_axiom_status(interstate_commerce_textually_limited_to_border_crossing, holdable).
narrative_ontology:cs_axiom_grounding('ef2d8d91-66bc-4d7d-8c28-eb4f125a5935', interstate_commerce_textually_limited_to_border_crossing, empirically_contingent).
narrative_ontology:cs_axiom('ef2d8d91-66bc-4d7d-8c28-eb4f125a5935', foundational, originalist_textual_methodology_binds_judicial_interpretation).
narrative_ontology:cs_axiom_status(originalist_textual_methodology_binds_judicial_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('ef2d8d91-66bc-4d7d-8c28-eb4f125a5935', originalist_textual_methodology_binds_judicial_interpretation, deontological).
narrative_ontology:cs_reference_frame('ef2d8d91-66bc-4d7d-8c28-eb4f125a5935', founding_era_federalism_boundaries).
narrative_ontology:cs_drift_state('ef2d8d91-66bc-4d7d-8c28-eb4f125a5935', contemporary_post_new_deal_framework, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('ef2d8d91-66bc-4d7d-8c28-eb4f125a5935', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, uniform_national_standards_regimes).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, externality_management_infrastructure).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, federal_regulatory_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, border_state_industries).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, intrastate_commerce_actors).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, border_state_industries).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, intrastate_commerce_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State legislatures, governors, and state regulatory agencies gain retained regulatory authority over intrastate commerce under the police power. They can set labor standards, environmental protections, product safety, and consumer protection rules without facing federal preemption based on interstate commerce grounds. This reading protects their revenue, autonomy, and policy discretion.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, state_governments, beneficiary,
    institutional, generational, analytical, national).

% Federalist scholars, conservative legal theorists, and state sovereignty advocates find constitutional warrant for their position in this reading. They argue for limiting federal power and can cite the originalist reading's textual grounding as evidence that broad federal authority is unconstitutional. They organize judicial appointment campaigns and amicus briefs to advance this reading.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates, beneficiary,
    organized, generational, mobile, national).

% Federal regulatory agencies (EPA, FDA, OSHA, FTC, NHTSA, etc.) that issue uniform national standards find their primary jurisdictional hook narrowed. They cannot justify their authority by claiming regulated activity has substantial effects on interstate commerce if that rationale is confined to border-crossing transactions. They must find alternative constitutional grounds (necessary-and-proper, treaty power, spending power) or face judicial invalidation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, uniform_national_standards_regimes, payer,
    institutional, generational, constrained, national).

% Federal systems for managing multi-state externalities (air pollution, water quality, species migration, climate change, infectious disease, financial contagion) cannot rely on interstate commerce authority for their constitutional foundation. These systems must operate through state-level agreements, dormant commerce clause preemption, or other constitutional hooks, or face fragmentation when states refuse to cooperate.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, externality_management_infrastructure, payer,
    institutional, generational, trapped, national).

% The federal government's general regulatory capacity contracts. Agencies must litigate each major statute's constitutional authority rather than invoking the well-established substantial-effects test. This creates litigation risk, delays regulation, and forces agencies to structure authority claims through narrower doctrinal bases (enumerated powers, necessary-and-proper means to enumerated powers).
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, federal_regulatory_capacity, payer,
    institutional, generational, constrained, national).

% Industries engaged in actual border-crossing commerce (shipping, trucking, telecommunications networks, pipeline operators, import/export) are within federal regulatory scope and subject to uniform federal standards. They benefit from regulatory certainty and preemption of conflicting state rules, but they also bear the cost of federal enforcement and cannot shop for favorable state regulation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, border_state_industries, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__originalist_narrow_reading, border_state_industries, payer).

% Businesses engaged exclusively in intrastate commerce (local retail, regional manufacturing not involving imported materials, state-licensed professions, local services) are insulated from federal regulation and subject to state police power. They benefit from avoiding federal compliance costs but face state-by-state regulatory variation and potential state protectionism. Their exit option is to scale into interstate commerce, which subjects them to federal authority.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, intrastate_commerce_actors, beneficiary,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__originalist_narrow_reading, intrastate_commerce_actors, payer).

% Originalist judges and justices interpret and enforce the narrow reading of interstate commerce. They invalidate federal statutes that rely on the substantial-effects rationale, narrow precedent supporting broad commerce authority, and write opinions emphasizing textual fidelity and federalism constraints. They set and enforce the interpretive line.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, originalist_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Federal agencies, progressive constitutional scholars, politicians who support broad federal regulatory authority, and civil rights advocates who rely on the commerce clause for federal anti-discrimination statutes are excluded from this reading's legitimacy structure. The originalist narrow reading disqualifies their theoretical framework and renders their preferred regulatory statutes constitutionally questionable or invalid. They are kept out of the constraint's decision-making by interpretive methodology itself.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, expansive_commerce_advocates, excluded,
    institutional, generational, constrained, national).

% Comparative federalism scholars study this constraint as an instantiation of competing constitutional visions of federal-state power allocation. They observe the trade-offs between regulatory uniformity and state experimentation, between national problem-solving capacity and local autonomy.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, observer_seat_comparative_federalism, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__originalist_narrow_reading, originalist_judiciary).
narrative_ontology:fixing_cost_class(commerce_clause_text__originalist_narrow_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a division of regulatory authority between federal and state governments: federal authority governs commerce crossing state borders and the instrumentalities that facilitate such commerce; state authority retained over intrastate commerce. This solves the dual-sovereignty problem inherent in federalism—how to allocate regulatory power without collapsing into either unitary federal control or anarchic state autonomy. The originalist reading claims to solve this problem by adhering to constitutional text rather than inventing new doctrinal tests.
% TRANSFER_FUNCTION: Moves regulatory authority, policy-setting power, and compliance-burden allocation from the federal government to state governments for all intrastate commerce. Also moves the burden of managing multi-state externalities and ensuring market uniformity away from the federal government, forcing those costs onto state-level coordination, interstate agreements, or reliance on alternative constitutional hooks. Federal regulatory agencies lose jurisdictional flexibility; state governments gain policy discretion.
% ABSENT_VOICES: Excluded from this reading's decision-making structure: federal regulatory agencies, progressive constitutional scholars, civil rights advocates, and expansion-of-commerce proponents. They would testify that the originalist reading is historically inaccurate, texturally implausible, and destructive to national problem-solving capacity. They are kept out by the interpretive methodology itself—originalism predefines the acceptable argumentative moves and excludes functionalist, living-constitution, and non-originalist reasoning from the legitimacy structure.
% DISAPPEARANCE_RATIONALE: If the originalist narrow reading were permanently displaced (e.g., by constitutional amendment or sustained judicial reversal), federal regulatory authority would consolidate dramatically. Environmental protection, labor standards, food and drug safety, telecommunications, financial regulation would all consolidate at the federal level under broad interstate commerce authority. State regulatory experimentation would shrink. Conversely, if the originalist reading were to become the stable, settled interpretation, federal regulatory capacity would contract sharply, state authority would expand, and regulatory fragmentation across state lines would increase. Multi-state problems (externalities, market competition, public health) would require interstate agreements or federal action through alternative constitutional hooks.
% FOUNDING_PROBLEM: The Framers of the Constitution intended to grant Congress specific, enumerated powers, including the power to regulate interstate commerce, while reserving police power to the states. They deliberately crafted federalism as a structural constraint on federal consolidation. The founding problem is: how can judicial interpretation prevent the federal government from expanding its power beyond its constitutional text by loose interpretation of the interstate commerce clause? The originalist reading holds that the post-New Deal substantial-effects test (Wickard v. Filburn, NLRB v. Jones & Laughlin Steel) violated the Framers' design by transforming 'interstate commerce' into a near-unlimited grant of federal authority.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (Randy Barnett, Ilya Somin) cite Founding-era documents, The Federalist Papers, and colonial precedent as evidence the Framers intended limited federal commerce authority. Mainstream constitutional law scholars and historians (Erwin Chemerinsky, Jack Rakove, H. Jefferson Powell) dispute this reading, arguing the Framers intended a broad commerce power, the substantial-effects test reflects constitutional purpose, and originalist interpretation of this clause is selective. The founding problem's status is contested because historical evidence is genuinely ambiguous: the founding documents do not unambiguously resolve whether 'interstate commerce' was meant narrowly (border-crossing only) or broadly (any commerce with interstate effects). No corroboration exists from outside the originalist-vs.-expansive camps; the disagreement is internal to constitutional law scholarship.
narrative_ontology:disappearance_verdict(commerce_clause_text__originalist_narrow_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__originalist_narrow_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__originalist_narrow_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the originalist reading transfers substantial regulatory authority from federal to state control, reducing federal governments' ability to address multi-state problems and imposing coordination burdens on states. The reading is NOT a natural law or brute fact; it is a constitutional interpretation requiring continuous judicial enforcement to prevent reversion to the substantial-effects framework. Suppression (0.45) is moderate-to-high: maintaining this narrow reading against the 90-year doctrinal incumbency of the expansive framework requires active suppression of the competing interpretation—judicial opinions must distinguish and narrow precedent supporting substantial-effects reasoning. Theater ratio (0.22) is low-to-moderate: while originalist textual methodology creates performative activity (extensive historical analysis, textual archaeology), the reading's substantive effect is real—it genuinely constrains federal jurisdiction. The measurement series traces the trajectory from Wickard (1937, extractiveness ~0.15, theater ~0.08) when the constraint was novel and theoretically marginal, through Lopez (1995, ~0.52) when originalism became a live judicial force, to the present (2026, ~0.68) where the reading has accumulated institutional presence and enforcing capacity. The extractiveness trajectory reflects not a change in the reading's theoretical position but rather the cumulative effect of originalist appointments and jurisprudential ascendancy increasing the enforcement machinery's strength. Theater is stable post-1995 at ~0.22 because originalist methodology is now institutionalized judicial practice, not a novelty.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (originalist judiciary) perceives this constraint as fidelity to constitutional text and restoration of proper federalism boundaries—from their seat, the substantial-effects framework is a doctrinal aberration to be corrected. The federal regulatory seats (EPA, FDA, Federal Reserve) perceive this constraint as an artificial, administrable-boundary-destroying limitation that paralyzes their capacity to address interstate externalities and uniform market regulation—from their seat, the originalist reading is textually implausible and destructive. State governments perceive this constraint as reclaiming lost authority—from their seat, it is a beneficial restoration of federalism. This divergence should compute into different per-seat type classifications: the agenda-setter may perceive rope (coordination of federalism); the federal payers perceive snare (extraction of federal capacity). The engine computes these divergences from the structural data (power, exit_options, time_horizon differentials).
 *
 * DIRECTIONALITY LOGIC:
 *   State governments are declared beneficiaries because this reading explicitly protects and expands their police power; they collect the authority transfer. Federal regulatory agencies are declared victims because their jurisdictional reach is constrained and their doctrinal flexibility is reduced; they bear the cost of having to argue harder for their regulatory authority. Uniform national standards regimes are victims because they lose the broad interstate commerce hook and must find alternative constitutional grounds. Externality management infrastructure is a victim because federal environmental/public-health authority is constrained by the narrow definition of interstate commerce. Anti-federal-consolidation advocates are beneficiaries because their federalism ideology is vindicated by this reading. The exit options reflect these relationships: state governments can exit by constitutional amendment but have no incentive to (exit_options = analytical, power = institutional); federal agencies cannot exit without doctrinal shift (exit_options = constrained); anti-consolidation advocates have some mobile exit via judicial appointment campaigns but ideological stake keeps them in the frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Framers' intent for limited federal enumerated powers) was once live and directly instantiated in Founding-era constitutional understanding. The substantial-effects framework declared this problem solved: federal commerce authority was sufficiently expansive to address interstate problems, and federalism remained as state police power for local matters. Under the mandatrophy lens, the originalist reading attempts to resurrect a dead founding problem—to treat the post-New Deal framework as a deviation and return to the Founding's original boundaries. This is a mismatch of disappearance_verdict (world_rearranges if the originalist reading prevails) and founding_problem_status (dead under the expansive framework, but asserted as live by originalism). The tension is not a defect in the constraint's classification but rather a key signal: the originalist reading's strength depends on whether it can credibly reframe the founding problem as un-dead. If it cannot (if courts and legislatures treat the substantial-effects framework as settled), the originalist reading becomes a piton—performed judicial activity maintaining textual fidelity but not actually displacing the functional framework. The theater_ratio remains stable at ~0.22 because originalist jurisprudence is now institutionalized enough to be real performance (not fakery), yet its practical effect remains limited while substantial-effects reasoning dominates actual outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_boundary_vs_economic_reality,
    'Is the textual boundary between ''interstate commerce'' (border-crossing, instrumentalities) and ''intrastate commerce'' (within-state production and consumption) a tenable constitutional line in modern integrated supply chains, or does it collapse under pressure from economic interdependence?',
    'Doctrinal boundary-testing via litigation: regulatory challenges to federal statutes under this reading''s narrow construction. If the judiciary can articulate a stable, administrable test for what is ''interstate'' vs. ''intrastate'' that survives application to agricultural production, manufacturing, telecommunications, and digital commerce, the boundary holds. If the test fragments or the line moves repeatedly, the boundary is unstable.',
    'If the boundary is administrable: the originalist reading is structurally viable and can be enforced consistently. If it collapses: the reading''s practical utility erodes and courts will revert to functional-effects reasoning (moving toward the expansive reading) or create new doctrinal safe harbors (moving toward the substantial-effects-limited reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_boundary_vs_economic_reality, empirical, 'Whether the originalist textual boundary between interstate and intrastate commerce is administrable in integrated modern economies.').

omega_variable(
    founding_intent_textual_ambiguity,
    'Does the Framers'' historical intent regarding the scope of ''interstate commerce'' unambiguously support the narrow reading (border-crossing only), or is the historical record ambiguous and subject to multiple interpretations?',
    'Historical scholarship and comparative constitutional analysis: reexamination of Founding-era documents, The Federalist Papers, colonial precedent, and parallel language in other constitutions. Scholarly consensus or divergence on what the phrase was understood to mean in 1787.',
    'Clear Framers'' intent favoring narrow reading would strengthen originalist claims to constitutional fidelity. Ambiguous or contested historical record would open space for alternative readings (expansive and limited) and weaken the claim that originalism is merely ''following the text,'' not making a choice among available interpretations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_intent_textual_ambiguity, empirical, 'Whether historical evidence unambiguously supports the originalist narrow reading of interstate commerce.').

omega_variable(
    reading_kernel_identity,
    'Is this constraint fundamentally a READING of the Commerce Clause text (a canonical kernel yielding different interpretations), or does it represent a NOVEL CONSTITUTIONAL CLAIM that the originalist methodology is applying to the text for the first time?',
    'Historical tracing: Did anyone in the Founding era, the 19th century, or the early 20th century advance this exact reading as the Constitution''s meaning? Or did originalism import this reading retroactively?',
    'If the reading was present in historical practice, it is a genuine historical interpretation and the constraint properly models a kernel reading. If it is a modern originalist innovation, it is a NEW structural claim about what the Constitution should say, not a recovery of what it originally said—the constraint would then be misclassified as a kernel reading and should instead be understood as a policy proposal grounded in originalist methodology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Whether this constraint is a historical kernel reading or a modern originalist policy innovation.').

omega_variable(
    originalist_textual_fixity,
    'Does this reading rely on a fixed, determinate textual meaning of ''interstate commerce,'' or does the meaning shift with how the originating generation would have understood the phrase in contemporary contexts (original public meaning vs. original intent)?',
    'Methodological analysis of originalist jurisprudence in action: does the reading''s application to modern phenomena (e.g., internet commerce, intrastate-but-interconnected digital platforms) maintain a consistent definition of ''interstate,'' or does the boundary flex to accommodate new situations?',
    'If the textual meaning is truly fixed, the reading''s application to novel cases should be predictable and consistent. If the meaning shifts, the reading is not rigidly textual but rather interpretively flexible—which undermines the claim to be doing ''mere'' interpretation and exposes the constraint as partly exercising judicial discretion about what the bounded definition permits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_textual_fixity, conceptual, 'Whether the originalist reading of ''interstate commerce'' maintains textual fixity or exhibits interpretive flexibility.').

omega_variable(
    sibling_reading_coordination_kernel,
    'Within a single constitutional framework (e.g., a single court''s jurisprudence), can the originalist reading coexist with the substantial-effects-limited reading, or do they logically foreclose each other?',
    'Doctrinal analysis: can a court hold that (1) federal commerce authority is limited to border-crossing transactions AND (2) federal authority extends to intrastate activity with substantial effects on interstate commerce? The readings appear to occupy the same logical space and thus appear to foreclose each other—but the engine will compute the relation based on the kernel context and reading claims.',
    'If they foreclose each other, the sibling readings cannot coexist within one framework; the kernel is a genuine binary fork, and the readings compete for judicial adoption. If they coexist (potentially at different factual premises or policy contexts), the constraint''s relationship to its siblings may shift from foreclosure to coexistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coordination_kernel, conceptual, 'Whether the originalist narrow reading and the substantial-effects-limited reading logically coexist or foreclose each other within a single constitutional framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 1937, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1937_wickard_judicial_novelty, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1937, 0.08).
narrative_ontology:measurement(theater_1964_commerce_clause_rationalization, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1964, 0.12).
narrative_ontology:measurement(theater_1995_lopez_originalist_pushback, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(theater_2005_raich_scalia_dissent_textual_focus, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2005, 0.21).
narrative_ontology:measurement(theater_2020_originalist_jurisprudence_ascendant, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(theater_2026_stable_originalist_performance, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(extractiveness_1937_wickard_nadir, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1937, 0.15).
narrative_ontology:measurement(extractiveness_1964_civil_rights_act, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1964, 0.35).
narrative_ontology:measurement(extractiveness_1995_united_states_v_lopez, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(extractiveness_2005_gonzales_v_raich, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement(extractiveness_2020_sebelius_v_national_federation, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(extractiveness_2026_contemporary, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(suppression_1937_lochner_era_residue, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1937, 0.25).
narrative_ontology:measurement(suppression_1964_federal_regulatory_expansion, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1964, 0.38).
narrative_ontology:measurement(suppression_1995_rehnquist_court_state_limits, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1995, 0.44).
narrative_ontology:measurement(suppression_2005_raich_expands_federal, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 2005, 0.46).
narrative_ontology:measurement(suppression_2020_conditional_federal_power, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement(suppression_2026_stable_enforcement_line, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__originalist_narrow_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__substantial_effects_limited_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, dormant_commerce_clause_state_protectionism).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, necessary_and_proper_clause_federal_scope).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Commerce Clause kernel. The expansive_federal_reading and substantial_effects_limited_reading are sibling constraints instantiating the same kernel with different interpretive premises. The three readings decompose a single contested constitutional text into structurally distinct constraints with different beneficiaries, victims, and classifications. All three readings affect dormant commerce clause doctrine (which presupposes some definition of interstate commerce) and the necessary-and-proper clause (which becomes the residual hook for federal authority if interstate commerce is narrowly construed).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
