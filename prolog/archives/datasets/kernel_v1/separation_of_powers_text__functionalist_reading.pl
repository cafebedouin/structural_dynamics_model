% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__functionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__functionalist_reading, []).

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
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Separation of Powers as Flexible Functionalist Framework
 *   domain: constitutional_law/administrative_law/political_theory
 *
 * SUMMARY:
 *   The functionalist reading of the separation of powers treats the
 *   constitutional framework as a flexible instrument for allocating
 *   authority to implement governance objectives, rather than as a rigid
 *   categorical prohibition on delegation. From this perspective, Congress
 *   legitimately delegates broad authority to executive agencies via
 *   intelligible-principle mandates ('establish fair labor standards',
 *   'protect the environment', 'ensure public safety'), and courts defer to
 *   agency interpretations of their statutory mandates through doctrines like
 *   Chevron deference. This reading dominates contemporary American
 *   constitutional law and administrative practice. The functionalist reading
 *   generates a tangled-rope constraint: it provides genuine coordination
 *   benefits (agencies can respond to technical complexity and changing
 *   conditions without constant legislative revision) while simultaneously
 *   extracting from textual constitutional constraint (the nondelegation
 *   doctrine is suppressed, congressional legislative authority is
 *   diminished, and executive power expands). The constraint exhibits
 *   moderate extractiveness (0.38) reflecting the mixed
 *   coordination-extraction character: some delegation is necessary for
 *   complex governance; much additional delegation is undertaken for
 *   institutional convenience or executive aggrandizement. Theater ratio
 *   (0.48) reflects that courts perform nondelegation review while rarely
 *   invalidating delegations, creating performative constraint while
 *   permitting unrestricted functional expansion.
 *
 * KEY AGENTS:
 *   - Administrative Agencies: Primary beneficiary (institutional/arbitrage) — gain operational authority and legitimacy through delegation framework
 *   - Executive Branch: Secondary beneficiary (powerful/mobile) — expands authority through agency delegation and Chevron deference
 *   - Congress: Mixed actor (institutional/constrained) — benefits from ability to pass broad mandates; bears cost of reduced oversight capacity and agency independence
 *   - Nondelegation Doctrine: Primary victim (powerless/trapped) — constitutional principle systematically suppressed by intelligible-principle framing that permits unlimited delegation
 *   - Textual Constitutional Constraint: Secondary victim (powerless/trapped) — constitutional text's structural requirement ('all legislative Power') subordinated to functionalist instrumentalism
 *   - Reform Movement (Organized): Organized opponents (organized/mobile) — advocates for revival of nondelegation, major-questions doctrine, or administrative procedure codification
 *   - Formalist Doctrine: Institutional artifact (institutional/arbitrage) — persists in rhetoric while losing functional relevance (piton perspective)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choice as immutable governance reality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.38).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.35).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Separation of Powers as Flexible Functionalist Framework").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional_law/administrative_law/political_theory").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, '0fa3d562-09bb-40e4-9229-2204117d44cd').
narrative_ontology:cs_kernel_codification('0fa3d562-09bb-40e4-9229-2204117d44cd', fixed_text).
narrative_ontology:cs_authority_grounding('0fa3d562-09bb-40e4-9229-2204117d44cd', lineage).
narrative_ontology:cs_interpretation_layer_present('0fa3d562-09bb-40e4-9229-2204117d44cd').
narrative_ontology:cs_reading_relation('0fa3d562-09bb-40e4-9229-2204117d44cd', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0fa3d562-09bb-40e4-9229-2204117d44cd', separation_of_powers_text__unitary_executive_reading, influences).
narrative_ontology:cs_axiom('0fa3d562-09bb-40e4-9229-2204117d44cd', foundational, delegation_compatibility_with_separation).
narrative_ontology:cs_axiom_status(delegation_compatibility_with_separation, holdable).
narrative_ontology:cs_axiom_grounding('0fa3d562-09bb-40e4-9229-2204117d44cd', delegation_compatibility_with_separation, instrumental).
narrative_ontology:cs_axiom('0fa3d562-09bb-40e4-9229-2204117d44cd', foundational, intelligible_principle_sufficiency).
narrative_ontology:cs_axiom_status(intelligible_principle_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('0fa3d562-09bb-40e4-9229-2204117d44cd', intelligible_principle_sufficiency, deontological).
narrative_ontology:cs_reference_frame('0fa3d562-09bb-40e4-9229-2204117d44cd', flexible_institutional_separation).
narrative_ontology:cs_drift_state('0fa3d562-09bb-40e4-9229-2204117d44cd', contemporary_administrative_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0fa3d562-09bb-40e4-9229-2204117d44cd', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, administrative_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, executive_branch).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, pragmatic_governance).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, textual_constitutional_constraint).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, nondelegation_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NONDELEGATION DOCTRINE (SNARE) — The constitutional principle that Congress cannot delegate legislative power to the executive is systematically suppressed by functionalist reasoning. Once a delegation is deemed 'intelligible' (merely requiring agencies to implement a principle), the doctrine has no escape route. Trapped in a framework that defines away its own application. Theater-heavy: Courts perform nondelegation review while never finding violations, creating the appearance of constraint while permitting unlimited delegation.
constraint_indexing:constraint_classification(separation_of_powers_text__functionalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TEXTUAL CONSTITUTIONAL CONSTRAINT (SNARE) — The constitutional text's structural requirement that 'all legislative Power' vest in Congress is subordinated to functionalist instrumentalism: 'all' becomes 'substantially all', which becomes 'most', which becomes 'whatever courts deem necessary for effective governance'. The text is trapped in a framework where its own plain meaning is treated as naive. At generational timescale, the extraction accumulates—constitutional constraint erodes into advisory fiction.
constraint_indexing:constraint_classification(separation_of_powers_text__functionalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: ADMINISTRATIVE AGENCIES (ROPE) — Functionalism provides legitimate coordination function: agencies solve the problem of implementing broad legislative directives in complex policy domains (environmental regulation, securities law, labor standards). The constraint flexibly allocates authority to entities with technical expertise and responsiveness capacity. From the agency perspective, functionalism is efficient coordination—they have exit options (can propose alternative regulatory approaches within their mandate) and clear benefits (operational authority and institutional legitimacy).
constraint_indexing:constraint_classification(separation_of_powers_text__functionalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONGRESS (TANGLED ROPE) — Congress benefits from functionalism (can pass broad mandates without specifying implementation details, reducing legislative burden and enabling compromise on general principles). But Congress also bears extraction: agencies exercise delegated power with limited congressional oversight, and courts' deference doctrines (Chevron, etc.) constrain Congress's ability to reverse agency interpretations without explicit statutory revision. Constrained exit—Congress can revoke delegation but faces political and coordination costs.
constraint_indexing:constraint_classification(separation_of_powers_text__functionalist_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EXECUTIVE BRANCH (TANGLED ROPE) — Executive benefits from functionalism: agencies expand executive authority into policy domains via delegation interpretation. But executive is also constrained by functionalist reasoning in reverse—when courts find delegation unconstitutionally broad (rare) or when Congress uses 'intelligibility' principle to impose procedural requirements (notice-and-comment, arbitrary-and-capricious review), the executive cannot claim pure discretion. Mobile exit options—executive can propose legislative amendments, administrative procedure reforms.
constraint_indexing:constraint_classification(separation_of_powers_text__functionalist_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: REFORM MOVEMENT (SCAFFOLD) — Organized administrative reform advocates (constitutional scholars, good-government groups, some judges) see functionalist flexibility as a temporary accommodation awaiting a more structured framework. They propose: heightened major questions doctrine, explicit nondelegation revival, clear congressional statement requirements, or administrative procedure codification that would replace ad-hoc functionalism with rule-based constraint. This perspective has a sunset: if major-questions doctrine or nondelegation revival gains traction, functionalism's extraction mechanism weakens.
constraint_indexing:constraint_classification(separation_of_powers_text__functionalist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: FORMALIST DOCTRINE (PITON) — The traditional formalist principle—rigid categorical boundaries between legislative, executive, and judicial power—persists in constitutional rhetoric and opinion structure but has lost functional relevance. Courts still cite formalist categories (Marbury, Myers) while functionalism determines actual outcomes. Theater ratio is high: the form of rigorous separation-of-powers analysis is performed while substance is delegated. The piton is maintained through institutional inertia and symbolic value, not functional constraint.
constraint_indexing:constraint_classification(separation_of_powers_text__functionalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, functionalism reflects an immutable structural reality: complex governance requires executive specialization and legislative delegation. The separation of powers cannot be maintained in a functionally differentiated state without sacrificing administrative capacity. This perspective risks naturalizing what is actually a contestable institutional choice. The analytical frame sees the constraint as inherent to modern governance rather than a contingent constitutional reading.
constraint_indexing:constraint_classification(separation_of_powers_text__functionalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__functionalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(separation_of_powers_text__functionalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(separation_of_powers_text__functionalist_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, TR),
    TR >= 0.70.

:- end_tests(separation_of_powers_text__functionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The functionalist reading genuinely solves coordination problems—agencies can implement complex regulatory schemes without constant legislative revision, and specialized expertise improves policy quality. But functionalism also permits extractive expansion: agencies interpret delegated authority beyond what Congress intended, Chevron deference constrains congressional ability to reverse agency interpretations, and executive power accumulates during emergency or deference episodes. The base extractiveness value reflects a mixed mechanism. Measurement trajectory shows slow increase (0.28 → 0.38 over 50 years), indicating gradual delegation accumulation and intensified agency independence. Suppression (0.35): Moderate. The nondelegation doctrine provides formal constraint but is suppressed through intelligible-principle reasoning that renders it nearly unenforceable. Congress could impose stricter statutory language or require explicit findings, but political incentives favor broad delegation. Agencies face administrative procedure requirements (notice-and-comment, arbitrary-and-capricious review) but these are secondary safeguards, not primary suppression. Theater ratio (0.48): Moderate. Courts perform nondelegation analysis in opinions (formal procedure) while almost never invalidating delegations (functional negligibility). The gap between doctrinal language and actual enforcement is substantial but not maximal—major-questions doctrine and administrative procedure review provide some real constraint, preventing the theater from being purely performative. Trajectory shows slow increase in theater (0.38 → 0.48), reflecting that separation-of-powers rhetoric persists while functional enforcement decays.
 *
 * PERSPECTIVAL GAP:
 *   The functionalist reading produces a radical perspectival divergence. Administrative agencies see legitimate coordination (Rope)—they are solving genuine governance problems. Congress sees mixed benefit and extraction (Tangled Rope)—they delegate for convenience but lose control. The executive sees expansion opportunity (Tangled Rope)—agencies multiply executive authority. But the nondelegation doctrine and textual constraint both see pure extraction (Snare)—their authority is systematically suppressed by a framework that defines them away. Reform advocates see a temporary problem with an exit path (Scaffold)—functionalism is an accommodation awaiting stricter doctrine revival. Formalist doctrine sees a degraded institutional mechanism (Piton)—it persists in rhetoric while losing functional relevance. The analytical observer risks seeing immutable governance necessity (Mountain)—functionalism reflects structural requirements of modern states—but this naturalizes what is actually a contestable reading of the constitutional kernel. The perspectival gaps are extreme: the constraint is Rope from one perspective, Snare from another, Mountain from a third, all from the same structural data. This is diagnostic of how constitutional readings function—the 'same' separation-of-powers text generates opposing classifications depending on which constitutional principle (executive efficiency vs. legislative supremacy vs. nondelegation) one treats as foundational.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position in the delegation extraction flow. Beneficiaries (agencies, executive) have arbitrage options—they can propose alternative regulatory approaches, negotiate with Congress, or adjust implementation without exiting the system. This produces low d (around 0.15-0.20), resulting in negative or minimal effective extraction from their perspective. Victims (nondelegation doctrine, textual constraint) have trapped options—their only 'exit' would be overthrowing functionalism entirely and replacing it with formalism, which is structurally difficult once delegation becomes entrenched. This produces high d (around 0.85-0.95), resulting in maximum experienced extraction. Congress occupies middle ground: constrained exit (could impose statutory strictures or revoke delegations, but faces coordination costs and political opposition). This produces moderate d (around 0.55-0.65), resulting in moderate extraction despite being a beneficiary in some respects. The analytical perspective's directionality is derived from observational access to the full structure: the analytical frame sees the extraction from all perspectives simultaneously, producing canonical d around 0.72.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that the functionalist reading legitimately combines coordination (valid agency delegation solves governance complexity) with extraction (functionalism permits authority expansion beyond what text or nondelegation doctrine would permit). The tangled-rope classification is not indeterminate—it reflects the genuine structure where the same reading enables efficient administration AND systematic suppression of constitutional constraint. Mandatrophy is resolved by accepting that the constraint is authentically mixed, not by claiming it is 'really' one type. The alternative readings would resolve mandatrophy differently: formalism would prioritize constitutional constraint over administrative efficiency, shifting toward snare or mountain; unitary-executive would prioritize executive coherence over separated authority, shifting toward rope. The existence of coexisting readings is itself the manifestation of mandatrophy at the constitutional level—no single reading can simultaneously maximize textual fidelity, administrative efficiency, and power-checking constraint. The functionalist reading sacrifices textual fidelity and constraint capacity to maximize administrative efficiency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intelligible_principle_threshold,
    'What constitutes an ''intelligible principle'' sufficient to validate legislative delegation under functionalist reading?',
    'Historical analysis of congressional statutes deemed valid vs invalid under intelligible-principle test; comparison of specificity levels across upheld delegations',
    'If threshold is truly constraining: functionalism produces meaningful nondelegation review. If threshold is permissive: intelligible-principle doctrine becomes purely formal, and extracted authority is unrestricted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intelligible_principle_threshold, conceptual, 'Threshold definition for intelligible principle validity').

omega_variable(
    formalist_vs_functionalist_foreclosure,
    'Does the functionalist reading logically foreclose the formalist reading, or do they represent coexisting institutional positions?',
    'Analysis of whether a single legal/constitutional framework can hold both: (a) rigid categorical separation (formalism) and (b) flexible overlapping authority (functionalism). Can courts apply both simultaneously, or must one be chosen?',
    'If forecloses: the readings compete as mutually exclusive constitutional theories. If coexists: both persist across institutional coalitions and time periods, and the constraint is the contest itself rather than victory of one reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalist_vs_functionalist_foreclosure, conceptual, 'Logical relationship between formalist and functionalist approaches').

omega_variable(
    delegation_extraction_vs_coordination,
    'Is the delegation of authority from Congress to executive/agencies primarily a coordination mechanism solving governance complexity, or an extraction mechanism where Congress loses control?',
    'Empirical: Congressional ability to override or constrain agency action; frequency of delegations Congress later regrets; trajectory of agency independence over time. Normative: evaluate whether agencies'' policy choices align with Congressional intent or systematically deviate toward executive preferences.',
    'If primarily coordination: ε should be lower (~0.25-0.35), classification solidifies as Rope. If primarily extraction: ε should be higher (~0.50+), solidifies as Snare or pure extraction. Current 0.38 assumes mixed mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_extraction_vs_coordination, empirical, 'Whether delegation operates as coordination or extraction').

omega_variable(
    alternative_constitutional_framings,
    'Would a return to formalist separation (rigid categorical boundaries, strict nondelegation enforcement) be functionally feasible in a modern administrative state, or is functionalism the only viable constitutional reading?',
    'Comparative constitutional analysis: how do civil-law jurisdictions with stricter delegation limits govern complexity? Can U.S. governance operate under strict nondelegation revival without legislative gridlock or constitutional suspension?',
    'If formalism is viable alternative: functionalism is one reading among genuine options, and its institutional leverage is contingent. If formalism is structurally infeasible: functionalism approaches necessity, and the extraction it permits is minimized—the reading naturalizes inevitable power distribution rather than extracting novel authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_constitutional_framings, empirical, 'Feasibility of alternative (formalist) constitutional governance').

omega_variable(
    reading_identity_kernel_question,
    'This constraint is a reading of the separation-of-powers kernel. Is the kernel best understood as the constitutional text (''all legislative Power''), the structural principle (divided authority), the historical practice (delegation patterns), or the legitimacy claim (why separation exists)?',
    'Examination of how courts, scholars, and institutional actors invoke ''separation of powers''—which aspect anchors their claims? Do formalist and functionalist readings converge on a shared kernel but diverge on interpretation, or do they disagree about what the kernel IS?',
    'If readings converge on kernel: the constraint is properly a reading with multiple interpretations (current framing). If readings diverge on kernel: the constraint may not be a single kernel with multiple readings but two different claims (''the constraint is the text'' vs ''the constraint is the principle''). Affects classification architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_kernel_question, conceptual, 'What entity is the separation-of-powers kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sopfunc_tr_t0, separation_of_powers_text__functionalist_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sopfunc_tr_t25, separation_of_powers_text__functionalist_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement(sopfunc_tr_t50, separation_of_powers_text__functionalist_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(sopfunc_be_t0, separation_of_powers_text__functionalist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sopfunc_be_t25, separation_of_powers_text__functionalist_reading, base_extractiveness, 25, 0.34).
narrative_ontology:measurement(sopfunc_be_t50, separation_of_powers_text__functionalist_reading, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__unitary_executive_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, administrative_deference_chevron).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, nondelegation_doctrine_revival).

% DUAL FORMULATION NOTE:
% The separation-of-powers kernel admits three structurally distinct readings (functionalist, formalist, unitary-executive), each with its own ε, perspectives, and classification profile. This story captures the functionalist reading only. Do NOT merge the readings into one constraint with measurement variance or observable-dependent classification. The kernel is one entity; the readings are separate constraints linked via network.affects_constraints. The three readings coexist across institutional coalitions—courts apply functionalism while scholars defend formalism while executive agencies rely on unitary-executive reasoning. The perspectival gap (Rope vs. Snare vs. Mountain from the same text) reveals the kernel is contested, not that single-reading classification is indeterminate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(separation_of_powers_text__functionalist_reading, institutional, 0.25).
constraint_indexing:directionality_override(separation_of_powers_text__functionalist_reading, analytical, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
