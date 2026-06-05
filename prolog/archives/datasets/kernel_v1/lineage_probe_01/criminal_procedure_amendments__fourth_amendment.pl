% ============================================================================
% CONSTRAINT STORY: criminal_procedure_amendments__fourth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_criminal_procedure_amendments__fourth_amendment, []).

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
 *   constraint_id: criminal_procedure_amendments__fourth_amendment
 *   human_readable: Fourth Amendment: Warrant and Probable Cause Requirement
 *   domain: political/legal
 *
 * SUMMARY:
 *   The Fourth Amendment, ratified in 1791, restricts state power to search
 *   persons, houses, papers, and effects by requiring warrants grounded in
 *   probable cause and issued by neutral magistrates. This constraint
 *   instantiates ONE reading of a contested kernel: the criminal procedure
 *   amendments. The Fourth Amendment's core claim is that investigative power
 *   must be bounded by a warrant/probable cause framework before intrusion
 *   occurs, not after. This reading directly addresses the suppression of
 *   general warrants (which empowered arbitrary searches) and dragnet
 *   surveillance by conditioning state intrusion on a judicial gate. The
 *   amendment's beneficiary is the surveilled and searched person; its victim
 *   is investigative efficiency (slower, narrower searches). The
 *   extractiveness is moderate (0.38) because the constraint both coordinates
 *   (legitimate searches are protected from challenge) and extracts
 *   (illegitimate searches are excluded). The constraint exhibits significant
 *   theater (0.58): warrant approval rates are extremely high (~95%),
 *   suggesting the magistrate serves a legitimating rather than filtering
 *   function. The suppression requirement (0.62) reflects that enforcement
 *   depends on post-facto litigation (suppression motions), not proactive
 *   prevention—a structural asymmetry that favors the state in the short run
 *   but provides a long-term corrective mechanism.
 *
 * KEY AGENTS:
 *   - Surveilled and Searched Persons: Primary beneficiary (powerless/trapped) — protected from general warrants and arbitrary searches by the warrant requirement's coordination function
 *   - Investigative State (Police, Federal Agents, Prosecutors): Primary victim (organized/constrained) — faces friction costs from warrant applications, suppression doctrines, and narrowed investigative scope
 *   - Magistrates and Judges: Authoritative interpreters (institutional/arbitrage) — issue warrants and adjudicate suppression motions; benefit from interpretive capacity to narrow warrant scope and expand exceptions
 *   - Criminal Defendants and Defense Counsel: Secondary beneficiary/victim (moderate/constrained) — nominally protected by Fourth Amendment but bear litigation burdens to enforce suppression; experience extraction through resource asymmetry
 *   - Executive and National Security Apparatus: Powerful institutional actor (powerful/mobile) — experiences extraction from warrant requirement but benefits from legitimacy conferred by compliance; deploys exceptions and technology drift to maintain investigative flexibility
 *   - The Warrant Apparatus: Institutional theater (institutional/arbitrage) — performs the neutral-magistracy function while degrading in filtering capacity; maintained through inertia despite doctrinal erosion
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the warrant requirement as an immutable natural law rather than recognizing it as a constructed institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(criminal_procedure_amendments__fourth_amendment, 0.38).
domain_priors:suppression_score(criminal_procedure_amendments__fourth_amendment, 0.62).
domain_priors:theater_ratio(criminal_procedure_amendments__fourth_amendment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(criminal_procedure_amendments__fourth_amendment, extractiveness, 0.38).
narrative_ontology:constraint_metric(criminal_procedure_amendments__fourth_amendment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(criminal_procedure_amendments__fourth_amendment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(criminal_procedure_amendments__fourth_amendment, tangled_rope).
narrative_ontology:human_readable(criminal_procedure_amendments__fourth_amendment, "Fourth Amendment: Warrant and Probable Cause Requirement").
narrative_ontology:topic_domain(criminal_procedure_amendments__fourth_amendment, "political/legal").

domain_priors:requires_active_enforcement(criminal_procedure_amendments__fourth_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(criminal_procedure_amendments__fourth_amendment, 'a309352c-7a29-499c-9b0d-e9a7e4e5464e').
narrative_ontology:cs_kernel_codification('a309352c-7a29-499c-9b0d-e9a7e4e5464e', fixed_text).
narrative_ontology:cs_authority_grounding('a309352c-7a29-499c-9b0d-e9a7e4e5464e', lineage).
narrative_ontology:cs_interpretation_layer_present('a309352c-7a29-499c-9b0d-e9a7e4e5464e').
narrative_ontology:cs_reading_relation('a309352c-7a29-499c-9b0d-e9a7e4e5464e', criminal_procedure_amendments__fifth_amendment, influences).
narrative_ontology:cs_reading_relation('a309352c-7a29-499c-9b0d-e9a7e4e5464e', criminal_procedure_amendments__sixth_amendment, influences).
narrative_ontology:cs_reading_relation('a309352c-7a29-499c-9b0d-e9a7e4e5464e', criminal_procedure_amendments__seventh_amendment, coexists_with).
narrative_ontology:cs_reading_relation('a309352c-7a29-499c-9b0d-e9a7e4e5464e', criminal_procedure_amendments__eighth_amendment, coexists_with).
narrative_ontology:cs_axiom('a309352c-7a29-499c-9b0d-e9a7e4e5464e', foundational, warrant_gate_required).
narrative_ontology:cs_axiom_status(warrant_gate_required, holdable).
narrative_ontology:cs_axiom_grounding('a309352c-7a29-499c-9b0d-e9a7e4e5464e', warrant_gate_required, deontological).
narrative_ontology:cs_axiom('a309352c-7a29-499c-9b0d-e9a7e4e5464e', secondary, suppression_remedy_vindicates_right).
narrative_ontology:cs_axiom_status(suppression_remedy_vindicates_right, overridden).
narrative_ontology:cs_axiom_grounding('a309352c-7a29-499c-9b0d-e9a7e4e5464e', suppression_remedy_vindicates_right, instrumental).
narrative_ontology:cs_reference_frame('a309352c-7a29-499c-9b0d-e9a7e4e5464e', warrant_gate_legitimacy).
narrative_ontology:cs_drift_state('a309352c-7a29-499c-9b0d-e9a7e4e5464e', contemporary_digital_age, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a309352c-7a29-499c-9b0d-e9a7e4e5464e', '').
narrative_ontology:cs_kernel_id(criminal_procedure_amendments__fourth_amendment, criminal_procedure_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(criminal_procedure_amendments__fourth_amendment, surveilled_and_searched_persons).
narrative_ontology:constraint_victim(criminal_procedure_amendments__fourth_amendment, investigative_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CITIZEN SUBJECT TO SEARCH (ROPE) — Trapped by jurisdiction (cannot exit national territory to avoid the amendment's regime), but perceives the constraint as genuine coordination: the warrant requirement establishes mutual expectations about state intrusion, creates a predictable boundary, and enables the citizen to understand when state power is being exercised lawfully vs. arbitrarily. The suppression of general warrants is the point—it coordinates investigative power against unlimited dragnet. No meaningful extraction from this perspective; the constraint protects rather than extracts.
constraint_indexing:constraint_classification(criminal_procedure_amendments__fourth_amendment, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LAW ENFORCEMENT AND INVESTIGATIVE AGENCIES (TANGLED ROPE) — Constrained by the warrant and probable cause requirement. They experience this as extraction (limits on scope and speed of investigation, narrowing of evidence admissibility, costs of warrant application). But they also benefit from the coordination function: the amendment legitimizes searches conducted within its bounds, prevents defense challenges to properly-warranted searches, and creates a stable epistemic authority (the issuing magistrate) that both law enforcement and courts rely on. The requirement is simultaneously a limitation and a source of investigative legitimacy. Active enforcement (magistrates issuing warrants, suppression motions in courts) is required to maintain the boundary.
constraint_indexing:constraint_classification(criminal_procedure_amendments__fourth_amendment, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEFENDANTS AND CRIMINAL DEFENDANTS' COUNSEL (SNARE) — Constrained by resource requirements (litigation costs, expert testimony to challenge searches) and institutional barriers (presumption of legality for magistrate-issued warrants, high bar for suppression motions). While the Fourth Amendment is nominally protective, the actual extraction operates through the suppression framework: defendants must affirmatively litigate violations in suppression motions, bearing the burden of proof on factual disputes about consent or exigent circumstances. The constraint's theater (the warrant/probable cause apparatus) creates an appearance of constraint but distributes evidentiary burdens in ways that favor the state. Extraction runs from defendants to investigative state.
constraint_indexing:constraint_classification(criminal_procedure_amendments__fourth_amendment, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE JUDICIARY (TANGLED ROPE) — Institutional beneficiary with arbitrage options (can deploy doctrinal innovation to reframe what counts as 'reasonable,' adjust suppression remedies, or limit the scope of warrant requirements). The judiciary benefits from the authoritative role the amendment grants: magistrates issue warrants, appellate courts interpret reasonableness. But they also face suppression doctrines that require exclusion of tainted evidence, limiting judicial flexibility to admit all evidence and maximize conviction rates. The constraint coordinates the adjudication process (warrants, suppression, appellate review) while extracting by limiting admissibility. The judiciary's arbitrage capacity shows in doctrinal narrowing (see third-party doctrine, curtilage doctrine, qualified immunity) that preserves the appearance of the warrant requirement while expanding its exceptions.
constraint_indexing:constraint_classification(criminal_procedure_amendments__fourth_amendment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EXECUTIVE AND NATIONAL SECURITY APPARATUS (TANGLED_ROPE) — Powerful actors with mobile exit options (can lobby for statutory exceptions, claim national security carveouts, deploy technologies that fall outside warrant scope, or shift investigative targets). The Fourth Amendment constrains investigative scope and creates friction costs. But the executive also benefits from the legitimacy the warrant requirement confers: properly-authorized searches receive judicial and legislative support, enabling broader investigative powers than would exist under explicit authorization schemes. The FISA framework demonstrates this hybrid: the executive accepts warrant requirements (coordination function) in exchange for broad authority within the surveillance regime (extraction benefit). National security exemptions and technological drift create de facto extraction space.
constraint_indexing:constraint_classification(criminal_procedure_amendments__fourth_amendment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: THE WARRANT APPARATUS (PITON) — The warrant and probable cause machinery is substantially performative. Magistrates issue warrants in routinized fashion at extremely high rates (~95% approval rates historically); the theater of neutral magistracy persists despite the low evidentiary bar in practice. Searches conducted with defective warrants are often admitted under harmless-error doctrine; the procedural requirement maintains institutional legitimacy while the doctrinal exceptions preserve evidence flow. The constraint has degraded through case-law attrition but persists through inertia: the warrant ritual remains because the alternative (explicit statutory authorization) would be politically explosive. Theater ratio reflects the gap between the warrant requirement's legitimating function and its actual filtering capacity.
constraint_indexing:constraint_classification(criminal_procedure_amendments__fourth_amendment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some boundary between state intrusion and private domain is inherent to any political order that purports to protect individual liberty. The Fourth Amendment might be read as codifying an immutable limit: the state cannot search arbitrarily; some standard of reasonableness or consent is a precondition of legitimate state action. This perspective sees the amendment as naturalizing a fundamental constraint on state power—not a contingent institutional choice but an irreducible feature of rights-respecting governance. However, the structural data contradicts pure mountain classification: the constraint has identifiable beneficiaries (persons protected from search) and victims (investigative efficiency), and the benefit is distributed asymmetrically. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(criminal_procedure_amendments__fourth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(criminal_procedure_amendments__fourth_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(criminal_procedure_amendments__fourth_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(criminal_procedure_amendments__fourth_amendment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(criminal_procedure_amendments__fourth_amendment, TR),
    TR >= 0.70.

:- end_tests(criminal_procedure_amendments__fourth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, increasing over time (0.15 → 0.38). The initial extractiveness was low because the warrant requirement operated as a genuine coordination mechanism in the 18th century—it suppressed general warrants that had been abused under the crown. Over 200+ years, extractiveness has risen as doctrinal exceptions (good-faith exception, third-party doctrine, curtilage limitations, digital surveillance carveouts) have accumulated, reducing the filtering capacity of the warrant requirement while maintaining its legitimizing theater. The trajectory reflects not a change in the constraint's formal rule but a drift in how that rule is applied—exceptions have eroded the scope. Suppression (0.62): Moderate-high but declining (0.75 → 0.62). Early enforcement relied on judicial exclusion of evidence as the sole remedy, creating high suppression pressure. Modern doctrine has diffused this through harmless-error analysis, good-faith exceptions, and standing doctrines that prevent many defendants from raising Fourth Amendment claims. The measured decline reflects that enforcement mechanisms have become less certain and less universal—suppression is now contingent on post-conviction appellate review, resource-dependent litigation, and doctrinal narrowing. Theater ratio (0.58): Moderate-high and increasing (0.35 → 0.58). The warrant apparatus is increasingly performative because: (1) magistrates approve ~95% of warrant applications, suggesting routinization rather than independent review, (2) technological surveillance often operates outside established doctrine, creating an enforcement gap, and (3) the suppression remedy requires defendant litigation rather than proactive state compliance. The theater has risen as the gap between the warrant requirement's legitimating function and its actual filtering capacity has widened.
 *
 * PERSPECTIVAL GAP:
 *   The Fourth Amendment exemplifies perspectival disagreement on a single institutional structure. The citizen subject to search sees rope (genuine protection from arbitrary intrusion). Law enforcement sees tangled rope (coordination benefit from legitimate searches balanced against extraction from warrant delays). The defendant sees snare (nominal protection but actual resource barriers and evidentiary burdens). The judiciary sees tangled rope with arbitrage capacity (can narrow scope while legitimizing state authority). The executive sees tangled rope with mobile options (can lobby for exceptions or deploy technological workarounds). The warrant apparatus itself is piton (performs legitimation function while filtering capacity degrades). The analytical observer risks mountain classification (treats the warrant requirement as an immutable feature of liberty rather than recognizing it as a contingent institutional choice with identifiable beneficiaries and victims). The perspectival gap is structural, not merely subjective: different actors experience the constraint differently because they occupy different structural positions relative to the extraction and coordination flows.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position: their power level, exit options, and relationship to the extraction/coordination flows. Citizens subject to search are trapped (cannot exit jurisdiction) and beneficiaries of the warrant requirement (protection from arbitrary search) — low d value → negative f(d) → negative chi (they experience the constraint as protecting, not extracting). Law enforcement is organized, constrained (must apply for warrants), and victim to investigative efficiency (narrowed scope) — higher d value → positive f(d) → positive chi (they experience extraction). Magistrates are institutional, arbitrage-enabled (can reinterpret doctrine) — moderate d value reflecting their dual role (limiting law enforcement while legitimizing state authority). Defendants are moderate power, constrained, and victims of evidence exclusion/litigation burden — high d value → high chi (they experience extraction despite nominal protection). The executive is powerful, mobile (can lobby for carveouts), and ambiguous beneficiary/victim — moderate d with arbitrage discount reflecting exit capacity. The warrant apparatus is institutional, arbitrage-enabled (can deploy exceptions) — low d reflecting benefit from legitimacy conferred.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy—it is a stable tangled rope across multiple perspectives, with perspectival variation reflecting genuine structural differences rather than incoherence. The mandatrophy question ('Is this coordination or extraction?') resolves to 'both': the warrant requirement coordinates investigative power (legitimate searches are protected) while extracting from inefficiency-optimizing actors (law enforcement bears warrant application costs). The fourth amendment precisely instantiates the tangled rope type: it has both a genuine coordination function (establishing expectations about when state intrusion is legitimate) and asymmetric extraction (from law enforcement / investigative efficiency). The piton classification (warrant apparatus itself) does not represent failure of type classification but a separate observation: the formal rule persists while its filtering function has eroded, maintained by inertia and institutional legitimacy rather than functional necessity. This is orthogonal to the tangled rope classification of the amendment's core constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    warrant_magistrate_independence,
    'Is the magistrate issuing the warrant a genuinely neutral authority, or a functional component of the investigative apparatus?',
    'Historical and empirical analysis of warrant approval rates, variance by magistrate, instances of warrant rejection, and correlation between warrant denial and appellate reversal vs. state success on Fourth Amendment appeals',
    'If magistrate is genuinely neutral: warrant requirement is a real coordination gate, and the rope classification dominates. If magistrate is procedurally integrated with police/prosecution: warrant requirement becomes performative theater, piton classification dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(warrant_magistrate_independence, empirical, 'Whether the magistrate functions as a neutral gate or as part of the investigative state').

omega_variable(
    suppression_remedy_adequacy,
    'Does the exclusionary rule remedy (suppression of evidence) actually deter Fourth Amendment violations, or is it an artifact of judicial doctrine?',
    'Comparison of violations detected before and after suppression remedy changes (e.g., good-faith exception carveouts); analysis of systemic violations that persist despite suppression availability; investigation of whether law enforcement adjusts conduct when suppression is threatened',
    'If adequate deterrent: suppression is the active enforcement mechanism, tangled_rope classification stable. If inadequate: suppression is theater, piton classification dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_remedy_adequacy, empirical, 'Whether exclusionary rule remedy deters Fourth Amendment violations').

omega_variable(
    technological_scope_divergence,
    'Do modern surveillance technologies (cell-site location, facial recognition, digital forensics) operate within or outside the warrant requirement''s historical scope?',
    'Doctrinal analysis of warrant requirements for novel technologies; empirical data on unwarranted surveillance conducted via technologies not yet subject to established doctrine; case law on reasonable expectation of privacy as applied to digital data',
    'If within scope: constraint adapts, tangled_rope stable. If technologies operate outside scope: constraint is degraded/inapplicable to modern surveillance, snare classification dominates (surveillance occurs without warrant barrier).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_scope_divergence, empirical, 'Whether technological surveillance operates within warrant requirement scope').

omega_variable(
    reasonableness_standard_stability,
    'Is the ''reasonableness'' standard (the core boundary of Fourth Amendment protection) stable across contexts, or has it been so narrowed by exceptions that it functions only in name?',
    'Doctrinal mapping of reasonableness standard across contexts (searches of premises vs. persons vs. digital data); analysis of exception categories and their scope creep over time; comparison of historical vs. contemporary jurisprudence on what counts as ''reasonable''',
    'If stable: constraint maintains its protective function, tangled_rope classification stable. If eroded: constraint becomes a naming convention masking arbitrary state authority, snare classification dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reasonableness_standard_stability, empirical, 'Stability and scope of the ''reasonableness'' standard').

omega_variable(
    natural_law_vs_constructed,
    'Is the Fourth Amendment''s warrant requirement a natural law constraint (immutable requirement of any legitimate state) or a constructed institutional arrangement that could be replaced with alternative authorization schemes?',
    'Comparative constitutional law: examination of how other liberal democracies authorize searches; historical analysis of pre-Amendment authorization systems; philosophical argument about what makes a state authority legitimate',
    'If natural law: mountain classification is defensible, constraint is immutable. If constructed: false summit detection fires, constraint reclassifies to tangled_rope or snare depending on actual extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed, conceptual, 'Whether warrant requirement is natural law or constructed institutional choice').

omega_variable(
    kernel_reading_contest,
    'How does the Fourth Amendment reading relate structurally to the other Bill of Rights amendments (Fifth, Sixth, Seventh, Eighth) that govern criminal procedure?',
    'Doctrinal and structural analysis of how Fourth Amendment (investigative limits) complements Fifth (accusatorial limits), Sixth (trial fairness), Seventh (jury preservation), and Eighth (punishment proportionality); identification of interactions, redundancies, and conflicts between readings',
    'If amendments are logically independent: each can be read separately as a distinct constraint. If amendments form a unified architecture: readings must coexist and influence each other''s scope and meaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship between Fourth Amendment reading and sibling readings in criminal procedure kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(criminal_procedure_amendments__fourth_amendment, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fourth_tr_t0, criminal_procedure_amendments__fourth_amendment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fourth_tr_t50, criminal_procedure_amendments__fourth_amendment, theater_ratio, 50, 0.48).
narrative_ontology:measurement(fourth_tr_t100, criminal_procedure_amendments__fourth_amendment, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(fourth_be_t0, criminal_procedure_amendments__fourth_amendment, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(fourth_be_t50, criminal_procedure_amendments__fourth_amendment, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(fourth_be_t100, criminal_procedure_amendments__fourth_amendment, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(fourth_su_t0, criminal_procedure_amendments__fourth_amendment, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(fourth_su_t50, criminal_procedure_amendments__fourth_amendment, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(fourth_su_t100, criminal_procedure_amendments__fourth_amendment, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(criminal_procedure_amendments__fourth_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(criminal_procedure_amendments__fourth_amendment, fifth_amendment).
narrative_ontology:affects_constraint(criminal_procedure_amendments__fourth_amendment, sixth_amendment).
narrative_ontology:affects_constraint(criminal_procedure_amendments__fourth_amendment, eighth_amendment).

% DUAL FORMULATION NOTE:
% The Fourth Amendment is one constraint in a cluster of criminal procedure amendments. Its extractiveness (0.38) and theater ratio (0.58) reflect the erosion of warrant filtering through doctrinal exceptions. The constraint is upstream of Fifth Amendment accusatorial limits (which depend on Fourth Amendment search limits) and Sixth Amendment trial fairness (which depends on evidence reliability shaped by Fourth Amendment exclusion). Network decomposition is necessary because each amendment has distinct ε values and beneficiary/victim structures: Fourth (investigative limits), Fifth (accusatorial limits), Sixth (trial fairness), Eighth (punishment proportionality) address different phases of criminal process with different extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
