% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Hybrid Statehood: Montevideo Criteria Plus Normative Legitimacy
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The Montevideo Convention on the Rights and Duties of States (1933)
 *   specifies four objective criteria for statehood: defined territory,
 *   permanent population, effective government, and capacity to enter
 *   relations. The hybrid reading adds a normative gate: statehood also
 *   requires democratic governance, respect for human rights, and
 *   non-aggression. This reading is distinct from the declaratory reading
 *   (which holds objectivity alone is sufficient) and the constitutive
 *   reading (which makes recognition discretionary). The hybrid reading
 *   operationalizes liberal democratic norms as a global standard, making
 *   sovereignty conditional on governance style. It benefits established
 *   liberal democracies and enables humanitarian intervention doctrine. It
 *   extracts from non-liberal secessionists, authoritarian breakaway
 *   movements, and indigenous nations with non-democratic governance
 *   structures by making their self-determination claims subject to external
 *   normative judgment.
 *
 * KEY AGENTS:
 *   - established_liberal_democracies: set recognition policy via UN, deploy normative gate to deny or condition statehood
 *   - human_rights_advocacy_networks: gain legitimacy and enforcement authority through embedding norms in recognition gate
 *   - non_liberal_secessionists: meet objective criteria but denied on normative grounds; primary target
 *   - authoritarian_breakaway_states: structurally trapped between objective and normative gates; exposed to regime-change pressure
 *   - indigenous_nations_without_democratic_credentials: identity-locked targets; cannot adopt liberal form without violating the identity that justifies their claim
 *   - humanitarian_intervention_advocates: benefit from the reading's transformation of norms into hard preconditions for sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.71).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Hybrid Statehood: Montevideo Criteria Plus Normative Legitimacy").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, 'c3cd311b-e51a-48dd-9bfb-15531bbdecb0').
narrative_ontology:cs_kernel_codification('c3cd311b-e51a-48dd-9bfb-15531bbdecb0', formalized).
narrative_ontology:cs_authority_grounding('c3cd311b-e51a-48dd-9bfb-15531bbdecb0', extraction).
narrative_ontology:cs_interpretation_layer_present('c3cd311b-e51a-48dd-9bfb-15531bbdecb0').
narrative_ontology:cs_reading_relation('c3cd311b-e51a-48dd-9bfb-15531bbdecb0', montevideo_statehood_criteria__declaratory_reading, influences).
narrative_ontology:cs_reading_relation('c3cd311b-e51a-48dd-9bfb-15531bbdecb0', montevideo_statehood_criteria__constitutive_reading, forecloses).
narrative_ontology:cs_axiom('c3cd311b-e51a-48dd-9bfb-15531bbdecb0', foundational, liberal_democracy_as_recognition_precondition).
narrative_ontology:cs_axiom_status(liberal_democracy_as_recognition_precondition, holdable).
narrative_ontology:cs_axiom_grounding('c3cd311b-e51a-48dd-9bfb-15531bbdecb0', liberal_democracy_as_recognition_precondition, deontological).
narrative_ontology:cs_axiom('c3cd311b-e51a-48dd-9bfb-15531bbdecb0', foundational, human_rights_compliance_as_sovereignty_gate).
narrative_ontology:cs_axiom_status(human_rights_compliance_as_sovereignty_gate, holdable).
narrative_ontology:cs_axiom_grounding('c3cd311b-e51a-48dd-9bfb-15531bbdecb0', human_rights_compliance_as_sovereignty_gate, deontological).
narrative_ontology:cs_axiom('c3cd311b-e51a-48dd-9bfb-15531bbdecb0', secondary, intervention_as_recognition_standard_enforcement).
narrative_ontology:cs_axiom_status(intervention_as_recognition_standard_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('c3cd311b-e51a-48dd-9bfb-15531bbdecb0', intervention_as_recognition_standard_enforcement, instrumental).
narrative_ontology:cs_reference_frame('c3cd311b-e51a-48dd-9bfb-15531bbdecb0', montevideo_objective_criteria_with_liberal_normative_floor).
narrative_ontology:cs_drift_state('c3cd311b-e51a-48dd-9bfb-15531bbdecb0', contemporary_post_hegemonic_order, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c3cd311b-e51a-48dd-9bfb-15531bbdecb0', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, established_liberal_democracies).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, human_rights_advocacy_networks).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionists).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, authoritarian_breakaway_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, indigenous_nations_without_democratic_credentials).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, existing_unrecognized_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, small_liberal_democracies).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_advocates).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, existing_unrecognized_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the UN Security Council, General Assembly voting blocs, and major multilateral institutions through which recognition is operationalized. Set the normative criteria (democratic governance, human rights compliance, non-aggression). Can deny recognition, threaten intervention, impose sanctions, or demand regime change. Use the hybrid reading to justify selective intervention in non-liberal states while maintaining that intervention is enforcement of universal principle, not geopolitical dominance.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, established_liberal_democracies, agenda_setter,
    institutional, generational, analytical, global).

% Operate globally under mandate that human rights are universal and non-negotiable. The hybrid reading embeds their advocacy into the statehood gate itself—making human rights compliance a recognition precondition, not a post-recognition aspiration. Their campaigns for gender rights, LGBTQ+ recognition, anti-torture standards, and media freedom gain enforcement authority when embedded in recognition criteria. Can leverage states' desire for recognition to pressure governments toward their norm agenda.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, human_rights_advocacy_networks, beneficiary,
    organized, generational, mobile, global).

% Movements seeking independence and statehood on grounds of ethnic, religious, or cultural self-determination. Meet the Montevideo objective criteria: control defined territory, have permanent population, maintain effective government (police, courts, administration), conduct relations with neighboring entities. Denied recognition or face conditional recognition tied to adopting liberal democratic institutions they do not possess or choose. Cannot exit by accepting the gate without betraying their political movement's core identity. Examples: Catalonia seeking independence under its own governance system; Kashmir independence movements; various ethnic autonomy movements in post-Soviet space.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionists, payer,
    moderate, biographical, trapped, regional).

% De facto states that have gained territorial control and functional government but lack recognition because their governance is explicitly authoritarian (one-party rule, strongman dictatorship, military control). Examples: Northern Cyprus, Transnistria, South Ossetia, Abkhazia. Meet objective Montevideo criteria but face recognition denial conditioned on democratization. Can attempt exit by reforming governance, but governance changes risk losing the cohesion that enabled territorial control. Trapped between military-backed stability (which denies recognition) and democratic reform (which might lose the movement's control).
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, authoritarian_breakaway_states, payer,
    moderate, biographical, constrained, regional).

% Indigenous or First Nations populations with centuries of prior sovereignty and collective identity, governing through traditional councils, consensus mechanisms, or hereditary structures rather than liberal-democratic institutions. Meet objective Montevideo criteria: occupy defined territory (ancestral lands), have permanent population (community members), maintain effective governance (traditional authority, dispute resolution, resource management), conduct relations (treaties, alliances). Denied statehood under the hybrid reading on grounds their governance is not democratic. Cannot adopt liberal democracy without dissolving the traditional authority structures that ground their sovereignty claims and identity cohesion. Maximally trapped: objective gate recognizes them; normative gate forecloses them.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, indigenous_nations_without_democratic_credentials, payer,
    powerless, generational, identity_locked, local).

% States like Russia, China, Iran: vast territories, large populations, complex governance, effective armies and administrations. Meet all objective Montevideo criteria. Suffer recognition contestation or denial on normative grounds (authoritarianism, human rights violations, aggressive foreign policy). Simultaneously sit as veto powers or major players in the UN and recognition system—beneficiary seats where they can weaponize the hybrid reading against rivals (threat of humanitarian intervention against rivals' governance failures while their own violations are overlooked). Constrained exit: cannot leave the international system; can only navigate selective enforcement of the normative gate.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, existing_unrecognized_states, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, existing_unrecognized_states, beneficiary).

% African Union, Arab League, ASEAN, OAS operate as secondary recognition adjudicators and enforcers of the hybrid reading within their regions. Caught between dual mandates: African Union Charter emphasizes sovereignty as inviolable; AU African Charter on Democracy emphasizes democratic governance as legitimacy condition. Selectively apply the hybrid reading based on geopolitical alignment with member states. Act as both enforcers and resistance points to the normative gate.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, regional_organizations, agenda_setter,
    institutional, generational, analytical, regional).

% Countries like the Baltics, Kosovo, East Timor that gained statehood after 2000 precisely because their democratic credentials provided normative legitimacy in post-Cold War recognition politics. Benefit from the hybrid reading as a shield: their democratic governance makes their territorial claims defensible against revisionist powers. Use the reading to prevent stronger neighbors from contesting their sovereignty on realpolitik grounds. Would lose this protection if the reading reverted to pure Montevideo objectivity.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, small_liberal_democracies, beneficiary,
    moderate, generational, mobile, national).

% International lawyers, NGOs, and state actors who champion Responsibility to Protect (R2P) and humanitarian intervention doctrine. The hybrid reading provides the legal and normative foundation for intervention: if a state fails the democratic governance or human rights conditions embedded in statehood criteria, intervention becomes enforcement of recognition preconditions, not violation of sovereignty. Benefit from the reading's transformation of intervention from exception to principle-backed practice.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_advocates, beneficiary,
    organized, generational, mobile, global).

% International law scholars, decolonial theorists, and states (notably many in the Global South and non-aligned movement) who argue statehood must be based solely on objective Montevideo criteria without normative conditions. They hold that normative conditionality is imperialism repackaged and violates self-determination. Excluded from operationalizing the recognition system; their reading remains in scholarly discourse but is not institutionalized in UN practice or major states' recognition policies. Cannot exit the debate without abandoning their theoretical position.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, declaratory_reading_advocates, excluded,
    organized, generational, trapped, global).

% International lawyers and diplomats holding that statehood is ultimately a matter of discretionary recognition by the existing state community, with no objective or normative preconditions. The hybrid reading marginalizes them by insisting recognition cannot be arbitrary but must conform to specified standards. Their framework is displaced in operationalized practice; they remain academically present but institutionally sidelined.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, constitutive_reading_advocates, excluded,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__hybrid_reading, established_liberal_democracies).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of when to admit new political units to the international system: provides a standard (objective criteria + normative legitimacy) that replaces purely discretionary recognition and ad hoc admission. Enables an international legal order with predictable membership criteria and reduces conflicts over who gets to speak in international forums.
% TRANSFER_FUNCTION: Moves sovereignty from breakaway populations and non-liberal movements to established liberal states and human rights-legitimized governance structures. Transfers the power to define what counts as legitimate statehood from territorial control and population consent alone to liberal democratic states and their normative standards. Transfers the authority to intervene (humanitarian intervention, regime change, sanctions) from isolated state action to collective enforcement of the recognition gate.
% ABSENT_VOICES: Non-liberal secessionists, authoritarian breakaway movements, indigenous nations with non-democratic governance structures, and scholars of international law who reject normative conditionality as imperialism are structurally excluded from the recognition apparatus itself. They would object that the hybrid reading violates self-determination, repackages Western imperialism as universal principle, and denies statehood on grounds orthogonal to Montevideo. Their exclusion is not accidental—it is the mechanism by which the constraint operates.
% DISAPPEARANCE_RATIONALE: If the hybrid reading and its normative gate disappeared, statehood would revert to pure Montevideo objectivity: territorial control, permanent population, effective government, capacity to conduct relations. Dozens of currently denied or delayed recognitions would materialize overnight (Palestinian state, various indigenous nations, non-liberal breakaway movements). The authority to intervene on humanitarian or regime-change grounds would lose its recognition-gate justification and revert to explicit violation of sovereignty. Humanitarian intervention doctrine would weaken. The liberal democratic state monopoly on recognition legitimacy would dissolve.
% FOUNDING_PROBLEM: The pure Montevideo criteria produced ambiguity: multiple territorial units met the objective gates but possessed unclear legitimacy status. The international system needed a principle to distinguish between breakaway movements that should be admitted and those that should not—beyond mere power politics. The hybrid reading was constructed to fill that gap by embedding liberal democratic norms and human rights standards as preconditions, making legitimacy objective in principle while wielding it as a tool for enforcing liberal governance globally.
% FOUNDING_PROBLEM_CORROBORATION: Established liberal democracies and human rights advocacy networks affirm the founding problem remains live: non-liberal, undemocratic breakaway movements still threaten stability and require a normative gate. Non-liberal states, decolonial scholars, international lawyers favoring declaratory readings, and indigenous rights advocates attest the founding problem was fabricated: the Montevideo criteria were sufficient; the normative gate was added post-hoc to justify selective recognition and intervention. Historical analysis from outside the liberal democratic consensus (works by scholars in the Global South, critiques from China, Russia, India) documents the hybrid reading as an innovation of late-20th-century liberal hegemony, not a timeless principle.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at 2025) and rising over the interval because the hybrid reading steadily narrows the path to recognition by adding normative conditions to objective ones. The constraint extracts authority from territorial control and popular consent, redirecting it to liberal democratic states and their normative definitions. Suppression is higher still (0.71) because the constraint's persistence depends on actively enforcing the normative gate—preventing recognition of movements that fail it, threatening intervention against those that gain territory without liberal credentials, and revising the normative standard to align with shifting liberal priorities (gender rights, LGBTQ+ recognition, anti-corruption, media freedom). Theater is moderate-high (0.44): the objective Montevideo criteria are real and function legitimately, but an increasing share of recognition decisions turn on normative grounds rather than objective territorial control or effective government. The measurement series traces the 80-year arc: pre-1945 the Montevideo criteria existed but without the normative gate; post-1945 liberal democracies gradually embedded democratic, human rights, and non-aggression conditions into recognition practice; post-1990 (Cold War end) the normative gate accelerated, linking recognition explicitly to governance form; post-2005 humanitarian intervention doctrine and R2P crystallized the link between normative compliance and sovereignty itself.
 *
 * PERSPECTIVAL GAP:
 *   From the established liberal democracies' and human rights networks' perspective, the hybrid reading is genuine coordination that imposes a shared standard and prevents norm-violating states from gaining sovereignty. From the non-liberal secessionists' and indigenous nations' perspective, it is naked extraction: the objective gate is denied by adding unmeetable conditions, and sovereignty is made contingent on adopting a governance form foreign to their political tradition. The engine should compute this divergence: liberal democratic seats see coordination + mild legitimate enforcement; non-liberal target seats see pure extraction + coercive normative imposition. Power asymmetry amplifies the divergence: liberal democracies can credibly threaten intervention and sanctions; non-liberal movements cannot.
 *
 * DIRECTIONALITY LOGIC:
 *   Established liberal democracies: agenda-setter role, institutional power, analytical exit (they write the rules and need not exit), directionality near 0.0 (full beneficiary). Human rights networks: beneficiary role, organized power, mobile exit (their norms are elevated to global standard), directionality near 0.15 (slight net benefit). Non-liberal secessionists: payer role, moderate power, trapped exit (meet objective criteria but denied on normative grounds they cannot satisfy without violating identity), directionality near 0.85 (full target). Indigenous nations: payer role, powerless, identity_locked (self-determination claims are structural to their existence; cannot exit by adopting liberal form), directionality near 0.95 (maximum target). Established unrecognized states: dual role (beneficiary as veto-wielding powers in UN; payer as targets of hybrid reading's normative conditions), medium power, constrained exit, directionality near 0.50 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimacy ambiguity post-WWII; which breakaway movements warrant admission) was live in 1945–1965. By 2005, the founding problem had shifted: liberal democracies had successfully embedded their normative standards into recognition practice, and the problem was no longer 'how to distinguish legitimate from illegitimate' but 'how to enforce liberal governance globally.' The hybrid reading now persists not because the coordination problem requires it but because the extraction mechanism has become institutionalized: the normative gate generates rents for liberal states (authority to intervene, humanitarian intervention doctrine, regime-change legitimacy) and imposes costs on non-liberal movements. The theater_ratio has risen from 0.15 to 0.44, indicating an increasing share of recognition decisions turn on performances of democratic credentials rather than objective territorial control. Mandatrophy is not yet resolved but approaching: the constraint's original function (coordination on a shared standard) has atrophied; what remains is mostly enforcement of liberal hegemony. If theater_ratio exceeds 0.65 or base_extractiveness reaches 0.80, classification should shift from tangled_rope (genuine coordination + extraction) to snare (extraction with coordination cover).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_normative_gate,
    'Are the normative conditions (liberal democracy, human rights, non-aggression) objectively necessary preconditions for stable statehood, or are they contingent criteria constructed by liberal hegemony and imposed as a gate to protect liberal dominance?',
    'Counterfactual analysis: would international stability collapse if the normative gate were removed and recognition reverted to pure Montevideo objectivity? Comparative case studies of non-liberal breakaway movements'' stability records post-recognition.',
    'If natural law: the constraint is genuine coordination with extraction as an accidental byproduct. If constructed: the constraint is pure extraction with coordination as cover. Classification would shift from tangled_rope to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_normative_gate, conceptual, 'Whether the normative gate is objectively necessary or hegemonically imposed.').

omega_variable(
    liberal_democracy_definition_volatility,
    'What counts as satisfying the liberal democracy norm? The definition has shifted multiple times (gender rights, LGBTQ+ recognition, anti-corruption standards, media freedom, judicial independence). Is the shifting definition evidence of epistemic refinement or of power re-coding to exclude new target groups?',
    'Genealogical analysis of recognition decisions: document how the normative criteria changed over time and correlate changes with geopolitical interests of powerful recognizing states. Analyze whether non-liberal states have been held to different standards than liberal allies.',
    'If refinement: extraction is incidental to legitimate norm-setting. If power-coding: extraction is systematic and accelerating. Would amplify the snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liberal_democracy_definition_volatility, empirical, 'Whether normative criteria reflect principle or power re-coding.').

omega_variable(
    indigenous_sovereignty_incommensurability,
    'Can indigenous nations satisfy the democratic governance criterion while maintaining their governing traditions (council-based, consensus-driven, non-liberal structures)? Or does the hybrid reading structurally foreclose indigenous sovereignty by requiring adoption of a foreign governance form?',
    'Legal precedent analysis and consultation with indigenous governance authorities: can a nation maintain traditional governance and still gain recognition under the hybrid reading? Document cases where indigenous nations were denied recognition on governance grounds.',
    'If closure is structural: the hybrid reading functions as a gate specifically designed to exclude indigenous nations, making it a snare for that constituency. Classification remains tangled_rope at system level but carries high extraction for indigenous target seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_sovereignty_incommensurability, empirical, 'Whether the democratic governance criterion forecloses indigenous sovereignty structurally.').

omega_variable(
    humanitarian_intervention_doctrine_capture,
    'Does the hybrid reading''s embedding of human rights norms as recognition preconditions provide genuine justification for humanitarian intervention, or does it provide legal cover for interventions driven by geopolitical interests unrelated to human rights protection?',
    'Comparative intervention analysis: document interventions justified on humanitarian/human rights grounds and assess whether the actual intervention outcomes improved human rights or served geopolitical aims of intervening powers. Assess non-intervention in cases of equal or worse human rights violations where interveners lacked geopolitical interest.',
    'If genuine justification: the constraint enables legitimate humanitarian action. If legal cover: the constraint functions as a machine for laundering geopolitical coercion as norm enforcement; classification shifts closer to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_intervention_doctrine_capture, empirical, 'Whether humanitarian intervention doctrine delivers legitimate protection or geopolitical coercion.').

omega_variable(
    kernel_reading_foreclosure_possibility,
    'Does the hybrid reading''s institutional dominance foreclose the declaratory reading''s possibility within international law, or do both readings remain live options held by different parties and traditions?',
    'Institutional analysis of non-Western state practice and decolonial legal scholarship: assess whether the declaratory reading is actively maintained as an alternative framework, or whether it has been displaced from operational practice by the hybrid reading''s embedding in UN recognition procedures.',
    'If foreclosed: only one reading functionally operates; the kernel is de facto fixed and the constraint becomes self-reinforcing. If coexisting: the kernel remains contested and the hybrid reading''s dominance is contingent. Affects classification stability and omega-to-terminal-state transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_possibility, empirical, 'Whether the declaratory reading survives as a live alternative or is foreclosed by the hybrid reading''s dominance.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the high suppression level (0.71) evidence of structural barriers (diplomatic isolation, threat of intervention, exclusion from international institutions) or of internalized acceptance of liberal norms as legitimate by non-liberal movements themselves?',
    'Interview and archival analysis: assess the subjective experience of recognition-denied movements. Do they resist the hybrid reading''s legitimacy frame or accept it as right while opposing its application to them?',
    'If structural: suppression would collapse if external pressure were removed (high post-exit suppression trajectory). If internalized: suppression persists after exit; the hybrid reading has achieved ideological hegemony, not just institutional dominance. Affects theater_ratio interpretation and long-term constraint stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is external coercion or internalized norm acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1945, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(mont_tr_t1965, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1990, 0.31).
narrative_ontology:measurement(mont_tr_t2005, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2005, 0.39).
narrative_ontology:measurement(mont_tr_t2015, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(mont_tr_t2025, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2025, 0.44).

% Extraction over time
narrative_ontology:measurement(mont_be_t1945, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1945, 0.25).
narrative_ontology:measurement(mont_be_t1965, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(mont_be_t2005, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement(mont_be_t2015, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(mont_be_t2025, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1945, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(mont_su_t1965, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(mont_su_t2005, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement(mont_su_t2015, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2015, 0.69).
narrative_ontology:measurement(mont_su_t2025, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__hybrid_reading, 0.18).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_doctrine).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, responsibility_to_protect_r2p).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_peace_hypothesis).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, decolonization_movement_legitimacy).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, indigenous_sovereignty_claims).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the montevideo_statehood_criteria kernel. The declaratory_reading (statehood requires objective criteria alone) and constitutive_reading (statehood requires discretionary recognition) are authored as separate constraints with their own ε values, stakeholders, and measurements. All three are linked via network.affects_constraints. The hybrid_reading coexists with the declaratory reading but influences its practical operationality; the hybrid_reading forecloses the constitutive reading's discretionary path by insisting recognition must conform to objective standards, even though those standards now include normative conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__hybrid_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
