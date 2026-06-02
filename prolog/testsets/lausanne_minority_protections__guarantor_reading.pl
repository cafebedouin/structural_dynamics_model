% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__guarantor_reading, []).

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
 *   constraint_id: lausanne_minority_protections__guarantor_reading
 *   human_readable: Lausanne Minority Protections: Guarantor Reading (International Supervision Pathway)
 *   domain: international_law/minority_rights/religious_governance
 *
 * SUMMARY:
 *   The Treaty of Lausanne (1923) creates a framework for minority
 *   protections in successor states of the Ottoman Empire. This constraint
 *   story instantiates the GUARANTOR READING: minority protections are
 *   understood as international obligations supervised by guarantor states
 *   (France, Italy, Greece, and others) through diplomacy and European human
 *   rights mechanisms, not merely as domestic legal commitments subject to
 *   each state's own interpretation. The guarantor reading embeds both
 *   coordination (creating an external appeal pathway for minorities) and
 *   conditional extraction (guarantor states extract legitimacy and
 *   diplomatic leverage from their supervision role, and minorities' actual
 *   protection depends on guarantor-state willingness to intervene). The
 *   constraint functions as a temporary scaffold: as domestic legal systems
 *   mature and internalize minority protections through European integration
 *   and constitutional development, the need for external guarantor
 *   supervision decays. The theater ratio (0.58) reflects that much of the
 *   enforcement apparatus is performative — League of Nations Councils and
 *   diplomatic notes operate largely through legitimacy claims rather than
 *   binding enforcement mechanisms. The guarantor reading coexists with two
 *   sibling readings (restrictive reading: minorities have only domestic
 *   legal recourse; expansive reading: minorities have direct supranational
 *   adjudication authority) which instantiate different structural
 *   commitments from the same formalized Treaty kernel.
 *
 * KEY AGENTS:
 *   - Guarantor States (France, Italy, Greece, Romania, Yugoslavia): Institutional beneficiaries (arbitrage exit) — extract diplomatic leverage and legitimacy from supervision role; benefit from framework for managing minority issues without direct occupation
 *   - Minority Communities (Christian minorities in Turkey, Muslim minorities in Greece, Jewish minorities across successor states): Primary beneficiaries of coordination function but trapped agents — have external appeal pathway but depend on guarantor state willingness to intervene
 *   - Treaty-Bound Successor States (Turkey, Greece, Romania, Yugoslavia): Institutional targets (constrained exit) — face international supervision constraints; exit is costly (diplomatic isolation); domestic sovereignty over minority policy is limited
 *   - European Court of Human Rights / League of Nations Council: Institutional adjudicators (institutional/arbitrage) — receive minority petitions and guarantor state complaints; enforce or interpret obligations; their authority is contested (binding vs. guidance only)
 *   - Analytical Observer: Sees both genuine coordination (external remedy pathway) and structural extraction (guarantor-state veto over minority remedy); notes that guarantor reading naturalizes guarantor states' advantage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__guarantor_reading, 0.28).
domain_priors:suppression_score(lausanne_minority_protections__guarantor_reading, 0.42).
domain_priors:theater_ratio(lausanne_minority_protections__guarantor_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__guarantor_reading, scaffold).
narrative_ontology:human_readable(lausanne_minority_protections__guarantor_reading, "Lausanne Minority Protections: Guarantor Reading (International Supervision Pathway)").
narrative_ontology:topic_domain(lausanne_minority_protections__guarantor_reading, "international_law/minority_rights/religious_governance").

domain_priors:requires_active_enforcement(lausanne_minority_protections__guarantor_reading).
narrative_ontology:has_sunset_clause(lausanne_minority_protections__guarantor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__guarantor_reading, '3a75ce50-41f4-493b-9e84-fb0c6772fbf8').
narrative_ontology:cs_kernel_codification('3a75ce50-41f4-493b-9e84-fb0c6772fbf8', formalized).
narrative_ontology:cs_authority_grounding('3a75ce50-41f4-493b-9e84-fb0c6772fbf8', lineage).
narrative_ontology:cs_interpretation_layer_present('3a75ce50-41f4-493b-9e84-fb0c6772fbf8').
narrative_ontology:cs_reading_relation('3a75ce50-41f4-493b-9e84-fb0c6772fbf8', lausanne_minority_protections__restrictive_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a75ce50-41f4-493b-9e84-fb0c6772fbf8', lausanne_minority_protections__expansive_reading, influences).
narrative_ontology:cs_axiom('3a75ce50-41f4-493b-9e84-fb0c6772fbf8', foundational, guarantor_state_mediated_adjudication).
narrative_ontology:cs_axiom_status(guarantor_state_mediated_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('3a75ce50-41f4-493b-9e84-fb0c6772fbf8', guarantor_state_mediated_adjudication, conventional).
narrative_ontology:cs_axiom('3a75ce50-41f4-493b-9e84-fb0c6772fbf8', foundational, international_supervision_temporary).
narrative_ontology:cs_axiom_status(international_supervision_temporary, holdable).
narrative_ontology:cs_axiom_grounding('3a75ce50-41f4-493b-9e84-fb0c6772fbf8', international_supervision_temporary, instrumental).
narrative_ontology:cs_reference_frame('3a75ce50-41f4-493b-9e84-fb0c6772fbf8', league_of_nations_guarantor_supervision).
narrative_ontology:cs_drift_state('3a75ce50-41f4-493b-9e84-fb0c6772fbf8', contemporary_european_integration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3a75ce50-41f4-493b-9e84-fb0c6772fbf8', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, guarantor_states).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, international_adjudication_bodies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY COMMUNITIES (TANGLED ROPE) — Access to guarantor-state diplomacy and European human rights mechanisms provides coordination benefit (external appeal pathway), but requires navigating bureaucratic processes and depends on guarantor state willingness to intervene. Trapped within national jurisdiction but have external leverage mechanism. Mixed: protection mechanism exists but is not guaranteed; diplomacy is coordination but requires state actors who may not be incentivized to use it.
constraint_indexing:constraint_classification(lausanne_minority_protections__guarantor_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERNATIONAL ADJUDICATION BODIES (SCAFFOLD) — European Court of Human Rights, League of Nations Permanent Court of International Justice, and guarantor-state diplomatic mechanisms create a temporary coordination layer above domestic law. Low effective extraction because international bodies have limited enforcement machinery and operate with sunset logic: as domestic legal systems mature and internalize minority protections, the need for external oversight decays. Constraint has explicit temporal horizon (Treaty of Lausanne as transitional framework, not permanent).
constraint_indexing:constraint_classification(lausanne_minority_protections__guarantor_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: GUARANTOR STATES (ROPE) — France, Italy, Greece, Romania, and Yugoslavia (original signatories) benefit from coordination function: the Treaty creates a framework for managing minority issues bilaterally and multilaterally without requiring direct military intervention. Guarantor states can exit through formal amendment or diplomatic recognition of changed circumstances. Net beneficiary — the constraint provides diplomatic leverage and legitimacy for intervention in minority protection without requiring permanent occupation or military commitment.
constraint_indexing:constraint_classification(lausanne_minority_protections__guarantor_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SUCCESSOR STATES (SNARE) — Turkey and other Treaty-bound states face significant constraint: international supervision limits their domestic sovereignty over minority policy, yet exit from the constraint is costly (diplomatic isolation, sanctions, international court judgments). Moderate power with constrained exit options. The suppression mechanism is institutional (treaty obligation + international enforcement threat) rather than physical coercion.
constraint_indexing:constraint_classification(lausanne_minority_protections__guarantor_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TREATY ENFORCEMENT MECHANISM (PITON) — The guarantor-reading enforcement apparatus (diplomatic notes, League Councils, later European Court proceedings) is substantially performative. Guarantor states rarely invoke the mechanism; League of Nations enforcement collapsed; actual constraint force comes from reputational pressure and soft power, not binding adjudication. Theater ratio (0.58) reflects that much of the enforcement function is maintained through ritual and diplomatic performance rather than through institutional capacity. Exists due to historical commitment and legitimacy claims, not because enforcement is effective.
constraint_indexing:constraint_classification(lausanne_minority_protections__guarantor_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the guarantor reading embeds both genuine coordination (external appeal pathway for minorities) and significant extraction (guarantor states extract legitimacy and diplomatic leverage from the commitment to supervise). The analytical observer sees that this reading naturalizes the guarantor states' structural advantage: framing international supervision as 'protection' obscures that supervision requires guarantor-state willingness to intervene, giving guarantor states veto power over minority remedy pathways.
constraint_indexing:constraint_classification(lausanne_minority_protections__guarantor_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__guarantor_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lausanne_minority_protections__guarantor_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lausanne_minority_protections__guarantor_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(lausanne_minority_protections__guarantor_reading, TR),
    TR >= 0.70.

:- end_tests(lausanne_minority_protections__guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The guarantor reading creates a coordination function (external appeal pathway for minorities without requiring domestic court reform), which reduces raw extractiveness below what a pure supervisory mechanism would exhibit. However, the condition that guarantor states must willingly invoke supervision rights introduces extraction: minorities' actual protection is contingent on guarantor-state strategic interests, not on their legal entitlements. The trajectory (0.18 → 0.28 over 100 years) reflects slow accumulation of European integration and domestic constitutional development that gradually reduces reliance on guarantor intervention. Suppression (0.42): Moderate. Treaty-bound states face institutional constraints (treaty obligation, international court oversight, reputational pressure) but not physical coercion. Successor states can negotiate, amend, or resist enforcement. However, the suppression mechanism is real: exit from the constraint is diplomatically expensive. Theater ratio (0.58): Moderate-high. The guarantor reading enforcement apparatus relies heavily on diplomatic ritual: League Councils deliberate but lack enforcement machinery; European Court judgments can be delayed or resisted; guarantor states invoke supervision rights selectively. Much of the constraint's apparent force comes from legitimacy claims and reputational pressure rather than binding enforcement. The increasing theater ratio (0.35 → 0.58) reflects that as domestic legal systems mature, the guarantee mechanism becomes increasingly performative — it persists through institutional commitment rather than through active enforcement function.
 *
 * PERSPECTIVAL GAP:
 *   The guarantor reading produces maximum perspectival divergence. Guarantor states see rope — a coordination framework for managing minority issues diplomatically without military cost. International adjudicators see scaffold — a temporary supervision mechanism that decays as domestic systems mature. Minority communities see tangled rope — external leverage but conditional on guarantor willingness. Treaty-bound successor states see snare — international constraints on sovereignty with no guaranteed exit. The enforcement mechanism itself is piton — maintenance through institutional inertia and legitimacy claims rather than functioning enforcement capacity. The analytical observer sees the core structural asymmetry: the guarantor reading's 'protection' mechanism requires guarantor-state agency, giving guarantor states veto power over minority remedy pathways.
 *
 * DIRECTIONALITY LOGIC:
 *   Guarantor states (institutional/arbitrage) experience low or negative effective extraction: they benefit from the coordination framework and can exit through formal amendment. Their power atom (institutional) and exit modulation (arbitrage) produce derived d ≈ 0.15, yielding negative f(d). Minority communities (powerless/trapped) experience moderate extraction: they have external appeal pathways (reducing d somewhat) but cannot exit the framework. Their power atom (powerless) and exit modulation (trapped) produce derived d ≈ 0.90, yielding high f(d) ≈ 1.35. Treaty-bound successor states (institutional/constrained) experience moderate extraction: they are institutional actors but face real diplomatic costs for exit. Their derived d ≈ 0.65, yielding f(d) ≈ 1.00. The chi formula (χ = ε × f(d) × σ(S)) scales these by scope: continental scope (σ=1.1) amplifies extraction relative to local scope. The guarantor reading's structural beneficiary is the guarantor state itself (extraction runs toward them through diplomatic leverage and legitimacy); the structural victim is the treaty-bound state (extraction runs away from them through sovereignty constraints). Minority communities are technically beneficiaries of the coordination function but victims of the conditional nature of guarantor intervention.
 *
 * MANDATROPHY ANALYSIS:
 *   The guarantor reading resolves the mandatrophy by accepting that the Lausanne kernel is contested. No single type correctly describes 'the constraint' because the kernel itself admits multiple readings. The guarantor reading is a scaffold because it creates a temporary coordination layer (international supervision) with explicit sunset logic (as domestic legal systems mature, external guarantor supervision becomes unnecessary). This reading should NOT be forced into a permanent classification. The sibling readings would produce different types: restrictive reading would be snare (pure international constraint with no coordination function); expansive reading would be rope or tangled_rope (direct supranational authority with coordination function). The mandatrophy is resolved by recognizing that all three types are legitimate perspectival readings of different interpretations of the same kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    guarantor_intervention_willingness,
    'What determines whether a guarantor state will invoke its supervision rights on behalf of minorities?',
    'Historical case analysis of League of Nations and European Court proceedings: when did guarantor states invoke minority protection mechanisms vs. ignore violations? What strategic interests aligned with or opposed intervention?',
    'If intervention is principled: the guarantor reading creates reliable external remedy pathway (scaffold confirmed). If intervention is strategic: the constraint is dependent on guarantor state interests, making minority protection contingent and extractive (snare from minority perspective). Classification shifts from scaffold to snare if guarantor intervention proves interest-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(guarantor_intervention_willingness, empirical, 'Whether guarantor state intervention on minority protection is principled or strategic').

omega_variable(
    reading_kernel_distinction,
    'Does the Lausanne Treaty kernel commit to (a) guarantor-state supervised protection (this reading) or (b) restrictive domestic interpretation or (c) expansive supranational authority?',
    'Textual analysis of Treaty language; historical negotiation records; legal precedent in League and European court interpretation; subsequent amendments and treaty amendment proposals.',
    'This omega documents that the guarantor reading is one of three coherent interpretations of the same kernel. The sibling readings (restrictive and expansive) instantiate different structural commitments from the same formalized text. No single reading is ''correct'' — the kernel itself is contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Which of three coherent readings of the Lausanne kernel the Treaty commits to').

omega_variable(
    european_court_authority_hierarchy,
    'Does the European Court of Human Rights constitute binding adjudication on minority rights, or merely interpretive guidance that states can resist?',
    'Analysis of European Court judgments on minority protection; state compliance rates; cases where states have refused or delayed implementation; formal enforcement mechanisms available to the Court.',
    'If binding: the guarantor reading creates genuine external constraint (scaffold with real teeth). If guidance only: the constraint is primarily diplomatic leverage (scaffold with sunset logic confirmed). Classification remains scaffold but with different mechanisms of enforcement decay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(european_court_authority_hierarchy, empirical, 'Whether European Court authority over minorities is binding or interpretive').

omega_variable(
    guarantor_reading_distinguishability,
    'Is the guarantor reading structurally distinct from the expansive reading, or does it embed assumptions about supranational authority that collapse into the expansive reading?',
    'Comparative analysis of guarantor reading vs. expansive reading on: (a) locus of authority (state-mediated diplomacy vs. direct supranational court); (b) enforcement mechanism (guarantor intervention threat vs. binding supranational judgment); (c) minority agent role (petitioner to guarantor state vs. direct litigant in international court).',
    'If distinct: the three readings represent genuinely different structural commitments. If guarantor reading is intermediate stage toward expansive reading: the lifecycle logic suggests guarantor reading is temporary (scaffold sunset confirmed). This omega documents the reading-family structure and tests whether decomposition into three stories is analytically justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guarantor_reading_distinguishability, conceptual, 'Whether guarantor reading is structurally distinct from expansive reading or intermediate stage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__guarantor_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lausanne_guar_tr_t0, lausanne_minority_protections__guarantor_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lausanne_guar_tr_t10, lausanne_minority_protections__guarantor_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(lausanne_guar_tr_t20, lausanne_minority_protections__guarantor_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(lausanne_guar_be_t0, lausanne_minority_protections__guarantor_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(lausanne_guar_be_t10, lausanne_minority_protections__guarantor_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(lausanne_guar_be_t20, lausanne_minority_protections__guarantor_reading, base_extractiveness, 20, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__guarantor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, greek_turkish_minority_exchange).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, european_court_human_rights_authority).

% DUAL FORMULATION NOTE:
% The Lausanne minority protections constraint decomposes into three structurally distinct stories corresponding to three readings of the same Treaty kernel. The guarantor reading (ε=0.28, scaffold) features international supervision with sunset logic. The restrictive reading (ε ≈ 0.60, snare) features pure international constraint. The expansive reading (ε ≈ 0.35, tangled_rope or rope) features direct supranational authority. All three stories link to the same kernel. Each story has its own ε value reflecting different structural features of the interpreted Treaty. The three readings are not observers looking at one constraint; they are different instantiations of one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__guarantor_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
