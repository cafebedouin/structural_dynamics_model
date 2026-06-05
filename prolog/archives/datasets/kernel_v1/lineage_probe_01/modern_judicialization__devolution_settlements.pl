% ============================================================================
% CONSTRAINT STORY: modern_judicialization__devolution_settlements
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_modern_judicialization__devolution_settlements, []).

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
 *   constraint_id: modern_judicialization__devolution_settlements
 *   human_readable: Devolution Settlements: Entrenched-in-Practice Powers and Westminster Recall
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   Devolution in the UK (initiated by the Scotland Act 1998, Government of
 *   Wales Act 1998, and subsequent Northern Ireland settlements) created a
 *   multi-tiered legislative structure in which Scottish, Welsh, and Northern
 *   Irish assemblies gained substantial statutory authority over 'devolved
 *   matters' (health, education, transport, land reform). Westminster
 *   retained theoretical unilateral recall authority under the doctrine of
 *   parliamentary sovereignty, but in practice has constrained itself through
 *   the Sewel Convention (1998), which establishes that Westminster will not
 *   normally legislate on devolved matters without the consent of the
 *   relevant assembly. This reading instantiates the 'modern_judicialization'
 *   kernel by showing how the UK state's architecture was restructured
 *   through devolution settlements: the previous unitary state was rebuilt
 *   with entrenched-in-practice powers that Westminster legislatures and
 *   courts now recognize but theoretically disavow. The constraint exhibits
 *   tangled_rope structure: genuine coordination (devolved legislatures
 *   coordinate regional policy response) combined with asymmetric extraction
 *   (Westminster's theoretical authority creates persistent uncertainty about
 *   devolved permanence; the Sewel Convention suppresses Westminster's
 *   unilateral recall but creates a convention-based rather than
 *   legally-entrenched constraint). Theater has risen as the gap between
 *   Sewel Convention observance and parliamentary sovereignty doctrine has
 *   widened — the theater is the persistent invocation of parliamentary
 *   supremacy while actual practice observes devolved constraints.
 *
 * KEY AGENTS:
 *   - Devolved Nations' Domestic Agendas (Scottish Parliament, Welsh Senedd, Northern Ireland Assembly): Primary beneficiaries (organized/arbitrage) — control regional policy on health, education, land reform, etc. Benefit from constraint by exercising substantive legislative authority. Exit option is arbitrage: independence or constitutional renegotiation raises Westminster's cost of casual recall.
 *   - Westminster Parliament: Theoretically sovereign but constrained (institutional/constrained) — retains theoretical unilateral recall power but in practice is suppressed from exercising it by Sewel Convention and political cost of constitutional crisis. Experiences extraction through path-dependent constraint on parliamentary action.
 *   - Uniformity Claimant (UK-wide policy advocates): Victim group (moderate/trapped) — seeks coherent UK-wide taxation, regulation, welfare standards but faces fragmentation through devolved legislation. Cannot exit the asymmetry; must negotiate with three separate legislatures.
 *   - Parliamentary Sovereignty Doctrine: Institutional actor (institutional/arbitrage) — traditional principle of Westminster supremacy persists but operates as performative backdrop to actual convention-based constraint (piton perspective).
 *   - Constitutional Reform Movement: Organized analytical agents (analytical/constrained) — recognizes devolution as temporary scaffolding within larger federal/confederal transition; pushes toward codified constitution with sunset logic.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing devolution as inevitable feature of multi-nation states (false summit risk).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(modern_judicialization__devolution_settlements, 0.38).
domain_priors:suppression_score(modern_judicialization__devolution_settlements, 0.42).
domain_priors:theater_ratio(modern_judicialization__devolution_settlements, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(modern_judicialization__devolution_settlements, extractiveness, 0.38).
narrative_ontology:constraint_metric(modern_judicialization__devolution_settlements, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(modern_judicialization__devolution_settlements, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(modern_judicialization__devolution_settlements, tangled_rope).
narrative_ontology:human_readable(modern_judicialization__devolution_settlements, "Devolution Settlements: Entrenched-in-Practice Powers and Westminster Recall").
narrative_ontology:topic_domain(modern_judicialization__devolution_settlements, "political/legal/constitutional").

domain_priors:requires_active_enforcement(modern_judicialization__devolution_settlements).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(modern_judicialization__devolution_settlements, '56b9341b-8ae8-4193-973c-9f35afecf489').
narrative_ontology:cs_kernel_codification('56b9341b-8ae8-4193-973c-9f35afecf489', distributed).
narrative_ontology:cs_authority_grounding('56b9341b-8ae8-4193-973c-9f35afecf489', extraction).
narrative_ontology:cs_interpretation_layer_present('56b9341b-8ae8-4193-973c-9f35afecf489').
narrative_ontology:cs_reading_relation('56b9341b-8ae8-4193-973c-9f35afecf489', modern_judicialization__human_rights_act_1998, influences).
narrative_ontology:cs_reading_relation('56b9341b-8ae8-4193-973c-9f35afecf489', modern_judicialization__uk_supreme_court_creation, influences).
narrative_ontology:cs_reading_relation('56b9341b-8ae8-4193-973c-9f35afecf489', modern_judicialization__eu_membership_and_exit, coexists_with).
narrative_ontology:cs_axiom('56b9341b-8ae8-4193-973c-9f35afecf489', foundational, multinational_state_territorial_autonomy).
narrative_ontology:cs_axiom_status(multinational_state_territorial_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('56b9341b-8ae8-4193-973c-9f35afecf489', multinational_state_territorial_autonomy, deontological).
narrative_ontology:cs_axiom('56b9341b-8ae8-4193-973c-9f35afecf489', foundational, convention_entrenchment_through_practice).
narrative_ontology:cs_axiom_status(convention_entrenchment_through_practice, holdable).
narrative_ontology:cs_axiom_grounding('56b9341b-8ae8-4193-973c-9f35afecf489', convention_entrenchment_through_practice, instrumental).
narrative_ontology:cs_reference_frame('56b9341b-8ae8-4193-973c-9f35afecf489', unitary_westminster_sovereignty).
narrative_ontology:cs_drift_state('56b9341b-8ae8-4193-973c-9f35afecf489', contemporary_devolved_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('56b9341b-8ae8-4193-973c-9f35afecf489', '').
narrative_ontology:cs_kernel_id(modern_judicialization__devolution_settlements, modern_judicialization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(modern_judicialization__devolution_settlements, devolved_nations_domestic_agendas).
narrative_ontology:constraint_victim(modern_judicialization__devolution_settlements, uniform_uk_wide_policy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNIFORMITY CLAIMANT (SNARE) — Seeks coherent UK-wide policy (taxation, regulation, welfare standards) but faces fragmentation through devolved legislation. Cannot exit the asymmetry; constrained to negotiate with three separate legislative bodies. Experiences extraction as loss of policy coherence and administrative efficiency. Maximum suppression of the unitary alternative.
constraint_indexing:constraint_classification(modern_judicialization__devolution_settlements, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEVOLVED LEGISLATURE (TANGLED ROPE) — Experiences genuine coordination function: devolved assembly coordinates local policy response to regional needs, educates constituents, and accumulates statutory authority through practice. BUT ALSO experiences suppression of unitary recall: Westminster may theoretically revoke powers, creating persistent uncertainty about the permanence of autonomy. Constrained exit because breaking from the UK framework is possible (Scotland independence precedent) but costly. Mixed coordination and extraction.
constraint_indexing:constraint_classification(modern_judicialization__devolution_settlements, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DEVOLVED NATIONS' DOMESTIC AGENDA (ROPE) — Primary beneficiary. Coordinates regional policy response to locally salient problems (healthcare allocation, education curriculum, land reform in Scotland). Genuine coordination function drives the constraint's existence. Arbitrage exit available: threat of independence, EU re-entry, or alternative constitutional arrangements raises the cost of casual Westminster recall. The devolved nation perceives the constraint as legitimate coordination with real benefits to constituents.
constraint_indexing:constraint_classification(modern_judicialization__devolution_settlements, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: WESTMINSTER PARLIAMENT (TANGLED_ROPE) — Theoretically retains unilateral recall authority (parliamentary sovereignty doctrine), but in practice is suppressed from exercising it. The Sewel Convention (1998) creates coordination function: Westminster consults devolved legislatures before legislating on devolved matters. BUT Westminster faces extraction through the convention: observed consent-seeking behavior without formal legal obligation creates path-dependent constraint on parliamentary action. Exit is theoretically available (unilaterally repeal delegation) but politically costly — doing so would trigger constitutional crisis and threaten union. Constrained exit produces mixed coordination/extraction experience.
constraint_indexing:constraint_classification(modern_judicialization__devolution_settlements, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM MOVEMENT (SCAFFOLD) — Organized analysts of UK constitutional practice see devolution as a temporary scaffolding within a larger federal/confederal transition. The constraint is treated as having a sunset: full codified federalism or explicit confederalism would formalize and replace the current convention-based uncertainty. Theater here is the non-binding status of Sewel — the movement sees formal written constitution as the eventual endpoint. Sunset clause is embedded in the logic: devolution-without-entrenchment is recognized as transitional.
constraint_indexing:constraint_classification(modern_judicialization__devolution_settlements, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PARLIAMENTARY SOVEREIGNTY DOCTRINE (PITON) — The foundational legal principle ('Parliament is sovereign and cannot bind itself') persists but is largely performative. The doctrine is invoked to justify Westminster's theoretical authority, but actual practice (the Sewel Convention) demonstrates degradation: parliament regularly accepts obligations it theoretically denies it has. The doctrine is maintained through theatrical deference to tradition, not through functional legal operation. Theater_ratio high because the doctrine's real operation is in the background while the surface narrative preserves parliamentary supremacy.
constraint_indexing:constraint_classification(modern_judicialization__devolution_settlements, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational timescale, federal or confederal arrangements appear as natural and inevitable developments in multi-nation states: historical trajectory suggests devolution is an immutable feature of plural state governance. This perspective sees the constraint as reflecting unchangeable requirements of political stability in diverse polities. Engine will flag as false summit: structural beneficiary data (devolved nations' control over agendas) contradicts pure natural law reading.
constraint_indexing:constraint_classification(modern_judicialization__devolution_settlements, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(modern_judicialization__devolution_settlements_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(modern_judicialization__devolution_settlements, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(modern_judicialization__devolution_settlements, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(modern_judicialization__devolution_settlements, TR),
    TR >= 0.70.

:- end_tests(modern_judicialization__devolution_settlements_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The devolved nations extract control over regional agendas from Westminster's traditional unitary authority. But extraction is not severe (would be 0.70+) because genuine coordination function exists — devolution enables legitimate regional policy response to locally salient problems. Westminster exercises some continued control through framework powers and intergovernmental relations. Measurement trajectory (0.28 → 0.33 → 0.38) reflects gradual accumulation of extraction as devolved legislatures have extended their authority through practice and accumulated statutory case law. Suppression (0.42): Moderate. Westminster's theoretical sovereignty is suppressed (Sewel Convention prevents casual recall), but the suppression is convention-based rather than legally-entrenched. Devolved legislatures face suppression uncertainty (permanent revocation is theoretically possible), creating persistent constraint. Theater ratio (0.55): Moderate. The constraint operates significantly through performance of parliamentary sovereignty doctrine while actual operation is constrained by Sewel Convention. Theater has risen as the gap between doctrine and practice has widened. Not as high as piton (0.70+) because the coordination function is real and the theater is a backdrop, not the primary mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives produce classification ranging from snare (uniformity victim) to rope (beneficiary nations) to mountain (false summit analytical view). The perspectival gap is constitutive of the constraint: from Westminster's theoretical supremacy position, devolution appears as snare (theoretical authority suppressed with no formal legal release); from devolved nations' perspective, it appears as rope (coordination function enabling regional governance); from uniformity advocate's perspective, it appears as snare (policy fragmentation with no exit); from analytical civilizational view, it appears as mountain (inevitable feature of multinational states) — but this is a false summit because structural beneficiary data (devolved nations extract control over regional agendas) contradicts pure natural law claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Devolved nations' power: organized (they coordinate complex legislative agenda internally; they have institutional capacity). Exit: arbitrage (they can threaten independence or constitutional renegotiation, raising Westminster's cost of casual recall). Beneficiary status: yes. Derived d is low (beneficiary + arbitrage → d ~ 0.15-0.25), producing negative or low f(d), low effective extraction chi. Westminster Parliament's power: institutional (parliamentary authority is institutionalized). Exit: constrained (theoretically could revoke devolution unilaterally, but politically costly — constitutional crisis, union dissolution threat). Victim status: yes (Westminster's theoretical authority is suppressed). Derived d is high (victim + constrained → d ~ 0.55-0.70), producing high f(d), higher effective extraction chi. Uniformity claimant's power: moderate (interest groups, policy advocates, administrative actors). Exit: trapped (cannot unilaterally exit the devolved fragmentation). Victim status: yes. Derived d is very high (victim + trapped → d ~ 0.85-0.95), producing maximum f(d), maximum experienced extraction chi. No overrides needed; structural derivation produces appropriate perspectival gaps.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sewel_convention_bindingness,
    'Is the Sewel Convention a binding constitutional principle or a mere political courtesy that Westminster can revoke unilaterally?',
    'Test via constitutional practice: if Westminster legislates on a devolved matter without consulting the relevant assembly, does the devolved legislature successfully challenge it judicially, or does the challenge fail due to lack of legal enforceability?',
    'If binding: constraint is entrenched (mountain-like immutability from analytical perspective becomes structurally real). If courtesy only: Westminster retains theoretical absolute recall power (snare for devolved nations intensifies).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sewel_convention_bindingness, empirical, 'Whether Sewel Convention is legally binding or politically binding only').

omega_variable(
    independence_as_exit,
    'Is Scottish or Welsh independence a credible unilateral exit from the devolution constraint, or is the exit blocked by constitutional/legal barriers Westminster can enforce?',
    'Legal analysis of Westminster''s authority to block devolved-legislature-initiated independence referenda; empirical precedent from the 2014 Scottish referendum and subsequent cases',
    'If credible unilateral exit: devolved legislatures have genuine arbitrage (high-cost but real), reducing experienced extraction. If blocked: exit is constrained or trapped, increasing experienced extraction severity and pushing classification toward snare for devolved victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independence_as_exit, empirical, 'Whether devolved nations can exit through independence').

omega_variable(
    uniform_policy_feasibility,
    'Is uniform UK-wide policy on devolved matters actually necessary for state coherence, or is the ''victim'' framing of uniform-policy-advocates self-interested rather than structurally required?',
    'Comparative institutional analysis: do other federal/confederal states (Germany, Canada, Australia) achieve coherent governance without uniform policy across all regions? What are the actual failures attributable to devolved divergence vs. attributed by Westminster actors?',
    'If uniform policy is not structurally necessary: the ''victim'' framing (uniform_uk_wide_policy) is a political interest group rather than a structural requirement, reducing the legitimacy of the extraction narrative. If necessary: the extraction of policy coherence is real and central to the constraint''s malign effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniform_policy_feasibility, conceptual, 'Whether uniform UK policy is structurally necessary or politically desired').

omega_variable(
    kernel_reading_contest,
    'Is devolution a reading of the kernel ''modern_judicialization'' (separation of powers restructuring the state), or is it a distinct constraint from the judicial kernel entirely?',
    'Trace causal and conceptual relationships: does devolution''s structural logic depend on the Human Rights Act, UK Supreme Court creation, or EU supremacy-sharing frameworks? Or does devolution operate via its own logic (territorial pluralism in a multinational state)?',
    'If devolution is a reading of the modern_judicialization kernel: its ε and classification are contingent on sibling readings'' status (HRA, UKSC, EU). If independent: devolution''s classification stands alone and is not necessarily affected by how other siblings resolve.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship between devolution constraint and modern_judicialization kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(modern_judicialization__devolution_settlements, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(devol_tr_t0, modern_judicialization__devolution_settlements, theater_ratio, 0, 0.35).
narrative_ontology:measurement(devol_tr_t5, modern_judicialization__devolution_settlements, theater_ratio, 5, 0.48).
narrative_ontology:measurement(devol_tr_t10, modern_judicialization__devolution_settlements, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(devol_be_t0, modern_judicialization__devolution_settlements, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(devol_be_t5, modern_judicialization__devolution_settlements, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(devol_be_t10, modern_judicialization__devolution_settlements, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(devol_su_t0, modern_judicialization__devolution_settlements, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(devol_su_t5, modern_judicialization__devolution_settlements, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(devol_su_t10, modern_judicialization__devolution_settlements, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(modern_judicialization__devolution_settlements, resource_allocation).
narrative_ontology:affects_constraint(modern_judicialization__devolution_settlements, modern_judicialization__human_rights_act_1998).
narrative_ontology:affects_constraint(modern_judicialization__devolution_settlements, modern_judicialization__uk_supreme_court_creation).
narrative_ontology:affects_constraint(modern_judicialization__devolution_settlements, modern_judicialization__eu_membership_and_exit).

% DUAL FORMULATION NOTE:
% Devolution is one reading of the modern_judicialization kernel. It is linked structurally to three sibling readings: HRA (courts apply rights across devolved territories, creating pressure for uniform standards); UKSC (courts review devolved legislation, entrenching separation of powers); EU membership/exit (precedent for Westminster accepting external legal constraint; Brexit tested devolution arrangements). All four readings are part of the same multi-decade restructuring of UK state architecture through juridification. Each story has its own ε and classification; the network edges represent causal and conceptual relationships between siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
