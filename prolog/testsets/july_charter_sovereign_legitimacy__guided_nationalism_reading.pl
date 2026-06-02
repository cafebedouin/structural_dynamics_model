% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: Islamic-Nationalist Sovereign Legitimacy in July Charter (Guided Nationalism Reading)
 *   domain: constitutional_law/state_legitimacy/post_revolutionary_governance
 *
 * SUMMARY:
 *   The July Charter, establishing Islamic-nationalist framework with
 *   religious identity as sovereign legitimacy ground, represents ONE READING
 *   of a contested constitutional kernel — the foundational claim about what
 *   grounds legitimate state authority in a post-revolutionary polity. This
 *   reading instantiates the 'guided nationalism' interpretation: sovereignty
 *   is legitimate to the degree it aligns with nationalist aspirations
 *   mediated through Islamic identity. The constraint exhibits tangled
 *   coordination and extraction: it solves the coordination problem of
 *   binding together nationalist and religious constituencies (genuine rope
 *   function) while simultaneously extracting compliance from secular civil
 *   society and religious minorities who cannot endorse the religious
 *   legitimacy ground (genuine snare mechanism for victims). The charter's
 *   theater_ratio (0.55) reflects that some legitimacy appeal is performative
 *   — ritual invocation of religious identity that operates at the discourse
 *   level without necessarily restructuring core institutional functions —
 *   while some is functionally embedded (religious law incorporated into
 *   family code, religious courts, religious content in national security
 *   doctrine). The extractiveness trajectory (0.42 → 0.58 over interval)
 *   shows the extraction mechanism accumulating as the legitimacy framework
 *   gets institutionalized: initial charter is ambiguous enough to maintain
 *   coalition; enforcement hardens over time.
 *
 * KEY AGENTS:
 *   - Nationalist-Religious Coalition: Primary beneficiary (organized/mobile) — architects of the charter; experience it as coordination mechanism binding their constituencies
 *   - State Institutional Apparatus: Beneficiary (institutional/arbitrage) — consolidates executive authority through religious legitimacy grounding; can modulate religious emphasis while maintaining nationalist frame
 *   - Secular Civil Society: Primary victim (powerless/trapped) — subordinated by constitutional embedding of religious nationalism; no viable exit from polity; constrained to operate within delegitimized framework
 *   - Religious Minorities: Primary victim (powerless/trapped) — trapped within sovereignty framework grounded in specific religious identity; face structural subordination in legitimacy hierarchy
 *   - Technocratic Bureaucracy: Secondary victim (moderate/constrained) — benefits from state apparatus but constrained to align administrative rationality with religious-nationalist claims; career costs to exit
 *   - Analytical Observer: Captures the charter reading (analytical/identity_locked) — sees both genuine coordination and genuine extraction but cannot step outside the 'legitimacy must be grounded in something' frame that enables the reading itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.58).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.68).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "Islamic-Nationalist Sovereign Legitimacy in July Charter (Guided Nationalism Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional_law/state_legitimacy/post_revolutionary_governance").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'guided-nationalism-kernel-reading-v1').
narrative_ontology:cs_kernel_codification('guided-nationalism-kernel-reading-v1', formalized).
narrative_ontology:cs_authority_grounding('guided-nationalism-kernel-reading-v1', extraction).
narrative_ontology:cs_interpretation_layer_present('guided-nationalism-kernel-reading-v1').
narrative_ontology:cs_reading_relation('guided-nationalism-kernel-reading-v1', july_charter_sovereign_legitimacy__secular_democratic_reading, coexists_with).
narrative_ontology:cs_reading_relation('guided-nationalism-kernel-reading-v1', july_charter_sovereign_legitimacy__military_custodian_reading, influences).
narrative_ontology:cs_axiom('guided-nationalism-kernel-reading-v1', foundational, religious_identity_constitutive_sovereignty).
narrative_ontology:cs_axiom_status(religious_identity_constitutive_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('guided-nationalism-kernel-reading-v1', religious_identity_constitutive_sovereignty, deontological).
narrative_ontology:cs_axiom('guided-nationalism-kernel-reading-v1', secondary, nationalist_religious_coalition_durability).
narrative_ontology:cs_axiom_status(nationalist_religious_coalition_durability, holdable).
narrative_ontology:cs_axiom_grounding('guided-nationalism-kernel-reading-v1', nationalist_religious_coalition_durability, conventional).
narrative_ontology:cs_reference_frame('guided-nationalism-kernel-reading-v1', islamic_nationalist_identity_fusion).
narrative_ontology:cs_drift_state('guided-nationalism-kernel-reading-v1', contemporary_global_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('guided-nationalism-kernel-reading-v1', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, nationalist_religious_coalition).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, state_institutional_apparatus).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, technocratic_bureaucracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECULAR CIVIL SOCIETY (SNARE) — Faces constitutional embedding of religious nationalism with no viable exit from the polity. Cannot opt out of the legitimacy framework; constrained to operate within or accept subordination. Maximum experienced extraction without agency or alternative institutional pathway.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__guided_nationalism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS MINORITIES (SNARE) — Trapped within a sovereignty framework that grounds legitimacy in a specific religious identity. Cannot exit nationality; face structural subordination in the legitimacy hierarchy. Subject to constitutional constraint with no meaningful agency in redefining legitimacy terms.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__guided_nationalism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOCRATIC BUREAUCRACY (TANGLED ROPE) — Benefits from state capacity and institutional continuity, but constrained by the requirement to align administrative rationality with religious-nationalist legitimacy claims. Experiences mixed coordination (state apparatus) and extraction (religious constraints on policy space). Exit is costly — abandoning professional role means losing state position and institutional authority.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NATIONALIST-RELIGIOUS COALITION (ROPE) — Primary coalition architects. Experience the charter as pure coordination: it solves collective action problem of legitimacy consolidation across nationalist and religious constituencies. Both groups have mobile exit options (can mobilize alternative coalitions or withdraw support) but are mutually reinforced by the constitutional framework. Net coordination benefit; minimal experienced extraction.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__guided_nationalism_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE INSTITUTIONAL APPARATUS (ROPE) — Benefits from constitutional legitimacy framework that consolidates executive authority. Religious nationalism provides a sovereignty grounding that is difficult for opponents to challenge (sacralized delegitimation). State extracts institutional durability and decision-making autonomy from the religious legitimacy layer. Exit option is arbitrage: can modulate religious emphasis while maintaining nationalist frame.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__guided_nationalism_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE — IDENTITY_LOCKED) — The analytical position is itself captured by the dual-legitimacy cognitive frame. Observer sees genuine coordination function (national cohesion) alongside genuine extraction (subordination of non-aligned groups) but cannot step outside the 'legitimacy is necessarily grounded in something sacred' frame that constitutes both readings simultaneously. The identity lock is epistemic: the framework of sovereignty analysis itself presupposes a legitimacy ground.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__guided_nationalism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__guided_nationalism_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The charter extracts compliance from non-aligned groups through constitutional embedding of religious identity as legitimacy ground. This is not total extraction (some secular policy space remains; religious minorities retain some institutional participation) but is substantial — the legitimacy framework makes contestation structurally harder and delegitimizes alternative frames. The trajectory rising from 0.42 to 0.58 reflects the extraction mechanism accumulating as the framework gets institutionalized and enforcement machinery matures. Suppression (0.68): High. Secular alternatives face both material barriers (institutional exclusion, resource concentration) and epistemic barriers (delegitimation through the religious-nationalist frame). Religious minorities face legal subordination in specific domains (family law, personal status) and categorical exclusion from full legitimacy participation. Theater ratio (0.55): Moderate. The religious legitimacy appeal operates partly at the discourse/ritual level (invocation in national identity narratives, religious content in education and media) and partly as functionally embedded institutional mechanism (religious courts, religious content in security doctrine, religious law in family code). Not purely performative (actual institutional mechanisms embedded) but not purely functional (significant rhetorical/symbolic content).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits acute perspectival divergence. The nationalist-religious coalition sees pure coordination (Rope) — the charter solves their collective action problem. The state apparatus sees coordination with arbitrage benefit (Rope) — religious legitimacy consolidates authority durably. Secular civil society sees pure extraction with no exit (Snare) — they are subordinated by constitutional terms and cannot contest the legitimacy frame itself. Religious minorities see snare-level extraction (Snare) — they are trapped within a sovereignty frame grounded in a religious identity they may not endorse. Technocratic bureaucrats see tangled coordination and constraint (Tangled Rope) — state capacity is real, but the religious-nationalist alignment requirement constrains policy autonomy. The analytical observer sees the full structure but is itself identity-locked within the 'legitimacy requires grounding' epistemic frame — unable to see whether the religious grounding is constitutive or contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status plus exit options. Beneficiaries of the charter (nationalist-religious coalition, state apparatus) have mobile or arbitrage exit options — they can modulate their commitment while maintaining institutional presence. Victims (secular civil society, religious minorities) are trapped with no viable exit — they cannot leave the polity and cannot contest the legitimacy framework itself. Technocratic bureaucrats are constrained (costly exit through career risk) and victims (policy autonomy limitation) — their d values reflect both identity with state apparatus and extraction through religious-nationalist constraint. The analytical observer's identity_locked classification reflects epistemic capture: the observer cannot see outside the frame that 'sovereignty must be grounded in something,' preventing recognition that the grounding itself is a choice, not an inevitable fact.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the kernel reading structure. The same structural data (charter embedding religious identity in legitimacy) classifies as Rope (coalition perspective), Tangled Rope (bureaucratic perspective), and Snare (victim perspectives). The mandatrophy is not 'which is correct?' but 'which reading of the kernel do you inhabit?' The guided nationalism reading presupposes that religious identity is a legitimate sovereignty ground, making the charter's coordination genuine and the extraction of non-aligned actors justified as defense against illegitimate contestation. The secular democratic reading (sibling constraint) presupposes that religious identity cannot legitimately constrain civic participation, making the charter's extraction primary and the coordination secondary (contingent on coercion). The military custodian reading (sibling constraint) presupposes that state stability requires institutional autonomy from all identity-based claims (religious and secular), making the charter a threat to institutional integrity. No single classification resolves across readings — the presuppositions of each reading generate different victim/beneficiary structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_identity_as_contingent_vs_essential,
    'Is religious identity constitutive of national sovereignty in this polity, or is the charter weaponizing religious identity as a contingent legitimacy claim?',
    'Comparative constitutional analysis: do pre-revolutionary legitimacy claims invoke religious grounding? Do core institutions (military, judiciary, legislature) function differently under religious vs nationalist framing? Historical track of policy reversals when religious vs nationalist priorities conflict.',
    'If constitutive: constraint is a genuine coordination mechanism binding diverse constituencies. If weaponized: constraint is primarily extractive, using religious framing as a barrier to contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_identity_as_contingent_vs_essential, conceptual, 'Whether religious identity is constitutive of sovereignty or a legitimacy claim mechanism').

omega_variable(
    secular_alternative_coalition_viability,
    'Could a coalition of secular-nationalist and religious-minority actors construct an alternative legitimacy framework that is institutionally viable?',
    'Historical counterfactual: analysis of organizational capacity, resource distribution, international support structures for alternative legitimacy frames. Empirical tracking of moments when alternative frames gained traction or were suppressed.',
    'If viable: trapped agents actually face constrained exit (not mountain-level immobility); perspectival classification shifts upward. If non-viable: trapped classification is accurate; constraint operates with higher suppression than declared.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secular_alternative_coalition_viability, preference, 'Whether alternative legitimacy coalitions are institutionally viable').

omega_variable(
    religious_minorities_dual_identity_loyalty,
    'Do religious minorities within the nation experience the religious-nationalist legitimacy frame as excluding them (victim status), or do they operate within nested religious identities (co-beneficiary to religious nationalism even as it excludes their specific sect/denomination)?',
    'Qualitative analysis of minority institutional positioning, representation, and agency. Empirical tracking of minority support or defection patterns across legitimacy moments.',
    'If excluding: victims are broader than declared. If nested: victims are narrower; some religious minorities may experience rope perspective (coordination benefit through shared religious legitimacy layer despite sectarian differences).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_minorities_dual_identity_loyalty, empirical, 'Whether religious minorities experience the frame as excluding or nested').

omega_variable(
    nationalist_religious_coalition_durability,
    'How stable is the nationalist-religious coalition boundary? Do these actors experience coalition cost or mutual reinforcement?',
    'Historical analysis of coalition divergence points, policy disputes, institutional conflict. Measurement of rhetoric and institutional behavior when nationalist and religious priorities diverge.',
    'If durable coalition: rope classification holds; coordination benefit is real. If coalition strain: effective extraction between coalition members; tangled_rope shifts to snare territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nationalist_religious_coalition_durability, empirical, 'Durability of nationalist-religious coalition as integrated actor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcsg_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(jcsg_tr_t3, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 3, 0.52).
narrative_ontology:measurement(jcsg_tr_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(jcsg_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(jcsg_be_t3, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(jcsg_be_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(jcsg_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(jcsg_su_t3, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(jcsg_su_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.12).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_custodian_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, family_law_religious_jurisdiction).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_court_authority).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, national_education_curriculum_islam).

% DUAL FORMULATION NOTE:
% The July Charter kernel has three structurally distinct readings with different ε values and beneficiary/victim structures. The guided nationalism reading (this file, ε=0.58) asserts religious identity as constitutive of sovereignty. The secular democratic reading (sibling, ε~0.72) asserts religious identity as contingent cultural fact, making the constraint primarily extractive. The military custodian reading (sibling, ε~0.35) asserts the charter threatens institutional autonomy, making it primarily a coordination failure. Network relationships: guided nationalism affects family law religious jurisdiction (specifies scope of religious authority), religious court authority (enables implementation), and national education curriculum islam (broadcasts legitimacy frame). All three readings form a presheaf over the kernel — no single reading subsumes the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
