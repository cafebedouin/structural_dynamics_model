% ============================================================================
% CONSTRAINT STORY: social_revolution_provisions__personal_law_compromise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_revolution_provisions__personal_law_compromise, []).

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
 *   constraint_id: social_revolution_provisions__personal_law_compromise
 *   human_readable: Social Revolution Provisions: Personal Law Compromise Reading
 *   domain: legal/constitutional/doctrinal
 *
 * SUMMARY:
 *   The social revolution stopped at the family: personal laws of the
 *   communities left standing, the uniform civil code parked in the
 *   directives — this reading instantiates one specific constitutional
 *   interpretation within a contested kernel. The kernel
 *   (social_revolution_provisions) contains at least three structurally
 *   distinct readings: (1) this one — the personal_law_compromise reading,
 *   which frames the deferral of family law uniformity as a bargained
 *   accommodation between revolutionary egalitarian principles and communal
 *   legal pluralism; (2) the reservation_architecture reading, which frames
 *   affirmative action for scheduled castes/tribes as equality's fulfillment
 *   rather than its exception; (3) the untouchability_abolition reading,
 *   which frames Article 17's direct constitutional prohibition as
 *   immediately enforceable against private actors. This reading focuses on
 *   the first: how the constitutional order traded the implementation of
 *   uniform family law (a direct equality commitment) for the political
 *   accession of community authorities who demanded preservation of personal
 *   law domains. The compromise appears in the Directive Principles (Article
 *   44) as an aspiration, not an enforceable right — the uniform code is
 *   deferred indefinitely, its implementation subject to the consent and
 *   cooperation of the very communities whose power the compromise protects.
 *   The structural delta this reading instantiates: suppression of family-law
 *   uniformity deferred (women and marginalized members remain subject to
 *   unreformed personal laws); beneficiary is community legal autonomy
 *   (religious authorities, cultural leaders, community councils retain
 *   jurisdiction over family matters); victim set is family members
 *   (especially women) under those unreformed regimes; extractiveness within
 *   the family is left to community rule-making processes without
 *   constitutional guarantee of equality. The extraction mechanism is not
 *   crude force but a constitutionally legitimated deferral: the state
 *   formally recognizes community legal authority while indefinitely
 *   postponing the equality it promised. Over 75+ years, this constraint has
 *   accumulated extractiveness as the gap between promised uniformity and
 *   actual pluralism has stabilized into institutional inertia; the theater
 *   ratio has risen as the Directive Principles language performs egalitarian
 *   commitment while the actual practice entrenches communal authority.
 *
 * KEY AGENTS:
 *   - Women Under Personal Law: Powerless/trapped. Bear extraction through inheritance restrictions, divorce impediments, guardianship requirements, maintenance denial. No formal exit to uniform civil code.
 *   - Marginalized Community Members: Powerless/constrained. Experience extraction through community council authority operating without constitutional safeguards. Some geographic exit possible but economic dependence and caste-embedded property restrictions bind most.
 *   - Women's Rights Movements and Reform Coalitions: Organized/constrained. Coordinate on uniform code implementation but extract negotiation burden — reform energy consumed bargaining with entrenched authorities rather than building universal law.
 *   - Community Religious and Cultural Authorities: Institutional/arbitrage. Primary beneficiaries of the compromise. Coordinate preservation of legal authority; have arbitrage exit if state pressure increases (appeal to religious identity politics, parallel systems).
 *   - The State (Constitutional Custodian): Institutional/constrained. Coordinates transition politics through compromise while deferring equality implementation. Constrained: pushing uniform code risks communal backlash; deferring indefinitely betrays equality promises. Requires active enforcement of the deferral itself (not legislating the code).
 *   - Article 44 Uniform Civil Code Directive: Institutional/arbitrage. Performs equality commitment while sustaining status quo through perpetual negotiation. Piton: formally present but theatrically deferred, maintained through inertia.
 *   - Constitutional Reform Movements: Organized/mobile. Building pressure for uniform civil code sunset. Scaffold perspective: explicitly committed to transitioning the compromise toward universal family law.
 *   - Analytical Observer: Analytical/analytical. Risks naturalizing compromise as immutable feature of multicultural constitutionalism. False summit candidate: the 'necessity' claim may itself be extraction mechanism (naturalization covering political power).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_revolution_provisions__personal_law_compromise, 0.52).
domain_priors:suppression_score(social_revolution_provisions__personal_law_compromise, 0.65).
domain_priors:theater_ratio(social_revolution_provisions__personal_law_compromise, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_revolution_provisions__personal_law_compromise, extractiveness, 0.52).
narrative_ontology:constraint_metric(social_revolution_provisions__personal_law_compromise, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(social_revolution_provisions__personal_law_compromise, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_revolution_provisions__personal_law_compromise, tangled_rope).
narrative_ontology:human_readable(social_revolution_provisions__personal_law_compromise, "Social Revolution Provisions: Personal Law Compromise Reading").
narrative_ontology:topic_domain(social_revolution_provisions__personal_law_compromise, "legal/constitutional/doctrinal").

domain_priors:requires_active_enforcement(social_revolution_provisions__personal_law_compromise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(social_revolution_provisions__personal_law_compromise, '942bc907-5036-4675-bc51-c7a4c48002c2').
narrative_ontology:cs_kernel_codification('942bc907-5036-4675-bc51-c7a4c48002c2', formalized).
narrative_ontology:cs_authority_grounding('942bc907-5036-4675-bc51-c7a4c48002c2', extraction).
narrative_ontology:cs_interpretation_layer_present('942bc907-5036-4675-bc51-c7a4c48002c2').
narrative_ontology:cs_reading_relation('942bc907-5036-4675-bc51-c7a4c48002c2', social_revolution_provisions__reservation_architecture, coexists_with).
narrative_ontology:cs_reading_relation('942bc907-5036-4675-bc51-c7a4c48002c2', social_revolution_provisions__untouchability_abolition_article_17, coexists_with).
narrative_ontology:cs_axiom('942bc907-5036-4675-bc51-c7a4c48002c2', foundational, community_legal_autonomy_over_family).
narrative_ontology:cs_axiom_status(community_legal_autonomy_over_family, holdable).
narrative_ontology:cs_axiom_grounding('942bc907-5036-4675-bc51-c7a4c48002c2', community_legal_autonomy_over_family, deontological).
narrative_ontology:cs_axiom('942bc907-5036-4675-bc51-c7a4c48002c2', foundational, uniformity_deferrable_via_accession).
narrative_ontology:cs_axiom_status(uniformity_deferrable_via_accession, holdable).
narrative_ontology:cs_axiom_grounding('942bc907-5036-4675-bc51-c7a4c48002c2', uniformity_deferrable_via_accession, instrumental).
narrative_ontology:cs_reference_frame('942bc907-5036-4675-bc51-c7a4c48002c2', community_legal_autonomy_preservation_framework).
narrative_ontology:cs_drift_state('942bc907-5036-4675-bc51-c7a4c48002c2', contemporary_reform_pressure_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('942bc907-5036-4675-bc51-c7a4c48002c2', '').
narrative_ontology:cs_kernel_id(social_revolution_provisions__personal_law_compromise, social_revolution_provisions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_revolution_provisions__personal_law_compromise, community_legal_autonomy).
narrative_ontology:constraint_beneficiary(social_revolution_provisions__personal_law_compromise, community_religious_authority).
narrative_ontology:constraint_victim(social_revolution_provisions__personal_law_compromise, family_law_members).
narrative_ontology:constraint_victim(social_revolution_provisions__personal_law_compromise, women_under_personal_law).
narrative_ontology:constraint_victim(social_revolution_provisions__personal_law_compromise, marginalized_community_members).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN UNDER PERSONAL LAW (SNARE) — Trapped within community family law systems that the constitutional compromise deferred reforming. No exit to uniform civil code; no structural appeal to national equality guarantees for family law matters. Bear maximum extraction through inheritance deprivation, divorce restrictions, guardianship requirements, and maintenance denial. Suppression is near-total: exit would require abandoning family, community, and often economic survival.
constraint_indexing:constraint_classification(social_revolution_provisions__personal_law_compromise, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITY MEMBERS (SNARE) — The compromise locks them into communal legal systems with minimal internal due process. Experience extraction through caste-based property restrictions, untouchability-adjacent exclusions (though Article 17 formally abolishes untouchability, personal law spheres can embed it structurally), and community council authority that operates without constitutional safeguards. Constrained rather than fully trapped because some have migrated to urban centers where enforcement weakens, but geographic and economic dependence bind most.
constraint_indexing:constraint_classification(social_revolution_provisions__personal_law_compromise, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WOMEN'S RIGHTS MOVEMENTS (TANGLED ROPE) — Coordinating on shared interest in uniform civil code implementation, but also extracting selective leverage from the deferral structure itself: the compromise creates a negotiation space where reform coalitions must bargain with community authorities rather than simply legislate. The constraint provides both coordination (the reform movement exists and organizes because the deferral is finite and negotiable) and asymmetric extraction (movement energy is consumed negotiating with entrenched authorities rather than building universally applicable law). Requires active enforcement: community leaders actively suppress reform initiatives through local institution control.
constraint_indexing:constraint_classification(social_revolution_provisions__personal_law_compromise, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMUNITY AUTHORITIES (ROPE) — The compromise is their coordination mechanism: formal recognition of community legal autonomy solves the collective action problem of preserving cultural authority structures during a democratic transition. They have arbitrage exit: if the state pushes too hard on uniform civil code, they can amplify religious identity politics and defect to alternative institutional forms (parallel legal systems, diaspora structures). The extracted benefit is explicit: constitutional protection of personal law domains where their authority is unquestioned. Low suppression requirement because the legal framework legitimates their role — members largely internalize community law as normal and binding.
constraint_indexing:constraint_classification(social_revolution_provisions__personal_law_compromise, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE STATE (TANGLED ROPE) — Coordinating on the legitimate function of constitutional equality while deferring its application to family law (solving the immediate political economy of accession); also extracting benefit through reduced opposition from community authorities who are mollified by legal autonomy preservation. The state's extraction is moderate but real: it defers implementing full equality, using community accommodation as political capital. Constrained exit: if the state pushes hard on uniform code, it risks communal backlash and destabilizes coalition governance; if it does nothing, it betrays equality promises. Requires active enforcement: the state must actively maintain the deferral (not legislating the code) while simultaneously operating formal equality machinery elsewhere.
constraint_indexing:constraint_classification(social_revolution_provisions__personal_law_compromise, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ARTICLE 44 UNIFORM CIVIL CODE DIRECTIVE (PITON) — The constitutional directive to enact a uniform civil code remains formally in the Constitution (Directive Principles) but has been theatrically deferred for decades. It performs the function of showing commitment to equality while actually sustaining the status quo through perpetual negotiation. The theater ratio is high because the directive exists but is not enforced — it is aspirational language maintained through formal recognition rather than functional implementation. The inertia is institutional: the directive cannot be repealed (too symbolically important) but also cannot be implemented (too politically costly). Piton classification reflects that the mechanism is primarily performative (shows commitment) rather than extractive (actively enforces extraction).
constraint_indexing:constraint_classification(social_revolution_provisions__personal_law_compromise, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: CONSTITUTIONAL REFORM MOVEMENTS (SCAFFOLD) — Organized agents working toward a sunset of the compromise: women's rights organizations, secular reform groups, and constitutional scholars are actively building pressure for uniform civil code implementation. The scaffold classification derives from the explicit sunset architecture embedded in the compromise itself — Article 44 creates a formal commitment to sunset the personal law regime, making this temporary support (theoretically) with a deadline. Low theater because the reform movements operate through genuine political and legal advocacy (constitutional amendments, litigation, policy development) rather than performative ritual. Temporal structure is clear: the scaffold exists to support transition from compromised pluralism to constitutional universalism.
constraint_indexing:constraint_classification(social_revolution_provisions__personal_law_compromise, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / ESSENTIALIST VIEW (MOUNTAIN) — From a civilizational perspective, the compromise appears as an immutable structural necessity: multicultural states cannot implement uniform family law without destroying cultural pluralism itself; therefore the deferral is an inevitable feature of any constitutional order that respects both equality and cultural difference. This perspective risks naturalizing a contingent political choice as an immutable law of constitutional design. The engine will flag this as a false summit: the structure is not a natural law but an active choice with identifiable beneficiaries (community authorities, state authorities managing the transition) and identifiable victims (women, marginalized members trapped in unreformed regimes).
constraint_indexing:constraint_classification(social_revolution_provisions__personal_law_compromise, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_revolution_provisions__personal_law_compromise_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_revolution_provisions__personal_law_compromise, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_revolution_provisions__personal_law_compromise, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_revolution_provisions__personal_law_compromise, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_revolution_provisions__personal_law_compromise, TR),
    TR >= 0.70.

:- end_tests(social_revolution_provisions__personal_law_compromise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The compromise extracts benefit to community authorities (protection of legal jurisdiction) and to state authorities managing transition (political stability through deferral). The extraction is not maximal because: (1) women and marginalized members have some (costly, constrained) exit options through state family courts and civil law alternatives; (2) reform movements have created political pressure that makes the extraction unstable — it must be actively enforced rather than passively inherited. The trajectory from 0.35 to 0.55 over 75 years reflects accumulation: as initial reform momentum faded and institutional inertia solidified, the cost to those trapped under personal law regimes increased. Suppression (0.65): High. Multiple layers: (1) Structural — women lack formal legal exit to uniform code, state courts have limited jurisdiction over personal law matters, community alternatives are economically ruinous. (2) Institutional — community authorities control dispute resolution, property registration, inheritance administration. (3) Internalized — women and marginalized members internalize personal law as legitimate community norms. Theater Ratio (0.58): Moderate-high. The Directive Principles language (Article 44) performs commitment to uniformity while the actual practice entrenches pluralism. Reform negotiations are theatrical in form (parliamentary debate, commission reports, stakeholder consultations) while substantive implementation remains indefinitely deferred. The ratio rose over time as the performance grew more elaborate while implementation stalled. Claimed type (Tangled Rope): The constraint exhibits genuine coordination (the compromise coordinates the transition from colonial rule with minimal immediate communal backlash) alongside asymmetric extraction (benefiting community authorities, extracting from those locked in unreformed family law). Requires active enforcement: the state must actively maintain the deferral, not legislating the code, while community authorities actively enforce jurisdiction over family matters. The presence of both beneficiaries (community authorities) and victims (women under personal law) confirms Tangled Rope gates are satisfied.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence. The constraint appears as Snare to those locked in unreformed personal law (powerless/trapped); Rope to community authorities who benefit from legal autonomy (institutional/arbitrage); Tangled Rope to the state managing transition (institutional/constrained); Scaffold to reform movements building toward sunset (organized/mobile); Piton to the formal directive now theatrically maintained (institutional/analytical); and risks Mountain to the analytical observer naturalizing the compromise as immutable (analytical/analytical). This divergence is diagnostic: each perspective is correct from its structural position. No single type captures the constraint — the presheaf over the observation site IS the answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from beneficiary/victim status and exit options. Community authorities as beneficiaries with arbitrage exit derive low d (approximately 0.15) → low effective extraction chi toward them. Women as victims with trapped exit derive high d (approximately 0.95) → high effective extraction chi against them. The state as both beneficiary (political stability through compromise) and victim (trapped implementing equality) derives moderate d (approximately 0.50). Reform movements as constrained victims with some institutional leverage derive moderate-high d (approximately 0.65). This perspectival differentiation explains the Snare/Rope/Tangled Rope divergence without invoking separate constraints: it is one constraint viewed from different structural positions, producing legitimate disagreement about its type. The canonical formula χ = ε × f(d) × σ(S) applies: baseline extractiveness 0.52, multiplied by f(d) sigmoid function applied to each agent's directionality. Beneficiaries experience negative or near-zero chi; victims experience high chi; ambivalent actors experience moderate chi.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING EXEMPLAR: The mandatrophy (the paradox of simultaneously coordination and extraction) is resolved by recognizing that the constraint is coordination for some agents (community authorities who benefit from preserved jurisdiction) and extraction for others (women locked in unreformed family law). The Tangled Rope classification encompasses both: the mechanism coordinates transition politics while asymmetrically extracting from those whose egalitarian promise is indefinitely deferred. The false summit risk at the analytical perspective (Mountain view — immutable feature of multicultural states) is a mandatrophy warning: naturalizing the compromise as inevitable risks erasing the contingency that made the compromise a choice rather than a law. The engine's FSM (false summit machine) will flag this: the mountain classification has identifiable beneficiaries and victims, indicating that what appears immutable is actually a structured extraction mechanism. Resolving the mandatrophy requires holding both truths: the compromise is genuinely a coordination solution to a real political problem (multiconfessional states cannot implement uniform family law without destabilizing) AND it is genuinely an extraction mechanism (those trapped in unreformed regimes bear the cost of the compromise's stability). The resolution is structural: classify as Tangled Rope, accept the perspectival gap, and track which agents have agency to exit or reform the compromise (they don't, structurally, making the constraint extractive; but reform movements are building such agency, making the constraint temporally bounded).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deferral_temporality_ambiguity,
    'Is the deferral of uniform civil code a genuinely temporary compromise with a real sunset, or a permanent institutional arrangement disguised as temporary?',
    'Longitudinal analysis of legislative attempts (frequency, intensity, political feasibility); tracking of reform coalitions'' capacity and resource allocation; assessment of whether community authority structures are weakening or consolidating over time.',
    'If genuinely temporary: constraint classification as Scaffold is correct — the mechanism is providing temporary support toward a defined transition. If permanent: reclassify as Tangled Rope sustained indefinitely — the deferral is the actual extraction mechanism, not a transitional device. Mandatrophy implications: Scaffold implies managed sunset; Tangled Rope implies indefinite structural extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deferral_temporality_ambiguity, empirical, 'Whether the deferral is genuinely temporary or a permanent arrangement').

omega_variable(
    community_authority_internal_pluralism,
    'Do community legal authority structures (religious councils, cultural bodies, family elders) have internal mechanisms for reform that could produce uniform-code-compatible family law from within, or are they structurally locked into unreformed precedent?',
    'Documentation of internal reform movements within communities; comparative analysis of communities that have reformed family law internally vs. those that have not; assessment of whether institutional incentives favor reform or entrenchment.',
    'If internal reform pathways exist and are accessible: beneficiary and victim classifications within communities must differentiate between reformist and traditional authorities; suppression mechanisms may be contestable from within rather than requiring external state intervention. If communities are structurally locked: the compromise is more extractive than current classification suggests — it protects conservative authorities against both state and internal reform pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_authority_internal_pluralism, empirical, 'Whether community legal structures have internal reform pathways').

omega_variable(
    kernel_reading_containment,
    'This reading instantiates one of three sibling readings of the social_revolution_provisions kernel. Does the existence of the other two readings (reservation_architecture, untouchability_abolition) undermine or strengthen the personal_law_compromise reading''s claim to represent the constitutional settlement?',
    'Constitutional history analysis: Did the framers intend the three provisions (personal law compromise, reservation architecture, untouchability abolition) to be mutually supporting or in tension? Have courts treated them as coherent or contradictory? Have reform movements mobilized any reading against the others?',
    'If the three readings are mutually reinforcing parts of a single integrative constitutional settlement: the personal_law_compromise reading is one legitimate facet of a larger structure. If they are in hidden contradiction: the reading may be concealing a deeper structural incoherence in the constitutional order. Classification might shift if the reading is revealed to be in foreclosing relationship to the untouchability abolition rather than coexisting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_containment, conceptual, 'Relationship between this reading and its sibling readings within the kernel').

omega_variable(
    false_summit_natural_law_claim,
    'Is the analytical observer''s classification of the compromise as a natural law of constitutional design (immutable feature of multicultural constitutionalism) a genuine structural insight, or a naturalization of a contingent political choice benefiting specific authorities?',
    'Comparative constitutional analysis: Do other multicultural democracies treat family law differently? Are there constitutional orders that combine cultural pluralism WITH uniform family law? What structural features (if any) truly force the choice between equality and pluralism?',
    'If the compromise is truly structurally necessary: Mountain classification is appropriate and the FSM (false summit machine) should not trigger. If the compromise is a contingent choice: FSM triggers, reclassifying to the actual constraint type (Tangled Rope or Snare), revealing that the ''necessity'' claim is itself an extraction mechanism (naturalization as cover for political power).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, empirical, 'Whether the compromise is a structural necessity or a contingent political choice').

omega_variable(
    women_s_agency_and_exit_modulation,
    'How much agency do women under personal law have to adopt alternative legal frameworks (civil marriage, secular inheritance, state-level family court jurisdiction) without formally exiting the community?',
    'Empirical documentation of exit pathways: civil marriage registries, state family courts'' jurisdiction over personal law matters, divorce and remarriage options under multiple legal regimes. Assessment of social/economic costs of pursuing formal alternatives while remaining community members.',
    'If significant agency exists (civil alternatives accessible, state courts available, low social cost): exit_options for women should be reclassified from ''trapped'' to ''constrained'' or ''mobile'' — suppression is high-cost but not absolute. If alternatives are unavailable or socially ruinous: ''trapped'' is correct. Classification of their perspective may shift from Snare to Tangled Rope if genuine exit options exist despite high suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_s_agency_and_exit_modulation, empirical, 'Degree of women''s exit agency within/across legal frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_revolution_provisions__personal_law_compromise, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(srp_plc_tr_t0, social_revolution_provisions__personal_law_compromise, theater_ratio, 0, 0.48).
narrative_ontology:measurement(srp_plc_tr_t15, social_revolution_provisions__personal_law_compromise, theater_ratio, 15, 0.52).
narrative_ontology:measurement(srp_plc_tr_t30, social_revolution_provisions__personal_law_compromise, theater_ratio, 30, 0.58).
narrative_ontology:measurement(srp_plc_tr_t45, social_revolution_provisions__personal_law_compromise, theater_ratio, 45, 0.61).

% Extraction over time
narrative_ontology:measurement(srp_plc_be_t0, social_revolution_provisions__personal_law_compromise, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(srp_plc_be_t15, social_revolution_provisions__personal_law_compromise, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(srp_plc_be_t30, social_revolution_provisions__personal_law_compromise, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(srp_plc_be_t45, social_revolution_provisions__personal_law_compromise, base_extractiveness, 45, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(srp_plc_su_t0, social_revolution_provisions__personal_law_compromise, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(srp_plc_su_t15, social_revolution_provisions__personal_law_compromise, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(srp_plc_su_t30, social_revolution_provisions__personal_law_compromise, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(srp_plc_su_t45, social_revolution_provisions__personal_law_compromise, suppression_requirement, 45, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_revolution_provisions__personal_law_compromise, identity_coordination).
narrative_ontology:affects_constraint(social_revolution_provisions__personal_law_compromise, social_revolution_provisions__reservation_architecture).
narrative_ontology:affects_constraint(social_revolution_provisions__personal_law_compromise, social_revolution_provisions__untouchability_abolition_article_17).

% DUAL FORMULATION NOTE:
% This reading is one of three sibling readings within the social_revolution_provisions kernel. The three readings are structurally distinct constraints with different ε values and different victim/beneficiary structures: this reading (personal_law_compromise, ε=0.52) focuses on family law deferral and beneficiaries community authorities; reservation_architecture (ε values TBD) focuses on affirmative action and beneficiaries scheduled castes/tribes; untouchability_abolition (ε values TBD) focuses on direct constitutional prohibition and beneficiaries untouchability victims. All three are linked through the contested kernel. Each should be authored as a separate story with its own ε, perspectives, and measurements. The network edges reflect that the three readings compete for authoritative interpretation of the constitutional settlement and create structural pressure on each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_revolution_provisions__personal_law_compromise, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
