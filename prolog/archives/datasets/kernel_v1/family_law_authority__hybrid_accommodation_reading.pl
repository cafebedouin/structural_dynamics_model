% ============================================================================
% CONSTRAINT STORY: family_law_authority__hybrid_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__hybrid_accommodation_reading, []).

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
 *   constraint_id: family_law_authority__hybrid_accommodation_reading
 *   human_readable: Family Law Authority: Hybrid Accommodation (State Floor + Community Governance)
 *   domain: constitutional_law/legal_pluralism/family_law
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid accommodation reading of
 *   contested family law authority in plural legal systems (primarily India,
 *   but structurally present in Israel, many Muslim-majority states, and
 *   post-colonial constitutional frameworks). The hybrid accommodation
 *   reading holds that family law authority is legitimately shared: religious
 *   communities govern internal practice (marriage ritual, divorce
 *   procedures, inheritance norms) through their own legal traditions, while
 *   the constitutional state enforces a floor of fundamental rights (gender
 *   equality, freedom of religion, protection from arbitrary harm). This
 *   reading positions itself between two competing claims: the communal
 *   autonomy reading argues that community tradition has autonomous validity
 *   independent of state approval; the constitutional supremacy reading
 *   argues that all family law must flow from state authority with no
 *   community veto. The hybrid accommodation claims both can coexist:
 *   communities retain genuine governance authority while the state retains
 *   ultimate authority to set and enforce the constitutional floor. The
 *   constraint exhibits tangled_rope classification because it contains both
 *   genuine coordination function (delegation to communities reduces
 *   enforcement burden, respects cultural diversity) and asymmetric
 *   extraction (state retains veto power, women's rights depend on state
 *   willingness to intervene, religious authorities extract deference from
 *   women through uncodified norms). The extractiveness value (0.35) reflects
 *   that enforcement is selective and depends on judicial intervention,
 *   political will, and willingness of victims to litigate against family and
 *   community. Theater ratio (0.58) indicates moderate performative content:
 *   the state performs respect for community authority while courts
 *   incrementally expand rights protections through interpretation.
 *
 * KEY AGENTS:
 *   - Constitutional State: Primary beneficiary (institutional/arbitrage) — retains ultimate authority, enforces floor through courts, maintains political legitimacy by respecting community traditions
 *   - Religious Legal Authorities (Codified): Moderate beneficiaries (institutional/arbitrage) — retain internal governance, win state enforcement machinery, lose discretion to judicial review
 *   - Religious Legal Authorities (Uncodified): Mixed beneficiaries/victims (organized/constrained) — retain governance authority but lack state enforcement, forced to use informal sanctions
 *   - Women in Codified Communities: Secondary victims (moderate/constrained) — face unequal rules but can access courts, codification creates enforceable standards
 *   - Women in Uncodified Communities: Primary victims (powerless/identity_locked) — face maximum extraction: traditional rules uncodified, enforcement depends on litigation against family/community, exit requires abandoning identity
 *   - Religious Minorities: Secondary victims (moderate/constrained) — excluded from state-recognized family law regimes, must navigate jurisdiction gaps
 *   - Secular Reformist Coalition: Organized agents (organized/constrained) — can litigate and lobby for reform, but extraction persists through state deference to community authority
 *   - Constitutional Courts: Institutional actors (institutional/arbitrage) — enforce floor through incremental interpretation, function as reform mechanism with sunset logic
 *   - Analytical Observer: Sees structure from civilizational perspective (analytical/analytical) — risks naturalizing constructed allocation as inevitable legal pluralism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hybrid_accommodation_reading, 0.35).
domain_priors:suppression_score(family_law_authority__hybrid_accommodation_reading, 0.48).
domain_priors:theater_ratio(family_law_authority__hybrid_accommodation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hybrid_accommodation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(family_law_authority__hybrid_accommodation_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(family_law_authority__hybrid_accommodation_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hybrid_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hybrid_accommodation_reading, "Family Law Authority: Hybrid Accommodation (State Floor + Community Governance)").
narrative_ontology:topic_domain(family_law_authority__hybrid_accommodation_reading, "constitutional_law/legal_pluralism/family_law").

domain_priors:requires_active_enforcement(family_law_authority__hybrid_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hybrid_accommodation_reading, '42a34b80-0106-4e6d-ae2d-18b33d8c0343').
narrative_ontology:cs_kernel_codification('42a34b80-0106-4e6d-ae2d-18b33d8c0343', fixed_text).
narrative_ontology:cs_authority_grounding('42a34b80-0106-4e6d-ae2d-18b33d8c0343', lineage).
narrative_ontology:cs_interpretation_layer_present('42a34b80-0106-4e6d-ae2d-18b33d8c0343').
narrative_ontology:cs_reading_relation('42a34b80-0106-4e6d-ae2d-18b33d8c0343', family_law_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('42a34b80-0106-4e6d-ae2d-18b33d8c0343', family_law_authority__constitutional_supremacy_reading, coexists_with).
narrative_ontology:cs_axiom('42a34b80-0106-4e6d-ae2d-18b33d8c0343', foundational, dual_legitimate_authority).
narrative_ontology:cs_axiom_status(dual_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('42a34b80-0106-4e6d-ae2d-18b33d8c0343', dual_legitimate_authority, conventional).
narrative_ontology:cs_axiom('42a34b80-0106-4e6d-ae2d-18b33d8c0343', foundational, recognition_without_subordination).
narrative_ontology:cs_axiom_status(recognition_without_subordination, holdable).
narrative_ontology:cs_axiom_grounding('42a34b80-0106-4e6d-ae2d-18b33d8c0343', recognition_without_subordination, deontological).
narrative_ontology:cs_axiom('42a34b80-0106-4e6d-ae2d-18b33d8c0343', secondary, constitutional_floor_supremacy).
narrative_ontology:cs_axiom_status(constitutional_floor_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('42a34b80-0106-4e6d-ae2d-18b33d8c0343', constitutional_floor_supremacy, deontological).
narrative_ontology:cs_reference_frame('42a34b80-0106-4e6d-ae2d-18b33d8c0343', constitutional_pluralism_framework).
narrative_ontology:cs_drift_state('42a34b80-0106-4e6d-ae2d-18b33d8c0343', contemporary_post_codification_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('42a34b80-0106-4e6d-ae2d-18b33d8c0343', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(family_law_authority__hybrid_accommodation_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hybrid_accommodation_reading, constitutional_state).
narrative_ontology:constraint_beneficiary(family_law_authority__hybrid_accommodation_reading, religious_legal_authorities).
narrative_ontology:constraint_victim(family_law_authority__hybrid_accommodation_reading, women_within_communities).
narrative_ontology:constraint_victim(family_law_authority__hybrid_accommodation_reading, religious_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN IN UNCODIFIED COMMUNITIES (SNARE) — Identity-locked within religious tradition and kinship structure. Nominally protected by constitutional floor but enforcement depends on willingness to litigate against community and family; litigation itself triggers social exclusion and identity dissolution. No real exit without leaving the community entirely. Maximum extraction — bears costs of both traditional family law and constitutional ambiguity.
constraint_indexing:constraint_classification(family_law_authority__hybrid_accommodation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: WOMEN IN CODIFIED COMMUNITIES (TANGLED ROPE) — Codified family law (Hindu Succession Act, Special Marriage Act) creates written standards that courts can enforce without invalidating community authority. Genuine coordination function (communities govern marriage, inheritance by tradition) mixed with asymmetric extraction (women's guardianship, inheritance shares, divorce grounds unequal). Exit is costly (social ostracism, economic vulnerability) but visible—can access courts, negotiate reforms within written rules.
constraint_indexing:constraint_classification(family_law_authority__hybrid_accommodation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL STATE (ROPE) — Retains ultimate authority: enforces constitutional floor through judiciary, licenses communities as legal authorities by recognizing their jurisdiction, can revise or withdraw recognition. Experiences the constraint as coordination—delegation to communities reduces enforcement burden, allows cultural legitimacy, avoids constitutional confrontation with entrenched institutions. Net beneficiary through institutional efficiency and political stability.
constraint_indexing:constraint_classification(family_law_authority__hybrid_accommodation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RELIGIOUS AUTHORITIES—CODIFIED (ROPE) — Hindu, Jewish law communities with written codes (Succession Act, Halakha compilations) experience the hybrid as coordination: they retain internal governance authority while the state enforces written rules against them. Extraction is minimal because codification constrains their discretion. Arbitrage option is real—can petition courts to amend interpretation, lobby legislature for legal reform.
constraint_indexing:constraint_classification(family_law_authority__hybrid_accommodation_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: RELIGIOUS AUTHORITIES—UNCODIFIED (TANGLED ROPE) — Communities with uncodified law (some Muslim communities, customary law in minority traditions) experience genuine extraction. The state recognizes their authority over internal practice but refuses to codify and enforce their rules, leaving them in limbo: retain governance authority but lose state enforcement machinery, so must use informal sanctions (social ostracism, economic pressure) to maintain compliance. Coordination function (regulate marriage, divorce, inheritance) compromised by lack of state backing; extraction mechanism (unequal power within community) unregulated.
constraint_indexing:constraint_classification(family_law_authority__hybrid_accommodation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: SECULAR REFORMIST COALITION (TANGLED ROPE) — Organized groups (women's rights NGOs, secular political parties) experience both coordination and extraction. The hybrid accommodation creates pathways for reform—can litigate in constitutional courts, lobby for codification, invoke gender equality clause. But extraction mechanism persists: state defers to community authority, limits reform to those willing to litigate, and leaves women without community status if they violate tradition. Mixed experience: real agency and partial success (some codification, some constitutional victories) alongside embedded extraction.
constraint_indexing:constraint_classification(family_law_authority__hybrid_accommodation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CONSTITUTIONAL COURTS AS REFORM MECHANISM (SCAFFOLD) — Judiciary sees the hybrid as a temporary structure with built-in reform pathway. Constitutional floor enables incremental expansion of women's rights through judicial interpretation (Katz v Ministry of Interior model: expand state enforcement of gender equality into family law domain without formally abolishing community authority). Courts function as the sunset mechanism—each generation of litigation incrementally shifts ground from community deference to rights enforcement. Theater ratio moderate—courts perform respect for tradition while systematically narrowing its scope.
constraint_indexing:constraint_classification(family_law_authority__hybrid_accommodation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: RELIGIOUS COMMUNITY AUTHORITY AS PERFORMATIVE (PITON) — From civilizational distance, the hybrid accommodation's 'respect for community authority' is largely theatrical. The state's enforcement of constitutional floor has already redefined what family law authority means—not autonomous religious governance but state-licensed jurisdiction exercised within constitutional constraints. Religious communities experience the constraint as preservation of ceremonial authority (control over marriage ritual, ceremonial divorce) while substantive authority (property, inheritance, custody) migrates to courts. High theater—the constraint maintains the appearance of community autonomy while structural authority has shifted.
constraint_indexing:constraint_classification(family_law_authority__hybrid_accommodation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER—NATURAL LAW VIEW (MOUNTAIN) — From the perspective of comparative law and sociological observation, legal pluralism (coexistence of state and community law) appears as an immutable feature of all complex societies. No state can fully enforce a unitary legal system; communities always generate internal normative orders. The hybrid accommodation appears as a natural expression of this inevitability—an accommodation to plural sovereignty that is inherent to human social organization. However, this perspective risks naturalizing what is actually a constructed allocation of authority. The mountain classification is a false summit: the apparent inevitability of pluralism conceals the specific institutional choices (which communities get recognized, under what conditions, with what constitutional constraints) that make the plural system work.
constraint_indexing:constraint_classification(family_law_authority__hybrid_accommodation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__hybrid_accommodation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(family_law_authority__hybrid_accommodation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_law_authority__hybrid_accommodation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(family_law_authority__hybrid_accommodation_reading, TR),
    TR >= 0.70.

:- end_tests(family_law_authority__hybrid_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35, rising trajectory 0.22→0.35): Moderate and increasing. The constraint begins with lower extractiveness when state deference to community is strongest (no constitutional scrutiny, informal enforcement). Extractiveness rises as judicial intervention increases—courts begin enforcing constitutional floor, creating new rules that conflict with traditional practice, women gain litigation pathways. The rising trajectory reflects that the hybrid accommodation is unstable: as enforcement capacity grows, the state's real authority becomes visible, and the appearance of shared authority becomes harder to maintain. The codification of some community laws (Hindu law) reduces extractiveness for those communities compared to uncodified ones (some Muslim practices)—ε should be lower for codified but we use aggregate. Suppression (0.48, falling trajectory 0.55→0.48): Moderate-high, declining. Initial suppression is high because women face triple barriers (community authority without state enforcement, lack of codified rules, social ostracism for litigation). Suppression falls as codification increases, courts become more willing to intervene, and women's movements create exit alternatives. Theater ratio (0.58, rising trajectory 0.42→0.58): Moderate-high. The hybrid accommodation increasingly functions as theater: the state performs respect for community authority while courts systematically narrow it; religious authorities perform autonomy while their rules are rewritten by courts; the system preserves the appearance of shared authority while concentrating power in judicial hands. Theater rises as the gap between formal claims and actual authority allocation becomes more visible.
 *
 * PERSPECTIVAL GAP:
 *   The hybrid accommodation reading produces maximum perspectival divergence. From the state's perspective (institutional/arbitrage), the constraint is rope—efficient coordination with cultural legitimacy. From women's perspective in uncodified communities (powerless/identity_locked), it is snare—maximum extraction with no exit. From women's perspective in codified communities (moderate/constrained), it is tangled rope—mixed coordination and extraction. From religious authorities with codified law (institutional/arbitrage), it is rope—they retain core authority. From uncodified religious authorities (organized/constrained), it is tangled rope—they are caught between authority and enforcement. From the secular reform coalition (organized/constrained), it is tangled rope—real reform pathways alongside embedded extraction. From constitutional courts (organized/constrained), it is scaffold—the structure contains seeds of its own transformation through incremental rights expansion. From the civilizational analytical perspective (analytical/analytical), it appears as mountain—plural sovereignty seems inevitable—but this is a false summit revealing the contingency of the specific institutional allocation. The perspectival divergence reflects that the hybrid accommodation is unstable: each institutional actor experiences it differently, and those experiences conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent derives from their structural relationship to this specific constraint. The state as institutional beneficiary with arbitrage options has low d (0.10)—the constraint positions the state as authority with exit option (can enforce floor unilaterally or recognize community autonomy). Codified religious authorities with institutional status and arbitrage options have low-to-moderate d (0.20)—they lose discretion but gain state enforcement. Uncodified religious authorities with organized status and constrained options have higher d (0.45)—they lack enforcement machinery despite retaining governance authority. Women with powerless status and identity_locked exit have maximum d (0.92)—they bear full extraction cost with no structural exit. Women in codified communities with moderate status and constrained options have moderate d (0.62)—they can access courts but face significant costs. The secular reform coalition with organized status and constrained options has moderate d (0.55)—they can litigate but extraction persists. Courts with organized status and constrained options have low d (0.35)—they function as authority but remain bound by constitutional constraints. Each directionality value feeds the sigmoid f(d) to produce experienced extractiveness chi, which explains the perspectival classification gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled_rope is the correct classification when a constraint contains both genuine coordination function and asymmetric extraction. The coordination function is real: delegation to communities reduces enforcement burden, respects cultural diversity, provides legitimate channels for internal norm evolution. The asymmetric extraction is equally real: women's rights depend on willingness to litigate against family and community; religious authorities extract deference; the state retains ultimate veto power. The hybrid accommodation reading cannot collapse into rope (pure coordination) because the extraction is substantial and structural. It cannot collapse into snare (pure extraction) because the coordination function genuinely exists and some agents (state, codified communities) experience it as such. The tangled rope classification is stable across the 20-year interval, indicating the constraint has not resolved toward pure extraction or pure coordination. The rising theater ratio suggests the constraint may eventually drift toward piton if the coordination function becomes purely performative—but the current measurement shows genuine functional content. The reading's axioms (dual legitimate authority, recognition-without-subordination) claim that the constraint is legitimately mixed, not a stepping stone to supremacy. If the axiom 'recognition_without_subordination' is overridden (if courts begin treating state authority as primary and community authority as subordinate), the constraint may reclassify toward snare for most agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    codification_enforcement_gap,
    'Does codification of family law create genuine equal protection under state courts, or does it merely formalize pre-existing community hierarchy under the appearance of legality?',
    'Comparative litigation analysis: same-rule provisions across codified vs uncodified communities; outcome analysis by gender, community status, wealth; interview data from women navigating formal vs informal family law processes',
    'If codification creates equal protection: tangled_rope classification holds—genuine coordination function mixed with extraction. If codification formalizes hierarchy: classification shifts toward snare—the state becomes complicit in enforcing community hierarchy. Affects reform pathway analysis (courts as real vs performative reform mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(codification_enforcement_gap, empirical, 'Whether codification creates genuine equal protection or formalizes community hierarchy').

omega_variable(
    constitutional_floor_enforcement_capacity,
    'How much extractive authority can a constitutional floor actually constrain when enforcement depends on willingness to litigate against family and community?',
    'Litigation rate analysis by community and gender; post-litigation social outcome data; comparison of enforcement rates for constitutional rights in family law vs other domains',
    'If enforcement capacity is high: suppression metric should be lower (0.35 actual), constitutional floor is functional. If capacity is low: suppression metric should be higher (0.60+), floor is performative, constraint is closer to snare for most agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_floor_enforcement_capacity, empirical, 'Actual enforcement capacity of constitutional floor in family law domain').

omega_variable(
    reading_foreclosure_test,
    'Can the hybrid accommodation framework hold the communal_autonomy reading''s position (community authority without constitutional constraint) OR the constitutional_supremacy reading''s position (unitary state law) without internal contradiction?',
    'Logical analysis: does accepting hybrid accommodation''s axioms (state floor + community governance as legitimate dual authority) require rejecting the core premises of the other readings? If yes: foreclosure relation. If both readings remain live in public discourse despite the hybrid framework: coexistence relation.',
    'If foreclosure: the hybrid reading has foreclosed alternatives—stronger canonical claim. If coexistence: readings are perspectival variations, not competing universals—weaker claim, more contingent. Affects interpretation of kernel_codification (whether the hybrid is a genuine synthesis or a perpetual contested compromise).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether hybrid accommodation logically forecloses or coexists with sibling readings').

omega_variable(
    gender_equality_versus_cultural_continuity_axis,
    'Is the apparent tension between gender equality enforcement and cultural community autonomy a genuine structural trade-off, or does it reflect false dichotomy?',
    'Case study analysis of communities that have reformed family law while preserving cultural identity (Jewish communities with egalitarian Halakha, Hindu law codification, Islamic jurisprudence evolution); identification of actual vs rhetorical incompatibility',
    'If genuine trade-off: beneficiary extraction for religious authorities is real (they must choose between cultural continuity and gender equality). If false dichotomy: extraction is performative (communities can reform internally without state intervention). Affects which agents are truly beneficiaries vs which merely claim beneficiary status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equality_versus_cultural_continuity_axis, conceptual, 'Whether gender equality and cultural continuity are genuinely in tension or represent false dichotomy').

omega_variable(
    reading_kernel_ambiguity,
    'Is the family law authority kernel itself a genuine constitutional principle that can be read multiple ways, or is the kernel ambiguity itself manufactured to delay clarity on women''s rights?',
    'Historical analysis of constitutional framing (intent of drafters, early interpretations); discourse analysis of legal argumentation (how often kernel ambiguity is invoked as justification for delay); comparison with other plural-law contexts (indigenous law, customary law) to establish baseline ambiguity',
    'If genuine kernel ambiguity: the hybrid reading is a legitimate constitutional interpretation. If manufactured: the hybrid reading is a false consensus that actually favors traditional authorities. Affects axiom status (holdable vs overridden) and reading_relations classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether kernel ambiguity is genuine or manufactured').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hybrid_accommodation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flaw_hybrid_tr_t0, family_law_authority__hybrid_accommodation_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(flaw_hybrid_tr_t10, family_law_authority__hybrid_accommodation_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(flaw_hybrid_tr_t20, family_law_authority__hybrid_accommodation_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(flaw_hybrid_be_t0, family_law_authority__hybrid_accommodation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(flaw_hybrid_be_t10, family_law_authority__hybrid_accommodation_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(flaw_hybrid_be_t20, family_law_authority__hybrid_accommodation_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(flaw_hybrid_su_t0, family_law_authority__hybrid_accommodation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(flaw_hybrid_su_t10, family_law_authority__hybrid_accommodation_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement(flaw_hybrid_su_t20, family_law_authority__hybrid_accommodation_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hybrid_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__hybrid_accommodation_reading, family_law_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(family_law_authority__hybrid_accommodation_reading, family_law_authority__constitutional_supremacy_reading).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel decomposes into three structurally distinct constraint stories, one for each reading. Each reading has its own ε value, beneficiary/victim structure, and perspectival gap. The hybrid_accommodation_reading (ε=0.35, tangled_rope) differs from communal_autonomy_reading (expected ε~0.15-0.25, rope or mountain) and constitutional_supremacy_reading (expected ε~0.40-0.50, tangled_rope or snare). The readings are related not via network affects_constraints (which tracks causal dependency) but via shared kernel. The network edge indicates that changes to this reading's classification conditions (judicial reinterpretation, legislative codification, social evolution) affect the other readings' epistemic standing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__hybrid_accommodation_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
