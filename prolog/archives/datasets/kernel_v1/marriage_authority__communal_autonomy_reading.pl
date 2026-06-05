% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__communal_autonomy_reading, []).

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
 *   constraint_id: marriage_authority__communal_autonomy_reading
 *   human_readable: Marriage Authority Grounded in Community Religious Tradition (Communal Autonomy Reading)
 *   domain: constitutional_law/legal_pluralism/family_law
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of a contested kernel: marriage
 *   authority. The kernel itself is ambiguous — multiple coherent frameworks
 *   claim legitimate authority over marriage norms. This story represents the
 *   COMMUNAL-AUTONOMY READING: the claim that marriage authority derives from
 *   community religious tradition, that state's role is to enforce (not
 *   author) those norms, and that variation in family law across traditions
 *   reflects legitimate pluralism rather than legal fragmentation. Under this
 *   reading, religious leadership institutions are primary beneficiaries
 *   (they retain authority and receive state enforcement support);
 *   intra-community dissenters and women facing exit barriers are victims
 *   (they cannot exit without dissolving community identity or facing
 *   material harm). The state adopts an arbitrage position: it enforces
 *   personal law variation while avoiding direct political conflict over
 *   family norms. This reading coexists — without logical foreclosure — with
 *   competing readings: the SECULARIST READING (marriage authority derives
 *   from state law alone, tradition is private choice with no coercive
 *   force), the GENDER-RIGHTS READING (marriage authority must secure equal
 *   rights regardless of tradition), the FEDERALIST-MILLET READING (state
 *   recognition of religious authority is conditional on minority protection
 *   and formal consent), and the JUDICIAL-HARMONIZATION READING (courts
 *   actively mediate between tradition and rights, case-by-case). The
 *   extractiveness trajectory shows rising ε over the interval (0.38 → 0.48),
 *   driven partly by accumulating institutional recognition of personal law
 *   variation (legitimizing extraction mechanism) and partly by rising
 *   dissent visibility (making extraction more salient as norms that once
 *   seemed natural appear increasingly contested). The constraint is
 *   classified as Tangled Rope at the analytical level because it embeds
 *   genuine coordination (community members do receive marital framework,
 *   ritual, meaning) alongside asymmetric extraction (dissenters have
 *   identity-locked exit; women face material barriers). The false-summit
 *   risk is high: this reading risks naturalizing asymmetric extraction as
 *   cultural or religious inevitability when in fact the distribution of exit
 *   costs reflects institutional choices (which traditions the state
 *   recognizes, which family law norms it enforces).
 *
 * KEY AGENTS:
 *   - Religious Leadership Institutions: Primary beneficiary (organized/arbitrage) — retain marriage authority and receive state enforcement support. Exit capacity is high (can migrate to other jurisdictions or parallel dispute-resolution). Experience constraint as pure coordination.
 *   - Intra-Community Dissenters: Primary victim (powerless/identity_locked) — structurally mobile within wider state but identity-locked within community. Exit from marriage norms means exit from identity, kinship, belonging. Cannot perceive constraint as changeable at biographical horizon.
 *   - Women Seeking Exit: Primary victim (powerless/trapped) — face material barriers to divorce, property rights, custody under religious family law. Some barriers persist across all time horizons (biographical and generational).
 *   - Mixed-Faith Couples: Secondary victim (moderate/constrained) — legal pluralism permits forum-shopping but creates exit costs (social disapproval, transaction costs of regime-switching). Experience constraint as tangled (genuine coordination in shared marital framework, asymmetric extraction in unequal treatment).
 *   - State Authority: Beneficiary and arbitrageur (institutional/arbitrage) — delegates marriage authority to religious institutions while retaining coercive capacity. Avoids political conflict by not authoring norms while enforcing pluralist framework.
 *   - Universalist Rights Doctrine: Institutional observer (organized/arbitrage) — maintains nominal adherence to equal family law standards globally while the actual governance remains community-controlled. Theater ratio rising (0.38 → 0.52) reflects increasing disconnect between rights language and structural authority.
 *   - Analytical Observer: Perspective collector (analytical/analytical) — perceives the constraint as a reading of a contested kernel, coexisting with siblings without logical foreclosure but generating persistent structural conflict.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.48).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.58).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Marriage Authority Grounded in Community Religious Tradition (Communal Autonomy Reading)").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "constitutional_law/legal_pluralism/family_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, '49e4b82e-d211-4550-9bd5-52d376413445').
narrative_ontology:cs_kernel_codification('49e4b82e-d211-4550-9bd5-52d376413445', formalized).
narrative_ontology:cs_authority_grounding('49e4b82e-d211-4550-9bd5-52d376413445', lineage).
narrative_ontology:cs_interpretation_layer_present('49e4b82e-d211-4550-9bd5-52d376413445').
narrative_ontology:cs_reading_relation('49e4b82e-d211-4550-9bd5-52d376413445', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('49e4b82e-d211-4550-9bd5-52d376413445', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('49e4b82e-d211-4550-9bd5-52d376413445', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('49e4b82e-d211-4550-9bd5-52d376413445', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('49e4b82e-d211-4550-9bd5-52d376413445', foundational, community_tradition_legitimate_marital_authority).
narrative_ontology:cs_axiom_status(community_tradition_legitimate_marital_authority, holdable).
narrative_ontology:cs_axiom_grounding('49e4b82e-d211-4550-9bd5-52d376413445', community_tradition_legitimate_marital_authority, deontological).
narrative_ontology:cs_axiom('49e4b82e-d211-4550-9bd5-52d376413445', foundational, state_enforcement_neutral_delegation).
narrative_ontology:cs_axiom_status(state_enforcement_neutral_delegation, holdable).
narrative_ontology:cs_axiom_grounding('49e4b82e-d211-4550-9bd5-52d376413445', state_enforcement_neutral_delegation, conventional).
narrative_ontology:cs_reference_frame('49e4b82e-d211-4550-9bd5-52d376413445', traditional_religious_family_authority).
narrative_ontology:cs_drift_state('49e4b82e-d211-4550-9bd5-52d376413445', contemporary_pluralist_states_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('49e4b82e-d211-4550-9bd5-52d376413445', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_leadership_institutions).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, tradition_adhering_majorities).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, mixed_faith_couples).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, women_seeking_exit).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTRA-COMMUNITY DISSENTER (SNARE) — Agent is structurally mobile within the wider state (mobile to constrained exit capacity) but identity-locked within the community. Exit from marriage norms controlled by religious authority means exit from community identity, kinship, and social belonging. Dissenter perceives the constraint as unchangeable at biographical horizon because changing it would require becoming a different person — not because barriers are material but because identity is constituted through the community tradition. Maximum effective extraction because the binding is cognitive and internalized.
constraint_indexing:constraint_classification(marriage_authority__communal_autonomy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: WOMEN SEEKING EXIT (SNARE) — At biographical horizon, faces material barriers: divorce under religious law may require husband's consent or community approval; property rights may vest in family rather than individual; children's custody may be controlled by religious authority. Trapped exit (material barriers) combines with regional scope and powerless power to produce maximum suppression. At generational horizon, potential for legislative reform exists, but within biographical lifetime the constraint is structurally immutable.
constraint_indexing:constraint_classification(marriage_authority__communal_autonomy_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: MIXED-FAITH COUPLE (TANGLED ROPE) — Faces constrained exit: legal pluralism permits choice of family law regime, but switching regimes carries social costs (community disapproval, relational rupture) and transaction costs (renegotiating marital contract, property reallocation). Genuine coordination function exists (religious tradition provides shared meaning, ritual, community support). Asymmetric extraction: one partner's tradition may dominate; state enforcement of choice creates exit cost. Not fully trapped (state permits forum-shopping) but not fully mobile (costs are substantial).
constraint_indexing:constraint_classification(marriage_authority__communal_autonomy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: RELIGIOUS LEADERSHIP INSTITUTIONS (ROPE) — Primary beneficiaries (organized/arbitrage). Control marriage authority and adjudicate family law within their tradition. Exit capacity is high: if state withdrew enforcement, religious institutions could migrate to other jurisdictions or parallel dispute-resolution mechanisms (arbitration, community courts). Experience the constraint as pure coordination of their tradition's normative order. No experienced extraction — the tradition defines legitimate family formation for adherents. Net beneficiary through institutional autonomy and member compliance.
constraint_indexing:constraint_classification(marriage_authority__communal_autonomy_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: STATE AUTHORITY (ROPE) — Institutional actor enforcing personal law variation; exercises arbitrage: delegates marriage authority to religious institutions while retaining jurisdiction over inter-community disputes and enforcement. Experiences constraint as coordination mechanism: legal pluralism enables the state to avoid enforcing controversial family law norms directly while maintaining social stability. State benefits from institutional division of labor (religious authorities bear political costs of enforcement; state retains coercive capacity).
constraint_indexing:constraint_classification(marriage_authority__communal_autonomy_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: UNIVERSALIST RIGHTS DOCTRINE (PITON) — Global human rights norms declare universal family law standards (equal marriage, equal divorce, property equality). Legal pluralism in this constraint represents theatrical compliance with rights doctrine: states ratify international conventions while permitting personal law regimes that contradict them. Universalist doctrine maintains nominally but has lost functional power — the actual governance remains community-controlled. Theater ratio: high (0.52+) because rights language is invoked while structural authority remains decentralized. Classification as piton reflects institutional inertia: the doctrine persists through international law machinery despite atrophied functional role.
constraint_indexing:constraint_classification(marriage_authority__communal_autonomy_reading, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, this constraint embeds genuine coordination (religious tradition provides shared marital framework for adherents) AND asymmetric extraction (community dissenters have no exit; women may be trapped; mixed-faith couples face forum costs). The constraint's legitimacy rests on framing community autonomy as natural right — but this frames asymmetric extraction as acceptable. Committer frame shows this reading coexists with competing readings (secularist, gender-rights) that interpret the same kernel (marriage authority) entirely differently.
constraint_indexing:constraint_classification(marriage_authority__communal_autonomy_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_authority__communal_autonomy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_authority__communal_autonomy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, TR),
    TR >= 0.70.

:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. The reading frames extraction as authority asymmetry — religious leadership controls marriage norms and state enforces those norms against dissenters. But the reading avoids measuring this as pure snare because genuine coordination exists (willing adherents receive shared marital framework). The trajectory rising from 0.38 to 0.48 reflects accumulating institutional legitimacy for personal law variation — as the state codifies and regularizes recognition of religious marriage, the extraction mechanism becomes more institutionalized and salient. The value 0.48 sits at the tangled-rope floor (χ boundary at 0.40–0.90 range), indicating this is a borderline case where some perspectives see snare and others see rope. Suppression (0.58): Moderate-high. Structural barriers to exit exist (material: legal barriers to divorce, property rights, guardianship; psychological: identity lock for dissenters; social: community disapproval of norm violation). But suppression is not total — the wider state offers formal exit channels (civil courts, secular marriage option) even if using them carries costs. Theater ratio (0.52): Moderate. The constraint shows increasing theatricality: rights doctrine claims universalism while practice remains pluralist; state claims neutrality while enforcing delegation; tradition claims to represent community consent while dissenters report coercion. The rising trajectory (0.38 → 0.52) reflects growing visibility of the gap between legitimating rhetoric and structural reality.
 *
 * PERSPECTIVAL GAP:
 *   The perspective distribution shows the full range of indexical classification. Religious leadership sees pure coordination (Rope) — the constraint solves the problem of organizing marital life within their tradition. Mixed-faith couples see tangled coordination and extraction (Tangled Rope) — genuine shared meaning alongside forum costs. State authority sees coordination (Rope) — division of labor with private institutions. Intra-community dissenters and women seeking exit see snares — they perceive the constraint as unchangeable and extractive. Universalist rights doctrine sees performative compliance (Piton) — nominal adherence to equal law standards with atrophied actual function. The analytical observer sees the full tangled structure: coordination + extraction + competing kernel readings. The perspectival gap reveals that what religious leadership experiences as legitimate authority appears to dissenters as coercive extraction. The gap itself is the diagnostic signal that the constraint is tangled, not pure coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's power level, exit options, and structural relationship to authority. Religious leadership (organized/arbitrage) has low d (~0.20) because they are primary beneficiaries with high exit capacity — they experience the constraint as net-beneficial. Dissenters (powerless/identity_locked) have high d (~0.89) because they are trapped by internalized identity fusion, making exit psychologically impossible despite theoretical state-level exit options. Women (powerless/trapped) have high d (~0.95) because they face both material barriers and cultural suppression. Mixed-faith couples (moderate/constrained) have moderate-high d (~0.65) because they face surmountable but substantial exit costs. The state (institutional/arbitrage) has low-moderate d (~0.30) because they are secondary beneficiary (avoid political conflict) with arbitrage options (can shift pluralist framework). Universalist doctrine (organized/arbitrage) has moderate-high d (~0.75) in analytical frame because its actual enforcement power has eroded while its nominal commitments remain, creating a gap between declared and effective authority. Derived d values feed the sigmoid f(d) function to compute effective extractiveness chi for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by acknowledging it as a READING — a coherent interpretation of a contested kernel that coexists (without foreclosure) with siblings. The ambiguity is not resolvable within the committer frame because the kernel (marriage authority) has no intrinsic ground — it is a stabilized institutional commitment that different authority structures defend differently. From the communal-autonomy reading's perspective: authority IS legitimately rooted in tradition, pluralism IS the right institutional arrangement, and dissenters SHOULD defer to community norms (or exit to other communities). From the gender-rights reading's perspective: authority MUST be rooted in equal rights, pluralism ENABLES gender extraction, and dissenters (especially women) require protective intervention. Both readings are internally coherent, but they cannot both be true within a single institutional framework. The engine cannot resolve this mandatrophy — it should expose it. The constraint's classification as Tangled Rope at the analytical level captures the irreducible tension: genuine coordination (community coherence) coexists with asymmetric extraction (dissenters trapped, women facing barriers). This tension IS the insight the framework provides: the reading naturalizes extraction as legitimate autonomy, but the structural data shows that extraction is real and asymmetric. Whether the extraction is justifiable is not a constraint-classification question — it is a political and normative question that the reading presupposes an answer to.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_vs_tradition_grounding,
    'Does this reading''s claim that ''community religious tradition provides legitimate marriage authority'' rest on explicit consent from all affected parties, or on inherited/ascribed community membership?',
    'Historical analysis of adherent conversion/entrance: proportion joining by individual choice vs. birth/family assignment. Exit surveys: percentage who remain in tradition by choice vs. structural inertia. Dissenter interviews: perceived voluntariness of consent to authority.',
    'If explicit consent: the reading''s legitimacy claim holds (contractual autonomy). If inherited: the reading risks collapsing into extraction mechanism (powerless agents born into tradition have no initial choice). The committer frame presupposes this ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_vs_tradition_grounding, empirical, 'Whether community membership and consent to authority is voluntary or inherited').

omega_variable(
    intra_community_dissent_visibility,
    'What proportion of tradition adherents actually dissent from marriage norms but remain within the community due to identity lock, family pressure, or material dependency?',
    'Ethnographic study of intra-community negotiation; interview data on exit contemplation; survey of private vs. public compliance (do adherents follow norms when anonymity permits deviation?). Divorce rate analysis: formal vs. informal separation (indicating norms are perceived as binding despite private wish to exit).',
    'If dissent is low (<15%): the extraction reading understates actual coordination. If dissent is high (>40%): the constraint is functioning primarily as extraction mechanism with performative coordination language. Affects whether tangled_rope classification underestimates snare component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intra_community_dissent_visibility, empirical, 'Prevalence of intra-community dissent regarding marriage norms').

omega_variable(
    competing_kernel_reading_structure,
    'Which sibling reading (secularist, gender-rights, federalist-millet, judicial-harmonization) would a court adopt if the kernel (marriage authority) were directly adjudicated?',
    'Comparative law analysis: jurisdictions that have explicitly chosen between readings (India, Egypt, Malaysia, Lebanon) show which reading becomes formalized. Historical trajectory: which reading is gaining institutional support? Which is losing ground?',
    'The committer frame presupposes that this (communal-autonomy) reading coexists with siblings without logical foreclosure. If courts consistently adopt a different reading, this one may be functionally foreclosed in the state''s official doctrine (even if retained in practice). Affects whether reading_relations should shift from coexists_with to influences (or subordinated_by).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_kernel_reading_structure, conceptual, 'Which reading of marriage authority becomes institutionalized when directly adjudicated').

omega_variable(
    extractiveness_measurement_observable,
    'Does the extractiveness value (0.48) measure authority asymmetry within the community, or does it measure the state''s enforcement burden? These are different observables.',
    'Decompose extractiveness into component observables: (a) authority concentration within religious leadership (high), (b) state enforcement costs (moderate), (c) exit costs for dissenters (high), (d) coordination benefits to willing adherents (moderate). Weight by salience for THIS reading''s framing. If reading emphasizes community autonomy, weight (a) and (d); if reading emphasizes state''s pluralist policy, weight (b).',
    'If measuring community internal authority: ε should be higher (~0.62). If measuring state''s enforcement load: ε should be lower (~0.35). Current 0.48 averages both. Classification would change if observables were disambiguated. This is the ε-invariance test: different observables yield different ε → different constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_measurement_observable, conceptual, 'Which observable grounds the extractiveness measure — community authority concentration or state enforcement load').

omega_variable(
    gender_axis_independence,
    'Is the gender-inequality component of this constraint (women''s unequal divorce, property, custody rights) intrinsic to the communal-autonomy reading, or a separable structural feature?',
    'Gender-neutral variant comparison: could this constraint instantiate communal autonomy with gender-equal norms (some religious traditions do permit equal divorce, property, guardianship)? If yes: gender is separable → write a separate constraint story (marriage_gender_equality__communal_autonomy variant) with lower ε and different victims. If no: gender hierarchy is constitutive → gender is intrinsic to THIS reading''s structure.',
    'If separable: the victim set should exclude ''women_seeking_exit'' and focus on ''intra_community_dissenters'' (gender-neutral). If intrinsic: the reading naturalizes gender extraction as part of tradition. The committer frame may presuppose this ambiguity intentionally — leaving unresolved whether communal autonomy requires gender hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_axis_independence, conceptual, 'Whether gender inequality is intrinsic or separable from this reading''s communal-autonomy structure').

omega_variable(
    state_enforcement_paradox,
    'This reading claims state ''enforces but does not author'' family law norms. But state enforcement of religious authority IS a form of authorship — the state is authoring a delegation, choosing which traditions to recognize. Does this reading coherently distinguish enforcement from authorship?',
    'Doctrinal analysis: how do courts in legal pluralist jurisdictions frame the distinction? Do they treat state recognition of religious marriage as purely procedural (neutral enforcement) or as substantive (authoring which norms have binding force)? Counterfactual: if the state withdrew recognition, would the norms persist with equal force?',
    'If the distinction collapses: this reading is understating state authority. The constraint should classify as closer to institutional/powerful rather than organized/arbitrage. If the distinction holds: it requires that religious authority have independent legitimacy source. The committer frame assumes the distinction holds, but this is contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_enforcement_paradox, conceptual, 'Whether state enforcement of religious authority can be coherently distinguished from authorship').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ma_comm_auto_tr_t0, marriage_authority__communal_autonomy_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ma_comm_auto_tr_t10, marriage_authority__communal_autonomy_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(ma_comm_auto_tr_t20, marriage_authority__communal_autonomy_reading, theater_ratio, 20, 0.52).

% Extraction over time
narrative_ontology:measurement(ma_comm_auto_be_t0, marriage_authority__communal_autonomy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ma_comm_auto_be_t10, marriage_authority__communal_autonomy_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(ma_comm_auto_be_t20, marriage_authority__communal_autonomy_reading, base_extractiveness, 20, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(ma_comm_auto_su_t0, marriage_authority__communal_autonomy_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(ma_comm_auto_su_t10, marriage_authority__communal_autonomy_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(ma_comm_auto_su_t20, marriage_authority__communal_autonomy_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a shared kernel (marriage_authority). All five readings — communal_autonomy, secularist, gender_rights, federalist_millet, judicial_harmonization — are separate constraint stories because each interprets the kernel differently and produces different structural relationships. They form a constraint family linked by network edges. Each reading has its own ε (extractiveness measured from that reading's framing), its own beneficiary/victim set, and its own terminal type. The committer frame shows that no single reading is logically necessary; all coexist without foreclosure in the presheaf over the kernel's meaning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__communal_autonomy_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
