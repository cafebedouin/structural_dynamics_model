% ============================================================================
% CONSTRAINT STORY: secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secular_civil_reading, []).

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
 *   constraint_id: secular_civil_reading
 *   human_readable: Secular Civil Marriage Authority (Special Marriage Act 1954)
 *   domain: constitutional_law/family_law/religious_governance
 *
 * SUMMARY:
 *   The secular civil reading of marriage authority in India derives
 *   legitimacy from constitutional individual rights (Articles 14, 15, 21:
 *   equality, non-discrimination, life and liberty) rather than from
 *   religious law traditions. The Special Marriage Act (1954) enables any two
 *   consenting adults to marry outside their respective religious
 *   communities, enforcing uniform civil code principles: no religious
 *   authority required, equal property and succession rights regardless of
 *   gender, secular grounds for divorce. This constraint is ONE reading of
 *   the marriage authority kernel — a contested foundational claim about who
 *   legitimately adjudicates marriage. The secular civil reading coexists
 *   with Hindu codified law (Hindu Marriage Act 1955), Muslim personal law
 *   (Shariat courts), Christian canon law (Christian Marriage Act), and Parsi
 *   communal law (Parsi Marriage and Divorce Act), each with its own
 *   constitutional grounding in the right to practice religion (Article 25).
 *   The secular civil reading's authority structure is doubly grounded: in
 *   individual rights (Articles 14, 15, 21) AND in the constitutional right
 *   to practice religion (Article 25), creating a latent logical tension that
 *   shapes the entire constraint. The extraction mechanism operates through
 *   jurisdictional displacement: the secular civil reading claims authority
 *   over marriage formation and dissolution, collecting adjudicatory power,
 *   filing fees, and legitimacy that would otherwise flow to religious
 *   communities. The coordination function is real — the civil code enables
 *   inter-religious marriage and provides uniform property/succession law
 *   that markets and inheritance systems require. The suppression mechanism
 *   is multi-layered: social cost of choosing civil marriage over communal
 *   law (family exclusion, community identity loss), state enforcement
 *   through civil courts (which have greater capacity than communal
 *   adjudicatory bodies), and asymmetric institutional power (state capacity
 *   vastly exceeds community capacity to resist). The theater ratio reflects
 *   that civil marriage registration has both genuine coordination function
 *   (property rights, succession clarity) and performative component (civil
 *   courts presenting themselves as the neutral arbiter of constitutional
 *   values when they are actually one reading of a contested kernel).
 *
 * KEY AGENTS:
 *   - Inter-religious couples: Primary beneficiary (moderate/mobile) — cannot marry under any single community law; secular civil reading enables their coordination
 *   - Community-embedded believers: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused with communal jurisdiction; exit into civil marriage means exit from religious identity as lived
 *   - Women exercising exit rights: Secondary beneficiary (moderate/constrained) — civil law provides exit from communal law constraints (divorce on equal grounds, property rights); faces social costs
 *   - Religious community authority structures: Institutional victim (institutional/constrained) — lose jurisdictional authority and fee collection; constrained by inability to prevent legal exit
 *   - Civil court bureaucracy: Institutional beneficiary (institutional/arbitrage) — collects jurisdictional authority, case volume, and legitimacy; low exit costs (state-backed)
 *   - Constitutional reform coalition: Organized agents (organized/mobile) — women's rights groups, inter-faith organizations; see civil code as transitional scaffold with sunset in reformed communal law
 *   - State secular apparatus: Institutional actor (institutional/arbitrage) — enforces civil code authority; backed by state capacity; low constraints on action
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secular_civil_reading, 0.35).
domain_priors:suppression_score(secular_civil_reading, 0.45).
domain_priors:theater_ratio(secular_civil_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secular_civil_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(secular_civil_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(secular_civil_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secular_civil_reading, tangled_rope).
narrative_ontology:human_readable(secular_civil_reading, "Secular Civil Marriage Authority (Special Marriage Act 1954)").
narrative_ontology:topic_domain(secular_civil_reading, "constitutional_law/family_law/religious_governance").

domain_priors:requires_active_enforcement(secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secular_civil_reading, 'b15d3c22-9b6b-42e6-b5b6-bc3674672df7').
narrative_ontology:cs_kernel_codification('b15d3c22-9b6b-42e6-b5b6-bc3674672df7', formalized).
narrative_ontology:cs_authority_grounding('b15d3c22-9b6b-42e6-b5b6-bc3674672df7', lineage).
narrative_ontology:cs_interpretation_layer_present('b15d3c22-9b6b-42e6-b5b6-bc3674672df7').
narrative_ontology:cs_reading_relation('b15d3c22-9b6b-42e6-b5b6-bc3674672df7', secular_civil_reading__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('b15d3c22-9b6b-42e6-b5b6-bc3674672df7', secular_civil_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('b15d3c22-9b6b-42e6-b5b6-bc3674672df7', secular_civil_reading__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('b15d3c22-9b6b-42e6-b5b6-bc3674672df7', secular_civil_reading__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('b15d3c22-9b6b-42e6-b5b6-bc3674672df7', foundational, individual_rights_prior_to_community).
narrative_ontology:cs_axiom_status(individual_rights_prior_to_community, holdable).
narrative_ontology:cs_axiom_grounding('b15d3c22-9b6b-42e6-b5b6-bc3674672df7', individual_rights_prior_to_community, deontological).
narrative_ontology:cs_axiom('b15d3c22-9b6b-42e6-b5b6-bc3674672df7', foundational, state_secular_competence_marriage_adjudication).
narrative_ontology:cs_axiom_status(state_secular_competence_marriage_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('b15d3c22-9b6b-42e6-b5b6-bc3674672df7', state_secular_competence_marriage_adjudication, instrumental).
narrative_ontology:cs_reference_frame('b15d3c22-9b6b-42e6-b5b6-bc3674672df7', individual_constitutional_rights_supremacy).
narrative_ontology:cs_drift_state('b15d3c22-9b6b-42e6-b5b6-bc3674672df7', contemporary_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b15d3c22-9b6b-42e6-b5b6-bc3674672df7', '').
narrative_ontology:cs_kernel_id(secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secular_civil_reading, inter_religious_couples).
narrative_ontology:constraint_beneficiary(secular_civil_reading, women_exercising_exit_rights).
narrative_ontology:constraint_beneficiary(secular_civil_reading, civil_court_institutional_authority).
narrative_ontology:constraint_victim(secular_civil_reading, religious_community_jurisdiction).
narrative_ontology:constraint_victim(secular_civil_reading, individuals_bound_to_communal_law).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMUNITY-EMBEDDED BELIEVER (SNARE) — Identity constituted through religious community law; structurally mobile (can legally choose civil marriage) but identity-fused with communal jurisdiction. Exit from community law means exit from religious identity as lived within family. Experiences the secular civil reading as displacement, not choice. The binding mechanism is cognitive (identity fusion) not material (legal barriers are actually removed). This perspective instantiates identity_locked: the agent could technically exercise civil marriage rights but cannot imagine doing so because it would require becoming a different person.
constraint_indexing:constraint_classification(secular_civil_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: INTER-RELIGIOUS COUPLE (ROPE) — Primary beneficiary of civil marriage option. No single communal law accommodates their union; secular civil code enables coordination that would be impossible under any single religious system. Exit options are genuine and accessible. Low perceived extraction because the constraint solves their actual coordination problem. Both parties benefit symmetrically — no asymmetric flow. Theater is minimal; the civil registration provides real function (legal recognition, property rights, inheritance clarity).
constraint_indexing:constraint_classification(secular_civil_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: RELIGIOUS COMMUNITY AUTHORITY (TANGLED ROPE) — Experiences the secular civil reading as displacement of traditional jurisdiction. Gains from civil code's enforcement mechanisms (property registration, inheritance clarity enforce communal contracts more reliably) but loses authority over marriage contract formation and dissolution. Constrained: could resist through social enforcement but cannot prevent legal exit. Coordination function: civil code enables larger-scale coordination (property markets, inheritance law, succession planning) that requires secular adjudication. Extraction mechanism: civil courts collect jurisdictional authority and fees; religious communities lose rents from marriage adjudication. Asymmetric enforcement burden: religious communities must enforce their own law through social means while civil courts have state backing.
constraint_indexing:constraint_classification(secular_civil_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM COALITION (SCAFFOLD) — Organized actors (women's rights groups, legal reformers, inter-faith organizations) see civil marriage as transitional: a temporary coordinate system enabling exit from communal law until communal laws themselves reform to guarantee gender equity and freedom of choice. Sunset logic: as Hindu, Muslim, Christian, and Parsi community law frameworks internalize constitutional rights standards (gender equity, freedom of conscience), the civil code's scaffolding function becomes less necessary. Organized agents with clear exit strategy and perceived finite duration. Theater is moderate — civil code has both functional and performative components (genuine property protection alongside symbolic commitment to secular authority).
constraint_indexing:constraint_classification(secular_civil_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL COURT BUREAUCRACY (PITON) — Institutional actor maintaining the secular civil reading as settled law despite ongoing jurisdictional contests. Theater ratio high: civil courts are invested in appearing as the legitimate venue for marriage authority, but their functional superiority (gender equity, inter-religious accommodation) is contested by communities that see themselves as the legitimate authority. The bureaucratic structure persists through state institutional inertia, fee collection, and case volume rather than because all parties accept the secular civil reading as obviously correct. Theater-to-function ratio reflects that a significant portion of the civil court's claim to marriage authority is performative maintenance of jurisdiction rather than solving unique coordination problems that only civil courts can solve.
constraint_indexing:constraint_classification(secular_civil_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, individual rights to freedom of conscience, marriage choice, and equal legal status are presented as natural law: pre-political, self-evident, and inhering to persons independent of community membership. Secular civil authority is treated as the neutral arbiter enforcing these natural rights. However, the structural data contradicts the mountain classification: the secular civil reading is one contingent institutional reading of marriage authority, not a law of nature. Beneficiaries exist (inter-religious couples, women exercising exit rights). The engine will identify this as a false summit, revealing that the 'individual rights are self-evident' framing naturalizes what is actually a specific constitutional choice.
constraint_indexing:constraint_classification(secular_civil_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secular_civil_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(secular_civil_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(secular_civil_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(secular_civil_reading, TR),
    TR >= 0.70.

:- end_tests(secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The secular civil reading extracts jurisdictional authority from religious communities, but the extraction is not pure — genuine coordination benefits accrue (inter-religious marriage enabled, property rights standardized). The secular civil reading creates value, not merely captures existing value. Suppression (0.45): Moderate. Social costs of choosing civil marriage (family exclusion, community identity loss) are significant but not insurmountable; legal barriers are removed (Special Marriage Act explicitly enables civil marriage). State enforcement capacity is asymmetric (civil courts have state backing vs community adjudication relies on voluntary compliance). Theater ratio (0.38): Moderate-low. Civil marriage registration has genuine function (property registration, succession clarity), but the secular civil reading's claim to represent neutral constitutional values is performative — it is one reading of a contested kernel, not the objective truth of constitutional meaning. Civil courts present themselves as above sectarian interest while actually enforcing one specific reading of Article 25 (right to practice religion) that interprets it as compatible with civil marriage authority.
 *
 * PERSPECTIVAL GAP:
 *   The semantic and structural gap between perspectives is the gap between the secular civil reading and alternative readings of the marriage authority kernel. From the inter-religious couple's view, the reading solves a genuine coordination problem (rope). From the community-embedded believer's view, the reading is pure displacement of legitimate authority (snare from their perspective, though the analytical observer might see it differently). From the religious community's view, the reading is a mixed loss of authority and gain in enforcement capacity (tangled rope). From the constitutional reform coalition's view, the reading is transitional support until communal laws themselves reform (scaffold). From the civil court bureaucracy's view, the reading is an institutionalized role, maintained partly through genuine function and partly through institutional inertia (piton). The analytical observer risks naturalizing the secular civil reading as the inevitable outcome of constitutional logic, when it is actually one contested reading among several.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from beneficiary/victim status plus exit options. Inter-religious couples are beneficiaries with mobile exit (d ≈ 0.2), experiencing low effective extraction. Community-embedded believers are victims with identity_locked exit (d ≈ 0.8), experiencing high effective extraction despite structural mobility — the cognitive lock (identity fusion) prevents exercising legal exit. Religious communities are institutional victims with constrained exit (d ≈ 0.65), experiencing moderate-to-high extraction. Civil court bureaucracy are beneficiaries with arbitrage exit (d ≈ 0.15), experiencing low/negative effective extraction. The perspectival gap reflects the wide range of d values across different agents: the same constraint is low-extraction from the inter-religious couple's seat (beneficiary + mobile) and high-extraction from the community believer's seat (victim + identity_locked). Identity_locked is the critical distinction here: the community believer is NOT trapped (legal exit exists) but cannot exercise it because their identity is constituted through the community's law. The binding mechanism is cognitive, not material.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The secular civil reading's mandate was to enable inter-religious marriage and establish gender equality in marital law. This mandate is still live — no constitutional amendment has overridden Articles 14, 15, 21, or the Special Marriage Act, and the problem the reading addressed (inability to marry across religious boundaries, gender inequity in communal law) persists. However, the structural question is whether the reading's function can be absorbed by reformed communal law (Hindu law and Christian law have substantially reformed toward gender equity; Muslim law reform movements exist; Parsi law is minimal). If communal law traditions can internally guarantee individual choice and gender equity, the secular civil reading's functional necessity diminishes. The mandatrophy is unresolved because the scaffold perspective's sunset condition depends on empirical outcomes (whether communal law can reform) that are not yet determined. Mandate obsolescence is conceptually live but empirically undecided.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_rights_vs_communal_flourishing,
    'Are individual rights to freedom of marriage choice a pre-political natural law, or one reading of competing goods (individual autonomy vs communal self-governance)?',
    'Comparative analysis of constitutional frameworks: jurisdictions that ground marriage law in communal autonomy vs individual rights; historical analysis of how individual rights language emerged and was chosen (not discovered)',
    'If natural law: the secular civil reading is inevitable and mountain-class. If constitutional choice: the secular civil reading is a specific instantiation of a particular rights regime, and the false summit gate fires. Affects how to model disputes with communities that prioritize collective over individual autonomy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_rights_vs_communal_flourishing, conceptual, 'Whether individual rights are natural law or constitutional choice').

omega_variable(
    gender_equity_metric_dependence,
    'Does the superiority of the secular civil reading rest on empirical outcomes (measurable gender equity gains) or on deontological commitments (individual rights independent of outcomes)?',
    'Longitudinal measurement: divorce rates, women''s property retention, child custody equity by legal forum (civil vs community law); causal analysis distinguishing improved outcomes from selection effects (women who choose civil law differ systematically from those choosing community law)',
    'If outcome-dependent: the secular civil reading''s extractiveness should decline as community law reforms achieve equivalent gender equity. If deontological: gender equity is a value commitment independent of empirical outcomes, and the reading''s advantage persists regardless of community law performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equity_metric_dependence, empirical, 'Gender equity dependency: empirical outcomes vs deontological values').

omega_variable(
    reading_selection_boundary,
    'Is the secular civil reading genuinely one permissible reading of the marriage authority kernel, or does its foundational axiom (individual rights as natural law) foreclose alternative readings by asserting a premise that religious readings deny?',
    'Formal analysis of axiom compatibility: can a committed believer hold both the individual-rights axiom and the communal-authority axiom simultaneously within any single framework, or does the former logically require denying the latter?',
    'If forecloses: the relation to all sibling readings should be forecloses, not coexists_with. If coexists: multiple readings remain live for different parties. Affects the engine''s computation of how this reading shapes successor constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_boundary, conceptual, 'Whether secular civil axioms foreclose or coexist with religious axioms').

omega_variable(
    community_law_reform_pathway,
    'Can Hindu, Muslim, Christian, and Parsi communal law traditions internally reform to guarantee individual marriage choice and gender equity, or does achieving these require abandoning communal law frameworks altogether?',
    'Documentary analysis of internal reform movements within each tradition; case studies of communities that have reformed internal law while maintaining communal authority; feasibility assessment of hybrid models (communal framework with constitutional rights guardrails)',
    'If internal reform is possible: the secular civil reading is genuinely transitional (scaffold), and the sunset logic holds. If impossible: communal law and individual choice are structurally incompatible, and the secular civil reading is permanent — reclassifies from scaffold toward snare or permanent tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_law_reform_pathway, empirical, 'Feasibility of gender-equitable communal law reform').

omega_variable(
    state_secular_authority_legitimacy,
    'Does the state have legitimate authority to adjudicate marriage on grounds of constitutional values (individual rights, gender equity), or does state authority require consent of the communities it governs?',
    'Political philosophy analysis: theories of state legitimacy in plural societies (Rawlsian public reason, deliberative democracy, minority-protective federalism); empirical: survey data on legitimacy acceptance among communities whose law is displaced; democratic process analysis (was secular marriage law enacted by majoritarian vote, or imposed by constitutional courts despite community objection?)',
    'If consent-based legitimacy: secular civil authority is contingent on community acceptance, and displacement beyond that consent is extractive (snare from community perspective). If constitutional legitimacy independent of consent: the extraction is justified by higher-order rights, but remains extraction. Affects the directionality computation and whether victims are truly victims or subject to justified constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_secular_authority_legitimacy, preference, 'State secular authority legitimacy in plural societies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secular_civil_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scr_theater_1954, secular_civil_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(scr_theater_1979, secular_civil_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(scr_theater_2004, secular_civil_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(scr_theater_2024, secular_civil_reading, theater_ratio, 70, 0.38).

% Extraction over time
narrative_ontology:measurement(scr_extract_1954, secular_civil_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(scr_extract_1979, secular_civil_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement(scr_extract_2004, secular_civil_reading, base_extractiveness, 50, 0.37).
narrative_ontology:measurement(scr_extract_2024, secular_civil_reading, base_extractiveness, 70, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(scr_suppress_1954, secular_civil_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(scr_suppress_1979, secular_civil_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(scr_suppress_2004, secular_civil_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(scr_suppress_2024, secular_civil_reading, suppression_requirement, 70, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secular_civil_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(secular_civil_reading, 0.12).
narrative_ontology:affects_constraint(secular_civil_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(secular_civil_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(secular_civil_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(secular_civil_reading, parsi_communal_reading).
narrative_ontology:affects_constraint(secular_civil_reading, gender_equity_in_family_law).
narrative_ontology:affects_constraint(secular_civil_reading, inter_religious_marriage_access).

% DUAL FORMULATION NOTE:
% The secular civil reading is one element of a constraint family covering marriage authority. Each sibling reading (Hindu, Muslim, Christian, Parsi) has its own constraint story with its own ε value, beneficiary/victim structure, and classified type. They are linked via network.affects_constraints because they compete for jurisdictional authority over the same domain. The secular civil reading's extractiveness (0.35) reflects the costs of displacement; the Hindu reading's extractiveness would reflect costs of potential reformulation of Hindu law to match civil code; the Muslim reading's extractiveness would reflect costs of Shariat enforcement and exclusion of inter-religious couples. Each story is ε-invariant and structurally independent, but they form a family because they are readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secular_civil_reading, powerless, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
