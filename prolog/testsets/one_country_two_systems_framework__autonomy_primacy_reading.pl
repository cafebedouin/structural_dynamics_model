% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__autonomy_primacy_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: One Country, Two Systems (Autonomy Primacy Reading)
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   The One Country, Two Systems (OCTS) framework for Hong Kong represents a
 *   contested kernel: a foundational commitment (the Sino-British Joint
 *   Declaration and Hong Kong Basic Law) that Beijing, London, Washington,
 *   and Hong Kong's political actors interpret radically differently. This
 *   JSON instantiates the AUTONOMY PRIMACY READING: the interpretation that
 *   emphasizes Hong Kong's substantive autonomy, judicial independence, and
 *   civil liberty protection as binding treaty obligations with international
 *   enforceability. Under this reading, mainland intervention in Hong Kong's
 *   internal affairs constitutes treaty violation; judicial review constrains
 *   executive power; and a democratic reform pathway remains structurally
 *   open. This reading treats the two-systems framework as a tangled_rope:
 *   genuine coordination of Hong Kong's distinct legal and economic systems
 *   with mainland China, but embedded in asymmetric extraction where
 *   Beijing's sovereignty claim and Hong Kong residents' autonomy claims are
 *   in structural tension. The measurements show progressive degradation of
 *   autonomy from 1997 (handover, extractiveness 0.35) through 2024
 *   (extractiveness 0.58), with theater_ratio and suppression_requirement
 *   rising monotonically. This degradation pattern supports the scaffold
 *   perspective (international actors see sunset approaching) and challenges
 *   the mountain perspective (treaty protection is not immutable). The false
 *   summit detection engine will identify the mountain perspective as
 *   naturalization: the 'international law immutability' framing conceals a
 *   contingent political arrangement that serves identifiable beneficiaries
 *   (international rule of law coalition, Hong Kong's internationalized
 *   capital, democratic ally networks).
 *
 * KEY AGENTS:
 *   - Hong Kong Residents Seeking Political Autonomy: Primary victims (powerless/trapped) — face suppression through national security legislation, exit barriers, and erosion of civil liberties
 *   - Mainland Central Authority (CCP/Beijing): Primary beneficiary and identity-locked actor (institutional/identity_locked) — benefits from unified sovereignty claim but structurally mobile implementation options constrained by institutional identity
 *   - Hong Kong Business Community (International Firms): Net beneficiary (organized/mobile) — benefits from rule of law and autonomy that enables international contract enforcement and capital flows
 *   - Hong Kong Judiciary: Institutional actor experiencing mixed coordination-extraction (institutional/constrained) — maintains formal independence while facing political pressure and red-line doctrine narrowing
 *   - International Rule of Law Coalition (UK, UN, Democratic Allies): Organized external beneficiary (organized/arbitrage) — benefits from two-systems framework through capital flows, geopolitical alignment, treaty precedent
 *   - Hong Kong Executive Apparatus: Performative institutional actor (institutional/arbitrage) — maintains form of autonomy while executing mainland directives; piton perspective
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent arrangement as immutable law; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.58).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.62).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "One Country, Two Systems (Autonomy Primacy Reading)").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, '19f8b145-0ecb-425a-bf19-bc62a62cd91c').
narrative_ontology:cs_kernel_codification('19f8b145-0ecb-425a-bf19-bc62a62cd91c', fixed_text).
narrative_ontology:cs_authority_grounding('19f8b145-0ecb-425a-bf19-bc62a62cd91c', lineage).
narrative_ontology:cs_interpretation_layer_present('19f8b145-0ecb-425a-bf19-bc62a62cd91c').
narrative_ontology:cs_reading_relation('19f8b145-0ecb-425a-bf19-bc62a62cd91c', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('19f8b145-0ecb-425a-bf19-bc62a62cd91c', one_country_two_systems_framework__balanced_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('19f8b145-0ecb-425a-bf19-bc62a62cd91c', foundational, treaty_autonomy_binding_constraint).
narrative_ontology:cs_axiom_status(treaty_autonomy_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('19f8b145-0ecb-425a-bf19-bc62a62cd91c', treaty_autonomy_binding_constraint, conventional).
narrative_ontology:cs_axiom('19f8b145-0ecb-425a-bf19-bc62a62cd91c', foundational, judicial_independence_autonomy_foundation).
narrative_ontology:cs_axiom_status(judicial_independence_autonomy_foundation, holdable).
narrative_ontology:cs_axiom_grounding('19f8b145-0ecb-425a-bf19-bc62a62cd91c', judicial_independence_autonomy_foundation, deontological).
narrative_ontology:cs_reference_frame('19f8b145-0ecb-425a-bf19-bc62a62cd91c', treaty_autonomy_framework_1997).
narrative_ontology:cs_drift_state('19f8b145-0ecb-425a-bf19-bc62a62cd91c', contemporary_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('19f8b145-0ecb-425a-bf19-bc62a62cd91c', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents_civil_liberty_beneficiaries).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_rule_of_law_coalition).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, mainland_central_authority).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_executive_unchecked_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HONG KONG POLITICAL DISSIDENT (SNARE) — Cannot exit the jurisdiction without abandoning home, family, career, and identity as Hong Kong resident. Trapped by geography, emotional bonds, and exit costs. Faces suppression through Article 23 national security legislation, selective prosecution, and chilling effect on speech. No meaningful check on mainland interference protects speech or assembly rights. Maximum experienced extraction through suppression and lost autonomy.
constraint_indexing:constraint_classification(one_country_two_systems_framework__autonomy_primacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HONG KONG CIVIL SOCIETY (TANGLED ROPE) — Constrained by regulatory restrictions and government pressure, but some protection from common law judicial review and residual autonomy. Experiences both coordination function (rule of law provides stability for contractual relationships, property rights, commercial disputes) and extraction (surveillance, NGO registration barriers, funding restrictions). Exit is costly but theoretically possible through relocation or formal dissolution. Mixed experience of constraint.
constraint_indexing:constraint_classification(one_country_two_systems_framework__autonomy_primacy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HONG KONG BUSINESS COMMUNITY (ROPE) — International firms benefit from rule of law, independent courts, common law contract enforcement, and stable property rights that differentiate Hong Kong from mainland legal systems. The constraint functions as pure coordination: two-systems autonomy enables business coordination that would not exist under mainland law. Exit options exist (relocation, trading elsewhere) but are not preferred given Hong Kong's benefits. Net beneficiary from the coordination mechanism.
constraint_indexing:constraint_classification(one_country_two_systems_framework__autonomy_primacy_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: HONG KONG JUDICIARY (TANGLED ROPE) — Operates as a genuine coordination mechanism for commercial law, contract disputes, property rights, and criminal justice within Hong Kong boundaries. Simultaneously experiences extraction through political pressure, the 'red line' doctrine narrowing scope of review, and systematic encroachment on judicial independence (BL 159 reinterpretations, deletion of rule of law references). Courts are forced into performative compliance with mainland political directives while maintaining formal independence. Active enforcement required to sustain the autonomy claim.
constraint_indexing:constraint_classification(one_country_two_systems_framework__autonomy_primacy_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MAINLAND CENTRAL AUTHORITY (SNARE) — Identity-locked to centralized control and sovereignty claim. Structurally mobile (could implement genuine autonomy) but identity fusion with unified CCP authority and sovereign supremacy makes autonomy unthinkable from within the mainland political framework. Trapped by institutional identity, not material barriers. Experiences the two-systems framework as extraction of sovereignty — loss of control is seen as loss of essential authority. The treaty commitment is a snare on mainland interests: must suppress Hong Kong autonomy to maintain identity coherence.
constraint_indexing:constraint_classification(one_country_two_systems_framework__autonomy_primacy_reading, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL RULE OF LAW COALITION (SCAFFOLD) — External actors (UK, UN, democratic allies, international courts) see the autonomy reading as a temporary structural scaffold: treaty enforcement through international pressure, bilateral negotiations, and reputational cost can sustain two-systems separation for a generation. But the sunset clause is implicit: mainland integration pressure is structural; genuine autonomy has a limited lifespan unless actively defended internationally. The analytical observer identifies the scaffold as progressively degrading through incremental encroachment (Article 23, BL 159 reinterpretations, arrest of activists under national security laws). Eventually treaty enforcement capacity will erode and the scaffold will collapse.
constraint_indexing:constraint_classification(one_country_two_systems_framework__autonomy_primacy_reading, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: HONG KONG EXECUTIVE APPARATUS (PITON) — The institutional machinery of Hong Kong government performs autonomy while receiving directives from mainland authorities. The Chief Executive, civil service, and administrative apparatus maintain the form of autonomous governance (separate legal system, Chief Executive symbolic authority, civil service meritocracy) while functionally executing mainland strategic interests. Theater ratio (0.65) reflects performative autonomy: the institutional apparatus produces the appearance of two-systems while actual decision-making migrates to mainland channels. Degraded from what Rope coordination would be toward inert institutional form maintained by ceremony and habit.
constraint_indexing:constraint_classification(one_country_two_systems_framework__autonomy_primacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (UNIVERSAL SOVEREIGNTY IMMUTABILITY) — From a civilizational perspective, this reading risks treating the treaty commitment as a mountain (natural law immutable guarantee). The framing naturalizes what is contingent: Beijing's willingness to tolerate dual sovereignty is presented as a structural feature of international law rather than a reversible political choice. This perspective is flagged as a false summit candidate — beneficiary declarations (international rule of law coalition) suggest the 'natural law' framing conceals a constructed constraint that serves identifiable interests (Western capital, democratic allies). The engine will compute this as false summit through FSM detection.
constraint_indexing:constraint_classification(one_country_two_systems_framework__autonomy_primacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(one_country_two_systems_framework__autonomy_primacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(one_country_two_systems_framework__autonomy_primacy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, TR),
    TR >= 0.70.

:- end_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The autonomy reading treats mainland interference and suppression as extraction from Hong Kong autonomy, but acknowledges that coordination benefits exist (commercial law, stable property rights, efficient administration). The trajectory from 0.35 to 0.58 reflects historical accumulation: Article 23 national security legislation (2003 failed, 2020 succeeded through NSL), BL 159 reinterpretations narrowing judicial review, selective prosecution of activists under national security laws, and gradual erosion of civil service meritocracy. The 0.58 figure reflects that suppression has become structural and systematic rather than episodic. Suppression (0.62): High. The constraint requires active enforcement of the 'red line' doctrine, surveillance apparatus, prosecutorial selectivity, and chilling effects on free expression. Suppression is not total (courts still function, press still criticizes, protests still occur) but severe enough to deter political organizing and constrain institutional independence. Theater ratio (0.65): Moderate-high. Hong Kong's executive and legislature perform autonomy through formal procedures (Chief Executive elections, Legislative Council debates, judicial proceedings) while actual decision-making power migrates through mainland channels (Liaison Office directives, CCP committees, personnel vetting). The theater has increased as the gap between formal autonomy and actual constraint has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. Hong Kong dissidents see pure extraction (snare): no meaningful autonomy, trapped by geography and exit costs. Business sees pure coordination (rope): rule of law enables international commerce. Mainland sees extraction of sovereignty (snare): treaty commitment constraints mainland's essential unified authority. International observers see temporary coordination with approaching sunset (scaffold): treaty enforcement can sustain two-systems for a generation but integration pressure is structural. Hong Kong courts see mixed coordination-extraction (tangled_rope): real function in commercial disputes, real pressure in political cases. The Executive apparatus performs autonomy while executing mainland directives (piton). The civilizational observer risks naturalizing the arrangement as immutable law (mountain), which the FSM detector will flag. The perspectival gaps are not artifacts of measurement ambiguity — they are structural: the same institutional framework genuinely provides different functional roles to different agents with incompatible interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from beneficiary/victim status and exit options. Mainland's identity-locked status (institutional actor, mobile structural options, but identity-fused to centralized sovereignty) produces high d (~0.89) because the identity lock prevents exercising structural mobility even though it exists. Hong Kong residents' trapped status (powerless, no exit, victim of suppression) produces maximum d (~0.95). International business' arbitrage exit (organized, can relocate or trade elsewhere but benefits from Hong Kong, beneficiary) produces low d (~0.15). Courts' constrained exit (institutional, forced compliance with political directives, mixed function) produces mid-range d (~0.55). The spread across perspectives (d from 0.15 to 0.95) produces chi values ranging from -0.12 (international beneficiary perspective) to 1.42 (trapped dissident perspective), validating the perspectival classifications from snare to rope within the same base constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is the apparent paradox that the same institutional framework classifies as six different types depending on perspective. The resolution is structural: OCTS is a contested kernel with incompatible readings (autonomy vs. sovereignty primacy). The autonomy reading instantiates a specific structural picture: genuine coordination (business beneficiaries), real autonomy (judiciary functions), real suppression (dissidents trapped), but increasing extraction over time (measurements show monotonic degradation). This reading is NOT claiming 'the truth' about OCTS — other readings (sovereignty_primacy_reading) would produce different measurements and classifications. The autonomy reading resolves mandatrophy by committing to a specific interpretation of the kernel and measuring internal consistency within that interpretation. The false summit detection (mountain perspective) reveals that claiming natural law immutability for a contingent political arrangement serves identifiable beneficiaries' interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_enforcement_mechanism_existence,
    'Do international enforcement mechanisms for the Joint Declaration and Basic Law actually constrain mainland action, or are they merely declaratory?',
    'Analysis of historical treaty violations (Article 23 national security legislation, BL 159 reinterpretations), documented international responses (UN statements, bilateral pressure, ICC jurisdiction limitations), and correlation between response severity and mainland policy persistence',
    'If enforcement exists: the two-systems framework is a genuine tangled_rope with real constraints on mainland extraction. If mechanisms are declaratory only: the autonomy reading is performative (piton) and the sovereignty_primacy reading is structurally dominant. This omega is THE critical distinction between autonomy and sovereignty readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_enforcement_mechanism_existence, empirical, 'Whether international enforcement mechanisms for treaties actually constrain mainland action').

omega_variable(
    judicial_independence_residual_capacity,
    'Do Hong Kong courts retain sufficient independence to invalidate mainland-aligned policies, or has the ''red line'' doctrine collapsed substantive judicial review?',
    'Analysis of recent high court decisions: rulings that contradicted government preference (or absence thereof), judicial review of national security cases, scope of cases where courts refuse to rule (JIT doctrine), proportion of government cases where courts find against the executive',
    'If courts retain independence: civil liberties are genuinely protected (snare perspective for dissenters is overstated). If red-line doctrine has collapsed review: courts are piton (performative) and civil liberties are snare. This resolves whether the tangled_rope coordination function is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_residual_capacity, empirical, 'Whether Hong Kong judicial independence remains meaningful relative to mainland pressure').

omega_variable(
    democratic_reform_pathway_closure,
    'Is democratic reform of Hong Kong''s electoral system possible within the autonomy reading, or has the pathway been structurally foreclosed?',
    'Analysis of recent electoral reform decisions (2019 district council elections, 2020 Legislative Council revisions, 2021 CE electoral committee changes), proportion of universal suffrage supporters in government, Beijing''s public statements on democracy timeline',
    'If pathway remains open: this reading includes a credible mechanism for expanding autonomy through internal institutional evolution. If foreclosed: the reading is aspirational and sovereignty_primacy reading is structurally dominant. Democracy is a legitimacy claim of this reading; its structural closure undermines autonomy framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_reform_pathway_closure, empirical, 'Whether democratic reform of Hong Kong''s political system remains structurally possible').

omega_variable(
    mainland_integration_vector_acceleration,
    'Is mainland institutional integration (economic interdependence, personnel transfer, administrative harmonization) accelerating at a rate that will erode two-systems separation within a generation?',
    'Time-series analysis of mainland-Hong Kong institutional coupling: cross-border administrative personnel, mainland firms'' legal jurisdiction claims, proportion of Hong Kong commerce dependent on mainland supply chains, cross-border legal harmonization initiatives',
    'If acceleration > historical trend: scaffold sunset is structural and inevitable. If stable: two-systems can persist if politically supported. This resolves the temporal scope of this reading''s validity and the magnitude of the scaffold perspective''s sunset clause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mainland_integration_vector_acceleration, empirical, 'Whether mainland institutional integration is accelerating toward erosion of two-systems separation').

omega_variable(
    reading_kernel_stability,
    'Does this autonomy_primacy reading represent a stable committer position held by coherent institutional actors (e.g., UK, UN, Hong Kong legal community), or is it an aspirational framing with eroding institutional support?',
    'Analysis of institutional actors'' public positions over time: UK government statements on Treaty obligations, UN Special Rapporteur reports, Hong Kong Bar Association and Law Society advocacy, Democratic parties'' platform continuity',
    'If stably held: this reading is a real contender in the kernel dispute and the engine should compute coexists_with relationships with other readings. If eroding: this reading is aspirational and the sovereignty_primacy reading is consolidating institutional dominance. Omega resolves whether the reading itself has drifted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_stability, empirical, 'Whether the autonomy_primacy reading represents a stable institutional position or is eroding toward sovereignty primacy').

omega_variable(
    false_summit_natural_law_status,
    'Is the two-systems framework presented as an immutable natural law of international law and treaty supremacy, when it is actually a contingent institutional arrangement that serves identifiable interests?',
    'Examine whether the framework is defended on grounds of ''international law immutability'' vs. grounds of ''pragmatic coordination for mutual benefit'' — the framing reveals whether the mountain perspective is natural law or false summit naturalization',
    'If natural law: mountain classification is legitimate and the framework is treaty-enforced. If false summit: the beneficiaries (international capital, democratic allies) are naturalizing a political arrangement as legal immutability, and the sovereignty_primacy reading better explains actual institutional dynamics',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_status, conceptual, 'Whether the autonomy_primacy reading naturalizes a contingent arrangement as immutable law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(octs_aut_theater_1997, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(octs_aut_theater_2009, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement(octs_aut_theater_2024, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 27, 0.65).

% Extraction over time
narrative_ontology:measurement(octs_aut_extractiveness_1997, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(octs_aut_extractiveness_2009, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(octs_aut_extractiveness_2024, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 27, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(octs_aut_suppression_1997, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(octs_aut_suppression_2009, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(octs_aut_suppression_2024, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 27, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_rule_of_law_erosion).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, mainland_legal_system_jurisdiction_expansion).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_democratic_reform_pathway).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, civil_liberties_protection_framework).

% DUAL FORMULATION NOTE:
% The OCTS kernel is decomposed across multiple constraint stories per the ε-invariance principle: (1) autonomy_primacy_reading (this file, ε=0.58, tangled_rope) emphasizes substantive autonomy and treaty enforceability; (2) sovereignty_primacy_reading (separate file, ε variable, different type structure) emphasizes Beijing's ultimate authority and temporary arrangement framing; (3) balanced_coexistence_reading (separate file) treats autonomy and sovereignty as genuinely dual and irreducible. Each reading has its own ε value, its own beneficiary/victim structure, its own measurement trajectory, and its own perspectives. The three stories form a constraint family linked through network.affects_constraints. Their ε values differ because the readings measure different structural relationships to the same institutional kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__autonomy_primacy_reading, institutional, 0.89).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
