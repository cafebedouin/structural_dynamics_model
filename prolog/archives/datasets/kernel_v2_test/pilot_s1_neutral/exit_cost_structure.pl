% ============================================================================
% CONSTRAINT STORY: exit_cost_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exit_cost_structure, []).

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
 *   constraint_id: exit_cost_structure
 *   human_readable: Exit Cost Structure in India's Parallel Personal Law Regime
 *   domain: comparative_law/legal_pluralism/constitutional_theory
 *
 * SUMMARY:
 *   India's parallel personal-law regime — where Hindu, Muslim, Christian,
 *   Parsi, and secular marriage/family law systems operate simultaneously
 *   under one constitutional state — creates a structure that appears to
 *   coordinate religious pluralism but functions substantially as extraction
 *   through exit costs. Individuals born into a religious community are
 *   legally registered into that community's personal law framework. Exiting
 *   to secular law is technically possible but carries severe costs: identity
 *   fracture (community membership, family honor, religious status loss),
 *   legal barriers (proof requirements, registration procedures), and social
 *   enforcement (ostracism, family rupture). The regime benefits religious
 *   authorities (who maintain institutional power through law delegation) and
 *   the constitutional state (which avoids majoritarian uniform law and
 *   preserves electoral coalitions based on religious voting blocs). The
 *   regime extracts from individuals who seek exit and from gender-parity
 *   claimants (because personal law regimes retain gender-asymmetric
 *   provisions such as unilateral talaq, differential inheritance, and male
 *   guardianship presumptions). The constraint exhibits a core tension: is
 *   this an immutable feature of religious pluralism (mountain), a
 *   transitional arrangement on the way to uniform law (scaffold), a degraded
 *   secularist mandate maintained as performance (piton), or a hybrid
 *   coordination-extraction mechanism (tangled rope)? The empirical answer
 *   shifts based on observer position, time horizon, and exit options. The
 *   temporal measurements show rising extractiveness and enforcement
 *   intensity from 1947-2020, driven by (a) accumulating policy decisions
 *   that entrench personal law (e.g., 1996 Hindu Succession Act preserving
 *   gender asymmetries, 2009 Sharia law judicial validation), (b) religious
 *   coalition hardening in electoral politics, and (c) state capture by
 *   religious institutional interests during moments of reform pressure.
 *
 * KEY AGENTS:
 *   - Exit-seeking individuals (powerless/identity_locked): bore full cost of identity fracture, legal barriers, social enforcement; trapped by internalized identity frames preventing even conceiving exit as legitimate
 *   - Gender-parity claimants (moderate/constrained): seek reform of gender-asymmetric provisions; face extraction through constrained exit (reform requires sustained organization, faces backlash)
 *   - Personal law authorities (institutional/arbitrage): religious institutions, community councils, dharmaśāstra interpreters; benefit from regime through authority delegation and institutional reproduction; experience as pure coordination
 *   - Constitutional state (institutional/constrained): claims coordination function (accommodate pluralism); extracts political stability at cost of constraining individual exit; constrained by religious coalition backlash
 *   - Gender justice coalition (organized/constrained): organized exit-seekers; reveal extraction as design choice through constitutional litigation and reform advocacy; constrained by political opposition
 *   - Secularist elite (powerful/mobile): inherited mandate of constitutional secularism; find mandate degraded into performance while extraction persists; maintain symbolic reformism while structural regime unchanged
 *   - Constitutional reform movement (organized/constrained): see regime as transitional with sunset into uniform law; maintain reform momentum; constrained by vested interests
 *   - Analytical observer (analytical/analytical): risks naturalizing contingent arrangement as immutable feature of pluralism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exit_cost_structure, 0.58).
domain_priors:suppression_score(exit_cost_structure, 0.62).
domain_priors:theater_ratio(exit_cost_structure, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exit_cost_structure, extractiveness, 0.58).
narrative_ontology:constraint_metric(exit_cost_structure, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(exit_cost_structure, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exit_cost_structure, tangled_rope).
narrative_ontology:human_readable(exit_cost_structure, "Exit Cost Structure in India's Parallel Personal Law Regime").
narrative_ontology:topic_domain(exit_cost_structure, "comparative_law/legal_pluralism/constitutional_theory").

domain_priors:requires_active_enforcement(exit_cost_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exit_cost_structure, '42de5973-87b0-4f8f-926a-21b9dbfe82aa').
narrative_ontology:cs_kernel_codification('42de5973-87b0-4f8f-926a-21b9dbfe82aa', formalized).
narrative_ontology:cs_authority_grounding('42de5973-87b0-4f8f-926a-21b9dbfe82aa', extraction).
narrative_ontology:cs_interpretation_layer_present('42de5973-87b0-4f8f-926a-21b9dbfe82aa').
narrative_ontology:cs_reading_relation('42de5973-87b0-4f8f-926a-21b9dbfe82aa', exit_cost_structure__hindu_majoritarian_family_law_reading, coexists_with).
narrative_ontology:cs_reading_relation('42de5973-87b0-4f8f-926a-21b9dbfe82aa', exit_cost_structure__secular_uniform_civil_code_reading, influences).
narrative_ontology:cs_reading_relation('42de5973-87b0-4f8f-926a-21b9dbfe82aa', exit_cost_structure__multiple_incoherent_kernels_reading, coexists_with).
narrative_ontology:cs_axiom('42de5973-87b0-4f8f-926a-21b9dbfe82aa', foundational, religious_community_autonomy_necessary).
narrative_ontology:cs_axiom_status(religious_community_autonomy_necessary, overridden).
narrative_ontology:cs_axiom_grounding('42de5973-87b0-4f8f-926a-21b9dbfe82aa', religious_community_autonomy_necessary, instrumental).
narrative_ontology:cs_axiom('42de5973-87b0-4f8f-926a-21b9dbfe82aa', foundational, majoritarian_law_imposition_delegitimizing).
narrative_ontology:cs_axiom_status(majoritarian_law_imposition_delegitimizing, holdable).
narrative_ontology:cs_axiom_grounding('42de5973-87b0-4f8f-926a-21b9dbfe82aa', majoritarian_law_imposition_delegitimizing, deontological).
narrative_ontology:cs_axiom('42de5973-87b0-4f8f-926a-21b9dbfe82aa', secondary, uniform_law_eventual_destination).
narrative_ontology:cs_axiom_status(uniform_law_eventual_destination, overridden).
narrative_ontology:cs_axiom_grounding('42de5973-87b0-4f8f-926a-21b9dbfe82aa', uniform_law_eventual_destination, empirically_contingent).
narrative_ontology:cs_reference_frame('42de5973-87b0-4f8f-926a-21b9dbfe82aa', plural_constitutional_accommodation).
narrative_ontology:cs_drift_state('42de5973-87b0-4f8f-926a-21b9dbfe82aa', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('42de5973-87b0-4f8f-926a-21b9dbfe82aa', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exit_cost_structure, personal_law_authorities).
narrative_ontology:constraint_beneficiary(exit_cost_structure, constitutional_majoritarian_stability).
narrative_ontology:constraint_victim(exit_cost_structure, exit_seeking_individuals).
narrative_ontology:constraint_victim(exit_cost_structure, gender_parity_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXIT-SEEKING INDIVIDUAL (SNARE) — Individual locked into religious personal law framework by identity fusion (family honor, community membership, religious identity). Structurally mobile (could convert to secular law framework) but functionally trapped because exit requires abandoning their entire relational identity. No alternatives presented as legitimate within the identity frame. Maximum extraction: the individual bears full cost of law-switching (social ostracism, family rupture, religious status loss) while locked into a legal regime they did not choose and cannot challenge from within. Cannot organize collectively because identity lock prevents public exit announcement.
constraint_indexing:constraint_classification(exit_cost_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: SECULAR-MINDED FAMILY MEMBER (TANGLED ROPE) — Genuinely benefits from coordinated family law (legal certainty in inheritance, marriage recognition, custody rights) but the coordination mechanism is hijacked by asymmetric enforcement. Bears substantial costs: gender-asymmetric rules (e.g., unilateral talaq under Islamic law), differential property rights, custody presumptions favoring male guardians. Exit to secular law is costly but possible (reputation damage, family rupture, loss of inheritance claims under personal law). This perspective reveals the hybrid: coordination function (family law solves real problems) + extraction function (power asymmetries within that coordination).
constraint_indexing:constraint_classification(exit_cost_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PERSONAL LAW AUTHORITY (ROPE) — Religious/community institutions (churches, mosque councils, Hindu dharmaśāstra interpreters, Parsi panchayats) experience the constraint as pure coordination: managing marriage, inheritance, and family matters according to community tradition. They collect rents in form of authority, community deference, and institutional reproduction. Exit from the regime would dissolve their institutional function entirely. From this seat, the parallel law system is a solution to plural coordination problems, not extraction — each community coordinates its own members' family law according its own norms. The extractive asymmetries invisible from this perspective because they appear as 'tradition' or 'natural order' rather than design choices.
constraint_indexing:constraint_classification(exit_cost_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL STATE (TANGLED ROPE / CAPTURED UMPIRE) — The Indian state claims coordination function: accommodate plural religious communities through constitutional pluralism rather than imposing uniform law. But the state's enforcement of this coordination extracts political stability at the cost of individual exit. The state prevents uniform civil code adoption (faces religious coalition backlash), constrains exit-seekers' legal options (must choose one registered personal law regime), and maintains enforcement machinery that locks individuals into religious identity. The state benefits from this arrangement (avoids communal conflict, preserves electoral coalitions based on religious voting blocs); the state pays through institutional limitation (cannot pursue gender-parity or secular modernization without fracturing the coalition). Constrained exit: the state could adopt uniform civil code but faces massive political extraction cost from religious coalitions.
constraint_indexing:constraint_classification(exit_cost_structure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GENDER JUSTICE COALITION (TANGLED ROPE) — Organized agents (women's rights groups, constitutional scholars, secular political movements) see the system as producing genuinely asymmetric extraction: personal law regimes contain gender-asymmetric provisions (unilateral talaq, unequal inheritance, guardianship presumptions). These groups benefit from the constitutional commitment to rule-of-law and individual rights (coordination function) but the parallel personal-law regime extracts from gender-parity commitments. Exit: reforming the system requires sustained political organization, constitutional litigation, and cultural persuasion. Some exit capacity through judicial reinterpretation (e.g., Shayara Bano case striking down instant talaq) but constrained by political backlash from religious institutions. The coalition organizes against extraction, revealing it as avoidable design choice, not natural necessity.
constraint_indexing:constraint_classification(exit_cost_structure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SECULARIST ELITE (PITON) — Intellectual and political actors who inherited the mandate of constitutional secularism (secular schooling, rational modernization, uniform law) find that mandate atrophied and now maintained as performance rather than function. The secularist vision of India — moving toward uniform civil code, gender-neutral law, secular identity — has been constrained into a symbolic posture maintained by courts and constitutional rhetoric while the actual regime produces exit costs that lock individuals into religious identity. The piton classification reflects that the secularist elite now performs modernization (court-ordered reforms, public statements favoring uniform code) while the extraction mechanism persists through political constraints. Theater ratio (0.48) reflects moderate performativity: some genuine reforms occur (Shayara Bano case) but fundamental regime structure unchanged.
constraint_indexing:constraint_classification(exit_cost_structure, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: CONSTITUTIONAL REFORM MOVEMENT (SCAFFOLD) — Reform-oriented political movements and civil society actors see the parallel personal-law regime as temporary: a transitional arrangement meant to ease the post-colonial Hindu-majority state's accommodation of religious minorities, with an implicit sunset into uniform civil code. This perspective sees generational change, declining religious identification, and cosmopolitan values as gradually reducing attachment to personal law regimes. Exit cost: sustained political organization required to keep reform momentum. The sunset logic appears plausible from this seat because constitutional law, elite discourse, and judicial decisions continually invoke the uniform civil code ideal even while political economy prevents its adoption. Scaffold classification reflects genuine transitional function plus constraint on exit by vested interests (religious institutions + majoritarian electoral coalitions).
constraint_indexing:constraint_classification(exit_cost_structure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the parallel personal-law regime might appear as an immutable feature of religious pluralism: where multiple faith communities coexist, each maintaining its own law traditions seems natural, inevitable, or even required by respect for difference. Legal pluralism itself appears as a structural law of multi-ethnic societies. However, the structural data contradicts this: the regime contains identifiable beneficiaries (religious institutions, majoritarian state coalition), identifiable victims (exit-seekers, gender-parity claimants), active enforcement machinery (registration requirements, legal barriers to exit), and measurable extraction (opportunity costs of law-switching, constrained options). The engine will compute this as a false summit — the 'natural' framing naturalizes what is actually a contingent institutional arrangement with losers and beneficiaries.
constraint_indexing:constraint_classification(exit_cost_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exit_cost_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exit_cost_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exit_cost_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exit_cost_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(exit_cost_structure, TR),
    TR >= 0.70.

:- end_tests(exit_cost_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The exit cost for individuals is substantial (opportunity cost of switching regimes, identity fracture, social enforcement, legal barriers). The measurement increased from 0.35 at independence (when personal law was framed as temporary accommodation) to 0.58 today (after decades of entrenchment). The increase reflects cumulative policy decisions that locked personal law into constitutional structure rather than treating it as transitional. Not higher (0.7+) because some exit pathways exist (conversion is legally possible, courts have reformed worst provisions like instant talaq), and the regime does solve genuine coordination problems (inheritance clarity, marriage recognition). Suppression (0.62): Moderate-high. Multiple enforcement mechanisms: (a) legal architecture requiring registration into personal law regime; (b) procedural barriers to exiting (proof of conversion, family consent, religious authority validation in some cases); (c) internalized suppression (identity lock preventing individuals from even articulating exit as possibility); (d) social enforcement (family rupture, community ostracism, honor dynamics). The measurement reflects structural suppression machinery rather than merely high costs. Theater ratio (0.48): Moderate. The regime exhibits dual character: some functional coordination (family law clarity within religious communities) but substantial performativity (state rhetoric about secular modernization while regime persists unchanged; court orders on gender equity without systemic reform; constitutional law celebrating individual rights while personal law constrains them). Theater increased from 0.25 at independence (when personal law was more openly transitional) to 0.48 (when regime became entrenched while rhetoric stayed reformist). Not higher because some genuine coordination functions remain and some reforms have teeth (Shayara Bano case on instant talaq was consequential).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The personal law authority experiences pure coordination (rope): they manage community family law, achieve social stability within their community, and see the pluralist regime as solving the problem of accommodating diverse traditions. They experience no extraction because they are not paying costs — the costs are borne by individuals and by the constitutional state's constrained reformism. The exit-seeking individual experiences snare: they are locked into a legal regime by identity fusion (their self-concept is constituted through family, community, religious membership), face structural barriers (legal requirements, family pressure), and cannot organize collectively (identity lock prevents public exit). The secular-minded family member experiences tangled rope: they benefit from family law coordination but the coordination is hijacked by gender asymmetries and constrains their options. The constitutional state experiences tangled rope differently: benefits from coalition stability, pays through constrained reformism capacity. The reform coalition experiences constrained tangled rope: benefits from constitutional law and rule-of-law norms, extracts against gender parity. The piton perspective (secularist elite) reveals that modernization mandate is degraded — court orders and constitutional rhetoric perform reform while structural regime unchanged. The scaffold perspective reveals genuine generational change pressure (religious identification declining, cosmopolitan values spreading) that could make reform feasible over generational time. The analytical observer's mountain perspective is a false summit: the regime appears natural/inevitable because 'pluralism requires accommodation' but that naturalizes a specific institutional design with winners and losers. The perspectival gap is not a problem to be solved but the diagnostic signal that the constraint is extractive: all the perspectives cannot be simultaneously true unless we are measuring something with real structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) differ sharply across observer positions because the constraint's structure is asymmetric. Personal law authorities: d ≈ 0.1 (full beneficiaries; the constraint subsidizes their institutional position). Exit-seeking individuals: d ≈ 0.85 (near-full targets; they bear maximum extraction cost while trapped by identity lock). Gender-parity claimants: d ≈ 0.65 (significant targets but with some agency through organization). Constitutional state: d ≈ 0.55 (symmetric mixed position: benefits from coalition stability but constrained by reform pressure; neither pure target nor pure beneficiary). The engine computes effective extraction chi by applying the sigmoid f(d) and scaling by scope. For powerless individuals with trapped/identity_locked exit, f(d) amplifies chi. For institutional beneficiaries with arbitrage exit, f(d) dampens chi. The national scope applies consistent scaling across all perspectives (larger scope would amplify verification difficulty and thus amplification; smaller scope would dampen). The directionality vector explains why the same constraint structure produces snare perception from some seats and rope perception from others: the structural asymmetry is real; the perspectival disagreement reflects different positions within that asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is RESOLVED. The constraint's founding mandate was 'accommodate religious pluralism in post-colonial constitution' — a genuine coordination problem given India's religious diversity and the need to avoid communal conflict at independence. The mandate still serves a function (prevents majoritarian imposition, provides legal space for religious community autonomy). However, the regime has accumulated extraction mechanisms that now substantially outlive the founding mandate. Two critical developments: (1) FUNCTION PERSISTENCE WITH EXTRACTION ACCUMULATION: the regime still coordinates family law within communities, but that coordination function now occupies only ~40-50% of its operation; the other 50-60% is pure extraction (exit costs, identity lock, gender asymmetry). (2) DEGRADATION THROUGH ENTRENCHMENT: what was meant to be temporary accommodation (personal law while transition to uniform code occurred) became entrenched through political capture, creating pseudo-permanent regime. The mandatrophy is resolved by recognizing that the constraint now serves two distinct functions with incompatible trajectories: (a) genuine religious accommodation (coordination function, worth preserving in some form), and (b) extraction mechanism locking individuals into religious identity (should be dismantled). The unified classification (tangled rope) correctly names this hybrid; the piton perspective correctly identifies the degradation of the secular modernization mandate; the scaffold perspective correctly identifies that reform momentum exists but is constrained by political economy. The regime is NOT a case of coordination masquerading as extraction (snare narrative) — the coordination is real. The regime IS a case of legitimate coordination (accommodation of pluralism) that has accumulated extraction mechanisms that now dominate its operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_pluralism_vs_constructed_regime,
    'Is the parallel personal-law system a natural feature of religious pluralism or a constructed extraction mechanism that instrumentalizes ''respect for difference''?',
    'Comparative analysis: (a) jurisdictions with religious pluralism that adopted uniform law without communal violence (e.g., civil marriage in Lebanon for some communities, Rwanda post-genocide); (b) historical trajectory of India''s personal law regime — was it designed for accommodation or did extraction mechanisms accumulate over time through political compromise?',
    'If natural: mountain classification correct; exit costs are inherent to pluralism. If constructed: tangled_rope classification correct; the regime can be reformed and exit costs are policy choices, not necessities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_pluralism_vs_constructed_regime, conceptual, 'Whether parallel personal law is natural feature of pluralism or constructed extraction mechanism').

omega_variable(
    identity_lock_mechanism_and_strength,
    'How much of the exit cost is structural (legal barriers, opportunity costs) versus internalized (identity fusion preventing even imagining exit)?',
    'Longitudinal qualitative research: exit-seeking individuals'' retrospective accounts of why they remained in personal law regime; post-exit interviews documenting the identity-reconstruction process; comparison of exit rates post-2009 (after Shayara Bano and easier conversion pathways became available).',
    'If heavily internalized: identity_locked exit option is appropriate; the constraint is partly Snare (structurally extractive) and partly shaped by cognitive frames. If heavily structural: trapped exit option is appropriate; the issue is legal/political barriers, not identity fusion. This determines whether reform requires cultural narrative change or merely legal-political change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_and_strength, empirical, 'Proportion of exit cost that is structural versus identity-fusion-based').

omega_variable(
    majoritarian_coalition_stability_hypothesis,
    'Does the parallel personal-law regime genuinely stabilize the Hindu-majority constitutional coalition, or is the perceived stability illusory (fragile because latent)?',
    'Political analysis of communal violence patterns; electoral coalition stability before/after moments of personal-law conflict (e.g., 2019 triple talaq reform, 2018 Sabarimala temple entry); comparative analysis of periods when uniform civil code was politically salient versus when it receded.',
    'If stabilizing: the state''s beneficiary status is real and the political extraction cost of reform is genuine (tangled_rope justified). If illusory: the state is not actually destabilized by personal-law reform and the extraction is ideological rather than structural — indicates a false beneficiary claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_coalition_stability_hypothesis, empirical, 'Whether parallel personal law actually stabilizes Hindu-majority coalition or stability is illusory').

omega_variable(
    reform_pathway_feasibility,
    'Is the scaffold sunset (gradual transition to uniform law) structurally plausible or is the regime locked into indefinite persistence?',
    'Scenario analysis: What political-cultural conditions would need to change for uniform civil code to become feasible? (a) religious identification declining (survey trends). (b) Generational change reducing personal-law demand. (c) Constitutional court pressure accumulating (judicial strike). (d) Women''s coalition becoming stronger electoral bloc. Which conditions are showing movement?',
    'If plausible: scaffold classification is justified; regime is transitional and exit costs will decrease generationally. If locked: piton classification may be more accurate; the regime persists indefinitely through inertia and political capture regardless of reform rhetoric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_pathway_feasibility, preference, 'Whether scaffold sunset pathway to uniform law is structurally plausible').

omega_variable(
    secular_law_framework_neutrality,
    'Is the secular/uniform law alternative genuinely neutral with respect to religious identity, or does it impose a particular secular worldview that functions as disguised cultural extraction?',
    'Philosophical/normative analysis: does uniform civil law embody Hindu-majoritarian assumptions (inheritance rules, family structure, secular presumptions)? Or do secular laws genuinely accommodate religious pluralism without imposing identity? Comparative: how do other constitutions (Canada, Malaysia) handle this tension?',
    'If genuinely neutral: reform toward uniform code reduces extraction. If disguised majoritarian: reform would transfer extraction from religious minorities (through identity lock) to religious minorities (through cultural imposition). The constraint''s nature changes — from religious-institution extraction to majoritarian-state extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_law_framework_neutrality, conceptual, 'Whether secular legal framework is genuinely neutral or disguised majoritarian imposition').

omega_variable(
    religious_authority_coordination_function_real,
    'Do personal law regimes solve genuine coordination problems (family law certainty, inheritance clarity, marriage recognition within community) or are they primarily extraction mechanisms with secondary coordination?',
    'Comparative study: what happens when individuals voluntarily exit personal law regime? Do they gain coordination benefits (secular law clarity, individual autonomy) that offset losses (community stability, tradition transmission)? What proportion of personal-law subjects would freely choose the regime if exit cost were zero?',
    'If coordination-primary: rope classification for religious authorities is justified; the regime solves real problems. If extraction-primary: the regime should be classified snare with beneficiary actors (religious institutions) that maintain it primarily for power, not coordination. This affects remedial strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_authority_coordination_function_real, empirical, 'Whether personal law regimes solve genuine coordination or function primarily as extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exit_cost_structure, 1947, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exit_tr_t1947, exit_cost_structure, theater_ratio, 1947, 0.25).
narrative_ontology:measurement(exit_tr_t1975, exit_cost_structure, theater_ratio, 1975, 0.32).
narrative_ontology:measurement(exit_tr_t2000, exit_cost_structure, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(exit_tr_t2010, exit_cost_structure, theater_ratio, 2010, 0.46).
narrative_ontology:measurement(exit_tr_t2020, exit_cost_structure, theater_ratio, 2020, 0.48).

% Extraction over time
narrative_ontology:measurement(exit_be_t1947, exit_cost_structure, base_extractiveness, 1947, 0.35).
narrative_ontology:measurement(exit_be_t1975, exit_cost_structure, base_extractiveness, 1975, 0.42).
narrative_ontology:measurement(exit_be_t2000, exit_cost_structure, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(exit_be_t2010, exit_cost_structure, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(exit_be_t2020, exit_cost_structure, base_extractiveness, 2020, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(exit_su_t1947, exit_cost_structure, suppression_requirement, 1947, 0.4).
narrative_ontology:measurement(exit_su_t1975, exit_cost_structure, suppression_requirement, 1975, 0.48).
narrative_ontology:measurement(exit_su_t2000, exit_cost_structure, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(exit_su_t2010, exit_cost_structure, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(exit_su_t2020, exit_cost_structure, suppression_requirement, 2020, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exit_cost_structure, identity_coordination).
narrative_ontology:boltzmann_floor_override(exit_cost_structure, 0.12).
narrative_ontology:affects_constraint(exit_cost_structure, gender_asymmetry_in_family_law).
narrative_ontology:affects_constraint(exit_cost_structure, religious_coalition_electoral_capture).
narrative_ontology:affects_constraint(exit_cost_structure, uniform_civil_code_constitutional_commitment).

% DUAL FORMULATION NOTE:
% The exit-cost-structure constraint is upstream of more specific constraints within India's legal pluralism. Gender asymmetry in family law (e.g., unilateral talaq, differential inheritance) is a downstream constraint enabled by the parallel personal-law regime. Religious coalition electoral capture constrains the constitutional state's capacity to reform. The uniform civil code commitment is a sibling constraint reflecting the reform trajectory. All three are linked through network.affects_constraints: the exit-cost structure amplifies extraction in gender-asymmetric regimes and constrains the state's capacity to implement uniform law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exit_cost_structure, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
