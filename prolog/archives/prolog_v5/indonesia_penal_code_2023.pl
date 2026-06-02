% ============================================================================
% CONSTRAINT STORY: indonesia_penal_code_2023
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indonesia_penal_code_2023, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: indonesia_penal_code_2023
 *   human_readable: Indonesia's 2023 Penal Code (KUHP) Criminalizing Personal Liberties
 *   domain: political/social
 *
 * SUMMARY:
 *   Indonesia's 2023 Penal Code (KUHP) represents a significant institutional
 *   shift toward state criminalization of personal liberties, particularly
 *   consensual intimate conduct and political expression. The code
 *   criminalizes extramarital sex, cohabitation by unmarried couples,
 *   same-sex relationships (through sodomy and morality clauses), and
 *   insulting the president or state institutions. This constraint exhibits
 *   the classic snare structure: high extraction (career damage,
 *   incarceration risk, social stigma), high suppression (state enforcement
 *   machinery + community policing), and moderate theater (framing as moral
 *   protection obscures extraction mechanism). However, the constraint also
 *   manifests piton characteristics—the code revises and partially restores
 *   colonial-era legal templates, creating institutional inertia rather than
 *   organic innovation. The theatrical component increases as the code faces
 *   international criticism and domestic resistance: official rhetoric
 *   emphasizes consensus-building and legal modernization while enforcement
 *   remains selective and opaque. The constraint analysis reveals multiple
 *   structural tensions: between state beneficiaries (institutional
 *   apparatus, conservative religious coalition) and victims (unmarried
 *   couples, LGBTQ individuals, political critics); between international
 *   treaty obligations (CEDAW, ICCPR) and domestic law; between formal legal
 *   processes (providing piton-level legitimacy) and informal social
 *   enforcement (providing snare-level extraction through gossip, family
 *   pressure, religious condemnation). The international human rights
 *   coalition perceives a scaffold structure with a generational sunset:
 *   demographic shifts and global norm diffusion will eventually undermine
 *   political support for criminalizing personal conduct.
 *
 * KEY AGENTS:
 *   - State Institutional Apparatus: Primary beneficiary (institutional/arbitrage) — gains enforcement authority, moral legitimacy, and capacity to suppress opposition through law
 *   - Conservative Religious Coalition: Primary beneficiary (institutional/arbitrage) — institutionalizes religious moral frameworks; mobilizes constituencies; outsources enforcement to communities
 *   - Unmarried Couples: Primary victim (powerless/trapped) — criminalized for consensual behavior; no legal exit within Indonesia; subjected to selective prosecution and social stigma
 *   - LGBTQ Individuals: Primary victim (powerless/trapped) — identity-based criminalization; permanent trap requiring either psychological coercion (conformity) or emigration
 *   - Political Critics: Secondary victim (moderate/constrained) — insulting-state-institutions clause constrains free expression; have procedural protections but are asymmetrically targeted
 *   - International Human Rights Coalition: Organized observer (organized/constrained) — sees constraint as temporary institutional failure with generational sunset via demographic/norm diffusion
 *   - Civil Society Organizations: Secondary victim (moderate/constrained) — face surveillance and selective prosecution for advocacy; constrained exit (cannot fully operate; some emigration of leadership)
 *   - Post-Colonial Legal Institutions: Structural holder (institutional/arbitrage) — benefit from institutional inertia in maintaining colonial-derived legal templates; experience constraint as coordination mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indonesia_penal_code_2023, 0.58).
domain_priors:suppression_score(indonesia_penal_code_2023, 0.72).
domain_priors:theater_ratio(indonesia_penal_code_2023, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indonesia_penal_code_2023, extractiveness, 0.58).
narrative_ontology:constraint_metric(indonesia_penal_code_2023, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(indonesia_penal_code_2023, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indonesia_penal_code_2023, snare).
narrative_ontology:human_readable(indonesia_penal_code_2023, "Indonesia's 2023 Penal Code (KUHP) Criminalizing Personal Liberties").
narrative_ontology:topic_domain(indonesia_penal_code_2023, "political/social").

domain_priors:requires_active_enforcement(indonesia_penal_code_2023).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indonesia_penal_code_2023, state_institutional_apparatus).
narrative_ontology:constraint_beneficiary(indonesia_penal_code_2023, conservative_religious_coalition).
narrative_ontology:constraint_victim(indonesia_penal_code_2023, unmarried_couples).
narrative_ontology:constraint_victim(indonesia_penal_code_2023, lgbtq_individuals).
narrative_ontology:constraint_victim(indonesia_penal_code_2023, political_critics).
narrative_ontology:constraint_victim(indonesia_penal_code_2023, civil_society_organizations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNMARRIED COUPLE (SNARE) — Trapped within Indonesian jurisdiction with no legal exit option. Criminalized for consensual private behavior. Cannot organize, migrate without severe cost, or challenge the constraint through institutional channels. Maximum suppression: state enforcement authority + social policing mechanisms create near-total behavioral chilling effect. No coordination benefit to offset extraction.
constraint_indexing:constraint_classification(indonesia_penal_code_2023, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LGBTQ INDIVIDUAL (SNARE) — Identity-based criminalization creates permanent trap. Exit options are binary: conform sexuality to legal/social norm (psychological coercion) or emigrate. Trapped agents experience maximum structural extraction. Social suppression layer amplifies state enforcement — peer and family surveillance supplement formal law enforcement. Theater ratio high: rhetorical framing as 'protecting morality' obscures extraction mechanism.
constraint_indexing:constraint_classification(indonesia_penal_code_2023, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: POLITICAL CRITIC (TANGLED ROPE) — Moderate power and constrained exit. The code provides coordination function (civil society reporting mechanisms, legal remedy framework) but weaponized against critics. Insulting state institutions clause creates asymmetric extraction: critics cannot freely express dissent, but the state can prosecute. Benefits from rule-of-law infrastructure (ability to mount defense, appellate process) but constrained by the code's design. Moderate extraction rather than maximum snare because institutional processes provide some procedural protection.
constraint_indexing:constraint_classification(indonesia_penal_code_2023, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE INSTITUTIONAL APPARATUS (ROPE) — Primary beneficiary. Gains enforcement authority, moral legitimacy framing, and capacity to suppress organized opposition. Experiences the constraint as a coordination mechanism: centralizes power distribution, clarifies enforcement hierarchy, and provides legal cover for state agents' discretionary enforcement. Can exit (repeal the code) but has no incentive. Arbitrage position: the code enables state to extract maximum compliance from citizens while maintaining institutional legitimacy through formalized legal process.
constraint_indexing:constraint_classification(indonesia_penal_code_2023, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSERVATIVE RELIGIOUS COALITION (ROPE) — Secondary beneficiary. Code institutionalizes religious moral frameworks into state enforcement mechanism. Coordination benefit: mobilizes religious constituencies, legitimizes religious authority within law, reduces enforcement costs by outsourcing social policing to religious communities. Has arbitrage exit (code repeal) but benefits from status quo. Experiences constraint as low-cost coordination of moral enforcement.
constraint_indexing:constraint_classification(indonesia_penal_code_2023, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL HUMAN RIGHTS COALITION (SCAFFOLD) — Organized but geographically constrained (limited enforcement power within Indonesia). Sees the code as a temporary institutional failure with a structural sunset: demographic shifts (Gen Z has lower acceptance of criminalization), global norm diffusion (ASEAN peers decriminalizing), economic incentives (tourism/FDI damage from human rights concerns), and generational pressure on legal institutions. Coordination function: international treaty obligations (CEDAW, ICCPR ratifications create internal tensions with KUHP). Theater remains high because compliance rhetoric masks non-compliance reality (uneven enforcement, circumvention practices).
constraint_indexing:constraint_classification(indonesia_penal_code_2023, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: COLONIAL LEGAL LEGACY (PITON) — The KUHP 2023 is partially a restoration/update of Dutch colonial criminal code architecture (the prior KUHP was inherited from 1918 Dutch law). From a civilizational view, the constraint represents institutional inertia: the code persists because legal infrastructure defaults to existing templates, institutional actors are trained in it, and replacement would require comprehensive legal rewriting. Theater ratio high: the new code is framed as 'modernization' when it largely codifies existing practices with added specificity to personal conduct. The underlying coordination structure (hierarchical criminal administration) is degraded by the code's overreach — it attempts to regulate behavior beyond what formal law can efficiently enforce (private intimate conduct), creating performative enforcement (selective prosecution for political/social purposes).
constraint_indexing:constraint_classification(indonesia_penal_code_2023, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indonesia_penal_code_2023_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indonesia_penal_code_2023, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indonesia_penal_code_2023, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indonesia_penal_code_2023, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indonesia_penal_code_2023, TR),
    TR >= 0.70.

:- end_tests(indonesia_penal_code_2023_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The code extracts behavioral compliance from powerless agents (unmarried couples, LGBTQ individuals) through criminalization threat, incarceration risk, and career/social damage. However, extractiveness is not maximal (0.66+) because: (1) enforcement is selective rather than systematic, reducing effective extraction for some groups; (2) formal legal processes provide procedural protections that create a thin layer of institutional legitimacy and appeal options; (3) exit through emigration is possible (though costly) for some agents, creating a constrained rather than absolute trap for the most privileged among targeted groups. The trajectory shows increasing extractiveness over the interval as enforcement becomes more confident and community policing mechanisms strengthen. Suppression (0.72): High. Multiple suppression mechanisms operate simultaneously: state enforcement authority (formal prosecution), social stigma (informal punishment via gossip and family pressure), community policing (religious and family-based surveillance), and psychological coercion (agents internalize the criminalization and self-censor). Suppression exceeds the snare minimum (0.60) because the code reaches into private intimate conduct, not just public behavior — this maximizes both state and social enforcement reach. Theater ratio (0.65): Moderate-high. The code is partially performative: official rhetoric emphasizes moral protection and modernization, but the underlying mechanism is behavioral extraction. Enforcement patterns reveal political targeting (critics prosecuted more aggressively than unmarried couples in some contexts), suggesting that the formal legal structure serves as theater for selective state power. Theater is not maximal (0.70+) because some enforcement is consistent enough to create genuine behavioral chilling effects, and the institutional legitimacy is partially real (some Indonesian constituencies genuinely support criminalization for religious reasons).
 *
 * PERSPECTIVAL GAP:
 *   The snare vs. rope gap is maximal. From the beneficiary view (state/religious coalition, institutional power, arbitrage exit), the code is a coordination mechanism with net benefit—it organizes society, legitimizes authority, and costs beneficiaries little (they are not targets). From the victim view (unmarried couples, powerless, trapped), the code is pure extraction with no coordination benefit—it constrains behavior, risks incarceration, and provides no reciprocal service. The political critic occupies the tension: they benefit from the rule-of-law infrastructure (due process, appeal rights) but are asymmetrically targeted by the same code. The international coalition sees a different gap: between the code's institutional form (legitimate legal process) and its functional degradation (selective enforcement, piton theater). The analytical observer risks the mountain error—treating the code as an inevitable feature of state capacity (all states need moral enforcement mechanisms)—but the structural data reveals this as naturalization of a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each agent's structural relationship to the constraint. Beneficiaries (state apparatus, religious coalition) have low d values (around 0.10-0.20, derived from institutional power + arbitrage exit): they experience low or negative extraction. Victims with trapped exit (unmarried couples, LGBTQ individuals) have high d values (0.90-0.95, derived from powerless position + trapped exit): they experience maximum extraction. The political critic has moderate d (around 0.60-0.70, derived from moderate power + constrained exit): they experience significant but not maximal extraction because institutional processes provide some protection and some agency. The international coalition has analytical d (around 0.72-0.80, derived from organized power + constrained geographic exit): they see the constraint from outside, with agency to advocate but limited enforcement power. The piton perspective operates outside the normal directionality computation—it treats the constraint's functional degradation as a structural property independent of who benefits or who bears costs. The theater-driven classification reveals that the code's legitimacy increasingly relies on performative compliance rather than functional enforcement, which is a hallmark of piton degradation. No overrides are necessary because the beneficiary/victim declarations and exit options sufficiently capture the directional relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED — The constraint exhibits the core mandatrophy: beneficiaries see pure coordination (Rope), victims see pure extraction (Snare), and both readings are structurally valid. The resolution comes from recognizing that the snare and rope are not contradictory perspectives on the same constraint—they are perspectival readings of an asymmetric distribution of extraction and coordination benefits. The code simultaneously: (1) solves a coordination problem for beneficiaries (how to institutionalize moral norms into law), and (2) extracts behavioral compliance from victims (through criminalization and suppression). Both are real. The mandate resolution (choosing between snare and rope as 'the' type) fails because it presumes a single objective constraint. Instead, the constraint is a presheaf over the observation site: each observer's structural position (power level, exit options, beneficiary/victim status) yields a legitimate classification. The analytical task is to chart the perspectival space, not to collapse it to a single type. The mandatrophy dissolves when the observer realizes that the snare-for-victims and rope-for-beneficiaries are not measurement errors—they are the constraint's true structure. The code is snare-shaped from below and rope-shaped from above, and this inversion of perspective is not a paradox; it is the defining feature of asymmetric institutional power. The engine's computation of mandatrophy_resolved=true for extractiveness > 0.70 is satisfied here (extractiveness = 0.58 does not trigger that requirement, but the perpectival analysis confirms mandatrophy resolution through structural understanding).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_threshold,
    'What distinguishes selective prosecution (political targeting) from systemic enforcement of the criminalization?',
    'Longitudinal analysis of prosecution patterns: do enforcement rates vary by socioeconomic status, political affiliation, or religious identity? Comparison with pre-2023 practice to identify whether new code changes enforcement probability.',
    'If enforcement is selective/political: constraint is snare with piton characteristics (institutionalized oppression with inconsistent application). If enforcement is systemic: constraint is more effectively coercive snare but loses piton theater (becomes efficient extraction rather than degraded ritual).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_selectivity_threshold, empirical, 'Whether enforcement is selective political targeting or systematic').

omega_variable(
    compliance_internalization_mechanisms,
    'Do criminalization effects operate primarily through state enforcement or through internalized social norms and community policing?',
    'Behavioral studies pre/post KUHP 2023: self-reported behavior change, survey data on perception of legal risk vs. social disapproval, ethnographic observation of community enforcement mechanisms vs. formal law enforcement.',
    'If state enforcement dominant: snare classification is accurate (extraction flows from institutional power). If social internalization dominant: suppression floor increases but benefits shift partially to conservative religious community (making constraint more hybrid). If mixed: constraint exhibits both snare (state) and rope (community coordination) simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_internalization_mechanisms, empirical, 'Relative weight of state enforcement vs. social norm internalization').

omega_variable(
    exit_migration_elasticity,
    'What fraction of trapped agents (unmarried couples, LGBTQ individuals) can and will exit through emigration vs. remaining subjected to constraint?',
    'Migration flow data post-KUHP 2023 (visa applications, emigration rates by demographic cohort, destination countries); survey data on migration intention among criminalized groups.',
    'If exit rate > 15%: snare classification weakened (some agents escape). If exit rate < 5%: snare confirmed as near-total trap. Exit patterns reveal which agent groups experience maximum vs. moderate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_migration_elasticity, empirical, 'Fraction of targeted individuals who emigrate vs. internalize constraint').

omega_variable(
    international_treaty_enforcement_gap,
    'Does Indonesia''s ratification of CEDAW and ICCPR (which protect personal liberty and non-discrimination) create a binding enforcement mechanism that contradicts KUHP 2023?',
    'International court proceedings (ICJ, CEDAW committee complaints), treaty body review cycles, enforceability of committee recommendations within Indonesian legal hierarchy.',
    'If treaties are enforceable: scaffold sunset mechanism is real (international pressure creates generational path to repeal). If treaties are non-binding: international human rights coalition perspective becomes aspirational (low organizational power despite coordinated rhetoric). Mandatrophy resolution depends on this gap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_treaty_enforcement_gap, conceptual, 'Whether international treaty obligations create enforceable contradiction with KUHP 2023').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indonesia_penal_code_2023, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(idnkuhp_tr_t0, indonesia_penal_code_2023, theater_ratio, 0, 0.58).
narrative_ontology:measurement(idnkuhp_tr_t6, indonesia_penal_code_2023, theater_ratio, 6, 0.62).
narrative_ontology:measurement(idnkuhp_tr_t12, indonesia_penal_code_2023, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(idnkuhp_be_t0, indonesia_penal_code_2023, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(idnkuhp_be_t6, indonesia_penal_code_2023, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(idnkuhp_be_t12, indonesia_penal_code_2023, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indonesia_penal_code_2023, enforcement_mechanism).
narrative_ontology:affects_constraint(indonesia_penal_code_2023, lgbtq_criminalization_southeast_asia).
narrative_ontology:affects_constraint(indonesia_penal_code_2023, political_speech_criminalization).
narrative_ontology:affects_constraint(indonesia_penal_code_2023, indonesian_civil_society_restrictions).

% DUAL FORMULATION NOTE:
% The KUHP 2023 constraint can be decomposed into three structurally distinct constraints with different ε values: (1) criminalization of consensual intimate conduct (ε ≈ 0.62, Snare) — primary extraction mechanism; (2) insulting-state-institutions clause (ε ≈ 0.55, Tangled Rope) — mixed coordination/extraction for political expression; (3) colonial legal infrastructure (ε ≈ 0.35, Piton) — institutional inertia and theater. These are linked via network relationships: the institutional inertia enables both criminalization clauses. In this story, they are unified under the claimed_type 'snare' because the primary structural force is extraction of personal liberty. Separate constraint stories with different ε values could analyze the political-speech dimension and colonial-legacy dimension independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
