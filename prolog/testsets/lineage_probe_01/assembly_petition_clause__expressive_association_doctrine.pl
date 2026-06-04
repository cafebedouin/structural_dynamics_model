% ============================================================================
% CONSTRAINT STORY: assembly_petition_clause__expressive_association_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_assembly_petition_clause__expressive_association_doctrine, []).

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
 *   constraint_id: assembly_petition_clause__expressive_association_doctrine
 *   human_readable: Expressive Association Doctrine: Organizational Integrity vs. State Transparency Demands
 *   domain: constitutional_law/association_rights
 *
 * SUMMARY:
 *   The expressive association doctrine represents a constitutional
 *   development in which assembly rights matured into a doctrine protecting
 *   organizations' and associations' expressive integrity. Beginning with
 *   NAACP v. Alabama (1958), which protected member list confidentiality
 *   against state compulsion, the doctrine evolved to protect organizations'
 *   rights to control membership, message, and expressive composition. This
 *   doctrine operates as a constraint on state power: states cannot compel
 *   disclosure of associational membership or force organizations to include
 *   members whose presence contradicts the organization's expressive mission.
 *   The constraint exhibits tangled-rope structure because it serves a
 *   genuine coordination function (organizations must maintain internal
 *   expressive coherence to function as collective speakers) while
 *   simultaneously suppressing state transparency demands. The doctrine
 *   benefits expressive organizations by protecting their internal autonomy
 *   and message control; it harms compelled-disclosure regimes and
 *   forced-inclusion mandates. The baseline extractiveness (0.32) reflects
 *   moderate asymmetry — the doctrine provides real protection to
 *   organizations while creating real costs for state transparency goals and
 *   for individuals subject to forced inclusion. The theater ratio (0.35) is
 *   relatively low because the coordination problem the doctrine addresses
 *   (organizational message integrity) is genuine, not primarily
 *   performative, though it coexists with state surveillance pressures the
 *   doctrine helps suppress.
 *
 * KEY AGENTS:
 *   - Expressive Organizations (institutional/arbitrage): Primary beneficiaries — NAACP, parade organizers, advocacy groups gaining protection for membership confidentiality and message control
 *   - Compelled Disclosers (powerless/trapped): Primary victims — individuals pressured to register membership or disclose affiliations facing state surveillance
 *   - Forced-Inclusion Targets (powerless/identity_locked): Secondary victims — parade organizers and organizations compelled to include banner-carriers or members whose presence contradicts organizational expressive mission; identity-fused with the organization's message
 *   - State Licensing Authorities (institutional/constrained): Institutional actors whose surveillance and disclosure powers are suppressed by the doctrine; bureaucratic interest in membership registration degraded from coordination function to surveillance infrastructure
 *   - Analytical Observer (analytical/analytical): Sees the doctrine as balancing organizational coordination (genuine need) against state reach suppression (asymmetric extraction protection)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(assembly_petition_clause__expressive_association_doctrine, 0.32).
domain_priors:suppression_score(assembly_petition_clause__expressive_association_doctrine, 0.48).
domain_priors:theater_ratio(assembly_petition_clause__expressive_association_doctrine, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(assembly_petition_clause__expressive_association_doctrine, extractiveness, 0.32).
narrative_ontology:constraint_metric(assembly_petition_clause__expressive_association_doctrine, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(assembly_petition_clause__expressive_association_doctrine, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(assembly_petition_clause__expressive_association_doctrine, tangled_rope).
narrative_ontology:human_readable(assembly_petition_clause__expressive_association_doctrine, "Expressive Association Doctrine: Organizational Integrity vs. State Transparency Demands").
narrative_ontology:topic_domain(assembly_petition_clause__expressive_association_doctrine, "constitutional_law/association_rights").

domain_priors:requires_active_enforcement(assembly_petition_clause__expressive_association_doctrine).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(assembly_petition_clause__expressive_association_doctrine, '5cc60f30-292c-49d7-9944-ef632bcbd168').
narrative_ontology:cs_kernel_codification('5cc60f30-292c-49d7-9944-ef632bcbd168', fixed_text).
narrative_ontology:cs_authority_grounding('5cc60f30-292c-49d7-9944-ef632bcbd168', lineage).
narrative_ontology:cs_interpretation_layer_present('5cc60f30-292c-49d7-9944-ef632bcbd168').
narrative_ontology:cs_reading_relation('5cc60f30-292c-49d7-9944-ef632bcbd168', assembly_petition_clause__permit_system_limits, coexists_with).
narrative_ontology:cs_reading_relation('5cc60f30-292c-49d7-9944-ef632bcbd168', assembly_petition_clause__petition_clause_independence, influences).
narrative_ontology:cs_axiom('5cc60f30-292c-49d7-9944-ef632bcbd168', foundational, organizations_require_expressive_autonomy).
narrative_ontology:cs_axiom_status(organizations_require_expressive_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('5cc60f30-292c-49d7-9944-ef632bcbd168', organizations_require_expressive_autonomy, instrumental).
narrative_ontology:cs_axiom('5cc60f30-292c-49d7-9944-ef632bcbd168', foundational, association_includes_exclusion_right).
narrative_ontology:cs_axiom_status(association_includes_exclusion_right, holdable).
narrative_ontology:cs_axiom_grounding('5cc60f30-292c-49d7-9944-ef632bcbd168', association_includes_exclusion_right, deontological).
narrative_ontology:cs_reference_frame('5cc60f30-292c-49d7-9944-ef632bcbd168', organizational_expressive_autonomy_framework).
narrative_ontology:cs_drift_state('5cc60f30-292c-49d7-9944-ef632bcbd168', contemporary_pluralist_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5cc60f30-292c-49d7-9944-ef632bcbd168', '').
narrative_ontology:cs_kernel_id(assembly_petition_clause__expressive_association_doctrine, assembly_petition_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(assembly_petition_clause__expressive_association_doctrine, expressive_organizations).
narrative_ontology:constraint_beneficiary(assembly_petition_clause__expressive_association_doctrine, advocacy_groups).
narrative_ontology:constraint_victim(assembly_petition_clause__expressive_association_doctrine, compelled_disclosure_regimes).
narrative_ontology:constraint_victim(assembly_petition_clause__expressive_association_doctrine, forced_inclusion_mandates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPELLED DISCLOSER (SNARE) — Individuals and small organizations pressured to register membership, disclose affiliations, or abandon association entirely. No exit: remaining silent forfeits the associational benefit; speaking reveals identity to hostile state surveillance. Experiences pure extraction with minimal coordination benefit. The suppression mechanism is total — participate and be tracked, or refrain and lose voice.
constraint_indexing:constraint_classification(assembly_petition_clause__expressive_association_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS ORGANIZATION (TANGLED ROPE) — Organizations like NAACP coordinate member action (genuine coordination function) while protecting members from state retaliation through confidentiality doctrine (asymmetric extraction protection). The organization benefits from expressive integrity; members benefit from protected association; but the organization must continuously litigate to maintain member confidentiality against state demands. Significant coordination value coupled with ongoing extraction pressure from compelled-disclosure regimes.
constraint_indexing:constraint_classification(assembly_petition_clause__expressive_association_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED ADVOCACY ORGANIZATION (ROPE) — Mature organizations with institutional resources (ACLU, Sierra Club, advocacy foundations) coordinate their members' actions and expressive output. The expressive association doctrine primarily benefits these institutional actors through protection of their internal coherence and message control. Low extraction experienced because these organizations have resources to defend their associational choices and benefit from the doctrine's protection of their message autonomy. This is their native coordination mechanism.
constraint_indexing:constraint_classification(assembly_petition_clause__expressive_association_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FORCED-INCLUSION VICTIM (SNARE) — Individuals compelled to associate with organizations or messages against their identity or conscience (parade organizers forced to include banner-bearers, organizations forced to accept members whose presence contradicts organizational mission). Exit is identity-shattering: leaving means abandoning organizational identity or accepting forced association as the price of membership. The suppression operates through identity fusion — the victim cannot exit without becoming someone different. This is pure extraction of the victim's expressive integrity.
constraint_indexing:constraint_classification(assembly_petition_clause__expressive_association_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE LICENSING AUTHORITY (PITON) — Government agencies claim interest in transparency and nondiscrimination, but the actual function of membership registration and disclosure requirements has degraded from administrative efficiency to surveillance infrastructure. The licensing theater persists through bureaucratic routine and statutory mandate, though courts increasingly recognize it as performance masking state monitoring capability. The enforcement mechanism is formal but the coordination function has atrophied.
constraint_indexing:constraint_classification(assembly_petition_clause__expressive_association_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the expressive association doctrine balances two legitimate but competing coordination problems: (1) organizations must coordinate internal expression and membership selection (genuine coordination function), and (2) state surveillance of association creates asymmetric extraction risks (victim set). The doctrine protects the first while suppressing the second. This is the canonical tangled rope case — real coordination benefit coupled with real extraction suppression mechanism. The doctrine's legitimacy turns on whether the suppression of state reach produces net reduction in extraction (yes empirically for protected organizations; mixed for organizations in adverse political climates).
constraint_indexing:constraint_classification(assembly_petition_clause__expressive_association_doctrine, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(assembly_petition_clause__expressive_association_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(assembly_petition_clause__expressive_association_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(assembly_petition_clause__expressive_association_doctrine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(assembly_petition_clause__expressive_association_doctrine, TR),
    TR >= 0.70.

:- end_tests(assembly_petition_clause__expressive_association_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The doctrine protects organizations from state transparency demands while enabling organizations to exclude members. This creates asymmetric benefit flow — organizations gain protection; the public/state loses transparency access; compelled disclosers lose anonymity protection without the doctrine. The 0.32 reflects that the doctrine serves a real coordination function (organizations need expressive integrity to function), but this function coexists with suppression of state reach (asymmetric). Over the interval (t=0 to t=50, representing pre-doctrine through post-expansion era), extractiveness rises as the doctrine expands from membership protection to forced-inclusion protection, gradually increasing state suppression from 0.18 to 0.32. Suppression (0.48): Moderate-high. The doctrine suppresses state transparency demands (compelled disclosure prohibited), suppresses forced-inclusion mandates (organizations can exclude), and suppresses state licensing discretion (though this is more prominent in the permit-system-limits reading). The suppression mechanism is doctrinal — state cannot compel disclosure or force inclusion. Suppression is not total because states retain some alternative transparency mechanisms (financial disclosures, tax filings, public records); but core surveillance interest in membership rolls is heavily suppressed. Theater ratio (0.35): Low-moderate. The coordination problem the doctrine addresses (organizations need internal message coherence) is genuine — there is real functional requirement for organizations to control membership and message. However, the doctrine also serves state-suppression functions that are somewhat performative — protecting organizational 'purity' can become theatrical (excluding members whose presence merely dilutes brand identity rather than materially compromising organizational function). The theater ratio rises slightly over the interval as doctrine expands from protective (NAACP confidentiality — clearly functional) to message-control (parade organizers excluding banner-carriers — more theater-prone).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range. Established advocacy organizations see rope — the doctrine enables them to coordinate expressive action with protected membership, genuine coordination benefit. Compelled disclosers see snare — surveillance exposure with no exit. Forced-inclusion targets see snare with identity-lock — organizational membership is their identity; forced inclusion dissolves their self-concept. The state sees piton — membership licensing persists as bureaucratic theater, but the doctrine has stripped it of coordination function. The analytical observer sees tangled rope — real coordination benefit (organizational message coherence) coexists with real asymmetric extraction (state reach suppression). The perspectival gap reflects that different agents experience the same doctrine through entirely different mechanisms: coordination, surveillance, identity fusion, bureaucratic routine, and abstract balance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) vary by agent's structural position relative to THIS constraint. Expressive organizations (institutional/arbitrage) benefit from the doctrine — state suppression is subordinated to their message autonomy — deriving low d (approximately 0.10-0.20, full beneficiaries with exit through arbitrage). Compelled disclosers (powerless/trapped) bear maximum extraction cost — surveillance exposure with no exit — deriving high d (approximately 0.95, full targets with trapped exit). Forced-inclusion targets (powerless/identity_locked) face identity-shattering exit — they are fused with the organization's expressive mission — deriving high d (approximately 0.89, full targets with identity-locked exit). The analytical observer (analytical/analytical) occupies canonical d ≈ 0.72 (observer position). The derived chi values follow: beneficiaries experience low effective extraction (state reach suppressed); victims experience high effective extraction (forced to accept state surveillance or organizational exclusion). The perspectival gap arises from these differentiated d values — the same doctrine produces rope (or near-rope) from the beneficiary's view and snare from the victim's view.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by distinguishing genuine coordination (organizations need internal message coherence to function collectively) from asymmetric extraction suppression (state surveillance interests are subordinated). The doctrine is legitimately tangled rope because it solves a real collective action problem (how do organizations maintain expressive coherence while protecting members from retaliation) while simultaneously suppressing state transparency demands. The doctrine is NOT pure extraction masquerading as coordination (that would be snare) because the coordination function — organizations coordinating expressive output and protecting members — is empirically real and functionally necessary. The doctrine is NOT pure coordination (that would be rope) because it suppresses state reach in ways that go beyond coordination necessity (message-purity exclusions, parade-banner cases). The tangled classification correctly captures that both elements are structurally real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_surveillance_intensity_threshold,
    'At what level of state hostility does the suppression-of-state-reach function of expressive association shift from net-beneficial coordination protection to rent-extraction protection?',
    'Historical comparison: membership protection doctrine''s protective value in neutral/supportive regimes vs. authoritarian climates; correlation between state persecution level and doctrine-driven litigation frequency; empirical tracking of associational freedom in regimes with vs. without expressive association protection',
    'If threshold is high (only extreme persecution): doctrine is primarily for organizations in favorable legal climates, making it inadvertently class-stratified (well-resourced organizations litigate; vulnerable organizations comply). If threshold is low (routine surveillance pressure): doctrine is essential coordination protection, legitimizing suppression of state disclosure demands as necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_surveillance_intensity_threshold, empirical, 'Threshold at which association protection shifts from coordination to extraction defense').

omega_variable(
    forced_inclusion_coordination_necessity,
    'Is forced inclusion a legitimate coordination problem (organizations genuinely unable to function with message dilution) or a disguised extraction mechanism (organizations protecting insider privilege)?',
    'Comparative case analysis: organizations that successfully exclude members vs. those forced to include; measurement of organizational effectiveness with vs. without message control; interviews with parade organizers about actual coordination costs of banner-carriers with different messages; empirical test of whether message dilution materially harms organizational function or merely dilutes brand control',
    'If primarily coordination: expressive association doctrine is essential protection; forced inclusion is extraction of organizational integrity. If primarily privilege protection: doctrine becomes protection of insider message control at cost of outsider voice suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forced_inclusion_coordination_necessity, empirical, 'Whether forced inclusion represents genuine coordination failure or privilege protection').

omega_variable(
    doctrine_class_stratification,
    'Does expressive association doctrine provide stronger protection to well-resourced organizations that can afford litigation (institutional power/arbitrage exit) than to vulnerable organizations (powerless/trapped exit)?',
    'Empirical tracking: litigation success rates by organization size/resources; availability and cost of legal defense for membership protection claims; comparison of compelled-disclosure compliance rates across organization types; analysis of whether doctrine''s main beneficiaries are established advocacy institutions vs. grassroots organizations',
    'If strongly stratified: doctrine is inadvertently a form of class-privileged expressive protection, leaving vulnerable organizations'' members exposed to surveillance despite formal doctrine. If weakly stratified: doctrine provides meaningful protection across organization types.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_class_stratification, empirical, 'Whether doctrine protection is stratified by organizational resources and power').

omega_variable(
    this_reading_vs_permit_system_limits,
    'Does the expressive association doctrine''s focus on membership integrity foreclose or coexist with permit system limits doctrine''s focus on content-neutral licensing standards?',
    'Legal doctrine analysis: can both readings be held within a single constitutional framework? (Yes: membership protection is distinct from licensing standards.) Does acceptance of expressive association doctrine require rejection of permit discretion limits? (No: both can constrain state action on different axes.) Is there logical conflict between prioritizing internal expressive integrity vs. external licensing neutrality? (No: different domains.)',
    'This reading COEXISTS_WITH permit system limits. They address different state actions — this reading addresses forced disclosure and forced inclusion; permit system limits address licensing discretion. Both can operate simultaneously without logical contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(this_reading_vs_permit_system_limits, conceptual, 'Relationship between expressive association doctrine and permit system limits reading').

omega_variable(
    this_reading_vs_petition_clause_independence,
    'Does the expressive association doctrine''s foundation in assembly-maturation foreclose the petition clause as a distinct, independent right?',
    'Constitutional architecture: are petition rights fully subsumed in assembly/expressive association doctrine? (Partially: petition is about demanding governmental response, distinct from assembly''s expressive coordination.) Does emphasis on expressive association necessarily minimize petition''s independent force? (Yes: petition gets read as subset of expression rather than as distinct categorical right.) Can both be held simultaneously? (Yes, but only if petition is recognized as structurally separate from assembly.)',
    'This reading INFLUENCES but does NOT FORECLOSE petition clause independence. By subordinating petition to expressive association doctrine, this reading creates downstream pressure on petition independence — petition must establish its own doctrinal ground rather than standing alongside assembly. Sibling petition reading must reconstruct petition as categorically distinct, not merely a form of expression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(this_reading_vs_petition_clause_independence, conceptual, 'Whether expressive association doctrine framework leaves room for petition clause independence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(assembly_petition_clause__expressive_association_doctrine, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(asse_tr_t0, assembly_petition_clause__expressive_association_doctrine, theater_ratio, 0, 0.28).
narrative_ontology:measurement(asse_tr_t25, assembly_petition_clause__expressive_association_doctrine, theater_ratio, 25, 0.32).
narrative_ontology:measurement(asse_tr_t50, assembly_petition_clause__expressive_association_doctrine, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(asse_be_t0, assembly_petition_clause__expressive_association_doctrine, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(asse_be_t25, assembly_petition_clause__expressive_association_doctrine, base_extractiveness, 25, 0.26).
narrative_ontology:measurement(asse_be_t50, assembly_petition_clause__expressive_association_doctrine, base_extractiveness, 50, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(assembly_petition_clause__expressive_association_doctrine, identity_coordination).
narrative_ontology:affects_constraint(assembly_petition_clause__expressive_association_doctrine, assembly_petition_clause__permit_system_limits).
narrative_ontology:affects_constraint(assembly_petition_clause__expressive_association_doctrine, assembly_petition_clause__petition_clause_independence).

% DUAL FORMULATION NOTE:
% The assembly/petition kernel decomposes into three structurally distinct constraints: (1) EXPRESSIVE_ASSOCIATION_DOCTRINE (this story) — organizations' membership and message control (ε≈0.32); (2) PERMIT_SYSTEM_LIMITS (sibling) — content-neutral licensing standards preventing official discretion (ε≈0.15, lower extractiveness because the coordination problem is simpler); (3) PETITION_CLAUSE_INDEPENDENCE (sibling) — petition as a separate constitutional right distinct from expressive association (ε≈0.48, higher extractiveness because petition's modern subordination to expression involves asymmetric suppression of petition's categorical force). All three readings operate within the contested kernel, but each has distinct ε, distinct beneficiary/victim structure, and distinct constitutional mechanics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(assembly_petition_clause__expressive_association_doctrine, powerless, 0.95).
constraint_indexing:directionality_override(assembly_petition_clause__expressive_association_doctrine, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
