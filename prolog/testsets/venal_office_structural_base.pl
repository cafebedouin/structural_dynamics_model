% ============================================================================
% CONSTRAINT STORY: venal_office_structural_base
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_venal_office_structural_base, []).

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
 *   constraint_id: venal_office_structural_base
 *   human_readable: Venal Office and the Remonstrance Right in Ancien Régime France
 *   domain: constitutional_history/political_economy
 *
 * SUMMARY:
 *   The venal office system in Ancien Régime France created a constitutional
 *   authority structure in which regional magistrates (Parlementary judges)
 *   purchased hereditary judicial and administrative posts, creating a
 *   proprietary interest in the governance order. The Crown needed these
 *   magistrates to register royal edicts before they could become law, and
 *   the magistrates asserted a 'remonstrance right' — the ability to delay
 *   registration and issue formal objections claiming constitutional
 *   violation. This constraint encodes a fundamental dispute about
 *   constitutional authority: did the remonstrance right represent a
 *   necessary check preserving the realm's unwritten constitutional order
 *   (magistrates' reading), or did it constitute minoritarian obstruction
 *   protecting elite fiscal privileges and blocking necessary taxation
 *   (Crown's reading)? The constraint exhibits all six DR types depending on
 *   the observer's structural position, revealing how a single institutional
 *   mechanism can simultaneously coordinate essential constitutional norms
 *   and extract fiscal privilege. The extractiveness measurement rises from
 *   0.35 (early period: genuine coordination of constitutional principles)
 *   through 0.48 (mid-period: coordination + extraction in equilibrium) to
 *   0.58 (late period: extraction increasingly dominant). Theater ratio
 *   similarly rises from 0.52 to 0.68, indicating that the institutional
 *   practice becomes increasingly performative — magistrates maintain the
 *   fiction of constitutional authority even as the underlying basis degrades
 *   into hereditary privilege. Suppression requirement rises from 0.45 to
 *   0.65, showing that the system requires increasing coercive enforcement as
 *   the Crown's fiscal needs rise and the magistrates' ability to justify
 *   obstruction as constitutional necessity weakens.
 *
 * KEY AGENTS:
 *   - Parlementary Magistrates: Primary beneficiaries (institutional/arbitrage) — extract fiscal privilege and social status through venal office; use remonstrance right to frame self-interest as constitutional principle
 *   - Crown / Central Fiscal Authority: Primary victim (institutional/trapped) — blocked from revenue collection and military mobilization necessary for state capacity and great-power competition
 *   - Provincial Nobility: Secondary beneficiary/victim (moderate/constrained) — benefit from remonstrance as check on Crown but lose tax-exempt privileges when Crown forces through reforms; constrained by dependence on Crown favor
 *   - Reform Movements and Enlightenment Advocates: Organized agents (organized/mobile) — see venal office as obstacle to rational administration; propose dismantling through written constitutions and centralized taxation
 *   - The Venal Office Institution: Institutional actor (institutional/arbitrage) — operates as piton: originally a delegation mechanism, degraded into hereditary privilege; maintains legitimacy through performative institutional ritual
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable constitutional principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(venal_office_structural_base, 0.58).
domain_priors:suppression_score(venal_office_structural_base, 0.65).
domain_priors:theater_ratio(venal_office_structural_base, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(venal_office_structural_base, extractiveness, 0.58).
narrative_ontology:constraint_metric(venal_office_structural_base, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(venal_office_structural_base, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(venal_office_structural_base, tangled_rope).
narrative_ontology:human_readable(venal_office_structural_base, "Venal Office and the Remonstrance Right in Ancien Régime France").
narrative_ontology:topic_domain(venal_office_structural_base, "constitutional_history/political_economy").

domain_priors:requires_active_enforcement(venal_office_structural_base).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(venal_office_structural_base, 'ad2a05d5-b783-441c-98e5-173a8320bb93').
narrative_ontology:cs_kernel_codification('ad2a05d5-b783-441c-98e5-173a8320bb93', implicit).
narrative_ontology:cs_authority_grounding('ad2a05d5-b783-441c-98e5-173a8320bb93', distributed).
narrative_ontology:cs_reading_relation('ad2a05d5-b783-441c-98e5-173a8320bb93', crown_fiscal_authority_ancien_regime, coexists_with).
narrative_ontology:cs_reading_relation('ad2a05d5-b783-441c-98e5-173a8320bb93', enlightenment_constitutional_rationalization, influences).
narrative_ontology:cs_axiom('ad2a05d5-b783-441c-98e5-173a8320bb93', foundational, remonstrance_right_constitutional_necessity).
narrative_ontology:cs_axiom_status(remonstrance_right_constitutional_necessity, holdable).
narrative_ontology:cs_axiom_grounding('ad2a05d5-b783-441c-98e5-173a8320bb93', remonstrance_right_constitutional_necessity, conventional).
narrative_ontology:cs_axiom('ad2a05d5-b783-441c-98e5-173a8320bb93', foundational, dispersed_authority_unwritten_constitutions).
narrative_ontology:cs_axiom_status(dispersed_authority_unwritten_constitutions, holdable).
narrative_ontology:cs_axiom_grounding('ad2a05d5-b783-441c-98e5-173a8320bb93', dispersed_authority_unwritten_constitutions, deontological).
narrative_ontology:cs_axiom('ad2a05d5-b783-441c-98e5-173a8320bb93', secondary, magistrate_proprietary_office_legitimacy).
narrative_ontology:cs_axiom_status(magistrate_proprietary_office_legitimacy, overridden).
narrative_ontology:cs_axiom_grounding('ad2a05d5-b783-441c-98e5-173a8320bb93', magistrate_proprietary_office_legitimacy, conventional).
narrative_ontology:cs_reference_frame('ad2a05d5-b783-441c-98e5-173a8320bb93', magistrate_constitutional_authority).
narrative_ontology:cs_drift_state('ad2a05d5-b783-441c-98e5-173a8320bb93', pre_revolutionary_crisis, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('ad2a05d5-b783-441c-98e5-173a8320bb93', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(venal_office_structural_base, parlementary_magistrates).
narrative_ontology:constraint_beneficiary(venal_office_structural_base, regional_nobility).
narrative_ontology:constraint_victim(venal_office_structural_base, crown_fiscal_authority).
narrative_ontology:constraint_victim(venal_office_structural_base, constitutional_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CROWN FISCAL AUTHORITY (SNARE) — Trapped by the remonstrance mechanism which functions as a veto on necessary taxation and military expenditure. The Crown cannot exit or override the system without destroying the magistracy's legitimacy. Maximum extraction directed upward; no alternatives for revenue collection exist. The constitutional structure itself becomes an impediment to state capacity.
constraint_indexing:constraint_classification(venal_office_structural_base, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PARLEMENTARY MAGISTRATES (ROPE) — Coordinate the realm's constitutional order through remonstrance. Genuinely solve a coordination problem: they articulate the realm's unwritten constitutional norms in the absence of any written constitution. Their net benefit is substantial — they extract fiscal privilege while framing it as constitutional necessity. Arbitrage because they can shift allegiance or negotiate individually with the Crown when pressure rises.
constraint_indexing:constraint_classification(venal_office_structural_base, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROVINCIAL NOBILITY (TANGLED ROPE) — Benefit from remonstrance as a check on Crown fiscal extraction (coordination function) but also use it to block reforms that would eliminate their tax exemptions (extraction function). Constrained by their dependence on Crown favor and military appointment. The constraint has both genuine coordinating and extractive functions for this agent.
constraint_indexing:constraint_classification(venal_office_structural_base, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM MOVEMENTS (SCAFFOLD) — See the remonstrance right as a temporary obstacle to rational fiscal administration. Organized reformers (Turgot, Necker, the physiocrats) view the system as fixable through sunset mechanisms: abolish venal office, establish written constitutional rules, centralize taxation. They see an exit path and a timeline. Theater ratio remains high because these reformers must maintain institutional legitimacy even while dismantling it.
constraint_indexing:constraint_classification(venal_office_structural_base, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: VENAL OFFICE SYSTEM (PITON) — The sale of judgeships and administrative posts was originally a revenue mechanism and a genuine administrative delegation. By the 18th century it has atrophied into performative legitimacy: judges hold office by birth/purchase, not competence; the system extracts revenue but provides degraded governance. Theater ratio is high (68%) because maintaining the fiction of meritocratic magistracy requires constant institutional performance despite the actual mechanism being hereditary privilege.
constraint_indexing:constraint_classification(venal_office_structural_base, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — Views the remonstrance right as an irreducible structural feature of medieval constitutionalism: the need to harmonize dispersed power centers without written constitutional text. From this view the constraint is unchangeable — any attempt to eliminate remonstrance would destroy the very constitutional order it protects. However, false summit detection will reveal that identifiable beneficiaries (parlementary magistrates, regional nobility) benefit from maintaining this 'natural law,' suggesting it is not immutable but contingently sustained.
constraint_indexing:constraint_classification(venal_office_structural_base, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(venal_office_structural_base_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(venal_office_structural_base, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(venal_office_structural_base, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(venal_office_structural_base, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(venal_office_structural_base, TR),
    TR >= 0.70.

:- end_tests(venal_office_structural_base_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.58 (endpoint): Moderate-high. The magistrates extract substantial fiscal privilege and social status through office purchase and remonstrance obstruction. However, the extraction is not total (does not reach snare threshold of 0.66+) because genuine coordination functions persist — remonstrance does articulate constitutional norms even if those norms protect fiscal interests. The measurement trajectory (0.35→0.48→0.58) shows extraction increasing over time as the hereditary basis of office weakens the coordination justification and increases the pure privilege component. Suppression at 0.65: Moderate-high. The Crown must suppress alternative administrative systems and suppress reformist challenge to venal office. Magistrates must suppress competing interpretations of constitutional authority. Theater ratio at 0.68: High. By the late period, the institutional practice is substantially performative — judges and magistrates maintain the fiction of meritocratic constitutional authority while succession is hereditary. The registration and remonstrance ceremonies perform constitutional legitimacy rather than exercise substantive constitutional judgment. The trajectory from 0.52→0.68 documents this degradation: the institution begins with some authentic coordination function (magistrates genuinely debate constitutional principles) and ends with primarily theatrical maintenance of institutional authority.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence from identical structural facts. The magistrates see coordination (Rope) — they are genuinely solving the problem of constitutional authority in an unwritten constitutional system. The Crown sees pure extraction (Snare) — they are blocked from necessary fiscal action by minority veto. Reform movements see a solvable temporary obstacle (Scaffold) — venal office can be abolished and replaced with written constitutional rules. The venal office institution sees its own degradation (Piton) — originally a delegation mechanism, now hereditary privilege maintained through performative ritual. The provincial nobility sees mixed benefits and costs (Tangled Rope) — coordination that protects them from Crown, extraction that harms their economic interests. The analytical observer risks seeing an immutable natural law (Mountain) — constitutional authority in unwritten systems requires dispersed checking mechanisms. The perspectival gap reveals that 'constitutional necessity' is the language magistrates use to describe their structural interest; they have successfully framed privilege as principle.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown's perspective derives d ≈ 0.95 (full victim): the Crown is trapped by the remonstrance mechanism, cannot exit, and bears the cost of blocked taxation and diminished state capacity. This produces f(d) ≈ 1.42, amplifying effective extractiveness. The magistrates' perspective derives d ≈ 0.15 (net beneficiary): they extract fiscal and status benefits, have arbitrage options (can negotiate individually or shift allegiance), and control the legitimacy narrative. This produces f(d) ≈ -0.01, suppressing or inverting effective extraction from their perspective — they experience the system as coordination enabling their legitimate constitutional role. The moderate (provincial nobility) perspective derives d ≈ 0.50 (symmetric): they benefit from remonstrance as Crown check but lose tax exemptions when reform succeeds. The organized (reform movement) perspective derives d ≈ 0.55: they are not trapped but face organized resistance and must maintain legitimacy while dismantling institutions. The analytical perspective derives d ≈ 0.72: observers see the full structure but risk naturalizing it as immutable. No overrides are needed — the derivation chain produces accurate directionality from beneficiary/victim + exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through perspectival identification of the false summit. The mountain classification (analytical observer) naturalizes what is actually a contestable institutional arrangement. False summit detection fires on two criteria: (1) beneficiaries are explicitly declared (parlementary_magistrates, regional_nobility), and (2) the mountain classification is challenged by lower-power perspectives (Crown's snare, reform movements' scaffold). The engine identifies that 'constitutional necessity' is a legitimacy claim advanced by beneficiaries, not a discovered natural law. The mandatrophy is resolved by documenting how the same structural facts produce rope for beneficiaries, snare for victims, scaffold for organized reformers, piton for the degraded institution, and tangled_rope for moderates, while only the analytical observer risks the false summit. The real classification is Tangled Rope (claimed_type correctly reflects this): genuine coordination of constitutional norms exists alongside extraction of fiscal privilege, and both components are required to understand the system. The mountain reading is available but is revealed as naturalization rather than natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_kernel_identity,
    'Is the remonstrance right a fundamental constitutional principle or a minority obstruction protecting fiscal privilege?',
    'Analysis of remonstrance content: do magistrates genuinely defend constitutional principles or primarily block revenue measures? Historical frequency: what proportion of remonstrances defend general principles vs specific fiscal interests?',
    'If constitutional principle: the constraint is rope (coordination). If fiscal obstruction: the constraint is snare (extraction). The answer determines whether the Crown''s perspective (snare) or magistrates'' perspective (rope) accurately describes the mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_kernel_identity, conceptual, 'Whether remonstrance right constitutes fundamental constitutional check or fiscal privilege protection').

omega_variable(
    venal_office_functional_decay,
    'At what point does the hereditary transmission of venal office shift from administrative delegation to pure extraction with no coordination function?',
    'Comparative analysis of judicial competence before/after hereditary transmission. Examination of whether office-holders perform administrative functions or purely extract revenue. Measurement of case quality, decision consistency, and citizen access over the constraint interval.',
    'If decay is early (pre-1700): system is piton throughout interval, theater ratio should be higher. If decay is late (post-1750): system transitions from tangled_rope to piton within the measurement interval, allowing detection of the theatrical shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(venal_office_functional_decay, empirical, 'Timing of venal office transition from functional delegation to pure revenue extraction').

omega_variable(
    crown_revenue_alternative_feasibility,
    'Did feasible administrative alternatives to venal office revenue exist that the Crown failed to pursue, or was the system locked-in by institutional path dependence?',
    'Comparative analysis: contemporary French proposals (Turgot, Necker) for direct taxation mechanisms. Analysis of neighboring states'' fiscal systems (England''s parliamentary taxation, Habsburg administrative centralization). Measurement of path-dependency: how many revenue mechanisms were foreclosed by the venal office structure?',
    'If alternatives were available: Crown''s snare classification is justified (trapped by institutional choice, not natural limit). If locked-in: classification shifts toward mountain or rope (structural inevitability). Directionality of extraction shifts based on whether Crown was agent or victim of the path dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_revenue_alternative_feasibility, empirical, 'Existence and feasibility of alternative revenue mechanisms in Ancien Régime').

omega_variable(
    false_summit_natural_law_risk,
    'Does the analytical observer''s mountain classification naturalize a historically contingent institutional arrangement?',
    'Comparison across constitutional traditions: do unwritten constitutions without remonstrance mechanisms exist or emerge? Analysis of whether the remonstrance right is unique to France or generalizable. Examination of post-revolutionary French constitutional development: can rational written constitutions replace the unwritten order without remonstrance?',
    'If naturalizing: classification is false summit; the ''immutable constitutional principle'' is actually sustained by beneficiary interests. Engine''s FSM signature will detect and flag. If genuinely irreducible: mountain classification is justified and no override occurs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Whether mountain classification represents genuine natural law or false summit naturalizing institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(venal_office_structural_base, 1710, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(venal_tr_t0, venal_office_structural_base, theater_ratio, 0, 0.52).
narrative_ontology:measurement(venal_tr_t25, venal_office_structural_base, theater_ratio, 25, 0.62).
narrative_ontology:measurement(venal_tr_t50, venal_office_structural_base, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(venal_be_t0, venal_office_structural_base, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(venal_be_t25, venal_office_structural_base, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(venal_be_t50, venal_office_structural_base, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(venal_su_t0, venal_office_structural_base, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(venal_su_t25, venal_office_structural_base, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(venal_su_t50, venal_office_structural_base, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(venal_office_structural_base, enforcement_mechanism).
narrative_ontology:affects_constraint(venal_office_structural_base, crown_fiscal_authority_ancien_regime).
narrative_ontology:affects_constraint(venal_office_structural_base, enlightenment_constitutional_rationalization).

% DUAL FORMULATION NOTE:
% The venal office system decomposes into three related constraints with different ε values: (1) venal_office_structural_base (ε=0.58) focuses on the remonstrance right and magistrate authority; (2) crown_fiscal_authority_ancien_regime (ε=0.72) focuses on the Crown's trapped capacity for taxation and military expenditure; (3) enlightenment_constitutional_rationalization (ε=0.42) focuses on reform movements' proposal to replace venal office with written constitutions. All three share the same institutional context but different structural positions produce different extractiveness values and different beneficiary/victim sets. This is constraint family decomposition per ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
