% ============================================================================
% CONSTRAINT STORY: sotu_1987_reagan_central_america_regional_containment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1987_reagan_central_america_regional_containment, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1987_reagan_central_america_regional_containment
 *   human_readable: U.S. Cold War Regional Containment in Central America (1987)
 *   domain: foreign_policy/geopolitical_containment
 *
 * SUMMARY:
 *   The 1987 State of the Union address articulated the Reagan
 *   administration's Central American containment policy as a necessity for
 *   regional stability and prevention of communist regional hegemony. The
 *   constraint operates as a dual-mechanism system: (1) direct military and
 *   economic aid to recipient Central American governments, creating a
 *   coordination function around shared anti-Sandinista interest; (2) covert
 *   and overt support for Contra forces aligned against the Sandinista
 *   regime, functioning as an extraction mechanism targeting Nicaragua and
 *   civilian populations caught in proxy warfare. The constraint benefits
 *   U.S. strategic interests (perceived prevention of Soviet-aligned
 *   expansion in the hemisphere), recipient government cohesion (security
 *   guarantees, military capacity), and anti-Sandinista forces. It extracts
 *   costs from Sandinista Nicaragua (economic blockade, military pressure,
 *   diplomatic isolation), civilian populations in conflict zones (violence,
 *   displacement, resource scarcity), and the principle of regional
 *   sovereignty (U.S. determines acceptable political outcomes). The
 *   mechanism depends on Congressional authorization (bipartisan consensus on
 *   Cold War containment doctrine) and on framing the constraint as defensive
 *   (prevention of hostile regional consolidation) rather than as coercive
 *   (imposition of U.S.-preferred political outcomes). Theater ratio measures
 *   the increasing gap between doctrinal justification (Monroe Doctrine,
 *   Truman Doctrine, hemispheric stability) and actual mechanism (proxy
 *   warfare, human rights abuses, authoritarian support). The extractiveness
 *   curve shows rising extraction over the 1980s as costs accumulated (Contra
 *   atrocities, civilian casualties, economic damage to Nicaragua), while the
 *   theater ratio rose as doctrinal justifications intensified.
 *
 * KEY AGENTS:
 *   - U.S. Executive Branch / Reagan Administration (institutional/arbitrage): Primary architect and beneficiary of the constraint; experiences it as pure coordination mechanism for hemisphere security
 *   - U.S. Congress / Bipartisan Foreign Policy Consensus (organized/constrained): Authorized the mechanism; experienced institutional lock-in around Cold War consensus despite growing evidence of Contra atrocities
 *   - Recipient Central American Governments (institutional/constrained): Honduras, El Salvador, Guatemala as security assistance recipients; benefited from U.S. support but experienced reduced sovereignty and conditional aid requirements
 *   - Anti-Sandinista Forces / Contras (organized/mobile): Armed opposition groups receiving U.S. support; had exit options (negotiations, dissolution, repatriation) but incentivized to continue by U.S. funding
 *   - Sandinista Nicaragua (institutional/trapped): Primary target; experienced maximum suppression and extraction with no effective exit options other than regime collapse
 *   - Civilian Populations in Conflict Zones (powerless/trapped): Populations in Nicaragua, Honduras, El Salvador caught in proxy warfare; bore extraction costs through violence and displacement
 *   - Regional Sovereignty Principle (abstract/trapped): Governance principle subordinated to U.S. containment doctrine; no agent available to advocate for it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1987_reagan_central_america_regional_containment, 0.58).
domain_priors:suppression_score(sotu_1987_reagan_central_america_regional_containment, 0.72).
domain_priors:theater_ratio(sotu_1987_reagan_central_america_regional_containment, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1987_reagan_central_america_regional_containment, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1987_reagan_central_america_regional_containment, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sotu_1987_reagan_central_america_regional_containment, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1987_reagan_central_america_regional_containment, tangled_rope).
narrative_ontology:human_readable(sotu_1987_reagan_central_america_regional_containment, "U.S. Cold War Regional Containment in Central America (1987)").
narrative_ontology:topic_domain(sotu_1987_reagan_central_america_regional_containment, "foreign_policy/geopolitical_containment").

domain_priors:requires_active_enforcement(sotu_1987_reagan_central_america_regional_containment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1987_reagan_central_america_regional_containment, us_strategic_interest).
narrative_ontology:constraint_beneficiary(sotu_1987_reagan_central_america_regional_containment, recipient_central_american_governments).
narrative_ontology:constraint_beneficiary(sotu_1987_reagan_central_america_regional_containment, anti_sandinista_aligned_forces).
narrative_ontology:constraint_victim(sotu_1987_reagan_central_america_regional_containment, sandinista_nicaragua).
narrative_ontology:constraint_victim(sotu_1987_reagan_central_america_regional_containment, civilian_populations_conflict_zones).
narrative_ontology:constraint_victim(sotu_1987_reagan_central_america_regional_containment, regional_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SANDINISTA NICARAGUA (SNARE) — Trapped by overwhelming military and economic pressure from the superpower. No exit options: cannot abandon the revolution without regime collapse; cannot escalate without inviting direct U.S. invasion; cannot negotiate from genuine position of strength. Experiences maximum extraction and suppression. The constraint forces a choice between revolutionary consolidation and national survival — both paths lead to further extraction.
constraint_indexing:constraint_classification(sotu_1987_reagan_central_america_regional_containment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CIVILIAN POPULATIONS IN CONFLICT ZONES (SNARE) — Trapped by proxy warfare. No exit option: caught between U.S.-backed forces and Sandinista consolidation efforts. Bear the extraction cost through displacement, recruitment pressure, violence, and resource scarcity. Classification identical to Nicaragua's perspective because the suppression mechanism is the same — structural violence with no civilian exit.
constraint_indexing:constraint_classification(sotu_1987_reagan_central_america_regional_containment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: RECIPIENT CENTRAL AMERICAN GOVERNMENTS (TANGLED ROPE) — Constrained by dependence on U.S. security assistance but benefit from it. Face genuine coordination problem: containing Sandinista influence protects regional stability and these governments' tenure. But also experience asymmetric extraction: U.S. conditions on aid, bases, intelligence sharing, and alignment reduce sovereignty. Moderate experienced extractiveness because they have partial agency — can negotiate aid levels, can condition cooperation — but cannot exit the constraint without losing security guarantees.
constraint_indexing:constraint_classification(sotu_1987_reagan_central_america_regional_containment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: U.S. STRATEGIC INTEREST / EXECUTIVE-DEFENSE ESTABLISHMENT (ROPE) — Experiences the constraint as a pure coordination mechanism. The goal is hemisphere stability and prevention of communist consolidation in the region. The mechanism (military aid, diplomatic pressure, Contra support) directly serves this goal. Minimal experienced extraction because the beneficiary is the agent with maximum power and maximum exit options (can change containment doctrine, can negotiate with Nicaragua, can redirect resources). This is the perspective that authored the constraint.
constraint_indexing:constraint_classification(sotu_1987_reagan_central_america_regional_containment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: U.S. CONGRESS / BIPARTISAN CONSENSUS (TANGLED ROPE) — Organized agents with genuine but constrained choice. Congress funds the mechanism and thus benefits from the coordination function (regional stability narrative, anti-communist doctrine preservation). But also experiences extraction through institutional lock-in: the consensus is self-reinforcing. Once the Cold War frame is accepted, alternatives (negotiation with Nicaragua, non-aligned Central America) become politically unthinkable. Constrained exit: a congressperson who breaks the consensus faces party pressure, committee assignments, primary challenges. Effective extraction moderates to tangled_rope level because Congress retains formal power but experiences real constraints on exercising it.
constraint_indexing:constraint_classification(sotu_1987_reagan_central_america_regional_containment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MONROE DOCTRINE / REGIONAL HEGEMONY DOCTRINE (PITON) — Analyzed at the civilizational scale as a statement of doctrine rather than current mechanism, this perspective observes that the 1987 containment strategy is substantially theater: it invokes a 164-year-old doctrine (Monroe Doctrine, 1823) to justify current geopolitical maneuvering, and it invokes the Truman Doctrine precedent (1947) to frame Central America as strategically essential. The doctrinal performance (speeches about hemispheric stability, anti-communist unity) has risen as the actual coordination problem it solved (preventing Soviet-aligned expansion in the Western Hemisphere during Cold War) has matured into institutional routine. Theater ratio reflects this: the doctrinal justification is now doing more work than the mechanism itself. Piton classification reflects that the constraint persists through institutional inertia and doctrinal theater rather than through dynamic functional necessity.
constraint_indexing:constraint_classification(sotu_1987_reagan_central_america_regional_containment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOPOLITICAL NECESSITY (MOUNTAIN) — From a civilizational, universal perspective, one interpretation reads the constraint as an immutable feature of interstate competition: great powers have always sought to prevent hostile rivals from consolidating regional control; the U.S. strategy in Central America is merely the application of this timeless principle in a specific context. The constraint appears as a mountain—unchangeable, natural, inherent to how international systems work. However, this perspective risks false summit status: the structural data shows identifiable beneficiaries and victims, making it contestable whether this is a natural law of geopolitics or a contingent institutional arrangement that serves U.S. interests.
constraint_indexing:constraint_classification(sotu_1987_reagan_central_america_regional_containment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1987_reagan_central_america_regional_containment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1987_reagan_central_america_regional_containment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1987_reagan_central_america_regional_containment, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1987_reagan_central_america_regional_containment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1987_reagan_central_america_regional_containment, TR),
    TR >= 0.70.

:- end_tests(sotu_1987_reagan_central_america_regional_containment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting the genuine coordination problem (preventing hostile regional consolidation) alongside asymmetric extraction (Sandinista subordination, civilian casualties, sovereignty constraint). The value increased from 0.42 to 0.58 over the interval as costs accumulated and the mechanism shifted from coordination-heavy to extraction-heavy. Suppression (0.72): High. The mechanism depends on structured violence (proxy warfare, blockade), elimination of Nicaragua's exit options (military pressure, diplomatic isolation, economic coercion), and suppression of alternative frames (denial of Sandinista legitimacy, marginalization of negotiations). Theater ratio (0.64): Moderate-high, reflecting the increasing gap between doctrinal performance (hemispheric stability, democratic promotion, anti-communist solidarity) and mechanistic reality (proxy warfare enabling human rights abuses, support for authoritarian governments). Theater ratio increased over the interval as doctrinal justifications intensified while atrocities (Iran-Contra scandal, Contra massacres) became public. The theater serves a suppression function: doctrinal frame prevents questioning of the mechanism's legitimacy. Mandatrophy resolved through tangled_rope classification: the constraint genuinely coordinates against perceived threat while simultaneously extracting through asymmetric coercion. Both functions are structurally real, not contradictory. The classification prevents mischaracterizing the mechanism as either pure coordination (ignoring extraction) or pure extraction (ignoring genuine strategic interest).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. U.S. strategic actors (Executive, core Congress) experience it as rope—solving a genuine coordination problem (shared interest in preventing communist regional consolidation). Recipient governments experience tangled_rope—genuine security coordination mixed with sovereignty extraction. Contras experience rope or arbitrage-exit mobility (rewarded for coordination with U.S. goals). Sandinista Nicaragua experiences snare—no exit, maximum extraction, structural violence. Civilian populations experience identical snare—proxy warfare with no exit. Congress experiences tangled_rope at the institutional level but individual congresspersons experience identity_locked (career identity fused with Cold War consensus, exit means ideological self-abandonment). The piton perspective at civilizational time horizon observes that doctrinal justification has increasingly detached from functional necessity—the Monroe Doctrine (1823) and Truman Doctrine (1947) are being invoked with rising frequency as the mechanism matures, suggesting theater substitution for genuine function. The mountain perspective (geopolitical necessity of great-power containment) risks false-summit classification because the constraint has identifiable beneficiaries, is enforced through institutional mechanisms rather than natural law, and exhibits measurable extraction mechanisms that could be terminated by policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. Executive derives d ≈ 0.05 (beneficiary + arbitrage exit): can exit containment doctrine at will, can redirect resources, can negotiate with Nicaragua. Experiences negative or near-zero effective extraction because all benefits accrue to this agent. Recipient governments derive d ≈ 0.40 (beneficiary + constrained exit): depend on U.S. aid, cannot credibly exit alliance, but retain some negotiating power over aid levels and conditions. Experiences moderate extraction despite beneficiary status because of sovereignty constraints. Sandinista Nicaragua derives d ≈ 0.95 (victim + trapped exit): targeted for extraction, no exit options, cannot negotiate from strength. Experiences maximum extraction (χ approaches 1.4 via f(d)). Congress derives d ≈ 0.55 (beneficiary in symbolic sense + constrained exit): benefits from Cold War consensus frame, can authorize or cut funding, but experiences significant pressure to maintain consensus. Constrained exit (party pressure, ideological commitment) produces moderate-high d. Civilian populations derive d ≈ 0.92 (victim + trapped): caught in proxy warfare with minimal exit options. The directionality structure explains perspectival gaps: beneficiaries see rope (coordination), victims see snare (extraction), organized agents see tangled_rope (mixed), doctrinal frame sees mountain (natural law of geopolitics). The engine's false-summit detection should flag the mountain perspective as naturalizing a contingent institutional arrangement that benefits specific actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through tangled_rope classification. The constraint is NOT a pure coordination mechanism (rope) because it involves asymmetric extraction—Nicaragua and civilians bear concentrated costs while U.S. and recipient governments capture disproportionate benefits. The constraint is NOT pure extraction (snare) because it solves a genuine collective action problem for the beneficiary coalition (preventing hostile regional consolidation) and involves institutional coordination across multiple state actors with aligned interests. The tangled_rope classification captures both functions: (1) genuine coordination among beneficiaries around shared anti-Sandinista interest; (2) asymmetric extraction targeting the victim group (Nicaragua, civilians, regional sovereignty principle). The beneficiary group (U.S., recipient governments) perceives and designs the mechanism as coordination; the victim group perceives and experiences it as extraction. Both perceptions are structurally accurate—the constraint enables coordination for the beneficiary coalition while extracting from the victim coalition. The iron is that the mechanism REQUIRES the asymmetric extraction to function as coordination for the beneficiaries—removing the coercive capacity (military aid, Contra support, embargo) would eliminate the coordination function because the beneficiaries' problem is precisely how to prevent Nicaragua from consolidating power. The mechanism is thus inseparably both coordination and extraction; tangled_rope resolves this by accepting both as constitutive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    soviet_threat_magnitude,
    'How severe was the actual Soviet threat to U.S. hemisphere interests via Sandinista consolidation, versus perceived threat shaped by Cold War ideology?',
    'Declassified intelligence assessments; comparison of intelligence community projections in 1987 with actual Soviet capability and intent in Central America; post-Cold War historical analysis of Nicaragua''s genuine alignment options',
    'If threat was actual: containment mechanism is genuine coordination function (raises χ justification). If threat was largely perceived/ideological: mechanism is cover story for regional extraction (reveals false summit in mountain perspective; confirms snare for Nicaragua). This directly determines whether the constraint should classify as rope-family or snare/tangled_rope-family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soviet_threat_magnitude, empirical, 'Actual magnitude of Soviet threat versus ideological perception').

omega_variable(
    contra_atrocity_knowledge_and_intent,
    'To what extent did U.S. policymakers knowingly authorize or tolerate Contra human rights abuses, and how much was this structural to the containment mechanism versus incidental to it?',
    'Declassified documents on CIA knowledge of Contra atrocities; testimony from policymakers and intelligence officials; comparison of Contra support levels before and after atrocity documentation became public',
    'If abuses were incidental and policymakers worked to minimize them: mechanism retains coordination-heavy classification. If abuses were knowingly tolerated as cost of containment: suppression parameter should increase and mechanism shifts toward snare. If U.S. actively enabled abuses as coercive mechanism: confirms snare classification for civilian victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contra_atrocity_knowledge_and_intent, empirical, 'Degree of knowing authorization of Contra human rights abuses').

omega_variable(
    democratic_governance_requirement_sincerity,
    'How genuine was the stated requirement for ''democratic governance'' in recipient Central American governments? Were anti-democratic governments (El Salvador, Guatemala) supported equally and without condition, revealing the democracy requirement as theater?',
    'Comparative analysis of aid levels to democratic vs authoritarian recipient governments; documentary evidence on U.S. pressure for democratic reforms; post-Cold War declassified correspondence on democracy requirements',
    'If democracy requirement was enforced: the constraint has genuine coordination function (promoting regional democratic stability). If democracy requirement was theater (equal support for authoritarian governments): the constraint is primarily extraction mechanism disguised as democratic promotion (increases theater ratio and confirms snare/piton classification). Theater ratio depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_governance_requirement_sincerity, empirical, 'Sincerity of democratic governance requirements for aid recipients').

omega_variable(
    congressional_mandate_versus_executive_dominance,
    'Was Congress genuinely exercising independent oversight authority over containment policy, or did Congressional authorization function as post-hoc rubber-stamp for Executive decisions already made?',
    'Historical analysis of Congressional debate and amendment process; testimony on executive pressure tactics; comparison of authorized versus requested aid levels; timing of intelligence disclosures relative to Congressional authorization votes',
    'If Congress exercised genuine independent authority: tangled_rope classification for Congress is accurate, exit_options are ''constrained'' with real deliberative choice. If Congress was procedurally dominated by Executive: Congress''s classification should downgrade to ''trapped'' or ''identity_locked'', and the bipartisan consensus itself becomes a snare mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_mandate_versus_executive_dominance, empirical, 'Congressional independence in oversight versus Executive domination').

omega_variable(
    containment_doctrine_post_cold_war_persistence,
    'Did the containment doctrine''s institutional commitment survive the Cold War''s end, and if so, what structural mechanisms maintain it absent the original threat?',
    'Analysis of U.S. Central America policy continuity post-1989; study of institutional lock-in mechanisms in foreign policy bureaucracy; examination of how Cold War frames persist in post-Cold War policy justifications',
    'If doctrine persists: piton classification is confirmed—the constraint is maintained by institutional inertia and theater. If doctrine was genuinely abandoned and replaced: 1987 constraint was a true rope-family mechanism whose time had ended. This determines whether the constraint is fundamentally degraded (piton) or was always a coordination mechanism within a specific historical window.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(containment_doctrine_post_cold_war_persistence, empirical, 'Institutional persistence of containment doctrine post-Cold War').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1987_reagan_central_america_regional_containment, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1987_reagan_central_america_regional_containment, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sotu_tr_t3, sotu_1987_reagan_central_america_regional_containment, theater_ratio, 3, 0.56).
narrative_ontology:measurement(sotu_tr_t6, sotu_1987_reagan_central_america_regional_containment, theater_ratio, 6, 0.64).
narrative_ontology:measurement(sotu_tr_t9, sotu_1987_reagan_central_america_regional_containment, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1987_reagan_central_america_regional_containment, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sotu_be_t3, sotu_1987_reagan_central_america_regional_containment, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(sotu_be_t6, sotu_1987_reagan_central_america_regional_containment, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(sotu_be_t9, sotu_1987_reagan_central_america_regional_containment, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1987_reagan_central_america_regional_containment, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1987_reagan_central_america_regional_containment, iran_contra_operational_secrecy).
narrative_ontology:affects_constraint(sotu_1987_reagan_central_america_regional_containment, central_american_refugee_crisis).
narrative_ontology:affects_constraint(sotu_1987_reagan_central_america_regional_containment, nicaraguan_economic_collapse_sanctions).

% DUAL FORMULATION NOTE:
% The 1987 containment constraint is downstream of Cold War doctrine (affects how that doctrine is operationalized) and upstream of specific operational constraints (Iran-Contra secrecy mechanism, refugee flows, economic sanctions targeting). The extractiveness (0.58) is higher than pure Cold War doctrine (which would be lower ε, rope-family) because it captures the specific operationalized mechanism with identifiable human costs. Separate constraint stories for Iran-Contra (ε ≈ 0.72, snare—pure covert extraction) and refugee crisis (ε ≈ 0.65, tangled_rope—coordination of displacement alongside extraction of asylum resources) show how the containment doctrine fragments into structurally distinct constraints when operationalized.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1987_reagan_central_america_regional_containment, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
