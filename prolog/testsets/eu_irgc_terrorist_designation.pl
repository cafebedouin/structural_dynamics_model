% ============================================================================
% CONSTRAINT STORY: eu_irgc_terrorist_designation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_irgc_terrorist_designation, []).

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
 *   constraint_id: eu_irgc_terrorist_designation
 *   human_readable: EU Terrorist Designation of Iran's IRGC
 *   domain: geopolitical/sanctions_regime
 *
 * SUMMARY:
 *   The European Union's potential designation of Iran's Islamic
 *   Revolutionary Guard Corps as a terrorist organization creates a complex
 *   geopolitical constraint that combines genuine coordination needs (unified
 *   sanctions against a revisionist regional power) with significant
 *   extraction mechanisms (lost economic opportunities, diplomatic leverage,
 *   and leverage over Iran's behavior). The constraint operates at the
 *   intersection of international security law, coalition politics, and
 *   strategic autonomy. The EU faces pressure from the US maximum-pressure
 *   campaign, concerns from Israel over regional security, and internal
 *   debate over whether counterterrorism designation is an appropriate tool
 *   for addressing the IRGC's military and strategic activities. The
 *   designation's theater ratio (0.64) reflects that the constraint functions
 *   partly as genuine security enforcement and partly as political signaling
 *   — the legal framework for terrorist designation carries elaborate
 *   procedural requirements that perform robustness without reliably
 *   gatekeeping on substance. The extractiveness has increased over the
 *   interval (0.32 to 0.58) as secondary sanctions have tightened and the
 *   IRGC's role in Iran's economy has become more difficult to cleanly
 *   separate from legitimate state functions.
 *
 * KEY AGENTS:
 *   - United States Government: Primary coalition beneficiary (institutional/arbitrage) — benefits from EU designation amplifying maximum-pressure strategy; can modulate pressure independently
 *   - Iran's Strategic Leadership (IRGC): Primary victim (powerful/trapped) — loses access to international financing and trade routes; faces resource constraints on regional activities; no unilateral exit mechanism
 *   - EU Member States (aggregate): Organizational coordinator (institutional/constrained) — benefits from unified sanctions coordination; bears costs of lost diplomatic channels and business opportunities; constrained exit through need to maintain coalition unity
 *   - Iran's Economic Actors (SMEs, importers, financial firms): Secondary victim (moderate/trapped) — face financing restrictions and trade route closures; cannot easily distinguish legitimate from IRGC-affiliated transactions; limited exit options within Iran's controlled economy
 *   - EU Diplomatic Actors: Potential negotiator (organized/mobile) — recognize implicit sunset through negotiation pathways; constrained by need to maintain US coalition alignment
 *   - Israel: Tertiary beneficiary (powerful/arbitrage) — benefits from constraint on IRGC funding for regional proxy activities; maintains independent exit options through bilateral US coordination
 *   - EU Legal Institutions: Custodian of institutional theater (institutional/arbitrage) — maintain designation procedure with elaborate due process; experience own process as degraded (court challenges overturn designations inconsistently)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_irgc_terrorist_designation, 0.58).
domain_priors:suppression_score(eu_irgc_terrorist_designation, 0.68).
domain_priors:theater_ratio(eu_irgc_terrorist_designation, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_irgc_terrorist_designation, extractiveness, 0.58).
narrative_ontology:constraint_metric(eu_irgc_terrorist_designation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(eu_irgc_terrorist_designation, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_irgc_terrorist_designation, tangled_rope).
narrative_ontology:human_readable(eu_irgc_terrorist_designation, "EU Terrorist Designation of Iran's IRGC").
narrative_ontology:topic_domain(eu_irgc_terrorist_designation, "geopolitical/sanctions_regime").

domain_priors:requires_active_enforcement(eu_irgc_terrorist_designation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_irgc_terrorist_designation, eu_political_credibility).
narrative_ontology:constraint_beneficiary(eu_irgc_terrorist_designation, us_aligned_coalition).
narrative_ontology:constraint_beneficiary(eu_irgc_terrorist_designation, israel_security_interests).
narrative_ontology:constraint_victim(eu_irgc_terrorist_designation, iran_economic_capacity).
narrative_ontology:constraint_victim(eu_irgc_terrorist_designation, eu_iran_diplomatic_channels).
narrative_ontology:constraint_victim(eu_irgc_terrorist_designation, eu_business_interests_in_iran).
narrative_ontology:constraint_victim(eu_irgc_terrorist_designation, irgc_affiliated_entities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRANIAN ECONOMIC AGENTS (SNARE) — Trapped within a designation that forecloses legitimate trade routes, financing options, and international commerce. No exit mechanism: designation applies to all agents conducting transactions touching IRGC-affiliated entities. Suppression is nearly total — alternative pathways are systematically blocked by secondary sanctions and banking restrictions.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EU MEMBER STATES (TANGLED ROPE) — Face a genuine coordination problem: fragmenting designations across member states create arbitrage opportunities for Iran to route capital through less-aligned jurisdictions. Designation provides coordination benefit (unified sanctions front). Simultaneously, designation extracts from EU interests through lost diplomatic channels, reduced leverage for negotiations, and economic costs to EU businesses operating in Iran. Constrained exit: cannot unilaterally abandon coordination without fragmenting EU sanctions authority.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: UNITED STATES STRATEGIC INTEREST (ROPE) — Benefits from EU designation as a coordination mechanism: amplifies US maximum-pressure campaign against Iran, reduces capital flows available for IRGC-funded regional activities, and strengthens the coalition of aligned states. Arbitrage exit: US can modulate pressure independently, using designation as leverage in any future negotiations. Net beneficiary — experiences the designation as a pure coordination gain.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EU DIPLOMATIC CAPACITY (SCAFFOLD) — Organized actors within EU institutions (diplomatic corps, EEAS) recognize the designation as a temporary enforcement mechanism with an implied sunset. The constraint has a coordination function (aligned sanctions) and a built-in exit mechanism: delisting becomes available if Iran negotiates compliance on regional activities or weapons programs. Theater moderately high because much of the designation functions as political signaling to domestic constituencies and US partners rather than direct enforcement. Sunset implicit: successful JCPOA-style negotiations would enable delisting without political cost — the designation is not permanent.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: EU LEGAL-INSTITUTIONAL FRAMEWORK (PITON) — The formal designation procedure (evidence gathering, due process, court challenges, listing review cycles) persists as institutional ritual even as its functional verification capacity has degraded. Courts regularly overturn or qualify designations on evidentiary grounds, yet the designation process continues unchanged. Theater is high because the legal apparatus performs robustness while actual enforcement depends on political will and US pressure. Theater ratio 0.64 reflects: procedure is elaborate, but substantive evidentiary gates are weak (designations withstand court review inconsistently); delisting is theoretically possible but politically expensive. The constraint persists through institutional inertia — the listing mechanism is maintained because no alternative enforcement architecture exists, not because the legal procedure is functionally sound.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: IRAN'S DIPLOMATIC ACTORS (TANGLED ROPE) — Experience the designation as both extraction and (potential) coordination mechanism. The constraint extracts through lost trade opportunities and economic capacity for regional activities. Simultaneously, designation creates a concrete negotiation objective: delisting becomes a coordination tool if Iran and EU agree on behavioral changes (regional proxy activities, weapons programs). Constrained exit: Iran cannot unilaterally escape the designation (no exit option available to Iran independently), but negotiation pathways exist through multilateral processes. The constraint is asymmetric but reversible — the coordination function (negotiated behavioral change in exchange for delisting) is latent and could activate.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INTERNATIONAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the designation reflects an immutable feature of international relations: powerful states use security classification to constrain rival state actors when negotiated agreements break down. This perspective sees the designation as following inevitably from the structural position of the EU within a US-led alliance and Iran's status as a revisionist power in the Middle East. However, the structural data (presence of beneficiaries, victims, reversibility through negotiation, institutional theater) contradicts the mountain classification. The engine should flag this as a false summit: naturalization of a contingent political choice as an immutable international law.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_irgc_terrorist_designation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_irgc_terrorist_designation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_irgc_terrorist_designation, TR),
    TR >= 0.70.

:- end_tests(eu_irgc_terrorist_designation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significantly from Iran through denied access to international capital markets, trade routes, and financial services. The extraction flows toward US security interests and EU coalition coordination benefits. However, extractiveness is not maximal (not 0.70+) because: (1) the constraint is theoretically reversible through negotiation, creating an implicit exit pathway; (2) Iran retains state capacity to route some economic activity through alternative jurisdictions and informal networks; (3) the designation targets a specific organizational entity rather than the full Iranian state. The increasing trajectory (0.32→0.58 over interval) reflects tightening secondary sanctions and growing difficulty in separating IRGC economic activities from legitimate state enterprises. Suppression (0.68): High. The constraint systematically forecloses Iran's exit options through financial sanctions, trade route restrictions, and secondary sanctions against third-party actors. Alternative pathways (sanctions circumvention) are technically possible but extremely costly and increasingly criminalized. Iran has limited options for negotiating partial relief without major behavioral concessions. Theater ratio (0.64): Moderate-high. The EU's legal designation procedure is elaborate (evidentiary review, due process, court access) but substantively weak (courts overturn or narrow designations irregularly; evidence standards are sometimes inconsistent). Much of the procedure functions as political theater demonstrating robustness without reliably gatekeeping. The increasing trajectory (0.42→0.64) reflects that as the primary enforcement mechanism shifts from legal designation to financial secondary sanctions, the legal framework's role becomes increasingly performative.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a sharp perspectival gap between the US/coalition beneficiary view (Rope — pure coordination benefit) and the Iranian victim view (Snare — pure extraction with no exit). The EU occupies an intermediate position (Tangled Rope — genuine coordination function alongside significant extraction). The gap reflects different structural positions: the US/Israel choose to enforce the designation and can modulate pressure; the EU must balance coalition coordination against lost diplomatic leverage; Iran experiences the constraint as irreversible and total. A secondary perspectival gap exists between the EU institutional actor (Tangled Rope with constrained exit) and the EU diplomatic corps (Scaffold with implicit negotiation sunset). This gap reveals internal EU heterogeneity: formal EU institutions experience the constraint as enforcement with no reversibility pathway, while diplomatic actors recognize that sufficient Iranian behavioral change (JCPOA-style compliance) would enable delisting. The analytical observer's mountain perspective (immutable feature of great power competition) misses the contingent political choices and reversibility pathways that structure the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values encode each actor's structural relationship to the extraction flow. Iran's economic actors (victims with trapped exit) derive high d → high f(d), experiencing maximum effective extraction. The US (beneficiary with arbitrage exit) derives low d → negative f(d), experiencing the constraint as a free coordination gain. The EU (mixed beneficiary-victim with constrained exit) derives moderate d → moderate f(d), experiencing significant extraction alongside coordination benefits. The directional asymmetry is fundamental: those who chose the designation (US, EU coalition members) retain exit options or offset benefits; those against whom it is enforced (Iran) have no unilateral exit. The engine derives d from these structural positions and the chi formula χ = ε × f(d) × σ(S) scales extractiveness by the agent's power and scope modifier. Continental scope (σ=0.9) slightly dampens χ compared to global scope, reflecting that the constraint's enforcement is strongest within EU/US jurisdictions and weaker in non-aligned regions.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by showing that the beneficiary and victim perspectives cannot both be correct — they cannot both be reading the same constraint from different angles. The US sees Rope (pure coordination benefit); Iran sees Snare (pure extraction). These are not perspectival readings of the same type — they reflect genuinely incompatible structural experiences. The mandatrophy resolves when we recognize that the constraint is indeed a Tangled Rope at the system level (combining genuine coordination benefits for the coalition with genuine extraction from Iran) but reads as Snare from Iran's trapped position and Rope from the US beneficiary position. The EU's intermediate Tangled Rope classification is the constraint's true type. The resolution confirms that mandatrophy detection is functioning: when a constraint appears simultaneously as both pure extraction (Snare) and pure coordination (Rope) from different perspectives, the constraint is genuinely Tangled Rope — it combines both functions asymmetrically. The theater ratio (0.64) and extractiveness (0.58) confirm this hybrid classification: extractiveness is high but not maximal; theater is moderate-high but not dominant (not Piton). The constraint is a real Tangled Rope with a reversible sunset (the diplomatic pathway), not a permanent Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irgc_organizational_boundaries,
    'How clearly can a terrorist designation target the IRGC as a distinct organizational entity without capturing civilian Iranian government functions, economic enterprises, and humanitarian activities?',
    'Legal analysis of previous EU designations of military/paramilitary organizations; examination of secondary effects on unrelated Iranian economic actors; court challenges to scope of designation',
    'If boundaries unclear: designation becomes functional tool for economic containment rather than counterterrorism, reclassifying from Tangled Rope (limited justification) to Snare (unjustifiable extraction). If boundaries clear: designation remains narrowly targeted, supporting Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(irgc_organizational_boundaries, empirical, 'Organizational boundaries and scope of IRGC designation').

omega_variable(
    negotiation_reversibility,
    'Is the designation genuinely reversible through negotiated Iranian behavioral change, or does it function as a permanent punishment mechanism?',
    'Historical analysis of EU delisting patterns; examination of stated conditions for potential delisting; tracking whether diplomatic engagement attempts to establish reversal pathways',
    'If reversible: Scaffold perspective is structural — sunset is real and embedded in negotiation logic. If irreversible: Scaffold perspective is aspirational, and the constraint functions as pure Snare or Tangled Rope extraction without exit mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiation_reversibility, empirical, 'Whether designation is reversible through negotiation').

omega_variable(
    eu_strategic_autonomy_vs_us_alignment,
    'To what degree does the EU designation reflect independent EU security assessment versus structural pressure to align with US maximum-pressure strategy?',
    'Comparative analysis of EU designation timing and language relative to US designations; examination of internal EU decision-making documents; analysis of cases where EU refused US pressure for designation or delisting',
    'If EU autonomous: Tangled Rope classification is structural — the EU faces genuine coordination-extraction tradeoff between sanctions unity and diplomatic leverage. If EU subordinate to US: EU becomes secondary beneficiary rather than primary actor, changing perspective tuples and potentially shifting toward Snare classification from EU institutional view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_strategic_autonomy_vs_us_alignment, conceptual, 'EU strategic autonomy relative to US in designation decisions').

omega_variable(
    terrorist_vs_military_distinction,
    'On what grounds can the designation distinguish IRGC terrorist activities from conventional military operations that international law permits for state actors?',
    'Analysis of IRGC activities designated as terrorist vs activities that would be lawful if attributed to equivalent US or Israeli military actors; examination of designation criteria and their consistency',
    'If distinction defensible: designation maintains legitimacy and Tangled Rope classification holds. If distinction indefensible: designation becomes a political tool disguised as counterterrorism, reclassifying to Snare from Iran''s perspective and potentially from international law view.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(terrorist_vs_military_distinction, conceptual, 'Terrorist vs military distinction in IRGC designation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_irgc_terrorist_designation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_irgc_tr_t0, eu_irgc_terrorist_designation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(eu_irgc_tr_t5, eu_irgc_terrorist_designation, theater_ratio, 5, 0.53).
narrative_ontology:measurement(eu_irgc_tr_t10, eu_irgc_terrorist_designation, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(eu_irgc_be_t0, eu_irgc_terrorist_designation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(eu_irgc_be_t5, eu_irgc_terrorist_designation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(eu_irgc_be_t10, eu_irgc_terrorist_designation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_irgc_terrorist_designation, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_irgc_terrorist_designation, us_iran_maximum_pressure).
narrative_ontology:affects_constraint(eu_irgc_terrorist_designation, eu_strategic_autonomy_vs_us_alignment).
narrative_ontology:affects_constraint(eu_irgc_terrorist_designation, jcpoa_collapse_and_renewal).
narrative_ontology:affects_constraint(eu_irgc_terrorist_designation, irgc_proxy_funding_constraints).

% DUAL FORMULATION NOTE:
% The IRGC designation is downstream of the US maximum-pressure strategy but represents a distinct constraint with its own extractiveness (EU contribution to coordination) and reversibility (negotiation pathway). The upstream constraint (US maximum pressure) has higher extractiveness and lower reversibility; this constraint has moderate extractiveness with explicit diplomatic exit mechanism. Related constraint: 'eu_strategic_autonomy_vs_us_alignment' models whether EU independently assesses IRGC threat or subordinates to US pressure — that constraint affects the interpretation of whether EU benefits from coordination or extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_irgc_terrorist_designation, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
