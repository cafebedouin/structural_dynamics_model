% ============================================================================
% CONSTRAINT STORY: icc_prosecution_ukraine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_icc_prosecution_ukraine, []).

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
 *   constraint_id: icc_prosecution_ukraine
 *   human_readable: ICC Prosecution of Ukraine Conflict Actors
 *   domain: international_law/conflict_accountability
 *
 * SUMMARY:
 *   The International Criminal Court's prosecution of Ukrainian conflict
 *   actors creates a structural tension between the legitimate need for
 *   accountability mechanisms in major international crimes and the risk that
 *   external prosecutorial authority becomes an instrument of extraction from
 *   the targeted state. Ukraine cannot opt out of ICC jurisdiction
 *   post-ratification, faces reputational and political pressure if its
 *   officials are prosecuted, and has limited control over case selection and
 *   timing. Simultaneously, conflict victims gain access to an accountability
 *   forum that domestic institutions cannot provide. International legal
 *   institutions benefit from expanded mandate and resources. The constraint
 *   exhibits all six DR types from different perspectives, revealing how
 *   indexical position determines whether the same institutional mechanism
 *   appears as justice-enabling coordination, as a tool of geopolitical
 *   subordination, or as a degraded ritual maintained by institutional
 *   inertia. The theater_ratio (0.65) reflects that formal procedural
 *   safeguards in ICC practice are substantially performative: witness
 *   protection fails in active conflict zones, state cooperation requests are
 *   routinely ignored, prosecutorial independence is constrained by UN
 *   Security Council dynamics, and case selection reflects geopolitical
 *   factors as much as legal principles.
 *
 * KEY AGENTS:
 *   - Ukrainian State Actors: Primary targets (powerless/trapped) — subject to ICC jurisdiction with no exit mechanism; bear reputational and legal risk; cannot control prosecution agenda
 *   - Conflict Victims and Civil Society: Primary beneficiaries and secondary victims (moderate/constrained) — gain accountability forum unavailable domestically but face selective prosecution and procedural barriers
 *   - International Criminal Court: Primary beneficiary (institutional/arbitrage) — expands jurisdiction, secures funding and mandate expansion, develops precedent; controls prosecutorial agenda
 *   - Transitional Justice Organizations: Secondary coordinator (organized/mobile) — build capacity-development pathway with sunset logic; face resource constraints and institutional resistance
 *   - Rome Statute Signatory States (P5-Aligned): Secondary beneficiary (institutional/arbitrage) — selectively enforce ICC authority to advance geopolitical interests; have exit options
 *   - Treaty Framework: Institutional structure (institutional/arbitrage) — persists through formal compliance despite functional degradation (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(icc_prosecution_ukraine, 0.58).
domain_priors:suppression_score(icc_prosecution_ukraine, 0.72).
domain_priors:theater_ratio(icc_prosecution_ukraine, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(icc_prosecution_ukraine, extractiveness, 0.58).
narrative_ontology:constraint_metric(icc_prosecution_ukraine, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(icc_prosecution_ukraine, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(icc_prosecution_ukraine, tangled_rope).
narrative_ontology:human_readable(icc_prosecution_ukraine, "ICC Prosecution of Ukraine Conflict Actors").
narrative_ontology:topic_domain(icc_prosecution_ukraine, "international_law/conflict_accountability").

domain_priors:requires_active_enforcement(icc_prosecution_ukraine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(icc_prosecution_ukraine, international_legal_institutions).
narrative_ontology:constraint_beneficiary(icc_prosecution_ukraine, conflict_victims_seeking_accountability).
narrative_ontology:constraint_victim(icc_prosecution_ukraine, ukrainian_sovereignty).
narrative_ontology:constraint_victim(icc_prosecution_ukraine, prosecutorial_impartiality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UKRAINIAN SOVEREIGNTY (SNARE) — Ukraine cannot exit ICC jurisdiction once the court asserts authority. The constraint imposes external legal authority over domestic conflict actors with minimal coordination benefit. Ukraine faces prosecution risk while remaining unable to control the court's agenda, timing, or scope. Maximum suppression from a powerless perspective.
constraint_indexing:constraint_classification(icc_prosecution_ukraine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONFLICT VICTIMS AND CIVIL SOCIETY (TANGLED ROPE) — Experience both coordination (ICC provides accountability mechanism otherwise unavailable) and extraction (prosecutorial bias, selective enforcement, long timelines). Can organize but face significant barriers — political pressure, resource constraints, lack of direct control over prosecution priorities. Mixed experience of genuine coordination benefit alongside asymmetric extraction through procedural imbalance.
constraint_indexing:constraint_classification(icc_prosecution_ukraine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INTERNATIONAL LEGAL INSTITUTIONS (ROPE) — ICC and allied institutions (UN bodies, EU mechanisms) benefit from expanded authority and resources flowing from Ukraine investigations. Experience the constraint as pure coordination: establishing precedent, demonstrating institutional capacity, securing funding. Have exit options (can deprioritize Ukraine cases) and experience extraction flow toward them, not away. Net beneficiary position.
constraint_indexing:constraint_classification(icc_prosecution_ukraine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRANSITIONAL JUSTICE ARCHITECTURE (SCAFFOLD) — Temporary coordination mechanism building complementarity between ICC and domestic Ukrainian courts. Organizations like the International Centre for Transitional Justice frame this as a developmental pathway with sunset: as Ukrainian institutions strengthen post-conflict, ICC role diminishes. High suppression currently but declining over generational timescale as capacity builds. Has sunset clause contingent on institutional development.
constraint_indexing:constraint_classification(icc_prosecution_ukraine, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TREATY-BASED PROCEDURAL FRAMEWORK (PITON) — The Rome Statute provisions on state cooperation, witness protection, and prosecutorial discretion are largely performative. High theater (0.65): formal procedures exist but enforcement mechanisms are weak; signatory states frequently ignore cooperation requests; witness protection fails in conflict zones; prosecutorial independence is rhetorical while political pressure shapes case selection. The framework persists through institutional inertia despite declining functional capacity. Theater exceeds coordination value.
constraint_indexing:constraint_classification(icc_prosecution_ukraine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some tension between sovereignty and accountability is inherent to international law: any system that prosecutes cross-border crimes creates authority conflicts. This perspective risks naturalizing what is actually a contingent institutional design choice — the ICC's specific prosecutorial model, not the accountability-sovereignty tension itself. The engine's false summit detector will reveal this as a false mountain.
constraint_indexing:constraint_classification(icc_prosecution_ukraine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(icc_prosecution_ukraine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(icc_prosecution_ukraine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(icc_prosecution_ukraine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(icc_prosecution_ukraine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(icc_prosecution_ukraine, TR),
    TR >= 0.70.

:- end_tests(icc_prosecution_ukraine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Ukraine faces asymmetric prosecution risk while having limited control over court priorities. The extractiveness is not as severe as pure predation (which would exceed 0.70) because genuine accountability coordination exists — victims do gain access to mechanisms, and some investigations target both sides. However, the extraction is real: geopolitical factors shape case selection disproportionately; powerful states exercise selective pressure on prosecution; Ukraine's sovereignty is constrained while its power to shape outcomes is minimal. Theater ratio (0.65): Moderate-high. Formal procedural safeguards create appearance of impartial justice, but witness protection fails when prosecution occurs during active conflict; state cooperation mechanisms are routinely defied; prosecutorial discretion operates within geopolitical constraints. The procedural theater has increased over the interval as pressure for prosecution intensifies and ICC formally expands Ukraine investigations. Suppression (0.72): High. Ukraine faces significant barriers to exit: treaty obligations are binding, reputational cost of withdrawal would be severe (perceived as obstruction of justice), domestic political pressure from victims and international community prevents opt-out, and no alternative accountability mechanism has equal legitimacy in international forums.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the indexical classification principle across the international law domain. Ukrainian state actors see a Snare — they cannot exit, face prosecution risk, and bear extraction with minimal coordination benefit (they did not request ICC intervention and cannot control its scope). Conflict victims see a Tangled Rope — the ICC provides accountability they need AND extracts through selective enforcement and procedural bias that favors institutional expansion over victim justice. The ICC and allied institutions see a Rope — they experience pure coordination benefit (institutional growth, funding, mandate expansion) with no experienced extraction. The transitional justice coalition sees a Scaffold — a temporary mechanism that will sunset as domestic Ukrainian capacity develops. The treaty framework itself is a Piton — formal procedures persist through institutional inertia despite functional degradation in conflict zone environments. The civilizational analytical observer risks a false Mountain — seeing the sovereignty-accountability tension as inherent to international law rather than as a contingent institutional design that could be restructured through complementarity mechanisms with genuine local control.
 *
 * DIRECTIONALITY LOGIC:
 *   Ukrainian state actors occupy the highest d value (close to 1.0) because they are trapped with no exit and bear costs asymmetrically — their d derives from victim status (targets of prosecution) combined with trapped exit option. International institutions have d near 0.0 because they benefit from the constraint and have arbitrage exit options (can reallocate resources away from Ukraine cases). Conflict victims occupy intermediate d (0.55–0.65) because they benefit from accountability access but face extractive procedures and selective enforcement — they are partial beneficiaries with constrained mobility. The directionality derivation creates a perspectival gap: the victim's experienced extractiveness (f(d) computed from their mixed position) is moderate, not maximum, reflecting that they gain real coordination benefit alongside extraction. This distinguishes the constraint from a pure Snare where all powerless agents would see maximum extraction regardless of derived d.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolution requires distinguishing genuine accountability coordination from geopolitical extraction disguised as justice. The Tangled Rope classification holds if: (1) victims genuinely benefit from accountability forum unavailable domestically, (2) the benefit is asymmetric (concentrated on international institutions relative to victim need), and (3) enforcement is active (the ICC actually investigates and prosecutes). If prosecutorial selectivity (omega_1) is confirmed to operate on geopolitical rather than legal principles, the constraint drifts toward Snare and the tangled rope classification becomes misdiagnosis of pure extraction. If Ukrainian capacity (omega_2) cannot materialize within generational timescale, the Scaffold sunset clause fails and suppression becomes permanent, reclassifying all perspectives toward extraction types. If victim participation (omega_4) is performative rather than authentic, the beneficiary status of victims becomes doubtful, and the constraint appears more extractive. The mandatrophy test: can this constraint serve both accountability (genuine coordination) and extraction (asymmetric benefit to institutions) simultaneously? Yes — international institutions coordinate accountability AND extract through control over case selection and procedural bias. This simultaneous functioning is what makes it Tangled Rope rather than requiring election between types.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prosecutorial_selectivity_mechanism,
    'Is ICC case selection driven by prosecution principles or by political pressure from powerful actors?',
    'Content analysis of prosecutor statements vs case docket timing; correlation between ICC case openings and geopolitical alignment of P5 states; comparison of prosecutorial intensity across NATO-aligned vs non-aligned conflict zones',
    'If political: extraction mechanism is real (Snare classification confirmed). If principled: coordination mechanism is stronger (Rope classification more defensible). This determines whether suppression is structural bias or procedural necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prosecutorial_selectivity_mechanism, empirical, 'Whether case selection reflects prosecution principles or political pressure').

omega_variable(
    ukrainian_institutional_capacity_trajectory,
    'Can Ukrainian domestic courts realistically achieve the capacity for genocide and crimes against humanity trials within the generational timescale the Scaffold perspective assumes?',
    'Assessment of Ukrainian court capacity, investigative resources, witness security infrastructure; comparison to timelines in post-conflict jurisdictions (Rwanda, former Yugoslavia, Cambodia); expert evaluation of training and institutional development programs',
    'If capacity is achievable: Scaffold classification with sunset clause is realistic. If not: scaffold is aspirational theater masking permanent ICC dependence (reclassifies to Tangled Rope or Snare). This determines whether generational-level suppression is temporary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ukrainian_institutional_capacity_trajectory, empirical, 'Whether Ukrainian courts can achieve capacity for complex trials within generational timeframe').

omega_variable(
    rome_statute_constraint_vs_design_choice,
    'Are the tension between sovereignty and accountability and the asymmetric enforcement patterns inherent to international law, or are they artifacts of specific ICC institutional design and P5 state resistance to universal jurisdiction?',
    'Historical comparison of accountability mechanisms in earlier conflicts; analysis of alternative prosecutorial models (international tribunals with explicit complementarity agreements, hybrid courts, transitional justice mechanisms); legal analysis of Rome Statute''s discretionary provisions and how they are exercised',
    'If inherent: Mountain classification has merit (tension is structural limit). If design artifact: the constraint is contingent and the false summit is correctly identified. This shapes whether suppression is unavoidable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rome_statute_constraint_vs_design_choice, conceptual, 'Whether accountability-sovereignty tension is inherent or design-dependent').

omega_variable(
    victim_participation_authenticity,
    'Do victim participation mechanisms in ICC proceedings constitute genuine accountability coordination or performative victim representation masking institutional priorities?',
    'Comparative analysis of victim input in case selection vs actual prosecution focus; interview data on victim satisfaction with participation mechanisms; correlation between victim-preferred prosecutorial priorities and ICC case docket',
    'If authentic: beneficiary status of victims is real (Tangled Rope extraction component justified). If performative: victims are instrumentalized by institution, raising extraction component (closer to Snare). This determines theater ratio validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_participation_authenticity, empirical, 'Whether victim participation is authentic coordination or performative representation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(icc_prosecution_ukraine, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(icc_ukr_tr_t0, icc_prosecution_ukraine, theater_ratio, 0, 0.42).
narrative_ontology:measurement(icc_ukr_tr_t3, icc_prosecution_ukraine, theater_ratio, 3, 0.55).
narrative_ontology:measurement(icc_ukr_tr_t6, icc_prosecution_ukraine, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(icc_ukr_be_t0, icc_prosecution_ukraine, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(icc_ukr_be_t3, icc_prosecution_ukraine, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(icc_ukr_be_t6, icc_prosecution_ukraine, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(icc_prosecution_ukraine, enforcement_mechanism).
narrative_ontology:affects_constraint(icc_prosecution_ukraine, ukrainian_sovereignty_constraints).
narrative_ontology:affects_constraint(icc_prosecution_ukraine, international_law_asymmetry).
narrative_ontology:affects_constraint(icc_prosecution_ukraine, conflict_victim_accountability_access).

% DUAL FORMULATION NOTE:
% ICC prosecution of Ukraine is downstream of specific war crimes allegations but represents a distinct structural constraint on how accountability mechanisms operate post-conflict. The upstream constraints (specific crimes, evidence availability) have their own epistemic status; ICC prosecution authority has distinct extractiveness reflecting institutional design choices about complementarity, state cooperation, and prosecutorial discretion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(icc_prosecution_ukraine, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
