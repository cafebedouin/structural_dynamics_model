% ============================================================================
% CONSTRAINT STORY: us_international_accountability_exemption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_international_accountability_exemption, []).

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
 *   constraint_id: us_international_accountability_exemption
 *   human_readable: U.S. International Accountability Exemption
 *   domain: international_law/geopolitics
 *
 * SUMMARY:
 *   The U.S. International Accountability Exemption is a structural
 *   arrangement in which the United States maintains systematic exclusion
 *   from international criminal jurisdiction while simultaneously
 *   participating in, funding, and legitimizing international law
 *   institutions that apply such jurisdiction to other nations. The exemption
 *   operates through multiple mechanisms: non-ratification of the
 *   International Criminal Court statute, security council veto power over
 *   ICC actions, immunity agreements with countries hosting U.S. military
 *   bases ('Article 98 agreements'), and diplomatic pressure against other
 *   signatories. This constraint represents a pure extraction mechanism
 *   targeting international rule of law while maintaining the theatrical
 *   appearance of commitment to universal accountability. The constraint has
 *   intensified over two decades: extractiveness has increased from 0.52 to
 *   0.68, reflecting expanded U.S. military operations in asymmetric
 *   conflicts where accountability questions are acute; theater has increased
 *   from 0.42 to 0.58 as the performative commitments to international law
 *   have grown while the operative exemption has solidified.
 *
 * KEY AGENTS:
 *   - Non-U.S. Citizens in Conflict Zones: Primary victims (powerless/trapped) — civilians and combatants subject to U.S. military action without access to international justice
 *   - International Rule of Law System: Primary victim (powerless/trapped) — abstract collective good undermined by selective enforcement
 *   - U.S. Military Leadership and Executive Branch: Primary beneficiaries (institutional/arbitrage) — capture protection from international prosecution and operational autonomy
 *   - U.S. Defense Contractors: Secondary beneficiaries (institutional/arbitrage) — protected from accountability for actions in conflict zones
 *   - Allied Nations with U.S. Military Presence: Secondary actors (powerful/mobile) — experience constraint as coordination mechanism, maintain exemption through basing agreements
 *   - International Criminal Court and UN System: Institutional victims (institutional/trapped) — maintain legitimacy through performative authority while actual jurisdiction is constrained
 *   - International Civil Society: Organized advocates (organized/constrained) — identify structural problems and build alternative accountability pathways
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing great power privilege as immutable law of international systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_international_accountability_exemption, 0.68).
domain_priors:suppression_score(us_international_accountability_exemption, 0.72).
domain_priors:theater_ratio(us_international_accountability_exemption, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_international_accountability_exemption, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_international_accountability_exemption, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_international_accountability_exemption, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_international_accountability_exemption, snare).
narrative_ontology:human_readable(us_international_accountability_exemption, "U.S. International Accountability Exemption").
narrative_ontology:topic_domain(us_international_accountability_exemption, "international_law/geopolitics").

domain_priors:requires_active_enforcement(us_international_accountability_exemption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_international_accountability_exemption, us_military_leadership).
narrative_ontology:constraint_beneficiary(us_international_accountability_exemption, us_executive_branch).
narrative_ontology:constraint_beneficiary(us_international_accountability_exemption, us_defense_contractors).
narrative_ontology:constraint_victim(us_international_accountability_exemption, international_rule_of_law).
narrative_ontology:constraint_victim(us_international_accountability_exemption, non_us_citizens).
narrative_ontology:constraint_victim(us_international_accountability_exemption, international_court_jurisdiction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-U.S. CITIZENS (SNARE) — Civilians and combatants in conflict zones where U.S. forces operate face military action with no recourse to international justice mechanisms. Cannot exit the operational theater without abandonment of territory/property. The exemption ensures that injuries, deaths, and property destruction cannot be adjudicated by ICC or equivalent bodies. Maximum experienced extraction with no exit option.
constraint_indexing:constraint_classification(us_international_accountability_exemption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNATIONAL RULE OF LAW (SNARE) — The abstract institutional commitment to universal jurisdiction and equal accountability before international courts is systematically undermined. One permanent Security Council member with veto power and the world's largest military maintains a blanket exemption from the system it helped create. Cannot exit; bears the extraction of legitimacy degradation. The system persists performatively while its core principle — universal accountability — is violated.
constraint_indexing:constraint_classification(us_international_accountability_exemption, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: U.S. EXECUTIVE BRANCH / MILITARY (TANGLED ROPE) — The exemption provides genuine coordination benefit (protection of U.S. personnel from international prosecution, freedom of operational planning without ICC constraint) alongside asymmetric extraction (enforced through military and diplomatic power). Benefits from the constraint while maintaining the institutional apparatus that enforces it. High agency and clear exit capacity (could ratify ICC at any time) but chooses not to. Sees the constraint as coordination.
constraint_indexing:constraint_classification(us_international_accountability_exemption, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALLIES WITH U.S. MILITARY PRESENCE (ROPE) — Countries hosting U.S. bases benefit from military protection and strategic alliance while accepting the exemption as a cost of partnership. Mobile exit capacity (can renegotiate basing agreements, exit the alliance at diplomatic cost). Experience the constraint as primarily coordination — the alliance benefits outweigh the accountability cost from their perspective. Network effects and security guarantees create genuine coordination function.
constraint_indexing:constraint_classification(us_international_accountability_exemption, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL CRIMINAL COURT / UN SYSTEM (PITON) — The ICC and UN human rights mechanisms maintain institutional legitimacy and symbolic authority despite systematic exemption of the world's largest military power. The institutions perform their functions (conduct investigations, issue arrest warrants, establish jurisprudence) while their core authority is structurally constrained by Security Council veto and U.S. non-membership. Theater ratio high (0.58) because the performative aspects — declarations of universal jurisdiction, trials of other nations' officials, human rights rhetoric — coexist with demonstrated powerlessness over U.S. actions. Maintained through institutional inertia and funding relationships.
constraint_indexing:constraint_classification(us_international_accountability_exemption, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL CIVIL SOCIETY (SCAFFOLD) — NGOs, international courts, and transitional justice advocates see the exemption as a temporary problem solvable through norm evolution. Constrained by lack of enforcement power but organized through international networks. The perspective envisions a sunset: as U.S. relative power declines, cost-benefit calculation for ICC ratification changes; norm cascades among Global South countries strengthen ICC legitimacy; future U.S. administrations ratify as political feasibility improves. Low current chi because the advocates have identified and are building alternative pathways (universal jurisdiction in third-party courts, investigation networks, documentation systems). Sunset estimated at 20-40 years for institutional norm maturation.
constraint_indexing:constraint_classification(us_international_accountability_exemption, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / REALIST NATURAL LAW VIEW (MOUNTAIN) — From a civilizational realist perspective, great power exemption from international law is a structural feature of anarchic international systems — mighty nations simply cannot be bound by rules they did not consent to and from which they cannot exit. The exemption appears as an immutable law of geopolitics: power differentials determine accountability structures. However, the structural data reveals this as a false summit — the exemption is enforced through active diplomatic and military choice, not through immutable physical laws. Institutional alternatives (binding treaty ratification, voluntary submission to ICC) exist but are not selected.
constraint_indexing:constraint_classification(us_international_accountability_exemption, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_international_accountability_exemption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_international_accountability_exemption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_international_accountability_exemption, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_international_accountability_exemption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_international_accountability_exemption, TR),
    TR >= 0.70.

:- end_tests(us_international_accountability_exemption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The exemption directly extracts accountability protection for U.S. military personnel and leadership, preventing prosecution for actions that would be subject to ICC jurisdiction if committed by personnel of non-exempt states. The extraction is not total (domestic U.S. courts retain theoretical jurisdiction; many non-signatory states have similar exemptions) but it is substantial for the world's largest military power. The measurement increase from 0.52 to 0.68 reflects the intensification of asymmetric warfare post-2001, where accountability gaps have become more salient. Suppression (0.72): High. Multiple reinforcement mechanisms prevent non-exempt states from prosecuting U.S. personnel: security council veto prevents ICC action against U.S. nationals; Article 98 agreements require countries hosting U.S. bases to grant immunity; diplomatic pressure threatens aid withdrawal and alliance termination for countries attempting prosecution; asymmetric power disparities make actual enforcement nearly impossible. Theater ratio (0.58): Moderate-high. The U.S. rhetoric about international law, human rights, and universal accountability coexists with demonstrated structural exemption. International institutions perform their functions (trials of other nations' officials, jurisprudence development, human rights documentation) while their core principle is systematically violated. The performative content has increased as international law commitments have been articulated more explicitly while structural exemption has solidified.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows sharp perspectival divergence. Non-exempt state actors see pure extraction (Snare) — accountability protection for others with no corresponding benefit. U.S. military leadership sees coordination (Tangled Rope) — the exemption enables military operations while maintaining some international law credibility through selective enforcement. Allied states see mixed coordination and acceptance (Rope) — military protection and alliance benefits outweigh the accountability cost. International institutions see their own degradation (Piton) — performative authority while operative power is constrained. Organized advocates see a temporary problem (Scaffold) — norm evolution and power decline will eventually make exemption untenable. The realist analyst risks seeing immutable great power privilege (Mountain), but the structural data shows this is a choice, not a law — the exemption is maintained through active mechanisms (veto power, diplomatic pressure, Article 98 agreements) rather than through any inherent physical or logical limit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply by agent position. Non-exempt states and international institutions have d ≈ 0.95 (trapped victims with no exit); they bear maximum experienced extraction. U.S. military leadership has d ≈ 0.05 (beneficiary with arbitrage exit); they experience negative extraction — the constraint subsidizes their operational freedom. Allied states have d ≈ 0.45 (symmetric position); they experience moderate extraction but also benefit from military protection and alliance security. The perspective differentials are sharp because the constraint is explicitly asymmetric — designed to exempt one agent class and constrain others. The snare classification for powerless victims reflects d ≈ 0.95 yielding high f(d) and thus high χ. The tangled_rope classification for the U.S. executive reflects both benefits (coordination benefit of operational autonomy) and costs (institutional legitimacy damage); this agent experiences extraction flowing toward them, not away.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is whether the exemption is a 'necessary coordination mechanism for great power military operations' (which would justify tangled_rope classification with coordination benefits) or 'systematic extraction masquerading as coordination.' The distinction turns on the suppression mechanism. If suppression were limited to preventing ICC interference with active operations (a narrow operational security function), tangled_rope would be justified. But the measured suppression (0.72) extends to preventing all investigation, prosecution, and accountability — far broader than operational necessity. This suggests that the 'coordination' frame is a cover story for extraction. The snare classification for non-exempt victims is unambiguous: they experience pure extraction with suppression and no coordination benefit. The international rule of law system bears extraction of legitimacy degradation with no offsetting benefit. The constraint resolves mandatrophy by revealing the asymmetry: for beneficiaries, it is tangled_rope with coordination benefits; for victims, it is pure snare. This asymmetry is the diagnostic signature of false coordination language — the constraint genuinely serves coordination functions for some agents (military planning autonomy) but imposes pure extraction on others (non-exempt populations). The question is whether that mixed profile makes the whole constraint tangled_rope or whether the predominant pattern is snare. The analytical answer depends on which victim population is weighted most heavily — if non-U.S. civilians in conflict zones (billions globally) are weighted against U.S. military personnel (millions), the snare classification dominates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    power_decline_threshold,
    'At what threshold of relative U.S. power decline does the cost-benefit calculation for ICC ratification reverse?',
    'Comparative analysis of great power behavior and treaty ratification patterns as power declines (British Empire and international law regimes, Soviet Union transition); modeling of U.S. strategic calculus under different GDP/military ratios',
    'If threshold < 15 years: scaffold sunset timeline is realistic and structure is temporary. If threshold > 50 years: exemption structure is effectively permanent at policy-relevant timescales.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(power_decline_threshold, empirical, 'Threshold of relative power decline for ICC ratification reversal').

omega_variable(
    complementarity_mechanism_effectiveness,
    'Can the ICC''s complementarity principle (investigating where national courts are unwilling or unable) circumvent U.S. exemption through third-party prosecution of U.S. personnel for crimes on foreign soil?',
    'Analysis of actual complementarity cases; examination of jurisdictional boundaries (whether complementarity extends to prosecuting occupying power personnel); tracking of third-party universal jurisdiction cases against U.S. officials',
    'If complementarity proves effective: exemption erodes gradually through backdoor jurisdiction, snare classification becomes weaker. If complementarity is limited by practice: exemption remains robust, snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_mechanism_effectiveness, empirical, 'Whether ICC complementarity principle can circumvent U.S. exemption').

omega_variable(
    norm_cascade_sufficiency,
    'Is norm evolution (strengthening of universal jurisdiction norms among non-great-power states) sufficient to change U.S. calculation, or does the exemption require great power consent to change?',
    'Analysis of norm cascade mechanisms in international law; examination of whether U.S. ratification is endogenous to norm strength or exogenous to power calculation; comparison with other great power exemptions (Russia, China)',
    'If norm cascade drives change: scaffold sunset is realistic and constraint structure is temporary. If change requires power alignment: exemption persists regardless of norm evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_cascade_sufficiency, conceptual, 'Whether norm cascades are sufficient to change great power accountability structure').

omega_variable(
    extraction_versus_coordination_boundary,
    'How much of the constraint''s suppression function is genuine operational security (coordination need to prevent ICC investigation interference) versus self-dealing extraction (protection of officials who would be criminally liable)?',
    'Comparative analysis of actual ICC investigations against non-exempt states; examination of disclosure barriers imposed on ICC by U.S. basing agreements; assessment of whether suppression targets operational security or individual accountability',
    'If coordination-dominant: tangled_rope classification is accurate and extraction is justified by coordination benefit. If extraction-dominant: suppression is unjustified and snare classification is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_versus_coordination_boundary, empirical, 'Boundary between operational security and self-dealing protection in suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_international_accountability_exemption, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usiae_tr_t0, us_international_accountability_exemption, theater_ratio, 0, 0.42).
narrative_ontology:measurement(usiae_tr_t10, us_international_accountability_exemption, theater_ratio, 10, 0.5).
narrative_ontology:measurement(usiae_tr_t20, us_international_accountability_exemption, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(usiae_be_t0, us_international_accountability_exemption, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(usiae_be_t10, us_international_accountability_exemption, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(usiae_be_t20, us_international_accountability_exemption, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_international_accountability_exemption, enforcement_mechanism).
narrative_ontology:affects_constraint(us_international_accountability_exemption, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(us_international_accountability_exemption, universal_jurisdiction_doctrine).
narrative_ontology:affects_constraint(us_international_accountability_exemption, humanitarian_intervention_selectivity).
narrative_ontology:affects_constraint(us_international_accountability_exemption, security_council_veto_power).

% DUAL FORMULATION NOTE:
% The U.S. accountability exemption is downstream of broader great-power privilege structures (Security Council veto, military superiority, nuclear deterrence) but represents a distinct structural constraint focused on criminal accountability specifically. Upstream constraints have their own ε values reflecting power asymmetries; the accountability exemption has ε=0.68 reflecting the specific institutional mechanisms (Article 98 agreements, ICC non-ratification, veto power) that enforce the exemption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_international_accountability_exemption, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
