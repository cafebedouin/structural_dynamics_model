% ============================================================================
% CONSTRAINT STORY: sovereign_immunity_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_immunity_extraction, []).

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
 *   constraint_id: sovereign_immunity_extraction
 *   human_readable: Sovereign Immunity as Extractive Constraint on International Justice
 *   domain: international_law/political_economy
 *
 * SUMMARY:
 *   Sovereign immunity protects states from being sued in foreign courts, an
 *   arrangement derived from Westphalian sovereignty and mutual state
 *   recognition. The constraint operates at the intersection of international
 *   law, political power, and justice — it simultaneously coordinates
 *   interstate relations through mutual non-interference while extracting
 *   protection for powerful states that commit atrocities. The extractiveness
 *   has increased over 200+ years as human rights norms create growing
 *   expectations of accountability, while immunity remains formally
 *   unchanged, creating widening gap between principle and practice. Theater
 *   ratio has also increased as international courts (ICJ, ICC) perform
 *   justice functions they cannot enforce, sustaining appearance of rule of
 *   law while immunity ensures powerful violators face no actual
 *   consequences.
 *
 * KEY AGENTS:
 *   - Transnational Victims: Primary victims (powerless/trapped) — persons harmed by state violence or environmental damage cannot sue the state responsible; immunity blocks all legal remedies
 *   - Weak or Non-Aligned States: Secondary victims (moderate/constrained) — theoretically protected by mutual immunity but cannot enforce claims against powerful states; face power asymmetry in selective immunity invocation
 *   - Powerful States (P5, regional hegemons): Primary beneficiaries (institutional/arbitrage) — extract protection from immunity while maintaining flexibility to pursue selective accountability mechanisms against weaker states
 *   - International Courts: Institutional actors (institutional/constrained) — maintain legitimacy theater; issue judgments that powerful states routinely ignore; constrained by inability to enforce against protected actors
 *   - Human Rights NGOs and Advocacy Coalitions: Organized victims (organized/constrained) — mobilize around immunity exceptions; achieve occasional breakthroughs (universal jurisdiction cases, truth commissions) but lack enforcement power; constrained by state resistance but organized enough to maintain pressure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing immunity as inherent to state sovereignty when it is historically contingent institutional convention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_immunity_extraction, 0.68).
domain_priors:suppression_score(sovereign_immunity_extraction, 0.72).
domain_priors:theater_ratio(sovereign_immunity_extraction, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_immunity_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(sovereign_immunity_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sovereign_immunity_extraction, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_immunity_extraction, snare).
narrative_ontology:human_readable(sovereign_immunity_extraction, "Sovereign Immunity as Extractive Constraint on International Justice").
narrative_ontology:topic_domain(sovereign_immunity_extraction, "international_law/political_economy").

domain_priors:requires_active_enforcement(sovereign_immunity_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_immunity_extraction, powerful_states).
narrative_ontology:constraint_victim(sovereign_immunity_extraction, transnational_victims).
narrative_ontology:constraint_victim(sovereign_immunity_extraction, weak_states).
narrative_ontology:constraint_victim(sovereign_immunity_extraction, international_courts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Victims of state atrocities (genocide, torture, environmental destruction, forced labor) cannot sue the state responsible because sovereign immunity blocks access to courts. No alternative remedy exists; victims are trapped within the immunity framework. Maximum extraction: the state extracts impunity while victims bear full cost of injury without legal redress.
constraint_indexing:constraint_classification(sovereign_immunity_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Smaller states benefit from the mutual recognition of sovereignty through immunity (coordination function) but also lose access to justice against powerful states. Constrained by power asymmetry — they could theoretically invoke immunity in a domestic court, but international enforcement of their claims against powerful states is blocked. Mixed experience: genuine coordination benefit paired with asymmetric extraction vulnerability.
constraint_indexing:constraint_classification(sovereign_immunity_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Benefits from sovereign immunity through arbitrage: can invoke immunity to block suits while maintaining diplomatic flexibility to sue other states in selective contexts. Experiences sovereignty framework as pure coordination — a mutual recognition system that preserves state autonomy. Net beneficiary: immunity protection runs toward these actors; costs are externalized to weaker actors and victims.
constraint_indexing:constraint_classification(sovereign_immunity_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% ICJ, ICC, and other tribunals maintain the fiction of sovereign equality while operating under immunity constraints that prevent enforcement against powerful states. The institutional theater is high: courts issue orders and judgments that powerful states ignore without consequences. Theater ratio reflects that courts perform legitimacy while lacking enforcement power — they exist partly to sustain the appearance of international law rather than to deliver justice.
constraint_indexing:constraint_classification(sovereign_immunity_extraction, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Organized coalitions (Amnesty International, Human Rights Watch, ICC prosecutor advocates) benefit from the immunity framework as a focus point for mobilization and legitimacy claims. But they also experience extraction: their advocacy lacks enforcement mechanism; states ignore their findings without cost. Constrained by lack of direct enforcement power but organized enough to maintain pressure and win occasional exceptions (universal jurisdiction prosecutions, ICC referrals). Mixed experience with genuine coordination benefit (shared norms) and asymmetric extraction (enforcement gap).
constraint_indexing:constraint_classification(sovereign_immunity_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% From civilizational perspective, sovereign immunity appears immutable: it is constitutive of the state system itself since Westphalia (1648). A world without sovereign immunity would require abandoning state sovereignty — it seems a natural law of international relations. However, this perspective risks naturalizing a contingent institutional arrangement. The structural data reveals that sovereignty and immunity are historically contingent, culturally specific to European interstate system, and already eroding through selective exceptions (universal jurisdiction, ICC, truth commissions). False summit alert.
constraint_indexing:constraint_classification(sovereign_immunity_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_immunity_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sovereign_immunity_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sovereign_immunity_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_immunity_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sovereign_immunity_extraction, TR),
    TR >= 0.70.

:- end_tests(sovereign_immunity_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The original extractiveness (0.45 at t=0, during peak Westphalian sovereignty consensus) has grown to 0.68 as human rights norms have matured, creating gap between principle (accountability for atrocities) and practice (immunity blocks suits). The asymmetry is severe: powerful states extract protection while powerless victims extract nothing. Suppression (0.72): Very high. Multiple barriers prevent victims from accessing justice: (1) jurisdictional immunity blocks suits in foreign courts, (2) victims lack resources to pursue ICC referrals or universal jurisdiction cases, (3) powerful states block ICC enforcement through Security Council or non-participation, (4) victims cannot exit the international legal system — it is the only system that exists. Suppression is structural, not fully internalized, because victims actively seek accountability mechanisms despite barriers. Theater ratio (0.65): Moderately high and increasing. International courts perform justice function they cannot enforce: ICC issues indictments against sitting heads of state whom Security Council members protect; ICJ issues orders that Russia and China ignore; regional courts convict powerful states whose enforcement mechanisms are blocked. The performance sustains legitimacy of the international legal system while immunity prevents enforcement. Theater has increased over time as court activity has grown while enforcement against powerful states has stalled.
 *
 * PERSPECTIVAL GAP:
 *   The constraint displays maximum perspectival divergence. Powerful states see pure coordination (Rope) — a system preserving mutual autonomy. Weak states see mixed coordination-extraction (Tangled Rope) — the system both protects their sovereignty and denies them justice against powerful violators. International courts see their own degraded theater (Piton) — they maintain legitimacy performance while lacking enforcement power. Human rights coalitions see a hybrid with sunset potential (Tangled Rope) — they recognize genuine accountability gains from exceptions (universal jurisdiction, ICC referrals) while facing extraction through enforcement gaps. Victims see pure extraction with no exit (Snare) — they bear full cost of state violence without access to justice. The civilizational analytical observer risks seeing natural law (Mountain) — Westphalian sovereignty seems immutable — but this is a false summit. Sovereignty itself is historical; immunity is not essential to it; the Westphalian system was contingent choice, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful states derive d ≈ 0.05-0.10 (beneficiaries + arbitrage options) producing negative f(d) — they experience effective extraction χ as subsidy, not cost. Victims derive d ≈ 0.95 (trapped + no arbitrage) producing f(d) ≈ 1.42 — they experience maximum extraction. Weak states derive d ≈ 0.55-0.65 (constrained exit + mixed beneficiary/victim status) producing f(d) ≈ 0.75-1.00 — they experience moderate extraction. International courts derive d ≈ 0.70 (constrained + no enforcement power) producing f(d) ≈ 1.12 — they experience above-baseline extraction in the form of enforcement failure. Organized coalition derives d ≈ 0.60 (constrained + partial agency + occasional wins) producing f(d) ≈ 0.85 — they experience moderate extraction. These derivations confirm snare classification from victim perspective (d → high χ → high experienced extraction) and rope from beneficiary perspective (d → low χ → low/negative experienced extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE WITH FALSE SUMMIT ALERT: The constraint resolves mandatrophy by revealing that the analytical 'natural law' perspective commits a category error. Sovereign immunity is not a natural law of state systems — it is a contingent institutional convention that emerged from 17th-century European practices and has never been universal (non-Westphalian systems operated differently; modern EU members accept accountability mechanisms). The mountain classification from the civilizational/analytical perspective is a false summit: it naturalizes what is actually a contingent arrangement serving powerful state interests. The genuine mandatrophy resolution is that immunity is a *political choice* disguised as natural law. States could abandon it (EU model shows viability) but don't, because powerful states benefit. This reveals the constraint as a snare: extraction is masked as necessity. The scaffold perspective (organized coalition seeing exceptions as sunset mechanism) is aspirational rather than structural — exceptions accumulate very slowly; functional collapse is centuries away if it happens at all. The constraint is stable snare, not temporary scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_necessity_thesis,
    'Is sovereign immunity truly essential to state sovereignty, or is it a contingent institutional convention?',
    'Historical analysis of pre-Westphalian state systems, non-European sovereignty concepts, and contemporary sovereignty arrangements without immunity (EU member states, regional courts with enforcement power). Comparative examination of whether states can maintain autonomy while accepting limited accountability.',
    'If essential: mountain classification is correct — immunity is structural law. If contingent: mountain is false summit — immunity is extractive arrangement misrepresented as natural law. Would shift all perspectives toward higher chi and snare/tangled_rope classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_necessity_thesis, conceptual, 'Whether sovereign immunity is essential to state sovereignty or contingent institutional convention').

omega_variable(
    enforcement_mechanism_viability,
    'Can international courts enforce judgments against powerful states without creating coercive hierarchy that contradicts sovereign equality?',
    'Analysis of enforcement mechanisms in regional systems (European Court of Human Rights, African Court on Human and Peoples'' Rights); examination of ICC enforcement against states without Security Council support; study of mechanisms that maintain peer accountability without dominance.',
    'If viable: organized coalition perspectives shift toward rope (genuine coordination). If not viable: confirms snare classification — immunity is lock-in, not coordination structure. Affects whether sunset clause is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_viability, empirical, 'Whether international enforcement against powerful states is structurally viable').

omega_variable(
    exception_accumulation_tipping_point,
    'At what accumulation rate of exceptions (universal jurisdiction, ICC referrals, truth commissions, sanctions) does sovereign immunity functionally collapse even if formally retained?',
    'Longitudinal tracking of exception cases over 20 years; measurement of effective enforcement rate by state power category; threshold identification where powerful states switch from ignoring to complying with international judgments.',
    'If tipping point is imminent: scaffold classification becomes plausible — immunity is temporary constraint. If tipping point is centuries away: snare classification holds — immunity is stable lock-in. Directs prediction of future constraint trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exception_accumulation_tipping_point, empirical, 'Accumulation rate of exceptions and functional collapse threshold').

omega_variable(
    moral_hazard_feedback_loop,
    'Does immunity protection incentivize states to commit high-cost atrocities, creating moral hazard that would collapse without immunity enforcement?',
    'Comparative analysis of state behavior before/after immunity exceptions (e.g., post-ICC indictment deterrence); study of sanction effectiveness as enforcement mechanism; examination of whether immunity removal would increase or decrease atrocity risk.',
    'If moral hazard is strong: suppression metric is justified; extraction requires enforcement suppression. If weak: suppression can be reduced without explosion in atrocities; snare classification may be too severe. Affects mandatrophy reasoning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard_feedback_loop, empirical, 'Whether immunity protection creates moral hazard incentivizing atrocities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_immunity_extraction, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sovimm_tr_t0, sovereign_immunity_extraction, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sovimm_tr_t100, sovereign_immunity_extraction, theater_ratio, 100, 0.58).
narrative_ontology:measurement(sovimm_tr_t200, sovereign_immunity_extraction, theater_ratio, 200, 0.65).

% Extraction over time
narrative_ontology:measurement(sovimm_be_t0, sovereign_immunity_extraction, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sovimm_be_t100, sovereign_immunity_extraction, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(sovimm_be_t200, sovereign_immunity_extraction, base_extractiveness, 200, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_immunity_extraction, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_immunity_extraction, international_court_legitimacy).
narrative_ontology:affects_constraint(sovereign_immunity_extraction, universal_jurisdiction_scope).
narrative_ontology:affects_constraint(sovereign_immunity_extraction, great_power_accountability_gap).

% DUAL FORMULATION NOTE:
% Sovereign immunity decomposes into distinct constraints: (1) mutual state recognition (rope-type, genuine coordination), (2) protective shield against accountability (snare-type, pure extraction), (3) international court performance theater (piton-type, degraded institution). This story focuses on the extraction dimension; the coordination dimension could be decomposed into a separate constraint story (state_mutual_recognition_coordination) with lower extractiveness. The network link reflects that they are structurally coupled — immunity protections depend on mutual state recognition, which feeds protective asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_immunity_extraction, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
