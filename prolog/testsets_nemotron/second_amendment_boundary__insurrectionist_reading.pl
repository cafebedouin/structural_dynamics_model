% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment — Insurrectionist Reading (Armed Resistance Against Tyranny)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   The insurrectionist reading of the Second Amendment treats the right to
 *   keep and bear arms as a constitutional guarantee of the people's capacity
 *   to overthrow a tyrannical government by force. Individual possession is
 *   instrumental: the right protects whatever arms are necessary for
 *   effective resistance, logically extending to military-grade weapons.
 *   State disarmament efforts are treated as precursors to tyranny. This
 *   reading has migrated from fringe to mainstream in conservative legal
 *   thought since the 1980s, culminating in Heller (2008) and Bruen (2022)
 *   adopting history-and-tradition methodology that insurrectionist scholars
 *   heavily influenced. The constraint is claimed as tangled_rope: it
 *   coordinates a genuine deterrent function (beneficiaries: armed citizens)
 *   while extracting asymmetric costs from state security apparatus and
 *   civilians caught in hypothetical conflict (victims), and requires active
 *   enforcement through judicial doctrine that blocks regulation. The
 *   claim/metric gap is deliberate: the reading claims rope (pure
 *   coordination against tyranny) while metrics describe substantial
 *   extraction and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.55).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment — Insurrectionist Reading (Armed Resistance Against Tyranny)").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, '107a6d5a-a831-467b-a403-32b9932634c8').
narrative_ontology:cs_kernel_codification('107a6d5a-a831-467b-a403-32b9932634c8', fixed_text).
narrative_ontology:cs_authority_grounding('107a6d5a-a831-467b-a403-32b9932634c8', lineage).
narrative_ontology:cs_interpretation_layer_present('107a6d5a-a831-467b-a403-32b9932634c8').
narrative_ontology:cs_reading_relation('107a6d5a-a831-467b-a403-32b9932634c8', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('107a6d5a-a831-467b-a403-32b9932634c8', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_axiom('107a6d5a-a831-467b-a403-32b9932634c8', foundational, armed_populace_deterrent_against_tyranny).
narrative_ontology:cs_axiom_status(armed_populace_deterrent_against_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('107a6d5a-a831-467b-a403-32b9932634c8', armed_populace_deterrent_against_tyranny, empirically_contingent).
narrative_ontology:cs_axiom('107a6d5a-a831-467b-a403-32b9932634c8', foundational, constitutional_protection_extends_to_military_grade_arms).
narrative_ontology:cs_axiom_status(constitutional_protection_extends_to_military_grade_arms, holdable).
narrative_ontology:cs_axiom_grounding('107a6d5a-a831-467b-a403-32b9932634c8', constitutional_protection_extends_to_military_grade_arms, deontological).
narrative_ontology:cs_reference_frame('107a6d5a-a831-467b-a403-32b9932634c8', founding_era_armed_populace_check).
narrative_ontology:cs_drift_state('107a6d5a-a831-467b-a403-32b9932634c8', post_bruen_history_tradition_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('107a6d5a-a831-467b-a403-32b9932634c8', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_in_hypothetical_armed_conflict).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, armed_populace_deterrent_against_tyranny).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, constitutional_protection_of_military_grade_arms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and organized groups who claim the right to possess military-grade arms as a deterrent against government tyranny. Their identity and political self-concept are fused with the insurrectionist reading; exit would mean abandoning a core constitutional commitment and community. They bear costs of legal compliance, social stigma, and potential criminal liability, but view these as the price of preserving the ultimate check on state power.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy, beneficiary,
    organized, generational, identity_locked, national).

% Law enforcement, military, and intelligence agencies tasked with maintaining public order and state monopoly on force. They bear operational costs of countering armed resistance movements, investigating illegal weapons, and managing civil unrest exacerbated by widespread military-grade arms. They cannot exit the constraint — they are institutionally bound to enforce laws that the insurrectionist reading treats as tyranny precursors.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus, payer,
    institutional, biographical, constrained, national).

% Ordinary civilians who would be caught in any actual armed conflict between insurrectionist groups and state forces. They bear the highest physical risk with zero political agency over the constraint. Geographic immobility, economic dependency, and lack of combat capability make exit impossible. They are the default victims if the deterrent fails and insurrection becomes kinetic.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_in_hypothetical_armed_conflict, payer,
    powerless, immediate, trapped, local).

% Judges, scholars, and advocacy organizations (e.g., certain Federalist Society networks, Gun Owners of America) who authoritatively interpret the Second Amendment through the insurrectionist lens. They set the legal agenda by litigating, drafting amicus briefs, and placing sympathetic jurists. Their power derives from institutional position and intellectual authority, not direct arms possession.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, constitutional_originalists_insurrectionist, agenda_setter,
    institutional, civilizational, analytical, national).

% Organizations and officials advocating for comprehensive firearms regulation (background checks, assault weapons bans, licensing). They are structurally excluded from the insurrectionist reading's framework — their policy preferences are treated as tyranny precursors rather than legitimate democratic output. They can litigate and legislate but cannot alter the constitutional reading that blocks their agenda.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, gun_control_advocates_and_regulators, excluded,
    organized, biographical, constrained, national).

% Scholars of constitutional history, comparative politics, and insurgency studies who analyze the insurrectionist reading's empirical claims (e.g., does an armed populace deter tyranny? What are the historical success rates of armed resistance against modern states?). They hold no stake in the constraint's enforcement but their findings challenge or support its foundational premises.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, political_science_and_history_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective deterrence against tyranny by distributing the credible threat of armed resistance across the citizenry, making oppression costly for any would-be tyrant. Solves the collective action problem of resistance: no single individual can overthrow a state, but a universally armed populace raises the cost of tyranny above the tyrant's willingness to pay.
% TRANSFER_FUNCTION: Transfers the burden of tyranny-prevention from the state (which would otherwise monopolize force) to armed citizens, who bear the costs of armament, training, legal risk, and potential kinetic conflict. Transfers physical security from civilians in conflict zones to the abstract deterrent value. Transfers regulatory authority from democratic legislatures to constitutional doctrine interpreted through the insurrectionist lens.
% ABSENT_VOICES: Victims of gun violence in non-tyrannical contexts (domestic violence, suicide, accidental shootings, urban homicide) are absent from the insurrectionist framework — their harm is not a coordinate of the constraint. Future generations who inherit a constitutional order where military-grade arms cannot be regulated are absent. International comparative cases (e.g., democracies without insurrectionist arms rights that have not descended into tyranny) are excluded from the reading's epistemic closure.
% DISAPPEARANCE_RATIONALE: If the insurrectionist reading vanished overnight, the constitutional barrier to comprehensive firearms regulation (especially military-grade arms) would collapse. Legislatures could enact bans, buybacks, and licensing without Second Amendment obstacle. The deterrent threat would dissolve, shifting the tyranny-prevention burden entirely to institutional checks. The political identity of armed-citizen communities would face existential crisis. The world would rearrange — but whether toward tyranny or toward stable regulated democracy is the contested empirical question.
% FOUNDING_PROBLEM: The Founders faced a weak central government under the Articles of Confederation and fresh memory of British tyranny. The insurrectionist reading holds that the Second Amendment was designed to ensure the people retained the physical capacity to overthrow a future tyrannical federal government — a final check when all institutional checks fail.
% FOUNDING_PROBLEM_CORROBORATION: Insurrectionist proponents (e.g., Halbrook, Kopel, certain Federalist Society jurists) attest the founding problem is live — they cite Federalist No. 46 (Madison on armed populace vs. standing army) and ratification-era state declarations. Critics (e.g., Rakove, Cornell, majority of professional historians) attest the founding problem was specifically about state militia autonomy vs. federal standing army, not individual insurrection — they cite the Militia Acts of 1792, the Whiskey Rebellion suppression, and the Constitution's Treason Clause as evidence the Founders rejected individual armed resistance to lawful government. No neutral corroborating source outside the beneficiary-adjacent tradition affirms the insurrectionist founding problem as the dominant original understanding.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__insurrectionist_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint transfers the entire burden of firearms regulation from democratic majorities to a constitutional veto held by a minority interpretation, while the actual deterrent value against modern state tyranny is empirically unproven and theoretically contested. Suppression (0.55) is moderate-high because the constraint actively blocks legislative responses to gun violence through judicial invalidation — the suppression is the point, not a side effect. Theater ratio (0.25) is low-moderate: the deterrent function is genuinely believed by beneficiaries, but the growing gap between 'arms necessary for resistance' (now requiring anti-tank, anti-aircraft, cyber capabilities) and 'arms legally protected' reveals performative maintenance. Accessibility collapse (0.45) reflects that alternatives (institutional checks, nonviolent resistance, international norms) persist but are dismissed within the reading. Resistance (0.72) is high: the constraint faces sustained legislative, scholarly, and public opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the armed citizen seat, the constraint is a rope: genuine coordination against tyranny at acceptable cost. From the civilian-in-conflict seat, it is a snare: pure extraction of their safety for a deterrent that may not work. From the state security seat, it is a tangled rope: they coordinate public order but are extractively burdened by a constitutional doctrine that treats their core mission as presumptively tyrannical. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the analytical observer's synthesis.
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizens claiming deterrent legitimacy are structural beneficiaries (d ≈ 0.2): they collect the constitutional veto and identity payoff, though they bear armament costs. State security apparatus (d ≈ 0.65): institutionally bound to enforce laws the reading treats as illegitimate, bearing operational costs of the deterrent's failure mode. Civilians in hypothetical conflict (d ≈ 0.95): trapped, powerless, bearing maximal physical risk with zero agency. Constitutional originalists (d ≈ 0.1): agenda-setters who benefit from the reading's intellectual and institutional dominance. Gun control advocates (d ≈ 0.8): excluded from the framework, their democratic output treated as tyranny. Observers (d = 0.5): analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (tyranny prevention via armed populace) is contested: insurrectionists say it's live; historians say it was solved by institutional design (separation of powers, federalism, civilian control of military) and is obsolete against modern state capacity. The arrangement persists despite the founding problem's contested status — classic mandatrophy signature. The reading's beneficiaries (armed citizens, originalist jurists) have strong identity-lock and institutional incentive to maintain the constraint even if its coordinating function is dead. The constraint is not a scaffold (no sunset, no transition plan) and not a piton (beneficiaries actively profit and defend it). Tangled rope captures the live coordination claim + asymmetric extraction + active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrent_effectiveness_against_modern_state,
    'Does an armed populace with legally accessible military-grade arms actually deter tyranny by a modern nuclear-capable surveillance state, or is the deterrent premise empirically falsified by the asymmetry of state capacity?',
    'Comparative historical analysis of armed resistance outcomes against modern states (post-1945), wargaming asymmetric conflict scenarios, and political science literature on authoritarian durability.',
    'If deterrent is falsified, the coordination function collapses and the constraint reclassifies as snare (pure extraction of civilian safety for a non-functional deterrent). If deterrent holds, tangled_rope stands with genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrent_effectiveness_against_modern_state, empirical, 'Whether the insurrectionist reading''s core coordination premise is empirically valid against 21st-century state capacity.').

omega_variable(
    military_grade_arms_boundary,
    'Where does the insurrectionist reading''s logical endpoint — ''arms necessary for effective resistance'' — actually stop? Does it include crew-served weapons, anti-aircraft, cyber weapons, WMD?',
    'Internal doctrinal development within insurrectionist legal scholarship; judicial opinions applying Bruen''s history-and-tradition test to progressively more destructive arms; legislative tests (e.g., state bans on .50 BMG, destructive devices).',
    'If the boundary is unbounded, the constraint''s extraction becomes existential (risk of WMD in private hands). If bounded, the reading must articulate a non-arbitrary limiting principle it currently lacks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_grade_arms_boundary, conceptual, 'Whether the insurrectionist reading has a coherent limiting principle for protected arms.').

omega_variable(
    committer_structure_second_amendment_kernel,
    'How does the insurrectionist reading''s structural relationship to the second_amendment_boundary kernel and its sibling readings affect its classification stability?',
    'Track judicial adoption rates of each reading; measure citation networks in Second Amendment litigation; observe whether Bruen''s history-and-tradition methodology privileges insurrectionist history over militia-conditioned history.',
    'If the insurrectionist reading becomes the dominant judicial methodology, its extraction metrics will be locked in by stare decisis. If the individual_right_reading displaces it as the controlling paradigm, the insurrectionist reading may become a piton (theatrical maintenance by identity-locked beneficiaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_second_amendment_kernel, conceptual, 'Committer-frame omega: this reading''s structural position within the kernel family and its dynamic relations to siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1868, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1868, 0.08).
narrative_ontology:measurement(seco_tr_t1934, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1934, 0.12).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1968, 0.18).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement(seco_be_t1868, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1868, 0.22).
narrative_ontology:measurement(seco_be_t1934, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1934, 0.35).
narrative_ontology:measurement(seco_be_t1968, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1968, 0.45).
narrative_ontology:measurement(seco_be_t2008, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2008, 0.58).
narrative_ontology:measurement(seco_be_t2024, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement(seco_su_t1868, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1868, 0.2).
narrative_ontology:measurement(seco_su_t1934, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1934, 0.35).
narrative_ontology:measurement(seco_su_t1968, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1968, 0.45).
narrative_ontology:measurement(seco_su_t2008, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2008, 0.5).
narrative_ontology:measurement(seco_su_t2024, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__insurrectionist_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, federal_firearms_regulatory_framework).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, state_preemption_laws_firearms).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the 'Second Amendment boundary' natural-language concept per the ε-invariance principle. The insurrectionist reading has the highest ε (0.68) because it logically requires protecting military-grade arms and treats all regulation as tyranny-precursor. The individual_right_reading has lower ε (self-defense scope, permits some regulation). The militia_conditioned_reading has lowest ε (permits comprehensive regulation). They are linked as a constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__insurrectionist_reading, institutional, 0.15).
constraint_indexing:directionality_override(second_amendment_boundary__insurrectionist_reading, powerless, 0.95).
constraint_indexing:directionality_override(second_amendment_boundary__insurrectionist_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
