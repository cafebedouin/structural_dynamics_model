% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__sacral_fidelity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: Lycurgan Laws as Sacred Divine Immutability
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   The sacral fidelity reading frames the Lycurgan laws (the constitutional
 *   code of ancient Sparta) as divine ordinance that must remain absolutely
 *   unchanged for Spartan civic virtue and stability to persist. This reading
 *   is authored from the seat of the Spartan priestly authority and the
 *   warrior elite who derive legitimacy from unquestioned adherence. The
 *   reading treats the laws as a natural law of the political
 *   universe—immutable not by human choice but by sacred mandate—and
 *   attributes Spartan decline to external military pressures (Thebes,
 *   Macedon, Persian wars) or to citizen moral failings, never to system
 *   design. This is one of three structurally distinct readings of the same
 *   kernel (the unchanging Lycurgan code); the other readings—the adaptive
 *   fiction reading and the demographic trap reading—produce different
 *   constraint types and different causal narratives from the same standing
 *   arrangement.
 *
 * KEY AGENTS:
 *   - Spartan priestly authority: interprets and guards the sacred narrative; benefits from monopoly on legitimacy
 *   - Warrior elite: derives identity and social order from law-fidelity; trapped by identity-fusion
 *   - Non-elite citizens and women: subject to strict enforcement; have no revision voice
 *   - Helots: enslaved, entirely excluded from the law's community
 *   - Military pragmatists (e.g., Lysander): would seek revision for operational effectiveness; silenced by sacred reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.18).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.22).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.11).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Lycurgan Laws as Sacred Divine Immutability").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political_philosophy/constitutional_theory").

domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, '822aaa00-f199-4ad2-b151-16f0cf1ab5bc').
narrative_ontology:cs_kernel_codification('822aaa00-f199-4ad2-b151-16f0cf1ab5bc', formalized).
narrative_ontology:cs_authority_grounding('822aaa00-f199-4ad2-b151-16f0cf1ab5bc', lineage).
narrative_ontology:cs_interpretation_layer_present('822aaa00-f199-4ad2-b151-16f0cf1ab5bc').
narrative_ontology:cs_reading_relation('822aaa00-f199-4ad2-b151-16f0cf1ab5bc', lycurgan_laws__demographic_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('822aaa00-f199-4ad2-b151-16f0cf1ab5bc', lycurgan_laws__adaptive_fiction_reading, coexists_with).
narrative_ontology:cs_axiom('822aaa00-f199-4ad2-b151-16f0cf1ab5bc', foundational, lycurgan_laws_divinely_ordained).
narrative_ontology:cs_axiom_status(lycurgan_laws_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('822aaa00-f199-4ad2-b151-16f0cf1ab5bc', lycurgan_laws_divinely_ordained, theological).
narrative_ontology:cs_axiom('822aaa00-f199-4ad2-b151-16f0cf1ab5bc', foundational, revision_is_sacrilege).
narrative_ontology:cs_axiom_status(revision_is_sacrilege, holdable).
narrative_ontology:cs_axiom_grounding('822aaa00-f199-4ad2-b151-16f0cf1ab5bc', revision_is_sacrilege, deontological).
narrative_ontology:cs_reference_frame('822aaa00-f199-4ad2-b151-16f0cf1ab5bc', lycurgan_divine_ordinance).
narrative_ontology:cs_drift_state('822aaa00-f199-4ad2-b151-16f0cf1ab5bc', late_classical_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('822aaa00-f199-4ad2-b151-16f0cf1ab5bc', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartan_warrior_elite).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, priestly_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, non_elite_spartan_citizens).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, women_and_dependents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derives legitimacy, martial virtue, and social order from unquestioned adherence to the divinely-mandated Lycurgan system. The code structures warrior identity, property arrangements, and communal hierarchy. Deviation from the code is understood as civic death and violation of sacred trust. Their exit is not merely constrained but ideologically closed: to question the laws is to cease being Spartan.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_warrior_elite, beneficiary,
    powerful, generational, identity_locked, regional).

% Administers the sacred narrative grounding Lycurgan immutability in divine will. Interprets omens, validates lawfulness of adherence, and declares revision to be sacrilege. Maintains the lineage of sacred knowledge linking the laws to Lycurgus and Apollo. Their authority rests on the unrevised state of the laws; revision would undermine their epistemic monopoly.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, priestly_authority, agenda_setter,
    institutional, generational, constrained, regional).

% Subject to strict code enforcement (military discipline, property communalism, forced austerity, eugenic oversight). Experience the laws as absolute constraints on personal autonomy and resource control. Cannot exit without becoming non-citizen or exile. The sacred framing preempts grievance: complaint is reframed as impiety or cowardice rather than legitimate objection.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, non_elite_spartan_citizens, payer,
    moderate, biographical, trapped, regional).

% Bound by stringent sumptuary, marriage, and reproductive laws justified as sacred ordinance. Have no formal voice in interpretation or amendment. Exit requires physical flight and social death. The sacred reading renders their structural subordination as divinely ordained and immune to moral review.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, women_and_dependents, payer,
    powerless, biographical, trapped, regional).

% Enslaved agricultural underclass whose labor sustains the leisure and martial training of the elite. Entirely excluded from the community whose laws are sacred to. The sacred reading legitimizes their exclusion as natural rather than constructed; they are not citizens and have no standing to contest the laws.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, helot_population, excluded,
    powerless, immediate, trapped, regional).

% Military commanders who encounter operational constraints from Lycurgan rigidity (rotation of ephorate, disband patterns, resource allocation). Would advocate for targeted revision to maximize battlefield effectiveness. Excluded from legitimate voice by the sacred reading: military pragmatism cannot override divine ordinance. Their suppression is framed as maintaining holy order against expedience.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, lysander_and_military_reformers, excluded,
    powerful, biographical, constrained, regional).

% Later historians, philosophers, and analysts examining whether Spartan decline resulted from external military factors (Thebes, Macedon), citizen virtue deficits, or inherent system brittleness. The sacral reading permits no internal-causation hypothesis; it closes the analytic space.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, historical_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains warrior martial culture and communal hierarchy through unchanging ordinance: property communalism eliminates intra-elite wealth conflict; military discipline is coordinated by fixed rotation; reproductive oversight ensures population quality. The laws coordinate a unified military society.
% TRANSFER_FUNCTION: Moves labor (helot agricultural output), autonomy (non-elite citizens and women), and interpretive authority (from citizen assemblies to priests) to sustain the warrior elite and priestly administrative class.
% ABSENT_VOICES: Helots, women, non-elite citizens, and military pragmatists who would contest the laws are structurally excluded. Women and helots have no standing in the framework at all. Non-elite citizens and military commanders are silenced by the sacred reading: their objections are reframed as impiety or cowardice rather than legitimate grievance. No external advisor could be heard because the reading is sealed to outsiders.
% DISAPPEARANCE_RATIONALE: From the sacral reading's perspective: if the laws vanished, Sparta ceases to exist as a coherent polity — communal virtue collapses, elite fragmentation into family factions follows, civic identity is destroyed. The reading's answer is world_rearranges into chaos. Sibling readings contest this: the demographic reading argues Sparta was already rearranging due to population collapse the unrevisable system could not address; the adaptive reading argues covert adaptation was already occurring beneath the sacred facade.
% FOUNDING_PROBLEM: Early Sparta faced aristocratic factional violence, property conflict, and oligarchic instability in the 8th century BCE. Lycurgus (or a reform coalition) imposed a comprehensive code to eliminate wealth-based factionalism and unify the warrior class around martial virtue through communal property and life-long military discipline.
% FOUNDING_PROBLEM_CORROBORATION: Plutarch and ancient sources attest the founding problem and the code's intention to solve it. Modern historians debate whether the problem was actually solved or whether the solution traded one set of tensions for another (demographic decline, helot rebellion risk, adaptation pressure). The sacral reading itself (articulated by Spartan priests and affirmed in Spartan practice) asserts the problem remains live: the threat of elite factionalism and civic decay requires perpetual vigilance and unrevised adherence. Corroboration from outside the benefiting parties: Aristotle critiques the system's demographic brittleness; later historians note Spartan military decline despite (or because of) absolute law-fidelity; no contemporary external source affirms the sacral reading's claim that revision would be sacrilege—that is the priests' own assertion.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, contested).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__sacral_fidelity_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__sacral_fidelity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, ExtMetricName, E),
    domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lycurgan_laws__sacral_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   From the sacral reading's vantage, extractiveness is low (0.18) because the laws are not understood as extraction but as natural/divine order—like gravity, not commerce. The sacred reading preempts the extraction framing itself. Suppression is modest (0.22) by design: genuine believers require little force; coercion is mainly applied to those who waver. Theater is minimal (0.08) because the reading positions itself as truth-telling, not performance. Accessibility collapse is very high (0.92): once the laws are understood as sacred, alternatives are not merely disfavored but literally unthinkable—to revise is to cease being Spartan and to commit sacrilege. Resistance is very low (0.11) from the elite seats; it is higher from excluded seats but those seats lack standing in the reading's epistemic framework. The measurement series tracks stability rather than drift: this reading's core claim is that the laws REMAIN unchanged across generations, so metrics stay flat or drift only slightly upward (minor increases in suppression and theater reflect the gradual erosion the demographic reading attributes to system brittleness, which the sacral reading cannot acknowledge as internal to the system).
 *
 * PERSPECTIVAL GAP:
 *   The priestly and elite seats experience the laws as natural law (unquestionable, beneficial through virtue alignment). Non-elite and women seats experience the same laws as severe constraints but lack standing to voice that experience—the sacred reading preempts their grievance by defining complaint as impiety. This is the core perspectival gap: the reading reserves the right to define what counts as legitimate critique and what is merely vice or impiety. The sibling readings (adaptive and demographic) would collapse this gap by asserting the elite have modified the laws covertly (adaptive) or that the system is failing (demographic); the sacral reading prevents both by ruling revision out of bounds.
 *
 * DIRECTIONALITY LOGIC:
 *   Spartan priestly authority and warrior elite benefit from the reading's closure: their monopoly on interpretation and their unquestioned social position rest on the sacred immutability claim (d near 0.0 for these seats). Non-elite citizens and women bear structural costs—autonomy, resource control, reproductive control—but within the reading, these costs are reframed as the price of sacred participation, not extraction (the reading denies the extraction framing). Helots are entirely outside the reading's moral community; the reading legitimizes their exclusion as pre-political. For the excluded military pragmatists and later historians, the reading is foreclosing: the sacred immutability thesis rules out operational or design-based reform, so their exit options are trapped or analytical only.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (elite factional violence and property conflict in archaic Sparta) is plausibly solved by the Lycurgan code. However, the sacral reading forbids ever testing whether the code is still necessary or whether alternative solutions might work better. The reading thus encodes a mandatrophy risk: as centuries pass and the external conditions that motivated the code change, the code persists not because it solves any live problem but because questioning it is forbidden by the sacred reading itself. The measurement series shows extractiveness and suppression stable but slightly rising—a signal that the system's costs are growing (demographic decline, helot rebellion risk, military inflexibility) but the reading's structure prevents anyone authorized to speak from acknowledging it. The reading is not itself mandatrophy (the laws are still enforced and believed by their carriers); but it is a structural closure that prevents the recognition of mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_ambiguity,
    'Are the Lycurgan laws a genuine natural law (immutable feature of human political nature) or a constructed institutional arrangement whose appearance of immutability is maintained by organized beneficiaries?',
    'Comparative institutional analysis: do other successful warrior societies require identical unchanging codes, or do they adapt? Do the Lycurgan laws exhibit markers of design (specificity to one polity''s contingencies) or universality (properties true of all warrior states)? Historical examination: do contemporary sources show priests/elite actively suppressing revision proposals, or do the laws persist without enforcement because they are genuinely universal?',
    'If natural law: the constraint is correctly classified as mountain; decline is external or moral. If constructed: the constraint should be reclassified as tangled_rope or snare; decline is system-internal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mountain_vs_constructed_ambiguity, conceptual, 'Whether the sacred immutability reading reflects genuine natural law or constructed institutional closure.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the compliance with Lycurgan immutability maintained by internalized identity-fusion (Spartans genuinely believe revision is sacrilege) or by structural suppression (priestly authority forcibly silences revision advocates)?',
    'Examination of helot rebellions, elite defections, and private criticism (if any survives in sources): do dissenters flee before speaking, suggesting structural suppression? Or do sources show no dissent at all, suggesting complete internalization? Post-exit trajectories: do exiled Spartans abandon the belief in the laws, or do they carry the sacred belief with them into exile?',
    'If internalized: suppression is high and sticky; change requires ideological conversion, not merely force. If structural: suppression is high but reversible; change becomes possible if enforcement apparatus weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether the Lycurgan constraint''s suppression is structural or internalized in identity.').

omega_variable(
    sibling_reading_forecast_pressure,
    'If the demographic reading''s claims are correct (population collapse is internal to the system, not external), does that falsify the sacral reading''s claim that the laws are divinely ordained and unchangeable?',
    'Historical data on Spartan population trends, helot population, military strength, and whether decline correlates with military losses (supporting external attribution) or precedes them (supporting internal system-design attribution). Modern demographic modeling to test whether Lycurgan reproductive laws could produce observed population trajectories.',
    'If demographic decline IS internal: the sacral reading''s claim that decline is external or moral is falsified. The reading forecloses the demographic reading''s causal story. This omega documents whether that foreclosure is justified by evidence or whether it reflects the reading''s protective closure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_forecast_pressure, empirical, 'Whether the sibling demographic reading''s findings about internal system brittleness are compatible with the sacral reading''s external-cause attribution.').

omega_variable(
    adaptive_fiction_detectability,
    'Does covert adaptation (the adaptive reading''s core claim) leave detectable traces in Spartan practice, or was the system genuinely unrevisable?',
    'Close reading of Plutarch and Xenophon for inconsistencies or hints of practice drift (e.g., property redistribution, ephoral term variation, helot manumission patterns). Archaeological evidence of wealth inequality or material culture variation that would suggest breakdown of communal property norms. Comparative examination of other Dorian states: did they show adaptation Sparta did not, and if so, can it be detected in the historical record?',
    'If covert adaptation is detectable: the adaptive reading''s claim that the sacred facade masked practical change gains empirical support, and the sacral reading''s claim of absolute immutability is undermined. If no adaptation is detectable: the sacral reading''s claim of literal unchangeability holds, but the demographic reading''s system-brittleness claim becomes harder to resolve.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptive_fiction_detectability, empirical, 'Whether the adaptive reading''s claim of covert adaptation beneath a sacred facade is empirically detectable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(lycu_tr_t50, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 50, 0.06).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 100, 0.07).
narrative_ontology:measurement(lycu_tr_t150, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 150, 0.08).
narrative_ontology:measurement(lycu_tr_t200, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 200, 0.08).
narrative_ontology:measurement(lycu_tr_t250, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 250, 0.08).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(lycu_be_t50, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 50, 0.17).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 100, 0.18).
narrative_ontology:measurement(lycu_be_t150, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 150, 0.19).
narrative_ontology:measurement(lycu_be_t200, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 200, 0.18).
narrative_ontology:measurement(lycu_be_t250, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 250, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(lycu_su_t50, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 50, 0.21).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 100, 0.22).
narrative_ontology:measurement(lycu_su_t150, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 150, 0.23).
narrative_ontology:measurement(lycu_su_t200, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 200, 0.22).
narrative_ontology:measurement(lycu_su_t250, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 250, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__sacral_fidelity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__sacral_fidelity_reading, 0.12).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__demographic_trap_reading).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% The Lycurgan laws kernel admits three structurally distinct readings: sacral_fidelity_reading (natural law, immutable, decline is external); demographic_trap_reading (brittle system, internal collapse); adaptive_fiction_reading (immutability is facade masking covert change). Each reading carries different beneficiary/victim structures, different ε values, and different causal narratives. They are linked as a constraint family via network.affects_constraints. The sacral reading forecloses the demographic and adaptive readings at the level of causal analysis—the reading preempts empirical attribution to system design. This is not a multi-observational single constraint; these are three separate constraints instantiated by three readings of one kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
