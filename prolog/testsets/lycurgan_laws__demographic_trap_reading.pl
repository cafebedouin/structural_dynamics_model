% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__demographic_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__demographic_trap_reading, []).

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
 *   constraint_id: lycurgan_laws__demographic_trap_reading
 *   human_readable: Lycurgan Constitutional Immutability as Demographic Trap
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   The Lycurgan laws—attributed to a legendary lawgiver from the 9th century
 *   BCE but codified and enforced in historical Sparta from at least the 6th
 *   century onward—created a constitutional system explicitly designed to be
 *   immutable. This constraint examines the demographic-trap reading: the
 *   very mechanism that was supposed to preserve Spartan martial superiority
 *   and egalitarianism—the irrevisable structure of the kleros system
 *   (inalienable land allotments), citizen-only military participation,
 *   restricted inheritance, and wealth-leveling mechanisms—prevented adaptive
 *   responses to population decline and became a death spiral. As Spartiate
 *   numbers collapsed from roughly 8,000 in 500 BCE to perhaps 1,000 by the
 *   3rd century BCE, the immutable law prevented the institutional adaptation
 *   (property consolidation, helot integration, mercenary reliance,
 *   immigration of new citizens) that other Greek poleis employed. The
 *   unrevisability clause itself became the snare: the constraint that was
 *   meant to prevent corruption of the ideal order ensured that when material
 *   conditions changed, Sparta could not evolve, only decline. By the
 *   Hellenistic period, Lycurgan immutability had become a myth invoked to
 *   explain Sparta's irrelevance—a piton: the performance of adhering to
 *   ancient law persisted long after the institutional function it grounded
 *   had failed.
 *
 * KEY AGENTS:
 *   - Spartiate Military Class (kleros-holding hoplites): Primary victim (powerless/trapped) — bound to immutable system; faces demographic extinction as land system prevents wealth consolidation and new citizen recruitment
 *   - Broader Spartan Citizen Body: Secondary victim (moderate/constrained) — constrained by mandatory participation in syssitia, age-grade structure, and wealth-leveling; cannot exit without expatriation
 *   - Conservative Ephorate (Five Ephors): Primary beneficiary (institutional/arbitrage) — authority locked in place by constitutional immutability; no challenge or reform possible; can reinterpret within fixed frame
 *   - Military Reformers (Agis IV, Cleomenes III): Organized reformers (organized/constrained) — see need for adaptation but face insurmountable constitutional barriers; constrained by political cost of challenging immutable law
 *   - Helot Population (Servile Labor Base): Tertiary beneficiary/victim (powerless/trapped) — provide economic base for citizen leisure, but also constrained by immutable restriction preventing their incorporation as full citizens even as Spartiate numbers decline
 *   - Lycurgan Myth System: Institutional performance (institutional/arbitrage) — by 4th-3rd century, the theater of invoking Lycurgus persists after institutional function has degraded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.58).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.72).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan Constitutional Immutability as Demographic Trap").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, '3a0b51ff-f414-42e4-87f0-8c83b6e07af8').
narrative_ontology:cs_kernel_codification('3a0b51ff-f414-42e4-87f0-8c83b6e07af8', fixed_text).
narrative_ontology:cs_authority_grounding('3a0b51ff-f414-42e4-87f0-8c83b6e07af8', lineage).
narrative_ontology:cs_interpretation_layer_present('3a0b51ff-f414-42e4-87f0-8c83b6e07af8').
narrative_ontology:cs_reading_relation('3a0b51ff-f414-42e4-87f0-8c83b6e07af8', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a0b51ff-f414-42e4-87f0-8c83b6e07af8', lycurgan_laws__adaptive_fiction_reading, coexists_with).
narrative_ontology:cs_axiom('3a0b51ff-f414-42e4-87f0-8c83b6e07af8', foundational, immutability_structurally_binding).
narrative_ontology:cs_axiom_status(immutability_structurally_binding, holdable).
narrative_ontology:cs_axiom_grounding('3a0b51ff-f414-42e4-87f0-8c83b6e07af8', immutability_structurally_binding, empirically_contingent).
narrative_ontology:cs_axiom('3a0b51ff-f414-42e4-87f0-8c83b6e07af8', foundational, demographic_decline_institutionally_caused).
narrative_ontology:cs_axiom_status(demographic_decline_institutionally_caused, holdable).
narrative_ontology:cs_axiom_grounding('3a0b51ff-f414-42e4-87f0-8c83b6e07af8', demographic_decline_institutionally_caused, empirically_contingent).
narrative_ontology:cs_reference_frame('3a0b51ff-f414-42e4-87f0-8c83b6e07af8', immutable_lycurgan_order).
narrative_ontology:cs_drift_state('3a0b51ff-f414-42e4-87f0-8c83b6e07af8', hellenistic_period, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3a0b51ff-f414-42e4-87f0-8c83b6e07af8', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, conservative_ephorate).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, spartiate_population).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, collective_sparta_viability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DECLINING SPARTIATE COHORT (SNARE) — Trapped in the kleros system (inalienable land allotments) and citizenship restrictions. Cannot exit the constraint without abandoning military caste status and political participatory rights. Bears full cost of demographic contraction: reduced number of hoplites, declining political power, eventual extinction as a ruling class. Maximum extraction — zero alternatives.
constraint_indexing:constraint_classification(lycurgan_laws__demographic_trap_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: BROADER SPARTAN CITIZEN BODY (SNARE) — Constrained by the immutable constitution's rigid age-grade structure, collective dining requirements (syssitia), and wealth-leveling mechanisms. Participation is mandatory; exit requires expatriation or slave rebellion, both forbidden or suicidal. Collective extractiveness: Lycurgan immutability prevents adaptive responses to population decline, military obsolescence, and economic stagnation. The citizen body experiences the constraint as existential trap as military utility declines.
constraint_indexing:constraint_classification(lycurgan_laws__demographic_trap_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONSERVATIVE EPHORATE (ROPE) — Institutional actors (the five ephors) benefit from constitutional immutability. The unrevisability clause locks in their power to enforce Lycurgan norms without risk of legal challenge or reform. They experience the constraint as pure coordination: maintaining the old order preserves their authority. Exit option is arbitrage — they can reinterpret within the fixed text or defect to Macedonian service, but the system rewards loyalty to the immutable frame.
constraint_indexing:constraint_classification(lycurgan_laws__demographic_trap_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: LYCURGAN MYTH SYSTEM (PITON) — By the 4th century BCE, Lycurgan reform was a founding myth, not a living institutional design. The theater of invoking 'Lycurgus' and 'ancient custom' persisted as legitimacy cover, but actual Spartan institutions had drifted far from their supposed exemplar. The constraint's function (maintaining egalitarianism and martial discipline) had atrophied; the theater (invoking immutable ancient law) remained. Theater ratio reflects this degradation: the myth persists long after the institutional function it supposedly grounded has failed.
constraint_indexing:constraint_classification(lycurgan_laws__demographic_trap_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, constitutional immutability appears as a natural consequence of legalist political philosophy: once laws are properly ordered (kata ten orthes logos), they should not be revised, as revision implies the original legislators were foolish or that the polis lacks wisdom. This perspective views Lycurgan unrevisability as emerging naturally from the core premise of Spartan constitutional theory. However, this is a false summit: the immutability is not a logical or natural necessity but a contingent institutional choice that benefited specific actors (the conservative ephorate) and became reified as immutable.
constraint_indexing:constraint_classification(lycurgan_laws__demographic_trap_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: MILITARY REFORM COALITION (TANGLED ROPE) — Organized agents (reform-minded ephors, mercenary commanders, helot-integrated military units) experience the constraint as mixed. The immutability prevents adaptive military reform (hoplite tactics obsolescence, need for cavalry, mercenary integration), but the coalition also benefits from the cultural prestige and cohesion that Lycurgan norms provide. Constrained by political risk of challenging immutable law, but organized enough to work within the frame. Effective extraction is mixed: some reform occurs through reinterpretation; some blocked by unrevisability.
constraint_indexing:constraint_classification(lycurgan_laws__demographic_trap_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__demographic_trap_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lycurgan_laws__demographic_trap_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lycurgan_laws__demographic_trap_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, TR),
    TR >= 0.70.

:- end_tests(lycurgan_laws__demographic_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from those locked into the kleros system and citizenship restrictions by preventing adaptive responses to changing material conditions. The original design (circa 6th century) may have been genuinely coordination-focused, but by the Hellenistic period (when we measure this story), the immutability mechanism is clearly extracting: it prevents demographic adaptation that would benefit the Spartiate class itself. The extractiveness value reflects that the constraint primarily extracts from Spartiates through forced adherence to a system that no longer serves them, not through external coercion. Suppression (0.72): High. Multiple layers prevent exit: legal prohibition of property consolidation, citizen-only military participation (helots cannot become citizens even as Spartiate numbers plummet), mandatory syssitia dining, mandatory military training, and cultural taboo against challenging Lycurgan law. No single barrier is total, but the combination is nearly absolute. Theater ratio (0.65): Moderate-high. By the Hellenistic period, invoking Lycurgus and 'ancient law' has become largely performative—the myth persists as legitimacy cover long after the institutional practice it justified has drifted or failed. Reforms are framed as reinterpretations of Lycurgus rather than as departures from him, indicating the theater has increased relative to function.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits striking perspectival divergence. The Conservative Ephorate sees pure coordination (Rope)—immutability as lock-in of their legitimate authority. The broader Spartan citizen body sees extraction (Snare)—immutability as trap. Military reformers see mixed extraction and coordination (Tangled Rope)—they need institutional flexibility but also benefit from cohesion that Lycurgan norms provide. The Lycurgan myth itself has become piton: performed loyalty to the immutable text while actual practice drifts. The analytical observer risks seeing a natural law (Mountain)—that properly-ordered constitutions should be immutable—but this is a false summit: the immutability is a contingent institutional choice that benefited specific actors and became reified. The perspectival gap reveals that 'Lycurgan immutability' is not a single constraint but a kernel read differently by different actors.
 *
 * DIRECTIONALITY LOGIC:
 *   The Conservative Ephorate's d-value is low (around 0.10-0.15): they are beneficiaries with arbitrage exit options. Their effective extraction chi is negative or near-zero—the constraint subsidizes them by locking in their power. The Spartiate class's d-value is high (0.85-0.95): they are victims facing trapped exit options. Their chi is maximized. Military reformers (organized power, constrained exit) occupy middle ground: d ≈ 0.50-0.60, producing moderate chi. The perspectival gap in d-values is large, reflecting that the constraint benefits and burdens different actors asymmetrically. The demographic trap reading emphasizes high d (victimhood) for the Spartiate class—the reading's core claim is that immutability traps them in a system that no longer serves their own interests.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy (ε > 0.70 gate requires resolution). The snare classification is mandatrophed by the piton perspective: if theater ratio has risen to 0.65 while extractiveness reached only 0.58, the constraint is experiencing functional degradation. By the time Sparta's military irrelevance was complete (late Hellenistic), the extractiveness had likely fallen while theater persisted—the constraint had become a piton. However, the demographic-trap reading's core claim is that the snare classification was accurate during the critical period (4th-3rd century BCE, t=150-300 in our measurement interval) when immutability prevented adaptive responses and demographic collapse ensued. The mandatrophy resolves by identifying the temporal boundary: snare from roughly 400-200 BCE, transitioning to piton by 100 BCE as the institutional function collapsed while the mythic frame persisted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_mechanism_binding_force,
    'Was Lycurgan immutability enforced through a legal mechanism (oath, constitutional clause, ephoral authority), a social mechanism (cultural taboo, religious sanction), or a psychological mechanism (internalized reverence for the founder)?',
    'Textual analysis of sources (Plutarch, Xenophon) for explicit prohibitions; examination of attempted constitutional challenges and their outcomes; analysis of reinterpretation strategies as evidence of the constraint''s actual flexibility',
    'If primarily legal: immutability is contingent and could have been legally overridden (snare classification justified). If primarily psychological/cultural: the constraint is binding through identity fusion, making it more like identity_locked at the collective level. If hybrid: the constraint is robust across multiple enforcement channels, making exit genuinely impossible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_mechanism_binding_force, empirical, 'Mechanism by which Lycurgan immutability was enforced').

omega_variable(
    demographic_decline_causation,
    'Did the kleros system and citizenship restrictions directly cause demographic decline, or were they sufficient but not necessary causes? Did external factors (warfare, economic change, emigration) dominate the causal picture?',
    'Demographic reconstruction from literary sources; comparative analysis with other Greek poleis facing similar external pressures but without Lycurgan constraints; counterfactual modeling of Spartan population trajectory under alternative inheritance/citizenship rules',
    'If kleros + restrictions were necessary: snare classification is robust. If merely sufficient: the constraint is one factor among many, potentially downgrading effective extraction. If external factors dominated: the demographic trap is a false cause, and the true constraint is elsewhere.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_decline_causation, empirical, 'Causal role of Lycurgan institutions in Spartan demographic decline').

omega_variable(
    unrevisability_degree_of_freedom,
    'How much reinterpretation actually occurred within the immutable frame? If Lycurgan law was effectively rigid, what explains the shifts in mercenary warfare, integration of lighter-armed units, and helot mobilization?',
    'Close reading of military practice evolution relative to stated Lycurgan principles; analysis of rhetorical gymnastics used to justify deviations; assessment of whether innovations constituted genuine constraint relaxation or merely functional drift within a nominally unchanged text',
    'High degree of freedom: the immutability is more theater than substance (piton classification strengthens). Low degree of freedom: immutability is genuinely binding (snare classification strengthens). The tension resolves the perspectival gap between the piton and snare readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unrevisability_degree_of_freedom, empirical, 'Effective degree of constraint rigidity vs. interpretive flexibility').

omega_variable(
    alternative_constitutional_framings,
    'This reading instantiates a demographic-trap framing of Lycurgan immutability. Could the same institutional data support a ''sacral fidelity'' reading (immutability as religiously mandated virtue) or an ''adaptive fiction'' reading (immutability as mythological cover that actually permits invisible adaptation)?',
    'Examination of how different historiographic traditions (ancient religious sources vs. modern structural analysis) produce different framings; identification of what evidence each reading privileges or excludes; assessment of whether the readings are compatible within a single framework or genuinely foreclosing',
    'If compatible: all three readings are simultaneously true from different observer positions — the constraint is a genuinely multivalent kernel. If foreclosing: the demographic-trap reading is incompatible with one or both siblings, indicating a deeper disagreement about Spartan institutional reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_constitutional_framings, conceptual, 'Whether sibling readings are compatible or foreclosing relative to this demographic-trap reading').

omega_variable(
    mountain_misclassification_risk,
    'Is the analytical perspective''s mountain classification a genuine natural law recognition, or is it a false summit that naturalizes a contingent institutional choice benefiting the ephorate?',
    'Examination of foundational assumptions in legalist political philosophy: does immutability follow logically from the premise that properly-ordered laws should not be revised, or is this premise itself a contingent choice? Historical comparison with other poleis that achieved constitutional stability without immutability clauses.',
    'If genuine natural law: the mountain classification is correct, and the constraint''s demographic consequences are unfortunate but inevitable outcomes of sound legalist theory. If false summit: the immutability is revealed as an extractive choice reified as law, and the snare classification is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mountain_misclassification_risk, conceptual, 'Whether legalist immutability is natural law or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycurg_demo_theater_t0, lycurgan_laws__demographic_trap_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lycurg_demo_theater_t150, lycurgan_laws__demographic_trap_reading, theater_ratio, 150, 0.52).
narrative_ontology:measurement(lycurg_demo_theater_t300, lycurgan_laws__demographic_trap_reading, theater_ratio, 300, 0.65).

% Extraction over time
narrative_ontology:measurement(lycurg_demo_extract_t0, lycurgan_laws__demographic_trap_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lycurg_demo_extract_t150, lycurgan_laws__demographic_trap_reading, base_extractiveness, 150, 0.48).
narrative_ontology:measurement(lycurg_demo_extract_t300, lycurgan_laws__demographic_trap_reading, base_extractiveness, 300, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lycurg_demo_suppression_t0, lycurgan_laws__demographic_trap_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(lycurg_demo_suppression_t150, lycurgan_laws__demographic_trap_reading, suppression_requirement, 150, 0.68).
narrative_ontology:measurement(lycurg_demo_suppression_t300, lycurgan_laws__demographic_trap_reading, suppression_requirement, 300, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__demographic_trap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__adaptive_fiction_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, spartan_helot_integration_barrier).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, classical_legalist_immutability_doctrine).

% DUAL FORMULATION NOTE:
% The Lycurgan constraint family decomposes into three readings of the same kernel (immutable laws of Lycurgus) and two upstream constraints (helot integration barrier, legalist immutability doctrine). The demographic-trap reading assumes that demographic decline was real and materially caused by institutional structure; the sacral-fidelity reading prioritizes interpretive fidelity over material outcome; the adaptive-fiction reading suggests institutional plasticity despite immutable text. These readings have different ε values if measured by different observables (institutional rigidity vs. demographic outcome vs. reinterpretive capacity), justifying separate stories. All are linked via network.affects_constraints to indicate they are readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__demographic_trap_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
