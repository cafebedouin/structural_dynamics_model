% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__syncretic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__syncretic_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Honji-Suijaku Syncretic Ontology: Unified Cosmological Order
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   Under honji-suijaku (original essence, manifestation) metaphysics, kami
 *   are understood as manifestations or aspects of buddhas, and Buddhist
 *   deities as the essential buddhas behind Shinto kami. This reading frames
 *   a unified cosmological order in which Shinto and Buddhism are integrated
 *   into a single ontological hierarchy. The syncretic reading was
 *   institutionally dominant from roughly the 9th century onward,
 *   particularly in Tendai and Shingon Buddhism. It enabled the absorption of
 *   Shinto shrines into Buddhist temple complexes (called shinbutsu-shugo,
 *   kami-buddha merger), subordinated Shinto autonomy to Buddhist
 *   institutional control, and generated doctrine to justify that
 *   subordination. The constraint's structural claim: this metaphysical
 *   commitment extracts institutional authority from Shinto practitioners by
 *   reframing their kami-worship as participation in a Buddhist-hierarchical
 *   cosmos. Shinto shrine priests and autonomous kami-worship communities are
 *   the identified targets; Buddhist institutional hierarchy and syncretist
 *   intellectual elites are the beneficiaries. The theater_ratio rises over
 *   the interval (0.18 → 0.42) as the need for active restatement of the
 *   metaphysical claim increases—a sign that the genuine coordination problem
 *   (relating two spirit-worlds) is increasingly performed rather than
 *   functionally solved.
 *
 * KEY AGENTS:
 *   - Buddhist institutional hierarchy (Tendai, Shingon, later Nichiren): beneficiary, institutional power, establishes honji-suijaku doctrine and incorporates shrines into temple complexes.
 *   - Syncretist intellectual elite (Buddhist scholars, monastery-based theologians): beneficiary, institutional power, authors and defends the metaphysical integration.
 *   - Independent Shinto shrine communities and priests: victim, moderate-to-powerful (individually), constrained exit; lose autonomy and doctrinal authority as shrines are incorporated into Buddhist temple hierarchies.
 *   - Shinto practitioners (lay worshippers, families maintaining household kami): victim, powerless-to-moderate, identity-locked; come to understand their kami-worship as participation in Buddhist cosmology rather than autonomous practice.
 *   - Meiji state modernizers: agenda-setter from c. 1868 onward, institutional power; forcibly separate shrines and temples, overturn the syncretic reading, and reassert Shinto as an autonomous national religion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.68).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.71).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-Suijaku Syncretic Ontology: Unified Cosmological Order").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious/philosophical/institutional").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, '05c41ea5-1307-476b-941e-88f25122da08').
narrative_ontology:cs_kernel_codification('05c41ea5-1307-476b-941e-88f25122da08', formalized).
narrative_ontology:cs_authority_grounding('05c41ea5-1307-476b-941e-88f25122da08', lineage).
narrative_ontology:cs_interpretation_layer_present('05c41ea5-1307-476b-941e-88f25122da08').
narrative_ontology:cs_reading_relation('05c41ea5-1307-476b-941e-88f25122da08', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('05c41ea5-1307-476b-941e-88f25122da08', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('05c41ea5-1307-476b-941e-88f25122da08', foundational, kami_buddha_ontological_unity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_unity, holdable).
narrative_ontology:cs_axiom_grounding('05c41ea5-1307-476b-941e-88f25122da08', kami_buddha_ontological_unity, conventional).
narrative_ontology:cs_axiom('05c41ea5-1307-476b-941e-88f25122da08', foundational, buddha_essential_hierarchy).
narrative_ontology:cs_axiom_status(buddha_essential_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('05c41ea5-1307-476b-941e-88f25122da08', buddha_essential_hierarchy, deontological).
narrative_ontology:cs_reference_frame('05c41ea5-1307-476b-941e-88f25122da08', unified_buddhist_cosmos).
narrative_ontology:cs_drift_state('05c41ea5-1307-476b-941e-88f25122da08', meiji_separation_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('05c41ea5-1307-476b-941e-88f25122da08', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, syncretist_intellectual_elite).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, independent_shinto_practice).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, shinto_shrine_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, shinto_practitioners_lay).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, independent_shinto_shrine_autonomy).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, shinto_practitioners_lay).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tendai, Shingon, and other Buddhist sects set the doctrinal terms of honji-suijaku, incorporate shrines into temple complexes, control shrine priest licensing, and define how kami are understood within Buddhist cosmology. They benefit from expanded institutional authority, claim to shrine revenues, and the prestige of providing a unified metaphysical framework. Their exit options are high: they can abandon the syncretic claim and compete on other grounds.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Buddhist scholars and monastery-based theologians author and defend honji-suijaku doctrine, gain intellectual prestige and institutional authority through their expertise in the metaphysical integration, and establish themselves as the canonical interpreters of both Shinto and Buddhism. They benefit from the authority conferred by being the doctrinal monopoly.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, syncretist_intellectual_elite, beneficiary,
    institutional, generational, mobile, national).

% Independent shrines lose the right to define their own kami, are physically incorporated into Buddhist temple complexes, subordinate their priest hierarchy to Buddhist authority, and are stripped of doctrinal autonomy. Even powerful shrines cannot exit: to refuse incorporation is to be labeled heretical or incoherent. The trapped exit reflects institutional lock-in: once incorporated, un-incorporation requires overturning the entire syncretic framework.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, independent_shinto_shrine_autonomy, payer,
    powerful, civilizational, trapped, national).

% Lay worshippers and families maintain household kami-worship, but increasingly under the intellectual framework that their kami are manifestations of buddhas. They experience the coordination benefit: a unified cosmological order relating their kami to the broader Buddhist cosmos. They also experience the extraction: their autonomy to define kami-worship on Shinto terms alone is suppressed, replaced by Buddhist interpretation. Their identity as Shinto practitioners becomes fused with Buddhist subordination; exit would mean abandoning not just a practice but a self-conception.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shinto_practitioners_lay, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, shinto_practitioners_lay, beneficiary).

% From c. 1850 onward, Japanese state modernizers increasingly see syncretic Buddhism-Shinto as an obstacle to national identity. They are excluded from the contemporary institutional conversation: the syncretic framework is self-policing and does not engage modernizers' objections. They become agenda-setters only after 1868, when they forcibly separate shrines and temples, overturn the syncretic reading, and assert Shinto as an autonomous national religion. Their eventual power to enforce the partition reading demonstrates that the syncretic reading, despite centuries of institutional dominance, was not inevitable or natural law.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, meiji_state_reformers, excluded,
    institutional, generational, analytical, national).

% Modern historians and religious studies scholars examine the syncretic constraint from outside the tradition: they analyze whether honji-suijaku represented genuine ontological integration or institutionally-imposed coherence, measure the extraction from Shinto autonomy, and trace how the constraint's persistence depended on continuous institutional enforcement.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, scholarly_observers_contemporary, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__syncretic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Honji-suijaku provides a unified cosmological framework relating Shinto kami and Buddhist deities: rather than two incoherent spirit-systems, kami become manifestations of buddhas, unifying the cosmos into a single Buddhist-hierarchical order. This solves the coordination problem of relating two religions into one coherent worldview, enables shrine-temple cooperation, and provides elite theological sophistication.
% TRANSFER_FUNCTION: Transfers institutional authority, doctrinal autonomy, and shrine independence from Shinto communities to Buddhist hierarchy. Buddhist institutions claim the right to define kami-nature, incorporate shrines into temple complexes, extract resources from subordinated shrine communities, and position themselves as the canonical interpreters of both Shinto and Buddhism.
% ABSENT_VOICES: Independent Shinto lineages and anti-syncretist shrine communities are excluded from the institutional conversation: their objection that kami are autonomous spirits, not buddha-manifestations, is treated as incoherent or heretical within the syncretic framework. Lay practitioners who might object to Buddhist reinterpretation of their household kami are socialized into the subordinated reading before they can formulate objections. The Meiji reformers, from 1850 onward, are excluded from dialogue and eventually overthrow the constraint through state power.
% DISAPPEARANCE_RATIONALE: If the syncretic reading vanished—if honji-suijaku were abandoned and kami and buddhas reasserted as separate or incoherent—shrine autonomy would immediately reassert itself, shrine-temple arrangements would reorganize around Shinto independence (as they did in 1868), and the Japanese religious landscape would rearrange from a Buddhist-hierarchical cosmos to a plural landscape. The Meiji separation demonstrates this: the constraint's removal did not leave a void but enabled reorganization.
% FOUNDING_PROBLEM: Pre-syncretic Japan (pre-Heian) contained two unrelated spirit-systems: indigenous Shinto kami and imported Buddhist deities. There was no coherent framework relating them. The coordination problem was how to relate two independent religious traditions into one cosmos—a genuine challenge to elite theology and institutional cooperation.
% FOUNDING_PROBLEM_CORROBORATION: The Meiji historical record provides external corroboration: when forced separation occurred in 1868, Shinto did not collapse for lack of Buddhist integration. Shrines immediately reasserted autonomous kami-worship and rejected Buddhist reinterpretation. This demonstrates that the founding coordination problem was not actually unsolvable without syncretic integration—other solutions (partition, pluralism) were viable. Modern religious studies scholarship (outside the tradition) corroborates: historians note that honji-suijaku was adopted as institutional policy rather than emerging organically from theological necessity, suggesting the coordination problem was real but the syncretic solution was a choice among alternatives, not the only possibility.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__syncretic_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68 at interval end) reflects the substantial institutional authority transferred from Shinto to Buddhist hierarchies through the metaphysical framing—Buddhist institutions gain canonical authority over kami interpretation, shrine naming, ritual procedure. Suppression (0.71) is high because the constraint's persistence depends on continuous institutional enforcement: shrine incorporation into temple complexes, doctrinal assertion in texts, legal subordination of Shinto priest authority, internalization of the cosmological order in practitioners' belief. The measurement series document a clear trajectory: extractiveness and suppression both rise over the 600-year interval (extractiveness climbs 0.42 → 0.68; suppression 0.45 → 0.71), consistent with institutional consolidation. Theater_ratio rises similarly (0.18 → 0.42), indicating that by the late medieval period, much of the constraint's operation is performative assertion of the metaphysical claim rather than functional problem-solving. All three metrics share the same time grid (measurements at t=0, 200, 400, 600).
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist institutional seat, the constraint is genuine coordination: honji-suijaku solves the problem of relating Shinto and Buddhism into one cosmos, enables cooperation between temple and shrine, and generates sophisticated theology. From the Shinto shrine and practitioner seats, the same structure operates as doctrinal imperialism dressed as metaphysics. The engine computes this gap from the structural data: beneficiary seats report coordination; target seats report extraction. The divergence is not an error—it is the measurement the classification exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist institutional hierarchy is the beneficiary seat: it gains canonical authority, institutional power over shrine incorporation, doctrinal control over kami interpretation, and the ability to extract obedience and resources from subordinated shrine communities. Their directionality d is low (~0.1–0.2), they gain from the constraint's operation. Shinto shrine autonomy and independent practitioners are the target seats: they lose the right to define their own kami, lose shrine independence, experience subordination to Buddhist hierarchy, have constrained exit (incorporated shrines cannot easily un-incorporate; practitioners internalize the syncretic reading and come to believe autonomy would be heretical). Their directionality d is high (~0.8–0.9). The intellectual elite who articulate the syncretic reading benefit institutionally from the authority it confers, so they sit with the beneficiary seats. Lay practitioners occupy a dual position: they participate in a coordination function (a unified cosmological framework relating two spirit-worlds) which is a genuine benefit, but they also experience the suppression of Shinto autonomy and the internalization of Buddhist hierarchy. The engagement is real but extracted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of honji-suijaku was genuine: pre-syncretic Japan contained two unrelated spirit-systems (Shinto kami and Buddhist deities), and there was a real coordination problem in relating them. The syncretic reading solved it at the level of elite theology. However, by the late medieval period (measurements at t=400, t=600), the metaphysical claim persists while the coordination problem it solved has ossified into institutional hierarchy. Shrine incorporation is no longer about solving a cosmological relation problem—it is about maintaining Buddhist dominance. The theater_ratio rising to 0.42 suggests the constraint is increasingly maintained by performative assertion of the metaphysics rather than by its functional necessity. This is a mandatrophy candidate: the founding problem (unrelated spirit-systems) could have been solved by other means (separate domains, pluralistic theology); instead, the syncretic solution hardened into institutional extraction. The Meiji separation (1868) reveals the mandatrophy: when forcibly separated, Shinto did not collapse for lack of Buddhist integration—it immediately reasserted autonomy and different metaphysical claims. The coordination function was not actually necessary; the extraction function was.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    syncretic_vs_incoherence_ontology,
    'Did honji-suijaku metaphysics represent a genuine philosophical integration of kami and buddhas into a coherent cosmological order, or was it an institutionally-imposed layer of coherence over fundamentally incoherent practices?',
    'Textual analysis of contemporary doctrinal writings (Tendai, Shingon) vs. shrine-level practice records; examination of whether practitioners'' actual behavior integrated the ontological claim or ignored it pragmatically.',
    'If genuine integration: the constraint solves a real coordination problem (relating two independent spirit-worlds into one framework) and extraction is the cost of that solution. If institutional coherence over incoherence: extraction is pure institutional dominance dressed as metaphysics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretic_vs_incoherence_ontology, conceptual, 'Whether honji-suijaku represents genuine ontological integration or performative coherence.').

omega_variable(
    buddhist_hierarchy_beneficiary_intent,
    'Did the Buddhist institutional hierarchy deliberately author honji-suijaku metaphysics to absorb Shinto into Buddhist cosmology and extract institutional authority, or did the metaphysics emerge organically from doctrinal elaboration with subordination as an unintended structural consequence?',
    'Historical analysis of the temporal sequence: did Buddhist doctrinal claims precede or follow institutional adoption of shrines as buddha-manifestation temples? Did institutional consolidation follow from or precede the metaphysical claim?',
    'If deliberate: the constraint is a Snare with theological cover. If organic emergence: extraction may be a side effect of genuine doctrinal coherence-seeking, supporting Tangled Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(buddhist_hierarchy_beneficiary_intent, empirical, 'Agency in the emergence of the syncretic ontology.').

omega_variable(
    suppression_mechanism_locus,
    'Is the measured suppression (0.71) primarily structural — Shinto shrines physically incorporated into Buddhist temple complexes, dependent on Buddhist institutional support, legally barred from autonomous action — or internalized — Shinto practitioners came to believe their own kami were genuinely manifestations of buddhas and thus did not wish to resist?',
    'Post-separation historical record: after Meiji 1868 forcibly separated shrines from temples, did Shinto practice immediately reassert autonomy and different metaphysical claims, or did it take generations for the internalized commitment to fade?',
    'If structural suppression: the constraint''s extraction would persist after separation (institutional dependency was the mechanism). If internalized: suppression traveled with practitioners even after structural barriers lifted (the reading had fused Shinto identity with subordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_locus, empirical, 'Structural vs. internalized suppression of Shinto autonomy.').

omega_variable(
    syncretic_reading_foreclosure_scope,
    'Does the syncretic reading''s core premise (kami and buddhas are unified aspects of one cosmological order) logically foreclose the partition reading (separate domains without integration), or do the two readings represent genuinely independent positions that different authorities could hold simultaneously?',
    'Examine whether a framework could coherently hold both: kami and buddhas are unified cosmologically AND they operate in separate, non-integrated domains. If the unity claim and the partition claim directly contradict at the foundational level, the relation is forecloses; if they target different domains or levels of analysis, the relation is coexists_with.',
    'If forecloses: the engine computes foreclosure from contradictory axioms; if coexists_with: different Buddhist sects and Shinto lineages could genuinely hold different positions without one being logically incoherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretic_reading_foreclosure_scope, conceptual, 'Logical relationship between syncretic and partition readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 200, 0.28).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 400, 0.38).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 600, 0.42).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(shin_be_t400, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 400, 0.64).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 600, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(shin_su_t200, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 200, 0.58).
narrative_ontology:measurement(shin_su_t400, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 400, 0.68).
narrative_ontology:measurement(shin_su_t600, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 600, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__syncretic_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-constraint family decomposing the shinbutsu_ontological_commitment kernel. The syncretic_reading instantiates the honji-suijaku metaphysical claim and its institutional extraction. The partition_reading models an alternative framework in which Shinto and Buddhism occupy separate ontological domains without integration. The incoherence_reading models the possibility that shinbutsu-shugo was institutionally tolerated inconsistency with no coherent metaphysics. Each reading produces a different ε, different beneficiary/victim structure, and different classification. They are linked via network.affects_constraints to indicate kernel kinship and mutual influence: the dominance of the syncretic reading suppressed the visibility of the partition reading; the eventual adoption of the partition reading (in Meiji separation) demonstrates that the syncretic reading was not logically necessary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
