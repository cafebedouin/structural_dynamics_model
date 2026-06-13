% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__syncretic_fusion_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shinbutsu_coexistence_commitment__syncretic_fusion_reading
 *   human_readable: Shinbutsu Coexistence: Syncretic Fusion Reading (Honji Suijaku)
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   The syncretic fusion reading of shinbutsu-shugo (Buddhist-Shinto
 *   coexistence) claims that kami and Buddhist deities are ontologically
 *   unified through honji suijaku: kami are manifestations of Buddhas and
 *   Bodhisattvas, appearing in local form to guide beings within Buddhist
 *   cosmic order. This reading was articulated by Tendai and Shingon
 *   theologians from the 9th-12th centuries onward and became the dominant
 *   institutional doctrine for over 1,000 years. It provided a single
 *   coherent theological framework that resolved the apparent tension between
 *   native kami veneration and imported Buddhist cosmology. However, this
 *   unity required constant interpretive work, institutional enforcement, and
 *   suppression of alternative readings—particularly the domain-partition
 *   reading (kami and Buddhas govern separate realms without ontological
 *   connection) and the incoherent-bundle reading (the whole system was
 *   sustained by deliberate ambiguity, not genuine coherence). The syncretic
 *   fusion reading is NOT the incoherent bundle; it is a coherent theological
 *   claim that the bundle was organized by a real unifying principle. This
 *   story instantiates that reading as a structured constraint and examines
 *   the extraction and suppression costs of maintaining it.
 *
 * KEY AGENTS:
 *   - Buddhist institutional authority: sets the interpretive framework, authenticates kami as manifestations, collects authority and resources
 *   - Jinguji priest class: maintains fusion through dual-practice mediation, benefits from theological legitimacy, pays through identity-lock to the reading
 *   - Folk kami practitioners: experience fusion as subordination of their practices to Buddhist approval, trapped with no exit
 *   - Doctrinal purists (both traditions): objections absorbed as incomplete understanding, suppressed rather than engaged
 *   - Shinto traditionalist movements: emerging counter-reading arguing kami are autonomous and degraded by overlay
 *   - Theological elite interpreters: maintain doctrinal authority by certifying coherence, increasingly elaborate scaffolding as contradictions surface
 *   - Meiji state modernizers: excluded from this constraint's operation, but will later dismantle it entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.58).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.72).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Shinbutsu Coexistence: Syncretic Fusion Reading (Honji Suijaku)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious/philosophical/institutional").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '91c653a4-7288-45a6-8826-401f260e15ae').
narrative_ontology:cs_kernel_codification('91c653a4-7288-45a6-8826-401f260e15ae', formalized).
narrative_ontology:cs_authority_grounding('91c653a4-7288-45a6-8826-401f260e15ae', lineage).
narrative_ontology:cs_interpretation_layer_present('91c653a4-7288-45a6-8826-401f260e15ae').
narrative_ontology:cs_reading_relation('91c653a4-7288-45a6-8826-401f260e15ae', shinbutsu_coexistence_commitment__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('91c653a4-7288-45a6-8826-401f260e15ae', shinbutsu_coexistence_commitment__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('91c653a4-7288-45a6-8826-401f260e15ae', foundational, kami_manifestation_of_buddhist_truth).
narrative_ontology:cs_axiom_status(kami_manifestation_of_buddhist_truth, holdable).
narrative_ontology:cs_axiom_grounding('91c653a4-7288-45a6-8826-401f260e15ae', kami_manifestation_of_buddhist_truth, theological).
narrative_ontology:cs_axiom('91c653a4-7288-45a6-8826-401f260e15ae', foundational, single_unified_soteriological_order).
narrative_ontology:cs_axiom_status(single_unified_soteriological_order, holdable).
narrative_ontology:cs_axiom_grounding('91c653a4-7288-45a6-8826-401f260e15ae', single_unified_soteriological_order, theological).
narrative_ontology:cs_reference_frame('91c653a4-7288-45a6-8826-401f260e15ae', early_heian_confusion_state).
narrative_ontology:cs_drift_state('91c653a4-7288-45a6-8826-401f260e15ae', late_edo_meiji_transition, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('91c653a4-7288-45a6-8826-401f260e15ae', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_institutional_authority).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_priest_class).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, folk_kami_practitioners).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, doctrinal_purists_both_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_priest_class).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, doctrinal_purists_buddhist).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinto_traditionalist_movements).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, universal_buddha_nature).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_as_manifestation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhist establishment (major temples, doctrinal schools) sets the interpretive framework: kami are manifestations of Buddhas and Bodhisattvas, subordinate to Buddhist cosmology. They integrate kami worship into Buddhist liturgy and theology, claiming this resolves apparent contradiction and elevates folk practice into coherent doctrine. They collect institutional prestige, devotional resources, and doctrinal authority from unified framework.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_institutional_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Shrine-priests who operate jinguji (Buddhist temple-shrine combined institutions) mediate between kami and Buddhist deities daily. They benefit from the fusion reading by claiming theological legitimacy—they are not mixing incompatible systems but serving a unified ontology. Their identity and professional survival depend on this reading holding; exit would mean doctrinal dishonor and institutional collapse. They pay by bearing the constant cognitive burden of maintaining coherence under tension.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_priest_class, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_priest_class, payer).

% Local communities performing kami veneration in agricultural and life contexts. They experience the fusion reading as constraint: their kami practices are reframed as manifestations of distant Buddhas, requiring Buddhist priestly mediation and doctrinal approval. They must adopt Buddhist interpretive frameworks they did not author to legitimate their own inherited practices. Exit means losing access to institutional validation and community cohesion.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, folk_kami_practitioners, payer,
    powerless, immediate, trapped, local).

% Buddhist scholars and reform movements arguing kami integration violates Buddhist purity, introduces syncretism, and corrupts doctrine. They pay by having their objections absorbed into the fusion framework as 'incomplete understanding' rather than substantive critique. Their alternatives are schism (costly) or silence (suppression of principled dissent).
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, doctrinal_purists_buddhist, payer,
    moderate, biographical, constrained, national).

% Emerging Shinto nationalist and 'restoration' movements (particularly post-18th century) arguing kami are autonomous native deities corrupted by Buddhist overlay. They experience the fusion reading as a false universalism that erases kami's authentic particularity. They bear the cost of heterodoxy and institutional marginalization until political conditions shift.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinto_traditionalist_movements, payer,
    moderate, biographical, constrained, national).

% Doctrinal authorities (Buddhist philosophers, Tendai and Shingon theologians) who articulate and defend honji suijaku interpretations. They maintain interpretive authority by certifying which understandings are 'coherent' and which are 'confused.' They benefit from this gatekeeping; they pay by being bound to defend increasingly elaborate doctrinal scaffolding as contradictions surface.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theological_elite_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Would eventually dismantle the constraint entirely by decree (Meiji Restoration, 1868+), enforcing sharp separation and elevating Shinto as state religion. Not present in the Edo/early-modern period when this constraint operated in stable form; their later intervention is what reveals the constraint's fragility and political nature.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, meiji_state_modernizers, excluded,
    powerful, biographical, mobile, national).

% Historian or anthropologist studying the constraint from outside: observing how coherence was maintained, what cognitive and institutional work it required, where tensions were suppressed, and how it eventually fractured.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_institutional_authority).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves an apparent theological contradiction: Japan has both native kami and imported Buddhist cosmology. Honji suijaku frames them as hierarchically unified—kami are local manifestations of universal Buddhist truth (Buddhas and Bodhisattvas appearing in kami form to guide beings). This provides a single coherent ontological framework instead of two separate systems or permanent doctrinal conflict.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional resources upward to the Buddhist establishment and theological elite. Local kami practices are subordinated to Buddhist doctrinal approval; folk practitioners gain coherence but lose autonomy; Buddhist institutions gain expanded jurisdiction and devotional resources. The transfer is primarily symbolic (authority over meaning) but carries material weight in temple revenue, political influence, and doctrinal gatekeeping.
% ABSENT_VOICES: Shinto traditionalists (who would argue kami are autonomous and degraded by Buddhist overlay), doctrinal purists in both traditions (who would argue syncretism is incoherent), and folk practitioners without literate theological training (whose kami-practices are reframed without their consent). The framework's coherence depends on excluding or absorbing their objections as 'incomplete understanding.'
% DISAPPEARANCE_RATIONALE: If the fusion reading and its enforcement vanished overnight, Japan would revert to either separate kami and Buddhist systems (Shinto restoration reading would resurface) or openly incoherent bundles (folk practices operating without theological legitimation). The Meiji Restoration demonstrates this empirically—when the state imposed sharp separation (1868+), both systems reconstituted themselves, and the century-long fusion apparatus collapsed within a generation.
% FOUNDING_PROBLEM: Japan absorbed Buddhism (6th century+) while maintaining indigenous kami veneration. Two centuries of coexistence produced pragmatic fusion at the institutional level, but no coherent single ontology was articulated. By the Heian period, theological confusion created doctrinal instability and institutional competition. Honji suijaku (developed fully by Tendai and Shingon schools, 9th-12th centuries) was the doctrine that resolved this: kami are manifestations of Buddhas, unifying the systems under Buddhist cosmology.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist establishment attests the founding problem was real (doctrinal confusion, institutional friction) and solved by honji suijaku. Shinto traditionalists and modern scholars attest the problem was real but the solution was a cover story—what actually happened was Buddhist institutional dominance suppressed kami autonomy under a theological mask. Contemporary historians outside both traditions document that syncretism persisted as pragmatic practice for 1,000+ years despite doctrinal inconsistency; no empirical evidence shows honji suijaku actually eliminated the confusion, only that it provided an authoritative narrative that suppressed public acknowledgment of it.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at interval end) is moderate-to-high because the fusion reading transfers interpretive authority upward to Buddhist institutions and theological elites, subordinating folk practice and alternative doctrinal readings. The transfer is primarily symbolic (authority over meaning of kami-practice) but carries material weight in temple revenue, priestly status, and control over ritual legitimacy. Suppression (0.72) is high because maintaining the fusion reading requires constant intellectual work—resolving contradictions, absorbing objections, reframing folk practices in theological language—and institutional enforcement against alternative readings (particularly Shinto restoration movements post-18th century). Theater ratio (0.48) indicates that roughly half the observable enforcement activity defends doctrinal coherence while half performs it: doctrinal work is genuine (theologians are engaged in real intellectual problems), but much of the institutional apparatus exists to maintain the appearance of unity and prevent public acknowledgment that the system is fragile. The measurement trajectory shows a rise in extractiveness and suppression from early Edo (t0, when the fusion reading was stable and unquestioned) to late Edo (t30, when Shinto restoration challenges and doctrinal purist objections mounted pressure). Theater ratio plateaus late (t25-30) as maintenance effort stabilizes despite growing pressure—the constraint is not strengthening, but neither is it relaxing; it is holding through sustained institutional performance. The final dip in extractiveness (t25-30) reflects the Shinto restoration movements gaining organizational capacity, which begins to displace the fusion reading's dominance before Meiji dissolution.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats will compute very different types from the same constraint story. From the Buddhist institutional seat, this is genuine tangled rope: real coordination (unified ontology solving practical theological problems) plus asymmetric extraction (authority concentrated in them, costs distributed to folk practitioners and doctrinal purists). From the folk practitioner seat, the same structure appears as a snare: the coordination story (unified ontology) is experienced as cover story, and the primary function is extraction (reframing practices, gatekeeping legitimacy, subordinating alternatives). From the Shinto traditionalist seat, it appears as a false summit—presented as natural/necessary theological unification, but actually a constructed power grab maintaining Buddhist dominance. The engine computes per-seat types from these structural positions; the claimed type (tangled rope) reflects the beneficiary/elite position.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutional authority sits at the beneficiary end: they set the framework, collect authority and resources, and face no real exit costs (their institutional survival is strengthened by the fusion reading). Jinguji priests are dual-positioned: they benefit from legitimacy and moderate power, but they are deeply identity-locked to the fusion reading—their professional identity is constituted through it, their institutions (jinguji) exist precisely to mediate it, and exit would mean doctrinal dishonor and unemployment. Their identity-lock moves their directionality toward the payer end despite beneficiary role, because the cost of maintaining the lock (cognitive burden, suppression of doubt, vulnerability to alternative readings) is substantial. Folk practitioners are pure targets: they experience the constraint as subordination of their practices, they are powerless relative to institutional authority, they are trapped with no exit (their kami practices require institutional legitimacy to be socially acceptable), and they pay through reframing of their inherited traditions without consent. Doctrinal purists pay by having principled objections absorbed rather than engaged. Shinto traditionalists pay by marginalization until political conditions shift. The directionality override is unnecessary here; the structural derivation (beneficiary/victim + power + exit) produces accurate d values for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The syncretic fusion reading exhibits some properties of mandatrophy (a founding problem now dead, constraint persisting through inertia). The founding problem was real—doctrinal confusion between kami and Buddhist systems, institutional friction, lack of coherent framework. However, by the late Edo period (t20+), scholarly evidence suggests the founding problem had substantially shifted: honji suijaku did not actually eliminate confusion; it provided a narrative that suppressed public acknowledgment of it. Folk practice remained pragmatic and incoherent; Shinto traditionalists argued kami were autonomous; doctrinal purists in Buddhism argued syncretism corrupted doctrine. The constraint persisted not because it solved the founding problem but because Buddhist institutional interests in maintaining it (temple revenue, priestly authority, cosmological dominance) were powerful enough to suppress alternatives. However, mandatrophy_resolved should be authored as FALSE here: the fusion reading DID have a real founding problem and a real coordination function. The constraint is not a zombie; it is a tangled rope. The twist is that it is a tangled rope whose mandate legitimacy is contested—some parties (Buddhist elite) still believe the founding problem is live and the solution is working; other parties (Shinto traditionalists, doctrinal purists) believe the mandate is dead and the constraint now persists as pure extraction. This contestation is captured in the founding_problem_status: contested. The mandatrophy dynamics are best tracked through the theater_ratio trajectory and the omega variable about coordination vs. extraction boundaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coherence_vs_suppression_boundary,
    'Is the fusion reading a genuinely coherent theological framework that unified Japanese religion, or is it an elaborate interpretive apparatus that suppresses underlying incoherence through institutional authority?',
    'Textual analysis of folk practice vs. theological doctrine across centuries: if folk practice aligns with honji suijaku teachings, the reading achieved genuine coherence; if folk practice remained pragmatically incoherent (kami as autonomous, separate prayers to Buddhas, inconsistent cosmologies), the reading provided narrative coherence without actual unification. Historical records of doctrinal debates and objections from purists in both traditions provide evidence for the suppression hypothesis.',
    'If coherence is genuine, the constraint is a successful tangled rope—real coordination with extractive consequences. If suppression is primary, the constraint is closer to a false summit or snare—the coherence claim is cover story. The engine classification depends on whether accessibility_collapse and resistance are authored accurately (high accessibility of alternatives = weak coherence; high resistance from purists = suppression at work).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_vs_suppression_boundary, empirical, 'Whether honji suijaku achieved genuine unified ontology or masked persistent incoherence.').

omega_variable(
    institutional_identity_lock_mechanism,
    'Is the jinguji priest class genuinely identity-locked to the fusion reading through professional constitution, or could they exit with costs that are primarily economic rather than identity-constituting?',
    'Historical cases of priests who abandoned the fusion reading or converted to pure Shinto: did they experience this as violation of core identity (confessional boundary) or as occupational transition? Do monastic records, letters, or memoirs indicate the lock is internalized (self-concept dissolved by exit) or structural (institutional elimination)? Post-Meiji cases where jinguji institutions dissolved—did individual priests'' identities dissolve with them, or did they reconstitute in new roles?',
    'If identity-lock is internalized, suppression is higher than the structural metrics suggest (the priest carries the suppression after exit). If lock is structural, suppression reflects institutional barriers rather than constitutive identity fusion. This affects whether the constraint''s suppression should be classified as partly internalized vs. purely structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_identity_lock_mechanism, empirical, 'Mechanism of jinguji priest identity-lock to the fusion reading.').

omega_variable(
    founding_problem_attenuation_rate,
    'When and at what rate did the founding problem (doctrinal confusion, institutional friction) become a non-problem that the fusion reading addressed, vs. persisting as unresolved tension?',
    'Timeline of doctrinal development: early Heian (6th-8th century) when fusion was pragmatic and confused; development of systematic honji suijaku doctrine (9th-12th century) when coherence was articulated; post-12th century institutional embedding when doctrine became orthodoxy; late Edo (17th-19th century) when Shinto traditionalism and doctrinal purists mounted organized challenges. If challenges accelerated after t15, the founding problem''s status shifted before Meiji.',
    'If the founding problem genuinely attenuated (disappeared) by t10-15, the constraint''s mandate was live and solid through that period. If attenuation was delayed (problem persisted through suppression), mandate legitimacy is contested earlier than t30. This shapes the mandatrophy calculation and the reading''s endpoint stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_attenuation_rate, empirical, 'Temporal attenuation of the founding problem.').

omega_variable(
    sibling_reading_foreclosure_status,
    'Does the syncretic fusion reading logically foreclose the domain partition reading (kami and Buddhas are definitely NOT in separate domains if they are ontologically unified), or do they coexist as two different conceptual frameworks that could both be held by different parties?',
    'Doctrinal analysis: does honji suijaku explicitly claim kami and Buddhas CANNOT be in separate domains, or does it merely claim they CAN be understood as unified manifestations? If the latter, a party could hold honji suijaku AND domain partition as compatible frameworks. Textual evidence from Tendai and Shingon scholars: do they argue the partition reading is incoherent or merely incomplete?',
    'If foreclosure is real, the two readings are in logical contradiction—only one can be true in any single framework. If coexistence is possible, the readings remain live alternatives that different parties hold simultaneously. This shapes the reading_relations classification (forecloses vs. coexists_with).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_status, conceptual, 'Whether syncretic fusion reading logically forecloses or coexists with domain partition reading.').

omega_variable(
    meiji_dissolution_causation,
    'Was the Meiji Restoration''s imposition of sharp kami-Buddhist separation (1868+) a logical deconstruction of the fusion reading''s internal contradictions, or an external political enforcement that suppressed a still-viable theological framework?',
    'Comparative evidence: did the fusion reading collapse because its coherence was illusory and Meiji pressure revealed it, or because Meiji state power was sufficient to overturn a still-coherent but politically vulnerable doctrine? Post-Meiji analysis: in the century after separation, have Japanese theologians and practitioners been able to reconstitute the fusion reading, or does its collapse demonstrate it was always incoherent?',
    'If the fusion reading''s collapse reveals internal weakness, the constraint was always a false summit or elaborate snare. If collapse reveals political vulnerability of a still-coherent position, the constraint was a successful tangled rope that political change unmade. This affects how the computed type relates to the claimed type and shapes long-term stability analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meiji_dissolution_causation, empirical, 'Causation of the fusion reading''s collapse under Meiji pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(shin_tr_t0, projected).
narrative_ontology:measurement(shin_tr_t5, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement_basis(shin_tr_t5, projected).
narrative_ontology:measurement(shin_tr_t10, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement_basis(shin_tr_t10, observed).
narrative_ontology:measurement(shin_tr_t15, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement_basis(shin_tr_t15, observed).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(shin_tr_t20, observed).
narrative_ontology:measurement(shin_tr_t25, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 25, 0.49).
narrative_ontology:measurement_basis(shin_tr_t25, observed).
narrative_ontology:measurement(shin_tr_t30, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(shin_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(shin_be_t0, projected).
narrative_ontology:measurement(shin_be_t5, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(shin_be_t5, projected).
narrative_ontology:measurement(shin_be_t10, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(shin_be_t10, observed).
narrative_ontology:measurement(shin_be_t15, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(shin_be_t15, observed).
narrative_ontology:measurement(shin_be_t20, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement_basis(shin_be_t20, observed).
narrative_ontology:measurement(shin_be_t25, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(shin_be_t25, observed).
narrative_ontology:measurement(shin_be_t30, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(shin_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(shin_su_t0, projected).
narrative_ontology:measurement(shin_su_t5, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(shin_su_t5, projected).
narrative_ontology:measurement(shin_su_t10, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(shin_su_t10, observed).
narrative_ontology:measurement(shin_su_t15, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(shin_su_t15, observed).
narrative_ontology:measurement(shin_su_t20, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(shin_su_t20, observed).
narrative_ontology:measurement(shin_su_t25, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 25, 0.73).
narrative_ontology:measurement_basis(shin_su_t25, observed).
narrative_ontology:measurement(shin_su_t30, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(shin_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% The shinbutsu-shugo kernel is contested across three readings, each instantiating a different constraint: (1) syncretic_fusion_reading (this file) claims kami are ontologically unified with Buddhas through honji suijaku—single coherent system, high doctrinal constraint, elite interpretive authority. (2) domain_partition_reading claims kami and Buddhas govern separate domains without ontological unification—pragmatic coexistence, lower doctrinal constraint, multiple interpretive authorities. (3) incoherent_bundle_reading claims the entire system was maintained through deliberate ambiguity and institutional power without genuine coherence—no stable mandate, suppression is primary. The three stories are linked by network.affects_constraints; each one instantiates a different constraint from the same kernel, with different epsilon values, beneficiary/victim structures, and classifications. The fusion reading influences both siblings by establishing the interpretive authority that either gets accepted (domain partition accepts the same institutional structure but rejects unified ontology) or revealed as illusory (incoherent bundle argues unity was never real).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
