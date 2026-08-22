% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Ritual Encoding of Intergenerational Trauma as Threat-Detection System
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   In communities marked by historical catastrophe, ritual practice encodes
 *   and transmits trauma narratives, threat patterns, and survival strategies
 *   across generations. This constraint models that encoding as a
 *   threat-detection mechanism: the ritual's repetition, emotional intensity,
 *   and identity-binding properties keep dangerous historical knowledge alive
 *   when direct experience fades. This reading instantiates the
 *   TRAUMA_ENCODING interpretation of the contested catastrophe_memory_kernel
 *   — one of four viable framings of the same kernel (boundary_maintenance,
 *   survival_competence, symbol_continuity being the siblings). From this
 *   reading's seat, the ritual functions as an intergenerational warning
 *   system whose benefit is threat-vigilance and whose cost is the
 *   psychological burden imposed on descendants who carry inherited
 *   hypervigilance, intrusive memories by proxy, and identity fusion with the
 *   trauma narrative. The claim/metric gap is deliberate: this reading CLAIMS
 *   tangled_rope (genuine coordination of threat-detection, asymmetric
 *   extraction of descendants) while the metrics describe substantial
 *   extractiveness with moderate enforcement suppression — the engine
 *   computes seat-divergence; the claim and metrics stay independent authored
 *   facts.
 *
 * KEY AGENTS:
 *   - Ritual custodians: maintenance agenda-setters; identity-locked to ritual authority and continuity stewardship
 *   - Threat-vigilance collective: beneficiaries of early-warning capacity; descendants with heightened historical awareness and persecution sensitivity
 *   - Trauma-bearing descendants: primary payees; carry psychological burden, inherited hypervigilance, obligation to transmit trauma; identity-locked exit
 *   - Secular descendants: constrained payees; doubt the ritual's necessity but remain embedded by social bonds and cultural identity
 *   - Founding trauma survivors: authority holders over meaning; source of the encoding; powerful but trapped in original experience
 *   - External threat agents: excluded referents; what the ritual encodes warnings against
 *   - Mental health professionals: analytical observers; measure psychological cost and adaptive function divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.45).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Ritual Encoding of Intergenerational Trauma as Threat-Detection System").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, 'a3ecdd75-ed16-4f3e-9755-1dc4511e1e22').
narrative_ontology:cs_kernel_codification('a3ecdd75-ed16-4f3e-9755-1dc4511e1e22', distributed).
narrative_ontology:cs_authority_grounding('a3ecdd75-ed16-4f3e-9755-1dc4511e1e22', lineage).
narrative_ontology:cs_interpretation_layer_present('a3ecdd75-ed16-4f3e-9755-1dc4511e1e22').
narrative_ontology:cs_reading_relation('a3ecdd75-ed16-4f3e-9755-1dc4511e1e22', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3ecdd75-ed16-4f3e-9755-1dc4511e1e22', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3ecdd75-ed16-4f3e-9755-1dc4511e1e22', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('a3ecdd75-ed16-4f3e-9755-1dc4511e1e22', foundational, trauma_encoding_necessity).
narrative_ontology:cs_axiom_status(trauma_encoding_necessity, holdable).
narrative_ontology:cs_axiom_grounding('a3ecdd75-ed16-4f3e-9755-1dc4511e1e22', trauma_encoding_necessity, empirically_contingent).
narrative_ontology:cs_axiom('a3ecdd75-ed16-4f3e-9755-1dc4511e1e22', secondary, intergenerational_extraction_justified).
narrative_ontology:cs_axiom_status(intergenerational_extraction_justified, holdable).
narrative_ontology:cs_axiom_grounding('a3ecdd75-ed16-4f3e-9755-1dc4511e1e22', intergenerational_extraction_justified, deontological).
narrative_ontology:cs_reference_frame('a3ecdd75-ed16-4f3e-9755-1dc4511e1e22', trauma_must_stay_embodied_in_community_practice).
narrative_ontology:cs_drift_state('a3ecdd75-ed16-4f3e-9755-1dc4511e1e22', contemporary_institutional_threat_monitoring, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a3ecdd75-ed16-4f3e-9755-1dc4511e1e22', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, threat_vigilance_collective).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, trauma_bearing_descendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, secular_descendants).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, secular_descendants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious and community leaders who maintain the ritual's structure, timing, and transmission protocols. They interpret the founding trauma, decide which elements are preserved and which are contextualized, and enforce participation. Their professional and spiritual identity is constituted through ritual authority and continuity stewardship.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, ritual_custodians, agenda_setter,
    organized, generational, identity_locked, regional).

% The community that benefits from the encoded threat-detection capacity: heightened historical awareness, rehearsed response patterns to persecution signs, collective memory of survival strategies, and early-warning sensitivity to group-targeting events. The ritual maintains readiness across generational transitions when direct experience fades.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, threat_vigilance_collective, beneficiary,
    organized, generational, mobile, regional).

% Generations born after the founding catastrophe who carry the ritual's psychological burden: intrusive memories by proxy, inherited hypervigilance, the emotional weight of reenactment, and the obligation to transmit trauma narratives to their own children. Their participation is often framed as duty rather than choice, and exit carries identity dissolution.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, trauma_bearing_descendants, payer,
    moderate, biographical, identity_locked, regional).

% Community members who question the ritual's necessity or psychological cost but remain embedded in the group. They pay the burden (emotional weight, time, identity fusion) while doubting whether the threat-vigilance justifies the extraction. Their exit is constrained by social bonds and cultural identity even when conviction wanes.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, secular_descendants, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__trauma_encoding_reading, secular_descendants, beneficiary).

% The generation that directly experienced the catastrophe and encoded the warning into ritual. They hold authority over the constraint's meaning but are also the source of the trauma encoding itself. Their interpretation shapes whether descendants read the ritual as essential vigilance or as obligatory suffering.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, founding_trauma_survivors, observer,
    powerful, biographical, trapped, regional).

% Historical and potential future threat sources (persecutors, aggressors, competing groups) that the ritual encodes warnings against. They are not stakeholders in the constraint's maintenance but are the referent of the threat-narrative the ritual preserves. If they could speak to the constraint, they would note that the ritual's threat-sensitivity potentially makes the group more resistant to future targeting.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, external_threat_agents, excluded,
    powerful, generational, analytical, regional).

% Clinical observers who assess the psychological impact of intergenerational trauma encoding: whether it functions as adaptive preparedness or as pathogenic load, whether descendants benefit from threat-vigilance or suffer from inherited dysregulation. Their expertise is external to the ritual's authority but increasingly consulted as the psychological cost becomes visible.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, mental_health_professionals, observer,
    powerful, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__trauma_encoding_reading, ritual_custodians).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__trauma_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes and transmits collective memory of catastrophic threat across generations when direct experience is absent: patterns of persecution, warning signs, survival strategies, and group-response protocols are embedded in ritual practice so that descendants carry historical knowledge even if the original threat is quiescent.
% TRANSFER_FUNCTION: Moves psychological burden (intrusive memory, hypervigilance, identity fusion with trauma narrative) from direct survivors to subsequent generations as the price of maintaining threat-detection capacity. Future generations inherit both the early-warning system and the psychic load it carries.
% ABSENT_VOICES: Descendants who experience the ritual's psychological cost as disproportionate or maladaptive are structurally constrained by identity-lock: exit carries the cost of cultural dissolution and group abandonment, so dissent is internal and muted. Alternative framings of the catastrophe (healing, transcendence, normalcy) are excluded by the ritual's structure, which privileges vigilance over recovery narratives.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the collective threat-detection apparatus would degrade: descendants would lose rehearsed response patterns, historical awareness would fragment, and the group's sensitivity to persecution signs would erode within one or two generations. The catastrophe would become abstraction rather than embedded warning. Conversely, the psychological burden on descendants would lift substantially, though collective memory would be outsourced to historians and archives rather than carried in practice.
% FOUNDING_PROBLEM: A community experienced catastrophic targeting and persecution. The survivors needed the next generation to remember the threat patterns, maintain readiness, and avoid the vulnerabilities that had enabled the original attack. Direct testimony would fade; ritual encoding kept the warning alive across generational boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Survivors and ritual custodians affirm the founding problem remains live: threat patterns recur, persecution targets groups with collective memory gaps, and vigilance has prevented repeated harm. Mental health professionals and secular descendants contest whether the founding problem justifies ongoing extraction: they argue threats are structurally changed, that external vigilance (institutions, state protection, diaspora networks) now handles early warning, and that the ritual's cost has decoupled from its function. Historians outside the benefiting parties document both the historical accuracy of the original catastrophe and the psychological impact on descendants of inherited trauma encoding.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the ritual imposes intrusive memory, hypervigilance, and psychological burden on descendants as the price of maintaining threat-detection capacity — a transfer from present-generation payees to collective vigilance benefit. The cost is structured as duty, so alternatives are not transparent alternatives but failures of cultural identity. Suppression is moderate (0.45) because the constraint's persistence depends on both structural factors (identity-lock, social bonds, ritual authority) and internalized factors (descendants have partially adopted the threat-narrative as their own framework). Theater is low-to-moderate (0.22): the ritual retains genuine coordination function (threat-detection, historical transmission), but as time recedes from the founding catastrophe and external institutions assume threat-monitoring (state protection, diaspora networks), the proportion of the ritual's activity devoted to ongoing validation of threat-necessity rather than novel warning-transmission increases. The measurement series is authored on one shared grid: extractiveness rising early (0.52→0.63 by t=15, observed) then plateauing as the founding problem recedes into history and becomes abstract; theater rising steadily (0.08→0.25 by t=40, projected) as descendants increasingly perform the ritual to maintain group identity rather than to encode new threat-detection; suppression rising slowly (structural factors persist, but resistance from secular descendants grows as the founding threat becomes historical rather than present). This trajectory models a constraint shifting from functional coordination toward inertial performance: the founding problem is live for custodians and survivors but contested for descendants, and the separation creates diverging extraction profiles across seats.
 *
 * PERSPECTIVAL GAP:
 *   The ritual custodians and threat-vigilance collective read the constraint as essential: threat patterns recur, descendants with weak historical memory are vulnerable to recapitulation, and the ritual's burden is the price of collective survival. From their seat, extractiveness is justified coordination cost, not extraction. Trauma-bearing descendants read the same structure as psychological exploitation: the founding problem may be historically real, but its present-threat level is contested, and the ritual imposes costs (intrusive memory, identity-lock, transmission obligation) that exceed the demonstrable early-warning benefit they receive. Secular descendants sit between: they acknowledge the historical accuracy of the founding catastrophe but question whether modern descendants bear the same threat-vulnerability, and therefore whether the ritual's cost is still extractive rather than adaptive. Mental health professionals add an empirical layer: they measure distinct dysregulation patterns (complex PTSD markers, hypervigilance, dissociation) in ritual-bearing descendants and compare them to control groups, finding that the ritual's effect on threat-detection capacity is heterogeneous—some descendants show improved danger-recognition, others show pathogenic anxiety that impairs rather than improves actual threat-response. These divergent readings all emanate from the same structural data: the ritual encodes trauma, subsequent generations bear the burden, and the boundary between adaptive warning-system and psychological extraction is contested.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual custodians are near the beneficiary end (d ≈ 0.20): they set the constraint, maintain authority, derive spiritual/professional identity from custodianship. Exit is analytical (they could reframe the ritual, but that would dissolve their authority role). Threat-vigilance collective sits near symmetric (d ≈ 0.45): they receive genuine coordination benefit (threat-detection, historical knowledge) and carry diffuse burden (time, emotional engagement, cultural obligation). Exit is mobile (they can disengage partially or relocate to less-ritual-intensive communities, though identity costs apply). Trauma-bearing descendants are near the target end (d ≈ 0.75): they bear the psychological extraction (intrusive memory, hypervigilance, transmission obligation) and exit is identity-locked—leaving means cultural dissolution and group abandonment. Secular descendants are between (d ≈ 0.65): they doubt the ritual's necessity but remain bound by social connection and cultural embededness; their exit is constrained rather than trapped, so d is lower than direct descendants but higher than beneficiaries. The directionality divergence predicts that the engine will compute the custodians' seat as experiencing a more coordinative (lower extraction) type classification while descendants' seats compute as more extractive. This is the intentional perspectival gap—the same constraint structure produces different type verdicts from different seats because the structural relationship to the constraint (who sets it, who bears costs, who exits) is asymmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification resists the false split between 'pure coordination' (rope) and 'pure extraction' (snare). The ritual accomplishes genuine coordination: collective memory transmission, threat-detection capacity, survival-strategy encoding. But it does so through a structure that asymmetrically imposes psychological burden on descendants as the price of the coordination. A pure-rope reading would ignore the extraction; a pure-snare reading would dismiss the coordination function. Tangled_rope names that asymmetry: there is real coordination happening AND real extraction happening through the same mechanism. The mandatrophy risk would be classifying the ritual as a rope and concluding that descendants benefit without cost, or classifying it as a snare and concluding that threat-vigilance is mere cover story. The tangled_rope classification holds both truths: the ritual coordinates threat-detection AND it extracts psychological burden from descendants. The committer frame adds precision: from the TRAUMA_ENCODING reading, the extraction is not accidental—it is the mechanism through which trauma stays embodied and present rather than becoming historical abstraction. Alternative readings (boundary_maintenance, survival_competence, symbol_continuity) would frame different aspects of the same ritual and might compute different extraction profiles, but this reading's structural claim is that trauma encoding is both the coordination function AND the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.45) the result of external enforcement (ritual custodians policing participation, social sanction for non-compliance) or internalized identity-fusion (descendants have adopted the trauma-narrative and threat-vigilance as constitutive of self)?',
    'Longitudinal study of descendants who exit the ritual community: if suppression drops sharply and does not return after the structural enforcement is removed, suppression was primarily structural; if suppression persists or returns (intrusive memories, shame, identity-void), suppression was substantially internalized. Contrast with descendants who remain but express private dissent.',
    'If suppression is primarily structural, the constraint''s effective force could be reduced by lowering enforcement pressure. If substantially internalized, descendants carry the extraction with them even after exit—the constraint has rewritten their threat-assessment mechanisms. This changes the remediation pathway and the true cost measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Structural vs. internalized suppression mechanism in ritual trauma-bearing').

omega_variable(
    threat_vigilance_benefit_heterogeneity,
    'Does the ritual''s threat-vigilance encoding produce adaptive danger-recognition in all descendants, or does it produce heterogeneous outcomes where some descendants have improved threat-detection while others have maladaptive hypervigilance that impairs decision-making?',
    'Comparative measurement: study threat-response accuracy and speed in descendants with high ritual engagement vs. low engagement, controlling for baseline threat exposure. Measure both false-positive rates (false alarms, hypervigilance-driven risk aversion) and true-positive rates (actual danger recognition). Compare to mental health outcomes (anxiety, PTSD, trauma symptoms).',
    'If threat-vigilance is universally adaptive, the extraction is justified as the price of coordination. If outcomes are heterogeneous—some descendants benefit, others are harmed—then the ritual is imposing costs on descendants who do not capture the vigilance benefit, making the extraction more visible and less justifiable. A divergence between threat-detection accuracy and mental health outcomes would show that the ritual works for early-warning but harms the psychological wellbeing of the carriers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threat_vigilance_benefit_heterogeneity, empirical, 'Whether threat-vigilance benefit is universal or heterogeneous across descendants').

omega_variable(
    founding_problem_recurrence_rate_contestation,
    'Does the historical threat that motivated the ritual''s founding continue to recur at rates that justify ongoing encoding and extraction, or has the threat become sufficiently rare or structurally changed that the ritual''s vigilance no longer prevents meaningful harm?',
    'Historical analysis: measure the frequency of targeting events against the group over defined periods (pre-ritual, early ritual adoption, recent decades); decompose into events that the ritual''s threat-detection mechanisms demonstrably helped prevent vs. events that occurred despite the ritual''s vigilance; measure the counterfactual—what the threat rate would be without the ritual, using comparable threat-exposed communities with and without trauma-encoding rituals.',
    'If the threat recurs frequently and the ritual demonstrably prevents harm, the extraction is justified as coordination cost. If the threat is rare or has changed structurally (institutional protection, diaspora networks now handle threat-detection, threat-source is historically neutralized), then the ritual persists as performance and inertia—the founding problem is dead, not live. This collapses the tangled_rope classification toward piton (extraction with degraded function maintained by institutional inertia).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_recurrence_rate_contestation, empirical, 'Whether the founding threat recurs at rates justifying ongoing extraction').

omega_variable(
    alternative_threat_encoding_mechanisms,
    'Are there mechanisms other than ritual trauma-encoding that would preserve threat-vigilance across generations with lower psychological cost to descendants? Institutional memory, archival history, educational curricula, secular commemoration?',
    'Comparative case study: communities that encode catastrophic-threat history via non-ritual mechanisms (institutional memorial, historical education, diaspora networks, state-level threat awareness); measure threat-vigilance outcomes and psychological burden on descendants; compare to ritual-encoding communities on both dimensions.',
    'If alternatives exist with equivalent threat-detection and lower burden, the ritual''s extraction becomes harder to justify as necessary. If alternatives exist but fail to transmit threat-vigilance (descendants forget, deprioritize, intellectualize the threat), then the ritual''s emotional and identity-binding properties are structurally necessary for the coordination function—the extraction is baked into the mechanism. If alternatives exist with equivalent outcome and lower cost, this is a mandate for ritual innovation or replacement—the constraint becomes a candidate for mandatrophy resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_threat_encoding_mechanisms, conceptual, 'Whether ritual trauma-encoding is structurally necessary or substitutable for threat-vigilance').

omega_variable(
    kernel_reading_committer_stakes,
    'Does the choice of reading (trauma_encoding vs. boundary_maintenance vs. survival_competence vs. symbol_continuity) materially affect the classification and remediation of the ritual constraint, or is the choice of reading a framing preference that leaves the structural classification unchanged?',
    'Generate the constraint stories for all four sibling readings using the same base ritual data; compare the computed engine classifications from each reading''s seat; measure the variance in classification across readings. If readings produce divergent classifications, the reading choice is structurally material. If they produce convergent classifications with different narrative framings, the reading is interpretive but not structurally distinguishing.',
    'If readings are structurally material, committer choice matters for policy—choosing trauma_encoding emphasizes extraction and descendants'' burden; choosing survival_competence emphasizes adaptive function and threat-prevention; choosing boundary_maintenance emphasizes identity and group-coherence. This means the ritual constraint cannot be adjudicated without resolving the kernel reading. If readings are merely narrative variations on the same structure, the committer frame is non-binding—all readings converge to the same underlying constraint, and the kernel dispute is philosophical rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_stakes, conceptual, 'Whether kernel-reading choice is structurally material or interpretive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 25, 0.21).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.25).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 30, 0.69).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cata_su_t5, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 25, 0.46).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__trauma_encoding_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested catastrophe_memory_kernel. Four structurally distinct constraints are authored from the same kernel ritual: trauma_encoding_reading (this story, D4 extraction focused), boundary_maintenance_reading (D4 identity focus), survival_competence_reading (D4 adaptive capacity focus), and symbol_continuity_reading (D4 narrative focus). The readings coexist as competing hermeneutics in field practice; each produces a different extraction profile and beneficiary/victim assignment from the same ritual structure. The network links are coexistential: the readings do not foreclose one another—different community positions weight them differently. Committer choice of reading matters for policy adjudication but does not settle the kernel dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__trauma_encoding_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
