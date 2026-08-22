% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo as Incoherent Bundle Maintained by Institutional Power
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   Shinbutsu-shugo (Shinto-Buddhism coexistence) was never a coherent
 *   philosophical or theological synthesis but an incoherent institutional
 *   bundle sustained by deliberate categorical avoidance and backed by the
 *   power of both temple and shrine establishments to enforce non-inquiry.
 *   The system extracted resources from ordinary practitioners through
 *   overlapping ritual systems (shrine for life-cycle rites, temples for
 *   death-and-afterlife, both for seasonal festivals) without settling what
 *   kami or Buddhas actually *were* or how they related. The Meiji bunri
 *   (separation edict) did not create this incoherence — it merely withdrew
 *   the suppression mechanism (institutional power, cultural normalization of
 *   ambiguity) that kept the incoherence from becoming visible. This reading
 *   instantiates the incoherence as structural fact, not as a
 *   coherence-seeking system that failed. The sibling readings
 *   (syncretic_fusion and domain_partition) treat shinbutsu-shugo as a
 *   coherent philosophical project; this reading denies the project existed
 *   at all — what was called shinbutsu-shugo was institutional rent
 *   collection dressed as coordination.
 *
 * KEY AGENTS:
 *   - Temple institutions: agenda-setters and beneficiaries; maintained authority through shrine-housing and doctrinal elaboration; benefited from categorical ambiguity that obscured institutional incursion.
 *   - Shrine institutions: agenda-setters and beneficiaries; dependent on temples yet autonomous in name; benefited from ambiguity that left their independent status unresolved.
 *   - Bakufu authority: agenda-setter; maintained legitimacy by non-intervention; extracted political stability from dual-institution tolerance without categorical adjudication.
 *   - Ordinary practitioners: payers; absorbed overlapping fees and contradictory doctrine; internalized institutional confusion as cultural norm rather than perceiving it as suppression.
 *   - Philosophical seekers (excluded): would have articulated categorical boundaries but were systematically kept from authority over definitions; exclusion was the maintenance mechanism.
 *   - Meiji reformers (observers): discovered that the system contained no hidden coherence — only institutional power maintaining deliberate confusion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.68).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.72).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.64).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, piton).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-shugo as Incoherent Bundle Maintained by Institutional Power").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious_studies/philosophy_of_religion/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '704fcff7-11e8-442f-9b13-51f1e8e477d9').
narrative_ontology:cs_kernel_codification('704fcff7-11e8-442f-9b13-51f1e8e477d9', distributed).
narrative_ontology:cs_authority_grounding('704fcff7-11e8-442f-9b13-51f1e8e477d9', extraction).
narrative_ontology:cs_interpretation_layer_present('704fcff7-11e8-442f-9b13-51f1e8e477d9').
narrative_ontology:cs_reading_relation('704fcff7-11e8-442f-9b13-51f1e8e477d9', shinbutsu_coexistence_commitment__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('704fcff7-11e8-442f-9b13-51f1e8e477d9', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('704fcff7-11e8-442f-9b13-51f1e8e477d9', foundational, ambiguity_is_structural_not_apparent).
narrative_ontology:cs_axiom_status(ambiguity_is_structural_not_apparent, holdable).
narrative_ontology:cs_axiom_grounding('704fcff7-11e8-442f-9b13-51f1e8e477d9', ambiguity_is_structural_not_apparent, empirically_contingent).
narrative_ontology:cs_axiom('704fcff7-11e8-442f-9b13-51f1e8e477d9', foundational, institutional_power_sustains_incoherence).
narrative_ontology:cs_axiom_status(institutional_power_sustains_incoherence, holdable).
narrative_ontology:cs_axiom_grounding('704fcff7-11e8-442f-9b13-51f1e8e477d9', institutional_power_sustains_incoherence, empirically_contingent).
narrative_ontology:cs_reference_frame('704fcff7-11e8-442f-9b13-51f1e8e477d9', shinbutsu_coexistence_as_incoherent_bundle).
narrative_ontology:cs_drift_state('704fcff7-11e8-442f-9b13-51f1e8e477d9', meiji_categorical_separation, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('704fcff7-11e8-442f-9b13-51f1e8e477d9', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, temple_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, bakufu_authority).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, ordinary_practitioners).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, philosophical_coherence_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhist temples maintain authority over doctrinal interpretation and ritual performance by integrating kami-centered practices within their precincts (shrine-temples, or jingū-ji). They benefit from the ambiguity between 'Buddhist' and 'Shinto' because it allows them to claim jurisdiction over both domains without committing to a stable definition of either. They elaborate doctrinal works (honji suijaku theology) that appear to unify kami and Buddhas while actually deferring categorical clarity. They enforce the ambiguity through alliance with shrines and the bakufu, framing non-inquiry as a virtue of cultural accommodation rather than institutional necessity.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, temple_institutions, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, temple_institutions, beneficiary).

% Shinto shrines depend on Buddhist temples for ritual expertise (Buddhist priests perform kami rituals), doctrinal legitimacy (honji suijaku places kami within Buddhist cosmology), and political support (bakufu favor for both institutions). They benefit from ambiguity that leaves their independent status unresolved — if clarity came, their dependence on temples would become visible and threaten their autonomy claims. They maintain the ambiguity through rituals that blend kami and Buddhist elements without specifying the ontological relationship, and through strategic alliances that preserve their stakes in both systems.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_institutions, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_institutions, beneficiary).

% The Tokugawa bakufu maintains social order by refusing to adjudicate between competing Buddhist and Shinto claims. Enforcing clarity would require choosing a winner — either elevating Buddhism (alienating Shinto constituencies and shrine-temple alliances) or elevating Shinto (undermining Buddhist institutional authority and philosophical legitimacy). Ambiguity is a governance technology: it permits dual extraction (both institutions pay allegiance to the bakufu; both extract resources from practitioners; the bakufu extracts legitimacy from tolerating both) without triggering sectarian conflict that would challenge bakufu authority. The bakufu enforces this non-adjudication actively through edicts, patronage decisions, and control of institutional appointments.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, bakufu_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Villagers, townspeople, and ordinary believers participate in shrine rituals (kami veneration for immediate, this-worldly concerns: purity, fertility, harvest, household welfare) and temple rituals (Buddhist doctrine for salvation, afterlife assurance, ancestral veneration) and overlapping hybrid rituals without clear guidance on the underlying metaphysics. They pay fees to both institutions for overlapping functions (both perform life-cycle rites, both conduct seasonal festivals, both claim authority over household religious welfare). They absorb contradictory doctrinal instruction — that kami are local manifestations of Buddhas (honji suijaku), that kami and Buddhas govern separate domains (domain partition), that the two are simply complementary aspects of a single cosmology (syncretism) — and internalize the dissonance as 'just how things are done,' normalizing confusion as cultural tradition. Exit is constrained by embedded ritual participation: refusing shrine rites makes one ritually impure and socially marginal; refusing temple ties means severing Buddhist salvation assurances and ancestral care.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, ordinary_practitioners, payer,
    powerless, biographical, constrained, local).

% Buddhist intellectuals (honji suijaku theorists, Tendai and Nichiren philosophers), Shinto nativists (Shinto scholars articulating independent kami-ontology), and Neo-Confucian synthesizers (attempting metaphysical unification of kami, Buddhas, and Heaven-and-Earth cosmology) all attempted to articulate a coherent framework that would clearly specify what kami and Buddhas are and how they relate. Their work was tolerated insofar as it did not disrupt institutional arrangements — it was praised as scholarship, incorporated into doctrinal traditions, taught in religious schools — but systematically prevented from adjudicating the categorical boundary in a way that would threaten either temples or shrines. Genuine unification would require one institution to subordinate the other; genuine partition would require clarifying which institutions govern which domains. Both would destabilize the power-sharing arrangement. Excluded voices would have authority only if institutional power consented to categorical clarity; their exclusion from that authority preserves institutional ambiguity.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, philosophical_coherence_seekers, excluded,
    moderate, generational, constrained, national).

% Meiji state authorities tasked with modernizing Japan's institutions and clarifying its relationship to Western nation-state categories. They investigated shinbutsu-shugo as a 'problem' to be solved — expecting either a coherent metaphysical synthesis that could be preserved, or a coherent institutional division that could be rationalized into modern categorical forms. They discovered instead an operating incoherence: no stable ontology, no principled institutional boundary, no founding problem that still needed solving. The system persisted through sheer institutional power and deliberate avoidance of clarity. Their bunri (separation) edict did not create this incoherence; it merely withdrew the suppression mechanism by forcing Western categorical clarity and prohibiting syncretic practice. Once suppression lifted, the incoherence became visible. They possessed the authority to dismantle the constraint but were also external to its extraction — they are observers, not beneficiaries or victims.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_reformers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__incoherent_bundle_reading, temple_institutions).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits two powerful institutional orders (temples and shrines) to coexist and extract resources without forcing either into subordination; allows practitioners to participate in life-cycle and spiritual activities without committing to a single ontological framework; solves the political problem of how the bakufu can maintain legitimacy with both institutional constituencies by refusing to adjudicate between them.
% TRANSFER_FUNCTION: Moves financial resources (ritual fees, votive offerings, alms) from ordinary practitioners to both temple and shrine institutions through overlapping, often-duplicate ritual systems. Moves administrative authority from both institutions upward to the bakufu, which extracts legitimacy and political stability from the non-adjudication. The transfer is rendered invisible by categorical ambiguity: practitioners cannot clearly specify what they are paying for because the system does not offer clear categorical boundaries between 'Buddhist' and 'Shinto' activities.
% ABSENT_VOICES: Philosophical coherence-seekers — Buddhist theorists of honji suijaku, Shinto nativists articulating independent kami-ontology, Neo-Confucian synthesizers — all had stakes in categorical clarity and would articulate the boundaries if permitted. They are systematically excluded from authority over definitions; their continued absence is the mechanism that sustains institutional ambiguity. If they were granted adjudicating power, institutional extraction would become transparent and bakufu governance would be exposed as non-neutral power-brokering.
% DISAPPEARANCE_RATIONALE: If shinbutsu-shugo disappeared (temples and shrines forced to separate), the immediate effect would be institutional conflict: temples would lose authority over kami rituals and shrine income, shrines would lose temple doctrinal support and legitimacy. Practitioners would demand coherent doctrine or choose which institution to follow. The bakufu would lose the non-adjudication mechanism and face pressure to state which institution the state sanctioned. Philosopher-seekers would finally access authority over categorical boundaries. The entire arrangement would reorganize — institutional power-sharing would break down, practitioners would consolidate around clearer frameworks, and the suppression mechanism would disappear because no institution would profit from maintaining it.
% FOUNDING_PROBLEM: Two powerful institutional orders (Buddhist temples and Shinto shrines) with incompatible claims to authority; each had sufficient institutional power to prevent subordination by the other; practitioners needed both life-cycle rites and spiritual/salvation doctrine; the bakufu could not favor one without losing the support of the other.
% FOUNDING_PROBLEM_CORROBORATION: Tokugawa-era institutional historians document that by the middle of the Edo period, temples and shrines had resolved their competition through stable resource-sharing and mutual accommodation. Shrine-temples (jingū-ji) combined both functions under unified administration; doctrinal works (honji suijaku) provided a framework that permitted both to coexist without direct competition. The incompatible-competition problem was solved. Yet shinbutsu-shugo persisted, now functioning purely as an extraction mechanism defended by theatrical coherence-seeking and deliberate ambiguity. The persistence of the constraint after its founding problem was solved is the diagnostic signature of mandatrophy and piton classification. Meiji reformers' own reports documented finding no hidden coherence — only institutional arrangements defended by power, not by genuine philosophical integration.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The piton classification rests on several structural observations: (1) Theater ratio at 0.64 indicates that most ongoing activity defends the ambiguity itself rather than generating genuine religious value — doctrinal elaboration, ritual innovation, and institutional alliance-building all function to obscure rather than clarify. (2) Extraction grows modestly over the interval (0.42 → 0.68) as institutional demand for resources increases while the founding problem (incompatible institutional competition) has long since been solved through accommodation — the constraint persists but no longer serves its original coordination function. (3) Suppression requirement also increases (0.52 → 0.72) because maintaining the ambiguity requires more active enforcement as literacy, philosophy, and institutional pressure from Meiji and Western categories make the incoherence harder to normalize. (4) Accessibility_collapse is moderate-high (0.71) because once you understand the system, you realize alternatives (coherent Buddhism, independent Shinto, foreign religions) are available but suppressed by institutional coordination. (5) Resistance is moderate (0.58) because criticism exists (philosophical seekers, Shinto nativists, some Buddhist reformers) but cannot penetrate the institutional power backing the ambiguity. The measurement series show steady degradation: theater rises because performance must compensate for growing incoherence; suppression must tighten to keep the confusion from becoming visible; extraction drifts upward as institutions that no longer coordinate anything still extract legitimacy and resources from practitioners. This is classic piton decay: the founding problem solved, but the extraction mechanism persists because no single party bears enough cost to motivate fixing it (temples benefit, shrines benefit, bakufu benefits; ordinary practitioners bear diffuse costs but cannot organize; excluded voices cannot access authority).
 *
 * PERSPECTIVAL GAP:
 *   The institutional seats (temples, shrines, bakufu) perceive shinbutsu-shugo as a coherent coordination mechanism they maintain through active authority and doctrinal work. From the ordinary practitioner seat, the system appears as confusion that must be tolerated — the cognitive dissonance is normalized as cultural tradition. From the excluded philosophical seat (where it is inaccessible), the system appears as institutional power preventing categorical clarity. From the Meiji reformer seat (external observer), the system appears as incoherence revealed by removing the suppression mechanism. These divergences arise from the structural fact that the constraint's persistence depends on avoiding any single coherent frame — it operates by preventing seats from sharing a common understanding of what the system *is*. The engine will compute different types from different seats: institutional seats may see rope (coordination benefiting all), practitioner seats may see snare (extraction with limited exit), observer seats may see piton (theater masking institutional power in service of no genuine function). These divergences ARE the constraint's working mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Temple and shrine institutions are structural beneficiaries (they extract resources, set rules, control definitions) — d near 0.1. The bakufu is a symmetric beneficiary with arbitrage exit (it extracts political stability and legitimacy, could in principle withdraw and let temples and shrines fight, but chooses not to) — d near 0.4-0.5. Ordinary practitioners are targets (they pay in resources and cognitive labor, confused by deliberate ambiguity, exit is constrained by embedded ritual participation) — d near 0.8-0.9. Philosophical seekers are excluded targets (they would contribute to clarification but are kept from authority; their exclusion is structural to the extraction) — d near 0.85. Meiji reformers are analytical observers (external vantage, capacity to withdraw the suppression mechanism, no direct stake in the constraint's persistence) — d = 1.0 analytical. The derivation from beneficiary/victim declarations is straightforward: temples and shrines are named beneficiaries (they collect institutional authority and resources); practitioners are named victims (they bear diffuse costs of ambiguity). Beneficiaries get low d; victims get high d. No overrides needed — the structural derivation captures the real directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy signal is strong: founding_problem_status = dead, disappearance_verdict = world_rearranges. This mismatch indicates the constraint persists despite serving no original purpose. The system was built to solve incompatible institutional competition; by the late Tokugawa period, that competition had been solved through resource-sharing and mutual accommodation. Yet the theatrical apparatus (doctrinal elaboration, ritual innovation, ambiguity-maintenance) persists with no new founding problem to justify it. The piton classification prevents mischaracterization as rope (which would require active coordination value) or tangled_rope (which would require genuine asymmetric exchange in service of coordination). The classification captures that the system is mostly performance now — both institutions maintain it because the cost of maintenance (institutional effort, doctrinal elaboration, alliance-building with bakufu) is less than the benefit of persistence (resource extraction, autonomy claims, political legitimacy). But the performance itself is the extraction: ordinary practitioners must absorb the confusion, must pay both institutions for overlapping functions, must internalize the incoherence as natural. The theater is not a side effect of coordination — it IS the extraction mechanism. Mandatrophy is resolved by naming this fact: the constraint persists because institutional power sustains it, not because any genuine founding problem remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_coherence_vs_institutional_performance,
    'Is the absence of coherent ontology in shinbutsu-shugo a descriptive fact about the system''s actual conceptual content, or is it a reading imposed by external observers who lacked access to the sophisticated doctrinal framework practitioners understood?',
    'Textual analysis of surviving doctrinal works (honji suijaku texts, shrine and temple liturgical documents, popular instruction manuals) from practitioners'' own frame, asking whether they offered coherent ontology or deliberately deferred categorical questions. Ethnographic reconstruction of lay understanding pre-Meiji to test whether practitioners perceived coherence or tolerated incoherence.',
    'If doctrinal works reveal coherent ontology, the system is syncretic_fusion_reading, not incoherent_bundle_reading. If they systematically avoid categorical clarity while performing coherence through ritual and ambiguous language, the incoherent_bundle reading is supported. If lay practitioners had coherent understanding despite doctrinal ambiguity, the domain_partition_reading is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_coherence_vs_institutional_performance, empirical, 'Whether shinbutsu-shugo contained coherent ontology that this reading fails to perceive, or genuinely lacked ontological coherence.').

omega_variable(
    suppression_mechanism_internalization,
    'Did ordinary practitioners internalize the categorical ambiguity as cultural norm through normalization (structural suppression: no one ever presented the alternative of coherence), or through active ideological work (internalized suppression: they were taught that ambiguity is a virtue)?',
    'Textual analysis of instruction materials (Buddhist sermons, Shinto teachings, popular guides) addressing how practitioners should understand kami-Buddha relationship. Post-Meiji interview data or diaries from practitioners who experienced the transition, showing whether bundling felt natural or revealed as confusion once Meiji categories separated them.',
    'If suppression is primarily structural (alternatives not presented), the constraint is more of a snare (victims trapped by information asymmetry). If suppression is primarily internalized (ambiguity taught as virtue or necessity), identity-locking is stronger and the piton classification is reinforced. If both, the suppression_mechanism omega documents the heterogeneity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of categorical clarity was structural or internalized in practice.').

omega_variable(
    institutional_power_necessity,
    'Would ordinary practitioners have spontaneously rejected shinbutsu-shugo as incoherent if institutional power did not enforce its continuation, or would they have defended it as a meaningful framework?',
    'Counterfactual reasoning from regions where institutional enforcement was weaker (peripheral shrines with minimal temple ties, or areas with strong Shinto nativist movements or Christian competition). Did practitioners in those contexts spontaneously move toward coherence-seeking alternatives, or remain with the ambiguous bundle?',
    'Strong institutional-power dependence (practitioners reject ambiguity only when suppression is withdrawn) supports the piton reading. Spontaneous coherence-seeking despite suppression would suggest practitioners perceived incoherence and suffered under it — supporting snare classification. Spontaneous defense of ambiguity despite institutional power withdrawal would suggest the framework had genuine participant value — supporting rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_power_necessity, conceptual, 'Whether the system''s persistence depended primarily on institutional suppression or had independent participant endorsement.').

omega_variable(
    meiji_bunri_as_revelation_vs_imposition,
    'Did Meiji bunri (the separation edict) reveal an existing underlying incoherence that institutional power had suppressed, or did it actively impose a coherence-demanding framework that overrode a functioning (if not coherent) system?',
    'Comparative examination of Meiji officials'' stated goals (understanding the ''true'' nature of kami and Buddhas) versus actual archival evidence of pre-Meiji institutional and doctrinal landscape. Did Meiji reformers discover incoherence or impose Western categorical demands that made traditional ambiguity suddenly appear incoherent?',
    'If bunri revealed pre-existing incoherence, the incoherent_bundle_reading is correct — the constraint''s essence was suppressed ambiguity. If bunri imposed external categorical demands, the system may have been genuinely coherent in its own frame (supporting syncretic_fusion or domain_partition readings). If bunri both revealed incoherence AND imposed external categories (making the revelation intelligible only in Western terms), all three readings coexist with different reference frames.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_bunri_as_revelation_vs_imposition, conceptual, 'Whether Meiji bunri revealed incoherence or imposed a coherence-demanding framework that made ambiguity visible as incoherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(shin_tr_t80, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 80, 0.56).
narrative_ontology:measurement(shin_tr_t120, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 120, 0.6).
narrative_ontology:measurement(shin_tr_t160, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 160, 0.62).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 200, 0.63).
narrative_ontology:measurement(shin_tr_t250, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 250, 0.64).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(shin_be_t40, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(shin_be_t80, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 80, 0.54).
narrative_ontology:measurement(shin_be_t120, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 120, 0.61).
narrative_ontology:measurement(shin_be_t160, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 160, 0.65).
narrative_ontology:measurement(shin_be_t200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 200, 0.67).
narrative_ontology:measurement(shin_be_t250, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 250, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(shin_su_t40, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(shin_su_t80, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 80, 0.63).
narrative_ontology:measurement(shin_su_t120, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 120, 0.68).
narrative_ontology:measurement(shin_su_t160, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 160, 0.7).
narrative_ontology:measurement(shin_su_t200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 200, 0.71).
narrative_ontology:measurement(shin_su_t250, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 250, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__domain_partition_reading).

% DUAL FORMULATION NOTE:
% Shinbutsu-shugo is a contested kernel with three structurally distinct readings: syncretic_fusion_reading (honji suijaku unification), domain_partition_reading (institutional role separation), and incoherent_bundle_reading (this constraint — no coherence, only suppression). The three constraints share a referent (the Tokugawa coexistence system) but instantiate different ε values and structural claims. The readings coexist as live positions held by different scholarly and religious communities; none forecloses the others within a single consistent framework. This constraint's claim is that the system's 'coherence' was an illusion created by suppression; the syncretic and domain-partition readings are coherent-system hypotheses that would be foreclosed by evidence confirming systematic avoidance of categorical clarity. See the three omegas (ontological_coherence, suppression_mechanism, institutional_power_necessity) for the empirical bases that would differentiate the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
