% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__orthodox_varna_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Orthodox Varna Reading of the Vedic Corpus as Divinely Mandated Cosmic Order
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested Vedic corpus kernel: the
 *   orthodox varna reading, which holds that Vedic and Dharmashastra texts
 *   literally prescribe a divinely mandated, hereditary social hierarchy
 *   (Brahmin-Kshatriya-Vaishya-Shudra, with avarna groups outside the schema
 *   entirely) as fixed cosmic order rather than metaphor or later gloss. Two
 *   sibling readings exist as separate constraints: the
 *   reformist_spiritual_reading (which denies any prescriptive social content
 *   in the texts) and the colonial_orientalist_reading (which treats the
 *   corpus as a unified administrative law code, an artifact of British
 *   codification practice). Per the ε-invariance principle, these are not
 *   three measurements of one constraint but three structurally distinct
 *   constraints sharing a textual kernel; only the orthodox reading is
 *   instantiated here, and its ε (0.86) is the extraction this reading's own
 *   institutional operation produces, not an average across readings.
 *
 * KEY AGENTS:
 *   - brahmin_caste: agenda_setter/beneficiary (institutional/arbitrage) — sets and benefits from interpretive authority
 *   - shudra_caste: primary payer (powerless/trapped) — bears occupational and ritual restriction
 *   - dalit_communities and avarna_outcaste_groups: primary payers (powerless/trapped) — bear exclusion beyond even subordinate inclusion
 *   - reform_movements: excluded voice (organized/constrained) — counter-exegesis kept outside orthodox interpretive space
 *   - comparative_religion_scholars: analytical observer — trace textual layering across periods
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.86).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.88).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Orthodox Varna Reading of the Vedic Corpus as Divinely Mandated Cosmic Order").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious_studies/social_stratification/hermeneutics").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, 'dc089dc5-51e6-4e3f-a090-7e95b68dc57e').
narrative_ontology:cs_kernel_codification('dc089dc5-51e6-4e3f-a090-7e95b68dc57e', fixed_text).
narrative_ontology:cs_authority_grounding('dc089dc5-51e6-4e3f-a090-7e95b68dc57e', lineage).
narrative_ontology:cs_interpretation_layer_present('dc089dc5-51e6-4e3f-a090-7e95b68dc57e').
narrative_ontology:cs_reading_relation('dc089dc5-51e6-4e3f-a090-7e95b68dc57e', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('dc089dc5-51e6-4e3f-a090-7e95b68dc57e', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('dc089dc5-51e6-4e3f-a090-7e95b68dc57e', foundational, varna_hierarchy_is_literal_cosmic_prescription).
narrative_ontology:cs_axiom_status(varna_hierarchy_is_literal_cosmic_prescription, holdable).
narrative_ontology:cs_axiom_grounding('dc089dc5-51e6-4e3f-a090-7e95b68dc57e', varna_hierarchy_is_literal_cosmic_prescription, theological).
narrative_ontology:cs_axiom('dc089dc5-51e6-4e3f-a090-7e95b68dc57e', secondary, birth_caste_duty_svadharma_is_binding_and_hereditary).
narrative_ontology:cs_axiom_status(birth_caste_duty_svadharma_is_binding_and_hereditary, holdable).
narrative_ontology:cs_axiom_grounding('dc089dc5-51e6-4e3f-a090-7e95b68dc57e', birth_caste_duty_svadharma_is_binding_and_hereditary, theological).
narrative_ontology:cs_reference_frame('dc089dc5-51e6-4e3f-a090-7e95b68dc57e', brahminical_smriti_orthodoxy).
narrative_ontology:cs_drift_state('dc089dc5-51e6-4e3f-a090-7e95b68dc57e', post_independence_constitutional_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('dc089dc5-51e6-4e3f-a090-7e95b68dc57e', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, avarna_outcaste_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_and_vaishya_castes).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_and_vaishya_castes).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, cosmic_order_doctrine).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, purusha_sukta_sacrificial_cosmology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the interpretive authority over the Purusha Sukta and Dharmashastra literature; administers ritual gatekeeping (who may recite, teach, and adjudicate scripture), occupies the top of the varna order the reading declares cosmically fixed, and collects the material and status benefits (ritual fees, land grants historically, deference, endogamous marriage protection) that flow from the hierarchy's operation. Can revise or soften the reading's application but structurally benefits from not doing so.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, beneficiary).

% Occupy the second and third rungs of the declared order; benefit from ritual and social superiority over Shudras and outcaste groups while remaining subordinate to Brahmin interpretive and ritual authority. Bear some cost (deference obligations, restricted upward ritual mobility) but net benefit from the hierarchy's downward-facing extraction.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_and_vaishya_castes, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_and_vaishya_castes, payer).

% Assigned by the reading to hereditary service occupations, barred from Vedic study and many ritual functions, and subject to marital and occupational restrictions justified as cosmically ordained duty (svadharma). Labor value is extracted through mandated service relationships; exit requires renouncing caste identity entirely, which carries severe social and economic penalty and is rarely achievable within the lifetime of an individual.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste, payer,
    powerless, generational, trapped, continental).

% Positioned outside the four-varna schema entirely (avarna/'untouchable'), bearing the most severe restrictions: exclusion from temples, wells, and shared public space historically, mandated degrading occupations, and ritual pollution taboos enforced through social and sometimes physical violence. The orthodox reading's cosmic-order framing supplies the theological justification for this exclusion; exit has historically required conversion out of the tradition entirely, itself heavily sanctioned.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities, payer,
    powerless, generational, trapped, continental).

% Overlapping with Dalit communities but including additional tribal and itinerant groups placed outside the varna order; bear the reading's exclusion without even nominal inclusion in the hierarchy's promised (if subordinate) place, making their structural position one of pure exclusion rather than ranked subordination.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, avarna_outcaste_groups, payer,
    powerless, generational, trapped, continental).

% Bhakti saints, Arya Samaj, Ambedkarite and other reform traditions have argued the prescriptive reading is a later, self-interested gloss on texts whose core content is spiritual and non-social. Within orthodox institutional space their arguments are treated as heterodox or as attacks on tradition rather than as legitimate alternative exegesis; they operate largely outside the interpretive authority structure this reading maintains.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, reform_movements, excluded,
    organized, biographical, constrained, national).

% Study the textual history of varna prescriptions (Purusha Sukta, Manusmriti, Dharmashastra corpus) across strata and periods, documenting where and when explicit social prescription was added, amplified, or contested, without being party to the material benefits or costs the reading distributes.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__orthodox_varna_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The orthodox reading provides a stable, cosmically-grounded division of ritual, martial, economic, and service labor across a large and diverse society — assigning each group a defined role and a theological account of why that role is legitimate and permanent, which reduces certain forms of status contestation by fixing it as sacred fact rather than negotiated arrangement.
% TRANSFER_FUNCTION: Moves labor, ritual deference, land and economic surplus, and social status from Shudra, Dalit, and avarna groups upward to Brahmin (primarily) and secondarily Kshatriya/Vaishya castes, justified as the fulfillment of cosmically assigned duty (svadharma) rather than as transfer.
% ABSENT_VOICES: Reform and Bhakti traditions, and directly the Shudra and Dalit communities themselves, have produced centuries of counter-exegesis (including readings that this hierarchy is a corruption or later insertion) that orthodox institutional space has treated as heterodoxy rather than admitting into the interpretive process that fixes the reading's authority.
% DISAPPEARANCE_RATIONALE: If the orthodox prescriptive reading lost its institutional and social force overnight, occupational restriction, marriage endogamy enforcement, and ritual exclusion tied to birth-caste would lose their theological cover; the material arrangements (land tenure patterns, service-caste labor relationships, temple access, endogamy) built on top of the reading over centuries would face sustained pressure to reorganize, as has partially occurred under legal abolition of untouchability and reservation policy — evidence the reading's removal produces real rearrangement, not mere relabeling.
% FOUNDING_PROBLEM: Early Vedic and post-Vedic society faced a coordination problem of integrating priestly, martial, economic, and service functions across an expanding and diversifying population without a centralized state apparatus; a cosmological account of fixed role and duty offered a stability mechanism where none of enforcement, negotiation, or market allocation was yet institutionally available.
% FOUNDING_PROBLEM_CORROBORATION: Comparative religion scholars and historians of the Dharmashastra corpus (documenting the textual layering and the substantial expansion of prescriptive detail in post-Vedic smriti literature relative to earlier Vedic strata) attest that whatever functional integration problem existed in the earliest period has long since been superseded by state law, market labor allocation, and constitutional abolition of untouchability; Ambedkarite scholarship and Dalit testimony from outside the Brahmin beneficiary group corroborate that the arrangement's persistence past this point serves status and material maintenance rather than any live coordination need.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 0.86, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.86) is authored high because the reading's operation directly and durably transfers labor, land, ritual deference, and status from Shudra/Dalit/avarna groups to Brahmin and secondarily Kshatriya/Vaishya castes, and because the transfer is framed as cosmic duty rather than negotiated arrangement, which historically forecloses renegotiation. Suppression (0.88) is authored even higher than extractiveness because the reading's persistence depends on active enforcement — social ostracism, ritual exclusion, historically sanctioned violence, and restriction of exit routes (conversion, migration) — not merely on participant preference; suppression is authored as the raw structural fact of enforcement intensity and is not scaled by scope or power in this metric (only extractiveness receives that scaling in the engine's computation). Accessibility collapse (0.62) is moderate rather than near-total because counter-traditions (Bhakti, Buddhist and Jain heterodoxy, later Ambedkarite reform) persisted continuously alongside the orthodox reading, meaning alternatives never fully vanished even though institutional dominance suppressed their uptake. Resistance (0.78) is authored high, reflecting centuries of documented resistance from within Shudra, Dalit, and reform communities. Theater ratio rises modestly over the measured interval (0.1 to 0.3) as legal abolition of untouchability and constitutional equality provisions push enforcement of the hierarchy increasingly into informal, social, and ritual registers rather than formal legal ones — the function persists but an increasing share of its maintenance is performative/social rather than backed by formal sanction.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahmin agenda-setter seat, the hierarchy is authored/experienced as sacred, stable order consistent with dharma — a genuine cosmological account, not an extraction scheme. From the Shudra/Dalit/avarna payer seats, the identical structure is authored/experienced as enforced subordination whose theological framing forecloses renegotiation. The engine computes these divergent per-seat classifications from the structural power/exit/directionality data; this story does not adjudicate which seat is 'correct' — it authors the structural data honestly for both.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin caste sits at the full-beneficiary end: it authors and administers the interpretation, occupies the top of the declared order, and collects the ritual, material, and status benefits the hierarchy distributes, with arbitrage-level exit (able to reposition doctrinally or institutionally without losing status). Kshatriya and Vaishya castes are declared as secondary beneficiaries who also pay some subordination cost to Brahmin authority — a genuinely dual-positioned relationship reflected in their secondary_role. Shudra, Dalit, and avarna groups sit at the full-target end: trapped exit options (renouncing caste identity or the tradition entirely carries severe penalty), powerless power atom, and directly named as victims whose labor and social standing the constraint extracts. Reform movements are excluded rather than coordinated or extracted from directly through this reading's structure — their objection exists but is kept outside the interpretive authority the reading maintains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem × disappearance_verdict pairing is deliberately a mismatch signal: founding_problem_status is authored 'dead' (the early coordination problem of integrating priestly/martial/economic/service functions without centralized state apparatus has been superseded by state law, constitutional abolition of untouchability, and market labor allocation) while disappearance_verdict is 'world_rearranges' (removing the reading's institutional force would still produce real material rearrangement in extant caste-based labor and marriage patterns). This is the classic zombie-mandate signature: the coordination function that once justified the arrangement no longer exists, but the extraction built on top of it persists and would visibly unwind if withdrawn — which is exactly the pattern that should read as tangled_rope-adjacent-snare rather than genuine ongoing coordination, and is why the claimed_type is authored as snare rather than tangled_rope: the coordination story is authored as functionally dead, leaving the extraction as the operative content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_layering_vs_original_content,
    'Does the prescriptive social content the orthodox reading relies on (varna duty, occupational restriction, marital endogamy) originate in the earliest Vedic strata (Samhitas), or is it substantially a later accretion concentrated in post-Vedic Brahmana, Dharmashastra, and Smriti literature attributed retroactively to Vedic authority?',
    'Philological dating and stratigraphic analysis of the corpus (comparing Rigvedic Purusha Sukta language against later Manusmriti elaboration), cross-referenced against comparative religion scholarship on when explicit occupational/marital restriction language appears and intensifies.',
    'If the prescriptive content is substantially a later accretion, the orthodox reading''s claim to represent ''the Vedic texts themselves'' rather than a specific interpretive tradition layered onto them is weakened, supporting the reformist_spiritual_reading''s account of the earliest strata; if the prescriptive content is genuinely present from the earliest layer, the orthodox reading''s textual claim is strengthened even though its normative status remains separately contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_layering_vs_original_content, empirical, 'Whether varna''s prescriptive social content is original to earliest Vedic strata or a later accretion.').

omega_variable(
    cosmic_order_naturalness_vs_construction,
    'Is the varna hierarchy, as this reading holds, a genuine feature of cosmic/divine order (which would make resistance to it a category error rather than legitimate objection), or is it a constructed social arrangement that identifiable groups (the Brahmin caste) benefit from maintaining under a naturalizing theological frame?',
    'This is not resolvable by data internal to the tradition since the tradition''s own authority structure adjudicates the question; cross-tradition comparison (other societies'' status hierarchies that made similar naturalizing claims and were later reformed or abolished) and beneficiary-concentration analysis (whether benefit tracks caste position as closely as the reading''s cosmological claim would predict if the hierarchy were function-based rather than birth-based) can inform but not fully settle it.',
    'If cosmic order, the high extraction and suppression metrics describe a structure this reading itself would not regard as extraction (a caste''s exit is not something a genuine cosmic order owes it); if constructed-with-beneficiaries, the metrics describe exactly the false-summit pattern of naturalized extraction, and the snare classification with concentrated beneficiary (Brahmin caste) is the structurally accurate reading rather than a hostile external framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmic_order_naturalness_vs_construction, conceptual, 'Whether the hierarchy is genuine cosmic order or naturalized, beneficiary-concentrated social construction — the central interpretive fault line this reading occupies.').

omega_variable(
    enforcement_source_ambiguity,
    'How much of the measured suppression (0.88) is sustained by formal legal/political enforcement historically (state-backed caste law, ruler enforcement of Dharmashastra) versus informal social enforcement (ostracism, ritual exclusion, community sanction) that persists independent of and after formal legal abolition?',
    'Compare enforcement mechanisms across periods where formal state backing existed (pre-colonial and colonial-era caste law) against the post-1950 constitutional period where untouchability is formally illegal in India but caste-based discrimination and violence continue to be documented — the persistence gap measures the informal/social component.',
    'If suppression is substantially informal/social rather than formal-legal, the constraint''s persistence is better modeled as internalized-plus-social rather than purely coercive-legal, which affects what kind of intervention (legal reform vs. social/educational change) would actually reduce it; the rising theater_ratio trajectory in this story''s measurements is offered as one signal that enforcement is shifting from formal to informal registers over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_source_ambiguity, empirical, 'Split between formal-legal and informal-social components of the measured suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vedi_tr_t40, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(vedi_tr_t80, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement(vedi_tr_t120, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 120, 0.22).
narrative_ontology:measurement(vedi_tr_t160, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 160, 0.27).
narrative_ontology:measurement(vedi_tr_t200, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 200, 0.3).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(vedi_be_t40, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(vedi_be_t80, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 80, 0.8).
narrative_ontology:measurement(vedi_be_t120, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 120, 0.83).
narrative_ontology:measurement(vedi_be_t160, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 160, 0.85).
narrative_ontology:measurement(vedi_be_t200, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 200, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(vedi_su_t40, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(vedi_su_t80, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 80, 0.78).
narrative_ontology:measurement(vedi_su_t120, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 120, 0.83).
narrative_ontology:measurement(vedi_su_t160, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 160, 0.86).
narrative_ontology:measurement(vedi_su_t200, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 200, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__orthodox_varna_reading, 0.08).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the single natural-language label 'the Vedic corpus on social order' per the ε-invariance principle. orthodox_varna_reading (this story, snare, ε=0.86) claims literal, cosmically-mandated prescriptive content with a concentrated beneficiary (Brahmin caste) and a large victim set (Shudra/Dalit/avarna groups). reformist_spiritual_reading denies prescriptive social content entirely, reading the same texts as metaphorical cosmology — under that reading ε approaches zero and no victim set exists. colonial_orientalist_reading treats the corpus as a unified administrative law code, an artifact of British-era codification for governance purposes, which is a claim about textual unity and legal-administrative function rather than theological content, with its own distinct beneficiary structure (colonial administrators, and post-colonial legal institutions that inherited codified 'Hindu law'). The three readings are linked here via affects_constraints because the orthodox reading's institutional dominance historically influenced which textual strata the colonial codification effort selected and canonized, and because reform movements' arguments are structured as direct rebuttals of the orthodox reading's textual claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
