% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__colonial_orientalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__colonial_orientalist_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__colonial_orientalist_reading
 *   human_readable: Colonial Codification of 'Hindu Law' from Dharmashastra Texts
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   Beginning in the late 18th century, colonial administrators and
 *   Orientalist scholars (Halhed, Jones, Colebrooke, and successors) treated
 *   Dharmashastra texts as evidence of a unified, timeless 'Hindu law'
 *   analogous to a civil code, commissioning digests and translations to
 *   render Indian legal practice administrable within a common-law-adjacent
 *   judicial system. This reading is one of three structurally distinct
 *   claims about the same textual corpus: the orthodox reading holds Varna
 *   hierarchy as divinely commanded cosmic order (a live theological claim
 *   within Hindu orthopraxy); the reformist reading holds the corpus is
 *   metaphorical/spiritual with no prescriptive social content (a live
 *   hermeneutic claim within reform and modernist traditions); this reading
 *   is the administrative-construction claim — that colonial and Orientalist
 *   actors treated a plural, contested, regionally variable textual and
 *   customary tradition as a single positive-law system for governance
 *   purposes, and that this construction (not any accurate transcription of a
 *   pre-existing unity) is what became enforceable law. The ε here (0.58)
 *   measures the extraction inherent in THIS administrative-legibility
 *   project as its own architects and beneficiaries experienced it — not the
 *   theological status of the underlying texts (addressed in the orthodox
 *   reading) and not their spiritual content (addressed in the reformist
 *   reading).
 *
 * KEY AGENTS:
 *   - colonial_administration: agenda-setter and primary institutional beneficiary — gains governability and revenue-assessment legibility
 *   - brahmin_pandit_informants: beneficiary — elevated to sole authoritative interpreters, gaining centralized institutional standing
 *   - anglo_indian_judiciary: agenda-setter/beneficiary — gains a stable citable rule system reducing judicial discretion burden
 *   - colonized_legal_subjects: primary payer — bound by a fixed code presented as immemorial with no forum to contest the premise
 *   - lower_caste_litigants and women: payers — lose flexibility and rights that existed under pre-codification customary plurality
 *   - regional_customary_communities: excluded — their practices are erased from the record entirely, not merely outvoted
 *   - orientalist_scholars: observer/agenda-setter — supply the philological apparatus that converts scholarly choice into administrative fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.58).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.62).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, scaffold).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Colonial Codification of 'Hindu Law' from Dharmashastra Texts").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "religious_studies/social_stratification/hermeneutics").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:has_sunset_clause(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, '047d2ad3-aa7f-470d-96a5-11641d20c514').
narrative_ontology:cs_kernel_codification('047d2ad3-aa7f-470d-96a5-11641d20c514', formalized).
narrative_ontology:cs_authority_grounding('047d2ad3-aa7f-470d-96a5-11641d20c514', extraction).
narrative_ontology:cs_interpretation_layer_present('047d2ad3-aa7f-470d-96a5-11641d20c514').
narrative_ontology:cs_reading_relation('047d2ad3-aa7f-470d-96a5-11641d20c514', vedic_corpus_social_prescription__orthodox_varna_reading, influences).
narrative_ontology:cs_reading_relation('047d2ad3-aa7f-470d-96a5-11641d20c514', vedic_corpus_social_prescription__reformist_spiritual_reading, coexists_with).
narrative_ontology:cs_axiom('047d2ad3-aa7f-470d-96a5-11641d20c514', foundational, textual_corpus_is_constructed_administrative_artifact).
narrative_ontology:cs_axiom_status(textual_corpus_is_constructed_administrative_artifact, holdable).
narrative_ontology:cs_axiom_grounding('047d2ad3-aa7f-470d-96a5-11641d20c514', textual_corpus_is_constructed_administrative_artifact, empirically_contingent).
narrative_ontology:cs_axiom('047d2ad3-aa7f-470d-96a5-11641d20c514', secondary, colonial_codification_serves_governability_not_fidelity).
narrative_ontology:cs_axiom_status(colonial_codification_serves_governability_not_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('047d2ad3-aa7f-470d-96a5-11641d20c514', colonial_codification_serves_governability_not_fidelity, instrumental).
narrative_ontology:cs_reference_frame('047d2ad3-aa7f-470d-96a5-11641d20c514', precolonial_plural_customary_order).
narrative_ontology:cs_drift_state('047d2ad3-aa7f-470d-96a5-11641d20c514', postcolonial_personal_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('047d2ad3-aa7f-470d-96a5-11641d20c514', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, brahmin_pandit_informants).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, anglo_indian_judiciary).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, lower_caste_litigants).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, women_under_codified_personal_law).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, regional_customary_communities).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, hindu_legal_unity_doctrine).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, textual_supremacy_over_custom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commissions translation and digest projects (Halhed, Jones, Colebrooke) to render a legible, administrable body of 'Hindu law' out of a vast, regionally variable, often orally-transmitted textual and customary tradition. Uses the resulting codified law to adjudicate disputes, assess revenue liability tied to caste-linked land tenure, and govern indirectly through recognized native categories. Collects governance legitimacy and administrative efficiency from treating the codification as discovery of a pre-existing timeless system rather than as construction.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).

% Serve as the exclusive authoritative interpreters consulted by colonial courts and compilers. Their particular textual tradition and caste-favorable readings of Dharmashastra are elevated to the status of pan-Indian law, displacing local custom, oral tradition, and rival juridical schools. Gain institutional standing and enforcement power over interpretation they did not previously hold in this centralized form.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, brahmin_pandit_informants, beneficiary,
    organized, generational, constrained, regional).

% Applies the codified digests (e.g., Manu-derived compilations) as positive law in civil courts, substituting textual citation for the diverse, negotiated, locally adjudicated practices that previously governed inheritance, marriage, and caste disputes. Benefits from a stable, citable, precedent-generating rule system that reduces judicial discretion burden and colonial administrative uncertainty.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, anglo_indian_judiciary, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, anglo_indian_judiciary, beneficiary).

% Are newly bound by a fixed textual code presented as immemorial and universal, even where it displaces more favorable or more flexible local practice. Have no forum to contest the premise that the digest accurately represents 'their' law; the court simply applies it. Exit from the codified system means exit from legal recognition altogether.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects, payer,
    powerless, biographical, trapped, continental).

% Find that Brahmin-derived textual hierarchies, once one interpretive strand among many contested local orders, are now backed by state enforcement power in matters of land, marriage, and status. Practices of caste mobility, occupational flexibility, and local status negotiation that existed before codification become harder to sustain once a court can cite a fixed text against them.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, lower_caste_litigants, payer,
    powerless, biographical, trapped, regional).

% Are subjected to the most restrictive strands of Dharmashastra text (on inheritance, widow remarriage, and marital property) selected and stabilized as 'the law,' overriding matrilineal, regional, or customary practices that had granted broader rights in some communities prior to codification.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, women_under_codified_personal_law, payer,
    powerless, generational, trapped, continental).

% Practiced localized, orally transmitted, often syncretic norms of kinship, land tenure, and dispute resolution that varied significantly by region and community. Their practices are not consulted in the codification process and are treated as deviations from the 'true' textual law rather than as independent, equally valid legal orders.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, regional_customary_communities, excluded,
    powerless, generational, trapped, regional).

% Produce the philological and translation apparatus (grammars, digests, comparative law treatises) that supplies the intellectual authority for treating Dharmashastra as a unified legal corpus analogous to Roman or English law. Their scholarly framing choices — which texts to privilege, how to resolve internal contradictions — become administrative fact once adopted by the colonial state.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars, observer,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__colonial_orientalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the colonial state with a stable, citable, precedent-based legal category ('Hindu law') that can be administered uniformly across a vast and linguistically/regionally diverse territory, reducing the transaction cost of case-by-case adjudication of local custom.
% TRANSFER_FUNCTION: Moves interpretive and enforcement authority from diffuse local, customary, and orally-negotiated legal orders to a centralized textual canon selected and stabilized by colonial scholars and Brahmin informants; moves substantive rights (inheritance shares, marriage terms, status mobility) away from those who benefited from flexible customary practice toward whichever textual reading was codified as authoritative.
% ABSENT_VOICES: Regional customary communities, lower-caste practitioners with non-Brahminical legal traditions, and women whose rights under matrilineal or regional custom exceeded the codified textual norm were not consulted in the compilation process; their objections surface later, in colonial-era social reform movements and in post-independence legal history, but not in the original codification.
% DISAPPEARANCE_RATIONALE: If the codified 'Hindu law' apparatus had never been constructed, adjudication would have remained closer to the plural, negotiated, locally-variable system of customary and textual authority that existed before colonial administration — courts would have had to engage directly and repeatedly with local practice rather than citing a fixed digest, and caste/gender status would likely have retained more regional variability and mobility than the codified regime allowed.
% FOUNDING_PROBLEM: The East India Company and later Crown administration needed a governable, citable body of law to adjudicate disputes among Indian subjects without either applying English law wholesale or engaging in costly, unpredictable case-by-case investigation of local custom in every dispute.
% FOUNDING_PROBLEM_CORROBORATION: Postcolonial legal historians (outside both the colonial administration and the Brahmin informant class) — including scholars of colonial legal anthropology — attest that the administrative problem of governability was real but was resolved through a construction, not a discovery, and that the resulting codified corpus outlived any administrative necessity, persisting into independent India's personal law framework long after direct colonial governance ended. No corroboration from outside the colonial administration or its Brahmin interpretive partners supports the claim that a unified textual system existed prior to codification.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__colonial_orientalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__colonial_orientalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__colonial_orientalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate rather than extreme: the codification did solve a genuine administrative coordination problem (courts needed SOME citable basis for adjudication across a vast territory) but it solved that problem by manufacturing a false uniformity that systematically favored certain textual strands (Brahminical, patriarchal, hierarchical) over others, and backed that selection with state coercive power. Suppression (0.62) reflects that once codified, the alternative — engaging directly with plural local custom — was not merely disfavored but structurally foreclosed: courts cited digests, not communities. Theater ratio (0.40) captures that a substantial share of the codification's legitimacy rested on the performance of scholarly 'discovery' rather than genuine philological neutrality — translators and compilers selected among contradictory textual strands and presented the result as recovery of a preexisting system. Accessibility collapse (0.50) and resistance (0.45) are moderate: customary and reformist alternatives persisted in social practice and later reform movements (Arya Samaj, Brahmo Samaj, and eventually the postcolonial state's own reform of personal law) even as the codified reading dominated formal adjudication — the constraint never achieved full closure.
 *
 * PERSPECTIVAL GAP:
 *   From the colonial administration and Anglo-Indian judiciary's seat, this looks like rope or even mountain-adjacent — 'discovering' and applying a pre-existing legal system, a neutral administrative technology. From the payer seats (colonized subjects, lower-caste litigants, women, and excluded customary communities), the same structure operates as an enforced, extractive scaffold — a temporary administrative expedient (the founding problem: governability at scale) that outlived its administrative necessity and calcified into a durable social-stratification apparatus, one whose sunset never arrived even after direct colonial rule ended.
 *
 * DIRECTIONALITY LOGIC:
 *   Colonial administration and Anglo-Indian judiciary sit near the beneficiary end: they set the codification agenda, hold institutional power, and can exit the arrangement's costs (they are not bound by the code themselves in the way subjects are) — d is low. Brahmin pandit informants are beneficiaries with moderate lock-in: their institutional gain is real but their own tradition's boundaries are also stabilized (possibly not to their liking in all respects) — treated here as beneficiaries per their structural gain in interpretive authority. Colonized legal subjects, lower-caste litigants, and women under codified personal law are trapped targets: no meaningful exit from the legal system that governs their status, inheritance, and civil standing — d is high. Regional customary communities are excluded rather than merely extracted from — their practices are not weighed and rejected, they are simply absent from the record, which is why they carry role 'excluded' rather than 'payer,' though the practical effect on them is also extractive (their prior legal standing is erased).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative governability of a plural legal landscape without case-by-case investigation) was real and finite in scope; the codified apparatus that solved it has persisted, largely unrevised in its structural premises, well past the end of colonial administration — into independent India's personal law system. Classifying this as scaffold-with-unfulfilled-sunset (rather than rope) prevents mislabeling an enduring, extraction-generating institutional structure as ongoing legitimate coordination; classifying it as scaffold rather than pure snare preserves the genuine (if narrow) coordination function it once served, which a snare classification would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_construction_vs_genuine_discovery,
    'Did the colonial-era digests substantially construct a new legal uniformity, or did they largely transcribe a pre-existing (if regionally inflected) shared textual-legal tradition that already had cross-regional authority among literate elites?',
    'Comparative textual-historical analysis of pre-colonial legal practice records (inscriptions, local court records, pre-19th-century commentarial literature) against the codified digests, assessing how much the digests diverge from documented pre-colonial adjudicative practice versus how much they align with elite Sanskritic tradition that predates colonial contact.',
    'If largely transcription of an already-dominant elite tradition, this reading''s extractiveness should be revised downward toward rope (genuine, if imperfect, coordination around an existing authority); if substantially novel construction serving administrative legibility, the scaffold/high-extraction reading is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_construction_vs_genuine_discovery, empirical, 'Whether codification was substantial invention or largely transcription of pre-existing elite consensus.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three readings of the Vedic/Dharmashastra kernel (colonial-administrative, orthodox-theological, reformist-spiritual) genuinely about the same textual corpus, or does the label ''Vedic/Dharmashastra texts'' itself conflate distinct textual strata (Samhita/Upanishad vs. Dharmashastra proper) that different readings implicitly privilege?',
    'Textual-critical mapping of which specific texts (Rigveda, Upanishads, Manusmriti, regional Dharmashastra commentaries) each reading actually draws on, to test whether the apparent disagreement is a disagreement about the same texts or a disagreement produced by each reading silently selecting a different textual subset.',
    'If the readings systematically select different textual strata, the kernel itself may require further decomposition into corpus-specific constraints rather than three readings of one corpus — this would be a second-order ε-invariance test on the kernel boundary itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three sibling readings share a genuine common textual referent or silently select different sub-corpora.').

omega_variable(
    postcolonial_persistence_beneficiary_shift,
    'After independence, did the beneficiary of the codified personal-law system shift from colonial administration to the postcolonial Indian state and its own elite legal/political class, or did the original beneficiary structure (Brahminical interpretive authority) simply continue unchanged under new sponsorship?',
    'Analysis of postcolonial personal-law reform debates (Hindu Code Bill 1950s) and who advocated for versus against retaining codified colonial-era categories, tracking whether reform served to dismantle or entrench the colonial-era codification.',
    'Clarifies whether the scaffold''s sunset failure is a colonial-era-specific extraction that should have ended in 1947, or whether it was absorbed into a new extractive arrangement with a different beneficiary set, which would argue for treating post-1947 personal law as a distinct downstream constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(postcolonial_persistence_beneficiary_shift, empirical, 'Whether the beneficiary of codified ''Hindu law'' changed at independence or merely changed sponsors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vedi_tr_t40, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(vedi_tr_t80, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 80, 0.33).
narrative_ontology:measurement(vedi_tr_t120, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 120, 0.36).
narrative_ontology:measurement(vedi_tr_t160, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 160, 0.38).
narrative_ontology:measurement(vedi_tr_t200, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 200, 0.4).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vedi_be_t40, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(vedi_be_t80, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 80, 0.5).
narrative_ontology:measurement(vedi_be_t120, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 120, 0.55).
narrative_ontology:measurement(vedi_be_t160, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 160, 0.57).
narrative_ontology:measurement(vedi_be_t200, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 200, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vedi_su_t40, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(vedi_su_t80, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(vedi_su_t120, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 120, 0.6).
narrative_ontology:measurement(vedi_su_t160, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 160, 0.61).
narrative_ontology:measurement(vedi_su_t200, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 200, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__colonial_orientalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.1).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_census_caste_enumeration).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the vedic_corpus_social_prescription kernel, decomposed per the epsilon-invariance principle: colonial_orientalist_reading (this file, moderate-epsilon scaffold, administrative-construction claim), orthodox_varna_reading (theological claim about divinely mandated cosmic order), and reformist_spiritual_reading (hermeneutic claim about metaphorical/spiritual content with no prescriptive force). Each carries its own stable epsilon and must not be averaged or reconciled with the others. This reading also structurally influences colonial_census_caste_enumeration, since the codified legal categories fed directly into census caste classification practices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
