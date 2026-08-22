% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Orthodox Varna Reading: Divinely Mandated Fourfold Social Order
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   vedic_corpus_social_prescription: the orthodox_varna_reading, on which
 *   the Vedic corpus literally prescribes a four-fold hierarchy of priest,
 *   warrior, producer, and servant as divinely mandated cosmic order, with
 *   birth-fixed duty (svadharma), endogamous marriage, and graduated ritual
 *   access. The epsilon referent is the standing arrangement under contest:
 *   the hereditary hierarchy as actually enforced across the subcontinent
 *   under this reading's warrant, not any alternative arrangement this or
 *   another reading would install. The claim/metric independence rule is
 *   load-bearing here: the reading's own rhetoric asserts naturality (divine,
 *   cosmic, eternal), while the authored metrics describe the arrangement's
 *   actual operation, which is constructed, actively enforced, and borne
 *   unevenly. Had this been authored as a mountain claim with beneficiaries,
 *   the false-summit signature would fire; the structural data support snare,
 *   and that is the claim authored. Per the epsilon-invariance principle, the
 *   colloquial label 'the Vedic varna system' decomposes into three
 *   structurally distinct constraints (this file and the two sibling
 *   readings), linked through network.affects_constraints; their epsilon
 *   values differ widely and must never be averaged.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_caste: Primary beneficiary and agenda-setter (institutional/identity_locked) — transmits the corpus, administers ritual, defines dharma, collects fees and grant income
 *   - kshatriya_ruling_caste: Secondary beneficiary (powerful/constrained) — purchases legitimation of rule with patronage, defends the order that ranks it second
 *   - vaishya_producer_castes: Intermediate payer-beneficiary (moderate/constrained) — funds the upper ranks, retains property and ritual access denied below
 *   - shudra_service_castes: Primary target (powerless/trapped) — bears service obligations, barred from study and initiation
 *   - dalit_untouchable_laborers: Extreme target (powerless/trapped) — outside the four-fold scheme entirely, bears polluting labor, excluded from the interpretive conversation
 *   - buddhist_jain_renouncer_traditions: Excluded rival (organized/mobile) — offers the priced exit, marginalized as heterodox
 *   - critical_hermeneutics_scholars: Analytical observer (analytical/analytical) — sees the whole structure, bears none of its costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.76).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.64).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Orthodox Varna Reading: Divinely Mandated Fourfold Social Order").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious_studies/social_stratification/hermeneutics").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, '2b89d049-b052-4bb4-b4b8-67f4e7a3243f').
narrative_ontology:cs_kernel_codification('2b89d049-b052-4bb4-b4b8-67f4e7a3243f', fixed_text).
narrative_ontology:cs_authority_grounding('2b89d049-b052-4bb4-b4b8-67f4e7a3243f', lineage).
narrative_ontology:cs_interpretation_layer_present('2b89d049-b052-4bb4-b4b8-67f4e7a3243f').
narrative_ontology:cs_reading_relation('2b89d049-b052-4bb4-b4b8-67f4e7a3243f', vedic_corpus_social_prescription__reformist_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b89d049-b052-4bb4-b4b8-67f4e7a3243f', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('2b89d049-b052-4bb4-b4b8-67f4e7a3243f', foundational, veda_is_binding_social_injunction).
narrative_ontology:cs_axiom_status(veda_is_binding_social_injunction, holdable).
narrative_ontology:cs_axiom_grounding('2b89d049-b052-4bb4-b4b8-67f4e7a3243f', veda_is_binding_social_injunction, theological).
narrative_ontology:cs_axiom('2b89d049-b052-4bb4-b4b8-67f4e7a3243f', secondary, varna_duty_fixed_by_birth).
narrative_ontology:cs_axiom_status(varna_duty_fixed_by_birth, holdable).
narrative_ontology:cs_axiom_grounding('2b89d049-b052-4bb4-b4b8-67f4e7a3243f', varna_duty_fixed_by_birth, theological).
narrative_ontology:cs_reference_frame('2b89d049-b052-4bb4-b4b8-67f4e7a3243f', apaurusheya_varna_cosmic_order).
narrative_ontology:cs_drift_state('2b89d049-b052-4bb4-b4b8-67f4e7a3243f', post_ambedkar_constitutional_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2b89d049-b052-4bb4-b4b8-67f4e7a3243f', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priestly_caste).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_ruling_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_service_castes).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_untouchable_laborers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_producer_castes).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_ruling_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_producer_castes).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, purusha_sukta_cosmogony).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, karma_rebirth_theodicy).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, svadharma_birth_determined_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Memorizes and transmits the Vedic corpus through closed lineages, performs and mediates ritual for every household, and defines duty through the commentarial schools. Receives ritual fees, endowment income, tax-free land grants, and first-place honors at every ceremony. Leaving ritual office would sever the lineage identity that constitutes status; the office and the self are the same inheritance.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priestly_caste, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priestly_caste, beneficiary).

% Holds military and administrative power and patronizes the ritual order that consecrates and legitimates rule. Funds major sacrifices, endows priests, and in exchange receives sacred warrant for taxation and command. Dynastic standing runs through the legitimation pipeline, so the dynasty defends an order that ranks it second to its own chaplains.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_ruling_caste, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_ruling_caste, payer).

% Produces, trades, and pays the taxes and ritual gifts that fund the top two ranks. Keeps property rights and household ritual access that the lowest ranks are denied, and can occasionally convert wealth into status, but occupation and marriage remain boxed by birth. Exit would mean abandoning the commercial networks and marriage pool that constitute livelihood.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_producer_castes, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_producer_castes, beneficiary).

% Carries the service obligations of the entire order: farm labor, artisan production, and domestic service for the twice-born. Barred from Vedic study on stated penalty and from the initiation rite that marks full membership. Status passes to children at birth, marriage is confined within the community, and departure means losing kin, livelihood, and ritual standing simultaneously.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_service_castes, payer,
    powerless, generational, trapped, continental).

% Performs labor the ritual order classes as polluting: scavenging, leatherwork, corpse handling. Lives in segregated settlements, is denied temple entry and access to shared wells, and sits outside the four-fold scheme altogether. Duties and disabilities are inherited at birth and enforced by village-level social sanction; the texts that define this position were never permitted to be heard by the people they bind.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_untouchable_laborers, payer,
    powerless, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_untouchable_laborers, excluded).

% Offers an exit the household order cannot price: ordination suspends caste obligation and installs the entrant in a new community with its own career ladder. Flourished whenever royal patronage shifted away from the Brahmanical establishment and contracted when it returned; the orthodoxy answers by treating departure as fall from duty and barring honorable return.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, buddhist_jain_renouncer_traditions, excluded,
    organized, biographical, mobile, continental).

% Reads the corpus philologically and comparatively, traces the historical construction of the hierarchy across the manuscript record, and publishes assessments of how much prescriptive social content the earliest layers actually contain. Holds no ritual office and bears none of the domestic costs, which is what permits a view of the whole structure.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, critical_hermeneutics_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_priestly_caste).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__orthodox_varna_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns occupational, ritual, and marital roles across a stratified agrarian society: priestly mediation of rites, military-administrative protection and governance, production and trade, and service labor, with hereditary transmission keeping each function continuously staffed and interdependence between ranks stabilized.
% TRANSFER_FUNCTION: Moves labor service, agricultural surplus, ritual fees, and tax-free land-grant income from Shudra and Dalit laborers (and taxed Vaishya production) upward to Brahmin ritual specialists and ruling elites; moves legitimation and ritual validation downward from Brahmins to rulers.
% ABSENT_VOICES: Those governed by the prescription were barred from the conversation that authored it: Shudras and Dalits were traditionally denied access to Vedic recitation and study, so the people whose duties the texts prescribe had no seat in interpreting or contesting them. Renouncer traditions objected from outside the fold and were classed as heterodox for doing so.
% DISAPPEARANCE_RATIONALE: Occupational guilds, marriage networks, land-labor tenures, temple economies, and royal legitimation all route through the hereditary hierarchy. Overnight removal would force wholesale reorganization of rural labor relations, ritual patronage, and kinship exchange across the subcontinent; nothing currently stands ready to replace the staffing and legitimation functions it monopolizes.
% FOUNDING_PROBLEM: Consolidating a settled agro-pastoral society's division of labor and integrating immigrant Indo-Aryan lineages with indigenous populations under a single ritual-political order, while supplying rulers with a sacred warrant and every function with a hereditary staffing mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of ancient India, writing from outside the beneficiary set, attest that the hierarchy consolidated gradually as landed and priestly elites hardened earlier fluid distinctions, and that the 'timeless unified prescription' is a retrospective construction. Dalit intellectual traditions attest from outside that the order operated as labor bondage rather than organic function. Only orthodox custodians attest the founding problem as eternal and fully live; no source outside the benefiting parties corroborates the eternal-necessity version, while the weaker claim that some coordination of functional specialization was needed is corroborated by mainstream historiography.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.76 at interval end) because the arrangement transfers labor service, surplus, and ritual income upward while the transferring seats receive no commensurate return; the rate is fixed by birth, not by contribution or consent. Suppression (0.64, raw and unscaled — only extractiveness is scaled by directionality and scope in the engine's computation) reflects dependence on active enforcement: purity-pollution sanction, endogamy policing, denial of study and ritual access, and village-level penalty. Accessibility_collapse (0.68) is high but not total: within the household order, alternatives collapse almost completely, yet renunciation, conversion, and migration remained real if costly exits, which is what keeps this below mountain-grade closure. Resistance (0.62) records three millennia of multi-front challenge: the renouncer movements, bhakti egalitarianism, and the modern anti-caste and constitutional struggles. Theater_ratio reaches 0.50 at the endpoint, crossing the Goodhart threshold: contemporary defense of the order increasingly invokes scripture while the practice it describes has attenuated, so a growing share of maintenance activity is performative. The temporal series run on one shared grid (eight points, every tracked metric authored at every point) so no end-state value is silently substituted into earlier rows. The suppression_requirement series is authored deliberately: enforcement capacity is the dynamic this story tracks, rising through Dharmashastra codification and medieval consolidation, peaking in the late pre-colonial period, and falling after constitutional abolition shifted enforcement from state and temple to society — a decay of official enforcement with social enforcement persisting. Coalition check: the classic objection that numerically overwhelming victim classes should dissolve the arrangement by coalition is answered by graded inequality — each rank's small advantage over the rank below purchases its enforcement labor, fragmenting vertical coalition (Ambedkar's diagnosis), which is precisely the stability mechanism a multi-victim snare requires.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different types from identical structural data. From the brahmin_priestly_caste seat the arrangement is a self-perpetuating sacred order it staffs and administers — directionality near the beneficiary end, effective burden near zero or negative. From the dalit_untouchable_laborers and shudra_service_castes seats the same structure is a birth-fixed servitude with no exit — directionality near the full-target end, amplified by trapped exit options and by continental scope, which makes verification of abuse harder and effective burden higher. The kshatriya_ruling_caste seat experiences subsidized rule; the vaishya seat experiences a mixed ledger; the renouncer seat, holding mobile exit, experiences the arrangement mainly as a boundary to cross. Identity-lock binds the priestly seat institutionally: the lineage is the office, so the agent that could most easily redesign the arrangement is also the agent least able to imagine itself apart from it. The engine computes this divergence from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (brahmin_priestly_caste, kshatriya_ruling_caste) drive those seats toward the beneficiary end; victim declarations (shudra_service_castes, dalit_untouchable_laborers) drive those seats toward the full-target end, with trapped exit pushing them further along it. One override is authored: the moderate power atom (occupied in this story solely by vaishya_producer_castes) is set to d=0.55. The derivation chain has no structural data for this seat — it appears in neither the beneficiary nor the victim arrays — so it would fall to the canonical fallback, which would misplace a genuinely dual-positioned actor: the vaishya seat pays the taxes and gifts that fund the upper ranks while retaining property and ritual rights denied below. The override encodes that mixed ledger; the commentary documents why the derivation alone would err.
 *
 * MANDATROPHY ANALYSIS:
 *   Decomposition is what prevents mislabeling here. Undecomposed, 'the Vedic varna system' would average a near-zero-epsilon spiritual-metaphorical reading with a high-epsilon enforcement reading and fabricate a mid-range epsilon that matches neither — a synthetic constraint belonging to no party. With one reading per file, epsilon stays invariant and the engine can compute per-seat divergence honestly. On the genealogy interview: founding_problem_status is contested (not dead), so the dead-mandate-plus-world_rearranges mismatch flag does not fire; the arrangement's mandate is disputed, not obsolete — extraction persists amid live contestation, which is the snare condition rather than the piton condition. The theater_ratio endpoint (0.50) marks where performative maintenance begins to dominate, the leading indicator that would eventually push this toward piton if official enforcement continues decaying faster than the social practice it once carried.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel vedic_corpus_social_prescription: the orthodox_varna_reading holds that the texts literally prescribe the hierarchy as divinely mandated. What happens to the victim set and beneficiary structure if a sibling reading governs instead?',
    'Philological assessment of prescriptive content in the earliest strata combined with reception history of how each reading acquired institutional carriers; the sibling files (reformist_spiritual_reading, colonial_orientalist_reading) carry their own structural data.',
    'Under the reformist reading the victim set empties entirely (no prescriptive social content, no enforceable duty hierarchy); under the orientalist reading extraction relocates to the colonial administrative apparatus. The high-epsilon profile is specific to THIS reading and must not be averaged across the family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel membership: classification is valid only for the orthodox literal-prescriptive reading, not the shared label.').

omega_variable(
    text_vs_practice_attribution,
    'Is the measured burden attributable to the Vedic prescriptive content itself, or to the post-Vedic elaboration (Dharmashastra codes, jati practice, medieval untouchability) erected under the texts'' authority and then attributed backward to them?',
    'Diachronic philology tracing each enforcement mechanism (endogamy rules, purity-pollution penalties, occupational disability) to its earliest textual warrant, separating shruti-layer injunction from smriti-layer codification and custom.',
    'If the burden is mostly post-Vedic accretion, the orthodox reading''s epsilon reflects its function as retroactive warrant rather than the content of the prescription; the reading then operates primarily as a legitimacy machine for practices it did not originate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_vs_practice_attribution, empirical, 'Whether epsilon belongs to the texts'' content or to the apparatus built beneath their authority.').

omega_variable(
    suppression_internalization_split,
    'How much of the measured suppression is structural (village sanction, denial of well and temple access, economic dependency) versus internalized (purity-pollution self-policing, karma-theodicy acceptance of birth status, sanskritization aspiration toward upper-caste norms)?',
    'Post-exit trajectory analysis: track communities that exited via conversion or urban migration; if purity anxieties, endogamous preference, and status deference persist across generations after structural barriers fall, the internalized share is substantial.',
    'If internalized, effective suppression exceeds the structural measure because targets carry the enforcement mechanism with them after exit; remediation aimed only at external barriers will underperform, and the constraint survives barrier removal in attenuated form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized shares of the suppression carrying the arrangement.').

omega_variable(
    gain_capture_concentration,
    'Does the brahmin_priestly_caste seat capture the arrangement''s gains, or are material gains split with landed upper-caste elites who take surplus through tenure rather than ritual?',
    'Historical accounting of receipt streams: ritual fees and dakshina, tax-free agrahara and brahmadeya land grants, temple endowments, and scribal-office income versus landlord surplus extraction under separate tenure arrangements.',
    'If landed elites capture the larger material share, the priestly seat captures chiefly status and ritual rents and the receipt surface is dual-headed; classification consequences follow the engine''s capture analysis rather than the single-seat assumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gain_capture_concentration, empirical, 'Concentration of gains in the priestly seat versus diffusion across landed upper castes.').

omega_variable(
    consent_under_theodicy,
    'Does karma-rebirth theodicy convert coerced position into apparently self-incurred position (birth rank as earned desert), and if so, does the resulting acquiescence reflect genuine consent or manufactured consent?',
    'Comparative analysis of acquiescence levels where the theodicy is taught versus where it is absent among structurally identical populations, plus documented defection rates at moments the theodicy loses explanatory grip (epidemic, conquest, market mobility).',
    'If the theodicy manufactures consent, part of the low observed revolt frequency is an artifact of the justification system rather than evidence of benign operation, and the arrangement''s stability is more coercively dependent than surface calm suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_under_theodicy, conceptual, 'Whether theodicy-mediated acquiescence counts as consent for classification purposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 0, 3500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vcsp_orthodox_tr_t0, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(vcsp_orthodox_tr_t0, observed).
narrative_ontology:measurement(vcsp_orthodox_tr_t500, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 500, 0.16).
narrative_ontology:measurement_basis(vcsp_orthodox_tr_t500, observed).
narrative_ontology:measurement(vcsp_orthodox_tr_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1000, 0.22).
narrative_ontology:measurement_basis(vcsp_orthodox_tr_t1000, observed).
narrative_ontology:measurement(vcsp_orthodox_tr_t1500, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1500, 0.28).
narrative_ontology:measurement_basis(vcsp_orthodox_tr_t1500, observed).
narrative_ontology:measurement(vcsp_orthodox_tr_t2000, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement_basis(vcsp_orthodox_tr_t2000, observed).
narrative_ontology:measurement(vcsp_orthodox_tr_t2500, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 2500, 0.38).
narrative_ontology:measurement_basis(vcsp_orthodox_tr_t2500, observed).
narrative_ontology:measurement(vcsp_orthodox_tr_t3000, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 3000, 0.4).
narrative_ontology:measurement_basis(vcsp_orthodox_tr_t3000, observed).
narrative_ontology:measurement(vcsp_orthodox_tr_t3500, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 3500, 0.5).
narrative_ontology:measurement_basis(vcsp_orthodox_tr_t3500, observed).

% Extraction over time
narrative_ontology:measurement(vcsp_orthodox_be_t0, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(vcsp_orthodox_be_t0, observed).
narrative_ontology:measurement(vcsp_orthodox_be_t500, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 500, 0.42).
narrative_ontology:measurement_basis(vcsp_orthodox_be_t500, observed).
narrative_ontology:measurement(vcsp_orthodox_be_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1000, 0.58).
narrative_ontology:measurement_basis(vcsp_orthodox_be_t1000, observed).
narrative_ontology:measurement(vcsp_orthodox_be_t1500, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1500, 0.7).
narrative_ontology:measurement_basis(vcsp_orthodox_be_t1500, observed).
narrative_ontology:measurement(vcsp_orthodox_be_t2000, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 2000, 0.76).
narrative_ontology:measurement_basis(vcsp_orthodox_be_t2000, observed).
narrative_ontology:measurement(vcsp_orthodox_be_t2500, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 2500, 0.82).
narrative_ontology:measurement_basis(vcsp_orthodox_be_t2500, observed).
narrative_ontology:measurement(vcsp_orthodox_be_t3000, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 3000, 0.84).
narrative_ontology:measurement_basis(vcsp_orthodox_be_t3000, observed).
narrative_ontology:measurement(vcsp_orthodox_be_t3500, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 3500, 0.76).
narrative_ontology:measurement_basis(vcsp_orthodox_be_t3500, observed).

% Suppression requirement over time
narrative_ontology:measurement(vcsp_orthodox_su_t0, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(vcsp_orthodox_su_t0, observed).
narrative_ontology:measurement(vcsp_orthodox_su_t500, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 500, 0.32).
narrative_ontology:measurement_basis(vcsp_orthodox_su_t500, observed).
narrative_ontology:measurement(vcsp_orthodox_su_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1000, 0.48).
narrative_ontology:measurement_basis(vcsp_orthodox_su_t1000, observed).
narrative_ontology:measurement(vcsp_orthodox_su_t1500, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1500, 0.62).
narrative_ontology:measurement_basis(vcsp_orthodox_su_t1500, observed).
narrative_ontology:measurement(vcsp_orthodox_su_t2000, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement_basis(vcsp_orthodox_su_t2000, observed).
narrative_ontology:measurement(vcsp_orthodox_su_t2500, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 2500, 0.8).
narrative_ontology:measurement_basis(vcsp_orthodox_su_t2500, observed).
narrative_ontology:measurement(vcsp_orthodox_su_t3000, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 3000, 0.84).
narrative_ontology:measurement_basis(vcsp_orthodox_su_t3000, observed).
narrative_ontology:measurement(vcsp_orthodox_su_t3500, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 3500, 0.64).
narrative_ontology:measurement_basis(vcsp_orthodox_su_t3500, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, resource_allocation).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, colonial_orientalist_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, dharmashastra_jati_codification).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the Vedic varna system' covers three structurally distinct claims. This file (orthodox_varna_reading) carries the high-epsilon enforcement arrangement with Shudra/Dalit victims and a Brahmin beneficiary seat. reformist_spiritual_reading dissolves the victim set (no prescriptive content, no enforceable duty hierarchy). colonial_orientalist_reading relocates extraction to the colonial administrative apparatus that codified the texts. The orthodox reading is upstream of both: its pandit-transmitted self-presentation supplied the legitimacy conditions and source material the orientalist codification consumed, and its institutional strength is what the reformist reading defines itself against. dharmashastra_jati_codification is the downstream enforcement apparatus this reading warrants. All edges are declared in both directions across the family files; no member is orphaned.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_corpus_social_prescription__orthodox_varna_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
