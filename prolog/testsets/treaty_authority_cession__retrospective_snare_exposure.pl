% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__retrospective_snare_exposure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__retrospective_snare_exposure, []).

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
 *   constraint_id: treaty_authority_cession__retrospective_snare_exposure
 *   human_readable: Treaty of Waitangi Textual Divergence as Covert Extraction Mechanism
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) presents as a bilateral agreement
 *   protecting both Crown governance authority and Māori property and
 *   political rights. In this reading, the constraint is the textual
 *   divergence itself—the fact that the Māori-language and English-language
 *   versions establish incompatible legal meanings for the same terms—and
 *   that divergence constitutes an extraction mechanism. Māori rangatira
 *   signed the Māori text understanding 'kāwanatanga' as limited governorship
 *   and 'tino rangatiratanga' as retained absolute chieftainship. The English
 *   text reads 'cession of sovereignty' and 'dominion' as absolute Crown
 *   control. The extraction operates covertly at the moment of signing: the
 *   rangatira could not understand themselves to be ceding what the English
 *   text claims they ceded, because they could not read or see that text. The
 *   extraction becomes visible only retrospectively, when Crown legislation
 *   (1860s–1970s) operationalizes the English reading and strips Māori of
 *   land and authority they understood themselves to have retained. The snare
 *   is that the textual divergence—a feature that appears to be a translation
 *   problem—becomes the legal mechanism for enforcing terms to which the
 *   Māori signatories never genuinely consented.
 *
 * KEY AGENTS:
 *   - Māori rangatira signatories: powerful actors at the time of signing, who understood themselves to be retaining mana and land through the Māori text; later discovered they had been understood (in English doctrine) to have ceded everything.
 *   - Māori iwi descendants: inheritors of the structural victimization; organized resistance to dispossession from the 1970s onward.
 *   - Crown land-purchasing and legislative apparatus: agenda-setter and beneficiary; controlled the English text, chose not to require Māori signatories to sign it, and later enforced it retroactively through legislation.
 *   - Settler colonists: beneficiaries who received land and security under the English-reading interpretation, largely unaware of the translation divergence.
 *   - Waitangi Tribunal and modern courts: observers and retrospective validators who documented the snare mechanism but cannot undo its effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.89).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.92).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.89).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty of Waitangi Textual Divergence as Covert Extraction Mechanism").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, '801ff578-bb8f-4883-8b01-f2e5745f04bb').
narrative_ontology:cs_kernel_codification('801ff578-bb8f-4883-8b01-f2e5745f04bb', fixed_text).
narrative_ontology:cs_authority_grounding('801ff578-bb8f-4883-8b01-f2e5745f04bb', extraction).
narrative_ontology:cs_interpretation_layer_present('801ff578-bb8f-4883-8b01-f2e5745f04bb').
narrative_ontology:cs_reading_relation('801ff578-bb8f-4883-8b01-f2e5745f04bb', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('801ff578-bb8f-4883-8b01-f2e5745f04bb', treaty_authority_cession__rangatiratanga_retention_reading, coexists_with).
narrative_ontology:cs_axiom('801ff578-bb8f-4883-8b01-f2e5745f04bb', foundational, textual_divergence_enables_coerced_consent).
narrative_ontology:cs_axiom_status(textual_divergence_enables_coerced_consent, holdable).
narrative_ontology:cs_axiom_grounding('801ff578-bb8f-4883-8b01-f2e5745f04bb', textual_divergence_enables_coerced_consent, deontological).
narrative_ontology:cs_axiom('801ff578-bb8f-4883-8b01-f2e5745f04bb', foundational, english_text_retroactive_enforcement_constitutes_betrayal).
narrative_ontology:cs_axiom_status(english_text_retroactive_enforcement_constitutes_betrayal, holdable).
narrative_ontology:cs_axiom_grounding('801ff578-bb8f-4883-8b01-f2e5745f04bb', english_text_retroactive_enforcement_constitutes_betrayal, deontological).
narrative_ontology:cs_reference_frame('801ff578-bb8f-4883-8b01-f2e5745f04bb', bilateral_consent_framework).
narrative_ontology:cs_drift_state('801ff578-bb8f-4883-8b01-f2e5745f04bb', enforcement_overrides_consent, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('801ff578-bb8f-4883-8b01-f2e5745f04bb', '2026-06-13T14:32:17Z').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_rangatira_signatories).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_iwi_descendants).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.89) because the constraint's operation transfers 95% of New Zealand land and supreme authority from Māori to Crown/settlers through an interpretation no Māori signatory could have understood at the time. Suppression is equally high (0.92) because the constraint's persistence depends on actively maintaining the English-text reading as binding law despite the Māori text's divergent terms. The enforcement machinery (courts, legislation, land court procedures) explicitly suppresses the Māori-text interpretation from the 1870s to the 1970s. Theater ratio is high initially (0.92 at t=0) because the constraint presents itself as a solemn bilateral agreement and mutual consent—pure theater, because the divergence makes mutual understanding impossible. Theater ratio declines over time (to 0.68 by t=184) as Waitangi Tribunal reports and Court of Appeal decisions expose the translation problem and the history of deliberate English-reading enforcement; the constraint's legitimacy narrative deteriorates as its mechanisms become visible. The measurement series tracks the constraint's lifecycle: at t=0 (signing), extraction is covert and theater is at maximum; by t=30 (1870s, after major confiscation acts), extraction becomes active and measurable; by t=130 (1970s), Waitangi Tribunal begins retrospective exposure; by t=184 (2024), theater has fallen as the snare mechanism is widely known, but suppression remains high because enforcement machinery (property law, settled title doctrines) continues to operationalize the English reading despite doctrinal shifts.
 *
 * PERSPECTIVAL GAP:
 *   The Crown and settler seats experience the constraint as legitimate law protecting property rights and establishing governance authority; the Māori seats experience it as a snare that stole what they understood themselves to have retained. The rangatira signatories at t=0 experienced no constraint at all—they understood themselves to be signing a partnership protecting their authority. Their descendants (t=184) experience the full extractive weight of the snare: dispossessed land, subordinated political authority, centuries of enforced legal subordination. The Waitangi Tribunal and modern courts occupy an intermediate position: they can document and validate the snare mechanism but cannot overturn the centuries of property transfers and legal entrenchment that depend on the English reading. The per-seat classification should diverge sharply: from the Crown institutional seat, the treaty is rope-grade coordination; from the Māori seat, it is snare-grade extraction; from the observer seat (judiciary), it is tangled_rope (coordination function stated and partly honored; extraction mechanism now visible and contested).
 *
 * DIRECTIONALITY LOGIC:
 *   Māori rangatira signatories and their descendants are full targets (d → 1.0): they are stripped of land and authority through a legal interpretation they never understood and could not have consented to. The Crown institutional apparatus is the agenda-setter and beneficiary (d → 0.0 for the Crown): it sets the terms, enforces the English reading retroactively, and collects the territorial benefit. Settler colonists are incidental beneficiaries (d → 0.3): they receive land and security but are largely unaware of the translation divergence and did not engineer it. The Waitangi Tribunal is an observer (d = 0.5 analytical): it has authority to investigate but no power to reverse property titles or legislation. The directionality should be structured as: Māori rangatira signatories (trapped, identity-locked to their iwi and lands) get d near 1.0; Crown institutional (powerful, arbitrage exit available to maintain English reading indefinitely) gets d near 0.0; settler colonists (organized, mobile if colonial arrangements were dismantled) get d around 0.3–0.4; courts/observers get d = 0.5 analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mutual security and consent-based governance) is DEAD. The constraint's founding mandate was to establish a framework where Crown governance authority and Māori property/authority could coexist. Instead, the constraint operationalizes to strip Māori of what the founding problem promised to protect. The mandate has been abandoned in the English-reading interpretation, which is enforced despite the Māori text's incompatibility. This is mandatrophy: the constraint persists long after its founding justification has been hollowed out, now sustained by property law entrenchment and political inertia rather than by any coherent governance logic. The Waitangi Tribunal's emergence (t=130) and subsequent Court of Appeal doctrinal shifts represent attempts to address mandatrophy, but they cannot undo the confiscations and land transfers completed under the false assumption of legitimacy. The snare becomes visible precisely because the mandate is dead: what looked like mutual agreement in 1840 is revealed to be coerced dispossession by 1974.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_of_textual_divergence,
    'Was the English–Māori textual divergence deliberately engineered by the Crown to create an undetectable snare, or was it an artifact of translation difficulty and time pressure, with the English-reading enforcement weaponizing an initially unintentional divergence?',
    'Historical analysis of Crown correspondence, missionary records, and the drafting process. The question hinges on whether Crown officials knew the English text diverged from the Māori text BEFORE the signing and chose not to require Māori signatories to sign the English version (deliberate snare) or whether the divergence was discovered only after the fact (opportunistic enforcement of an initially accident-prone divergence).',
    'If deliberate: the snare is intentional from t=0; the constraint is pure-snare from inception. If opportunistic: the snare emerges retrospectively as an enforcement mechanism; the constraint''s type at t=0 is different from its type at t=30 (snare emerges only as enforcement begins). This affects how the engine classifies the constraint''s founding period vs. its mature period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_of_textual_divergence, empirical, 'Whether the textual divergence was deliberately engineered or opportunistically weaponized.').

omega_variable(
    rangatira_linguistic_capacity,
    'To what extent could the rangatira signatories meaningfully evaluate the Māori-text translation at the time of signing? Did they understand ''kāwanatanga'' and ''tino rangatiratanga'' as translators intended, or did their own pre-existing concepts of mana and authority diverge from the missionary translation?',
    'Linguistic and ethnographic analysis of pre-1840 Māori political concepts and the rangatira''s own statements about their understanding. The Māori text may have been a translation that created new conceptual categories (''kāwanatanga'' as a neologism) that rangatira could not have fully grasped at the time.',
    'If rangatira linguistic capacity was constrained: even the Māori text becomes a partial snare—they understood something, but not the full legal meaning later attributed to it. If rangatira understood the Māori text clearly and diverged consciously from the English terms: the snare is purely in the English text and its later enforcement. This affects whether suppression operates on language barriers or on legal enforcement machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rangatira_linguistic_capacity, empirical, 'Whether Māori-text understanding was itself adequate or linguistically mediated by translation.').

omega_variable(
    contra_proferentem_validity,
    'Is the contra proferentem principle (ambiguity in a contract read against the drafter) a valid interpretive rule in colonial-era treaty law? If valid, does it apply to the Treaty of Waitangi such that the Māori text becomes controlling, or is Crown sovereignty doctrine understood to supersede general contract principles?',
    'Legal doctrine analysis and judicial precedent. The Waitangi Tribunal and Court of Appeal have increasingly adopted contra proferentem, but settled Crown law and property doctrine resist it. The question is whether this is a law-change (reading shifts the constraint''s legal status) or whether settled law permits the English reading to override contra proferentem.',
    'If contra proferentem applies and the Māori text becomes controlling: the Crown''s entire beneficiary position collapses; claims for land return and co-governance become legally sustainable; the constraint reclassifies from snare (under English reading) to tangled_rope (under Māori reading with enforcement resistance) or rope (if Māori text is treated as clear and uncontested). If Crown sovereignty doctrine supersedes contra proferentem: the English reading remains legally binding; the snare persists despite its exposure. This is a legal-doctrinal omega with direct classification consequence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contra_proferentem_validity, conceptual, 'Whether contra proferentem principle applies to the Treaty, shifting legal controlling text from English to Māori.').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent is Māori political subordination sustained by internalized acceptance of Crown authority (Māori law-abidingness, deference to settled Crown doctrine) vs. external legal enforcement (confiscation acts, land court procedures, property title registration)?',
    'Post-enforcement scenario analysis and decolonial psychology research. If Māori were to withdraw from Crown legal frameworks (declare autonomy, establish independent land management, reject Crown jurisdiction), what enforcement would face them? How much of the suppression persists after the external legal machinery is refused?',
    'If suppression is substantially internalized: the constraint''s effective suppression is higher than the structural enforcement measure suggests; the constraint carries forward even if external machinery is removed. If suppression is primarily external: removing enforcement machinery (e.g., judicial recognition of Māori sovereignty over Māori lands) would substantially weaken the constraint. This affects whether the constraint is truly snare (identity-locked victims unable to exit) or whether it is maintained primarily by institutional coercion (and thus vulnerable to institutional change).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Internalized vs. external suppression mechanism in maintaining Crown authority over Māori subjects.').

omega_variable(
    crown_vs_settler_beneficiary_divergence,
    'Are the Crown institutional apparatus and settler colonists genuinely aligned beneficiaries, or do they have divergent interests in the treaty''s interpretation?',
    'Analysis of property litigation and political positions: settlers benefit from absolute cession and inalienable Crown title; the Crown benefits from maintaining both Crown sovereignty AND negotiation flexibility with Māori over settlement terms. In recent settlement negotiations (1990s–2024), Crown has sometimes acknowledged Māori text primacy to enable settlements; this diverges from settler-property-rights maximalism. The question is whether the snare persists because both Crown and settlers want it to, or whether Crown could unwind it if property-rights entrenchment were less politically costly.',
    'If beneficiary interests diverge: the Crown is not a monolithic beneficiary but an agenda-setter trying to manage Crown sovereignty doctrine while settlers press for absolute cession and inalienable titles. This affects whether mandatrophy applies: Crown abandonment of the English-reading snare might be possible if political will shifts, whereas settler property-rights entrenchment creates a separate structural barrier. If interests are aligned: the snare is sustained by two institutional forces moving together, making it more durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_vs_settler_beneficiary_divergence, empirical, 'Whether Crown institutional and settler-colonist beneficiary interests remain aligned or diverge over treaty interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 0, 184).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t0, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0, 0.92).
narrative_ontology:measurement(trea_tr_t10, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 10, 0.88).
narrative_ontology:measurement(trea_tr_t30, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 30, 0.81).
narrative_ontology:measurement(trea_tr_t60, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 60, 0.75).
narrative_ontology:measurement(trea_tr_t130, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 130, 0.7).
narrative_ontology:measurement(trea_tr_t184, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 184, 0.68).

% Extraction over time
narrative_ontology:measurement(trea_be_t0, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(trea_be_t10, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(trea_be_t30, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(trea_be_t60, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 60, 0.85).
narrative_ontology:measurement(trea_be_t130, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 130, 0.88).
narrative_ontology:measurement(trea_be_t184, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 184, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t0, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(trea_su_t10, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 10, 0.76).
narrative_ontology:measurement(trea_su_t30, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(trea_su_t60, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 60, 0.9).
narrative_ontology:measurement(trea_su_t130, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 130, 0.92).
narrative_ontology:measurement(trea_su_t184, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 184, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__retrospective_snare_exposure, 0.15).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, maori_land_dispossession_confiscation).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, settler_property_title_entrenchment).

% DUAL FORMULATION NOTE:
% The Treaty of Waitangi kernel is instantiated across three constraint stories: crown_cession_reading (Mountain or Rope, English-text interpretation, treats treaty as absolute Crown sovereignty), rangatiratanga_retention_reading (Tangled Rope or Rope, Māori-text interpretation, treats treaty as limited Crown authority with Māori retention), and retrospective_snare_exposure (Snare, textual divergence as extraction mechanism covert at signing, visible retrospectively). The three readings have incompatible ε values (crown_cession: ε~0.1 as natural law or minimal coordination; retrospective_snare: ε~0.89 as extraction; rangatiratanga: ε~0.45 as contested coordination with enforcement resistance). They are not different observations of one constraint—they are different constraints instantiated from the same kernel, each with its own beneficiary/victim structure and temporal trajectory. Links: crown_cession influences retrospective_snare (the English reading enables the snare); retrospective_snare forecloses crown_cession (if the snare is exposed and remedied, the absolute-cession doctrine collapses); rangatiratanga_retention coexists with retrospective_snare (both Māori-text focused readings are live among different parties but have different emphases: retention focuses on ongoing Māori authority; exposure focuses on the snare mechanism itself).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__retrospective_snare_exposure, powerful, 0.92).
constraint_indexing:directionality_override(treaty_authority_cession__retrospective_snare_exposure, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
