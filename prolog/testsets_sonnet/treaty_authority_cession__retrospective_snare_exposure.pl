% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__retrospective_snare_exposure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: treaty_authority_cession__retrospective_snare_exposure
 *   human_readable: Treaty of Waitangi Textual Divergence as Extraction Mechanism (Retrospective Snare Exposure)
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This story instantiates the retrospective_snare_exposure reading of the
 *   treaty_authority_cession kernel: it treats the textual divergence between
 *   the English and Māori versions of te Tiriti o Waitangi not as a contested
 *   interpretive question (that is the domain of the crown_cession_reading
 *   and rangatiratanga_retention_reading, filed as separate sibling
 *   constraints) but as the extraction mechanism itself. The claim here is
 *   that a covert mistranslation — invisible or unrecognized as an operative
 *   gap at the moment of signing in 1840 — became visible only
 *   retrospectively, through 175 years of land confiscation, legislative
 *   override, and land-court title conversion executed by relying on the
 *   English text against parties who signed the Māori text. The beneficiary
 *   is the Crown land-purchasing apparatus and the settler administrative and
 *   property system built on the strength of the English text's cession
 *   claim; the victims are the Māori signatory hapū, their descendants, and
 *   even non-signatory iwi bound by the sovereignty doctrine once it was
 *   asserted. Unlike the two sibling readings, which each argue over which
 *   text SHOULD control, this reading is agnostic on that interpretive
 *   question and instead names the existence of an uncorrected divergence,
 *   exploited over time, as the constraint itself.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.88).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.79).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.88).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty of Waitangi Textual Divergence as Extraction Mechanism (Retrospective Snare Exposure)").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, '46d2137e-6d9e-4f11-8eba-fa225b87eb86').
narrative_ontology:cs_kernel_codification('46d2137e-6d9e-4f11-8eba-fa225b87eb86', fixed_text).
narrative_ontology:cs_authority_grounding('46d2137e-6d9e-4f11-8eba-fa225b87eb86', extraction).
narrative_ontology:cs_interpretation_layer_present('46d2137e-6d9e-4f11-8eba-fa225b87eb86').
narrative_ontology:cs_reading_relation('46d2137e-6d9e-4f11-8eba-fa225b87eb86', treaty_authority_cession__crown_cession_reading, influences).
narrative_ontology:cs_reading_relation('46d2137e-6d9e-4f11-8eba-fa225b87eb86', treaty_authority_cession__rangatiratanga_retention_reading, influences).
narrative_ontology:cs_axiom('46d2137e-6d9e-4f11-8eba-fa225b87eb86', foundational, divergence_itself_is_the_extraction_regardless_of_correct_text).
narrative_ontology:cs_axiom_status(divergence_itself_is_the_extraction_regardless_of_correct_text, holdable).
narrative_ontology:cs_axiom_grounding('46d2137e-6d9e-4f11-8eba-fa225b87eb86', divergence_itself_is_the_extraction_regardless_of_correct_text, empirically_contingent).
narrative_ontology:cs_axiom('46d2137e-6d9e-4f11-8eba-fa225b87eb86', foundational, consent_requires_comprehension_of_the_actual_terms_relied_upon).
narrative_ontology:cs_axiom_status(consent_requires_comprehension_of_the_actual_terms_relied_upon, holdable).
narrative_ontology:cs_axiom_grounding('46d2137e-6d9e-4f11-8eba-fa225b87eb86', consent_requires_comprehension_of_the_actual_terms_relied_upon, deontological).
narrative_ontology:cs_reference_frame('46d2137e-6d9e-4f11-8eba-fa225b87eb86', dual_text_signing_moment_1840).
narrative_ontology:cs_drift_state('46d2137e-6d9e-4f11-8eba-fa225b87eb86', post_waitangi_tribunal_te_paparahi_o_te_raki_2014, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('46d2137e-6d9e-4f11-8eba-fa225b87eb86', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, settler_colonial_administration).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, post_treaty_land_title_holders).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatory_hapu).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendants).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, non_signatory_iwi_bound_by_precedent).
narrative_ontology:constraint_vindicates(treaty_authority_cession__retrospective_snare_exposure, doctrine_of_crown_sovereignty_by_cession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted the English text asserting cession of sovereignty, commissioned the Māori translation using 'kāwanatanga' (governance) rather than a term for full sovereignty, circulated the Māori text for signature, and subsequently relied on the English text in courts, legislatures, and land courts to authorize confiscation, pre-emption purchase, and legislative override of Māori authority. Controls which text is treated as authoritative and has never borne the cost of the divergence it authored.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, beneficiary).

% Chiefs and their hapū read and signed te Tiriti o Waitangi in Māori, understanding themselves to be ceding kāwanatanga (governorship, a limited administrative authority) while retaining tino rangatiratanga (full chieftainship over lands, villages, and treasures). They had no access to the English text, no independent translation, and no capacity to negotiate wording once presented for signature at the moment of assent. The gap between what they signed and what was later enforced against them was invisible to them at the time and became visible only through later land confiscations, court rulings, and legislative acts they had no part in shaping.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_signatory_hapu, payer,
    powerless, generational, trapped, regional).

% Inherit the compounding consequences of land loss, legislative disempowerment, and the doctrine of Crown sovereignty asserted on the strength of an English text their ancestors never signed. They pursue redress through the Waitangi Tribunal and courts, but the extraction already executed — land alienation, resource loss, jurisdictional subordination — is largely irreversible even where the mistranslation is now formally acknowledged.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendants, payer,
    organized, civilizational, constrained, national).

% Some iwi never signed either text, yet the Crown's assertion of sovereignty derived from the treaty was applied to them regardless, via the doctrine that sovereignty, once asserted, extended uniformly. They bear the extraction of a mechanism they were never even nominally party to.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, non_signatory_iwi_bound_by_precedent, payer,
    moderate, generational, trapped, national).

% The colonial government and subsequent settler institutions built legislative, judicial, and land-title systems on the premise that the English text's sovereignty claim was valid and binding, using it to authorize the New Zealand Wars land confiscations, the Native Land Court's individualization of title, and a century of legislative override of Māori self-governance.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, settler_colonial_administration, beneficiary,
    institutional, civilizational, arbitrage, national).

% Settlers and their descendants who acquired land through Crown pre-emption purchases and post-confiscation grants hold title whose ultimate legal root traces to the sovereignty claim enabled by the English text. They benefit from the extraction without having personally negotiated or drafted the divergent texts.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, post_treaty_land_title_holders, beneficiary,
    organized, generational, mobile, national).

% A standing commission of inquiry established in 1975 (retrospectively empowered to hear claims from 1840) to investigate Crown breaches of the treaty's principles, including the textual divergence itself. It can recommend but historically could not compel full restitution; its findings document the mechanism this constraint names but operate downstream of the extraction already completed.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% The missionaries and officials who produced the rushed Māori translation overnight before the Waitangi signing had neither the time, terminological resources, nor structural incentive to render an exact equivalent for 'sovereignty' in te reo Māori, and their translation choices were not treated as a matter for negotiation with the chiefs who would sign the result. Their translation judgment, made under time pressure, became the instrument through which chiefs formed their actual understanding of what was signed — yet neither they nor the chiefs had any voice in reconciling the two resulting texts once both were in circulation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, translators_and_missionaries_1840, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At the moment of signing, te Tiriti o Waitangi purported to solve a genuine coordination problem: establishing a framework under which the Crown could exercise governance over increasing numbers of British settlers while Māori retained authority over their own lands, people, and customs — averting unregulated settlement and inter-hapū or settler-Māori conflict.
% TRANSFER_FUNCTION: The arrangement moves sovereignty, land, and governmental authority from Māori signatories and their descendants to the Crown and subsequent settler institutions, executed through reliance on the English text's cession language against parties who assented only to the Māori text's narrower kāwanatanga grant.
% ABSENT_VOICES: The translators and missionaries who produced the Māori text overnight had no seat in reconciling the divergence they created; the chiefs who signed had no access to independent legal or linguistic counsel; non-signatory iwi subsequently bound by the sovereignty claim were never in the room at all. Their absence is why the divergence went unnoticed and uncontested for decades — not because it was accepted, but because the extraction mechanism was constructed precisely where no party capable of objecting was present.
% DISAPPEARANCE_RATIONALE: If the Crown's reliance on the English-text sovereignty claim were retroactively voided in favor of the Māori text's narrower kāwanatanga grant, the legal foundation for confiscations, the Native Land Court's title conversions, and a century of legislative override would be structurally undermined — land title chains, constitutional doctrine, and the relationship between Crown and iwi governance would require wholesale reconstruction. This is precisely why the divergence remained legally 'settled' for so long: the world built on top of it is enormous.
% FOUNDING_PROBLEM: Regulate escalating, unlawful British settlement in New Zealand and establish a framework for coexistence between Māori authority and Crown governance before conflict became uncontainable.
% FOUNDING_PROBLEM_CORROBORATION: Historians (Claudia Orange, Ranginui Walker), the Waitangi Tribunal's own findings (particularly the 2014 Te Paparahi o Te Raki inquiry, which found the chiefs did not cede sovereignty), and linguistic analysis of the 1840 translation independently corroborate that the Māori text's founding problem — coexistence under limited Crown governance — remains substantively unresolved, while the Crown's own historical land-purchasing and legislative record corroborates that the English-text sovereignty claim was treated as settled and acted upon regardless. The corroboration explicitly comes from outside the Crown's benefiting apparatus: the Tribunal's finding contradicts a century of Crown legal position.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__retrospective_snare_exposure, 0.88, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness starts moderate in 1840 (0.2) — the divergence exists but has not yet been operationalized into confiscation or land-court machinery — and rises sharply through the New Zealand Wars period (1863, 0.55) and the era of Native Land Court individualization and confiscation (1900, 0.78), plateauing near its ceiling by mid-20th century as the doctrine of Crown sovereignty by cession becomes fully institutionalized in law, land title, and constitutional practice. Theater ratio rises in step: what began as straightforward administrative reliance on the English text (low theater in 1840) becomes increasingly performative as the Crown maintains legal fictions of settled sovereignty even as historical and linguistic scholarship documents the gap (theater rising toward 0.62 by 2014, when the Tribunal's own findings formally acknowledge the mistranslation while Crown legal doctrine remains largely unrevised in practice). Suppression spikes hard during the confiscation wars (0.75-0.82 from 1863-1900, reflecting military and legislative coercion) and declines only modestly thereafter as legal and political suppression of the underlying claim persists in subtler form (court doctrine, legislative supremacy) even after overt military suppression ends.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown land-purchasing apparatus's institutional seat, the arrangement appears as settled constitutional law — a completed, legitimate cession, with any translation imprecision treated as a historical footnote rather than an operative defect. From the Māori signatory hapū and descendant seats, the same structure appears as an extraction executed through language they could not have contested, discoverable in full only after generations of land loss and legal precedent had already been built on top of it. The Waitangi Tribunal's observer seat exists specifically to hold and adjudicate this gap without being able to fully close it retroactively.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown land-purchasing apparatus sits at the full-beneficiary end: it authored both texts, controls which is treated as authoritative in courts and legislatures, and has never itself borne the cost of the divergence. Māori signatory hapū and their descendants sit at the full-target end: trapped exit (they cannot retroactively renegotiate the 1840 signing), generational to civilizational time horizon (costs compound across generations), and the extraction was executed against an understanding they could not have avoided given the information available to them at the time. Non-signatory iwi are a distinctive victim class — extraction without even nominal consent — which the retrospective_snare_exposure reading foregrounds precisely because their situation makes the extraction-independent-of-agreement structure undeniable.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem — peaceful coexistence between Crown governance and Māori authority — was real and continues to be cited by all three kernel readings as the treaty's origin. This reading does not deny that founding function; it holds that the mechanism used to implement it (two divergent texts, one relied upon exclusively after signing) converted a genuine coordination attempt into a durable extraction structure. This is precisely the mandatrophy the classification exists to detect: labeling this a pure Rope (crown_cession_reading's implicit frame) would erase the extraction; labeling it a pure Mountain (treating cession as an inevitable, natural outcome of contact) would erase the mechanism's constructedness and its identifiable beneficiary and victim sets. The Snare classification is warranted because the coordination story functioned as cover — chiefs were told, and believed, they were retaining tino rangatiratanga — while the enforcement apparatus (courts, land confiscation, legislative supremacy) operated on the opposite premise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Is the operative extraction located in which text SHOULD control as a matter of law (the crown_cession_reading vs. rangatiratanga_retention_reading dispute), or in the mere fact that a covert divergence existed and was exploited regardless of which text is eventually vindicated (this reading)?',
    'Compare outcomes: if courts and tribunals fully vindicate the rangatiratanga_retention_reading and restitution is executed at a scale matching the extraction, the divergence-as-mechanism framing collapses into a temporary interpretive error rather than a durable snare. If restitution remains partial or symbolic despite formal acknowledgment of the mistranslation, the snare framing is corroborated as the operative structure regardless of the correct legal reading.',
    'If restitution fully closes the gap, this reading''s ε would need re-evaluation downward toward a resolved historical injustice rather than an ongoing extraction structure. Partial or symbolic restitution corroborates the extraction as still substantially live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the extraction is located in the interpretive dispute itself or in the exploited divergence independent of which reading is correct.').

omega_variable(
    translation_intentionality_ambiguity,
    'Was the divergence between the English and Māori texts a product of genuine translation difficulty and time pressure (no equivalent term for Western sovereignty existed in 1840 te reo Māori), or was it constructed or left uncorrected with knowledge that it would be exploited?',
    'Historical and archival analysis of missionary and colonial-office correspondence around the 1840 drafting and subsequent decades of Crown legal reliance on the English text despite documented awareness of the divergence (e.g., 19th-century legal opinions noting the discrepancy).',
    'If the original divergence was genuinely inadvertent but subsequently exploited with knowledge, the extraction mechanism is best located in the decades of deliberate reliance rather than the 1840 drafting itself — this would not change the classification but would refine the temporal onset of the snare''s operative extraction, currently modeled as sharply rising from 1863 onward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_intentionality_ambiguity, empirical, 'Whether the 1840 translation divergence was inadvertent or constructed, and when knowing exploitation began.').

omega_variable(
    restitution_adequacy_and_ongoing_extraction,
    'Does the existence of the Waitangi Tribunal and settlement processes since 1975 constitute genuine resolution of the extraction, or does it function as a partial, capped mechanism that formally acknowledges the mechanism while leaving the bulk of the original extraction (land value, foregone sovereignty, compounding intergenerational loss) uncompensated?',
    'Quantitative comparison of Tribunal settlement values against independent estimates of land and resource value extracted since 1840, adjusted for compounding economic loss.',
    'If settlements are found to systematically undervalue the original extraction, this corroborates continuing snare operation even in the presence of a formal redress mechanism — the redress mechanism itself would then warrant examination as a possible scaffold or piton overlay on the underlying snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restitution_adequacy_and_ongoing_extraction, empirical, 'Whether Tribunal-era redress substantively closes the extraction or merely formalizes acknowledgment of it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2014).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.1).
narrative_ontology:measurement_basis(trea_tr_t1840, observed).
narrative_ontology:measurement(trea_tr_t1863, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1863, 0.25).
narrative_ontology:measurement_basis(trea_tr_t1863, observed).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1900, 0.4).
narrative_ontology:measurement_basis(trea_tr_t1900, observed).
narrative_ontology:measurement(trea_tr_t1940, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1940, 0.5).
narrative_ontology:measurement_basis(trea_tr_t1940, observed).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1975, 0.58).
narrative_ontology:measurement_basis(trea_tr_t1975, observed).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2000, 0.6).
narrative_ontology:measurement_basis(trea_tr_t2000, observed).
narrative_ontology:measurement(trea_tr_t2014, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2014, 0.62).
narrative_ontology:measurement_basis(trea_tr_t2014, observed).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.2).
narrative_ontology:measurement_basis(trea_be_t1840, observed).
narrative_ontology:measurement(trea_be_t1863, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1863, 0.55).
narrative_ontology:measurement_basis(trea_be_t1863, observed).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1900, 0.78).
narrative_ontology:measurement_basis(trea_be_t1900, observed).
narrative_ontology:measurement(trea_be_t1940, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1940, 0.85).
narrative_ontology:measurement_basis(trea_be_t1940, observed).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1975, 0.87).
narrative_ontology:measurement_basis(trea_be_t1975, observed).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement_basis(trea_be_t2000, observed).
narrative_ontology:measurement(trea_be_t2014, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2014, 0.88).
narrative_ontology:measurement_basis(trea_be_t2014, observed).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement_basis(trea_su_t1840, observed).
narrative_ontology:measurement(trea_su_t1863, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1863, 0.75).
narrative_ontology:measurement_basis(trea_su_t1863, observed).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1900, 0.82).
narrative_ontology:measurement_basis(trea_su_t1900, observed).
narrative_ontology:measurement(trea_su_t1940, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1940, 0.79).
narrative_ontology:measurement_basis(trea_su_t1940, observed).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1975, 0.7).
narrative_ontology:measurement_basis(trea_su_t1975, observed).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2000, 0.76).
narrative_ontology:measurement_basis(trea_su_t2000, observed).
narrative_ontology:measurement(trea_su_t2014, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2014, 0.79).
narrative_ontology:measurement_basis(trea_su_t2014, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, rangatiratanga_retention_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the treaty_authority_cession kernel. crown_cession_reading and rangatiratanga_retention_reading each adjudicate which text should control as a matter of legal interpretation and therefore whether the treaty completed a legal cession or established an ongoing partnership. This story, retrospective_snare_exposure, is orthogonal to that interpretive dispute: it treats the uncorrected textual divergence and its 175-year exploitation as the extraction mechanism regardless of which interpretive reading is eventually vindicated. Each story carries its own ε, stakeholder set, and classification per the ε-invariance principle; they are linked via affects_constraints because judicial or legislative resolution of the interpretive dispute (the siblings) would directly determine whether this reading's extraction is closed, continued, or newly quantified.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
