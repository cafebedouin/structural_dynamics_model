% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__retrospective_snare_exposure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: treaty_authority_cession__retrospective_snare_exposure
 *   human_readable: Treaty of Waitangi Sovereignty Cession as Retrospective Extraction Snare
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   In February 1840 and subsequent months, over 500 Māori rangatira signed
 *   te Tiriti o Waitangi, a document translated into Māori by Henry Williams
 *   under time pressure. The Māori text offered the Crown kāwanatanga
 *   (governance) while explicitly guaranteeing tino rangatiratanga (full
 *   chiefly authority) over lands and taonga. The English text, which was not
 *   read to most signatories and existed as the version dispatched to London,
 *   claimed full sovereignty cession. For the following century and a half,
 *   the Crown's land purchasing apparatus, courts, and Parliament operated on
 *   the English text's authority — enabling large-scale land confiscation
 *   (particularly following the New Zealand Wars), the Native Land Court's
 *   individualization of communal title, and the assertion of unchallengeable
 *   legislative supremacy over Māori affairs. The extraction was not visible
 *   as extraction to most signatories at the time of signing: they had
 *   assented to a governance-sharing arrangement, not a sovereignty transfer,
 *   and the mechanism by which the divergent claim was operationalized
 *   (translation control, subsequent unilateral assertion, legal enforcement
 *   by the same body that authored the divergence) meant the true structure
 *   only became demonstrable through retrospective textual and documentary
 *   analysis conducted more than a century later.
 *
 * KEY AGENTS:
 *   - crown_land_purchasing_apparatus: institutional beneficiary — authored the divergent texts, enforced the English version, collected land and authority
 *   - maori_signatory_iwi: primary historical victim — assented to a limited governance arrangement, bore full sovereignty-cession consequences they could not have agreed to
 *   - maori_descendants_present_day: ongoing victim — inherit compounded land loss and constrained redress within a system still grounded in the contested cession
 *   - crown_treaty_negotiators_1840: agenda-setters at point of origin — controlled translation and dissemination of both texts
 *   - waitangi_tribunal: analytical observer — documents the divergence but lacks binding authority to unwind it
 *   - settler_landholders_and_successors: downstream beneficiary — holds title dependent on the disputed cession's validity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.88).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.79).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.88).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty of Waitangi Sovereignty Cession as Retrospective Extraction Snare").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, 'c6a173a1-f1b4-435a-951c-5610db8d6fe9').
narrative_ontology:cs_kernel_codification('c6a173a1-f1b4-435a-951c-5610db8d6fe9', fixed_text).
narrative_ontology:cs_authority_grounding('c6a173a1-f1b4-435a-951c-5610db8d6fe9', extraction).
narrative_ontology:cs_interpretation_layer_present('c6a173a1-f1b4-435a-951c-5610db8d6fe9').
narrative_ontology:cs_reading_relation('c6a173a1-f1b4-435a-951c-5610db8d6fe9', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('c6a173a1-f1b4-435a-951c-5610db8d6fe9', treaty_authority_cession__rangatiratanga_retention_reading, influences).
narrative_ontology:cs_axiom('c6a173a1-f1b4-435a-951c-5610db8d6fe9', foundational, divergence_itself_is_the_extraction_mechanism).
narrative_ontology:cs_axiom_status(divergence_itself_is_the_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('c6a173a1-f1b4-435a-951c-5610db8d6fe9', divergence_itself_is_the_extraction_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('c6a173a1-f1b4-435a-951c-5610db8d6fe9', foundational, assent_requires_access_to_the_operative_text).
narrative_ontology:cs_axiom_status(assent_requires_access_to_the_operative_text, holdable).
narrative_ontology:cs_axiom_grounding('c6a173a1-f1b4-435a-951c-5610db8d6fe9', assent_requires_access_to_the_operative_text, deontological).
narrative_ontology:cs_reference_frame('c6a173a1-f1b4-435a-951c-5610db8d6fe9', dual_text_signing_event_1840).
narrative_ontology:cs_drift_state('c6a173a1-f1b4-435a-951c-5610db8d6fe9', post_tribunal_documentary_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('c6a173a1-f1b4-435a-951c-5610db8d6fe9', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, settler_colonial_administration).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatory_iwi).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendants_present_day).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, settler_landholders_and_successors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operated the pre-emption purchasing scheme and subsequent legislative confiscation mechanisms (Native Land Court, Land Wars confiscations) built directly on the English-text claim of full sovereign cession. Drafted and circulated the divergent English text, controlled the only translation into Māori that chiefs actually read and signed, and later cited the English text as controlling in courts and Parliament it also controlled. Collected land, resource rights, and governing authority as the direct yield of the divergence it authored.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, agenda_setter).

% Signed te Tiriti o Waitangi in Māori, in which kāwanatanga (a limited governance term) was offered while tino rangatiratanga (full chiefly authority) was retained. Had no access to, and could not have assented to, the English-language sovereignty cession that was never read to them. Subsequently faced land confiscation, loss of self-government, and legal proceedings adjudicated under the English text they did not sign. Exit was foreclosed after the fact: the extraction was not visible as extraction until legal and land loss had already occurred, at which point resistance (armed and legal) met an apparatus that now controlled the courts, the land title system, and the legislature.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_signatory_iwi, payer,
    organized, generational, trapped, national).

% Inherit the compounded consequences of land loss and authority stripping validated by a translation their ancestors never agreed to. Pursue redress through the Waitangi Tribunal and courts, which operate within a legal system whose foundational authority still rests on the disputed cession. Their exit options are constrained by the settlement process itself being administered by the same state whose founding legitimacy is the contested object.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendants_present_day, payer,
    organized, civilizational, constrained, national).

% William Hobson and the missionary translators produced the Māori text under time pressure to secure signatures quickly across multiple hui, while the English text embodying a stronger sovereignty claim existed as the version intended for London and subsequent imperial administration. Whether by design or negligence, the translator (Henry Williams) rendered 'sovereignty' as 'kāwanatanga,' a term with no prior meaning of total cession in Māori political vocabulary.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_treaty_negotiators_1840, agenda_setter,
    institutional, immediate, arbitrage, national).

% Established in 1975 to hear claims of Treaty breach; empowered to examine both texts and make findings but historically limited to recommendatory rather than binding authority over land title and legislative supremacy. Documents the divergence extensively but operates downstream of, and without power to unwind, the extraction it identifies.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Acquired title to confiscated or purchased land under a legal system whose authority to grant clean title depends on the English-text cession being valid. Many hold title in good faith generations removed from the original transaction, but their property interest is a direct downstream beneficiary of the same divergence that dispossessed the signatory iwi.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, settler_landholders_and_successors, beneficiary,
    powerful, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:fixing_cost_class(treaty_authority_cession__retrospective_snare_exposure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuine at the point of signing for the sovereignty claim itself — the Māori text did coordinate a real transitional arrangement (limited governance rights to the Crown over settlers, in exchange for protection and retained chieftainship), but that coordination function attached to the Māori text's terms, not the English text's terms. The extraction mechanism (retrospective snare) operates precisely because the coordination story was told in one language and the extraction was executed under the authority of a different, undisclosed one.
% TRANSFER_FUNCTION: Moves land, resource rights, and governing authority from Māori signatory iwi and their descendants to the Crown and downstream settler title-holders, transferred nominally under 'the Treaty' but substantively secured by enforcing the English text's sovereignty claim that was never the object of Māori assent.
% ABSENT_VOICES: The Māori chiefs and their communities who signed te Tiriti in 1840 are structurally absent from the process by which the English text came to control legal and political outcomes — they were never consulted on, nor shown, the operative document that courts and Parliament subsequently treated as authoritative. Their present-day descendants participate through the Tribunal but within a forum whose jurisdiction is itself downstream of the contested cession.
% DISAPPEARANCE_RATIONALE: If the English-text sovereignty claim were retrospectively voided as never validly assented to, New Zealand's constitutional foundation, land title regime, and legislative supremacy structure would require complete reconstruction — land title chains running through confiscation-era transfers would face challenge, and government authority itself would need re-grounding in the Māori text's more limited terms or in fresh negotiation. This is not a peripheral mechanism; it is load-bearing for the entire settler state's legal architecture.
% FOUNDING_PROBLEM: Ostensibly: to establish an orderly, consensual framework for British governance presence in New Zealand that protected Māori land and authority from the lawlessness of unregulated settler encroachment already underway. That was the problem the Māori text was presented as solving.
% FOUNDING_PROBLEM_CORROBORATION: The Waitangi Tribunal, an institution structurally outside the beneficiary apparatus in its investigatory function (though housed within the same state), has repeatedly found in reports (Te Paparahi o Te Raki, 2014; Muriwhenua) that the chiefs who signed at Waitangi and subsequent sites did not cede sovereignty in February 1840 as the English text claims, and that the Crown's subsequent unilateral assertion of full sovereignty was not supported by the instrument the signatories actually agreed to. This is corroboration from a body created by the Crown itself but exercising an investigatory function independent of the land-purchasing and legislative apparatus that benefits from the cession claim — historians outside any state apparatus (Ruth Ross, Claudia Orange) reached the same textual-divergence conclusion independently.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.88 at present) because the transfer of land and governing authority rests on a sovereignty claim the primary victim class never assented to in the only text they were shown — this is not a contested interpretation dispute but a documented translation divergence with a one-sided enforcement history. Suppression (0.79) reflects that the arrangement's persistence has required active legal and, historically, military enforcement (the New Zealand Wars, confiscation legislation, Native Land Court proceedings) to hold against Māori resistance and later legal challenge; the suppression trajectory shows a peak during the confiscation-war era (1860-1900) followed by partial relaxation and a renewed rise as legal challenge (Tribunal claims, litigation) required the state to develop more sophisticated legal defenses of the same underlying claim. Accessibility collapse is authored moderate (0.35) rather than high because, unlike a genuine natural-law constraint, alternatives were never foreclosed as a matter of logic or physics — the Māori text itself preserved an alternative (rangatiratanga retention) that remained legally arguable throughout, which is precisely why the Tribunal and courts have been able to revisit the claim. Theater ratio rises over time (0.15 to 0.42) as the Crown's defense of the arrangement shifted from raw enforcement toward increasingly elaborate legal and procedural justification (Tribunal processes, settlement negotiations) that manage the appearance of redress without fully relinquishing the underlying sovereignty claim.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown apparatus's own historical self-conception, the arrangement was and is a completed, legitimate act of state formation — this is the crown_cession_reading, a different constraint entirely. From the signatory iwi's structural position, assessed by what they actually assented to in the only text available to them, the same arrangement is an extraction mechanism whose true operation was concealed by the translation gap and only demonstrable after the fact through documentary reconstruction. The engine computing divergent seat classifications from the same structural data is exactly the phenomenon this reading exists to make visible.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown land-purchasing apparatus and its institutional successors sit at the full-beneficiary end: they authored the divergence, controlled its translation, and directly collected the land and authority that flowed from enforcing the English text. Māori signatory iwi and their descendants sit at the full-target end: trapped exit options at the point of the original transaction (no alternative sovereign structure was available to negotiate with), and constrained exit even now, since redress mechanisms are administered by the same state whose foundational legitimacy is the contested object. Settler landholders occupy an intermediate position — genuine beneficiaries of downstream title security, but with constrained rather than arbitrage exit, since their title's validity is itself hostage to how the underlying cession claim is eventually resolved.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — orderly, consensual British governance presence protecting Māori land and authority from unregulated settler encroachment — is dead by this reading's assessment: the arrangement now serves primarily to secure title chains and constitutional authority built on a claim the original signatories did not make. This is not a case of legitimate coordination that later atrophied into pure inertia (which would suggest a piton); it is a case where the coordination function attached to one text while the extraction was executed under a different, undisclosed one from the outset — making this a snare with a genealogy that only became legible retrospectively, not a genuinely coordinating rope that decayed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    translation_intent_ambiguity,
    'Was the divergence between the Māori and English texts a product of deliberate strategic ambiguity by Crown negotiators, or a genuine translation failure arising from the absence of a Māori concept equivalent to European sovereignty?',
    'Documentary analysis of Hobson''s and Williams''s private correspondence and instructions from the Colonial Office regarding the intended scope of ''sovereignty'' versus ''kāwanatanga''; comparison with contemporaneous translation practice in other British treaty-making with indigenous polities.',
    'If deliberate, this reading''s snare classification is strongly corroborated as intentional extraction. If genuine translation failure without intent to deceive, the mechanism remains structurally extractive in its operation and consequences but the moral valence of the founding act shifts — though this reading holds that intent is not required for the structural classification to hold, since the subsequent century of enforcement on the English text regardless of translation difficulty is itself the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_intent_ambiguity, empirical, 'Whether textual divergence originated in deliberate strategy or translation limitation.').

omega_variable(
    which_text_is_the_kernel,
    'Is there a single Treaty of Waitangi kernel with two competing readings (crown_cession and rangatiratanga_retention), or are there structurally two different treaties that happened to be signed on the same occasion, making ''the Treaty'' itself a category error?',
    'This is fundamentally a conceptual question about how to model textual divergence in constitutional founding documents; it cannot be resolved by further evidence but depends on whether one treats ''the Treaty'' as a single instrument with interpretive uncertainty or as two distinct instruments falsely presented as one.',
    'If two distinct instruments, the retrospective_snare_exposure reading is arguably the MOST accurate framing of all three kernel readings, since it is the only one that takes the divergence itself as constitutive rather than as a problem to be resolved by picking a controlling text. If a single instrument with contestable interpretation, this reading''s emphasis on extraction risks obscuring the genuine interpretive question the other two readings engage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_text_is_the_kernel, conceptual, 'Whether textual divergence constitutes one contested document or two distinct instruments.').

omega_variable(
    redress_adequacy_and_ongoing_extraction,
    'Do current Waitangi Tribunal settlements adequately dissolve the extraction mechanism, or do they operate as a new, lower-intensity extraction layer that formalizes partial restitution while leaving the underlying sovereignty claim and most land title intact?',
    'Comparative analysis of settlement quantum against independently estimated value of land and resources taken, and analysis of whether settlements require claimant iwi to accept full and final discharge of claims as a condition of any redress.',
    'If settlements are adequate, the snare''s present-day operation is substantially resolved even if the historical extraction stands; if settlements are a formalized partial-restitution layer, the extraction mechanism should be understood as ongoing rather than purely historical, which would raise the currently authored extractiveness value further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redress_adequacy_and_ongoing_extraction, preference, 'Whether Treaty settlement processes resolve or merely re-formalize the underlying extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.15).
narrative_ontology:measurement(trea_tr_t1860, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1860, 0.2).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1900, 0.25).
narrative_ontology:measurement(trea_tr_t1950, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1975, 0.38).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.35).
narrative_ontology:measurement(trea_be_t1860, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1860, 0.68).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1900, 0.82).
narrative_ontology:measurement(trea_be_t1950, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1950, 0.85).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1975, 0.83).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2000, 0.86).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement(trea_su_t1860, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1860, 0.75).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(trea_su_t1950, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2024, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__retrospective_snare_exposure, 0.1).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, rangatiratanga_retention_reading).

% DUAL FORMULATION NOTE:
% This constraint is the third member of the treaty_authority_cession kernel family. crown_cession_reading authors the arrangement as completed legitimate cession (low ε from that reading's own lights); rangatiratanga_retention_reading authors it as an ongoing partnership requiring consent, breached by unilateral Crown action (high ε directed at the breach rather than at cession itself); this story (retrospective_snare_exposure) authors ε highest of the three, treating the textual divergence itself — independent of which text should control as a matter of doctrine — as the extraction mechanism, covert at signing and demonstrable only through retrospective analysis. All three share the same underlying historical kernel (the 1840 signing events and the two-text problem) but instantiate structurally distinct constraints with distinct beneficiary/victim structures and distinct ε values, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__retrospective_snare_exposure, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
