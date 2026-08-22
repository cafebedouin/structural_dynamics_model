% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__crown_cession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__crown_cession_reading, []).

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
 *   constraint_id: treaty_authority_cession__crown_cession_reading
 *   human_readable: Treaty of Waitangi — Crown Cession Reading (English Text Controls, Kāwanatanga = Full Sovereignty)
 *   domain: constitutional/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This story authors the Crown cession reading of the Treaty of Waitangi
 *   kernel: the English-language text controls interpretation, 'kāwanatanga'
 *   is read as a full transfer of sovereignty equivalent to the Māori concept
 *   of complete governmental authority, and the treaty is treated as having
 *   legally completed cession of sovereign authority from the signatory
 *   rangatira to the British Crown. This reading grounds the entirety of
 *   subsequent Crown land purchase, legislative supremacy, and (for over a
 *   century) judicial treatment of Māori customary title as legally
 *   subordinate or extinguishable at the Crown's discretion (culminating
 *   doctrinally in Wi Parata v Bishop of Wellington, 1877, which held the
 *   treaty was not part of municipal law). The reading is authored here as
 *   ONE constraint among three siblings sharing the treaty_authority_cession
 *   kernel — the rangatiratanga_retention_reading (Māori text controls, tino
 *   rangatiratanga retained, treaty as ongoing partnership) and
 *   retrospective_snare_exposure (the textual divergence itself is the
 *   extraction mechanism) are separate constraints, not alternative
 *   measurements of this one. This story's epsilon is authored as high
 *   because, by its own lights, the reading treats sovereignty transfer and
 *   consequent land alienation as legally settled and legitimate — but the
 *   metrics track the reading's actual historical operation (active
 *   suppression of the Māori-text understanding, escalating land loss under
 *   its authority), not the endorsed alternative.
 *
 * KEY AGENTS:
 *   - crown_government: agenda_setter (institutional/arbitrage) — drafted controlling text, administers and legislates under it
 *   - settler_land_purchasers: beneficiary (organized/mobile) — title chain depends on Crown pre-emption grounded in this reading
 *   - colonial_judiciary: beneficiary/agenda_setter (institutional/analytical) — doctrinally enforces the reading via case law
 *   - signatory_hapu_and_iwi: payer (organized/trapped) — signed a different text than the one held controlling
 *   - subsequent_generations_of_maori_landholders: payer (powerless/trapped) — inherit compounding land loss
 *   - te_reo_maori_speaking_signatories_1840: excluded (powerless/trapped) — their understanding at signing has no standing under this reading
 *   - waitangi_tribunal: observer (institutional/analytical) — documents the divergence without power to void completed transfers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.81).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.78).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, tangled_rope).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Treaty of Waitangi — Crown Cession Reading (English Text Controls, Kāwanatanga = Full Sovereignty)").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, '18d12fed-b80c-4408-85e0-c60c7cf2d138').
narrative_ontology:cs_kernel_codification('18d12fed-b80c-4408-85e0-c60c7cf2d138', fixed_text).
narrative_ontology:cs_authority_grounding('18d12fed-b80c-4408-85e0-c60c7cf2d138', extraction).
narrative_ontology:cs_interpretation_layer_present('18d12fed-b80c-4408-85e0-c60c7cf2d138').
narrative_ontology:cs_reading_relation('18d12fed-b80c-4408-85e0-c60c7cf2d138', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('18d12fed-b80c-4408-85e0-c60c7cf2d138', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('18d12fed-b80c-4408-85e0-c60c7cf2d138', foundational, english_text_is_sole_controlling_instrument).
narrative_ontology:cs_axiom_status(english_text_is_sole_controlling_instrument, overridden).
narrative_ontology:cs_axiom_grounding('18d12fed-b80c-4408-85e0-c60c7cf2d138', english_text_is_sole_controlling_instrument, conventional).
narrative_ontology:cs_axiom('18d12fed-b80c-4408-85e0-c60c7cf2d138', foundational, kawanatanga_equals_undivided_sovereignty).
narrative_ontology:cs_axiom_status(kawanatanga_equals_undivided_sovereignty, overridden).
narrative_ontology:cs_axiom_grounding('18d12fed-b80c-4408-85e0-c60c7cf2d138', kawanatanga_equals_undivided_sovereignty, empirically_contingent).
narrative_ontology:cs_reference_frame('18d12fed-b80c-4408-85e0-c60c7cf2d138', crown_sovereignty_completed_at_signing).
narrative_ontology:cs_drift_state('18d12fed-b80c-4408-85e0-c60c7cf2d138', post_waitangi_tribunal_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('18d12fed-b80c-4408-85e0-c60c7cf2d138', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, crown_government).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, settler_land_purchasers).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, colonial_judiciary).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, signatory_hapu_and_iwi).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, subsequent_generations_of_maori_landholders).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, crown_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, parliamentary_supremacy_over_treaty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted the English text, administered the signing process, and subsequently governs, legislates, and adjudicates through courts that treat the English version and 'kāwanatanga equals full sovereignty' as the controlling legal fact. Collects legislative authority, land title regularization powers, and the founding legitimacy claim of the state itself from this reading.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, crown_government, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Acquire land titles that depend on the Crown's exclusive pre-emption right, itself grounded in the claim that full sovereignty (and with it the sole right to extinguish native title) passed to the Crown at signing. Their title chain is legally secure only under this reading.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, settler_land_purchasers, beneficiary,
    organized, generational, mobile, national).

% Nineteenth- and much of twentieth-century case law (e.g. Wi Parata) treats the treaty as a legal nullity or as fully absorbed into Crown sovereignty, insulating legislative acts of land confiscation and alienation from challenge on treaty grounds. The judiciary both benefits from the doctrinal simplicity of this reading and is the mechanism that enforces it.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, colonial_judiciary, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__crown_cession_reading, colonial_judiciary, agenda_setter).

% Signed the Māori-language text (te reo Māori version, the version actually read to and discussed with most signatories) which promised 'tino rangatiratanga' retained over lands and taonga while ceding 'kāwanatanga' — a narrower governance function. Under the Crown cession reading, their signatures are treated as consenting to full sovereignty transfer regardless of what they were told or what the Māori text says. They cannot exit the jurisdiction that now claims authority over them; their only avenue is litigation and political petition within a system whose founding premise this reading supplies.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, signatory_hapu_and_iwi, payer,
    organized, civilizational, trapped, regional).

% Inherit the consequences of land alienation and legislative override legitimated by this reading — confiscations, forced sales, and statutory extinguishment of customary title all trace their legal validity to 'kāwanatanga equals full sovereignty.' They bear the compounding cost across generations without having been party to the original signing.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, subsequent_generations_of_maori_landholders, payer,
    powerless, generational, trapped, national).

% The rangatira who actually signed at Waitangi and subsequent sites read or heard the Māori text, not the English one. Their understanding of what they were agreeing to has no standing within the Crown cession reading, which treats the English text as controlling regardless of what was communicated to signatories at the moment of signing.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, te_reo_maori_speaking_signatories_1840, excluded,
    powerless, biographical, trapped, regional).

% A body established later to hear claims of Crown breach; it operates partly outside the Crown cession reading's own logic (since its inquiries take the Māori text and its promises seriously), producing findings that document the divergence this reading suppresses, without being able to retroactively void title already transferred under it.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__crown_cession_reading, crown_government).
narrative_ontology:fixing_cost_class(treaty_authority_cession__crown_cession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable legal foundation for state authority, land title, and legislative supremacy across the colony/nation — a coordination good for anyone transacting property or seeking a settled rule of law, so long as they are positioned to benefit from the rule as settled.
% TRANSFER_FUNCTION: Moves legislative and land authority from hapu and iwi to the Crown, and subsequently moves land itself from Māori landholders to settlers and the Crown, using the English-text/full-sovereignty reading as the legal instrument that authorizes both transfers.
% ABSENT_VOICES: The rangatira who signed the Māori text, and their descendants, are structurally excluded from the interpretive process this reading enshrines — the reading's own operation is what removes their account of the agreement from legal relevance.
% DISAPPEARANCE_RATIONALE: If this reading were withdrawn as the controlling legal fact, the entire chain of Crown pre-emption, statutory land alienation, and legislative supremacy over Māori customary authority would lose its founding legal premise; land title systems, constitutional doctrine, and the legitimacy claims of the state itself would require reconstruction on a different basis (as partially occurred once courts began crediting the Māori text and tribunal findings).
% FOUNDING_PROBLEM: The Crown sought a legal instrument that would establish unambiguous sovereign authority over the colony, secure exclusive purchasing rights over Māori land against competing European interests, and provide a title chain settlers and later courts could rely upon without contest.
% FOUNDING_PROBLEM_CORROBORATION: The Crown and colonial judiciary (through case law such as Wi Parata v Bishop of Wellington) attest the founding problem was solved and the sovereignty transfer is settled. Independent corroboration from outside the beneficiary set — the Waitangi Tribunal's own findings, historians working from the Māori-language record, and comparative treaty-law scholarship — hold that the English text was neither read to nor agreed by most signatories, and that the founding problem this reading claims to have solved (legitimate sovereign transfer) was never actually resolved on its own terms.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__crown_cession_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__crown_cession_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__crown_cession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 by 2000) because the reading's operation directly authorizes land alienation and legislative override of customary authority without requiring ongoing Māori consent — the transfer, once read as completed sovereignty cession, forecloses continuous negotiation. Suppression is authored high (0.78) and shows a sharp mid-19th-century peak (0.88 at 1877, coinciding with Wi Parata) reflecting the period of most aggressive doctrinal enforcement (land wars, confiscation legislation, judicial nullification of treaty status), followed by partial decline as direct military coercion receded, then a partial rise again toward 2000 as land title and resource-management disputes intensified political contestation of the reading. Theater ratio rises through the same period (peaking at 0.55 in 1877) as legalistic doctrine substitutes for the original coordination claim — courts perform a finding of legal completeness that increasingly diverges from what was actually agreed at signing. All three tracked metrics share the single interval grid (1840–2000) at seven aligned time points.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown and colonial judiciary sit at the beneficiary end: this reading is the legal instrument that grounds their authority and is authored, administered, and enforced by them. Settler land purchasers benefit derivatively — their title security depends on the reading holding. Signatory hapu and iwi and their descendants sit at the target end: trapped by a jurisdictional claim they did not assent to in the terms it is read to impose, with no exit from the polity now claiming authority over them. The excluded 1840 signatories are not victims in the beneficiary/victim sense used for chi computation (they are historical actors, not an ongoing structural position) but are named to show whose account this reading's operation removes from legal relevance — this is the absent_voices structure, not the payer/beneficiary axis.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing a legal basis for Crown sovereign authority and secure land title) is treated by the Crown and judiciary as resolved and closed — 'live' only in the sense that the doctrine remains actively defended, not in the sense that the underlying legitimacy question is open. Corroboration from outside the beneficiary set (the Tribunal, historical linguistics, comparative treaty scholarship) holds the founding problem was never actually resolved on this reading's own terms, since the instrument used to resolve it was not the instrument most signatories agreed to. This mismatch — status effectively 'dead as a genuinely settled question' while the reading's institutional apparatus behaves as though disappearance would be catastrophic (world_rearranges) — is exactly the capture/zombie signature the R5 interview is designed to surface, distinct from asking whether the reading is 'good.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_text_is_the_treaty,
    'Is the Treaty of Waitangi, as a legal instrument, properly identified with the English text (as this reading holds) or the Māori text that most signatories actually read or heard explained (as the rangatiratanga_retention_reading holds), given that the two texts differ substantially on the scope of authority transferred?',
    'Comparative linguistic and historical analysis of what was communicated to signatories at each signing location, cross-referenced against contemporaneous colonial records of explanation given by missionaries and officials acting as translators.',
    'If the Māori text is held controlling (or co-controlling under contra proferentem), the entire legal chain of Crown sovereignty and consequent land alienation legitimated by this reading loses its foundational premise; land title regimes and legislative supremacy doctrine built on this reading would require re-examination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(which_text_is_the_treaty, empirical, 'Which treaty text is the legally operative instrument — a question this reading resolves by fiat rather than by demonstrated consent.').

omega_variable(
    sovereignty_kawanatanga_translation_gap,
    'Does ''kāwanatanga'' as used and understood by rangatira in 1840 carry the meaning of full, undivided sovereignty (this reading''s claim) or a narrower delegated governance function compatible with retained tino rangatiratanga (the sibling reading''s claim)?',
    'Historical linguistics on 1840s Māori usage of kāwanatanga versus mana and rangatiratanga; missionary correspondence describing how the term was explained to signatories; comparison with other contemporaneous Māori-language treaty and governance documents.',
    'If kāwanatanga was understood narrowly, this reading''s central premise — that full sovereignty transferred — is not merely contested but was never the agreement actually made, sharpening the case for the retrospective_snare_exposure reading over this one as the historically accurate account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_kawanatanga_translation_gap, empirical, 'Whether the sovereignty-equivalence claim central to this reading matches 1840s Māori linguistic understanding.').

omega_variable(
    natural_law_vs_constructed_beneficiary_note,
    'This reading names beneficiaries (Crown, settlers, judiciary) while claiming legal completeness as though it were a settled, near-natural fact of constitutional law rather than one contested interpretive choice among several live readings — is the ''completed cession'' status better understood as a discovered legal fact or a constructed doctrinal choice that happens to benefit identifiable parties?',
    'Track whether courts and legislatures increasingly treat the reading as revisable (as occurred post-1975 with the Treaty of Waitangi Act and subsequent tribunal jurisprudence) versus treating it as an immutable constitutional bedrock; a revisable doctrine with clear beneficiaries indicates construction, not discovery.',
    'Directly informs whether this reading''s persistence should be understood as institutional inertia protecting a constructed legal fiction (tangled_rope/piton trajectory) or as a stable, if contested, constitutional settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_beneficiary_note, conceptual, 'Whether the reading''s ''completed cession'' framing is a discovered fact or a constructed doctrine serving named beneficiaries — required omega given this constraint names beneficiaries and claims doctrinal settledness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 1840, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__crown_cession_reading, theater_ratio, 1840, 0.15).
narrative_ontology:measurement(trea_tr_t1863, treaty_authority_cession__crown_cession_reading, theater_ratio, 1863, 0.25).
narrative_ontology:measurement(trea_tr_t1877, treaty_authority_cession__crown_cession_reading, theater_ratio, 1877, 0.55).
narrative_ontology:measurement(trea_tr_t1910, treaty_authority_cession__crown_cession_reading, theater_ratio, 1910, 0.5).
narrative_ontology:measurement(trea_tr_t1950, treaty_authority_cession__crown_cession_reading, theater_ratio, 1950, 0.4).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__crown_cession_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__crown_cession_reading, theater_ratio, 2000, 0.42).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1840, 0.55).
narrative_ontology:measurement(trea_be_t1863, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1863, 0.72).
narrative_ontology:measurement(trea_be_t1877, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1877, 0.85).
narrative_ontology:measurement(trea_be_t1910, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1910, 0.82).
narrative_ontology:measurement(trea_be_t1950, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1950, 0.78).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1975, 0.74).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2000, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1840, 0.4).
narrative_ontology:measurement(trea_su_t1863, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1863, 0.65).
narrative_ontology:measurement(trea_su_t1877, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1877, 0.88).
narrative_ontology:measurement(trea_su_t1910, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1910, 0.8).
narrative_ontology:measurement(trea_su_t1950, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2000, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the treaty_authority_cession kernel, each authored as a separate constraint per the ε-invariance principle: crown_cession_reading (this story — English text controls, kāwanatanga equals full sovereignty, epsilon authored high reflecting active enforcement of land alienation and legislative override under this reading's own operation); rangatiratanga_retention_reading (Māori text controls, kāwanatanga read narrowly, tino rangatiratanga retained, treaty as ongoing partnership — a structurally different beneficiary/victim configuration and likely lower epsilon by that reading's own lights); retrospective_snare_exposure (the textual divergence itself is treated as the extraction mechanism, making the mistranslation the constraint's subject rather than either substantive sovereignty claim). Each carries its own epsilon, stakeholders, and classification; none averages across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
