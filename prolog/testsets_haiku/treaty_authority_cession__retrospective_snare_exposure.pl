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
 *   constraint_id: treaty_authority_cession__retrospective_snare_exposure
 *   human_readable: Treaty Authority Cession via Mistranslation: Retrospective Snare Exposure
 *   domain: constitutional/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) is the foundational document of Aotearoa
 *   New Zealand's legal order. Two versions exist: a Māori-language version
 *   that Māori chiefs signed, and an English-language version that the Crown
 *   invoked to claim total sovereignty and land cession. The Māori text uses
 *   kāwanatanga (governance/authority to regulate) for the Crown and reserves
 *   tino rangatiratanga (absolute authority) for Māori. The English text
 *   translates these terms as 'sovereignty' (total) and 'rights and
 *   privileges' (subordinate). For 150+ years, the Crown treated the English
 *   text as binding and denied the Māori text's legal force. Over that
 *   period, Māori were dispossessed of 95%+ of productive land through
 *   legislation, court rulings, and administrative action grounded in the
 *   English version. When linguists and historians documented the textual
 *   divergence (1980s–1990s), the mechanism became visible: the extraction
 *   operated by preventing the victims from understanding the English claim
 *   to sovereignty, then using that claim to override their actual authority.
 *   This is the retrospective snare exposure reading: the extraction
 *   mechanism itself WAS the mistranslation, and it only becomes fully
 *   visible when the textual divergence is documented and analyzed.
 *
 * KEY AGENTS:
 *   - Māori signatories and descendants: the victims, who signed the Māori text and believed they were preserving tino rangatiratanga; they were trapped by the English version they could not read.
 *   - Crown land-purchasing apparatus: the beneficiary and agenda-setter, which set the terms, used the English text to claim sovereignty, and orchestrated the dispossession.
 *   - Crown judiciary and parliament: the mechanisms of enforcement, treating the English text as binding and overriding Māori claims through legislation and court rulings.
 *   - English-speaking colonists and settlers: the beneficiary class, who inherited land, wealth, and governance authority from the extraction.
 *   - Treaty translation specialists (linguists and historians): the excluded voices, who documented the textual divergence and made the mechanism visible, but only after 150+ years.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.89).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.92).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.89).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty Authority Cession via Mistranslation: Retrospective Snare Exposure").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, 'eb39f430-351d-45f8-96e5-a41f1c4b1aa5').
narrative_ontology:cs_kernel_codification('eb39f430-351d-45f8-96e5-a41f1c4b1aa5', fixed_text).
narrative_ontology:cs_authority_grounding('eb39f430-351d-45f8-96e5-a41f1c4b1aa5', extraction).
narrative_ontology:cs_interpretation_layer_present('eb39f430-351d-45f8-96e5-a41f1c4b1aa5').
narrative_ontology:cs_reading_relation('eb39f430-351d-45f8-96e5-a41f1c4b1aa5', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('eb39f430-351d-45f8-96e5-a41f1c4b1aa5', treaty_authority_cession__rangatiratanga_retention_reading, coexists_with).
narrative_ontology:cs_axiom('eb39f430-351d-45f8-96e5-a41f1c4b1aa5', foundational, textual_divergence_is_extraction_mechanism).
narrative_ontology:cs_axiom_status(textual_divergence_is_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('eb39f430-351d-45f8-96e5-a41f1c4b1aa5', textual_divergence_is_extraction_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('eb39f430-351d-45f8-96e5-a41f1c4b1aa5', foundational, maori_text_is_operative_at_signing).
narrative_ontology:cs_axiom_status(maori_text_is_operative_at_signing, holdable).
narrative_ontology:cs_axiom_grounding('eb39f430-351d-45f8-96e5-a41f1c4b1aa5', maori_text_is_operative_at_signing, empirically_contingent).
narrative_ontology:cs_axiom('eb39f430-351d-45f8-96e5-a41f1c4b1aa5', secondary, retrospective_snare_visibility_required_for_remediation).
narrative_ontology:cs_axiom_status(retrospective_snare_visibility_required_for_remediation, holdable).
narrative_ontology:cs_axiom_grounding('eb39f430-351d-45f8-96e5-a41f1c4b1aa5', retrospective_snare_visibility_required_for_remediation, deontological).
narrative_ontology:cs_reference_frame('eb39f430-351d-45f8-96e5-a41f1c4b1aa5', maori_text_binding_at_signing).
narrative_ontology:cs_drift_state('eb39f430-351d-45f8-96e5-a41f1c4b1aa5', contemporary_2024, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('eb39f430-351d-45f8-96e5-a41f1c4b1aa5', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatories_and_descendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_judiciary_and_parliament).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, english_speakers_and_colonists).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, maori_governance_structures).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_governance_structures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Chiefs signed a Māori-language text (Te Tiriti o Waitangi) believing they were preserving their authority (tino rangatiratanga) while accepting limited Crown governance (kāwanatanga). The English version they could not read claimed total sovereignty and land cession. Over 150+ years, they and their descendants were dispossessed of land, water rights, and governance authority through legislative override and court rulings grounded in the English text. Exit was never available: the mechanism trapped them retroactively by denying them the understanding they acted on. Knowledge of the textual divergence came much later.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_signatories_and_descendants, payer,
    powerless, generational, trapped, national).

% Set the terms of the treaty, presented as a Crown offer to acknowledge Māori authority while establishing British sovereignty. Used the English text (which the signatories could not read) as the legal instrument for land acquisition and legislative override. Enforced the dispossession through court rulings, Acts of Parliament, and administrative control, treating the English version as binding and the Māori text as subordinate or merely aspirational. Collected the extracted value: 95%+ of Aotearoa/New Zealand's productive land.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Over 150+ years, passed legislation and issued court rulings that treated the English text as authoritative, overrode Māori claims to land and authority, and denied the binding status of the Māori text (e.g., the 1975 Treaty of Waitangi Act initially recognized only the English version). They administered the extraction through property law, resource legislation, and statutory interpretation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_judiciary_and_parliament, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, crown_judiciary_and_parliament, beneficiary).

% Received land, resource rights, governance positions, and wealth through the dispossession mechanism. The extraction enriched them and their descendants across generations, funding development, farming, mining, and urban settlement on land taken under mistranslation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, english_speakers_and_colonists, beneficiary,
    powerful, generational, arbitrage, national).

% Linguists and historians who documented the textual divergence (starting in the 1980s–1990s) were structurally excluded from the interpretation authority at the time of signing. Their analysis came too late to inform the original transaction and was long resisted by Crown legal positions. They would have testified that kāwanatanga does not mean sovereignty and tino rangatiratanga is incompatible with cession — but had no seat at the initial negotiation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, treaty_translation_specialists, excluded,
    powerless, biographical, trapped, national).

% Contemporary Māori institutions (iwi, hapū, boards) claim authority under the Māori text and tino rangatiratanga. They benefit from some contemporary recognition and co-governance frameworks (partial restoration) while remaining systematically under-resourced and subject to override. They are simultaneously beneficiaries of recognition movements and payers of the ongoing cost of dispossession.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_governance_structures, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, maori_governance_structures, payer).

% The wider non-Māori population inherited the benefits of the dispossession (property, wealth, opportunity) without explicit choice or knowledge of the extraction mechanism. They are implicated beneficiaries but positioned as observers of a dispute they did not author and from which they cannot simply exit.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, settler_public, observer,
    organized, biographical, constrained, national).

% Indigenous rights organizations and UN mechanisms document the pattern: textual divergence as extraction technique, retrospective exposure, and demand for restitution. They are analytical observers making the structure visible to external audiences.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, international_indigenous_advocacy, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:fixing_cost_class(treaty_authority_cession__retrospective_snare_exposure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The treaty, presented in both Māori and English versions, claims to establish a framework for peaceful coexistence: the Crown accepts responsibility for governing settlers and protecting Māori rights, while Māori retain authority (tino rangatiratanga) over hapū and iwi. If this framing were accurate and both texts meant the same thing, it would solve the genuine coordination problem of the era: rapid settler colonization without legal frameworks, epidemic disease, and conflicting claims to land and authority.
% TRANSFER_FUNCTION: Transfers control of land (95%+ of Aotearoa's productive landmass) from Māori collective ownership and governance to Crown and settler private ownership and control. Transfers legislative and judicial authority from Māori hapū/iwi to the Crown parliament and courts. Transfers wealth from Māori descendants to settler descendants across 180+ years. The mechanism of transfer is the enforcement of the English text's claim to absolute Crown sovereignty, combined with legislative override and court denials of the Māori text's binding legal force.
% ABSENT_VOICES: The chiefs and Māori negotiators who signed the Māori text were never seated as authoritative voices in the interpretation of what they signed. Their understanding of the text (preserved in the Māori version) was structurally excluded from the legal interpretation that followed. Linguists and historians who could have testified to the textual divergence and mistranslation were absent from the transaction and only became audible 150+ years later. Contemporary Māori communities demanding restoration of tino rangatiratanga are the returned absent voice, but their inclusion came too late to prevent the dispossession and comes now only in the context of a constraint already operated for generations.
% DISAPPEARANCE_RATIONALE: If the extraction mechanism vanished (Crown recognized the Māori text as equally binding, courts applied contra proferentem to the English text, Parliament restored Māori authority over land and resources), Aotearoa's property law, governance structure, resource allocation, and wealth distribution would fundamentally reorganize. Settler claims to 95% of productive land would be invalidated or renegotiated. Parliamentary supremacy would be limited by recognized Māori tino rangatiratanga. Entire industries (farming, mining, urban development) built on settler-held property would face reallocation. Crown revenue (through land sales and resource extraction) would be redirected or reduced. This is why the extraction persists despite exposure: the beneficiaries cannot exit without losing the extracted value, and the legal system structurally protects their position.
% FOUNDING_PROBLEM: The Crown and Māori leadership faced acute coordination problems in 1840: British settlers were arriving rapidly and settling on Māori land without legal purchase or consent; Māori were dying from introduced diseases; there were no agreed-upon frameworks for land sales, governance of settlers, or protection of Māori interests. The treaty was framed as the solution: British law would govern settlers, Māori rights would be protected, and land could be transferred 'voluntarily' through the Crown rather than through uncontrolled settler seizure.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem was functionally solved within 30 years: settler governance was established (solved through legislative and military means), epidemic mortality was controlled (solved through medical and social changes unrelated to the treaty), and land transfers accelerated dramatically (achieved through the extraction mechanism itself). Historians and demographers document that the founding problem was addressable and was addressed, but the method of solution was not coordination — it was covert extraction masquerading as coordination. Crown judiciary and parliament initially treated the founding problem as irrelevant (the English text was treated as unambiguous law, not as a negotiated solution to a specific problem). Contemporary Crown acknowledgements (the 1975 Treaty of Waitangi Act recognizing the Māori text, official apologies by Crown ministers, Waitangi Tribunal findings) confirm that the founding problem is dead and the mechanism is now visible as extraction operating under mistranslation. Historians and indigenous scholars document that the founding-problem narrative has become purely theatrical — the constraint persists not to solve coordination but to maintain the extracted distribution.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__retrospective_snare_exposure, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is extremely high (0.89) because the mechanism strips Māori of land (95%+ dispossession), governance authority (tino rangatiratanga), and legal standing (denied until late 20th century). The extraction is masked as 'legitimate treaty implementation' and 'rule of law.' Suppression is nearly total (0.92) because Māori resistance is met with military suppression (1860–1872), legislative denial (voting rights, personhood, court standing), and contemporary legislative override (resource law, marine coastal area). Theater is high (0.78) because the remedy structures (Waitangi Tribunal, apologies, settlements, co-governance) are now primarily performative: they acknowledge the wrong while refusing to overturn the core legal fiction that dispossession is permanent and the English text controls. Accessibility collapse is high (0.88): once the English text's supremacy is established and enforced through courts, no alternative (tino rangatiratanga under the Māori text) is accessible to Māori. Resistance is substantial (0.71) because Māori have continuously resisted (the New Zealand Wars, land rights movements, treaty claims) even though resistance has been largely ineffective at stopping the extraction. The snare classification is structural: the mechanism was covert at signing (victims did not understand the English claim), remained hidden behind legal legitimacy for 150+ years, and only became fully visible when linguists and historians exposed the textual divergence. The Crown's enforcement of the English text despite the documented divergence proves the snare persists even in its exposed state.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown judiciary and parliament's seat, the constraint is 'rule of law': the English text is the binding legal instrument, court rulings are impartial, and legislative override is necessary to manage a changing society. From the Māori signatories' and descendants' seat, the same structure is 'systematic fraud': they signed a text they understood (Māori version), the Crown invoked a text they could not read (English version) to claim power they never ceded, and they were dispossessed through legal forms that denied them standing to contest the mechanism. The engine computes per-seat classification: from the Crown seat, this might appear as tangled rope (coordination plus extraction) until the structural dependence on mistranslation is registered; from the Māori seat, it is clearly snare (extraction mechanism covert at operation, visible only retrospectively, with no exit available). The measurement series tracks how the mechanism evolved: from covert at signing to enforced through courts and legislation to exposed through scholarship to persisting despite exposure through legislative override.
 *
 * DIRECTIONALITY LOGIC:
 *   Māori signatories and descendants are the full targets of extraction (d = 1.0): they were trapped at signing by the textual divergence they could not understand, dispossessed over 150+ years, denied legal standing and governance authority, and offered only partial and contested remedies. Their exit is retroactively impossible: they cannot undo the signing or the dispossession, and contemporary legal structures deny them the authority they believe they retained. The Crown land-purchasing apparatus is the full beneficiary (d = 0.0): it set the terms, used the textual divergence to claim sovereignty, collected the extracted value (land, wealth, governance authority), and continues to enforce the extraction despite exposure. English-speaking settlers are secondary beneficiaries (d = 0.0 to 0.2): they inherited the extracted value but did not orchestrate the mechanism. Contemporary Māori governance structures and the settler public are at intermediate positions: partially benefiting from some recognition while carrying the cost of an ongoing extracted state. No directionality override is needed: the structural data (beneficiary/victim + trapped exit + powerless power for Māori, arbitrage exit + institutional power for Crown) derive the correct directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids misclassification as rope because the founding problem (establishing a peaceful coexistence framework) is dead: it was functionally solved within 30 years (settler governance established, epidemic mortality addressed through non-treaty causes, land transfers accelerated). The constraint persists not to solve the founding problem but to maintain the extraction. The 'coordination' narrative (two authority structures coexisting peacefully) is the theater that covers the snare: the English text claims total sovereignty (incompatible with Māori tino rangatiratanga), and that claim is enforced through courts and legislation despite the Māori text's clearly different meaning. The snare classification holds because: (1) the mechanism was covert at operation (victims did not understand the English claim), (2) it persists through active suppression and legislative override despite exposure, and (3) alternatives are completely foreclosed (the Māori text, tino rangatiratanga, and Māori governance are denied legal force by courts and Acts of Parliament). The mandatrophy diagnosis is confirmed by the founding_problem_status (dead) + disappearance_verdict (world_rearranges) mismatch: if the constraint disappeared, Aotearoa's entire property law, governance, and resource allocation would reorganize. This reveals that the founding-problem narrative is now purely theatrical — the extraction persists because beneficiaries cannot exit without losing the extracted value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covert_vs_intentional_fraud,
    'Was the textual divergence an intentional fraud (Crown officials knew the texts were inconsistent and deliberately presented the Māori version to secure signatures while invoking the English version for land claims), or an accident of translation and interpretation that calcified into systematic extraction?',
    'Historical document review: Crown correspondence, Hobson''s instructions, drafting notes. Did Crown lawyers anticipate the divergence? Did they deliberately create it? Did they rely on it in subsequent claims?',
    'If intentional fraud, the constraint is a deliberate snare from inception. If accidental calcification, it is a snare-forming tangled rope (coordination attempt corrupted by mistranslation into extraction). The classification stays snare under both resolutions, but the founding narrative changes: planned predation vs. pathological institutional drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_vs_intentional_fraud, empirical, 'Whether the textual divergence was intentional Crown fraud or accidental calcification of misinterpretation.').

omega_variable(
    translation_understanding_at_signing,
    'How much did individual chiefs understand about the English text and its implications at the moment of signing? Did any chiefs understand that ''sovereignty'' was being claimed in English, or were all signatories acting on their understanding of the Māori text alone?',
    'Contemporary accounts, missionary records, hui transcripts, later testimony from signatories or their immediate descendants. Did any chief state they understood the English claim to sovereignty?',
    'If all chiefs signed understanding only the Māori version (most likely scenario), the snare is covert from inception and justified as fraud/deception. If some chiefs understood both versions and signed anyway, it becomes a contested transaction (some agency despite information asymmetry). The snare classification holds either way, but the agency and intent attribution changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(translation_understanding_at_signing, empirical, 'Whether chiefs understood the English sovereignty claim at the moment of signing, or acted under information asymmetry.').

omega_variable(
    remedial_scope_limitation,
    'Why does Crown legal interpretation refuse to overturn the English text''s primacy and retrospectively restore Māori authority under the Māori text, even after the divergence is documented and accepted as fact?',
    'Contemporary Crown legal positions (court rulings, statute law, government statements). Is the refusal grounded in ''settled law'' doctrine (property law is too embedded to overturn), in Crown sovereignty doctrine (Parliament cannot be bound by judges), or in cost-benefit reasoning (restoring tino rangatiratanga would require wholesale redistribution of property and governance)?',
    'If grounded in doctrine, the constraint persists because the legal system structurally protects dispossession. If grounded in cost-benefit, the constraint is a deliberate choice not to remedy despite knowledge of fraud. Either resolution confirms the snare persists despite exposure: the beneficiaries will not exit even when the mechanism is known because the exit cost exceeds the benefit to those with power to change it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remedial_scope_limitation, conceptual, 'Why Crown law refuses to treat the Māori text as controlling despite documented divergence and admitted superiority in meaning.').

omega_variable(
    identity_lock_maori_governance,
    'To what extent is Māori identity (iwi, hapū, whānau) inseparable from the claim to tino rangatiratanga? If Māori identity were severed from the governance claim, would the resistance to the snare persist, or would the identity-governance fusion make exit structurally impossible?',
    'Qualitative research: do Māori view tino rangatiratanga as a separable political claim (which could be abandoned), or as constitutive of Māori identity itself (which cannot be abandoned without ceasing to be Māori)? What happens to Māori self-determination if they accept the English text''s legal supremacy?',
    'If governance is constitutive of identity, Māori are identity-locked to resistance despite trapping: exit would be self-negation. If governance is a separable claim, Māori have a theoretical exit (accept dispossession, retain cultural identity) that is practically blocked by institutional suppression. Either way, the snare classification holds, but the suppression mechanism changes: one is cultural/relational, the other is institutional/legal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_maori_governance, empirical, 'Whether Māori resistance to the snare is structurally tied to identity (making exit self-negation) or is a separable political claim.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the retrospective_snare_exposure reading logically foreclose the crown_cession_reading and rangatiratanga_retention_reading, or do all three readings coexist as live but incompatible positions held by different parties?',
    'Logical analysis: if textual divergence is proven, does the crown_cession_reading''s claim that ''the English text is the binding authoritative source'' remain intellectually defensible? Or is it foreclosed by the fact that signatories could not have understood the English claim? Similarly, does documenting the divergence strengthen the rangatiratanga_retention_reading''s claim that the Māori text should control, or merely add empirical support?',
    'If retrospective_snare_exposure forecloses crown_cession_reading, then the legal legitimacy of 150+ years of Crown rule is undermined retroactively. If coexists_with holds, then the readings remain in active dispute despite the documented divergence, and the snare persists because the Crown can invoke crown_cession_reading despite retrospective exposure. Foreclosure would be a gateway to remediation; coexistence allows persistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the retrospective snare exposure reading logically rules out (forecloses) the Crown cession reading and Rangatiratanga retention reading, or whether readings coexist despite the documented divergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.0).
narrative_ontology:measurement(trea_tr_t1880, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1880, 0.42).
narrative_ontology:measurement(trea_tr_t1920, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1920, 0.68).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1975, 0.72).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2000, 0.75).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2024, 0.78).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.0).
narrative_ontology:measurement(trea_be_t1880, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1880, 0.72).
narrative_ontology:measurement(trea_be_t1920, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1920, 0.85).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1975, 0.87).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2024, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.0).
narrative_ontology:measurement(trea_su_t1880, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1880, 0.68).
narrative_ontology:measurement(trea_su_t1920, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1920, 0.81).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1975, 0.88).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2000, 0.91).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession__crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession__rangatiratanga_retention_reading).

% DUAL FORMULATION NOTE:
% This constraint (retrospective_snare_exposure) is one reading of the contested kernel 'treaty_authority_cession'. The kernel is the Treaty of Waitangi itself — a foundational text interpreted radically differently by the Crown, Māori, and judicial authorities. The sibling constraints model the crown_cession_reading (English text controls, kāwanatanga = sovereignty, treaty completed legal cession) and the rangatiratanga_retention_reading (Māori text controls, kāwanatanga limited to settler governance, tino rangatiratanga retained, ongoing partnership). These three readings are structurally distinct constraints with different ε values, beneficiary/victim sets, and classifications. The retrospective_snare_exposure reading instantiates a third interpretation: that the textual divergence IS the mechanism of extraction — the Crown used mistranslation to prevent chiefs from assenting to the English sovereignty claim, then enforced that claim through courts and legislation. The snare classification is specific to this reading. The three readings coexist as live but mutually incompatible positions held by different institutional actors (Crown, Māori, courts, historians). All three are documented in the same corpus of source materials; they differ in which text is treated as primary, what the terms mean, and what remedies are available.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
