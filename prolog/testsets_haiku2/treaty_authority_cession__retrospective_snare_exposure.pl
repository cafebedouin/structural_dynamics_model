% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__retrospective_snare_exposure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Treaty Cession via Textual Divergence: Retrospective Snare Exposure
 *   domain: constitutional/indigenous-rights/colonial-history
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) exists in two texts: Māori and English.
 *   Chiefs signed the Māori version, which stated that the British Crown
 *   would hold 'kāwanatanga'
 *   (governance/authority-over-settlers-and-disputes) while Māori retained
 *   'tino rangatiratanga' (absolute chieftainship/sovereignty over lands and
 *   peoples). The English version, which the Crown treats as authoritative,
 *   states that the chiefs ceded 'sovereignty' to the Crown—a different and
 *   incompatible claim. This textual divergence was not accidental: different
 *   colonial agents drafted different texts for different audiences. For over
 *   130 years, the Crown operationalized the English version through courts,
 *   legislation, land confiscation, and military force while the Māori
 *   signatories and their descendants believed a different arrangement had
 *   been made. Only in the 1970s–1980s did historical scholarship establish
 *   the divergence beyond dispute. Yet the Crown's claimed authority—and New
 *   Zealand's entire property law and constitutional order—rest on the
 *   mistranslated English text. This reading instantiates the constraint as a
 *   retrospective snare: a covert extraction mechanism that only became
 *   visible after the extraction had already occurred and hardened into
 *   institutional fact. The beneficiary is the Crown land-purchasing
 *   apparatus and the colonial state's legitimacy; the victims are the Māori
 *   signatories who could not assent to what they could not read, and their
 *   descendants, who inherited the dispossession.
 *
 * KEY AGENTS:
 *   - maori_signatories: Chiefs who signed in Māori, believed they had negotiated partnership, retained rangatiratanga
 *   - crown_land_purchasing_apparatus: Institutional beneficiary, enforcer, defender of the mistranslated cession doctrine
 *   - crown_legal_authorities: Courts and Parliament that interpreted and enforced the English version as supreme
 *   - maori_descendants: Inherit dispossession and identity-lock under an arrangement they cannot exit
 *   - english_settlers_and_colonists: Incidental beneficiaries of the divergence; received land titles authenticated by the mistranslated treaty
 *   - colonial_historians_and_scholars: Retrospective observers who established the factual divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.89).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.91).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.89).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty Cession via Textual Divergence: Retrospective Snare Exposure").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional/indigenous-rights/colonial-history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, 'd6dd9af7-5991-478e-95e1-9d54c18007d1').
narrative_ontology:cs_kernel_codification('d6dd9af7-5991-478e-95e1-9d54c18007d1', fixed_text).
narrative_ontology:cs_authority_grounding('d6dd9af7-5991-478e-95e1-9d54c18007d1', extraction).
narrative_ontology:cs_interpretation_layer_present('d6dd9af7-5991-478e-95e1-9d54c18007d1').
narrative_ontology:cs_reading_relation('d6dd9af7-5991-478e-95e1-9d54c18007d1', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('d6dd9af7-5991-478e-95e1-9d54c18007d1', treaty_authority_cession__rangatiratanga_retention_reading, influences).
narrative_ontology:cs_axiom('d6dd9af7-5991-478e-95e1-9d54c18007d1', foundational, textual_divergence_enabled_covert_extraction).
narrative_ontology:cs_axiom_status(textual_divergence_enabled_covert_extraction, holdable).
narrative_ontology:cs_axiom_grounding('d6dd9af7-5991-478e-95e1-9d54c18007d1', textual_divergence_enabled_covert_extraction, empirically_contingent).
narrative_ontology:cs_axiom('d6dd9af7-5991-478e-95e1-9d54c18007d1', foundational, covert_extraction_delegitimizes_crown_cession_claim).
narrative_ontology:cs_axiom_status(covert_extraction_delegitimizes_crown_cession_claim, holdable).
narrative_ontology:cs_axiom_grounding('d6dd9af7-5991-478e-95e1-9d54c18007d1', covert_extraction_delegitimizes_crown_cession_claim, deontological).
narrative_ontology:cs_reference_frame('d6dd9af7-5991-478e-95e1-9d54c18007d1', treaty_signed_under_mutual_understanding_of_partnership).
narrative_ontology:cs_drift_state('d6dd9af7-5991-478e-95e1-9d54c18007d1', contemporary_post_tribunal_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d6dd9af7-5991-478e-95e1-9d54c18007d1', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatories).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendants).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, iwi_land_rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_legal_authorities).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, english_settlers_and_colonists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Chiefs who signed the Māori-language text of the Treaty of Waitangi, understanding 'kāwanatanga' (governance/authority-over-land) as limited to British management of settlers and disputes, retained 'tino rangatiratanga' (absolute chieftainship) over their lands. They bore the cost of signing a document whose English version contained a divergent cession clause; the trap was that no exit was semantically available — they could not 'unsign' without appearing to renege on a commitment they believed they had made to partnership. Their legal and practical power to refuse subsequent land confiscations was foreclosed by the Crown's unilateral interpretation of the English text.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_signatories, payer,
    moderate, generational, trapped, regional).

% Inherited the consequences of the treaty divergence: dispossession of lands, exclusion from governance, subordination under Crown law that claimed authority via the English text of a document their ancestors signed in Māori. Their identity as iwi members and kaitiaki (land guardians) became impossible to exercise under the imposed legal framework. Exit from the identity frame would require renouncing kinship and genealogical connection; no practical exit exists.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendants, payer,
    powerless, civilizational, identity_locked, national).

% Hold customary title and kaitiaki responsibilities under tikanga Māori (Māori law and custom) but are met with Crown legislative override (Land Wars confiscations, Native Land Court, Crown pre-emptive rights). Their alternatives are confined to Crown-authorized channels: litigating in Crown courts under Crown-authored law, petitioning Parliament, or organizing resistance that the Crown suppresses militarily. Practical exit requires abandoning both land and identity.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, iwi_land_rights_holders, payer,
    powerless, generational, constrained, regional).

% Sets the interpretive frame: the English text controls, 'cession' is absolute, Crown sovereignty is complete. Administers the dispossession through land courts, legislation, confiscation, and military enforcement. Benefits from the treaty's textual divergence by claiming it authorized cession while the Māori signatories believed they had retained rangatiratanga. Collects land rents and sovereignty rents across centuries. Has the power to reinterpret the treaty, change legislation, or negotiate redress but does not; instead actively defends the divergent reading in courts and political discourse.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Courts, Parliament, and Crown counsel who interpret and enforce the treaty via the English text. Their authority to adjudicate legitimacy derives from the Crown's sovereign claim, which the treaty (in English) purports to establish. They benefit from defending the divergent reading because admitting the mistranslation would undermine the foundational claim that legitimates their own authority. Exit would require Constitutional restructuring and power redistribution.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_legal_authorities, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, crown_legal_authorities, beneficiary).

% Received land title, settlement rights, and legal security derived from the Crown's claim to have acquired absolute cession. They benefited from the ambiguity without bearing the cost of the divergence. Over generations, they built wealth, political participation, and identity around land titles the Crown authenticated via the mistranslated treaty. Their exit is practical (migrate, sell land) but costly in terms of accumulated wealth and social position.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, english_settlers_and_colonists, beneficiary,
    organized, biographical, mobile, regional).

% Analyzed the textual divergence decades after the fact, producing historical scholarship showing that the Māori and English versions conveyed incompatible meanings. Their observations established the factual basis for recognizing the constraint as a snare—extraction via mistranslation. They hold no enforcement power but provide the epistemic frame that makes the retrospective exposure possible.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, colonial_historians_and_scholars, observer,
    analytical, biographical, analytical, global).

% Established (1975) to investigate Crown breaches of the treaty and recommend redress. Acknowledged the textual divergence and recommended remedies but has no enforcement power—the Crown accepts or rejects its findings, which means the tribunal remains structurally subordinate to the same authority whose divergent interpretation it is investigating. Its role is to testify to the divergence while remaining formally external to the extraction mechanism itself.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, waitangi_tribunal, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, waitangi_tribunal, excluded).

% Argue for reinterpretation or renegotiation of the treaty based on the textual divergence, sovereignty restoration, and land return. They are not in the room where the extraction is administered (courts, Cabinet, Parliament) and their exclusion is enforced by the Crown's control of the legal frame. They can petition, organize, and litigate within Crown-authorized channels, but fundamental renegotiation would require the Crown to voluntarily diminish its claimed authority—which the extraction mechanism itself makes unlikely.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, contemporary_maori_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:fixing_cost_class(treaty_authority_cession__retrospective_snare_exposure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At the moment of signing, the treaty was presented (in Māori) as establishing a governance partnership: Crown would manage relations between settlers and Māori, defend against external threats, and regulate settlers' conduct; Māori would retain tino rangatiratanga (absolute chieftainship) over their lands and peoples. The coordination problem was the collision of two political systems: British settler expansion and Māori territorial authority. The treaty offered a framework for coexistence rather than conquest.
% TRANSFER_FUNCTION: The constraint transfers absolute title and governing authority from Māori signatories to the Crown. The mechanism: the English text of the treaty, interpreted unilaterally by the Crown and enforced through courts, legislation, confiscation, and military action, authoritatively states that the chiefs ceded sovereignty. The Māori signatories transferred nothing in the Māori text they understood and signed; the transfer was unilaterally asserted in a version they could not read. Subsequent transfers of land (through the Native Land Court, pre-emption rights, confiscations, and legislative acts) are all predicated on this foundational divergence.
% ABSENT_VOICES: The chiefs who signed but are now dead; Māori ancestors whose descendants inherit the dispossession but had no voice in subsequent legislative and court decisions; Māori women (largely absent from the signing, thus absent from the formal treaty record); rival interpretive authorities (tikanga-based dispute resolution, Māori law) that were never admitted to the room where the divergent English text was applied. The Waitangi Tribunal (1975 onward) eventually spoke, but from a subordinate institutional position—their findings are advisory, not binding on the Crown.
% DISAPPEARANCE_RATIONALE: If the retrospective recognition of the snare were to materialize into constitutional redress—if the divergence were formally acknowledged, the English-only interpretation nullified, and Māori sovereignty or co-sovereignty over lands restored—the Crown's title to vast territories would evaporate, wealth would be redistributed, and political authority would be restructured. New Zealand's entire post-1840 legal and property order would require renegotiation. The constraint does not simply describe a historical wrong; it grounds the entire colonial state's property law, legislative authority, and international recognition as a sovereign kingdom. If it disappeared, the world would rearrange.
% FOUNDING_PROBLEM: British settlers in New Zealand were expanding into territories controlled by Māori iwi under tikanga Māori. Conflict over land, resources, and jurisdiction was escalating. The Crown sought to prevent inter-settler warfare over land claims and to establish Crown authority over settlers' conduct. Māori chiefs sought to regulate settler incursion, protect their lands, and maintain authority over their peoples. The Treaty of Waitangi (1840) was framed as the solution: a document that both Crown and Māori could sign to establish a mutual understanding of governance and land rights.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem—preventing settler-Māori conflict and establishing a framework for coexistence—was the stated rationale at the time (Governor Hobson's dispatches, missionary correspondence, Crown agents' reports). But by 1860–1880, the problem the treaty claimed to solve had already been solved differently: the Crown unilaterally seized vast territories, militarily defeated Māori resistance, and imposed Crown law. Scholars and the Waitangi Tribunal (from outside the benefiting Crown apparatus) have established that the founding problem was resolved not by treaty partnership but by military dispossession and legislative override. The treaty's founding rationale no longer governs its operation; the mechanism persists as pure extraction defended by circular invocation of the mistranslated English text.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   This constraint is a snare because: (1) the extraction mechanism—the textual divergence—was covert at the moment of operation (the chiefs could not read English; the Crown's agents exploited this); (2) persistence depends on active enforcement: courts uphold the English reading, Parliament maintains Crown sovereignty, property titles are authenticated via the mistranslation, and Māori legal challenges are defeated; (3) suppression is very high (0.91) because the entire constitutional and property order is built on the mistranslation—questioning it threatens foundational state legitimacy, which motivates aggressive defense; (4) theater ratio is high (0.72) because modern performance (apologies, settlements, cultural recognition, partnership rhetoric) coexists with the persistence of the core extraction (land dispossession, Crown sovereignty); (5) accessibility_collapse is high (0.78) because Māori alternatives are extremely constrained—they can litigate only in Crown courts, petition Parliament, or organize politically, but the legal frame itself is built on the mistranslation they would need to overturn, making escape from the constraint logically circular. The measurements show base_extractiveness rising from near-zero in 1840 (covert) to 0.89 in 2026 (hardened into property law and constitutional fact) even after the divergence became publicly known in the 1970s. This is the mark of a successful snare: retrospective exposure does not terminate the extraction, only makes the mechanism visible.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's institutional seat: the treaty ceded sovereignty in perpetuity (English text controls); subsequent land acquisitions were lawful exercises of Crown prerogative; settlements and acknowledgments are gracious redress for subordinates' hardships. This seat computes the constraint as legitimate state authority. From the Māori victim seat: the treaty promised partnership and retained rangatiratanga (Māori text is what they signed and understood); the English version was a unilateral forgery operationalized by conquest; land confiscations are theft sanctioned by Crown courts; settlements are token gestures that do not restore title or authority. This seat computes the constraint as dispossession masked by legal theater. The Crown seat has institutional power to enforce its interpretation and define what is 'lawful'; the victim seats have epistemic clarity about the divergence but no institutional power to overturn the frame. The engine's per-seat computation should diverge sharply: the Crown's analytical seat may compute the constraint as legitimate partnership or coordination, while the victim seats compute it as pure extraction under suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown land-purchasing apparatus and legal authorities are near full-target beneficiaries (d near 0.0): they collect sovereignty rents, land rents, legislative supremacy, and institutional authority derived from the cession claim. Their exit is arbitrage-class (they can reframe the treaty but choose not to). Māori signatories and descendants are full targets (d near 1.0): they bear the cost (dispossession, subordination, identity-lock), have trapped or identity_locked exit (cannot renounce kinship or land responsibility), and benefit from nothing the constraint offers. English settlers are secondary beneficiaries (d moderate, ~0.3): they receive land titles and security but did not author the divergence and could operate (though less profitably) under alternative property arrangements. Māori legal observers (tribunal, advocates) are near neutral (d ~0.5): they have no enforcement power, analyze the constraint from outside, and would benefit from redress but do not currently collect extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing settler-Māori conflict, establishing a framework for peaceful coexistence) was solved in 1865–1880, not by the treaty partnership it claimed to enable, but by military victory and legislative override. The Crown's unilateral seizure of territories, suppression of the Land Wars, and enactment of confiscatory legislation all violated the treaty's coordination function. By 1900, the mandate was dead: the problem the treaty was built to solve had been solved by conquest instead. Yet the treaty persists as the legal instrument that authenticates the Crown's sovereignty and property titles. This is textbook mandatrophy: a coordination device (the treaty) whose founding problem has been resolved by other means (military force, legislation) but which persists because the beneficiary (Crown) has rerouted it to serve extraction instead. The constraint is a snare precisely because the mandatrophy is structural: the Crown cannot resolve the contradiction (the treaty's real purpose is extinct) without also dissolving the sovereignty claim (the English-text cession) that legitimates its entire post-1840 authority. The retrospective snare exposure makes this mandatrophy visible: the treaty's true function is now transparently extraction, not coordination. But the institutional inertia is so great that the constraint persists despite the exposure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_divergence_or_accident,
    'Was the textual divergence between Māori and English versions a deliberate colonial strategy, or an unintended consequence of hasty translation and multiple drafting authorities?',
    'Archival evidence of the drafting process, correspondence between colonial agents, and comparative analysis of how other colonial treaties were authored (India, Africa, Pacific). Evidence of prior knowledge that ''kāwanatanga'' and ''sovereignty'' were incommensurable would support intentionality.',
    'If intentional, the constraint is a snare by design: the extraction mechanism was engineered to be covert. If accidental, it is still a snare (the Crown operationalized the English version regardless of intent), but the characterization of the Crown''s agency changes from deliberate conspiracy to opportunistic exploitation of ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentional_divergence_or_accident, empirical, 'Whether the textual divergence was colonial strategy or translation accident').

omega_variable(
    crown_knowledge_of_divergence,
    'When did the Crown become aware of the Māori text''s alternative meaning, and did it actively suppress that knowledge to maintain the cession claim?',
    'Archival records of Crown legal and executive deliberations, 1840–1880. Evidence of conscious knowledge followed by deliberate suppression would establish bad faith; ignorance would shift the characterization to negligence or exploitation of ambiguity.',
    'If the Crown knew of the divergence and suppressed it, the snare is compounded by conspiracy—the extraction was not just covert but actively concealed. If ignorant until retrospectively exposed, the extraction is still a snare but the Crown''s current defense of the English reading (post-exposure) becomes unambiguously bad faith.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_knowledge_of_divergence, empirical, 'Crown consciousness and management of the textual divergence').

omega_variable(
    contra_proferentem_binding,
    'Is the contra proferentem rule (ambiguous contracts are construed against the drafter) binding on the Crown''s interpretation of the treaty, or does Crown sovereignty override ordinary contract principles?',
    'Constitutional law analysis and jurisdiction-specific precedent on treaty interpretation. If contra proferentem applies, the Māori text (favoring the signatories) should control; if sovereignty overrides it, the Crown''s unilateral interpretation is unfettered by contract law.',
    'If contra proferentem applies, the snare is legally remediable: courts could reinterpret the treaty in line with the Māori text, which would terminate the cession claim and require restitution. If sovereignty overrides it, the legal-remedial path is blocked; only constitutional or legislative reform could address the snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contra_proferentem_binding, empirical, 'Whether ordinary contract law constrains the Crown''s treaty interpretation').

omega_variable(
    remedy_impossibility,
    'Even if the snare were judicially recognized and the cession declared void, what is the practical and Constitutional basis for undoing 186 years of property conveyances, statutes, and Crown administrative action?',
    'Constitutional theory, comparative decolonization cases (India, Kenya, Israel/Palestine), and New Zealand political economy. Analysis of whether full restitution is possible, what partial remedies are feasible, and what structural obstacles exist.',
    'If remedy is impossible or prohibitively costly, the snare becomes unfixable despite recognition—a locked-in historical injustice. If remedy is possible (e.g., via constitutional amendment and negotiated land return), exposure of the snare could translate to justice; if impossible, exposure becomes a permanent epistemic wound without redress.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedy_impossibility, preference, 'Practical feasibility of remediating the snare after retrospective exposure').

omega_variable(
    kernel_reading_distinction,
    'This reading (retrospective snare exposure) treats the divergence as the EXTRACTION MECHANISM itself. The rangatiratanga_retention reading treats it as evidence that the partnership was real and the Crown breached it. How do these two readings differ in their treatment of the same textual fact?',
    'Comparative analysis of the two readings'' core premises about what the divergence means. The snare reading says: the two-text structure enabled unilateral extraction covertly. The retention reading says: the two-text structure proves the Māori understanding was partnership, which the Crown then violated. Both agree the texts diverge; they differ in what that divergence was FOR.',
    'The snare reading suggests no valid cession occurred at all—the treaty is void ab initio due to fraud/misunderstanding. The retention reading suggests the treaty is valid but the Crown breached it, triggering Crown liability for damages and breach remedies. These are different legal paths to different remedies. The snare reading implies restitutio in integrum (undo the whole thing); the retention reading implies specific performance (enforce the partnership) or damages (compensate the breach).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Structural difference between snare-exposure and retention readings of the same divergence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.0).
narrative_ontology:measurement(trea_tr_t1860, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1860, 0.2).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1900, 0.4).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1975, 0.65).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2000, 0.7).
narrative_ontology:measurement(trea_tr_t2026, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2026, 0.72).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.0).
narrative_ontology:measurement(trea_be_t1860, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1860, 0.35).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1900, 0.72).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1975, 0.85).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2000, 0.87).
narrative_ontology:measurement(trea_be_t2026, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2026, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.0).
narrative_ontology:measurement(trea_su_t1860, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1860, 0.6).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1975, 0.87).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2000, 0.89).
narrative_ontology:measurement(trea_su_t2026, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2026, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__retrospective_snare_exposure, 0.25).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession__crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession__rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, native_land_court_confiscation_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, parliamentary_legislative_supremacy_doctrine).

% DUAL FORMULATION NOTE:
% The Treaty of Waitangi kernel admits at least three structurally distinct constraint readings: (1) crown_cession_reading — English text controls, absolute cession, Crown sovereignty is legitimate; (2) rangatiratanga_retention_reading — Māori text controls via contra proferentem, partnership is the valid arrangement, Crown breached it; (3) retrospective_snare_exposure (this reading) — the divergence itself is the extraction mechanism, enabling covert cession. These are not different metrics on one constraint; they are different constraints derived from different readings of the same kernel. Each reading has its own ε (cession reading: ε≈0 if legitimacy holds, ε high if unjust; retention reading: ε measures the breach damages; snare reading: ε measures the covert extraction). The network links these three stories so analysis can track how they influence each other: cession legitimacy depends on suppressing exposure (snare); retention redress depends on accepting partnership framing (retention); snare exposure requires rejecting both dominant readings. See commentary.kernel_context for the committer-frame details.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__retrospective_snare_exposure, institutional, 0.05).
constraint_indexing:directionality_override(treaty_authority_cession__retrospective_snare_exposure, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
