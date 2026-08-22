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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Treaty of Waitangi: Retrospective Snare Exposure (Mistranslation Mechanism)
 *   domain: constitutional/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) is the founding document of New Zealand's
 *   Crown sovereignty and Māori rights claims. It was negotiated and signed
 *   in two language versions: a Māori text and an English text. This
 *   constraint story documents one reading of the treaty: the retrospective
 *   snare exposure, in which textual divergence itself is the extraction
 *   mechanism. The Māori text, signed by chiefs, specified that Māori would
 *   cede kāwanatanga (governance/authority over British settlers) while
 *   retaining tino rangatiratanga (absolute authority over their own lands
 *   and peoples). The English text declared unconditional cession of
 *   sovereignty to the Crown. Māori signatories could not access or verify
 *   the English version; they signed believing they were retaining autonomy.
 *   The Crown, over the next 180+ years, enforced the English-text
 *   interpretation through legislation and land confiscation, while
 *   suppressing acknowledgment of the textual divergence. Only in the
 *   1970s–2000s, through historical scholarship and the Waitangi Tribunal,
 *   did the extraction mechanism become visible as a structured covert
 *   mechanism rather than a historical error. This reading treats the
 *   mistranslation not as a bug, but as the extraction mechanism itself: the
 *   Snare persisted because it was enforced as law, and the law's authority
 *   rested on a text that the signers had not read.
 *
 * KEY AGENTS:
 *   - maori_signatories: Chiefs who signed the Māori version in 1840; they understood themselves to be retaining tino rangatiratanga (absolute authority) over their lands and peoples. They were the targets of the extraction mechanism — they paid in land, authority, and descendants' freedom.
 *   - maori_descendants: Inherited the consequences of the mistranslated treaty: dispossession, legal marginalization, suppression of te reo Māori, intergenerational poverty. They remain trapped within the legal order the mistranslation established.
 *   - crown_land_purchasing_apparatus: The institutional mechanism that enforced the English-text interpretation, used it to justify compulsory land purchase and confiscation, and built colonial authority on the foundation of 'legal' cession. The beneficiary structure — though this reading names no beneficiary, only the apparatus that extracted.
 *   - colonial_settler_population: Land grants, agricultural security, and legal titles derived from Crown land acquisition (obtained via treaty). They benefited from the Snare without designing it.
 *   - crown_legal_authority_structure: Courts, parliament, and the executive that maintained the English-text interpretation as binding law and suppressed the rangatiratanga reading for 135+ years. Its legitimacy depended on denying or minimizing the textual divergence.
 *   - treaty_interpretation_observers: Legal scholars, historians, and human-rights bodies whose alternative readings were excluded from binding interpretation until the Waitangi Tribunal's establishment in 1975.
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
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty of Waitangi: Retrospective Snare Exposure (Mistranslation Mechanism)").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, '25031920-1317-4705-a27f-3063f7c1cfd6').
narrative_ontology:cs_kernel_codification('25031920-1317-4705-a27f-3063f7c1cfd6', fixed_text).
narrative_ontology:cs_authority_grounding('25031920-1317-4705-a27f-3063f7c1cfd6', extraction).
narrative_ontology:cs_interpretation_layer_present('25031920-1317-4705-a27f-3063f7c1cfd6').
narrative_ontology:cs_reading_relation('25031920-1317-4705-a27f-3063f7c1cfd6', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('25031920-1317-4705-a27f-3063f7c1cfd6', treaty_authority_cession__rangatiratanga_retention_reading, influences).
narrative_ontology:cs_axiom('25031920-1317-4705-a27f-3063f7c1cfd6', foundational, textual_divergence_enables_covert_extraction).
narrative_ontology:cs_axiom_status(textual_divergence_enables_covert_extraction, holdable).
narrative_ontology:cs_axiom_grounding('25031920-1317-4705-a27f-3063f7c1cfd6', textual_divergence_enables_covert_extraction, empirically_contingent).
narrative_ontology:cs_axiom('25031920-1317-4705-a27f-3063f7c1cfd6', foundational, signatories_could_not_consent_to_unseen_text).
narrative_ontology:cs_axiom_status(signatories_could_not_consent_to_unseen_text, holdable).
narrative_ontology:cs_axiom_grounding('25031920-1317-4705-a27f-3063f7c1cfd6', signatories_could_not_consent_to_unseen_text, deontological).
narrative_ontology:cs_reference_frame('25031920-1317-4705-a27f-3063f7c1cfd6', partnership_framework_on_maori_terms).
narrative_ontology:cs_drift_state('25031920-1317-4705-a27f-3063f7c1cfd6', contemporary_post_tribunal_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('25031920-1317-4705-a27f-3063f7c1cfd6', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatories).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, colonial_settler_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Chiefs who signed the Treaty in 1840, reading it in te reo Māori. The Māori text (aukatoa/kaitiakitanga framing) presented a partnership with Crown governance of European settlers, not full sovereignty. They understood themselves to be retaining tino rangatiratanga (absolute authority) over their lands and peoples. At signature, they could not access or verify the English text that declared unconditional cession of sovereignty. They paid in land, authority, and descendants' freedom — the extraction only became visible when the Crown enforced the English-text interpretation and land confiscation accelerated in subsequent decades.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_signatories, payer,
    moderate, generational, trapped, national).

% Inherited the consequences of the mistranslated treaty: dispossession of ancestral lands, legal marginalization, suppression of te reo Māori and cultural authority, intergenerational poverty and health impacts. The extraction mechanism that produced these outcomes was the covert textual divergence — something their ancestors could not have detected or consented to at the moment of signing. They remain trapped within the legal order the mistranslation established, with limited recourse because the text they signed and the text that controls them are two different claims.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendants, payer,
    powerless, civilizational, trapped, national).

% The Crown administration that enforced the English-text interpretation of the Treaty, used it to justify compulsory land purchase, Crown grants, and legislative confiscation, and built colonial authority on the foundation of 'legal' cession obtained through mistranslation. The apparatus benefited from the textual divergence: it could present itself as acting lawfully while simultaneously crushing Māori resistance as treaty breach. The extraction persisted because it was enforced as law, and the law's authority rested on the English text that the signers had not read.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Land grants, agricultural settlement security, and legal title derived from Crown land acquisition (obtained via treaty) enabled settler agricultural expansion and wealth accumulation. They did not design the mistranslation mechanism but benefited from its operation — the Snare persisted because settler interests aligned with Crown land acquisition, and land tenure security depended on enforcing the English-text interpretation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, colonial_settler_population, beneficiary,
    organized, generational, mobile, national).

% Courts, parliament, and the executive that maintained the English-text interpretation and enforced it as the binding law of the realm. The legal structure's legitimacy depended on treating the mistranslation as either non-existent (denialism) or immaterial (the English text was what mattered; the Māori version was auxiliary). Changing this would require either acknowledging systematic fraud at the Crown's founding moment, or adopting a contra proferentem reading (ambiguity defaults to the other party) that subordinates the English text. The structure persisted in enforcing extraction because institutional inertia and reputational risk made reversal costly.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_legal_authority_structure, agenda_setter,
    institutional, civilizational, analytical, national).

% Legal scholars, historians, and human-rights bodies who analyzed the textual divergence and its extraction mechanism. They are excluded from the binding interpretation process — New Zealand courts retained interpretive authority for decades, and only since the late 20th century have alternative readings gained institutional purchase through the Waitangi Tribunal. Their exclusion meant that the mistranslation remained officially invisible as an extraction mechanism until long after its effects were locked in.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, treaty_interpretation_observers, excluded,
    powerful, biographical, constrained, national).

% Established 1975 to investigate Crown breaches of the Treaty. It has authored retrospective analysis of the mistranslation mechanism and made non-binding recommendations for remedies and historical acknowledgment. It lacks enforcement power over Crown policy but has legitimated the rangatiratanga reading within New Zealand discourse, making the Snare's extraction mechanism visible retrospectively.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:fixing_cost_class(treaty_authority_cession__retrospective_snare_exposure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At the time of signing (1840), the constraint presented as establishing a shared governance arrangement: the Crown would govern British settlers; Māori chiefs would retain authority over their lands and peoples. This framing depended entirely on the Māori text, which specified that Māori would cede kāwanatanga (governance of British colonists) while retaining tino rangatiratanga (absolute authority over their own territories).
% TRANSFER_FUNCTION: Land, political authority, legal sovereignty, and the capacity for self-determination: moved from Māori signatories and their descendants to the Crown and settler polity. The mechanism: the Crown presented the Māori text as binding while enforcing the English-text interpretation in law and land administration. Māori signed believing they were retaining authority; the Crown enforced a reading that extinguished it.
% ABSENT_VOICES: Legal scholars, historians, and Māori advocates who understood the mistranslation mechanism were excluded from binding interpretation for 135+ years. The Crown's own legal advice acknowledged the textual divergence in private; public law suppressed it. Rival interpretations (the rangatiratanga-retention reading) were treated as historical grievance rather than live law until the Waitangi Tribunal's establishment.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared — if the English-text interpretation lost legal force and the Māori text (or a contra proferentem reading) became binding law — the Crown's land titles would rest on a different legal foundation, Māori self-determination would require ongoing Crown negotiation rather than unilateral legislation, and the 180+ years of confiscation would become visible as breach rather than law. New Zealand's entire property regime and constitutional order rest on this constraint's enforcement.
% FOUNDING_PROBLEM: European colonization of Aotearoa (New Zealand) in the 1830s–1840s created conflict over land rights, settler security, and the basis of Crown authority. The Crown and Māori chiefs negotiated a treaty to establish an agreed framework. The founding problem was: how to secure both settler confidence in land tenure and Māori confidence in retained autonomy, when those interests were in tension.
% FOUNDING_PROBLEM_CORROBORATION: The Crown, in 1840, attested the founding problem as live — settlement required a legal framework. Māori signatories attested they understood the Māori text as solving it through partnership. Modern scholarship (independent historians, the Waitangi Tribunal, human-rights bodies) attests the founding problem was solved differently than signatories understood: the English text created unilateral Crown sovereignty, not partnership. The Crown suppressed this finding for 135 years; it is now established in academic and legal circles outside the initial benefiting parties.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is 0.89 at the interval end (2026): the constraint has stripped Māori of land (over 90% of the country by 1975), political authority, legal standing, and the capacity for self-determination — one of the largest and most complete extractions in the historical record. Suppression is 0.91 because the constraint persisted through active enforcement (land confiscation legislation, criminal suppression of resistance, legal denial of alternative interpretations) and through internalized suppression (te reo Māori was made illegal; Māori children were punished for speaking it; institutional marginalization taught generations that Crown authority was inevitable). Theater ratio is 0.72 because, for 130+ years, the Crown performed lawfulness while systematically extracting — the performance of legal authority was essential to the Snare's operation, and that performance was most intense during the period of fastest land confiscation. The measurement series captures the constraint's evolution: at 1840 (signature), extractiveness is very low (0.15) because the mechanism was covert — the signers believed they had solved the founding problem via partnership. From 1870 onwards (Crown land confiscation accelerates), extractiveness rises sharply to 0.42, then continues accumulating to 0.89 by 2026. Theater rises fastest from 1920–2000, reflecting the institutional necessity to justify extraction as lawful even as its scope became undeniable. Suppression rises and plateaus at 0.89–0.91, reflecting the mechanisms (legal exclusion, cultural suppression, economic marginalization, intergenerational trauma) that held the extraction in place. The claim/metric independence is intentional: this reading claims the constraint is a Snare (pure extraction, coercive, with identifiable victims and no genuine coordination function), and the metrics bear that out — but the constraint was PRESENTED at signature as a Rope (partnership, coordination, mutual benefit). The divergence between the crown_cession_reading (which would claim Rope at signature and through to present) and this reading (which claims retrospective Snare exposure) is the engine's subject matter.
 *
 * PERSPECTIVAL GAP:
 *   The Crown's institutional seat (crown_legal_authority_structure) experienced and continues to experience the constraint as lawful authority derived from a valid treaty cession. From this seat, the English text is the binding version, the Māori text is a translation aid, and the constraint is law, not extraction. The Māori victim seats (maori_signatories, maori_descendants) experienced and continue to experience the constraint as covert extraction operating under mistranslation — they signed one document and are governed by another. Their exit options are trapped: they cannot un-sign, cannot repudiate the text they actually signed (the Māori version), and cannot escape the legal order built on the English-text interpretation without institutional remedies outside their power. The settler beneficiary seat (colonial_settler_population) did not design the mechanism but benefits from the extraction it produces — they experience the constraint as legitimate land tenure and legal order, because that is what the extraction apparatus made it. The observer seats (treaty_interpretation_observers, waitangi_tribunal) can see the mechanism but lack enforcement power to change it; they can make it visible retrospectively but cannot undo 180 years of confiscation.
 *
 * DIRECTIONALITY LOGIC:
 *   Māori signatories and descendants carry d approaching 1.0 (full targets): the constraint extracts from them, they are trapped, and they bear the suppression necessary to hold the extraction in place. The Crown apparatus carries d near 0.5 (symmetric, in a technical sense — it runs the mechanism but is also bound by the legal fiction that justifies it). Colonial settlers carry d near 0.0 in the extraction, but d closer to 0.3 in the suppression (they benefit from stable land tenure that extraction produces, but they do not bear the suppression cost directly; Māori bear it). The extraction's directionality is clear: Māori pay, Crown and settlers collect. The mechanism's invisibility (until 1970s) is what made the Snare work — it persisted because both Crown and settlers could deny its extractive character while actively extracting.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (how to secure settler confidence in land tenure while respecting Māori autonomy) is DEAD at the interval end. The founding problem was solved in 1840 at signature — signatories believed they had achieved a framework that addressed both interests. By 1860–1880, the Crown had unilaterally reinterpreted the treaty to assert full sovereignty and began confiscation; by 1975, Māori had lost over 90% of their land and political authority. The constraint persists not to solve the founding problem but to lock in the extraction: the English-text interpretation is now defended by 180 years of legislation, court precedent, and property rights vested in Crown and settlers. To acknowledge the founding problem is dead and the constraint is pure extraction would require unwinding those rights and accepting that New Zealand's property regime rests on breach of treaty. The Crown legal structure resists this acknowledgment (denialism of the textual divergence; minimization of its effects) and the settler population benefits from the extraction it produces. This is a classic mandatrophy case: the constraint's founding function is gone, but it persists because the extraction it produces is now locked in, and the cost to undo it exceeds the political will of those who would have to bear the cost. The Waitangi Tribunal has explicitly named this dynamic: the Crown violated the Treaty; remedies would require returning land and authority, which Crown and settler interests resist. The theater_ratio's rise from 0.05 to 0.72 reflects this mandatrophy: the Crown increasingly performs historical acknowledgment and cultural respect while resisting substantive remedy, because acknowledging breach without remedying it maintains the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(trea_tr_t1870, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1870, 0.18).
narrative_ontology:measurement(trea_tr_t1920, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1920, 0.35).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1975, 0.58).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2000, 0.68).
narrative_ontology:measurement(trea_tr_t2026, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2026, 0.72).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.15).
narrative_ontology:measurement(trea_be_t1870, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1870, 0.42).
narrative_ontology:measurement(trea_be_t1920, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1920, 0.68).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1975, 0.81).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2000, 0.86).
narrative_ontology:measurement(trea_be_t2026, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2026, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.62).
narrative_ontology:measurement(trea_su_t1870, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1870, 0.76).
narrative_ontology:measurement(trea_su_t1920, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1920, 0.84).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1975, 0.89).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(trea_su_t2026, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2026, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession__crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession__rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, maori_land_confiscation__legislative_override).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, te_reo_maori__suppression_and_revival).

% DUAL FORMULATION NOTE:
% The Treaty of Waitangi kernel decomposes into three structurally distinct constraints, each instantiating a different reading: crown_cession_reading (Mountain or Rope, depending on whether the English-text interpretation is treated as natural law or human-imposed order), rangatiratanga_retention_reading (Rope or Tangled Rope, depending on whether partnership can be established without remedying past extraction), and retrospective_snare_exposure (Snare — the reading you are reading now). The three readings share the same kernel (the treaty text and 180+ years of practice) but produce different epsilon values depending on how the referent is framed: the crown_cession_reading treats the constraint as solving a coordination problem (settlers need security, Crown needs authority); the rangatiratanga_retention_reading treats it as a coordination mechanism with asymmetric extraction baked in (coordination was promised but never delivered); this reading treats the textual divergence itself as the extraction mechanism, making the founding claim (partnership) structurally impossible to satisfy given the textual divergence. These are not different measurements of one constraint — they are three different constraints with three different epsilon values. ε for the crown_cession_reading is low (the Crown narrative: lawful authority, valid cession, no extraction). ε for the rangatiratanga_retention_reading is moderate-to-high (extraction occurred through breach of the partnership promise). ε for this reading (retrospective_snare_exposure) is very high (the extraction mechanism was embedded in the textual divergence from inception, making the other party's consent to the English-text meaning structurally impossible). The three readings are linked via network.affects_constraints: each reading's viability constrains the others' epistemological purchase.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
