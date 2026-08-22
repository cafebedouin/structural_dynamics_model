% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__crown_cession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Treaty Authority Cession — Crown Sovereignty Reading
 *   domain: constitutional/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) exists in two texts: an English version and
 *   a Māori-language version. This constraint story instantiates the Crown's
 *   reading: that the English text controls, 'kāwanatanga' (appearing in the
 *   Māori text) equals full sovereignty, and the treaty completed the legal
 *   cession of Aotearoa New Zealand to the British Crown. Under this reading,
 *   the Crown's legislative authority is absolute, Māori customary authority
 *   is subordinate or extinguished, and land alienation by the Crown is
 *   legitimate. This is ONE READING of the contested Treaty kernel. The
 *   sibling readings—the rangatiratanga retention reading and the
 *   retrospective snare exposure reading—instantiate different understandings
 *   of what the treaty means, what the signatories understood, and what text
 *   controls. This story generates ONLY the Crown cession reading as a
 *   structurally clean constraint; the measurement series and commentary
 *   explain why the Crown's reading, though textually grounded, operates as
 *   an extractive constraint when measured against what the Māori signatories
 *   understood and what contemporary scholarship attests.
 *
 * KEY AGENTS:
 *   - Crown authority: Defines and enforces the English-text reading; controls the legal framework interpreting the treaty; sets agenda on what 'kāwanatanga' means
 *   - Settler colonial administration: Benefits from the reading that legitimizes land alienation, unilateral legislation, and legislative supremacy
 *   - Māori iwi leadership: Identity-locked bearers of customary claims that the Crown reading declares invalid; cannot exit without abandoning territorial foundation claims
 *   - Māori people: Powerless subjects of legislation enacted without their consent under the claimed absolute sovereignty
 *   - Treaty translation community / historians: Observers who document the textual divergence and the chiefs' stated understanding (evidence structurally excluded from the authoritative legal reading)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.87).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.79).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, tangled_rope).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Treaty Authority Cession — Crown Sovereignty Reading").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, '28e6f5aa-27d1-471c-8a8e-dcb0258b119b').
narrative_ontology:cs_kernel_codification('28e6f5aa-27d1-471c-8a8e-dcb0258b119b', fixed_text).
narrative_ontology:cs_authority_grounding('28e6f5aa-27d1-471c-8a8e-dcb0258b119b', extraction).
narrative_ontology:cs_interpretation_layer_present('28e6f5aa-27d1-471c-8a8e-dcb0258b119b').
narrative_ontology:cs_reading_relation('28e6f5aa-27d1-471c-8a8e-dcb0258b119b', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('28e6f5aa-27d1-471c-8a8e-dcb0258b119b', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('28e6f5aa-27d1-471c-8a8e-dcb0258b119b', foundational, english_text_supremacy).
narrative_ontology:cs_axiom_status(english_text_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('28e6f5aa-27d1-471c-8a8e-dcb0258b119b', english_text_supremacy, conventional).
narrative_ontology:cs_axiom('28e6f5aa-27d1-471c-8a8e-dcb0258b119b', foundational, kawanatanga_equals_absolute_sovereignty).
narrative_ontology:cs_axiom_status(kawanatanga_equals_absolute_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('28e6f5aa-27d1-471c-8a8e-dcb0258b119b', kawanatanga_equals_absolute_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('28e6f5aa-27d1-471c-8a8e-dcb0258b119b', secondary, complete_legal_cession_accomplished).
narrative_ontology:cs_axiom_status(complete_legal_cession_accomplished, holdable).
narrative_ontology:cs_axiom_grounding('28e6f5aa-27d1-471c-8a8e-dcb0258b119b', complete_legal_cession_accomplished, conventional).
narrative_ontology:cs_reference_frame('28e6f5aa-27d1-471c-8a8e-dcb0258b119b', crown_sovereign_legal_supremacy).
narrative_ontology:cs_drift_state('28e6f5aa-27d1-471c-8a8e-dcb0258b119b', contemporary_post_treaty_settlement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('28e6f5aa-27d1-471c-8a8e-dcb0258b119b', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, crown_authority).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, settler_colonial_administration).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_iwi_leadership).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Crown administers the interpretation that 'kāwanatanga' in Article Two of the Treaty of Waitangi (1840) transfers full sovereignty and legislative supremacy to the British Crown. Enforces this reading through land courts, legislation, and constitutional doctrine. Sets and modifies the legal framework interpreting the treaty. Controls the official English-language version as the authoritative text.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, crown_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Land administration, legislative authority, and economic policy benefit from the reading that the Crown obtained absolute cession of territory and sovereignty. Land alienation legitimacy, absence of requirement for ongoing Māori consent to major policy, and uncontested legislative supremacy all flow from this reading's establishment.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, settler_colonial_administration, beneficiary,
    institutional, civilizational, arbitrage, national).

% Signed the Māori-language version of the treaty believing they were ceding 'kāwanatanga' (governance/authority) while retaining 'tino rangatiratanga' (absolute chieftainship/control of property) over their lands and taonga. Face a legal and constitutional framework that declares their understanding invalid and subordinates their customary authority to Crown sovereignty. Cannot exit the constraint without rejecting foundational Treaty-era agreements or abandoning their people's territorial claims.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_iwi_leadership, payer,
    organized, civilizational, identity_locked, national).

% Governed by legislation enacted without their consent under the Crown's claimed absolute legislative supremacy. Land alienated and sold under the reading that the Crown obtained full cession. Subject to policies (health, education, criminal justice) designed and implemented unilaterally by Crown authorities. Cannot withdraw from the constraint except through systemic constitutional change, which the constraint itself structurally impedes.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_people, payer,
    powerless, civilizational, trapped, national).

% Linguistic analysis shows 'kāwanatanga' in 1840 Māori usage did not mean absolute sovereignty but rather governance authority and authority to make law — 'tino rangatiratanga' was the phrase used to mean full, absolute control. Historians and treaty scholars document the divergence between English and Māori texts and the chiefs' stated understanding of what they were signing.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, treaty_translation_community, observer,
    analytical, generational, analytical, national).

% A legal principle (contra proferentem) holds that ambiguities in a contract are resolved against the drafter. If applied to the Treaty, ambiguities between the English and Māori texts would be read in favor of the Māori signatories (who did not draft the English version). This doctrine is structurally excluded from the treaty-interpretation framework that privileges the Crown's reading.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, contra_proferentem_doctrine, excluded,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(treaty_authority_cession__crown_cession_reading, contra_proferentem_doctrine).

% Substantial academic work from outside the Crown's benefiting parties documents what chiefs understood they were signing, the linguistic divergence between texts, and the unequal negotiating power at the treaty table. This evidence is present in the discourse but structurally subordinated to the official Crown reading in constitutional and land law.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, historical_scholarship, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__crown_cession_reading, crown_authority).
narrative_ontology:fixing_cost_class(treaty_authority_cession__crown_cession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The treaty reading creates a unified legal framework for property, governance, and sovereignty under a single Crown authority. This solves the coordination problem of establishing consistent law, property rights, and administration over a territory where previously multiple iwi held distributed authority. From the Crown's perspective, this is genuine coordination: one sovereign, one property law, predictable enforcement.
% TRANSFER_FUNCTION: Moves land title (from iwi to Crown and then to settlers), legislative authority (from distributed iwi/chief decision-making to Crown parliament), and customary decision-making power (from iwi to Crown courts and bureaucracy). The transfer is presented as consensual cession; the mechanism is the Crown's unilateral interpretation that 'kāwanatanga' meant absolute sovereignty, not shared governance or retained rangatiratanga.
% ABSENT_VOICES: The voices of the Māori signatories—what they believed they understood the treaty to mean—are structurally excluded from the authoritative legal interpretation that creates this constraint. The Treaty Grounds themselves in 1840 recorded chiefs' statements that they were NOT ceding lands absolutely and that the Crown's authority was to be shared. These voices are present in historical records and contemporary Māori oral traditions but structurally subordinated in the legal framework (land courts, constitutional doctrine) that defines and enforces the Crown's reading.
% DISAPPEARANCE_RATIONALE: If the Crown conceded that the treaty did not cede absolute sovereignty—if the English text did not control, or 'kāwanatanga' meant shared governance rather than absolute sovereignty, or the Māori text's retained 'tino rangatiratanga' meant what the signatories understood it to mean—the entire structure of New Zealand property law, legislative supremacy, and constitutional authority would require wholesale renegotiation. Land titles, Crown land revenues, the legal basis for unilateral legislation affecting Māori, and the constitutional structure that subordinates Māori authority would all be thrown into fundamental uncertainty.
% FOUNDING_PROBLEM: The British colonial administration required unambiguous legal sovereignty over the territory (to exercise unified authority), the legal ability to alienate land to settlers (to fund the colony and attract immigration), and uncontested legislative supremacy (to impose British law and institutions). The treaty, if read as creating a partnership with retained Māori authority, would leave all three in doubt. The Crown's reading solved all three by declaring the treaty a complete cession of sovereignty and land to the Crown.
% FOUNDING_PROBLEM_CORROBORATION: The Crown's own historical records (official correspondence, legislative debates, settler policy documents) from 1840 onward attest that colonial administrators believed they needed absolute sovereignty and land alienation rights to pursue settler colonization. By contrast, the signatories' understanding is attested in: (1) oral traditions and speeches recorded at the time (preserved by iwi), (2) contemporary letters from Māori leaders explicitly stating they were NOT ceding lands absolutely, (3) missionary accounts documenting chiefs' understanding, and (4) modern scholarship from historians, linguists, and legal scholars outside the benefiting parties. The founding problem (establishing unambiguous Crown sovereignty to enable administration and land settlement) is SOLVED as of 1890—Crown sovereignty is fully institutionalized, land alienation has transferred most arable land, and legislators operate under unchallenged supremacy. No administrator argues the Crown still needs to enforce the reading to maintain governance. The constraint persists not because the founding problem is live but because relinquishing the reading would require returning land, acknowledging Māori authority, and restructuring constitutional relationships—costs the beneficiaries of the current system refuse.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__crown_cession_reading, 0.87, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.87 at 2026) and rose substantially from 1840 (0.68) because the Crown reading's enforcement machinery intensified: early enforcement relied partly on the chiefs' negotiated acceptance and continued diplomatic relations; by the late 19th century, the constraint operated through unilateral land confiscation, legislative prohibition on Māori customary authority, and constitutional doctrine that declared the Crown's reading canonically true. Suppression rose from 0.55 to 0.79 because suppression of Māori customary law, Māori language use, Māori authority in policy-making, and Māori voice in treaty interpretation was not present at the signing but became systematically institutionalized. Theater rose from 0.15 to 0.42 because early enforcement (land purchase, legislation) had a genuine coordination function for European settlement; modern enforcement largely performs the reading's legitimacy rather than solving a live coordination problem—the constraint persists by constitutional doctrine and institutional inertia more than by solving an ongoing governance problem. The measurement series reflects ENFORCEMENT INTENSIFICATION: the constraint started with some negotiated acceptance and localized application; it became a total framework of sovereignty and property control by the 20th century. All measurements use the same time grid (1840, 1875, 1920, 1975, 2000, 2026) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The Crown's seat and the Māori iwi leadership's seat compute completely different types from the same constraint. The Crown sees a rope (genuine coordination, mutual benefit in establishing unified authority, lawful cession). The Māori reading sees a tangled rope or snare (coordination story deployed to cover asymmetric extraction, benefit accrues entirely to the Crown, suppression of alternatives that would honor what they understood the treaty to mean). The engine computes this divergence from the structural data: the Crown has arbitrage-level exit (it can reinterpret the treaty or negotiate new arrangements as a sovereign); Māori iwi have identity-locked exit (they cannot abandon the territorial claims the treaty anchors without cease to exist as distinct peoples); the beneficiary/victim declarations show who collects and who pays. The perspectival gap is structural, not merely evaluative.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown's directionality (d ≈ 0.15) comes from: beneficiary status (collects sovereignty and land revenues), powerful/institutional power, arbitrage exit (can reinterpret, negotiate, amend the constitution), and large scope. Māori iwi leadership's directionality (d ≈ 0.92) comes from: victim status (bears loss of authority and land), organized but subordinate power, identity-locked exit (cannot abandon treaty-anchored claims without group dissolution), and national scope. Powerless Māori people's directionality (d ≈ 0.98) comes from: victim status, powerless power atom, trapped exit, and national scope. The asymmetry drives the computed extraction differently for each seat—the Crown experiences a low χ (favorable), Māori iwi experience a high χ (unfavorable).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing unified Crown sovereignty over the territory to enable consistent administration and European settlement) was live and real in 1840. By 2026, this problem is fully solved and dead: Crown sovereignty is institutionalized, property law is uniform, and no administrator or legal scholar argues the Crown requires ongoing absolute dominion under the treaty to sustain governance. The constraint persists not because the problem it was built for still exists, but because the reading distributes massive benefit to Crown authority and settler property owners—land titles, legislative power, revenue streams, and national authority all depend on the reading that the treaty ceded absolute sovereignty and land. The rising theater_ratio (from 0.15 to 0.42) reflects this zombification: early enforcement (1840–1875) was against live Māori resistance and ongoing dispute about what the treaty meant; modern enforcement (1975–2026) is largely performative—the Crown reasserts the reading through legal precedent, constitutional doctrine, and institutional routine, not by solving any live coordination problem. This is classic mandatrophy: the founding problem is dead, the constraint persists by institutional inertia and beneficiary capture, and enforcement morphs from solving a real problem to performing legitimacy for an arrangement that would not survive explicit re-justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chiefs_understanding_reconstruction,
    'What did the chiefs who signed the treaty understand ''kāwanatanga'' to mean at the time of signing?',
    'Linguistic and historical analysis: 1840 Māori-language documents, oral histories from descendants, letters from Māori leaders, comparison with other treaty terms across iwi.',
    'If chiefs understood themselves as ceding only kāwanatanga (governance, not absolute control) while retaining tino rangatiratanga, the Crown''s reading operates as extraction via mistranslation. Computed type shifts toward snare for Māori seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chiefs_understanding_reconstruction, empirical, 'Reconstructing signatories'' understanding of treaty terms at signing.').

omega_variable(
    textual_divergence_deliberate_vs_honest,
    'Is the English/Māori textual divergence a product of honest translation difficulty or deliberate strategic drafting to secure different meanings?',
    'Historical analysis of text drafting process, linguistic comparison of translation intent, analysis of how British officials at the time understood ''kāwanatanga''.',
    'If deliberate, extraction mechanism is engineered deception. If honest, extraction is real but ambiguity-based rather than deliberately orchestrated. Affects legal remedies and moral standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_divergence_deliberate_vs_honest, empirical, 'Whether textual asymmetry arose from translation challenges or deliberate strategic drafting.').

omega_variable(
    contra_proferentem_applicability,
    'Should contra proferentem (ambiguities resolved against drafter) apply to the Treaty, with Māori text controlling and English text as the drafter''s version?',
    'Legal analysis of international treaty law, assessment of who drafted which version and to whom it was presented, analysis of signatories'' capacity to assent to English version.',
    'If applicable, Crown''s reading is legally wrong and Māori text controls. If not, Crown''s interpretive authority persists despite linguistic asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contra_proferentem_applicability, conceptual, 'Whether contract interpretation principles constrain the Crown''s treaty-reading authority.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.79) primarily structural (legal exclusion, institutional barriers) or partially internalized (absorbed legitimacy of Crown authority)?',
    'Post-removal trajectory: if Māori voice emerges when barriers are removed, suppression was structural. If suppression persists after removal, it is internalized—carried by targets as cognitive/identity patterns.',
    'Structural suppression requires legal/institutional remedies. Internalized suppression requires identity decolonization—slower, deeper, operates through cultural patterns after external mechanisms are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Suppression mechanism composition: external barriers vs. internal cognitive patterns.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 1840, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__crown_cession_reading, theater_ratio, 1840, 0.15).
narrative_ontology:measurement(trea_tr_t1875, treaty_authority_cession__crown_cession_reading, theater_ratio, 1875, 0.22).
narrative_ontology:measurement(trea_tr_t1920, treaty_authority_cession__crown_cession_reading, theater_ratio, 1920, 0.28).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__crown_cession_reading, theater_ratio, 1975, 0.38).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__crown_cession_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(trea_tr_t2026, treaty_authority_cession__crown_cession_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1840, 0.68).
narrative_ontology:measurement(trea_be_t1875, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1875, 0.76).
narrative_ontology:measurement(trea_be_t1920, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1920, 0.82).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1975, 0.85).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2000, 0.84).
narrative_ontology:measurement(trea_be_t2026, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2026, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1840, 0.55).
narrative_ontology:measurement(trea_su_t1875, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1875, 0.68).
narrative_ontology:measurement(trea_su_t1920, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1920, 0.74).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1975, 0.78).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2000, 0.77).
narrative_ontology:measurement(trea_su_t2026, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2026, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__crown_cession_reading, 0.12).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% The Treaty of Waitangi kernel family decomposes into three structurally distinct constraint stories: the Crown cession reading (this story), the rangatiratanga retention reading, and the retrospective snare exposure reading. Each reading has a different ε, different beneficiary/victim structure, and different computed type. They are not three measurements of one constraint under different observables—they are three constraints with different referents (what counts as the 'standing arrangement under contest' in each reading). The Crown reading's ε (0.87) measures extraction under the Crown's interpretation; the rangatiratanga reading's ε measures extraction under the Māori interpretation; the snare reading's ε measures extraction under the reading that sees textual divergence as the mechanism itself. All three are readings of the same kernel (the Treaty text), but each instantiates a different constraint with different structural properties. They are linked via network.affects_constraints because changes to one reading's legitimacy affect the others—if historical evidence conclusively shows what the chiefs understood, it undermines the Crown reading and strengthens the rangatiratanga reading; if contra proferentem doctrine is applied, it changes the legal framework all three readings operate within.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__crown_cession_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
