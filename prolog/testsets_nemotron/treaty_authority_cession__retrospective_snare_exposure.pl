% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__retrospective_snare_exposure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Treaty Authority Cession — Retrospective Snare Exposure
 *   domain: constitutional/indigenous/colonial
 *
 * SUMMARY:
 *   This constraint story models the Treaty of Waitangi / Te Tiriti o
 *   Waitangi as a retrospective snare: the extraction mechanism was the
 *   textual divergence between the English text (which claims full
 *   sovereignty cession) and the Māori text (which grants kāwanatanga —
 *   governance — while retaining tino rangatiratanga — full authority).
 *   Chiefs signing the Māori text could not assent to the English claim
 *   because it was not presented to them, not explained, and contradicted the
 *   Māori text they did sign. The Crown's land-purchasing apparatus and
 *   colonial parliament used the English text as legal foundation for
 *   pre-emption, legislative override, and Native Land Court
 *   individualization — transferring ~90% of Māori land to Crown and settlers
 *   by 1900. The extraction was covert at time of operation (chiefs believed
 *   they retained authority); it becomes visible only retrospectively through
 *   Tribunal findings and constitutional analysis. This is the
 *   retrospective_snare_exposure reading of the treaty_authority_cession
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.93).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.88).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.93).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty Authority Cession — Retrospective Snare Exposure").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional/indigenous/colonial").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, '4eb21218-1e06-4065-aded-df9d8afd70e6').
narrative_ontology:cs_kernel_codification('4eb21218-1e06-4065-aded-df9d8afd70e6', fixed_text).
narrative_ontology:cs_authority_grounding('4eb21218-1e06-4065-aded-df9d8afd70e6', extraction).
narrative_ontology:cs_reading_relation('4eb21218-1e06-4065-aded-df9d8afd70e6', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('4eb21218-1e06-4065-aded-df9d8afd70e6', treaty_authority_cession__rangatiratanga_retention_reading, coexists_with).
narrative_ontology:cs_axiom('4eb21218-1e06-4065-aded-df9d8afd70e6', foundational, textual_divergence_is_extraction_mechanism).
narrative_ontology:cs_axiom_status(textual_divergence_is_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('4eb21218-1e06-4065-aded-df9d8afd70e6', textual_divergence_is_extraction_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('4eb21218-1e06-4065-aded-df9d8afd70e6', foundational, maori_text_assent_excludes_english_cession_claim).
narrative_ontology:cs_axiom_status(maori_text_assent_excludes_english_cession_claim, holdable).
narrative_ontology:cs_axiom_grounding('4eb21218-1e06-4065-aded-df9d8afd70e6', maori_text_assent_excludes_english_cession_claim, deontological).
narrative_ontology:cs_reference_frame('4eb21218-1e06-4065-aded-df9d8afd70e6', treaty_as_sovereignty_transfer).
narrative_ontology:cs_drift_state('4eb21218-1e06-4065-aded-df9d8afd70e6', contemporary_tribunal_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('4eb21218-1e06-4065-aded-df9d8afd70e6', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, colonial_parliament).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, settler_land_speculators).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatories).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendants).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, hapu_collective_authority).
narrative_ontology:constraint_vindicates(treaty_authority_cession__retrospective_snare_exposure, terra_nullius_rejection).
narrative_ontology:constraint_vindicates(treaty_authority_cession__retrospective_snare_exposure, translation_as_extraction_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the native land purchase system that converted Māori customary title into Crown grants. Relied on the English text's sovereignty claim to assert pre-emption rights and legislative override. Collected the difference between purchase prices paid to Māori and resale value to settlers — the primary extraction flow.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, beneficiary).

% Enacted legislation (Native Lands Acts, Suppression of Rebellion Act, etc.) that progressively stripped collective title and imposed individualization, using the treaty's English text as legal foundation. The legislative program was the enforcement machinery converting textual ambiguity into land transfer.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, colonial_parliament, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, colonial_parliament, beneficiary).

% Purchased Māori land at Crown-mediated prices, often through debt-pressure or factional manipulation. Their gains were the realized uplift from customary land to freehold speculation. Exit was open — they could sell and leave — but the extraction flow ran through them to the Crown apparatus.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, settler_land_speculators, beneficiary,
    organized, biographical, mobile, national).

% Rangatira who signed the Māori text (Te Tiriti) understanding kāwanatanga as governance delegation and tino rangatiratanga as retained authority. Could not read English; the English text's sovereignty claim was not explained. Their assent was to the Māori text only. The extraction mechanism operated through the gap between what they assented to and what the Crown claimed they ceded.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_signatories, payer,
    organized, biographical, identity_locked, local).

% Inherit the land loss and authority erosion initiated under the translation gap. Excluded from the constitutional conversation that treated the English text as controlling. Identity-locked because the relationship to whenua and whakapapa makes exit from the colonial legal order structurally unthinkable — not merely difficult.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendants, payer,
    powerless, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, maori_descendants, excluded).

% The collective decision-making structures that the treaty's Māori text guaranteed (tino rangatiratanga). Systematically dismantled by Native Land Court individualization and Crown pre-emption. Trapped because the legal form (individual title) destroys the collective subject that could exercise exit.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, hapu_collective_authority, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, hapu_collective_authority, excluded).

% Commission of inquiry established 1975 to hear claims. Retrospectively exposes the extraction mechanism by documenting the textual divergence and its material consequences. Has no enforcement power — produces reports that the Crown may accept or ignore. Its analytical seat sees the full structure the operational seats could not.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Analyze the treaty as a case study in translation-as-extraction, contra proferentem application, and retrospective snare exposure. Provide the conceptual vocabulary (this reading) that makes the extraction mechanism visible across time.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — the treaty's Māori text coordinated a relationship of delegated governance (kāwanatanga) alongside retained authority (tino rangatiratanga). The English text's sovereignty claim was not a coordination function; it was the extraction cover story.
% TRANSFER_FUNCTION: Moves land and decision-making authority from Māori collective structures (hapū, iwi) to Crown land-purchasing apparatus and colonial parliament, via the mechanism of textual divergence: the English text claims a cession the Māori text does not grant, and the Crown enforces the English claim.
% ABSENT_VOICES: The rangatira who signed Te Tiriti — they would object to the English text's sovereignty claim if they had been told it meant cession. Their descendants, excluded from the constitutional order until the Tribunal era. The missionaries who translated (Henry Williams et al.) — their role in the divergence is contested; some evidence suggests deliberate ambiguity.
% DISAPPEARANCE_RATIONALE: If the English-text sovereignty claim disappeared overnight, the Crown's pre-emption right, legislative override authority, and Native Land Court jurisdiction would lose their foundational legal basis. Māori collective title and tino rangatiratanga would be the presumptive constitutional starting point. Land transfers executed under the English claim would be legally vulnerable. The entire colonial property order would reorganize.
% FOUNDING_PROBLEM: The British Crown sought to acquire sovereignty over New Zealand and establish a colony without triggering the humanitarian scrutiny that followed the Australian frontier wars. The treaty was the instrument: a document Māori would sign that the Crown could read as cession.
% FOUNDING_PROBLEM_CORROBORATION: British Colonial Office correspondence (e.g., Normanby to Hobson, 1839) explicitly frames the treaty as a sovereignty acquisition tool. Waitangi Tribunal reports (e.g., Te Paparahi o Te Raki, 2014) document the Crown's intention to obtain cession while presenting a different text to Māori. No corroboration from outside the Crown's benefiting apparatus supports the 'founding problem' as legitimate.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__retrospective_snare_exposure, 0.93, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness rises from 0.15 (1840, when the treaty was fresh and land transfers minimal) to 0.93 (2024, retrospective assessment of cumulative land loss and authority erosion). The low initial value reflects the snare's covert nature — the extraction mechanism was not yet fully deployed. Suppression requirement peaks 1880-1900 (Native Land Act enforcement, military suppression of resistance) then dips slightly 1975 (Tribunal establishment) before rising again (ongoing legislative resistance to Tribunal recommendations, foreshore and seabed legislation). Theater ratio stays low throughout — the Crown's legal machinery was functionally extractive, not performative; the 'partnership' rhetoric is late (post-1975) and thin. Accessibility collapse is moderate (0.4): alternatives existed (Māori retained de facto authority in many regions until 1880s; the King movement offered a political alternative) but collapsed under sustained enforcement. Resistance is high (0.75): armed resistance (1860s), legal resistance (Native Land Court petitions), political resistance (Kīngitanga, Kotahitanga), and contemporary Treaty claims.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's operational seat (1840-1900), the treaty appeared as a completed cession — a rope-like coordination of sovereignty transfer. From the Māori signatory seat, it was a delegation of governance while retaining authority — a different coordination. From the retrospective analytical seat (this reading), the divergence between these two perceptions IS the extraction mechanism. The engine computes this divergence from the structural data: the same constraint produces near-beneficiary χ for the Crown and near-target χ for Māori across the interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Crown apparatus and colonial parliament are structural beneficiaries (d ≈ 0.1) — they set the agenda, collect the land value uplift, and control the legal interpretation. Settler speculators are mobile beneficiaries (d ≈ 0.2) — they capture gains but can exit. Māori signatories are identity-locked targets (d ≈ 0.95) — their assent was structurally to a different text; the extraction operates through the identity-binding relationship to whenua. Descendants and hapū authority are similarly identity-locked or trapped. The Waitangi Tribunal and scholars are analytical observers (d = 0.5). The directionality is driven by the textual divergence: the English text's claim is the extraction mechanism, and the Crown enforces it.
 *
 * MANDATROPHY ANALYSIS:
 *   The treaty's founding problem (Crown sovereignty acquisition without humanitarian scandal) is dead — the colony is established, the humanitarian context gone. But the arrangement persists and extracts. The mandatrophy is resolved: the constraint is a snare, not a scaffold or piton. The 'partnership' rhetoric (post-1975) is the theatrical maintenance of a degraded extraction mechanism — theater_ratio rising slightly but staying low confirms this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    translation_intentionality,
    'Was the textual divergence deliberately engineered by the translators (Henry Williams, et al.) as an extraction mechanism, or was it a genuine translation error amplified by Crown opportunism?',
    'Missionary correspondence, Colonial Office instructions to Hobson, and comparative analysis of Williams'' other translations. The Tribunal''s Te Paparahi o Te Raki report (2014) treats it as deliberate; some historians argue for inadvertent ambiguity.',
    'If deliberate, the snare was designed — the extraction mechanism is intentional fraud. If inadvertent, the Crown''s subsequent enforcement of the English reading is the snare, not the translation itself. Classification remains snare either way; the omega distinguishes design from exploitation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(translation_intentionality, empirical, 'Intentionality of the translation divergence as extraction mechanism design.').

omega_variable(
    contra_proferentem_scope,
    'Does contra proferentem apply to the treaty as a whole, or only to specific clauses? If the former, the Māori text controls entirely; if the latter, the divergence may be partially resolved in the Crown''s favor.',
    'Supreme Court jurisprudence on treaty interpretation; international law on indigenous treaties; the Tribunal''s evolving methodology.',
    'If contra proferentem applies globally, the rangatiratanga_retention_reading gains structural force and the extraction mechanism''s legal basis collapses. If limited, the Crown retains partial legal cover. This reading (retrospective_snare_exposure) treats the divergence as the mechanism regardless of interpretive rules — the extraction operated under whatever rules the Crown chose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contra_proferentem_scope, conceptual, 'Scope of contra proferentem in treaty interpretation and its effect on the extraction mechanism''s legal visibility.').

omega_variable(
    retrospective_visibility_threshold,
    'At what point did the extraction mechanism become visible to the operational parties (not just analysts)? When did Māori leadership collectively recognize the divergence as extractive rather than a misunderstanding?',
    'Māori petitioning record (1860s onward), Kīngitanga and Kotahitanga political platforms, Tribunal claimant testimony. The shift from ''the Crown is not honoring the treaty'' to ''the treaty itself was a trap'' marks the visibility threshold.',
    'If visibility occurred early (1860s), the snare''s covert phase was short and resistance was informed. If late (post-1975), the snare operated covertly for 130+ years. Affects the extraction trajectory''s moral and structural characterization but not the endpoint classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retrospective_visibility_threshold, empirical, 'When the extraction mechanism became visible to its targets.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel ''treaty authority cession'' the correct framing, or should the kernel be ''Crown sovereignty acquisition mechanism'' — where the treaty is one instrument among many (proclamations, legislation, war)?',
    'Constitutional history of 1840: Hobson''s proclamations of sovereignty (May 1840) preceded most treaty signings. The treaty was not the sole sovereignty instrument. If the kernel is broader, this reading is one component of a larger extraction apparatus.',
    'If the kernel is broader, this constraint story captures only the treaty-component of the extraction mechanism. The network.affects_constraints should then link to the broader sovereignty_acquisition_mechanism kernel. This reading''s ε would be a component of a larger ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel framing captures the full extraction apparatus or only its treaty component.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(trea_tr_t1860, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1860, 0.1).
narrative_ontology:measurement(trea_tr_t1880, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1880, 0.12).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1900, 0.13).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1975, 0.14).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.15).
narrative_ontology:measurement(trea_be_t1860, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1860, 0.45).
narrative_ontology:measurement(trea_be_t1880, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1880, 0.72).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1900, 0.85).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1975, 0.88).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2024, 0.93).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement(trea_su_t1860, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1860, 0.65).
narrative_ontology:measurement(trea_su_t1880, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1880, 0.82).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1900, 0.88).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1975, 0.75).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__retrospective_snare_exposure, 0.1).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, native_land_court_individualization).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, crown_preemption_right).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, foreshore_seabed_legislation).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, treaty_settlement_process).

% DUAL FORMULATION NOTE:
% The treaty_authority_cession kernel decomposes into three constraint stories: crown_cession_reading (claimed mountain, computes as snare from Māori seat), rangatiratanga_retention_reading (claimed rope, computes as tangled_rope from Crown seat), and retrospective_snare_exposure (this story — claimed snare, computes as snare from all target seats). The divergence is not a measurement ambiguity — it is the extraction mechanism itself. Each reading instantiates a different constraint with different ε, different beneficiaries/victims, and different structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__retrospective_snare_exposure, organized, 0.95).
constraint_indexing:directionality_override(treaty_authority_cession__retrospective_snare_exposure, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
