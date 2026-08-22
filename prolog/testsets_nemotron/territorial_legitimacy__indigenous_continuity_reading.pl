% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Legitimacy via Continuous Indigenous Habitation and Anti-Colonial Self-Determination (1948 as Nakba)
 *   domain: political/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the indigenous_continuity_reading of
 *   the territorial_legitimacy kernel. It reads 1948 as Nakba (catastrophe),
 *   not partition — the founding displacement of the Palestinian people and
 *   the imposition of a settler-colonial state. The reading asserts that
 *   legitimacy derives exclusively from continuous indigenous habitation and
 *   anti-colonial self-determination. Consequently: Palestinian sovereignty
 *   over all historic Palestine is the only legitimate arrangement; the
 *   Israeli state is structurally illegitimate as a settler-colonial entity;
 *   the right of return for 1948 refugees and their descendants is
 *   structurally central and non-negotiable. The reading operates as a snare:
 *   it presents itself as a coordination framework (anti-colonial justice,
 *   legal principle) but its persistence depends on suppressing the partition
 *   framework and its material costs fall overwhelmingly on the Palestinian
 *   population under occupation and the mobilized Israeli public, while the
 *   reading's maximalism forecloses any interim relief or negotiated
 *   improvement.
 *
 * KEY AGENTS:
 *   - palestinian_refugees_1948: Primary beneficiary and payer (identity-locked, generational) — bears the right of return claim and the cost of its non-realization
 *   - palestinian_civil_society: Beneficiary (organized, identity-locked) — sustains the narrative infrastructure
 *   - palestinian_population_occupied_territories: Payer (powerless, trapped) — bears the material cost of the reading's maximalism
 *   - israeli_state_institutions: Agenda setter (institutional, arbitrage) — the actual governing authority the reading delegitimizes
 *   - israeli_citizens_subject_to_mobilization: Payer (moderate, constrained) — bears costs of permanent confrontation
 *   - international_legal_order: Observer (institutional, analytical) — the arena where claims are adjudicated but whose architecture the reading rejects
 *   - western_governments: Excluded (powerful, mobile) — complicit in the colonial structure per the reading
 *   - anti_colonial_movements_global: Beneficiary (organized, identity-locked) — depends on the reading as paradigmatic case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.85).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.92).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Legitimacy via Continuous Indigenous Habitation and Anti-Colonial Self-Determination (1948 as Nakba)").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, 'e9bd5839-6f44-4c58-b080-41f62084a8c4').
narrative_ontology:cs_kernel_codification('e9bd5839-6f44-4c58-b080-41f62084a8c4', distributed).
narrative_ontology:cs_authority_grounding('e9bd5839-6f44-4c58-b080-41f62084a8c4', extraction).
narrative_ontology:cs_interpretation_layer_present('e9bd5839-6f44-4c58-b080-41f62084a8c4').
narrative_ontology:cs_reading_relation('e9bd5839-6f44-4c58-b080-41f62084a8c4', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('e9bd5839-6f44-4c58-b080-41f62084a8c4', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('e9bd5839-6f44-4c58-b080-41f62084a8c4', foundational, nakba_as_originary_injustice).
narrative_ontology:cs_axiom_status(nakba_as_originary_injustice, holdable).
narrative_ontology:cs_axiom_grounding('e9bd5839-6f44-4c58-b080-41f62084a8c4', nakba_as_originary_injustice, deontological).
narrative_ontology:cs_axiom('e9bd5839-6f44-4c58-b080-41f62084a8c4', foundational, right_of_return_non_negotiable).
narrative_ontology:cs_axiom_status(right_of_return_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('e9bd5839-6f44-4c58-b080-41f62084a8c4', right_of_return_non_negotiable, deontological).
narrative_ontology:cs_axiom('e9bd5839-6f44-4c58-b080-41f62084a8c4', foundational, settler_colonial_structure_illegitimate).
narrative_ontology:cs_axiom_status(settler_colonial_structure_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('e9bd5839-6f44-4c58-b080-41f62084a8c4', settler_colonial_structure_illegitimate, deontological).
narrative_ontology:cs_reference_frame('e9bd5839-6f44-4c58-b080-41f62084a8c4', pre_1948_indigenous_sovereignty).
narrative_ontology:cs_drift_state('e9bd5839-6f44-4c58-b080-41f62084a8c4', post_oslo_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e9bd5839-6f44-4c58-b080-41f62084a8c4', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugees_1948).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_civil_society).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, anti_colonial_movements_global).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_population_occupied_territories).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, israeli_citizens_subject_to_mobilization).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, right_of_return_1948_refugees).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, palestinian_sovereignty_historic_palestine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, settler_colonial_illegitimacy_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, anti_colonial_self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Descendants of 1948 displaced populations. The reading names them as the primary bearers of the right of return and sovereignty claim. They pay the ongoing cost of statelessness, camp life, and denied return. Their identity is fused with the Nakba narrative — exit from the claim would constitute identity dissolution. They are structurally excluded from the partition framework's recognition.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugees_1948, beneficiary,
    powerless, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugees_1948, payer).

% NGOs, unions, academic institutions, and grassroots organizations that sustain the continuity narrative. They benefit from the reading's moral and legal framework for advocacy. Their organizational existence is constituted through the anti-colonial frame; adopting a partition reading would dissolve their mandate. They bear costs of repression, funding restrictions, and criminalization.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_civil_society, beneficiary,
    organized, generational, identity_locked, regional).

% Population under military occupation in West Bank and Gaza. They bear the extraction of the reading's operationalization: the reading's insistence on total sovereignty and return provides the ideological cover for rejectionist politics that foregoes interim relief, state-building, or negotiated improvements. Their daily life is structured by checkpoints, permits, land confiscation, and violence — costs the reading does not alleviate and may intensify by delegitimizing any engagement with existing structures.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_population_occupied_territories, payer,
    powerless, biographical, trapped, local).

% Jewish Israeli citizens who bear the costs of permanent mobilization, military service, economic burden of occupation, and international delegitimization that the reading fuels. They are not beneficiaries of the reading — the reading declares their polity illegitimate. Exit from the constraint means emigration (constrained) or internal dissent (socially sanctioned). The reading's maximalist frame gives no structural recognition to their collective existence or security claims.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_citizens_subject_to_mobilization, payer,
    moderate, biographical, constrained, national).

% The actual governing authority over the territory. The reading treats them as the primary extractive agent — a settler-colonial regime whose legitimacy is structurally zero. They set the agenda of control, enforcement, and settlement. They have arbitrage-grade exit from the reading's moral claims (they reject the frame entirely) but no exit from the material confrontation the reading helps sustain. They benefit from the reading's maximalism by using it to justify rejection of any compromise.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% UN bodies, ICJ, ICC, treaty regimes. The reading appeals to them (self-determination, anti-colonial law) while simultaneously rejecting their partition-based instruments (UNGA 181, UNSC 242). They are the arena where the reading's claims are adjudicated but their existing architecture is structurally incompatible with the reading's maximalist demand.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_legal_order, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(territorial_legitimacy__indigenous_continuity_reading, international_legal_order).

% States that provide diplomatic, military, and economic support to Israel. The reading identifies them as complicit in the colonial structure. They would object to the reading's delegitimization of Israel and its implication for their foreign policy. They are excluded from the reading's moral community — their recognition of Israel is treated as evidence of colonial complicity, not a legitimate difference of interpretation.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, western_governments, excluded,
    powerful, biographical, mobile, global).

% Transnational solidarity networks, Global South states, decolonial movements. They benefit from the reading as a paradigmatic case for anti-colonial struggle. Their political identity is partially constituted through solidarity with Palestine. The reading's maximalism (no partition, full return) serves as an ideological anchor; compromise readings weaken the global anti-colonial frame they depend on.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, anti_colonial_movements_global, beneficiary,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a transnational moral and legal claim that centers the 1948 displacement as the origin of all subsequent illegitimacy, providing a unified framework for Palestinian national struggle, global solidarity, and legal advocacy that rejects partition as colonial imposition.
% TRANSFER_FUNCTION: Transfers the burden of proof and the cost of compromise onto the partition framework and its beneficiaries: the reading demands that any settlement must begin from full return and full sovereignty, making the status quo bear the full weight of justification. Materially, it transfers the cost of continued conflict onto the occupied population and the mobilized Israeli public by foreclosing interim arrangements.
% ABSENT_VOICES: Palestinian voices that would accept a two-state settlement based on 1967 lines (e.g., segments of the PLO post-Oslo, Palestinian business elites, some refugee communities prioritizing compensation over return) are structurally excluded — the reading treats such positions as collaboration. Israeli peace camp voices that ground their opposition to occupation in Israeli self-interest rather than anti-colonial principle are also excluded — the reading grants them no structural legitimacy.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the Palestinian national movement would lose its foundational moral-legal architecture. The right of return would cease to be a non-negotiable principle. The anti-colonial frame linking Palestine to global decolonization would collapse. Negotiations would immediately shift to partition-based frameworks (1967 lines, land swaps, symbolic return). The global solidarity movement would lose its paradigmatic case. The material confrontation would continue but its ideological structure would fundamentally reorganize.
% FOUNDING_PROBLEM: The 1948 Nakba — the displacement of 700,000+ Palestinians and the establishment of a Jewish state on 78% of historic Palestine through military force and colonial settlement — created a structural injustice that no subsequent agreement, recognition, or fait accompli can legitimize. The founding problem is the original colonial displacement and the ongoing settler-colonial structure that maintains it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: UNRWA's registration of 5.9 million refugees (2023), ICJ 2024 advisory opinion on Israel's occupation citing the continuity of the 1948 displacement, Human Rights Watch and Amnesty International apartheid reports (2021-2022) documenting the structural continuity from 1948, Palestinian oral history archives (e.g., Nakba Archive, Palestinian Museum), and the unanimous position of the Global South in UNGA voting records on Palestine. No significant non-Palestinian, non-anti-colonial source corroborates the claim that the founding problem is resolved.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because the reading's maximalist demand (full return, full sovereignty, zero legitimacy for Israel) extracts the entire space of the possible — it forecloses any compromise that could alleviate material suffering. The occupied Palestinian population pays this extraction daily through the continuation of occupation that the reading's rejectionism helps sustain. Suppression is extreme (0.92) because the reading's persistence requires active exclusion of the partition framework: Palestinian voices accepting two states are delegitimized as collaborators; the international legal order's partition-based instruments are treated as colonial impositions; the Oslo framework is read as a trap. Theater ratio is relatively low (0.25) because the reading's core claim — that 1948 was a colonial displacement, not a legitimate partition — is empirically and morally substantial. The performative element lies in the reading's operationalization: the gap between the maximalist demand and any achievable political reality generates ritualized repetition (annual Nakba commemorations, UN resolutions, solidarity statements) that sustains the frame without advancing its realization. Accessibility collapse is high (0.75) — once the anti-colonial frame is adopted, alternatives (partition, confederation, binationalism) appear as betrayals. Resistance is high (0.88) — the reading meets fierce resistance from the Israeli state, its allies, and the partition-based international order, and also from Palestinian voices it excludes.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute sharply divergent per-seat classifications. From the palestinian_refugees_1948 and palestinian_civil_society seats (identity-locked beneficiaries), the constraint computes as a rope or tangled_rope — a genuine coordination framework for justice. From the palestinian_population_occupied_territories seat (trapped payer), it computes as a snare — extraction without relief. From the israeli_citizens_subject_to_mobilization seat (constrained payer), it computes as a snare — extraction with no recognition. From the israeli_state_institutions seat (agenda_setter with arbitrage exit), it computes as a mountain or piton — an external ideological claim with no structural purchase on their power. The international_legal_order observer seat sees a contested claim that the existing system cannot adjudicate. This seat divergence IS the structural reality of the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (palestinian_refugees_1948, palestinian_civil_society, anti_colonial_movements_global) are identity-locked — their political self-concept is constituted through the claim. The reading subsidizes their moral standing and organizational coherence (d → 0.0). Victims include palestinian_population_occupied_territories (trapped, powerless — d → 1.0) who bear the material costs of the reading's maximalism, and israeli_citizens_subject_to_mobilization (constrained, moderate — d → 0.7-0.8) who bear confrontation costs without recognition. The israeli_state_institutions (agenda_setter, arbitrage exit) are structurally outside the reading's moral economy — they reject the frame entirely (d derivation reverts to canonical fallback). The international_legal_order (observer, analytical) sits at d ≈ 0.5 — the reading appeals to it while rejecting its instruments.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem (the 1948 Nakba and ongoing settler-colonial structure) is live — corroborated by ICJ, UNRWA, major human rights organizations, and Global South consensus. The mandatrophy question is whether the reading's *maximalist operationalization* (full return, full sovereignty, zero partition) has outlived its function. The reading coordinates a genuine anti-colonial claim (coordination function) but extracts the possibility of any interim improvement for the occupied population (transfer function). This is the snare signature: coordination as cover for extraction. The mandate has not atrophied — the injustice persists — but the reading's refusal of all partial solutions functions as extraction from the very population it claims to liberate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maximalism_as_extraction_mechanism,
    'Does the reading''s maximalist demand (full return, full sovereignty, zero partition) function as a genuine coordination mechanism for justice, or as an extraction mechanism that forecloses material relief for the occupied population?',
    'Counterfactual analysis: if the reading adopted a phased approach (right of return recognized in principle, implemented gradually; sovereignty over 1967 lines now; final status negotiated), would the occupied population''s material conditions improve? Would the anti-colonial frame survive? Track Palestinian public opinion on compromise over time — does the maximalist frame correlate with worse outcomes for the occupied?',
    'If maximalism is extraction, the reading is a snare with the occupied population as primary victim. If maximalism is necessary coordination, the reading is a tangled_rope — genuine coordination with asymmetric costs. This distinction determines whether the reading''s persistence serves justice or entraps its beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maximalism_as_extraction_mechanism, conceptual, 'Whether the reading''s refusal of compromise is structurally necessary or extractive.').

omega_variable(
    identity_lock_mechanism_palestinian,
    'What specific identity-fusion mechanism binds Palestinian refugees and civil society to the maximalist frame? Is it professional identity (advocacy careers), relational identity (community constituted through the claim), ideological identity (anti-colonial worldview), or institutional identity (organizations that would dissolve without the frame)?',
    'Sociological study of Palestinian advocacy organizations, refugee camp leadership, and civil society: track what happens to actors who publicly endorse compromise — do they lose funding, social standing, organizational mandate? Compare with other anti-colonial movements that accepted phased settlements.',
    'If identity_locked is driven by institutional survival (organizations would dissolve), the lock is structural and the reading''s persistence may serve organizational interests over population interests. If driven by ideological/relational fusion, the lock is deeper and the reading''s maximalism reflects genuine constitutive commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_palestinian, empirical, 'Mechanism of identity lock for Palestinian beneficiaries of the reading.').

omega_variable(
    suppression_mechanism_partition_voices,
    'Is the suppression of partition-accepting Palestinian voices structural (funding cuts, PA security coordination, Israeli permit regime) or internalized (moral conviction that compromise is betrayal, fear of social sanction)?',
    'Post-Oslo trajectory analysis: did suppression of dissenting Palestinian voices increase after the PA''s creation (structural) or does it persist in diaspora communities beyond PA reach (internalized)? Interview Palestinian activists who shifted positions — what drove the shift and what were the consequences?',
    'If structural, the reading''s suppression is enforced by power (PA, donors, Israel) and could change with political shifts. If internalized, the reading carries its suppression mechanism within the population — exit from the frame carries identity dissolution regardless of external conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_partition_voices, empirical, 'Structural vs. internalized suppression of intra-Palestinian dissent.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the territorial_legitimacy kernel admit only these three readings, or is there a fourth framing — e.g., legitimacy via transformative justice (acknowledgment, reparations, shared sovereignty) — that neither forecloses nor coexists with the existing three but reconfigures the kernel''s terms?',
    'Map the full space of legitimacy claims in Palestinian and Israeli discourse: confederation models (Two States One Homeland), binationalism, transformative justice frameworks. Test whether they instantiate new readings or are hybrids of existing ones. If a coherent fourth reading exists, the kernel''s decomposition is incomplete.',
    'If a transformative justice reading exists as a distinct framing, the current three-reading decomposition misses a structural alternative that could resolve the foreclosure/coexistence tension. This would be a CS-framing under-determination omega — the declared kernel structure would be incomplete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s reading decomposition is complete or misses a transformative justice framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_tr_t1948, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_tr_t1967, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_tr_t1987, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1987, 0.2).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_tr_t1993, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1993, 0.22).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_tr_t2000, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2000, 0.23).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_tr_t2005, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_tr_t2014, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2014, 0.24).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_tr_t2024, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_be_t1948, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1948, 0.65).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_be_t1967, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1967, 0.72).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_be_t1987, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1987, 0.75).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_be_t1993, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1993, 0.78).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_be_t2000, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2000, 0.81).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_be_t2005, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2005, 0.82).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_be_t2014, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2014, 0.84).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_be_t2024, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_su_t1948, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1948, 0.85).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_su_t1967, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1967, 0.88).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_su_t1987, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1987, 0.9).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_su_t1993, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1993, 0.91).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_su_t2000, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2000, 0.92).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_su_t2005, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2005, 0.92).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_su_t2014, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2014, 0.92).
narrative_ontology:measurement(territorial_legitimacy_indigenous_continuity_su_t2024, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__indigenous_continuity_reading, 0.08).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, palestinian_right_of_return_implementation).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, israeli_settlement_enterprise).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, gaza_blockade_regime).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, west_bank_area_c_control).

% DUAL FORMULATION NOTE:
% The territorial_legitimacy kernel decomposes into three readings with divergent ε: indigenous_continuity_reading (ε=0.85, snare) — maximalist anti-colonial frame; partition_reading (ε=0.45, tangled_rope) — coordination via international law with asymmetric enforcement; security_necessity_reading (ε=0.65, snare) — coordination via security with extraction from Palestinian population. The ε-invariance principle requires separate stories because the referent (the standing arrangement of territorial control) is assessed differently: indigenous continuity reads the arrangement as colonial extraction; partition reads it as incomplete legal implementation; security reads it as necessary defense. Each reading instantiates a different constraint with different beneficiaries, victims, and type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__indigenous_continuity_reading, institutional, 0.95).
constraint_indexing:directionality_override(territorial_legitimacy__indigenous_continuity_reading, powerless, 0.98).
constraint_indexing:directionality_override(territorial_legitimacy__indigenous_continuity_reading, organized, 0.15).
constraint_indexing:directionality_override(territorial_legitimacy__indigenous_continuity_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
