% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Territorial Legitimacy: Indigenous Continuity / Anti-Colonial Self-Determination Reading
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested
 *   territorial_legitimacy kernel: legitimacy grounded in continuous
 *   indigenous habitation and anti-colonial self-determination, treating 1948
 *   as an ongoing Nakba (catastrophe/dispossession) rather than a completed
 *   and legally settled partition. This reading structurally centers
 *   Palestinian sovereignty over all of historic Palestine, treats the
 *   Israeli state as a settler-colonial entity of contested legitimacy, and
 *   makes the right of return for 1948 refugees a non-negotiable structural
 *   element rather than a negotiable final-status item. This is a distinct
 *   constraint from the partition_reading (which grounds legitimacy in UN
 *   Resolution 181 and international legal recognition) and the
 *   security_necessity_reading (which grounds legitimacy in post-1967
 *   defensive control and strategic depth) — each reading has a different
 *   epsilon, different beneficiary/victim structure, and different type, and
 *   they are linked here only via network edges, not merged.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.68).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.72).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy: Indigenous Continuity / Anti-Colonial Self-Determination Reading").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, '2d674f30-4d25-4c17-b263-274e275ba962').
narrative_ontology:cs_kernel_codification('2d674f30-4d25-4c17-b263-274e275ba962', distributed).
narrative_ontology:cs_authority_grounding('2d674f30-4d25-4c17-b263-274e275ba962', distributed).
narrative_ontology:cs_reading_relation('2d674f30-4d25-4c17-b263-274e275ba962', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('2d674f30-4d25-4c17-b263-274e275ba962', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('2d674f30-4d25-4c17-b263-274e275ba962', foundational, continuous_habitation_grounds_sovereignty_over_legal_partition).
narrative_ontology:cs_axiom_status(continuous_habitation_grounds_sovereignty_over_legal_partition, holdable).
narrative_ontology:cs_axiom_grounding('2d674f30-4d25-4c17-b263-274e275ba962', continuous_habitation_grounds_sovereignty_over_legal_partition, deontological).
narrative_ontology:cs_axiom('2d674f30-4d25-4c17-b263-274e275ba962', foundational, settler_colonial_entities_lack_founding_legitimacy_regardless_of_recognition).
narrative_ontology:cs_axiom_status(settler_colonial_entities_lack_founding_legitimacy_regardless_of_recognition, holdable).
narrative_ontology:cs_axiom_grounding('2d674f30-4d25-4c17-b263-274e275ba962', settler_colonial_entities_lack_founding_legitimacy_regardless_of_recognition, conventional).
narrative_ontology:cs_axiom('2d674f30-4d25-4c17-b263-274e275ba962', secondary, right_of_return_is_non_negotiable_structural_precondition).
narrative_ontology:cs_axiom_status(right_of_return_is_non_negotiable_structural_precondition, holdable).
narrative_ontology:cs_axiom_grounding('2d674f30-4d25-4c17-b263-274e275ba962', right_of_return_is_non_negotiable_structural_precondition, deontological).
narrative_ontology:cs_reference_frame('2d674f30-4d25-4c17-b263-274e275ba962', pre_1948_demographic_continuity).
narrative_ontology:cs_drift_state('2d674f30-4d25-4c17-b263-274e275ba962', post_oslo_two_state_stagnation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2d674f30-4d25-4c17-b263-274e275ba962', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_national_movement).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, diaspora_refugee_advocacy_networks).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, anti_colonial_solidarity_states).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, displaced_1948_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, west_bank_gaza_residents_under_closure).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, israeli_jewish_population_of_disputed_legitimacy_status).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, displaced_1948_refugees).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, settler_colonial_analytic_framework).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, indigenous_self_determination_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, right_of_return_as_customary_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Descendants of those displaced in 1948, mostly living in refugee camps or diaspora across neighboring states and beyond. The reading structurally centers their claim (right of return as the organizing demand) but their actual condition — statelessness, camp confinement, generational limbo — has not been resolved by the reading's political durability. They benefit symbolically and organizationally from a framework that keeps their claim alive, but bear the ongoing cost of a resolution that has not arrived across 75+ years.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, displaced_1948_refugees, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, displaced_1948_refugees, beneficiary).

% Live under military occupation, blockade, or settlement expansion. The indigenous-continuity reading provides the moral and legal vocabulary for resistance and solidarity movements, but does not itself control checkpoints, permits, or borders. They pay the immediate costs of the underlying conflict regardless of which reading of legitimacy prevails internationally, and have no meaningful exit — mobility is state-controlled.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, west_bank_gaza_residents_under_closure, payer,
    powerless, biographical, trapped, regional).

% Political factions, the PLO/PA apparatus, and allied civil-society institutions that articulate and enforce this reading as the frame for legitimacy claims in diplomacy, law, and mobilization. They set and administer the narrative (which resolutions to cite, which historical framing to insist on), and derive organizational legitimacy, funding, and diplomatic standing from maintaining the reading's coherence — even when its practical yield (statehood, return) has stalled.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_national_movement, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, palestinian_national_movement, beneficiary).

% NGOs, academic centers, and transnational advocacy networks that build institutional and professional identity around the indigenous-continuity/settler-colonial framework. They gain funding, publication platforms, and standing from the reading's persistence as a live intellectual and political program; their material stake in the reading is largely decoupled from whether it changes conditions on the ground for refugees.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, diaspora_refugee_advocacy_networks, beneficiary,
    organized, generational, mobile, global).

% States and blocs (in UN forums, non-aligned coalitions) that invoke this reading to build diplomatic capital, anti-Western coalition politics, or domestic legitimacy, with minimal direct cost or stake in the underlying territory. They can adopt or drop the framing as international alignments shift.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, anti_colonial_solidarity_states, beneficiary,
    institutional, generational, arbitrage, global).

% Israeli Jewish citizens whose state's foundational legitimacy this reading structurally denies (framed as a settler-colonial entity requiring dissolution or fundamental transformation, not merely border adjustment). Materially powerful and secure in the near term, but the reading positions their state's existence itself as the object requiring remedy, which they experience as an existential rather than negotiable claim, foreclosing normal diplomatic compromise from their perspective.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_jewish_population_of_disputed_legitimacy_status, payer,
    powerful, generational, constrained, national).

% Would strenuously object to the framing that denies its foundational legitimacy, but this reading's own internal logic excludes the Israeli state's self-conception as a legitimate national liberation project from serious consideration within the frame — it appears in this reading only as the settler-colonial entity, not as a party with a competing indigenous or refugee-derived claim of its own (the Jewish return/refugee narrative from Middle Eastern and European expulsions).
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state_and_security_apparatus, excluded,
    institutional, generational, arbitrage, national).

% Assess competing legitimacy claims (indigenous continuity, partition legality, security necessity) against international law doctrines on self-determination, occupation, and refugee return. They are not parties to the conflict but their rulings and scholarship shift which reading has external legal purchase over time.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_law_scholars_and_tribunals, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Palestinian national identity, diaspora political mobilization, and international anti-colonial solidarity around a single continuous historical narrative — treating 1948 as an ongoing, unresolved dispossession rather than a settled founding event — which allows fragmented and dispersed populations (refugees, occupied residents, diaspora) to organize collective claims and international pressure under one coherent frame.
% TRANSFER_FUNCTION: Moves diplomatic legitimacy, moral standing, and international solidarity capital toward the Palestinian national movement and away from the Israeli state's self-narrative; moves organizational and institutional resources (funding, academic platforms, advocacy infrastructure) toward diaspora and solidarity networks; does not itself move material resources, land, or citizenship status to refugees or occupied residents, whose conditions remain governed by separate, unresolved political and military facts on the ground.
% ABSENT_VOICES: Israeli Jewish citizens and the Israeli state's own foundational narrative (including Jewish refugee displacement from Arab and Muslim states in the same period) are structurally excluded from consideration within this reading's frame — the reading treats their claim as the illegitimate object to be corrected, not as a competing indigenous or refugee claim requiring adjudication. Mizrahi and Sephardi Jewish refugee history in particular receives no seat in this reading despite structural parallels to the Palestinian refugee claim it centers.
% DISAPPEARANCE_RATIONALE: If this reading vanished from international discourse overnight, the Palestinian national movement's diplomatic vocabulary and the international solidarity architecture built around it would need reconstruction, and refugee advocacy organizations would lose a load-bearing framework — genuine institutional rearrangement. But the underlying facts (displacement, occupation, statelessness) would persist unchanged, since the reading is an interpretive and legitimating frame layered atop conditions, not the mechanism producing those conditions. Whether 'the world' rearranges depends on whether one means the political-diplomatic world (yes) or the material condition of refugees and occupied residents (largely no) — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The 1948 displacement of the majority of the Arab population of Mandatory Palestine, and the absence of any accepted legal, political, or historical framework that named this displacement as the central and ongoing fact requiring remedy, rather than as an unfortunate side effect of a legitimate state-founding or war.
% FOUNDING_PROBLEM_CORROBORATION: UN agencies (UNRWA's continued operation for multi-generational refugee registration) and numerous international law scholars and human rights bodies outside the Palestinian national movement corroborate that mass displacement occurred and that refugee status has not been resolved. However, whether the appropriate remedy is full return and dissolution of the settler-colonial framing (this reading) versus negotiated compensation/resettlement (implied by partition or security readings) remains sharply contested even among corroborating outside observers; no neutral outside body corroborates this reading's specific normative conclusion (illegitimacy of the Israeli state as such), only the underlying historical fact of displacement.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, contested).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate-high and rising level (0.35 to 0.68 over the interval) because the reading increasingly functions to sustain organizational, diplomatic, and academic infrastructure (advocacy networks, solidarity-state diplomacy) whose institutional stake has grown even as the material condition of refugees and occupied residents has not improved in step — an accumulating gap between the reading's coordination promise (unifying dispersed claims) and its extraction reality (resourcing institutions around an unresolved claim). Suppression is authored high and rising (0.5 to 0.72) reflecting that maintaining the reading's coherence requires actively excluding competing historical narratives (Jewish refugee displacement from Arab states, the security-necessity framing) from the same frame, and requires resisting revision even where practical political returns (statehood, right of return) have stalled for decades. Theater ratio is authored moderate and rising (0.1 to 0.28) — real solidarity and legal advocacy work is genuine, but a growing share of institutional activity maintains the narrative's coherence and international standing independent of its yield to actual refugees.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian national movement and diaspora advocacy networks are beneficiaries — they derive organizational legitimacy, funding, and diplomatic standing from the reading's persistence and coherence (low-to-moderate d). Anti-colonial solidarity states benefit diplomatically at low direct cost (low d, high mobility). Displaced 1948 refugees and occupied residents are structurally the reading's intended beneficiaries in principle but functionally payers in practice — trapped, powerless, bearing the ongoing costs of unresolved status regardless of the reading's international standing (high d despite nominal beneficiary status; this divergence between nominal and functional position is the story's central tension, and is why refugees carry a payer role primary with beneficiary secondary). Israeli Jewish citizens are payers under this specific reading (high d) because the reading's core claim treats their state's existence as the illegitimate object requiring remedy — an existential rather than distributive cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mass 1948 displacement lacking an accepted remedial framework) remains genuinely live in the narrow sense that displacement and statelessness persist uncorroborated resolution. But the reading's institutional apparatus (academic centers, solidarity diplomacy, advocacy funding) has partially decoupled from yielding material remedy to refugees, which is the mandatrophy signature: the mandate (secure return and remedy) has not been achieved, yet the apparatus built to pursue it has developed independent institutional life and interests. This is authored as contested rather than resolved because outside corroboration supports the underlying historical fact but not the specific verdict that the mandate itself has become obsolete — the refugees' claim has not disappeared, only the apparatus's relationship to resolving it has become more complex.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indigenous_continuity_identification,
    'This story instantiates the indigenous_continuity_reading of the territorial_legitimacy kernel. What would the sibling readings (partition_reading, security_necessity_reading) change structurally if adopted instead?',
    'Compare beneficiary/victim structures and epsilon across all three sibling constraint files: partition_reading would recognize the Israeli state''s legal legitimacy via UN Resolution 181 and shift the victim structure toward disputing only post-1967 territory rather than the state''s foundational existence; security_necessity_reading would ground legitimacy in post-1967 defensive control and treat the 1948 population transfer as a closed historical question, centering strategic depth and security rather than refugee return as the organizing claim.',
    'Adopting partition_reading instead would remove the Israeli state''s existence itself as the contested object, converting the dispute into a bounded territorial/border question with a narrower victim set (occupied residents post-1967, not the state''s foundational legitimacy). Adopting security_necessity_reading would additionally deprioritize the refugee right-of-return claim as structurally central, replacing it with negotiated resettlement or compensation framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indigenous_continuity_identification, conceptual, 'Location of the kernel contest: this reading''s premise (continuous indigenous habitation, ongoing Nakba) versus sibling premises (legal partition, security necessity).').

omega_variable(
    settler_colonial_framework_applicability,
    'Is the settler-colonial analytic framework the historically and structurally correct lens for the 1948 population transfer, or does it flatten a case with distinct features (post-Holocaust refugee state formation, competing indigenous claims from Jewish historical presence) that don''t fit the settler-colonial paradigm developed primarily from Anglophone settler states?',
    'Comparative historical analysis against canonical settler-colonial cases (Australia, North America, Algeria) on criteria such as pre-existing metropole relationship, demographic replacement intent, and competing indigenous claims; adjudicated by historians and comparative genocide/colonialism scholars outside both national movements.',
    'If the framework fits cleanly, this reading''s core premise is strengthened and the settler-colonial vindicated proposition holds firmly. If it fits poorly or only partially, the reading''s foundational axiom is weakened without necessarily validating either sibling reading, and the constraint''s claimed type could shift toward a more contested or asymmetric characterization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settler_colonial_framework_applicability, conceptual, 'Whether the settler-colonial analytic frame is the correct structural description of the 1948 events, an irreducibly contested historiographical question.').

omega_variable(
    mizrahi_refugee_claim_asymmetric_exclusion,
    'Does this reading''s exclusion of the Jewish refugee displacement from Arab and Muslim states (a roughly contemporaneous and comparably sized population transfer) reflect a genuine structural asymmetry between the two cases, or an authored omission driven by the reading''s political commitments?',
    'Comparative analysis of the two population transfers'' causes, state involvement, and remedial frameworks by historians outside both national movements'' advocacy apparatus.',
    'If the asymmetric treatment is not structurally justified, this reading''s absent_voices problem is more severe than authored, and the exclusion functions as suppression of a comparably legitimate competing claim rather than a defensible narrowing of scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mizrahi_refugee_claim_asymmetric_exclusion, conceptual, 'Whether the reading''s silence on Mizrahi/Sephardi Jewish refugee history is a defensible scope boundary or a suppressed competing indigenous-refugee claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1967, 0.14).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(terr_tr_t2012, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2012, 0.24).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1967, 0.42).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(terr_be_t2012, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2012, 0.6).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1967, 0.58).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1993, 0.62).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(terr_su_t2012, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2012, 0.68).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__security_necessity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the territorial_legitimacy kernel, decomposed per the epsilon-invariance principle because the natural-language label 'legitimate sovereignty over historic Palestine/Israel' covers structurally distinct claims with different beneficiary/victim structures and different epsilon values. indigenous_continuity_reading (this file) centers ongoing Nakba and full Palestinian sovereignty; partition_reading centers UN Resolution 181 legal recognition; security_necessity_reading centers post-1967 defensive control. Each carries its own epsilon and classification; they are linked here as a constraint family, not merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
