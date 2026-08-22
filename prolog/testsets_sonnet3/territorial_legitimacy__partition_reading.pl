% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Partition-Legality Reading of Territorial Legitimacy (UNGA Res. 181 / 1948 Borders)
 *   domain: political/legal/territorial
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   territorial_legitimacy kernel: legitimacy grounded in the international
 *   legal partition instrument (UNGA Resolution 181) and subsequent state
 *   recognition, using 1948 partition lines (and, by extension in most
 *   contemporary invocations, the 1967 ceasefire lines) as the boundary of
 *   what is legally legitimate. Under this reading, Israel's statehood within
 *   recognized borders and a prospective Palestinian state within the
 *   territory allocated to it are BOTH structurally legitimate, while
 *   settlement expansion beyond the 1967 lines is structurally illegitimate.
 *   This is not a claim about which reading is correct — it is the structural
 *   content of THIS reading, evaluated by its own lights. Sibling readings
 *   (security_necessity_reading, grounding legitimacy in defensive strategic
 *   depth rather than the partition instrument;
 *   indigenous_continuity_reading, grounding legitimacy in continuous
 *   habitation and treating 1948 as the Nakba rather than a lawful partition)
 *   are separate constraints with their own ε and stakeholder structures,
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - israeli_state_within_partition_lines: primary beneficiary of legal recognition (institutional/arbitrage)
 *   - palestinian_statehood_claimants: intended co-beneficiary whose entitlement was never realized (organized/constrained)
 *   - palestinian_communities_displaced_1948: bears the reading's structural blind spot on refugee return (powerless/trapped)
 *   - settler_populations_beyond_1967_lines: rendered illegitimate by this reading's own logic (organized/constrained)
 *   - international_legal_order_institutions: beneficiary of the doctrine's continued citation (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.58).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.62).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Partition-Legality Reading of Territorial Legitimacy (UNGA Res. 181 / 1948 Borders)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political/legal/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, '917fe134-e497-42ed-a8c3-dc37ec82f34d').
narrative_ontology:cs_kernel_codification('917fe134-e497-42ed-a8c3-dc37ec82f34d', formalized).
narrative_ontology:cs_authority_grounding('917fe134-e497-42ed-a8c3-dc37ec82f34d', lineage).
narrative_ontology:cs_interpretation_layer_present('917fe134-e497-42ed-a8c3-dc37ec82f34d').
narrative_ontology:cs_reading_relation('917fe134-e497-42ed-a8c3-dc37ec82f34d', territorial_legitimacy__security_necessity_reading, influences).
narrative_ontology:cs_reading_relation('917fe134-e497-42ed-a8c3-dc37ec82f34d', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('917fe134-e497-42ed-a8c3-dc37ec82f34d', foundational, un_partition_instrument_confers_valid_statehood).
narrative_ontology:cs_axiom_status(un_partition_instrument_confers_valid_statehood, holdable).
narrative_ontology:cs_axiom_grounding('917fe134-e497-42ed-a8c3-dc37ec82f34d', un_partition_instrument_confers_valid_statehood, conventional).
narrative_ontology:cs_axiom('917fe134-e497-42ed-a8c3-dc37ec82f34d', foundational, post_1967_settlement_beyond_recognized_lines_is_illegitimate).
narrative_ontology:cs_axiom_status(post_1967_settlement_beyond_recognized_lines_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('917fe134-e497-42ed-a8c3-dc37ec82f34d', post_1967_settlement_beyond_recognized_lines_is_illegitimate, conventional).
narrative_ontology:cs_reference_frame('917fe134-e497-42ed-a8c3-dc37ec82f34d', un_general_assembly_partition_mandate).
narrative_ontology:cs_drift_state('917fe134-e497-42ed-a8c3-dc37ec82f34d', post_oslo_two_state_stalemate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('917fe134-e497-42ed-a8c3-dc37ec82f34d', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, israeli_state_within_partition_lines).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, international_legal_order_institutions).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, settler_populations_beyond_1967_lines).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_communities_displaced_1948).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_statehood_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, palestinian_statehood_claimants).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, un_partition_resolution_legal_authority).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, two_state_framework_viability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derives its clearest international legal legitimacy claim from UNGA Resolution 181's partition mandate and subsequent state recognition. This reading grants the state's existence firm legal footing within recognized boundaries, while creating exposure on territory acquired or settled beyond the 1967 lines, which the reading treats as outside the legitimating instrument.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_state_within_partition_lines, beneficiary,
    institutional, generational, arbitrage, national).

% The partition reading is the source of their strongest available legal claim to a parallel state — Resolution 181 allocated territory to an Arab state that was never seated. They benefit from the reading's affirmation that a Palestinian state is legally due, but pay through the reading's inability to compel implementation: the promised state has never materialized, and the reading offers a legal claim without an enforcement path.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_statehood_claimants, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, palestinian_statehood_claimants, beneficiary).

% Displaced during and after the 1948 war that followed partition and the failure of the Arab state to be established. Under this reading, their displacement is a consequence of rejected partition terms and subsequent conflict, not a foundational wrong requiring remedy through this legal instrument — the reading structurally underweights refugee return claims relative to state-recognition claims, leaving this population's core grievance outside what the reading can resolve.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_communities_displaced_1948, payer,
    powerless, generational, trapped, regional).

% Have built permanent communities, infrastructure, and multi-generational residence beyond the 1967 lines, often with state support. Under this reading their settlements sit outside any territory legitimated by the partition and later ceasefire lines, making their presence, however entrenched, a standing illegitimacy the reading cannot resolve without relocation or a negotiated land-swap settlement they did not choose.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, settler_populations_beyond_1967_lines, payer,
    organized, biographical, constrained, regional).

% The UN system and the broader postwar international legal order derive continuing authority from the precedent that a General Assembly partition resolution can generate durable statehood claims. Every invocation of Resolution 181 as a legitimating instrument reinforces the institution's relevance and the doctrine that international law, not military outcome or continuous habitation, is the proper ground of sovereignty.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_legal_order_institutions, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, international_legal_order_institutions, agenda_setter).

% States and diplomatic bodies that invoke the 1967-lines-plus-partition-legality framework as the basis for two-state negotiations, peace plans, and recognition policy. They can shift resources, recognition, and diplomatic pressure based on which reading of legitimacy they adopt, without themselves bearing the territorial consequences.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, third_party_states_and_mediators, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__partition_reading, israeli_state_within_partition_lines).
narrative_ontology:fixing_cost_class(territorial_legitimacy__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, third-party-adjudicable standard — codified international law and UN-recognized boundaries — that lets states, mediators, and courts evaluate competing territorial claims without relying on force of arms or continuous-habitation genealogy alone, in principle enabling a negotiated two-state settlement.
% TRANSFER_FUNCTION: Legal legitimacy and recognition flow toward the state (Israel) whose founding instrument this reading treats as valid, and toward the promise of a second state (Palestine) that the same instrument authorized but never delivered; the costs of the gap between legal entitlement and physical realization are borne by displaced 1948 refugees, unrecognized Palestinian statehood claimants, and post-1967 settlers whose facts on the ground the reading treats as illegitimate.
% ABSENT_VOICES: Palestinian refugees and their descendants outside the West Bank and Gaza are rarely direct parties to negotiations conducted under this framework; their return claims are treated as a secondary 'final status' issue rather than a founding element of the legitimacy question. Settler communities, though organized and vocal domestically, are excluded from the international legal conversation about whether their presence is a legitimate long-term fact.
% DISAPPEARANCE_RATIONALE: If the partition-legality reading were abandoned as a legitimating framework, the entire architecture of two-state negotiations, UN resolutions referencing 1967 lines, and international recognition policy toward both a fixed-border Israel and a prospective Palestinian state would lose its legal anchor; diplomacy would have to re-ground itself in either security-necessity claims or indigenous-continuity claims, each of which redraws the legitimate map differently.
% FOUNDING_PROBLEM: In 1947-48, the departing British Mandate authority and the United Nations needed a legal mechanism to resolve competing Jewish and Arab claims to Mandatory Palestine without continued colonial administration or open-ended civil war; partition into two states was proposed as the coordinating instrument.
% FOUNDING_PROBLEM_CORROBORATION: UN institutional history and international law scholarship (largely outside either party's direct beneficiary interest) attest that Resolution 181 was adopted as a partition mechanism and that the Arab state it authorized was never established due to rejection and subsequent war — a status independent legal historians and UN archival records corroborate. Whether the founding problem (competing claims requiring adjudication) remains live or has been superseded by facts on the ground is disputed by the parties themselves; neither the Israeli state nor Palestinian claimant institutions are neutral corroborators of this status question, and no fully disinterested corroborating body exists — this absence of a disinterested corroborator is itself part of the story.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the partition-legality reading, while providing a genuine coordination function (a shared adjudicable standard replacing pure force), also structurally transfers legitimacy and international standing toward the party whose statehood the instrument successfully delivered while treating the undelivered half of its own promise, and the resulting refugee displacement, as a secondary or unresolved matter rather than a foundational cost. Suppression (0.62) is elevated because maintaining this reading as the operative diplomatic standard requires continuous enforcement — UN votes, recognition policy, sanctions regimes, and treaty conditionality all actively work to keep alternative legitimating frameworks (indigenous continuity, security necessity) from displacing it in international forums. Theater ratio (0.40) captures that a substantial share of invocation of the partition framework in diplomatic settings functions as performative reaffirmation (resolutions, statements) rather than instruments that change facts on the ground, particularly regarding refugee return and settlement rollback. Resistance (0.72) is high because every party dissatisfied with where this reading lands — settlers, refugee advocates, and rejectionist actors on both sides — actively contests it. Accessibility collapse (0.45) is moderate: unlike a mountain, alternative legitimating frameworks remain fully articulable and are actively advanced by other parties (hence three sibling readings existing at all), so alternatives have not collapsed.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state within partition lines and the international legal institutions that cite Resolution 181 as precedent sit closest to the beneficiary end: the former gets its clearest legal claim, the latter gets a validated doctrine. Palestinian statehood claimants occupy a genuinely mixed position — beneficiaries of the reading's legal logic (a state was promised to them) but payers of its non-implementation (the state never arrived) — hence the dual role. Displaced 1948 communities and post-1967 settlers are structural payers: the former because the reading's founding-document focus underweights the refugee-return claim relative to the state-recognition claim; the latter because the very reading that legitimates Israel's core state delegitimizes their specific settlements. Neither group chose this asymmetry, and neither has a low-cost exit — refugees cannot un-displace themselves, and settlers cannot retroactively relocate their communities into 1967-legitimate territory.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competing claims to Mandatory Palestine requiring third-party legal adjudication rather than open war) is genuinely contested as to whether it is still live: for parties who accept the partition framework as the ongoing basis for a negotiated two-state outcome, the problem remains live and the instrument still does real coordination work. For parties who read the failure to implement the Arab state and the accumulation of settlements as evidence that the instrument's core promise died decades ago while its citation persists as legal theater, the reading has drifted toward a zombie mandate — cited to legitimate one outcome (Israeli statehood) while never having delivered its paired outcome (Palestinian statehood). The tangled_rope classification captures exactly this: a real coordination function (a legal standard displacing force) coexists with asymmetric extraction (one promised beneficiary receives, the other does not, and displaced populations bear the gap) sustained by active diplomatic and legal enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_reading_vs_security_necessity_boundary,
    'Where the partition-legality reading and the security-necessity reading disagree about post-1967 territory, is the disagreement located in what counts as the relevant legitimating instrument (UN resolution vs. defensive necessity) or in a shared acceptance of 1967 as a baseline with disagreement only about permissible departures from it?',
    'Comparative analysis of how each reading''s own advocates justify departures from 1967 lines — if security-necessity advocates concede the partition instrument''s authority but argue an exception, the readings partially overlap rather than fully diverge; if they reject the instrument''s authority outright, the divergence is total.',
    'If the readings share more structural ground than a strict decomposition suggests, an `influences` relation may understate the coupling; if they are genuinely independent legitimating logics, `coexists_with` is the more accurate structural relation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_reading_vs_security_necessity_boundary, conceptual, 'Whether partition-legality and security-necessity readings share or fully diverge on the 1967-line baseline.').

omega_variable(
    founding_problem_disinterested_corroboration_gap,
    'Is there any institutional or scholarly body positioned to corroborate the founding-problem status question (live vs. dead) that is genuinely outside the interest structure of either the Israeli state, Palestinian claimant institutions, or the UN system whose own authority the reading vindicates?',
    'Systematic review of international law scholarship, ICJ advisory opinions, and third-state diplomatic archives for assessments that neither cite state interest nor UN institutional continuity as a premise.',
    'If no such disinterested corroborator exists, the founding_problem_status answer of ''contested'' should be read as structurally uncorroborated rather than merely disputed — a stronger caution about the reading''s self-referential legitimation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_disinterested_corroboration_gap, empirical, 'Whether any truly disinterested corroborator exists for the founding-problem status claim.').

omega_variable(
    reading_selection_under_determination,
    'The territorial_legitimacy kernel could be framed around the partition instrument itself (this reading), around the 1967 ceasefire lines plus security doctrine (sibling), or around continuous habitation and decolonization theory (sibling) — is the choice of which framing is ''the'' natural starting point for diplomatic practice itself contested, or does international diplomatic convention treat the partition/1967 framing as primary by default?',
    'Survey of UN Security Council resolution language, Oslo-era agreements, and post-2000 peace-plan texts to determine which legitimating logic is invoked as the default baseline versus argued for as a contested position.',
    'If diplomatic convention already privileges this reading as the default baseline, that convention itself is doing legitimating work the reading''s own metrics do not capture — potentially understating this reading''s structural power relative to its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_under_determination, conceptual, 'Whether the partition/1967 framing enjoys unearned default status in diplomatic convention, independent of its substantive merits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__partition_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__partition_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(terr_tr_t1980, territorial_legitimacy__partition_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__partition_reading, theater_ratio, 1993, 0.32).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy__partition_reading, theater_ratio, 2005, 0.36).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy__partition_reading, theater_ratio, 2015, 0.39).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__partition_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__partition_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__partition_reading, base_extractiveness, 1967, 0.42).
narrative_ontology:measurement(terr_be_t1980, territorial_legitimacy__partition_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__partition_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy__partition_reading, base_extractiveness, 2005, 0.54).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy__partition_reading, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__partition_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__partition_reading, suppression_requirement, 1948, 0.4).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__partition_reading, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(terr_su_t1980, territorial_legitimacy__partition_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__partition_reading, suppression_requirement, 1993, 0.58).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy__partition_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy__partition_reading, suppression_requirement, 2015, 0.61).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__partition_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__partition_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the territorial_legitimacy kernel. partition_reading (this file) grounds legitimacy in UNGA Resolution 181 and subsequent recognition, treating both Israeli statehood within recognized lines and Palestinian statehood claims as legitimate while treating post-1967 settlement as illegitimate. security_necessity_reading grounds legitimacy in defensive strategic depth, potentially legitimating some post-1967 territorial control this reading rejects. indigenous_continuity_reading grounds legitimacy in continuous habitation and anti-colonial self-determination, treating 1948 itself as dispossession (the Nakba) rather than as this reading's foundational lawful instrument — the sharpest structural divergence in the triplet. Each reading carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__partition_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
