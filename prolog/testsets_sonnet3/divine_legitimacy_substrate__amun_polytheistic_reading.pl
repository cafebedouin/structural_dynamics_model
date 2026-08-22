% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__amun_polytheistic_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Amun-Ra Priestly Interpretive Monopoly on Divine Legitimacy
 *   domain: religious/political economy
 *
 * SUMMARY:
 *   This constraint models the standing arrangement under the Amun-Ra
 *   polytheistic reading of divine legitimacy in New Kingdom Egypt: the Amun
 *   priesthood at Karnak holds interpretive authority over a multi-deity
 *   cosmology in which Amun-Ra is chief patron, and this interpretive
 *   monopoly is the mechanism by which pharaonic authority is validated. Over
 *   centuries the temple economy accumulated land, labor, and grain reserves
 *   that rivaled crown wealth, while the priesthood's oracular pronouncements
 *   on succession gave it leverage over royal decision-making. This is a
 *   genuine coordination structure — providing legible, transmissible
 *   legitimacy across succession crises and unifying regional cults under a
 *   common hierarchy — layered with substantial asymmetric extraction from
 *   peasant taxpayers and subordinated provincial cults. The claimed type
 *   (tangled_rope) and the metrics (moderate-to-high extraction, moderate
 *   suppression, rising theater ratio) are authored independently and happen
 *   to agree; the engine's computation from the structural data is what
 *   actually certifies this.
 *
 * KEY AGENTS:
 *   - amun_priesthood: agenda_setter, controls oracular interpretation and doctrine
 *   - karnak_temple_economy: beneficiary, accumulates land and tribute
 *   - pharaonic_office: beneficiary and payer, receives legitimacy but funds and defers to the priesthood
 *   - peasant_taxpayers: payer, powerless, trapped, funds the entire apparatus
 *   - provincial_temple_cults: payer, moderate power, must accommodate subordination to Amun-Ra
 *   - pharaoh_political_autonomy: the constrained capacity itself, non-agent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.62).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.55).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Amun-Ra Priestly Interpretive Monopoly on Divine Legitimacy").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "religious/political economy").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, '19d87820-36a0-4c0b-946c-af37fda52be2').
narrative_ontology:cs_kernel_codification('19d87820-36a0-4c0b-946c-af37fda52be2', distributed).
narrative_ontology:cs_authority_grounding('19d87820-36a0-4c0b-946c-af37fda52be2', lineage).
narrative_ontology:cs_interpretation_layer_present('19d87820-36a0-4c0b-946c-af37fda52be2').
narrative_ontology:cs_reading_relation('19d87820-36a0-4c0b-946c-af37fda52be2', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('19d87820-36a0-4c0b-946c-af37fda52be2', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('19d87820-36a0-4c0b-946c-af37fda52be2', foundational, legitimacy_requires_priestly_ratification).
narrative_ontology:cs_axiom_status(legitimacy_requires_priestly_ratification, holdable).
narrative_ontology:cs_axiom_grounding('19d87820-36a0-4c0b-946c-af37fda52be2', legitimacy_requires_priestly_ratification, conventional).
narrative_ontology:cs_axiom('19d87820-36a0-4c0b-946c-af37fda52be2', foundational, multiple_deities_coexist_under_amun_ra_supremacy).
narrative_ontology:cs_axiom_status(multiple_deities_coexist_under_amun_ra_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('19d87820-36a0-4c0b-946c-af37fda52be2', multiple_deities_coexist_under_amun_ra_supremacy, theological).
narrative_ontology:cs_reference_frame('19d87820-36a0-4c0b-946c-af37fda52be2', amun_ra_chief_patron_synthesis).
narrative_ontology:cs_drift_state('19d87820-36a0-4c0b-946c-af37fda52be2', late_new_kingdom_temple_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('19d87820-36a0-4c0b-946c-af37fda52be2', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, karnak_temple_economy).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_office).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, peasant_taxpayers).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, provincial_temple_cults).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh_political_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_office).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the oracular interpretation of Amun-Ra's will, the coronation and jubilee rites that confer legitimacy, and vast temple estates and grain reserves accumulated through mandatory offerings. Sets doctrine on which pharaonic acts are divinely sanctioned and can withhold or grant validation. Exits are not needed — the priesthood's position is the institution itself.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Receives land grants, labor levies, and tribute justified by the need to maintain Amun-Ra's cult and the cosmological order it underwrites. Accumulates wealth and land holdings that rival the crown's own treasury over generations, entirely dependent on the interpretive framework remaining unchallenged.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, karnak_temple_economy, beneficiary,
    institutional, civilizational, arbitrage, national).

% Receives legitimacy and the mandate to rule by being declared the living embodiment of Horus and beloved of Amun-Ra, validated through priestly ritual. In exchange, must fund temple construction, endow priestly estates, and defer to oracular pronouncements on succession and major decisions — the pharaoh benefits from the cosmology but pays a real political price for the validation it requires.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_office, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_office, payer).

% Render grain, labor corvée, and goods to temple estates as offerings sustaining the cosmological order that legitimizes both priesthood and pharaoh. Have no standing to question the interpretation and no practical way to withhold contributions administered through the state-temple apparatus that also assesses their taxes.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, peasant_taxpayers, payer,
    powerless, biographical, trapped, local).

% Local deity cults must accommodate Amun-Ra's supremacy within the interpretive hierarchy or lose royal patronage and legal standing. Regional priesthoods can negotiate syncretic accommodation (Amun-Ra absorbing or pairing with local gods) but cannot reject the hierarchy outright without risking resources and legitimacy.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, provincial_temple_cults, payer,
    moderate, generational, constrained, regional).

% The pharaoh's capacity for unilateral religious or administrative reform is the thing constrained — any attempt to bypass or diminish priestly interpretive authority (as later attempted under Akhenaten) triggers doctrinal delegitimization and resource withdrawal from the most powerful institutional actor in the kingdom besides the throne itself.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh_political_autonomy, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh_political_autonomy).

% Reconstruct the arrangement from temple inscriptions, administrative papyri, and the Amarna interruption as a case study in how interpretive religious authority accumulates institutional power parallel to and sometimes exceeding secular rule.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, later_historians_and_priesthoods, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__amun_polytheistic_reading, diffuse).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__amun_polytheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, legible cosmological order that legitimizes succession, arbitrates disputes over royal authority, and gives disparate regional cults a common hierarchy under Amun-Ra, reducing the risk of legitimacy crises and civil fragmentation.
% TRANSFER_FUNCTION: Moves grain, land, labor, and tribute from peasant taxpayers and provincial temples to the Amun priesthood and its temple economy, in exchange for the priesthood certifying pharaonic legitimacy and maintaining cosmological continuity.
% ABSENT_VOICES: Peasant taxpayers who fund the entire apparatus through corvée and tithe have no interpretive voice in the cosmology that justifies their contributions; provincial cults that might prefer a flatter, non-hierarchical pantheon are absent from the doctrinal settlement that subordinates them to Amun-Ra.
% DISAPPEARANCE_RATIONALE: If the Amun priesthood's interpretive monopoly vanished, temple land and grain reserves would need new administration, royal succession would lose its ritual validation mechanism and require an alternative legitimacy source, and provincial cults would likely fragment into independent regional hierarchies rather than defer to a single chief patron — this nearly happened during the Amarna interlude and reversed immediately upon Tutankhamun's restoration.
% FOUNDING_PROBLEM: Early dynastic and Middle Kingdom Egypt needed a stable, transmissible justification for pharaonic authority across succession crises, regional rivalries, and periods of weak central rule — a cosmological order that made the king's rule appear necessary and eternal rather than contingent on military force alone.
% FOUNDING_PROBLEM_CORROBORATION: Temple inscriptions and priestly records (produced by the beneficiary institution itself) attest the arrangement as eternal cosmic necessity. Independent corroboration is thin: administrative papyri documenting land consolidation under the Amun priesthood by the late New Kingdom, and the Amarna episode itself — where a pharaoh attempted to dissolve the arrangement entirely — are read by modern Egyptologists as evidence the founding legitimation problem had long since been solved and the priesthood's continued claim on resources was self-perpetuating rather than functionally necessary.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises from 0.35 to 0.62 across the interval as the temple economy consolidates land holdings and the priesthood's administrative reach grows beyond its original ritual function — the classic pattern of coordination accumulating extractive overhead. Theater ratio rises in parallel (0.30 to 0.48) as oracular and ritual performance increasingly substitutes for genuine crisis-resolution function once succession disputes become routine rather than existential. Suppression tracks a middle path (0.35 to 0.55): this is not primarily a coercive constraint enforced by violence, but one enforced by doctrinal exclusion, resource withdrawal, and the near-total absence of an alternative interpretive framework for peasants and provincial cults, who lack the analytical distance to contest cosmological claims.
 *
 * DIRECTIONALITY LOGIC:
 *   The Amun priesthood and the temple economy sit at the full-beneficiary end: they administer, collect, and are structurally arbitraged against any single reform effort. The pharaoh occupies a genuinely mixed position — benefiting from the legitimacy the priesthood confers while paying a real and rising cost in land grants, deference, and constrained autonomy; this is why pharaonic_office carries both beneficiary and payer roles. Peasant taxpayers are the clearest full-target case: trapped, powerless, and bearing the material cost of an arrangement whose justificatory content they have no power to contest. Provincial cults occupy an intermediate position, able to negotiate syncretic accommodation but not exit the hierarchy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the need for a stable, transmissible legitimacy mechanism to prevent succession crises and civil fragmentation — is genuinely contested rather than simply dead: it recurred sharply during the Amarna period when Akhenaten's attempted abolition of the Amun cult provoked civil and administrative crisis, suggesting the coordination function had not fully atrophied even as the extractive overhead had grown large. This prevents a simple snare classification: there was a real problem being solved, but by the late New Kingdom the priesthood's accumulated economic and political weight (rivaling the crown's own resources) suggests the mandate had substantially outlived its minimal necessary form, layering heavy extraction onto a genuine but increasingly vestigial coordination core — the tangled_rope classification captures this hybrid rather than collapsing it into either pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary_amun,
    'How much of the Amun priesthood''s accumulated economic power reflects necessary institutional overhead for maintaining legitimacy coordination, versus purely extractive rent-seeking that outgrew the coordination function?',
    'Comparative analysis of temple land holdings and administrative complexity across periods of stable versus contested succession — if extraction remained flat during high-legitimacy-crisis periods and rose mainly during stable periods, this points toward rent-seeking rather than crisis-coordination cost.',
    'If extraction tracked genuine coordination need, the tangled_rope classification understates the coordination function; if extraction rose independent of coordination need (as the temporal data suggests), the classification is conservative and a drift toward snare is the more accurate long-run reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_amun, empirical, 'Whether accumulated priestly wealth reflects coordination cost or rent-seeking drift.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the Amun-Ra polytheistic framing the dominant lived reality of divine legitimacy for most of New Kingdom Egypt, or is it primarily the elite/state-level reading that coexists with a much more syncretistic lived religious practice at the household and village level (the folk_syncretistic_reading)?',
    'Archaeological and papyrological evidence of household shrine practice versus state temple records — divergence between the two evidentiary bases would indicate the readings coexist as genuinely separate, non-competing legitimacy substrates operating at different social scales rather than one being the ''true'' account and the other derivative.',
    'If the folk reading dominates lived practice, the Amun polytheistic reading''s extraction and suppression figures may overstate its reach into non-elite religious life, since peasant_taxpayers may experience household syncretism as primary and state cosmology as a secondary tax obligation with little doctrinal weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the Amun reading is the dominant legitimacy substrate or one of several coexisting scale-specific readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(divi_tr_t100, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 100, 0.34).
narrative_ontology:measurement(divi_tr_t200, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 200, 0.38).
narrative_ontology:measurement(divi_tr_t300, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 300, 0.42).
narrative_ontology:measurement(divi_tr_t400, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 400, 0.46).
narrative_ontology:measurement(divi_tr_t500, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 500, 0.48).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(divi_be_t100, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 100, 0.42).
narrative_ontology:measurement(divi_be_t200, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 200, 0.5).
narrative_ontology:measurement(divi_be_t300, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 300, 0.56).
narrative_ontology:measurement(divi_be_t400, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 400, 0.6).
narrative_ontology:measurement(divi_be_t500, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 500, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(divi_su_t100, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement(divi_su_t200, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 200, 0.45).
narrative_ontology:measurement(divi_su_t300, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 300, 0.5).
narrative_ontology:measurement(divi_su_t400, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 400, 0.53).
narrative_ontology:measurement(divi_su_t500, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 500, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, atenist_monotheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the divine_legitimacy_substrate kernel. The atenist_monotheistic_reading forecloses this reading's core premise (multi-deity coexistence under priestly mediation is logically incompatible with exclusive pharaonic revelation of a single deity) — historically this foreclosure was attempted and then reversed. The folk_syncretistic_reading coexists with this reading: household-level pragmatic polytheism and state-level Amun-centered doctrinal hierarchy can and did operate simultaneously at different social scales without either eliminating the other. Each reading carries its own ε, beneficiary/victim structure, and classification; they are not three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
