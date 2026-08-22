% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Territorial Legitimacy via Security Necessity (Strategic Depth Reading)
 *   domain: political/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the security-necessity reading of
 *   territorial legitimacy in the Israeli-Palestinian context: the claim that
 *   control of the West Bank, Golan Heights, and strategic buffer zones
 *   beyond the 1949 armistice lines is legitimated by defensive necessity
 *   arising from the 1967 and 1973 wars, and that Palestinian sovereignty is
 *   properly conditioned on verified demilitarization. This is ONE of three
 *   structurally distinct readings of a shared kernel
 *   (territorial_legitimacy) — the partition_reading (legitimacy via UN
 *   Resolution 181 and 1948 state recognition) and the
 *   indigenous_continuity_reading (legitimacy via continuous habitation and
 *   anti-colonial self-determination) are separate constraint stories with
 *   their own ε values, beneficiary/victim structures, and classifications.
 *   This story does not describe or adjudicate those readings; it authors
 *   only the security-necessity claim, assessed by its own lights, applied to
 *   the standing arrangement (military administration, settlement expansion,
 *   buffer-zone control) it is about.
 *
 * KEY AGENTS:
 *   - israeli_security_establishment: agenda_setter (institutional/arbitrage) — designs and administers the doctrine
 *   - settlement_movement: beneficiary (organized/mobile) — expands territorial presence under the security rationale
 *   - west_bank_palestinian_residents: payer (powerless/trapped) — bears the costs of buffer-zone administration
 *   - golan_druze_residents: payer (powerless/constrained) — bears indefinite unresolved status under strategic-depth logic
 *   - palestinian_statehood_claimants: payer (organized/constrained) — sovereignty conditioned on externally-set demilitarization benchmarks
 *   - international_legal_bodies: excluded (institutional/analytical) — rulings against the doctrine go unenforced
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.68).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.79).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Territorial Legitimacy via Security Necessity (Strategic Depth Reading)").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, 'c6560db2-8b97-424f-84ab-4351acab6e8a').
narrative_ontology:cs_kernel_codification('c6560db2-8b97-424f-84ab-4351acab6e8a', distributed).
narrative_ontology:cs_authority_grounding('c6560db2-8b97-424f-84ab-4351acab6e8a', extraction).
narrative_ontology:cs_interpretation_layer_present('c6560db2-8b97-424f-84ab-4351acab6e8a').
narrative_ontology:cs_reading_relation('c6560db2-8b97-424f-84ab-4351acab6e8a', territorial_legitimacy__partition_reading, influences).
narrative_ontology:cs_reading_relation('c6560db2-8b97-424f-84ab-4351acab6e8a', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('c6560db2-8b97-424f-84ab-4351acab6e8a', foundational, defensible_borders_supersede_prior_armistice_lines).
narrative_ontology:cs_axiom_status(defensible_borders_supersede_prior_armistice_lines, holdable).
narrative_ontology:cs_axiom_grounding('c6560db2-8b97-424f-84ab-4351acab6e8a', defensible_borders_supersede_prior_armistice_lines, instrumental).
narrative_ontology:cs_axiom('c6560db2-8b97-424f-84ab-4351acab6e8a', foundational, sovereignty_conditional_on_verified_demilitarization).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_verified_demilitarization, holdable).
narrative_ontology:cs_axiom_grounding('c6560db2-8b97-424f-84ab-4351acab6e8a', sovereignty_conditional_on_verified_demilitarization, instrumental).
narrative_ontology:cs_reference_frame('c6560db2-8b97-424f-84ab-4351acab6e8a', post_1967_defensible_borders_doctrine).
narrative_ontology:cs_drift_state('c6560db2-8b97-424f-84ab-4351acab6e8a', post_oslo_settlement_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c6560db2-8b97-424f-84ab-4351acab6e8a', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, settlement_movement).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, strategic_depth_advocates).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, west_bank_palestinian_residents).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, golan_druze_residents).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_statehood_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, regional_security_partners).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, defensible_borders_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, demilitarization_precondition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the security-necessity doctrine: military government over the West Bank, control of the Jordan Valley as a buffer, retention of the Golan Heights as high ground overlooking Israeli population centers, and the framing of settlement expansion as strategic presence rather than annexation. Sets the terms under which any territorial concession is evaluated against defensibility criteria it itself defines.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_security_establishment, agenda_setter,
    institutional, generational, arbitrage, national).

% Builds and expands civilian settlements justified under the security-presence rationale, receiving state subsidy, infrastructure, and military protection framed as necessary for strategic depth. Benefits directly from the doctrine's legitimation of continued territorial presence regardless of final-status outcomes.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, settlement_movement, beneficiary,
    organized, generational, mobile, regional).

% Military planners, security scholars, and political factions whose institutional relevance and policy influence depend on the continued salience of the security-necessity framework. Gain authority and resources from the doctrine's persistence as the dominant legitimacy narrative.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, strategic_depth_advocates, beneficiary,
    powerful, civilizational, analytical, national).

% Live under military administration justified by the security-necessity doctrine: checkpoints, land requisition for buffer zones and settlement expansion, and restricted movement all defended as defensive necessity. Their sovereignty claims are treated as contingent on demilitarization benchmarks they do not control and cannot unilaterally satisfy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, west_bank_palestinian_residents, payer,
    powerless, biographical, trapped, local).

% Reside in the Golan Heights under Israeli administration justified by the strategic-high-ground rationale. Many retain Syrian identity and citizenship claims; their political status remains unresolved and subordinate to the security framing that treats the territory's retention as non-negotiable.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, golan_druze_residents, payer,
    powerless, generational, constrained, local).

% Pursue sovereign statehood but face a legitimacy framework in which recognition is conditioned on demilitarization guarantees, security coordination, and territorial adjustments for buffer zones — terms set unilaterally by the security-necessity doctrine's administrators. Every incremental sovereignty gain is renegotiated against a shifting security threshold.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_statehood_claimants, payer,
    organized, civilizational, constrained, national).

% UN bodies and the ICJ have repeatedly found settlement activity and prolonged occupation inconsistent with international humanitarian law, but their rulings carry no enforcement mechanism against the security-necessity framework and are treated by its administrators as advisory at most.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_legal_bodies, excluded,
    institutional, generational, analytical, global).

% Neighboring states and security-cooperation partners assess the doctrine's stability implications for their own borders and threat calculus; some benefit from the buffer arrangement's regional deterrence value while remaining formally outside the territorial dispute.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, regional_security_partners, observer,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, regional_security_partners, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__security_necessity_reading, israeli_security_establishment).
narrative_ontology:fixing_cost_class(territorial_legitimacy__security_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a defensible-borders framework intended to reduce the risk of surprise attack and rocket/artillery range against population centers by retaining high ground and buffer territory — a genuine security-planning problem given the 1967 and 1973 war experiences.
% TRANSFER_FUNCTION: Moves land-use rights, freedom of movement, and sovereignty timelines from West Bank and Golan residents to the Israeli security establishment and settlement enterprise, in exchange for a security guarantee whose benchmarks are set and revised unilaterally by the beneficiary side.
% ABSENT_VOICES: West Bank and Golan residents subject to the buffer-zone and strategic-depth logic have no vote in Israeli security policy and limited standing in the international bodies whose rulings go unenforced; Palestinian negotiators are present but structurally unable to set or contest the demilitarization benchmarks themselves.
% DISAPPEARANCE_RATIONALE: If the security-necessity legitimation collapsed as a governing framework, settlement expansion would lose its primary domestic and international justification, military administration of the West Bank would face immediate pressure to convert to a negotiated sovereignty transfer, and Golan status would default to unresolved territorial dispute without the strategic-high-ground rationale anchoring it — the entire architecture of buffer zones, checkpoints, and conditional statehood would require renegotiation from a different premise.
% FOUNDING_PROBLEM: The 1967 and 1973 wars demonstrated that pre-1967 borders left Israeli population centers within artillery range and without defensible high ground, creating a genuine strategic vulnerability that the retention of the West Bank, Golan Heights, and Jordan Valley was intended to address.
% FOUNDING_PROBLEM_CORROBORATION: Israeli military planners and security scholars attest the vulnerability remains live given rocket proliferation and regional instability. Independent security analysts, including some retired Israeli generals (e.g., Council for Peace and Security veterans), and international legal bodies attest that modern missile range and air-defense capability have substantially reduced the original topographic rationale, and that settlement expansion in particular has exceeded any strict security-buffer logic — corroboration from outside the beneficiary set is mixed and points toward the founding problem being partially obsolete even as the doctrine persists.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 (high but not maximal) because the doctrine carries a genuine coordination kernel — the 1967/1973 vulnerability was real and the topographic logic was not fabricated — but the doctrine's benchmarks (what counts as sufficient demilitarization, how much settlement is 'security presence' versus annexation) are set and revised unilaterally by the beneficiary side over five decades, producing sustained asymmetric extraction layered onto the original coordination function. Suppression is authored higher (0.79) because the arrangement persists through active military administration, checkpoint infrastructure, and land requisition — coercive machinery, not voluntary alignment. Theater ratio rose from 0.15 to 0.42 over the interval as missile-defense technology (Iron Dome, precision-guided munitions doctrine) reduced the topographic rationale's practical force while settlement expansion continued to invoke it — a classic Goodhart drift where the justification's functional core eroded while its administrative and rhetorical use expanded.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (israeli_security_establishment) experiences this as ongoing, unresolved coordination — a live security problem requiring continuous management, closer to a rope or scaffold from that seat given the sunset-eligible framing often attached to 'temporary' security measures. The payer seats (West Bank and Golan residents) experience the same structure as enforced extraction with no credible sunset — permanent administration justified by a threshold that is never conclusively met because it is defined by the party benefiting from non-satisfaction. This divergence is exactly what the tangled_rope classification is built to hold: both a real coordination function (defensible borders) and real asymmetric extraction (unilaterally-set demilitarization goalposts) coexist in the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (israeli_security_establishment, settlement_movement, strategic_depth_advocates) sit near the beneficiary end of directionality because the doctrine's revision authority, resource flows, and territorial control all accrue to them with high exit/arbitrage capacity — they can adjust the doctrine's application without losing their underlying security or territorial position. Victims (west_bank_palestinian_residents, golan_druze_residents, palestinian_statehood_claimants) sit near the full-target end: trapped or constrained exit, no capacity to unilaterally satisfy or contest the benchmarks that gate their sovereignty, and the costs (land, movement, self-governance) are borne directly and continuously.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as 'contested' rather than 'dead' because there is corroborated disagreement even among security-establishment-adjacent voices (retired generals, independent analysts) about whether the topographic security rationale still holds given modern missile range. This prevents two symmetric mislabeling errors: treating the entire arrangement as pure fabricated pretext (ignoring the genuine 1967/1973-era vulnerability that motivated it) and treating it as pure ongoing necessity (ignoring that the benchmark-setting has been unilateral and unrevised for over fifty years despite technological change that undercuts the original logic). Tangled Rope captures both facts simultaneously rather than forcing a choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    topographic_rationale_technological_obsolescence,
    'Has missile-defense and precision-strike technology (Iron Dome, GPS-guided systems, drone warfare) substantially obsoleted the original topographic/strategic-depth rationale for retaining the West Bank and Golan, or does the rationale retain independent force given regional missile proliferation and non-state actor threats?',
    'Independent military-technical assessment (e.g., from RAND, IISS, or comparable non-partisan defense analysts) of how far modern air-defense and precision-strike capability substitutes for physical buffer distance in the specific terrain and threat environment; comparison with Israeli security establishment''s own internal threat assessments where declassified.',
    'If substantially obsoleted, the founding_problem_status shifts from contested toward dead, and continued territorial retention reads increasingly as inertial extraction (piton-adjacent) rather than live coordination — strengthening the tangled_rope reading''s extraction side. If the rationale retains independent force, the coordination function remains substantially live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(topographic_rationale_technological_obsolescence, empirical, 'Whether modern defense technology has obsoleted the topographic security rationale.').

omega_variable(
    demilitarization_benchmark_unilaterality,
    'Is the demilitarization benchmark that gates Palestinian sovereignty a jointly-negotiated, verifiable standard, or is it unilaterally defined and revised by the Israeli security establishment such that it functions as a moving target?',
    'Comparative analysis of past negotiation rounds (Oslo, Camp David 2000, Annapolis) to determine whether demilitarization criteria were fixed and mutually agreed or repeatedly redefined by one party; examination of whether any Palestinian demilitarization proposal has been accepted as sufficient.',
    'If unilaterally defined and never satisfiable in practice, this substantially strengthens the extraction reading (the security condition functions as permanent veto rather than negotiable threshold). If jointly negotiated with clear satisfaction criteria, the coordination function is stronger than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demilitarization_benchmark_unilaterality, empirical, 'Whether demilitarization benchmarks are jointly fixed or unilaterally moving.').

omega_variable(
    kernel_reading_framing_choice,
    'Is the security_necessity_reading properly evaluated against the standing military-administration and settlement arrangement (as authored here), or does a defensible version of this reading exist that treats only the pre-1993 buffer-zone logic as its referent, excluding post-Oslo settlement expansion as a separate, less-defensible extension?',
    'This is a conceptual framing choice, not an empirical one: a narrower version of the security_necessity_reading (buffer zones and military administration only, no settlement legitimation) would produce a lower ε and might classify closer to scaffold (temporary, sunset-eligible) than tangled_rope. The choice to include settlement legitimation in this reading''s scope was made because contemporary security-necessity arguments as actually deployed (e.g., in political and legal defenses of settlement activity) routinely fold settlement presence into the security-buffer rationale rather than treating them as distinct.',
    'A narrower framing excluding settlements would lower ε substantially and could shift the classification toward scaffold; the broader framing authored here (which tracks how the reading is actually used in practice) supports tangled_rope. This is documented as a conceptual omega per the CS-framing under-determination guidance rather than silently choosing one framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing_choice, conceptual, 'Whether the security-necessity reading should be scoped to buffer-zone administration alone or include settlement legitimation as authored.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__security_necessity_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(terr_tr_t1980, territorial_legitimacy__security_necessity_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__security_necessity_reading, theater_ratio, 1993, 0.28).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy__security_necessity_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy__security_necessity_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__security_necessity_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(terr_be_t1980, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2015, 0.64).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(terr_su_t1980, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1980, 0.62).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1993, 0.6).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2015, 0.76).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2024, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__security_necessity_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three members of the territorial_legitimacy kernel family. partition_reading grounds legitimacy in UN Resolution 181 and 1948 international recognition (different baseline year, different beneficiary/victim structure, likely lower ε if evaluated on its own coordination terms). indigenous_continuity_reading grounds legitimacy in continuous habitation and self-determination, treating 1948 as Nakba (structurally forecloses this reading's premise that 1948 sovereignty is the uncontested baseline). All three stories share the same underlying territorial dispute but instantiate structurally distinct constraints per the ε-invariance principle — each has its own ε, beneficiaries, victims, and classification, linked here for contamination-propagation and network analysis rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
