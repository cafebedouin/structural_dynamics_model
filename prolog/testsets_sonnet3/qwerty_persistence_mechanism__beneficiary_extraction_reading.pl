% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__beneficiary_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__beneficiary_extraction_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: QWERTY Keyboard Standard as Maintained Extraction Arrangement
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   This story instantiates the beneficiary-extraction reading of the QWERTY
 *   persistence kernel: the claim that QWERTY's dominance since the 1870s is
 *   best explained not by technical adequacy (naturalization reading) or by
 *   pure coordination inertia (lock-in reading), but by active, ongoing
 *   maintenance work performed by identifiable beneficiaries — Remington and
 *   its cartel successors, incumbent typing schools, and certification bodies
 *   — who profit from switching costs they helped construct and continue to
 *   police. The theater ratio rises over the interval because as the original
 *   mechanical jamming problem became obsolete (with improved typebars and
 *   later electric/electronic keyboards), an increasing share of the
 *   apparatus defending QWERTY (curricula, certification standards, 'proven
 *   and tested' marketing claims) became justificatory theater rather than
 *   functional necessity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.71).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Keyboard Standard as Maintained Extraction Arrangement").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic_history/technology_studies/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, '57b503fc-2993-4da9-b65b-9c6225b747c8').
narrative_ontology:cs_kernel_codification('57b503fc-2993-4da9-b65b-9c6225b747c8', distributed).
narrative_ontology:cs_authority_grounding('57b503fc-2993-4da9-b65b-9c6225b747c8', extraction).
narrative_ontology:cs_interpretation_layer_present('57b503fc-2993-4da9-b65b-9c6225b747c8').
narrative_ontology:cs_reading_relation('57b503fc-2993-4da9-b65b-9c6225b747c8', qwerty_persistence_mechanism__naturalization_reading, forecloses).
narrative_ontology:cs_reading_relation('57b503fc-2993-4da9-b65b-9c6225b747c8', qwerty_persistence_mechanism__lock_in_reading, influences).
narrative_ontology:cs_axiom('57b503fc-2993-4da9-b65b-9c6225b747c8', foundational, persistence_reflects_administered_extraction_not_merit).
narrative_ontology:cs_axiom_status(persistence_reflects_administered_extraction_not_merit, holdable).
narrative_ontology:cs_axiom_grounding('57b503fc-2993-4da9-b65b-9c6225b747c8', persistence_reflects_administered_extraction_not_merit, empirically_contingent).
narrative_ontology:cs_axiom('57b503fc-2993-4da9-b65b-9c6225b747c8', secondary, switching_costs_were_constructed_not_emergent).
narrative_ontology:cs_axiom_status(switching_costs_were_constructed_not_emergent, holdable).
narrative_ontology:cs_axiom_grounding('57b503fc-2993-4da9-b65b-9c6225b747c8', switching_costs_were_constructed_not_emergent, empirically_contingent).
narrative_ontology:cs_reference_frame('57b503fc-2993-4da9-b65b-9c6225b747c8', mechanical_jam_prevention_standard).
narrative_ontology:cs_drift_state('57b503fc-2993-4da9-b65b-9c6225b747c8', post_electronic_keyboard_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('57b503fc-2993-4da9-b65b-9c6225b747c8', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_and_successor_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, touch_typing_certification_bodies).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, novice_typists).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_layout_inventors).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, clerical_workforce_entrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Standardized on the QWERTY arrangement in the 1870s-1880s and then actively worked, through trade agreements, dealer networks, and coordinated marketing with rival manufacturers (culminating in Union Typewriter Company's cartelization), to keep every subsequent machine compatible with it. Benefits from every new typist trained on QWERTY becoming a captive customer for QWERTY-compatible machines and from competitors' switching costs being borne by the market, not by the firm.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_and_successor_manufacturers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_and_successor_manufacturers, beneficiary).

% Built curricula, certification exams, and speed-typing competitions entirely around QWERTY finger patterns starting in the 1880s-1890s. Their institutional capital (instructors, textbooks, standardized tests) is denominated in QWERTY proficiency; they profit from every student who must relearn QWERTY and have no incentive to teach or certify alternative layouts, effectively gatekeeping entry into clerical employment through a layout they helped entrench.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools, beneficiary,
    organized, biographical, arbitrage, national).

% Issued the credentials employers required for clerical hiring, all denominated in QWERTY words-per-minute. Controlled the metric by which typists were judged employable, which meant controlling which layout was employable at all — a structural veto over any alternative layout's entry into the labor market regardless of the alternative's technical merit.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, touch_typing_certification_bodies, beneficiary,
    organized, generational, arbitrage, national).

% Must invest months learning QWERTY specifically because that is the only layout schools teach and employers test for, regardless of whether an alternative would let them type faster or with less strain. Their sunk training cost, once paid, becomes the very switching cost that later locks them and the market to QWERTY — they pay to enter a system they had no role in designing and cannot exit without repeating the sunk cost.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, novice_typists, payer,
    powerless, biographical, trapped, national).

% Designed layouts (most prominently Dvorak, 1936) demonstrating measurable speed and fatigue advantages in controlled trials, but found no path to market: no school would teach it, no employer would test for it, and no manufacturer would tool for it at scale, because doing so would devalue the training and hardware capital already invested in QWERTY. Their technical claims were never defeated on the merits; they were excluded from the venues where merit could be demonstrated at scale.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_layout_inventors, payer,
    moderate, biographical, trapped, national).

% Especially women entering typing-pool clerical work from the 1880s onward, for whom typing certification was often the only credentialed path into paid office work. Had to accept whatever layout the certification apparatus demanded; had no market power to demand or even request an alternative, since refusing QWERTY meant refusing employment.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, clerical_workforce_entrants, payer,
    powerless, biographical, trapped, national).

% Study the QWERTY case as the canonical path-dependence example, debating (this is the live contest) whether persistence reflects genuine coordination lock-in, adequate technical performance, or active incumbent maintenance of switching costs for profit. Their disagreement is the kernel this story is one reading of.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_and_successor_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__beneficiary_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is a genuine coordination problem underneath the extraction: a shared keyboard layout lets typists move between employers and machines without retraining, and lets manufacturers build one product line instead of many. QWERTY did solve this once it was adopted widely enough.
% TRANSFER_FUNCTION: Moves the cost of standardization from the manufacturers and institutions who benefit from switching-cost lock-in onto novice typists (who must learn a layout not chosen for their benefit) and onto alternative-layout inventors (whose technical improvements are foreclosed from ever reaching a market test at scale).
% ABSENT_VOICES: Dvorak and other alternative-layout advocates had no seat in the certification bodies or trade associations that controlled market access; ergonomics researchers documenting repetitive strain costs of QWERTY were not consulted by the manufacturers or schools whose revenue depended on the existing arrangement.
% DISAPPEARANCE_RATIONALE: If active maintenance (school curricula, certification metrics, manufacturer tooling commitments) disappeared overnight, this reading holds the market would gradually permit alternative layouts to compete on merit, and typing pedagogy would fragment — a genuine rearrangement. The naturalization reading disputes this, holding that QWERTY would persist anyway because it is adequate; the lock-in reading holds it would persist through pure coordination inertia even without anyone actively maintaining it. The three readings disagree about what would happen, which is exactly the kernel contest.
% FOUNDING_PROBLEM: In the 1870s, mechanical typewriters jammed when adjacent typebars were struck in rapid succession; QWERTY's arrangement separated common letter-pairs to slow typists down enough to reduce jams, and once adopted, gave manufacturers and schools a shared standard to coordinate around.
% FOUNDING_PROBLEM_CORROBORATION: Mechanical engineers and typewriter historians outside the manufacturing and certification trades (notably in Paul David's and Stephen Jay Gould's independent economic-history accounts) attest the jamming problem was solved by improved typebar mechanisms decades before electric and electronic keyboards made the original mechanical constraint entirely moot; the manufacturers and certification bodies who benefit from continued QWERTY dominance are not disinterested attesters and have not corroborated the problem's obsolescence.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, contested).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.2 at founding (when the arrangement was closer to genuine coordination around a real mechanical constraint) to 0.68 by the modern era, tracking the growing gap between the founding problem's obsolescence and the persistence of the training/certification apparatus that depends on it. Suppression is authored high (0.71) because this reading's structural claim is specifically that alternatives were kept out of the market by certification gatekeeping and manufacturer non-adoption, not merely out-competed. Theater ratio rises in parallel because an increasing share of the defense of QWERTY (efficiency claims, 'proven' framing) is not doing coordination work but justifying continued gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   From the manufacturer/school/certifier seats, QWERTY is a rope: a voluntary, mutually beneficial standard that lets typists and machines interoperate. From the novice-typist and inventor seats, the same arrangement computes as a tangled rope or worse: coordination function present, but access to alternatives is actively foreclosed by the very parties who profit from the standard's continuation. The engine should register this divergence structurally, not resolve it — that divergence is what the beneficiary-extraction reading is claiming exists.
 *
 * DIRECTIONALITY LOGIC:
 *   Manufacturers, typing schools, and certification bodies sit at the beneficiary end: they collect training revenue, certification fees, and locked-in hardware demand without bearing the switching costs they impose on others. Novice typists, alternative-layout inventors, and clerical entrants sit at the target end: they pay the sunk training cost, absorb the foreclosure of superior alternatives, and have no exit because the credentialing apparatus that gates employment is controlled by the beneficiaries. This is a textbook tangled-rope directionality split — the coordination function (a shared standard) is real, but the parties who administer it extract asymmetrically from the parties who must adopt it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical typebar jamming) is dead, corroborated by disinterested engineering historians, yet the arrangement's institutional infrastructure (certification, curricula, manufacturer tooling commitments) persists at full enforcement strength. This reading holds that mandatrophy is realized: the mandate (solve a mechanical problem) has expired, but the apparatus built to serve it has been repurposed by its administrators into continued extraction, which is exactly what distinguishes this reading from the lock-in reading's claim of inertia without an active maintaining agent, and from the naturalization reading's claim that no mandate ever needed resolving because QWERTY remained the adequate choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_maintenance_vs_passive_inertia,
    'Is QWERTY''s persistence better explained by active, ongoing suppression work performed by identifiable beneficiaries (this reading), or by passive coordination inertia with no one actively defending the standard (the lock-in reading)?',
    'Archival research into 19th/20th century manufacturer trade-association records, typing-school lobbying activity, and certification-body standard-setting minutes: evidence of coordinated action to exclude alternative layouts (e.g., refusal to certify Dvorak typists, tooling agreements) would support active maintenance; absence of such coordination, with persistence explained purely by uncoordinated individual switching-cost calculations, would support lock-in.',
    'If active maintenance is not evidenced, this reading''s tangled_rope classification collapses toward the lock-in reading''s structure (a rope with unfortunate emergent path-dependent costs, no identifiable extracting agent) rather than a tangled rope with named beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_maintenance_vs_passive_inertia, empirical, 'Whether beneficiaries actively suppressed alternatives or persistence was uncoordinated inertia.').

omega_variable(
    dvorak_technical_superiority_magnitude,
    'How large, in practice, was/is the Dvorak (or other alternative layout) speed and ergonomic advantage over QWERTY, and does that magnitude matter to this reading''s classification?',
    'Independent replication of typing-speed and repetitive-strain studies (the original Dvorak-funded studies are contested for conflict of interest; later independent studies show smaller, contested effect sizes).',
    'This reading''s extraction claim does not strictly require a large technical gap — active suppression of a marginal alternative is still extraction if it forecloses a real market test — but a vanishingly small or negative gap would strengthen the naturalization reading at this reading''s expense.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dvorak_technical_superiority_magnitude, empirical, 'Magnitude of alternative layouts'' technical advantage, and its bearing on the extraction claim.').

omega_variable(
    kernel_framing_selection,
    'Is the QWERTY persistence phenomenon best modeled as three competing readings of one kernel (as done here), or does the beneficiary-extraction claim actually subsume the lock-in claim (active maintenance being the mechanism BY WHICH lock-in occurs), making them not siblings but nested claims?',
    'Conceptual analysis: determine whether ''coordination failure'' (lock-in) and ''active beneficiary maintenance'' (this reading) describe the same causal mechanism at different levels of agency-attribution, or genuinely distinct causal claims that could each be true independently.',
    'If nested rather than sibling, the three-way kernel decomposition should be revised to two readings (naturalization vs. maintained-lock-in) rather than three; this would not change this story''s own ε or classification but would affect the network topology linking it to siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Whether the three-reading kernel decomposition is the correct carving of the underlying disagreement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(qwer_tr_t70, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 70, 0.38).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 100, 0.4).
narrative_ontology:measurement(qwer_tr_t140, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 140, 0.42).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(qwer_be_t70, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 70, 0.6).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 100, 0.65).
narrative_ontology:measurement(qwer_be_t140, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 140, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(qwer_su_t70, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 70, 0.68).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 100, 0.7).
narrative_ontology:measurement(qwer_su_t140, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 140, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.08).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the qwerty_persistence_mechanism kernel, each authored as a separate constraint story per the ε-invariance principle: naturalization_reading (near-zero ε, mountain/rope-adjacent, no beneficiaries), lock_in_reading (moderate ε from coordination-failure cost, rope or tangled_rope depending on switching-cost severity, no named suppressing agent), and this beneficiary_extraction_reading (ε=0.68, tangled_rope, named beneficiaries and active suppression). All three describe the same observable historical persistence of QWERTY but assign it structurally different causal mechanisms and different extraction profiles — they are not the same constraint viewed three ways, but three distinct structural claims sharing a label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
