% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__rent_seeking_suppression, []).

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
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Occupational Licensing Statute as Incumbent Rent Extraction
 *   domain: labor_economics/regulatory_policy
 *
 * SUMMARY:
 *   A state licensing board, captured by incumbent-dominated composition and
 *   sustained by trade association lobbying, sets entry requirements (hours
 *   of training, examination content, continuing education, reciprocity
 *   denial) that track incumbent interests far more closely than documented
 *   harm rates. Entrants pay in foreclosed wages and sunk training cost;
 *   consumers pay in price premiums; incumbents and the board capture the
 *   resulting rents. The claimed type (snare) and the authored metrics are
 *   independently asserted: extraction and suppression are both substantial
 *   and rising, consistent with an entrenching extraction mechanism rather
 *   than a stable coordination function.
 *
 * KEY AGENTS:
 *   - incumbent_licensed_practitioners: primary beneficiary (organized/arbitrage) — captures wage premium and board control
 *   - professional_licensing_boards: agenda_setter (institutional/arbitrage) — sets and enforces entry barriers, captured by incumbents
 *   - trade_association_lobbies: agenda_setter/beneficiary (organized/arbitrage) — drafts model legislation, defends statute
 *   - prospective_entrants: primary target (powerless/trapped) — bears foreclosed wages and training cost
 *   - consumers_paying_higher_prices: secondary target (powerless/constrained) — bears price premium
 *   - cross_state_migrant_workers: secondary target (powerless/trapped) — barrier re-imposed at jurisdictional lines
 *   - legislators: excluded from meaningful revision (institutional/analytical) — could act but face concentrated-vs-diffuse incentive asymmetry
 *   - labor_economists: analytical observer (analytical/analytical) — documents effect without power to change it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.78).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.72).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.78).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Occupational Licensing Statute as Incumbent Rent Extraction").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "labor_economics/regulatory_policy").

domain_priors:requires_active_enforcement(licensing_statute_mandate__rent_seeking_suppression).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, '9368cb58-57f4-421b-9562-8ef16f5534d4').
narrative_ontology:cs_kernel_codification('9368cb58-57f4-421b-9562-8ef16f5534d4', formalized).
narrative_ontology:cs_authority_grounding('9368cb58-57f4-421b-9562-8ef16f5534d4', extraction).
narrative_ontology:cs_interpretation_layer_present('9368cb58-57f4-421b-9562-8ef16f5534d4').
narrative_ontology:cs_reading_relation('9368cb58-57f4-421b-9562-8ef16f5534d4', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('9368cb58-57f4-421b-9562-8ef16f5534d4', licensing_statute_mandate__graduated_access_filter, influences).
narrative_ontology:cs_axiom('9368cb58-57f4-421b-9562-8ef16f5534d4', foundational, incumbent_control_of_entry_terms_is_illegitimate_self_dealing).
narrative_ontology:cs_axiom_status(incumbent_control_of_entry_terms_is_illegitimate_self_dealing, holdable).
narrative_ontology:cs_axiom_grounding('9368cb58-57f4-421b-9562-8ef16f5534d4', incumbent_control_of_entry_terms_is_illegitimate_self_dealing, empirically_contingent).
narrative_ontology:cs_axiom('9368cb58-57f4-421b-9562-8ef16f5534d4', secondary, requirement_stringency_must_track_harm_evidence_not_lobbying_capacity).
narrative_ontology:cs_axiom_status(requirement_stringency_must_track_harm_evidence_not_lobbying_capacity, holdable).
narrative_ontology:cs_axiom_grounding('9368cb58-57f4-421b-9562-8ef16f5534d4', requirement_stringency_must_track_harm_evidence_not_lobbying_capacity, instrumental).
narrative_ontology:cs_reference_frame('9368cb58-57f4-421b-9562-8ef16f5534d4', consumer_protection_founding_rationale).
narrative_ontology:cs_drift_state('9368cb58-57f4-421b-9562-8ef16f5534d4', contemporary_licensing_proliferation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9368cb58-57f4-421b-9562-8ef16f5534d4', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensed_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, professional_licensing_boards).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, trade_association_lobbies).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, prospective_entrants).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, consumers_paying_higher_prices).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, cross_state_migrant_workers).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__rent_seeking_suppression, occupational_self_regulation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Already hold the license and face no new compliance burden; benefit directly from reduced competition and the wage premium that scarcity produces. Sit on or fund the board that sets entry requirements, giving them ongoing control over the barrier's height.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensed_practitioners, beneficiary,
    organized, biographical, arbitrage, regional).

% Composed predominantly of incumbent practitioners, sets examination content, continuing-education mandates, and reciprocity rules. Justifies each requirement as protecting the public, but requirements consistently track what raises entry cost rather than what predicts harm reduction. Funded by licensing fees paid by the very entrants it screens.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, professional_licensing_boards, agenda_setter,
    institutional, generational, arbitrage, regional).

% Lobbies statehouses to expand scope-of-practice restrictions and resist reciprocity agreements that would let out-of-state licensees compete locally. Drafts model legislation later adopted verbatim by state boards, then represents its members' interests in defending the resulting statute.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, trade_association_lobbies, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__rent_seeking_suppression, trade_association_lobbies, beneficiary).

% Must complete lengthy, costly training and examination requirements often unrelated to the tasks actually performed, or exit the field entirely. Cannot practice, even briefly or under supervision, without the credential; migrating to another jurisdiction usually means repeating requirements from scratch.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, prospective_entrants, payer,
    powerless, biographical, trapped, regional).

% Pay higher prices for licensed services because supply is artificially restricted; have no visibility into whether the price premium buys any corresponding safety improvement. Can sometimes substitute unlicensed or DIY alternatives, but for many services the statute makes unlicensed provision illegal, closing that option too.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumers_paying_higher_prices, payer,
    powerless, immediate, constrained, regional).

% Hold a valid license in one state but face non-recognition or costly re-certification when relocating, effectively re-imposing the entry barrier at every jurisdictional line despite having already demonstrated competence.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, cross_state_migrant_workers, payer,
    powerless, biographical, trapped, national).

% Nominally hold authority to revise or repeal licensing statutes but rarely do, given concentrated incumbent lobbying versus diffuse consumer interest; genuinely could reweigh the tradeoff but the political incentive structure keeps the question off the floor.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, legislators, excluded,
    institutional, biographical, analytical, regional).

% Study wage premiums, price effects, and entry-rate data across licensed and unlicensed occupations; publish findings that consistently show licensing raises prices with little measurable quality improvement in many occupations, but their findings do not translate into statutory change.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, labor_economists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensed_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__rent_seeking_suppression, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, licensing coordinates a signal of minimum competence so consumers need not individually verify every practitioner's skill before transacting.
% TRANSFER_FUNCTION: Moves income from prospective entrants (foreclosed wages, sunk training costs) and consumers (price premiums) to incumbent practitioners (wage premium) and licensing boards (fee revenue), via a statutory barrier to entry that exceeds what competence verification alone would require.
% ABSENT_VOICES: Prospective entrants who have not yet entered the field have no seat on the boards that set the barriers they will face; unlicensed practitioners who could offer lower-cost service are legally barred from testifying as market participants, only as petitioners for exemption.
% DISAPPEARANCE_RATIONALE: If the statute vanished overnight, entry would rise, incumbent wage premiums would compress toward competitive levels, and consumer prices would fall in the affected occupation — the licensing board itself would lose its funding base and likely dissolve. Some minimum-competence signaling would likely reconstitute voluntarily (certification, insurance requirements) but at a fraction of the current barrier height.
% FOUNDING_PROBLEM: Originally proposed to address genuine information asymmetries where consumers could not assess practitioner competence before suffering harm from incompetent service.
% FOUNDING_PROBLEM_CORROBORATION: Licensing boards and incumbent associations attest the safety problem remains live and justifies current requirements. Labor economists and legislative auditors outside the beneficiary set attest that requirement stringency (hours, fees, exam content) correlates with incumbent lobbying intensity rather than with documented harm rates, and that many requirements (cosmetology hour minimums, geographic reciprocity denial) have no plausible safety rationale — supporting the reading that the founding problem has been substantially decoupled from the current statute's actual function.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__rent_seeking_suppression, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__rent_seeking_suppression, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__rent_seeking_suppression, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__rent_seeking_suppression, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__rent_seeking_suppression, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.78) is high because the entry barrier's stringency has been shown, in the empirical record for many licensed occupations, to track incumbent political influence rather than harm-reduction evidence — the wage premium and price premium are the measured transfer. Suppression (0.72) is high because exit for entrants is not merely inconvenient but statutorily foreclosed: practicing without the credential is often a criminal or civil violation, not just a market disadvantage. Theater ratio (0.55) reflects that continuing-education and re-certification requirements increasingly serve to maintain the appearance of ongoing competence verification while functioning mainly as recurring fee extraction and barrier maintenance — this rises over the measured interval as the coordination rationale increasingly serves as cover rather than function. All three time series share one grid (0/8/16/24/32/40) as required.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent/board seat, the statute reads as legitimate professional self-governance protecting consumers and the profession's reputation. From the entrant/consumer seat, the same statute reads as an entry toll with a safety veneer. The engine computes these as structurally different seat classifications from the same base data — the divergence is the point of the classification, not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbents and the board sit at the beneficiary end of directionality: they collect fees and wage premiums and control the mechanism that produces them, with arbitrage-grade exit (they can relocate their practice or shift board composition without losing the rent). Entrants and consumers sit at the target end: trapped or constrained exit, no control over requirement-setting, and the extraction lands on them structurally. Legislators are excluded rather than positioned as beneficiary or victim — they hold formal authority to change the arrangement but structurally do not exercise it, which is a distinct situation from either capturing or bearing the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genuine information asymmetry about practitioner competence) may have been real at statute inception, but the corroboration record shows it has substantially decoupled from the current requirement structure: requirement stringency tracks lobbying intensity, not harm data, and reciprocity denial (which has zero plausible safety rationale — a competent practitioner does not become incompetent by crossing a state line) persists specifically because it protects local incumbents from out-of-state competition. This is the signature of mandatrophy: the mandate (protect consumers) has been substantially retained as language while the operative function (protect incumbents) has taken over the machinery originally built for the mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Given the same statutory text, is the primary structural function rent extraction (this reading), genuine safety coordination (public_safety_coordination reading), or class-based access sorting (graduated_access_filter reading)?',
    'Compare requirement stringency against documented harm-rate data by occupation, and against the demographic/class composition of entrants versus incumbents; occupations where stringency has zero correlation with harm data and strong correlation with incumbent lobbying spend support this reading over public_safety_coordination; occupations where entry barriers disproportionately track prior resource access (unpaid apprenticeship hours, licensing exam fees relative to income) support graduated_access_filter as an additional or alternative structural layer.',
    'If public_safety_coordination dominates for a given occupation''s actual data, this reading''s ε is overstated for that occupation and the statute should be re-authored as a separate, less extractive constraint. If graduated_access_filter dominates, the primary victim axis is class rather than incumbent-vs-entrant, which changes the beneficiary/victim structure entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Which of the three kernel readings best fits the empirical operation of a given occupation''s licensing statute.').

omega_variable(
    board_capture_degree,
    'To what degree is board composition genuinely captured by incumbent interest versus genuinely representing broader public-interest appointees?',
    'Audit board member professional affiliation, appointment process, and voting record correlation with incumbent-favorable versus consumer-favorable rule changes over the statute''s history.',
    'High capture supports the snare classification robustly; if boards include substantial independent or consumer-representative membership with demonstrated influence on outcomes, the coordination function may be more genuine than this reading assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(board_capture_degree, empirical, 'Whether licensing board composition is substantively captured by incumbent interest.').

omega_variable(
    reciprocity_denial_rationale,
    'Is there any occupation-specific safety rationale for denying reciprocity to out-of-state licensees, or is reciprocity denial purely a competitive barrier?',
    'Compare harm rates for cross-state-licensed practitioners operating under reciprocity agreements against locally-licensed practitioners in occupations that do versus do not have reciprocity.',
    'Absence of any measurable harm differential would strongly corroborate this reading''s characterization of reciprocity denial as pure rent protection with zero safety content.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reciprocity_denial_rationale, empirical, 'Whether reciprocity denial has any safety justification beyond incumbent protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0, 0.25).
narrative_ontology:measurement(lice_tr_t8, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 8, 0.32).
narrative_ontology:measurement(lice_tr_t16, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 16, 0.38).
narrative_ontology:measurement(lice_tr_t24, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 24, 0.44).
narrative_ontology:measurement(lice_tr_t32, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 32, 0.5).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lice_be_t8, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(lice_be_t16, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(lice_be_t24, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(lice_be_t32, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(lice_su_t8, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(lice_su_t16, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(lice_su_t24, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(lice_su_t32, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__rent_seeking_suppression, enforcement_mechanism).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__graduated_access_filter).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the licensing_statute_mandate kernel, sharing the same statutory text but instantiating structurally distinct constraints per the ε-invariance principle: rent_seeking_suppression (this story, snare, ε=0.78 — incumbents as beneficiary, entrants/consumers as victim), public_safety_coordination (mountain-or-rope framing, negligible extraction — genuine competence-signaling), and graduated_access_filter (tangled_rope framing — class-stratified access as the primary transfer axis, distinct victim set from this story). The three do not average into one ε; each is authored independently with its own beneficiary/victim structure and linked here for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
