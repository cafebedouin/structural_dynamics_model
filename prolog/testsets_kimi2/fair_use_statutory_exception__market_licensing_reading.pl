% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__market_licensing_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use Statutory Exception â Market Licensing Reading
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   This constraint story models the 'market licensing' reading of the U.S.
 *   fair use statutory exception: the interpretive stance that any use for
 *   which a license could be obtained is presumptively unfair because it
 *   harms the potential market for the copyrighted work. Under this reading,
 *   fair use collapses to de minimis or genuinely unmonetizable fringe uses;
 *   the statutory safety valve becomes null in practice. The kernel is the
 *   fair use doctrine itself (17 U.S.C. Â§ 107); this is one of three
 *   contested readings. Key agents include the content industry and licensing
 *   collectives (beneficiaries), transformative creators, educators,
 *   documentary filmmakers, and remix artists (targets), and excluded
 *   public-domain advocates. The story is authored as a kernel reading with
 *   extremely high extractiveness because the reading's structural effect is
 *   to eliminate the doctrine's protective function wherever markets can be
 *   imagined.
 *
 * KEY AGENTS:
 *   - rights_holder_industry: Primary beneficiary (institutional/arbitrage) â collects licensing rents expanded by the reading
 *   - licensing_collectives: Secondary beneficiary (organized/mobile) â intermediates and administers the expanded licensing base
 *   - transformative_creators: Primary target (moderate/constrained) â bears extraction through license fees or litigation risk
 *   - educational_users: Target (organized/constrained) â pays for previously exempt instructional uses
 *   - documentary_filmmakers: Target (moderate/constrained) â faces clearance costs that alter the historical record
 *   - remix_creators: Target (powerless/constrained) â cannot practically license the multiplicity of sources
 *   - public_domain_advocates: Excluded voice (moderate/analytical) â structurally absent from doctrinal formation
 *   - fair_use_scholars: Analytical observer (analytical/analytical) â documents the divergence without market stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.92).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.88).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use Statutory Exception â Market Licensing Reading").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, '84dace72-acb4-4bef-a7a7-aa94fd2e3632').
narrative_ontology:cs_kernel_codification('84dace72-acb4-4bef-a7a7-aa94fd2e3632', formalized).
narrative_ontology:cs_authority_grounding('84dace72-acb4-4bef-a7a7-aa94fd2e3632', lineage).
narrative_ontology:cs_interpretation_layer_present('84dace72-acb4-4bef-a7a7-aa94fd2e3632').
narrative_ontology:cs_reading_relation('84dace72-acb4-4bef-a7a7-aa94fd2e3632', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('84dace72-acb4-4bef-a7a7-aa94fd2e3632', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_axiom('84dace72-acb4-4bef-a7a7-aa94fd2e3632', foundational, licensable_use_presumptively_unfair).
narrative_ontology:cs_axiom_status(licensable_use_presumptively_unfair, holdable).
narrative_ontology:cs_axiom_grounding('84dace72-acb4-4bef-a7a7-aa94fd2e3632', licensable_use_presumptively_unfair, conventional).
narrative_ontology:cs_axiom('84dace72-acb4-4bef-a7a7-aa94fd2e3632', foundational, market_failure_prerequisite_for_fair_use).
narrative_ontology:cs_axiom_status(market_failure_prerequisite_for_fair_use, holdable).
narrative_ontology:cs_axiom_grounding('84dace72-acb4-4bef-a7a7-aa94fd2e3632', market_failure_prerequisite_for_fair_use, conventional).
narrative_ontology:cs_reference_frame('84dace72-acb4-4bef-a7a7-aa94fd2e3632', market_clearing_statutory_framework).
narrative_ontology:cs_drift_state('84dace72-acb4-4bef-a7a7-aa94fd2e3632', contemporary_digital_reproduction_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('84dace72-acb4-4bef-a7a7-aa94fd2e3632', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, rights_holder_industry).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, licensing_collectives).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, transformative_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, educational_users).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, remix_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collects licensing revenue from an ever-expanding set of uses by arguing that any potential market for a use should be preserved exclusively for rights holders. Benefits from statutory damages and automatic injunctions that enforce this monopoly.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, rights_holder_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Administers blanket and transactional licenses, expanding their portfolio as the definition of licensable uses grows. Their business model depends on the elimination of uncompensated uses.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, licensing_collectives, beneficiary,
    organized, generational, mobile, global).

% Create new works that borrow from existing culture but must either pay for licenses or risk litigation. Under this reading, transformative purpose does not shield them if a licensing market can be imagined.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, transformative_creators, payer,
    moderate, biographical, constrained, national).

% Schools and universities that rely on fair use for course packs, digital reserves, and classroom display face demands for licensing fees for uses previously treated as exempt.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, educational_users, payer,
    organized, biographical, constrained, national).

% Must clear incidental background music, archival footage, and quoted media, often facing exorbitant fees or refusals that force alteration of the historical record.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, documentary_filmmakers, payer,
    moderate, biographical, constrained, national).

% Produce derivative works combining existing media; under a market-licensing regime, each source requires a license, making most remixes legally untenable regardless of transformative value.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, remix_creators, payer,
    powerless, biographical, constrained, national).

% Argue for robust public rights in culture and against enclosure, but are structurally excluded from the forums where this reading is advanced and adopted.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, public_domain_advocates, excluded,
    moderate, generational, analytical, global).

% Analyze and critique the doctrinal drift, documenting the divergence between statutory text and judicial interpretation without direct stake in the market outcomes.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, fair_use_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__market_licensing_reading, rights_holder_industry).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__market_licensing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Directs all potential uses of copyrighted works into a unified market-clearing mechanism, offering rights holders predictable revenue streams and reducing uncertainty about whether a given use requires permission.
% TRANSFER_FUNCTION: Moves the economic value of secondary and transformative uses from creators, educators, and remix artists to rights holders and licensing intermediaries by eliminating the statutory exemption where any market can be posited.
% ABSENT_VOICES: Remix communities, amateur creators, and educational practitioners who would be licensed out of existence are not represented in the litigation and lobbying forums where this reading is advanced; their objections surface only when they are already defendants.
% DISAPPEARANCE_RATIONALE: If this interpretive constraint vanished, fair use would expand to cover transformative and educational uses regardless of licensing availability, licensing markets would face competition from zero-cost lawful uses, and the current clearance culture would recede.
% FOUNDING_PROBLEM: The Copyright Act of 1976 embedded fair use to prevent market failure from over-reaching exclusivity, ensuring that education, criticism, and transformative culture could proceed without permission when social benefit exceeded private harm.
% FOUNDING_PROBLEM_CORROBORATION: Congressional record and legislative history from 1976 attest the balancing purpose. Post-hoc content-industry arguments claim markets now prevent the original problem, but independent legal historians and public-interest scholars confirm the founding purpose was to preserve non-market breathing room; no outside corroboration supports the claim that market completeness has eliminated the need for the safety valve.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.92, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__market_licensing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__market_licensing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.92) because the reading eliminates fair use for any use with a potential market, which in a digital economy means nearly all uses. Suppression is very high (0.88) due to statutory damages, preliminary injunctions, automated takedown systems, and the chilling effect of litigation risk. Theater ratio is moderate-high (0.45): courts still perform the four-factor statutory analysis, but the market-harm factor is outcome-determinative, making much of the balancing ritual performative. Accessibility collapse (0.75) reflects that once a user understands the reading, alternatives collapse to licensing or abstention. Resistance (0.70) is substantial but losing ground as clearance culture normalizes. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The rights-holder industry experiences this reading as legitimate property protection and market coordination; transformative creators and educators experience it as the elimination of their statutory breathing room. The engine computes this divergence from the structural data: identical legal text produces opposite directionalities depending on whether the agent collects rents or pays them.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights holders and licensing collectives are structural beneficiaries (d near 0.0): the constraint subsidizes their control by expanding the licensing monopoly to all conceivable uses. Transformative creators, educators, documentary filmmakers, and remix artists are structural targets (d near 1.0): they bear the extraction through license fees, abstention, or litigation risk. Public-domain advocates are excluded (no directional role in the active constraint). The scholar/observer seat sees the divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preserving non-market spaces for socially valuable uses â is dead under this reading, but the arrangement persists because it serves a different function (rent extraction). The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges flags this as a potential zombie constraint. However, because the coordination function (predictable licensing markets) is real, the classification remains Tangled Rope rather than Piton or Snare: the constraint is actively maintained for extraction but still coordinates a market.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_completeness_assumption,
    'Is there actually a functioning, low-transaction-cost licensing market for every secondary use that this reading presumes licensable?',
    'Empirical survey of licensing availability, transaction costs, and response times for documentary, educational, and remix uses.',
    'If markets are incomplete, the reading extracts from users for uses that cannot practically be licensed, converting a coordination mechanism into pure rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_completeness_assumption, empirical, 'Whether licensing markets are complete enough to support the reading''s premise').

omega_variable(
    statutory_text_contradiction,
    'Does the market-licensing reading contradict the statutory text of 17 U.S.C. Â§ 107, which lists specific purposes independent of market availability?',
    'Textual analysis of ''purpose and character,'' ''nature,'' ''amount,'' and ''effect'' factors against the market-only interpretation.',
    'If the text is irreconcilable with the reading, the constraint is an extraction mechanism operating against the kernel''s formal content, suggesting a stronger extraction classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_text_contradiction, conceptual, 'Tension between market-only reading and statutory language').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of unauthorized uses structural (statutory damages, injunctions) or internalized (clearance culture, self-censorship)?',
    'Comparative study of creative production before and after exposure to copyright risk; post-litigation behavior of documentary filmmakers and remix creators.',
    'If internalized suppression dominates, effective extraction exceeds the structural measure â the constraint operates through fear and identity-lock, not just legal enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in creative practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_market_tr_t0, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fair_use_market_tr_t8, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(fair_use_market_tr_t16, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(fair_use_market_tr_t24, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(fair_use_market_tr_t32, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(fair_use_market_tr_t40, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(fair_use_market_be_t0, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(fair_use_market_be_t8, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(fair_use_market_be_t16, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(fair_use_market_be_t24, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 24, 0.8).
narrative_ontology:measurement(fair_use_market_be_t32, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 32, 0.87).
narrative_ontology:measurement(fair_use_market_be_t40, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 40, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_market_su_t0, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fair_use_market_su_t8, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(fair_use_market_su_t16, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(fair_use_market_su_t24, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(fair_use_market_su_t32, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 32, 0.82).
narrative_ontology:measurement(fair_use_market_su_t40, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
