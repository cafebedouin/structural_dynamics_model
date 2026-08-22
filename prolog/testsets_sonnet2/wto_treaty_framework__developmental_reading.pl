% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__developmental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__developmental_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: wto_treaty_framework__developmental_reading
 *   human_readable: WTO Treaty Framework Read as Developmental Compact (Policy Space, S&D, Technology Transfer)
 *   domain: international_trade_law/development_economics/political_economy
 *
 * SUMMARY:
 *   This story instantiates the developmental reading of the WTO treaty
 *   framework kernel: policy space for development, S&D provisions, and
 *   technology transfer obligations are treated as equal-status, permanent
 *   structural features of the treaty commitment rather than transitional
 *   concessions on the road to full liberalization. Under this reading, the
 *   framework functions as a tangled rope — it genuinely coordinates a
 *   multilateral system that accommodates asymmetric industrial starting
 *   points (real coordination function), while simultaneously extracting
 *   policy flexibility and technology rents from developed-economy IP holders
 *   and, over time, requiring newly-industrialized former beneficiaries to
 *   relinquish accommodations they no longer structurally need (asymmetric
 *   extraction running through the same S&D machinery). The rising
 *   theater_ratio and suppression_requirement trace the documented pattern of
 *   S&D provisions being progressively narrowed through dispute panel rulings
 *   even as their formal textual status as 'permanent' accommodation is
 *   defended rhetorically — the accommodation increasingly performs
 *   continuity while its substantive scope contracts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__developmental_reading, 0.38).
domain_priors:suppression_score(wto_treaty_framework__developmental_reading, 0.42).
domain_priors:theater_ratio(wto_treaty_framework__developmental_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__developmental_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__developmental_reading, "WTO Treaty Framework Read as Developmental Compact (Policy Space, S&D, Technology Transfer)").
narrative_ontology:topic_domain(wto_treaty_framework__developmental_reading, "international_trade_law/development_economics/political_economy").

domain_priors:requires_active_enforcement(wto_treaty_framework__developmental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__developmental_reading, '0720564b-d4fd-4de1-9dfb-1b16867a3bc3').
narrative_ontology:cs_kernel_codification('0720564b-d4fd-4de1-9dfb-1b16867a3bc3', formalized).
narrative_ontology:cs_authority_grounding('0720564b-d4fd-4de1-9dfb-1b16867a3bc3', extraction).
narrative_ontology:cs_interpretation_layer_present('0720564b-d4fd-4de1-9dfb-1b16867a3bc3').
narrative_ontology:cs_reading_relation('0720564b-d4fd-4de1-9dfb-1b16867a3bc3', wto_treaty_framework__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('0720564b-d4fd-4de1-9dfb-1b16867a3bc3', foundational, asymmetric_starting_conditions_require_structural_accommodation).
narrative_ontology:cs_axiom_status(asymmetric_starting_conditions_require_structural_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('0720564b-d4fd-4de1-9dfb-1b16867a3bc3', asymmetric_starting_conditions_require_structural_accommodation, empirically_contingent).
narrative_ontology:cs_axiom('0720564b-d4fd-4de1-9dfb-1b16867a3bc3', foundational, technology_transfer_is_core_treaty_obligation_not_optional_cooperation).
narrative_ontology:cs_axiom_status(technology_transfer_is_core_treaty_obligation_not_optional_cooperation, holdable).
narrative_ontology:cs_axiom_grounding('0720564b-d4fd-4de1-9dfb-1b16867a3bc3', technology_transfer_is_core_treaty_obligation_not_optional_cooperation, conventional).
narrative_ontology:cs_reference_frame('0720564b-d4fd-4de1-9dfb-1b16867a3bc3', special_and_differential_treatment_as_permanent_accommodation).
narrative_ontology:cs_drift_state('0720564b-d4fd-4de1-9dfb-1b16867a3bc3', post_doha_round_stalemate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0720564b-d4fd-4de1-9dfb-1b16867a3bc3', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__developmental_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, global_south_member_states).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, infant_industries_in_developing_economies).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, least_developed_countries).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, global_south_member_states).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, multinational_ip_holders).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, technology_licensing_firms).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, common_but_differentiated_treaty_obligation_doctrine).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, asymmetric_starting_conditions_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain tariff flexibility, subsidy space, and compulsory licensing authority under S&D provisions read as permanent structural accommodation rather than transitional exception. They gain policy room to nurture infant industries and access technology transfer commitments, but remain bound to the same dispute settlement machinery and enforcement infrastructure as fully liberalized members, and must still negotiate market access concessions to retain the accommodation politically.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, global_south_member_states, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__developmental_reading, global_south_member_states, payer).

% Structurally most dependent on S&D flexibility surviving as a permanent feature rather than a sunset clause; have the least capacity to independently develop industrial policy or negotiate bilateral technology transfer and rely almost entirely on the treaty's own accommodation architecture to create developmental room.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, least_developed_countries, beneficiary,
    powerless, generational, trapped, global).

% Domestic firms and sectors protected by tariff flexibility and subsidy latitude that would otherwise be foreclosed by symmetric liberalization obligations; their viability depends on the accommodation reading holding at the treaty's interpretive core rather than being read down as temporary.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, infant_industries_in_developing_economies, beneficiary,
    powerless, biographical, trapped, national).

% Hold patents and proprietary technology that technology transfer obligations require licensing or disclosing to developing-country firms and compulsory licensing regimes. They bear reduced rent extraction and diminished exclusivity where the developmental reading treats transfer as a core commitment rather than optional cooperation, and cannot exit the framework without losing access to the markets it governs.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, multinational_ip_holders, payer,
    powerful, biographical, constrained, global).

% Compete for licensing revenue in developing markets and find compulsory licensing authority and transfer obligations compress their pricing power; can partially route around exposure by relocating production or shifting IP-heavy activity to jurisdictions with weaker enforcement of the developmental reading.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, technology_licensing_firms, payer,
    powerful, biographical, mobile, global).

% Negotiate and periodically re-litigate the scope of S&D provisions at ministerial rounds; can tighten or loosen the developmental reading's practical force by controlling accession terms, dispute panel composition, and enforcement priorities, while retaining full market access rights themselves under the parallel liberalization track.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, developed_economy_governments, agenda_setter,
    institutional, generational, arbitrage, global).

% Administer and adjudicate disputes over what counts as permissible policy space versus prohibited subsidy or protectionism; their interpretive rulings determine in practice whether S&D functions as durable accommodation or narrows toward the market-access reading over successive cases.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, wto_secretariat_and_dispute_bodies, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__developmental_reading, wto_secretariat_and_dispute_bodies, observer).

% Compete against subsidized or tariff-protected developing-country producers and against domestic firms that must now transfer technology to foreign licensees; largely absent from the treaty text's own framing of S&D as a developmental accommodation and voice objections instead through domestic trade politics rather than within the treaty's interpretive apparatus.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, domestic_industries_in_developed_economies, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a multilateral trading system in which states with vastly different industrial starting points can commit to shared rules without the weaker parties being forced into immediate symmetric liberalization that would foreclose their own industrialization path.
% TRANSFER_FUNCTION: Moves policy latitude (tariff flexibility, subsidy authority, compulsory licensing power) and technology (via transfer obligations) from established industrial and IP-holding states and firms toward developing and least-developed member states and their infant industries.
% ABSENT_VOICES: Domestic industries and labor constituencies in developed economies who bear competitive pressure from protected developing-country producers are not party to the S&D negotiating table; their objections surface in national trade politics rather than in the treaty's own accommodation logic.
% DISAPPEARANCE_RATIONALE: If the developmental reading collapsed and S&D provisions were read as merely transitional, developing states would lose treaty-protected tariff and subsidy latitude, compulsory licensing authority would face heightened challenge, and technology transfer would revert to voluntary cooperation — industrial policy space for the Global South would contract sharply and infant industries would face accelerated exposure to full liberalization.
% FOUNDING_PROBLEM: Post-colonial and newly industrializing states entered a multilateral trading system built substantially around already-industrialized economies' interests; without recognized asymmetry, uniform obligations would lock in the existing division of industrial capacity rather than allow catch-up development.
% FOUNDING_PROBLEM_CORROBORATION: UNCTAD analyses and G77 negotiating positions attest the asymmetry problem remains live and that S&D erosion through dispute panel narrowing is an ongoing structural threat; developed-economy trade ministries and some WTO panel rulings attest the founding problem has substantially resolved as many S&D beneficiaries industrialized, framing continued accommodation as no longer justified by original conditions — corroboration is split between institutions outside the direct beneficiary states (UNCTAD, independent development economists) and institutions aligned with the payer states (developed-economy governments, industry associations), with no fully disinterested third-party consensus.
narrative_ontology:disappearance_verdict(wto_treaty_framework__developmental_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__developmental_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__developmental_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_treaty_framework__developmental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__developmental_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__developmental_reading_tests).
:- end_tests(wto_treaty_framework__developmental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) reflecting the developmental reading's own framing: this is not primarily an extraction mechanism but a coordination structure with a genuine extraction component running through technology transfer and compulsory licensing that transfers real value from IP holders to developing-country industries. Suppression (0.42) reflects the treaty's active enforcement apparatus (dispute settlement, retaliation authority) which both protects and increasingly constrains the S&D accommodation. Theater ratio has risen to 0.4, tracking the gap between formal S&D permanence and its practical erosion through graduation clauses and panel interpretation — the accommodation is increasingly asserted rhetorically while narrowing in operation. Accessibility collapse is moderate-low (0.35): alternatives to WTO membership exist (regional agreements, bilateral deals) but carry substantial costs, and resistance (0.55) is real and organized, coming from both developing states defending S&D scope and developed-economy industries contesting it.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South member states and least developed countries sit as primary beneficiaries under this reading — the accommodation subsidizes their industrial policy space. But they also appear as partial payers: they remain bound to the treaty's broader enforcement and market-opening machinery, and successful developing states face graduation pressure that converts prior beneficiary status into payer status over time. Multinational IP holders and technology licensing firms are structural targets — the technology transfer and compulsory licensing obligations extract value from their exclusivity. Developed-economy governments retain agenda-setting power over how tightly or loosely S&D is interpreted, giving them durable influence over the accommodation's actual scope despite not being named beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-colonial industrial asymmetry) is contested rather than resolved: for many LDCs it remains fully live, while for graduated middle-income states the original justification has partly dissolved even as the treaty's formal accommodation language persists unchanged — a mandatrophy candidate specifically for the subset of states whose industrial capacity has converged with the historical beneficiaries the provisions were built for. Classifying this as tangled_rope rather than snare prevents mislabeling the entire framework as pure extraction: the coordination function (enabling asymmetric-start states to participate in multilateral trade without immediate full liberalization) is real and documented, even as the enforcement and graduation machinery increasingly extracts concessions from the same states it once protected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sd_permanence_vs_transition_framing,
    'Does the treaty text and its negotiating history support reading S&D provisions as a permanent structural feature of equal-status treaty commitment, or as a temporary transitional mechanism intended to expire as developing states industrialize?',
    'Comparative analysis of GATT/WTO founding negotiating records, the explicit graduation clauses in specific S&D provisions versus open-ended ones, and the pattern of dispute panel rulings narrowing or affirming S&D scope over three decades.',
    'If the transitional framing is textually and historically dominant, this developmental reading''s claimed permanence is itself contested at the kernel level, and the constraint''s own beneficiary structure would need re-evaluation as time-limited rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sd_permanence_vs_transition_framing, conceptual, 'Whether the kernel''s own text supports permanent or transitional S&D framing — the central interpretive fork between readings.').

omega_variable(
    graduation_beneficiary_to_payer_transition,
    'At what point does a developing member state''s industrial convergence with developed economies convert it from a genuine beneficiary of S&D accommodation into an agent extracting continued advantage from a framework whose original justification for that state no longer holds?',
    'Track GNI per capita, technological capability indices, and export sophistication for graduated or graduating WTO members against their continued invocation of special and differential treatment provisions in disputes.',
    'If graduation is empirically clear-cut for a subset of states, the tangled_rope classification''s victim/beneficiary overlap (global_south_member_states appearing as both) is validated as time-varying rather than internally contradictory; if graduation criteria remain contested, the ambiguity is irreducible and supports treating S&D scope disputes as ongoing kernel-level contest rather than resolved fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduation_beneficiary_to_payer_transition, empirical, 'Whether industrial convergence empirically converts specific S&D beneficiaries into net payers within the same accommodation structure.').

omega_variable(
    technology_transfer_enforcement_gap,
    'Is the technology transfer obligation under this developmental reading actually enforced with comparable rigor to market-access and IP-protection obligations, or does it function primarily as aspirational treaty language with weak compliance mechanisms?',
    'Compare the dispute settlement caseload and remedy outcomes for technology transfer non-compliance claims against caseload and outcomes for IP infringement and market access claims over the same period.',
    'A large enforcement asymmetry would indicate the developmental reading''s core commitment is structurally weaker in practice than its market-access counterpart, supporting a higher theater_ratio and lower effective extraction from IP holders than the authored metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_enforcement_gap, empirical, 'Whether technology transfer commitments are enforced as rigorously as market-access and IP obligations, or persist mainly as declaratory text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__developmental_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_treaty_framework__developmental_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(wto__tr_t2001, wto_treaty_framework__developmental_reading, theater_ratio, 2001, 0.27).
narrative_ontology:measurement(wto__tr_t2007, wto_treaty_framework__developmental_reading, theater_ratio, 2007, 0.32).
narrative_ontology:measurement(wto__tr_t2013, wto_treaty_framework__developmental_reading, theater_ratio, 2013, 0.36).
narrative_ontology:measurement(wto__tr_t2019, wto_treaty_framework__developmental_reading, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(wto__tr_t2025, wto_treaty_framework__developmental_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_treaty_framework__developmental_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(wto__be_t2001, wto_treaty_framework__developmental_reading, base_extractiveness, 2001, 0.3).
narrative_ontology:measurement(wto__be_t2007, wto_treaty_framework__developmental_reading, base_extractiveness, 2007, 0.33).
narrative_ontology:measurement(wto__be_t2013, wto_treaty_framework__developmental_reading, base_extractiveness, 2013, 0.35).
narrative_ontology:measurement(wto__be_t2019, wto_treaty_framework__developmental_reading, base_extractiveness, 2019, 0.37).
narrative_ontology:measurement(wto__be_t2025, wto_treaty_framework__developmental_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_treaty_framework__developmental_reading, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement(wto__su_t2001, wto_treaty_framework__developmental_reading, suppression_requirement, 2001, 0.34).
narrative_ontology:measurement(wto__su_t2007, wto_treaty_framework__developmental_reading, suppression_requirement, 2007, 0.37).
narrative_ontology:measurement(wto__su_t2013, wto_treaty_framework__developmental_reading, suppression_requirement, 2013, 0.4).
narrative_ontology:measurement(wto__su_t2019, wto_treaty_framework__developmental_reading, suppression_requirement, 2019, 0.41).
narrative_ontology:measurement(wto__su_t2025, wto_treaty_framework__developmental_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, market_access_reading).

% DUAL FORMULATION NOTE:
% This story and market_access_reading are two readings of the same wto_treaty_framework kernel. Both share the same treaty text but author structurally distinct ε and classification: this developmental reading treats S&D as permanent structural accommodation (moderate ε, tangled_rope, Global South states as primary named beneficiaries with IP holders as targets); the market_access_reading treats liberalization and non-discrimination as the primary obligation and S&D as transitional exception (expected higher ε for developing-state compliance burden, different beneficiary/victim assignment). The two files are linked via affects_constraints in both directions and must not be merged or averaged — per the ε-invariance principle, differing ε values across observables mean two constraints, not one with a parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
