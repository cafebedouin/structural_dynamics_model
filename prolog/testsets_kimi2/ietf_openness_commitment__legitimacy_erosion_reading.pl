% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__legitimacy_erosion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__legitimacy_erosion_reading, []).

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
 *   constraint_id: ietf_openness_commitment__legitimacy_erosion_reading
 *   human_readable: IETF Rough Consensus Legitimacy Erosion
 *   domain: technology governance / internet standards / institutional economics
 *
 * SUMMARY:
 *   The IETF's rough consensus mechanism is a foundational governance
 *   technology for Internet standards, relying on open participation,
 *   volunteer deliberation, and chair-mediated consensus rather than formal
 *   voting. This reading treats the mechanism itself as contested: despite
 *   procedural safeguards (open mailing lists, transparent minutes, chair
 *   neutrality), well-resourced corporate actors have captured the
 *   consensus-formation process, extracting procedural legitimacy to ratify
 *   standards that encode their commercial and architectural interests. The
 *   extraction target is not merely a specific standard but the credibility
 *   of the consensus mechanism itselfâa commons that is depleted when
 *   ratified outcomes systematically track resource advantage rather than
 *   technical merit. This constraint story decomposes the 'IETF openness
 *   commitment' kernel into the legitimacy-erosion reading, distinct from
 *   commons-stewardship and capture-substrate siblings.
 *
 * KEY AGENTS:
 *   - ietf_administration: agenda_setter (institutional/constrained) â administers the rough consensus process and procedural safeguards
 *   - dominant_platform_vendors: primary beneficiary (powerful/constrained) â extract procedural legitimacy to ratify self-serving technical outcomes
 *   - unaffiliated_technical_contributors: primary payer (moderate/constrained) â bear the cost of diminishing influence and legitimacy erosion
 *   - small_independent_implementers: secondary payer (moderate/constrained) â absorb standards that encode incumbent advantage
 *   - public_interest_advocates: excluded (organized/trapped) â would object to capture dynamics but lack resources for sustained working group participation
 *   - internet_governance_researchers: observer (analytical/analytical) â sees the structural gap between procedural form and captured outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.72).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.68).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Rough Consensus Legitimacy Erosion").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "technology governance / internet standards / institutional economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, 'c296e3be-5e9d-4ccd-8297-37871772352c').
narrative_ontology:cs_kernel_codification('c296e3be-5e9d-4ccd-8297-37871772352c', formalized).
narrative_ontology:cs_authority_grounding('c296e3be-5e9d-4ccd-8297-37871772352c', practice).
narrative_ontology:cs_interpretation_layer_present('c296e3be-5e9d-4ccd-8297-37871772352c').
narrative_ontology:cs_reading_relation('c296e3be-5e9d-4ccd-8297-37871772352c', ietf_openness_commitment__commons_stewardship_reading, influences).
narrative_ontology:cs_reading_relation('c296e3be-5e9d-4ccd-8297-37871772352c', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_axiom('c296e3be-5e9d-4ccd-8297-37871772352c', foundational, procedural_safeguards_fail_against_concentrated_resources).
narrative_ontology:cs_axiom_status(procedural_safeguards_fail_against_concentrated_resources, holdable).
narrative_ontology:cs_axiom_grounding('c296e3be-5e9d-4ccd-8297-37871772352c', procedural_safeguards_fail_against_concentrated_resources, empirically_contingent).
narrative_ontology:cs_axiom('c296e3be-5e9d-4ccd-8297-37871772352c', foundational, consensus_legitimacy_is_the_extracted_good).
narrative_ontology:cs_axiom_status(consensus_legitimacy_is_the_extracted_good, holdable).
narrative_ontology:cs_axiom_grounding('c296e3be-5e9d-4ccd-8297-37871772352c', consensus_legitimacy_is_the_extracted_good, empirically_contingent).
narrative_ontology:cs_reference_frame('c296e3be-5e9d-4ccd-8297-37871772352c', open_participatory_consensus).
narrative_ontology:cs_drift_state('c296e3be-5e9d-4ccd-8297-37871772352c', contemporary_capture_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c296e3be-5e9d-4ccd-8297-37871772352c', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, dominant_platform_vendors).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, unaffiliated_technical_contributors).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, small_independent_implementers).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__legitimacy_erosion_reading, multi_stakeholder_governance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the rough consensus process through working group chairs, area directors, and the IESG. Enforces procedural safeguardsâopen mailing lists, transparent minutes, chair neutralityâthat constitute the mechanism. Cannot easily abandon the rough consensus model without losing institutional identity and global legitimacy.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_administration, agenda_setter,
    institutional, generational, constrained, global).

% Deploy large engineering teams to sustain long-term working group participation, draft authorship, and meeting attendance. Use resource advantage to shape draft text and consensus declarations, extracting procedural legitimacy from the rough consensus mechanism to ratify standards that entrench their architectural and business interests.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, dominant_platform_vendors, beneficiary,
    powerful, biographical, constrained, global).

% Participate in working groups on personal time and limited funding. Bear diminishing relative influence as corporate-backed participants dominate draft cycles and consensus calls. Their technical contributions are often absorbed into drafts controlled by well-resourced actors, and their trust in the mechanism's neutrality erodes.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, unaffiliated_technical_contributors, payer,
    moderate, biographical, constrained, global).

% Rely on IETF standards for product interoperability but lack resources to attend meetings, author drafts, or sustain multi-year working group processes. Bear the cost of standards complexity and incumbent-favorable architectural choices that are ratified through captured consensus, reducing their competitive viability.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, small_independent_implementers, payer,
    moderate, biographical, constrained, global).

% Represent user privacy, accessibility, and equity concerns but are structurally disadvantaged by the IETF's volunteer-resource model and lack of funding for sustained engagement. Their absence from consensus calls means user-facing protections are often deprioritized in favor of implementer convenience.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, public_interest_advocates, excluded,
    organized, generational, trapped, global).

% Study the IETF as an institution, documenting capture dynamics, participation demographics, and outcome distributions. Neither collect benefits from the mechanism nor bear its direct costs; their analyses feed external accountability frameworks and policy debates.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, internet_governance_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__legitimacy_erosion_reading, dominant_platform_vendors).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__legitimacy_erosion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables multi-stakeholder technical coordination for Internet protocol development without centralized authority, resolving interoperability problems through distributed deliberation and shared text evolution.
% TRANSFER_FUNCTION: Moves procedural legitimacy and ratification authority from the unaffiliated technical community to well-resourced corporate actors who can sustain long-term participation, while transferring the appearance of consensus to standards that encode incumbent advantage.
% ABSENT_VOICES: End-users with no technical representation, smaller implementers who cannot afford sustained working group participation, and public interest advocates lacking corporate sponsorship are structurally absent from rough consensus formation despite the open mailing list architecture.
% DISAPPEARANCE_RATIONALE: If the rough consensus mechanism vanished overnight, Internet standards development would lose its primary deliberative and legitimation apparatus; alternative models such as formal voting, corporate consortiums, or regulatory delegation would compete to fill the vacuum, fundamentally rearranging power relations among implementers and the technical community.
% FOUNDING_PROBLEM: Early Internet development required a governance mechanism that could resolve technical disputes among competing researchers and engineers without formal institutional authority, producing interoperable standards through peer deliberation rather than vendor dictate.
% FOUNDING_PROBLEM_CORROBORATION: Early IETF participants and Internet historians corroborate the need for distributed coordination in the research era; however, independent governance researchers and critical Internet scholars contest whether the founding problem remains live in its original form, arguing the current environment of concentrated corporate power has superseded the peer-researcher context.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ietf_openness_commitment__legitimacy_erosion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the mechanism transfers ratification authority and legitimacy to actors whose dominance derives from sustained resource investment rather than distributed technical consensus. Suppression (0.68) reflects the procedural and cultural barriers that prevent alternative governance models from gaining traction within the IETF ecosystem; the open-participation frame suppresses critiques by treating absence as non-interest rather than exclusion. Theater ratio (0.55) is substantial because procedural safeguards (open calls, documented consensus) function partly to perform legitimacy while outcomes are resource-determined. Accessibility collapse (0.60) captures the dominance of the IETF modelâalternative standards bodies exist but lack the credibility to discipline IETF outcomes. Resistance (0.45) is moderate: independent contributors grumble and occasionally fork, but institutional inertia and network effects keep most dissent inside the tent. The temporal series show monotonic increase in extractiveness and theater from 0 to 30, modeling the commercialization and capture of the Internet standards space from the early research era to the current platform-dominated period.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (IETF administration) experiences the constraint as a genuine coordination mechanism with occasional capture problems, computing toward tangled_rope or even rope. The payer seats (unaffiliated contributors, small implementers) experience it as a legitimacy-laundering apparatus where their participation is harvested to certify corporate preferences, computing toward tangled_rope with higher effective extraction. The beneficiary seat (dominant vendors) experiences it as a functioning coordination substrate that happens to reflect their technical contributions, computing toward rope. The engine derives this divergence from identical structural data through directionality and scope modulation: the same mechanism is subsidy for the powerful and extraction for the moderate.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant platform vendors are declared beneficiaries: they gain ratified standards, architectural control, and legitimacy rent (d near 0.0, chi damped). Unaffiliated contributors and small implementers are declared victims/payers: they lose relative influence, absorb complexity, and suffer credibility erosion (d near 1.0, chi amplified). The IETF administration sits near symmetric (d ~0.5): it both benefits from institutional continuity and pays the cost of legitimacy depletion, though its institutional power and constrained exit keep it from full target status. Public interest advocates are excluded rather than integrated, receiving no directional flow. No override is needed because the structural derivation accurately captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, the story would bifurcate into an implausible rope (ignoring the documented capture and legitimacy extraction) or an implausible snare (denying the genuine coordination function that rough consensus still performs for interoperability). The tangled_rope gate requires naming both beneficiaries and victims, forcing the author to acknowledge that the same mechanism coordinates and extracts. The R5 genealogy interview reveals a contested founding problem: the mechanism was built for a peer-researcher environment that no longer obtains, suggesting mandatrophy pressure without resolving it into a simple zombie verdict. The metrics and claim are authored independently: the claimed type is tangled_rope, while the metrics describe a mechanism approaching snare-level extraction with substantial theaterâdivergence that the engine measures rather than reconciles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organized_capture_vs_meritocratic_participation,
    'Is the dominance of well-resourced corporate actors in rough consensus formation organized capture, or the legitimate accumulation of influence through sustained technical contribution?',
    'Longitudinal participant-influence mapping correlated with employer resource levels versus individual technical output metrics.',
    'If dominance tracks resources more than contribution, the mechanism functions as tangled_rope or snare; if it tracks contribution, the extraction reading is weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organized_capture_vs_meritocratic_participation, empirical, 'Empirical ambiguity about whether observed dominance reflects capture or meritocracy.').

omega_variable(
    procedural_safeguard_theater_boundary,
    'To what extent do procedural safeguards (open mailing lists, chair neutrality, transparent minutes) functionally prevent capture versus performatively legitimize outcomes already determined by resource advantage?',
    'Comparative case analysis of working group decisions where corporate interests are at stake, measuring alignment between participant resource levels and outcomes before and after procedural closure.',
    'If safeguards are primarily theater, theater_ratio rises and the constraint approaches snare; if they materially constrain capture, tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_safeguard_theater_boundary, conceptual, 'Conceptual ambiguity about whether procedural safeguards are functional or performative.').

omega_variable(
    ietf_openness_kernel_framing_ambiguity,
    'This constraint is the legitimacy_erosion_reading of kernel ietf_openness_commitment; the disagreement with commons_stewardship_reading centers on whether procedural safeguards preserve a genuine commons or mask extraction, while capture_substrate_reading focuses on resource translation rather than legitimacy depletion. Does the kernel admit structurally distinct constraints or a single unified reading?',
    'Engine classification comparison across the constraint family; if metrics diverge significantly, the decomposition is validated.',
    'If the readings are structurally distinct, the epsilon-invariance principle is confirmed and each reading warrants its own constraint story; if convergent, the kernel is unified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ietf_openness_kernel_framing_ambiguity, conceptual, 'Kernel reading structural location and decomposition validity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ietf_tr_t6, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(ietf_tr_t12, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(ietf_tr_t18, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(ietf_tr_t24, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(ietf_tr_t30, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ietf_be_t6, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 6, 0.32).
narrative_ontology:measurement(ietf_be_t12, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(ietf_be_t18, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 18, 0.52).
narrative_ontology:measurement(ietf_be_t24, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(ietf_be_t30, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ietf_su_t6, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(ietf_su_t12, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(ietf_su_t18, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(ietf_su_t24, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(ietf_su_t30, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, capture_substrate_reading).

% DUAL FORMULATION NOTE:
% The ietf_openness_commitment kernel decomposes into three structurally distinct readings: commons_stewardship (low extraction, public infrastructure), capture_substrate (coordination substrate with gatekeeping), and legitimacy_erosion (mechanism credibility as extracted good). Each reading carries a distinct epsilon and stakeholder geometry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
