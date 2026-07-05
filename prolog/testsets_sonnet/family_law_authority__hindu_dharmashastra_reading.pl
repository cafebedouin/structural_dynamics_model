% ============================================================================
% CONSTRAINT STORY: family_law_authority__hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__hindu_dharmashastra_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: family_law_authority__hindu_dharmashastra_reading
 *   human_readable: Hindu Marriage as Sacramental Samskara under Dharmashastra and Custom
 *   domain: comparative_law/religious_governance/family_structure
 *
 * SUMMARY:
 *   This story instantiates the hindu_dharmashastra_reading of the
 *   family_law_authority kernel: marriage as a sacramental samskara (ritual
 *   sacrament) governed by dharmic texts (Manusmriti, Yajnavalkya Smriti) and
 *   customary practice, rather than as a revocable civil contract. Before the
 *   Hindu Marriage Act 1955, this reading treated marriage as indissoluble by
 *   design; statute has since layered divorce and widow-remarriage rights on
 *   top, but customary enforcement through caste panchayats and family
 *   councils persists beneath the codified layer, particularly around caste
 *   endogamy and joint-family property. The interval (0-70) spans roughly the
 *   pre-independence colonial-era customary regime through the post-2005
 *   statutory amendments, tracking extraction rising as the sacramental cover
 *   persists after most of its original coordination function has been
 *   supplanted by codified family law, while suppression (customary sanction
 *   machinery) gradually declines as state courts absorb jurisdiction but
 *   never disappears.
 *
 * KEY AGENTS:
 *   - male_coparceners_in_joint_family: institutional beneficiary/agenda_setter — administers property and legitimacy under the sacramental frame
 *   - wives_seeking_dissolution: powerless payer, trapped — bears the cost of indissolubility framing
 *   - inter_caste_couples: powerless payer, trapped — bears social sanction for endogamy violation
 *   - widows_excluded_from_remarriage: powerless payer, trapped — bears historical remarriage bar
 *   - caste_endogamy_networks: organized beneficiary — administers boundary maintenance
 *   - dharmashastra_pandits_and_family_courts: institutional agenda_setter — interprets text and custom
 *   - reformist_legislators_and_womens_rights_organizations: excluded organized actor — statutory reform without customary enforcement reach
 *   - comparative_legal_scholars: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.68).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.71).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Hindu Marriage as Sacramental Samskara under Dharmashastra and Custom").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "comparative_law/religious_governance/family_structure").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, '2b5003ac-a8ce-450c-93ba-8ba07427293f').
narrative_ontology:cs_kernel_codification('2b5003ac-a8ce-450c-93ba-8ba07427293f', fixed_text).
narrative_ontology:cs_authority_grounding('2b5003ac-a8ce-450c-93ba-8ba07427293f', lineage).
narrative_ontology:cs_interpretation_layer_present('2b5003ac-a8ce-450c-93ba-8ba07427293f').
narrative_ontology:cs_reading_relation('2b5003ac-a8ce-450c-93ba-8ba07427293f', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b5003ac-a8ce-450c-93ba-8ba07427293f', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b5003ac-a8ce-450c-93ba-8ba07427293f', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b5003ac-a8ce-450c-93ba-8ba07427293f', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('2b5003ac-a8ce-450c-93ba-8ba07427293f', foundational, marriage_as_sacramental_samskara_not_contract).
narrative_ontology:cs_axiom_status(marriage_as_sacramental_samskara_not_contract, overridden).
narrative_ontology:cs_axiom_grounding('2b5003ac-a8ce-450c-93ba-8ba07427293f', marriage_as_sacramental_samskara_not_contract, theological).
narrative_ontology:cs_axiom('2b5003ac-a8ce-450c-93ba-8ba07427293f', foundational, caste_endogamy_as_marital_validity_condition).
narrative_ontology:cs_axiom_status(caste_endogamy_as_marital_validity_condition, holdable).
narrative_ontology:cs_axiom_grounding('2b5003ac-a8ce-450c-93ba-8ba07427293f', caste_endogamy_as_marital_validity_condition, conventional).
narrative_ontology:cs_axiom('2b5003ac-a8ce-450c-93ba-8ba07427293f', secondary, wife_as_ritual_participant_not_autonomous_party).
narrative_ontology:cs_axiom_status(wife_as_ritual_participant_not_autonomous_party, overridden).
narrative_ontology:cs_axiom_grounding('2b5003ac-a8ce-450c-93ba-8ba07427293f', wife_as_ritual_participant_not_autonomous_party, conventional).
narrative_ontology:cs_reference_frame('2b5003ac-a8ce-450c-93ba-8ba07427293f', pre_1955_indissoluble_sacramental_marriage).
narrative_ontology:cs_drift_state('2b5003ac-a8ce-450c-93ba-8ba07427293f', post_2005_coparcenary_amendment_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('2b5003ac-a8ce-450c-93ba-8ba07427293f', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, natal_and_marital_patrilineages).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, caste_endogamy_networks).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, male_coparceners_in_joint_family).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, wives_seeking_dissolution).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, inter_caste_couples).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, widows_excluded_from_remarriage).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, sacramental_indissolubility_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, varnashrama_social_order_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold and administer joint family property under the coparcenary structure that the sacramental, indissoluble marriage model preserves intact across generations. They interpret custom and dharmic text through family councils and caste panchayats, deciding what counts as legitimate marriage, valid dissolution grounds (traditionally none), and eligibility for property claims. Their exit from the constraint is effectively arbitrage — they can invoke custom when convenient and modern statute when custom disadvantages them.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, male_coparceners_in_joint_family, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, male_coparceners_in_joint_family, agenda_setter).

% Enter marriage framed as an unbreakable ritual bond (samskara) rather than a revocable contract; pre-1955 they had no dharmashastric mechanism to exit an abusive or abandoned marriage, and post-1955 statutory divorce remains socially and economically costly because leaving forfeits standing in the natal and marital lineage networks that structure their entire material and social life. Their only realistic exits are enduring the marriage, informal desertion with no legal protection, or litigation under overlapping statutory law that the community treats as illegitimate.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, wives_seeking_dissolution, payer,
    powerless, biographical, trapped, local).

% Face social excommunication, family disownment, and sometimes violence for marrying outside prescribed varna/jati boundaries, because caste endogamy is treated by customary authority as intrinsic to what makes a marriage a valid samskara rather than a separable social preference. Legal recognition under the Special Marriage Act exists but does not remove the customary penalty structure they still live inside.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, inter_caste_couples, payer,
    powerless, biographical, trapped, local).

% Historically barred from remarriage because the sacramental reading treats marriage as a once-only ritual bond persisting even through the husband's death; widow remarriage was legalized by statute in 1856 but customary sanction against it persisted in many communities long after, leaving widows dependent on the same joint family that benefits from not having to divide inheritance with a remarried widow's new household.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, widows_excluded_from_remarriage, payer,
    powerless, biographical, trapped, local).

% Caste associations and jati panchayats maintain the marriage-as-samskara framing because it naturalizes endogamy as a religious requirement rather than a social preference, which protects the caste's boundary-maintenance function, property consolidation within the group, and internal marriage-brokering economy. They administer social sanction against violators and are rarely themselves subject to the sanctions.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, caste_endogamy_networks, beneficiary,
    organized, generational, arbitrage, regional).

% Interpret classical texts (Manusmriti, Yajnavalkya Smriti) and customary precedent to adjudicate what counts as valid marriage, and since the Hindu Marriage Act 1955 operate as an interpretive layer reconciling codified statute with surviving customary and textual authority. They set the terms under which the sacramental reading is applied or relaxed, without bearing its costs themselves.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, dharmashastra_pandits_and_family_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Pushed through the 1955-56 Hindu Code Bills establishing statutory divorce, widow remarriage rights, and daughters' coparcenary rights (2005 amendment), but continue to be structurally outside the customary adjudication process at the community level — statute exists on the books while caste panchayats and family authority continue to enforce the older sacramental and endogamous norms in practice, especially in rural and lower-court venues.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, reformist_legislators_and_womens_rights_organizations, excluded,
    organized, generational, constrained, national).

% Study how the sacramental samskara reading interacts with India's other personal law systems (Muslim, Christian, Parsi) and with the secular civil option, documenting where the dharmashastra reading has been statutorily overridden versus where custom persists beneath codified law.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__hindu_dharmashastra_reading, male_coparceners_in_joint_family).
narrative_ontology:fixing_cost_class(family_law_authority__hindu_dharmashastra_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, legible ritual and property framework for family formation, lineage continuity, and intergenerational property transfer within a caste-structured social order, reducing disputes over legitimacy, inheritance, and ritual status that would otherwise require case-by-case negotiation.
% TRANSFER_FUNCTION: Moves exit options, bargaining power, and control over property and remarriage from wives, inter-caste couples, and widows toward the patrilineal joint family and the caste networks that administer and benefit from endogamy and indissolubility norms.
% ABSENT_VOICES: Wives seeking dissolution, inter-caste couples, and widows are formally covered by post-1955 statute but structurally absent from the customary adjudication venues (caste panchayats, family councils) where the sacramental reading is actually enforced day to day; their objections surface in family courts and reform advocacy, not in the bodies that set community norms.
% DISAPPEARANCE_RATIONALE: If the sacramental samskara framing and its customary enforcement vanished overnight, joint family property arrangements would need explicit renegotiation, caste endogamy would lose its religious cover and become visibly a social preference open to contest, and marriage would default toward the statutory/contractual model already available under the Hindu Marriage Act and Special Marriage Act — a substantial rearrangement of inheritance practice, caste-panchayat authority, and social sanction structures.
% FOUNDING_PROBLEM: Pre-modern Hindu social organization needed a stable, religiously legitimated mechanism to bind reproduction, property transfer, and lineage continuity to caste and family structure without relying on state contract enforcement, in a context where formal state courts did not adjudicate personal status.
% FOUNDING_PROBLEM_CORROBORATION: Dharmashastra pandits and caste networks attest the sacramental framing remains necessary for social and ritual coherence. Independent sources outside the beneficiary set — the Law Commission of India, the drafters and defenders of the Hindu Code Bills, and empirical studies of caste-panchayat adjudication (e.g. research on khap panchayats) — attest that the founding problem of orderly lineage transmission is now substantially handled by codified statute (Hindu Marriage Act 1955, Hindu Succession Act as amended 2005), and that the surviving sacramental/endogamy enforcement functions primarily to preserve caste boundary maintenance and patrilineal property control rather than to solve any problem statute cannot.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__hindu_dharmashastra_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__hindu_dharmashastra_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__hindu_dharmashastra_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that the sacramental/endogamy/coparcenary bundle now extracts more than it coordinates: the coordination problem (stable lineage and property transmission) is substantially solved by codified statute, so the surviving customary enforcement functions primarily to preserve caste boundary control and patrilineal property advantage. Suppression (0.71) captures the caste-panchayat and family-council sanction machinery, which was near-total pre-1955 (0.82) and has declined but not disappeared as state courts absorbed jurisdiction. Theater ratio rises over the interval (0.18 to 0.42) because an increasing share of the sacramental framing's public defense is performative — citing textual sanctity while statute has already displaced most of the substantive function it once served. Accessibility collapse (0.58) and resistance (0.62) are mid-range: alternatives (statutory divorce, inter-caste civil marriage, secular contract) exist and are used, but community-level sanction still meaningfully narrows their practical availability, and resistance from wives, inter-caste couples, and reform movements is real and organized, not negligible.
 *
 * PERSPECTIVAL GAP:
 *   From the coparcener/caste-network seat, the sacramental reading looks like a rope: a coordination mechanism preserving family and community coherence that everyone nominally consents to through participation in ritual. From the trapped payer seats, the identical structure computes as tangled rope shading toward snare: a coordination story providing cover for asymmetric extraction of exit options, property claims, and social standing. The engine should register this divergence directly from the structural power/exit data rather than from either side's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Male coparceners and caste networks sit near the beneficiary end: they administer the interpretive apparatus, benefit from consolidated joint-family property and endogamous marriage brokering, and can invoke either customary or statutory law depending on which favors them (arbitrage exit). Wives seeking dissolution, inter-caste couples, and widows sit near the full-target end: trapped exit options, local scope, and direct bearing of the sanctions the sacramental framing imposes. Dharmashastra pandits and family courts are agenda-setters who administer the interpretive boundary without directly bearing either the costs or capturing the property gains themselves — an interpretive layer rather than a beneficiary seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legible lineage and property transmission without state contract enforcement — was largely live in the pre-colonial and colonial period when no alternative state adjudication machinery existed. Since 1955-56 codification and the 2005 coparcenary amendment, statute now performs that coordination function directly. The sacramental/endogamy apparatus surviving at the customary level past that point is a case of institutional mandatrophy: the mandate (solve the coordination problem the state cannot yet solve) has been substantially fulfilled by statute, yet the customary enforcement structure persists, now serving primarily to preserve caste boundary control and patrilineal advantage rather than to solve a live coordination gap. Classifying this as tangled_rope rather than either pure rope or pure snare captures that a genuine (now largely obsolete) coordination function and an ongoing asymmetric extraction both remain present in the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_vs_constructed_hierarchy_ambiguity,
    'Is the sacramental samskara framing a genuine religious-metaphysical claim about the nature of marriage, or a constructed doctrine that happens to concentrate property and social control in patrilineal and caste-dominant hands?',
    'Textual-historical analysis of pre-classical versus classical dharmashastra sources, comparison with matrilineal and non-endogamous Hindu traditions (e.g. Nair, Kerala matrilineal custom) that coexisted under the same broad religious umbrella without the same property/endogamy structure, and empirical study of who administers and benefits from customary enforcement today.',
    'If the sacramental framing is shown to be substantially a later codification serving property-consolidation interests rather than an inherent feature of Hindu religious practice, this strengthens the tangled_rope-toward-snare reading; if it is a robust, cross-regional, cross-caste religious commitment independent of property effects, it strengthens a genuine-coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_vs_constructed_hierarchy_ambiguity, conceptual, 'Whether sacramental indissolubility/endogamy is intrinsic religious doctrine or constructed property-preserving overlay.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading''s structural claim diverge from the secular_contractual_reading and muslim_shariat_reading siblings, and is that divergence located in the nature-of-marriage premise (sacrament vs. contract) or in the enforcement-authority premise (customary/textual vs. state/codified)?',
    'Comparative doctrinal analysis of how each reading''s kernel_codification and authority_grounding differ, and case-law tracing of how Indian courts have adjudicated conflicts between the Hindu Marriage Act and residual customary practice.',
    'If the divergence is primarily about enforcement authority (customary vs. codified) rather than about the sacrament/contract premise itself, the sacramental_indissolubility_doctrine axiom may be less foundational than the endogamy and coparcenary axioms, changing which axiom should be treated as this reading''s core distinguishing claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the precise structural disagreement between this reading and its kernel siblings.').

omega_variable(
    caste_endogamy_separability,
    'Is caste endogamy structurally necessary to the samskara conception of marriage, or is it a separable social norm that has been bundled with the religious framing for enforcement leverage?',
    'Examine whether valid samskara marriage rites are recognized by dharmashastric authorities and community practice across caste lines in any documented historical or contemporary community, and whether such marriages are treated as ritually complete.',
    'If endogamy is separable, the extraction attributable to caste-boundary enforcement is analytically distinct from the extraction attributable to indissolubility/coparcenary structure, which would support decomposing this story further; if inseparable, the bundled treatment here is appropriate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(caste_endogamy_separability, empirical, 'Whether caste endogamy is intrinsic to or separable from the samskara marriage concept.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(fami_tr_t0, observed).
narrative_ontology:measurement(fami_tr_t12, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(fami_tr_t12, observed).
narrative_ontology:measurement(fami_tr_t24, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(fami_tr_t24, observed).
narrative_ontology:measurement(fami_tr_t36, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 36, 0.33).
narrative_ontology:measurement_basis(fami_tr_t36, observed).
narrative_ontology:measurement(fami_tr_t48, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 48, 0.37).
narrative_ontology:measurement_basis(fami_tr_t48, observed).
narrative_ontology:measurement(fami_tr_t60, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement_basis(fami_tr_t60, observed).
narrative_ontology:measurement(fami_tr_t70, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 70, 0.42).
narrative_ontology:measurement_basis(fami_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(fami_be_t0, observed).
narrative_ontology:measurement(fami_be_t12, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement_basis(fami_be_t12, observed).
narrative_ontology:measurement(fami_be_t24, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement_basis(fami_be_t24, observed).
narrative_ontology:measurement(fami_be_t36, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 36, 0.66).
narrative_ontology:measurement_basis(fami_be_t36, observed).
narrative_ontology:measurement(fami_be_t48, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 48, 0.67).
narrative_ontology:measurement_basis(fami_be_t48, observed).
narrative_ontology:measurement(fami_be_t60, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(fami_be_t60, observed).
narrative_ontology:measurement(fami_be_t70, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 70, 0.68).
narrative_ontology:measurement_basis(fami_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement_basis(fami_su_t0, observed).
narrative_ontology:measurement(fami_su_t12, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 12, 0.79).
narrative_ontology:measurement_basis(fami_su_t12, observed).
narrative_ontology:measurement(fami_su_t24, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement_basis(fami_su_t24, observed).
narrative_ontology:measurement(fami_su_t36, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 36, 0.7).
narrative_ontology:measurement_basis(fami_su_t36, observed).
narrative_ontology:measurement(fami_su_t48, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 48, 0.72).
narrative_ontology:measurement_basis(fami_su_t48, observed).
narrative_ontology:measurement(fami_su_t60, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(fami_su_t60, observed).
narrative_ontology:measurement(fami_su_t70, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 70, 0.71).
narrative_ontology:measurement_basis(fami_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__hindu_dharmashastra_reading, 0.08).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the family_law_authority kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. All five readings coexist within India's personal law system, which permits different religious communities to be governed by different family law regimes. This reading's distinguishing structural features are sacramental indissolubility (historically), caste endogamy as a validity condition, joint-family coparcenary property rules, and the wife's status as ritual participant rather than autonomous contracting party — contrasted with the muslim_shariat_reading's civil-contract (nikah) structure, the christian_canonical_reading's ecclesiastical-sacrament structure, the parsi_zoroastrian_reading's community-preservation structure, and the secular_contractual_reading's autonomous-individual-contract structure. Each sibling file carries its own ε, beneficiaries, victims, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
