% ============================================================================
% CONSTRAINT STORY: family_law_authority__hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Hindu Marriage as Sacramental Samskara under Dharmashastra Authority
 *   domain: comparative_law/religious_governance/family_law
 *
 * SUMMARY:
 *   This story instantiates the Hindu dharmashastra reading of the
 *   family_law_authority kernel: marriage as sacramental samskara,
 *   indissoluble prior to statutory reform, governed by texts (Manusmriti,
 *   Yajnavalkya Smriti, and successor commentaries) and layered customary
 *   practice specific to region, caste, and sect. The classical reading
 *   combines a genuine coordination function (settling lineage, property
 *   devolution, and ritual expectations across two families without a
 *   centralized civil registry) with asymmetric extraction (wives and widows
 *   bear the indissolubility doctrine's full weight while men retain greater
 *   informal exit; caste-endogamy externalizes costs onto couples who cross
 *   varna/jati lines; brahminical interpreters capture rents from being the
 *   necessary adjudicators of a deliberately plural, textually ambiguous
 *   corpus). The 1955-56 codification did not delete this reading; it
 *   overlaid statute atop surviving custom, so both extraction and theatrical
 *   (customary-but-non-binding) practice persist into the present.
 *
 * KEY AGENTS:
 *   - joint_family_karta: administers property and arranges marriages, primary beneficiary of lineage-binding function
 *   - male_householders: ritual actor whose lineage the samskara secures, asymmetric beneficiary of the indissolubility norm
 *   - brahminical_interpretive_authorities: capture rents from being necessary interpreters of an ambiguous textual tradition
 *   - wives_seeking_dissolution and widows: bear the doctrine's binding weight with least ability to exit
 *   - inter_caste_couples: bear externalized social and sometimes violent costs of the endogamy norm
 *   - post_1955_indian_state: partially displaced the classical reading via statute, now co-administers alongside surviving custom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.68).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.72).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Hindu Marriage as Sacramental Samskara under Dharmashastra Authority").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "comparative_law/religious_governance/family_law").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, '9400490c-9e22-4945-a53c-c7b74962bfe7').
narrative_ontology:cs_kernel_codification('9400490c-9e22-4945-a53c-c7b74962bfe7', distributed).
narrative_ontology:cs_authority_grounding('9400490c-9e22-4945-a53c-c7b74962bfe7', lineage).
narrative_ontology:cs_interpretation_layer_present('9400490c-9e22-4945-a53c-c7b74962bfe7').
narrative_ontology:cs_reading_relation('9400490c-9e22-4945-a53c-c7b74962bfe7', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('9400490c-9e22-4945-a53c-c7b74962bfe7', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('9400490c-9e22-4945-a53c-c7b74962bfe7', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('9400490c-9e22-4945-a53c-c7b74962bfe7', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('9400490c-9e22-4945-a53c-c7b74962bfe7', foundational, marriage_as_indissoluble_sacrament).
narrative_ontology:cs_axiom_status(marriage_as_indissoluble_sacrament, overridden).
narrative_ontology:cs_axiom_grounding('9400490c-9e22-4945-a53c-c7b74962bfe7', marriage_as_indissoluble_sacrament, theological).
narrative_ontology:cs_axiom('9400490c-9e22-4945-a53c-c7b74962bfe7', foundational, caste_endogamous_ritual_purity).
narrative_ontology:cs_axiom_status(caste_endogamous_ritual_purity, holdable).
narrative_ontology:cs_axiom_grounding('9400490c-9e22-4945-a53c-c7b74962bfe7', caste_endogamous_ritual_purity, conventional).
narrative_ontology:cs_axiom('9400490c-9e22-4945-a53c-c7b74962bfe7', secondary, wife_as_ritual_participant_not_contracting_party).
narrative_ontology:cs_axiom_status(wife_as_ritual_participant_not_contracting_party, overridden).
narrative_ontology:cs_axiom_grounding('9400490c-9e22-4945-a53c-c7b74962bfe7', wife_as_ritual_participant_not_contracting_party, conventional).
narrative_ontology:cs_reference_frame('9400490c-9e22-4945-a53c-c7b74962bfe7', classical_dharmashastra_samskara_doctrine).
narrative_ontology:cs_drift_state('9400490c-9e22-4945-a53c-c7b74962bfe7', post_1955_codification_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('9400490c-9e22-4945-a53c-c7b74962bfe7', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, natal_and_marital_patrilineages).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, male_householders).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, brahminical_interpretive_authorities).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, joint_family_karta).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, wives_seeking_dissolution).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, widows).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, inter_caste_couples).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, daughters_in_joint_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers joint family property under coparcenary rules, arranges marriages within caste-endogamous norms, and invokes dharmashastra and custom to settle disputes. Controls the family's economic and ritual life and benefits from the indissolubility norm, which keeps property and labor (especially of wives and daughters-in-law) bound to the lineage.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, joint_family_karta, agenda_setter,
    institutional, generational, arbitrage, national).

% Enter marriage as the ritual actor whose lineage continuity, property rights, and social standing the samskara secures. Retain far greater practical capacity to remarry, take additional wives historically, or exit informally than wives do; bear little of the sacramental indissolubility's binding weight in practice.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, male_householders, beneficiary,
    powerful, generational, mobile, national).

% Priests, pandits, and dharmashastra commentators interpret which texts and customs govern a given marriage dispute, officiate the sacrament, and adjudicate questions of caste purity and ritual validity. Their social and material position depends on being the necessary interpreters of an intentionally plural and textually ambiguous tradition.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, brahminical_interpretive_authorities, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, brahminical_interpretive_authorities, agenda_setter).

% Bound by the sacramental doctrine that marriage is indissoluble and continues across lifetimes; before 1955 codification, had essentially no doctrinal path to divorce regardless of abandonment, cruelty, or incompatibility. Dependent on natal family intervention or informal customary exceptions where these existed, which varied by region and caste and were never guaranteed.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, wives_seeking_dissolution, payer,
    powerless, biographical, trapped, local).

% Denied remarriage under classical dharmashastra doctrine on the theory that the sacramental bond persists after the husband's death; historically subject to austere widowhood regimes and, in the most extreme historical cases, immolation pressure. Property claims within the joint family were often minimal or contingent on sons.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, widows, payer,
    powerless, biographical, trapped, local).

% Caste endogamy norms treat marriage across varna or jati lines as ritually invalid or polluting under the classical reading. Couples who marry across these lines face social ostracism, loss of family and community standing, and in some contexts violent reprisal; the doctrine's ritual-purity logic supplies the justification for exclusion.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, inter_caste_couples, payer,
    powerless, biographical, constrained, local).

% Historically excluded or given reduced shares under classical coparcenary property rules that ran through the male line; married out of the natal family without commensurate property rights, while their labor and ritual participation were treated as instrumental to the samskara's completion rather than as autonomous contracting.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, daughters_in_joint_families, payer,
    powerless, biographical, constrained, local).

% Enacted the Hindu Marriage Act 1955 and Hindu Succession Act 1956, codifying limited divorce grounds and amending coparcenary rules, partially displacing the classical dharmashastra reading while retaining sacramental language and much customary practice in application. Continues to arbitrate the boundary between codified statute and surviving custom.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, post_1955_indian_state, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, post_1955_indian_state, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__hindu_dharmashastra_reading, diffuse).
narrative_ontology:fixing_cost_class(family_law_authority__hindu_dharmashastra_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared ritual and normative framework binding two families into an alliance, coordinating lineage continuity, inheritance transmission, and social status recognition within a caste-structured kinship system, and supplying settled expectations about property devolution and ritual obligation across generations.
% TRANSFER_FUNCTION: Moves women's labor, reproductive capacity, and (via reduced inheritance shares) property claims into the husband's patrilineage; moves interpretive authority and social capital to brahminical ritual specialists; moves practical exit options asymmetrically toward male householders and away from wives and widows.
% ABSENT_VOICES: Wives and widows seeking dissolution or remarriage had no doctrinal voice in classical dharmashastra adjudication; their objections surface historically only through reform movements (Brahmo Samaj, Arya Samaj, colonial-era women's associations) external to the sacramental framework itself. Inter-caste couples are structurally outside the endogamy norm's own terms of debate.
% DISAPPEARANCE_RATIONALE: If the dharmashastra reading's authority vanished overnight, joint family property arrangements, caste-endogamous marriage norms, and the indissolubility doctrine would lose their justificatory grounding; inheritance would default fully to codified statute, remarriage and inter-caste marriage would lose their remaining social sanction cost, and brahminical interpretive authority over family disputes would collapse to an advisory role.
% FOUNDING_PROBLEM: Classical Hindu jurisprudence sought to secure lineage continuity, orderly property transmission, and ritual completeness (the samskara sequence) in a context without a centralized civil registry, treating marriage as a sacred rite that stabilized kinship, inheritance, and caste boundaries simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Indian family-law scholars, the Law Commission of India, and post-independence legislative debate (culminating in the Hindu Code Bills) attest, from outside the interpretive-authority seat, that the lineage-continuity and property-transmission problems are now substantially addressed by codified statute, while indissolubility and caste-endogamy functions persist mainly as social and customary practice rather than doctrinal necessity. Traditionalist dharmashastra commentators and some community elders continue to attest the founding problem as live and the sacramental reading as still normatively binding.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness starts high (0.78) reflecting the fully unmitigated classical doctrine and declines toward statutory reform (0.65 at t=42-56) before ticking back up slightly (0.68) as the theater_ratio rises — a substantial share of the sacramental doctrine's continued invocation is now performative (ritual language retained in ceremonies and social expectation) rather than operative in law, yet it still does real extractive work in social enforcement outside the courtroom (caste-endogamy social sanction, widow remarriage stigma) even where statute has formally opened exits. Suppression declines steadily (0.85 to 0.72) as codified divorce grounds and succession reform reduce the doctrine's coercive machinery, but remains substantial because customary and community-level enforcement (social ostracism, family pressure, caste councils) persists independent of statute.
 *
 * PERSPECTIVAL GAP:
 *   From the karta's and interpretive authorities' seats, the arrangement reads as coordination: dharmashastra and custom solve a real problem of lineage and property continuity that no other institution in the classical period addressed. From the payer seats — wives, widows, inter-caste couples — the identical structure operates as enforced extraction with no doctrinal exit. The engine's per-seat computation should register this divergence directly from the declared power/exit asymmetries; the claimed_type (tangled_rope) reflects that both the coordination function and the extraction are structurally real and coexist through the same mechanism, which is exactly the tangled_rope signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (karta, male householders, interpretive authorities) sit near the full-beneficiary end: they set or interpret the rules, retain superior practical exit, and capture social/material rents from the arrangement's operation. Victims (wives seeking dissolution, widows, inter-caste couples, daughters) sit near the full-target end: trapped or constrained exit, no interpretive voice, and the doctrine's costs land disproportionately on them through indissolubility, endogamy, and property exclusion. The post_1955_indian_state occupies a genuinely mixed seat — partly an external reforming observer, partly an agenda-setter now co-administering the domain — which is why it carries a secondary_role rather than a single clean position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (securing lineage continuity and orderly property transmission absent centralized civil registration) is now substantially addressed by codified statute (Hindu Marriage Act 1955, Hindu Succession Act 1956 and its 2005 amendment), which is why founding_problem_status is 'contested' rather than 'dead' outright — traditionalist seats maintain the sacramental function is still live, while the corroborating outside record (Law Commission, legislative history) treats the coordination function as largely superseded. Classifying this as tangled_rope rather than snare preserves the historically real coordination the doctrine performed, while classifying it as tangled_rope rather than rope registers that the extraction (on wives, widows, inter-caste couples) was never incidental — it rode the same textual and customary machinery that did the coordinating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_vs_customary_authority_locus,
    'Is the binding force of this reading located in the sacramental textual doctrine (samskara theology) itself, or in regionally variable customary practice that dharmashastra texts were retrospectively invoked to legitimize?',
    'Comparative legal-historical analysis of regional variation: where customary practice (e.g., permitted divorce among certain non-Brahmin communities, matrilineal property systems in parts of the south) diverges sharply from textual dharmashastra prescription, the binding force is more plausibly customary with textual post-hoc legitimation than the reverse.',
    'If custom is primary, the reading''s coordination claim is weaker (custom varied by community and was often more flexible than the classical texts suggest) and the extraction reading strengthens, since textual invocation would function mainly to override locally workable customary alternatives in favor of a more rigid pan-Indian norm serving brahminical and patriarchal interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_vs_customary_authority_locus, conceptual, 'Whether textual doctrine or customary practice is the true locus of authority in this reading.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the hindu_dharmashastra_reading structurally diverge from the muslim_shariat_reading and secular_contractual_reading — is it primarily on dissolubility (contract vs. sacrament), or on the unit of agency (individual autonomous contractor vs. lineage/caste-embedded ritual participant)?',
    'Structural comparison of the beneficiary/victim sets and exit_options across the sibling constraint files: if the dissolubility axis alone explains the divergence, reform (statutory divorce grounds) should converge the readings; if the agency-unit axis is doing the work, statutory divorce reform would leave the caste-endogamy and joint-family extraction largely intact, which is what the historical record shows.',
    'If the agency-unit axis is primary, this reading''s extraction persists even after the dissolubility gap closes with secular/statutory reform — consistent with the observed post-1955 measurement trajectory where extractiveness declines but does not approach zero.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the precise structural axis of disagreement between this reading and its kernel siblings.').

omega_variable(
    caste_endogamy_naturalization_ambiguity,
    'Is caste-endogamy enforcement within this reading better modeled as intrinsic to the sacramental doctrine itself, or as a separable social-enforcement layer riding on top of a doctrine that is, on its own textual terms, agnostic to caste?',
    'Textual analysis of variation across dharmashastra commentaries and regional practice on inter-varna marriage (some classical texts permit certain hypergamous unions); compare enforcement intensity where state or community sanction for inter-caste marriage has been legislatively removed (e.g., Special Marriage Act availability) versus where social enforcement persists unchanged.',
    'If separable, endogamy enforcement is better modeled as a distinct constraint riding on the sacramental doctrine''s social authority rather than as part of ε for this constraint itself, which would lower the appropriately attributed extractiveness here and shift some of the victim set (inter_caste_couples) to a sibling or downstream constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(caste_endogamy_naturalization_ambiguity, conceptual, 'Whether caste-endogamy extraction belongs to this constraint''s ε or to a separable downstream social-enforcement constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fami_tr_t14, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 14, 0.18).
narrative_ontology:measurement(fami_tr_t28, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 28, 0.25).
narrative_ontology:measurement(fami_tr_t42, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 42, 0.34).
narrative_ontology:measurement(fami_tr_t56, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 56, 0.38).
narrative_ontology:measurement(fami_tr_t70, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 70, 0.4).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(fami_be_t14, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 14, 0.76).
narrative_ontology:measurement(fami_be_t28, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 28, 0.72).
narrative_ontology:measurement(fami_be_t42, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 42, 0.68).
narrative_ontology:measurement(fami_be_t56, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 56, 0.65).
narrative_ontology:measurement(fami_be_t70, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 70, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(fami_su_t14, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 14, 0.83).
narrative_ontology:measurement(fami_su_t28, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 28, 0.8).
narrative_ontology:measurement(fami_su_t42, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 42, 0.76).
narrative_ontology:measurement(fami_su_t56, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 56, 0.73).
narrative_ontology:measurement(fami_su_t70, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 70, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
